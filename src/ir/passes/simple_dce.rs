pub const SIMPLE_DCE: &str = "simple-dce";

use super::control_flow::CfgSimplify;
use crate::{
    collections::linked_list::LinkedListContainerPtr,
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        Context,
        Func,
        InstKind,
    },
    utils::def_use::Usable,
};

pub struct SimpleDce;

impl LocalPassMut for SimpleDce {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        use InstKind as Ik;

        let mut insts_to_remove = Vec::new();

        for block in func.iter(ctx) {
            for inst in block.iter(ctx) {
                match inst.kind(ctx) {
                    Ik::Undef
                    | Ik::IConst(_)
                    | Ik::FConst(_)
                    | Ik::IBinary(_)
                    | Ik::FBinary(_)
                    | Ik::IUnary(_)
                    | Ik::FUnary(_)
                    | Ik::Cast(_)
                    | Ik::GetGlobal(_)
                    | Ik::Load
                    | Ik::Offset
                    | Ik::StackSlot(_) => {
                        let mut used = false;
                        for result in inst.results(ctx) {
                            if !result.users(ctx).is_empty() {
                                used = true;
                                break;
                            }
                        }

                        if !used {
                            insts_to_remove.push(inst);
                        }
                    }
                    Ik::Call(_) | Ik::CallIndirect(_) => {
                        // maybe side effect, cannot remove (for now)
                    }
                    Ik::Store | Ik::Br | Ik::Jump | Ik::Ret => {
                        // cannot remove
                    }
                }
            }
        }

        let mut changed = !insts_to_remove.is_empty();

        for inst in insts_to_remove {
            inst.remove(ctx);
        }

        // remove all the block params that are not used
        let mut cursor = func.cursor();
        while let Some(block) = cursor.next(ctx) {
            if block == func.head(ctx).unwrap() {
                // we cannot remove the params of the entry block, because they are crucial to
                // arguments passing
                continue;
            }

            let params = block.params(ctx).to_vec();

            for (i, param) in params.into_iter().enumerate().rev() {
                // XXX: we need unreach elim (in cfg-simplify) here to remove the unreachable
                // blocks, so all preds pass args to the block
                if param.users(ctx).is_empty() {
                    // the index will be updated after each removal, so we do a reverse iteration
                    block.drop_param(ctx, i);
                    changed = true;
                }
            }
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for SimpleDce {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        for func in ctx.funcs() {
            let ((), local_changed) = LocalPassMut::run(self, ctx, func)?;
            changed |= local_changed;
        }

        Ok(((), changed))
    }
}

impl TransformPass for SimpleDce {
    fn register(passman: &mut crate::ir::passman::PassManager) {
        passman.register_transform(SIMPLE_DCE, SimpleDce, vec![Box::new(CfgSimplify)]);
    }
}
