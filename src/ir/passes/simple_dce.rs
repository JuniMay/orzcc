pub const SIMPLE_DCE: &str = "simple-dce";

use super::control_flow::CfgCanonicalize;
use crate::{
    collections::linked_list::LinkedListContainerPtr,
    ir::{
        function_analysis::FunctionAnalysis,
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        Context,
        Func,
        InstKind,
    },
    utils::def_use::Usable,
};

pub struct SimpleDce {
    func_analysis: FunctionAnalysis,
}

impl Default for SimpleDce {
    fn default() -> Self {
        Self {
            func_analysis: FunctionAnalysis::new(),
        }
    }
}

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
                        if !inst.is_used(ctx) {
                            insts_to_remove.push(inst);
                        }
                    }
                    Ik::Call(symbol) => {
                        if let Some(called_func) = ctx.lookup_func(symbol) {
                            // we can remove the call if the function is pure
                            if self.func_analysis.is_pure(called_func) && !inst.is_used(ctx) {
                                insts_to_remove.push(inst);
                            }
                        }
                    }
                    Ik::CallIndirect(_) => {
                        // cannot remove
                    }
                    Ik::Store
                    | Ik::Br
                    | Ik::Jump
                    | Ik::Ret
                    | Ik::LoadElem { .. }
                    | Ik::StoreElem { .. } => {
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
                // XXX: we need unreach elim (in cfg-canonicalize) here to remove the
                // unreachable blocks, so all preds pass args to the block
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

        self.func_analysis.analyze_all(ctx);

        for func in ctx.funcs() {
            let ((), local_changed) = LocalPassMut::run(self, ctx, func)?;
            changed |= local_changed;
        }

        Ok(((), changed))
    }
}

impl TransformPass for SimpleDce {
    fn register(passman: &mut crate::ir::passman::PassManager) {
        passman.register_transform(
            SIMPLE_DCE,
            SimpleDce::default(),
            vec![Box::new(CfgCanonicalize)],
        );
    }
}
