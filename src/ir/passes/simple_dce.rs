pub const SIMPLE_DCE: &str = "simple-dce";

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

        let changed = !insts_to_remove.is_empty();

        for inst in insts_to_remove {
            inst.remove(ctx);
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
        passman.register_transform(SIMPLE_DCE, SimpleDce, Vec::new());
    }
}
