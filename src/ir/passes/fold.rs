use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        fold::FoldedConstant,
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        Context,
        FoldContext,
        Func,
        Inst,
    },
    utils::def_use::{Usable, User},
};

pub const CONSTANT_FOLDING: &str = "constant-folding";

#[derive(Default)]
pub struct ConstantFolding {
    fold_ctx: FoldContext,
}

impl LocalPassMut for ConstantFolding {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        self.fold_ctx.clear();

        let mut folded_insts = Vec::new();

        for block in func.iter(ctx) {
            for inst in block.iter(ctx) {
                if inst.results(ctx).len() != 1 {
                    continue;
                }
                if let Some(constant) = inst.fold(ctx, &mut self.fold_ctx) {
                    let value = inst.result(ctx, 0);
                    self.fold_ctx.set(value, constant);
                    folded_insts.push(inst);
                }
            }
        }

        let changed = !folded_insts.is_empty();

        for inst in folded_insts {
            if inst.is_iconst(ctx) || inst.is_fconst(ctx) || inst.is_undef(ctx) {
                // no need to add & replace if it's already a constant
                continue;
            }

            let value = inst.result(ctx, 0);
            let folded = self.fold_ctx.lookup(value).cloned().unwrap();
            let ty = value.ty(ctx);
            let new_inst = match folded {
                FoldedConstant::Integer(v) => Inst::iconst(ctx, v, ty),
                FoldedConstant::Float(v) => Inst::fconst(ctx, v, ty),
                FoldedConstant::Undef => Inst::undef(ctx, ty),
            };

            inst.insert_after(ctx, new_inst);
            let new_value = new_inst.result(ctx, 0);

            for user in value.users(ctx) {
                user.replace(ctx, value, new_value);
            }

            // all the uses are replaced, remove the original instruction
            inst.remove(ctx);
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for ConstantFolding {
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

impl TransformPass for ConstantFolding {
    fn register(passman: &mut crate::ir::passman::PassManager) {
        let pass = ConstantFolding::default();
        passman.register_transform(CONSTANT_FOLDING, pass, Vec::new());
    }
}
