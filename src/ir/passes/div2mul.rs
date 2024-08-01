use crate::{
    collections::linked_list::LinkedListContainerPtr,
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        Context,
        Func,
        IBinaryOp,
        InstKind,
    },
};

pub const DIV2MUL: &str = "div2mul";

#[derive(Default)]
pub struct Div2mul;

impl LocalPassMut for Div2mul {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        for block in func.iter(ctx) {
            for inst in block.iter(ctx) {
                if let InstKind::IBinary(op) = inst.kind(ctx) {
                    // match op {}
                }
            }
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for Div2mul {
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

impl TransformPass for Div2mul {
    fn register(passman: &mut crate::ir::passman::PassManager) {
        passman.register_transform(DIV2MUL, Div2mul::default(), vec![]);
    }
}
