use super::control_flow::CfgCanonicalize;
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        Context,
        Func,
        IBinaryOp,
        ICmpCond,
        Inst,
        InstKind,
    },
};

pub const BRANCH2SELECT: &str = "branch2select";

pub struct Branch2Select;

impl LocalPassMut for Branch2Select {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        let mut cursor = func.cursor();

        while let Some(block) = cursor.next(ctx) {
            let tail = block.tail(ctx).unwrap();

            if !tail.is_br(ctx) {
                continue;
            }

            let block_then = tail.succ(ctx, 0).block();
            let block_else = tail.succ(ctx, 1).block();

            if block_then != block_else {
                continue;
            }

            let params = block_then.params(ctx).to_vec();

            let cond = tail.operand(ctx, 0);

            if let Some(inst) = cond.def_inst(ctx) {
                if let InstKind::IBinary(IBinaryOp::Cmp(ICmpCond::Slt | ICmpCond::Sle)) =
                    inst.kind(ctx)
                {
                    let lhs = inst.operand(ctx, 0);
                    let rhs = inst.operand(ctx, 1);

                    // check if lhs is passed to the first succ and rhs is passed to the second
                    for param in params {
                        let arg_then = tail.succ(ctx, 0).get_arg(param).unwrap();
                        let arg_else = tail.succ(ctx, 1).get_arg(param).unwrap();
                        if arg_then == lhs && arg_else == rhs {
                            // lhs < /<= rhs => jump with lhs
                            // --> min lhs, rhs
                            let min = Inst::ibinary(ctx, IBinaryOp::Min, lhs, rhs);
                            tail.replace_args(ctx, param, min.result(ctx, 0));
                            inst.insert_before(ctx, min);
                            changed = true;
                            break;
                        } else if arg_then == rhs && arg_else == lhs {
                            // lhs < / <= rhs => jump with rhs
                            // --> max lhs, rhs
                            let max = Inst::ibinary(ctx, IBinaryOp::Max, lhs, rhs);
                            tail.replace_args(ctx, param, max.result(ctx, 0));
                            inst.insert_before(ctx, max);
                            changed = true;
                            break;
                        }
                    }
                }
            }
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for Branch2Select {
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

impl TransformPass for Branch2Select {
    fn register(passman: &mut crate::ir::passman::PassManager) {
        passman.register_transform(
            BRANCH2SELECT,
            Branch2Select,
            vec![Box::new(CfgCanonicalize)],
        );
    }
}
