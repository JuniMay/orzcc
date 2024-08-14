//! Use heuristics to predict the branch direction of conditional branches.
//!
//! - [`equal_is_uncommon`]
//! ```orzir
//! %0 = icmp.eq %a, %b: i1
//! ...
//! br %0, %then, %else
//! ```
//! -->
//!
//! ```orzir
//! %0 = icmp.ne %a, %b: i1
//! br %0, %else, %then
//! ```

use std::vec;

use super::control_flow::CfgCanonicalize;
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassManager, PassResult, TransformPass},
        Context,
        Func,
        IBinaryOp,
        ICmpCond,
        Inst,
        InstKind,
    },
    utils::def_use::User,
};

pub const STATIC_BRANCH_PREDICTION: &str = "static-branch-prediction";

pub struct StaticBranchPrediction;

impl StaticBranchPrediction {
    pub fn equal_is_uncommon(ctx: &mut Context, func: Func) {
        let mut cursor = func.cursor();

        while let Some(block) = cursor.next(ctx) {
            let tail = block.tail(ctx).unwrap();

            if tail.is_br(ctx) {
                let cond = tail.operand(ctx, 0);

                if let Some(cond_def) = cond.def_inst(ctx) {
                    if let InstKind::IBinary(IBinaryOp::Cmp(ICmpCond::Eq)) = cond_def.kind(ctx) {
                        let lhs = cond_def.operand(ctx, 0);
                        let rhs = cond_def.operand(ctx, 1);

                        let new_cond = Inst::ibinary(ctx, IBinaryOp::Cmp(ICmpCond::Eq), lhs, rhs);
                        cond_def.insert_after(ctx, new_cond);

                        tail.inverse_br(ctx);
                        tail.replace(ctx, cond, new_cond.result(ctx, 0));
                    }
                }
            }
        }
    }
}

impl LocalPassMut for StaticBranchPrediction {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        Self::equal_is_uncommon(ctx, func);
        Ok(((), false))
    }
}

impl GlobalPassMut for StaticBranchPrediction {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;
        for func in ctx.funcs() {
            let (_, local_changed) = LocalPassMut::run(self, ctx, func)?;
            changed |= local_changed;
        }
        Ok(((), changed))
    }
}

impl TransformPass for StaticBranchPrediction {
    fn register(passman: &mut PassManager) {
        passman.register_transform(
            STATIC_BRANCH_PREDICTION,
            StaticBranchPrediction,
            vec![Box::new(CfgCanonicalize)],
        )
    }
}
