//! Sink branch conditions prior to the branch instruction.
//!
//! ```orzir
//! %0 = icmp.slt %a, %b: i1
//! ...
//! br %0, %then, %elses
//! ```
//! -->
//!
//! ```orzir
//! %0 = icmp.slt %a, %b: i1
//! br %0, %then, %else
//! ```
//!
//! we need to make sure that the condition is only used by the branch
//! instruction.

use super::control_flow::CfgCanonicalize;
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassManager, PassResult, TransformPass},
        Context,
        Func,
    },
    utils::def_use::Usable,
};

pub const BRANCH_CONDITION_SINK: &str = "branch_condition_sink";

pub struct BranchConditionSink;

impl LocalPassMut for BranchConditionSink {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        let mut cursor = func.cursor();

        while let Some(block) = cursor.next(ctx) {
            let tail = block.tail(ctx).unwrap();

            if !tail.is_br(ctx) {
                continue;
            }

            let cond = tail.operand(ctx, 0);

            if cond.users(ctx).len() != 1 {
                continue;
            }

            if let Some(cond_def) = cond.def_inst(ctx) {
                // move the condition prior to the branch instruction
                cond_def.unlink(ctx);
                block.push_inst_before_terminator(ctx, cond_def);

                changed = true;
            }
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for BranchConditionSink {
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

impl TransformPass for BranchConditionSink {
    fn register(passman: &mut PassManager) {
        passman.register_transform(
            BRANCH_CONDITION_SINK,
            BranchConditionSink,
            vec![Box::new(CfgCanonicalize)],
        );
    }
}
