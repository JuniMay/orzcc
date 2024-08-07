use super::{scalar_evolution::LoopScevRecord, Lcssa, LoopSimplify, Scev, ScevAnalysis};
use crate::{
    collections::linked_list::LinkedListContainerPtr,
    ir::{
        passes::{control_flow::CfgCanonicalize, loops::InductionOp},
        passman::{GlobalPassMut, LocalPass, LocalPassMut, PassResult, TransformPass},
        Block,
        Context,
        Func,
        ICmpCond,
        InstKind,
    },
    utils::{
        def_use::User,
        loop_info::{Loop, LoopWithDepth},
    },
};

pub const INDVAR_SIMPLIFY: &str = "indvar-simplify";

#[derive(Default)]
pub struct IndvarSimplify {
    /// Scalar evolution analysis.
    scev: ScevAnalysis,
}

impl IndvarSimplify {
    fn process_loop(&mut self, ctx: &mut Context, lp: Loop<Block>, scevs: &LoopScevRecord) -> bool {
        let mut changed = false;

        let (lhs, cmp_cond, rhs) = if let Some(bound) = scevs.loop_bounds.get(&lp).unwrap() {
            bound
        } else {
            return false;
        };

        match cmp_cond {
            ICmpCond::Eq | ICmpCond::Ne => return false,
            // TODO: Support unsigned bounds
            ICmpCond::Ule | ICmpCond::Ult => return false,
            ICmpCond::Sle | ICmpCond::Slt => {}
        }

        if let Some(Scev { repr, step, op, .. }) = scevs
            .scevs
            .get(&lp)
            .unwrap()
            .iter()
            .find(|s| s.repr == *lhs || s.repr == *rhs)
        {
            let (bound, cmp_cond, reversed) = if repr == lhs {
                (rhs, cmp_cond, false)
            } else if repr == rhs {
                (lhs, cmp_cond, true)
            } else {
                unreachable!()
            };

            let mut step_const = None;
            if let Some(inst) = step.def_inst(ctx) {
                if let InstKind::IConst(step) = inst.kind(ctx) {
                    step_const = Some(step.as_signed());
                }
            }

            if let (InductionOp::Add, ICmpCond::Slt, false) = (op, cmp_cond, reversed) {
                if step_const == Some(1) {
                    // replace the use of the induction variable with the bound
                    // we can only replace the use in the header, and only the
                    // successor jump to exit blocks.
                    let header = lp.header(&self.scev.loop_ctx);
                    let tail = header.tail(ctx).unwrap();

                    if !tail.is_br(ctx) {
                        return false;
                    }

                    let blocks = lp.get_blocks(ctx, &self.scev.loop_ctx);

                    if blocks.len() != 2 {
                        // only process the loop with 2 blocks, header and body (also the backedge).
                        return false;
                    }

                    tail.replace(ctx, *repr, *bound);

                    changed = true;
                }
            }
        }

        changed
    }
}

impl LocalPassMut for IndvarSimplify {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        let scevs = LocalPass::run(&mut self.scev, ctx, func)?;

        let mut loops = Vec::new();

        for lp in self.scev.loop_ctx.loops() {
            let depth = lp.depth(&self.scev.loop_ctx);
            loops.push(LoopWithDepth { lp, depth });
        }

        loops.sort();

        for LoopWithDepth { lp, .. } in loops {
            changed |= self.process_loop(ctx, lp, &scevs);
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for IndvarSimplify {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;
        for func in ctx.funcs() {
            let (_, local_changed) = LocalPassMut::run(self, ctx, func).unwrap();
            changed |= local_changed;
        }
        Ok(((), changed))
    }
}

impl TransformPass for IndvarSimplify {
    fn register(passman: &mut crate::ir::passman::PassManager)
    where
        Self: Sized,
    {
        let pass = Self::default();

        passman.register_transform(
            INDVAR_SIMPLIFY,
            pass,
            vec![
                Box::new(CfgCanonicalize),
                Box::new(LoopSimplify::default()),
                Box::new(Lcssa::default()),
            ],
        );
    }
}
