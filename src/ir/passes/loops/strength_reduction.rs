use super::{scalar_evolution::LoopScevRecord, InductionOp, Lcssa, LoopSimplify, ScevAnalysis};
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passes::{
            control_flow::CfgCanonicalize,
            loops::{scalar_evolution::LoopBound, Scev},
            simple_dce::SimpleDce,
        },
        passman::{GlobalPassMut, LocalPass, LocalPassMut, PassManager, PassResult, TransformPass},
        Block,
        Context,
        Func,
        IBinaryOp,
        Inst,
        InstKind,
    },
    utils::{
        def_use::{Usable, User},
        loop_info::{Loop, LoopWithDepth},
    },
};

pub const LOOP_STRENGTH_REDUCTION: &str = "loop-strength-reduction";

#[derive(Default)]
pub struct LoopStrengthReduction {
    /// Scalar evolution analysis.
    scev: ScevAnalysis,
}

impl LoopStrengthReduction {
    fn process_loop(&self, ctx: &mut Context, lp: Loop<Block>, scevs: &LoopScevRecord) -> bool {
        let preheader = lp.get_preheader(ctx, &self.scev.loops).unwrap();
        let header = lp.header(&self.scev.loops);

        let body = lp.get_blocks(ctx, &self.scev.loops);

        if body.len() != 2 {
            return false;
        }

        let LoopBound { block_param, .. } = if let Some(bound) = scevs.loop_bounds.get(&lp).unwrap()
        {
            bound
        } else {
            return false;
        };

        let Scev { init, step, op, .. } = scevs.scevs.get(block_param).unwrap();

        let ibinary_op = match op {
            InductionOp::Add => IBinaryOp::Add,
            InductionOp::Sub => IBinaryOp::Sub,
            InductionOp::Mul | InductionOp::SDiv | InductionOp::Shl => return false,
        };

        let mut changed = false;

        for user in block_param.users(ctx) {
            if let InstKind::IBinary(IBinaryOp::Mul) = user.kind(ctx) {
                let lhs = user.operand(ctx, 0);
                let rhs = user.operand(ctx, 1);

                let k = if lhs == *block_param {
                    rhs
                } else if rhs == *block_param {
                    lhs
                } else {
                    unreachable!()
                };

                let k_def_block = k.def_block(ctx);
                if self.scev.loops.is_in_loop(k_def_block, lp) {
                    // k is not a loop invariant
                    return false;
                }

                // create init * k in the preheader
                let mul = Inst::ibinary(ctx, IBinaryOp::Mul, *init, k);
                preheader.push_inst_before_terminator(ctx, mul);

                // create new block parameter for the loop
                let new_block_param = header.new_param(ctx, lhs.ty(ctx));

                // also need step * k as the new step for the mul
                let new_step = Inst::ibinary(ctx, IBinaryOp::Mul, *step, k);
                preheader.push_inst_before_terminator(ctx, new_step);

                // inside the loop, use the new block parameter and addition
                let induction_inst =
                    Inst::ibinary(ctx, ibinary_op, new_block_param, new_step.result(ctx, 0));
                user.insert_after(ctx, induction_inst);

                let old = user.result(ctx, 0);
                for user in old.users(ctx) {
                    // use before add
                    user.replace(ctx, old, new_block_param);
                }

                let preds = header.preds(ctx);
                // single back-edge is guaranteed by loop-simplify, so there should be only
                // two preds, preheader and backedge.
                assert_eq!(preds.len(), 2);

                for pred in preds {
                    let tail = pred.tail(ctx).unwrap();
                    if pred == preheader {
                        tail.add_succ_arg(ctx, header, new_block_param, mul.result(ctx, 0));
                    } else {
                        tail.add_succ_arg(
                            ctx,
                            header,
                            new_block_param,
                            induction_inst.result(ctx, 0),
                        );
                    }
                }

                changed = true;
            }
        }

        changed
    }
}

impl LocalPassMut for LoopStrengthReduction {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        let scevs = LocalPass::run(&mut self.scev, ctx, func)?;

        let mut loops = Vec::new();

        for lp in self.scev.loops.loops() {
            let depth = lp.depth(&self.scev.loops);
            loops.push(LoopWithDepth { lp, depth });
        }

        loops.sort();

        for LoopWithDepth { lp, .. } in loops {
            changed |= self.process_loop(ctx, lp, &scevs);
        }

        Ok(((), changed))
    }
}
impl GlobalPassMut for LoopStrengthReduction {
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

impl TransformPass for LoopStrengthReduction {
    fn register(passman: &mut PassManager)
    where
        Self: Sized,
    {
        let pass = Self::default();

        passman.register_transform(
            LOOP_STRENGTH_REDUCTION,
            pass,
            vec![
                Box::new(CfgCanonicalize),
                Box::new(LoopSimplify::default()),
                Box::new(Lcssa::default()),
            ],
        );

        passman.add_post_dep(LOOP_STRENGTH_REDUCTION, Box::new(SimpleDce::default()));
    }
}
