use super::{scalar_evolution::LoopScevRecord, Lcssa, LoopSimplify, Scev, ScevAnalysis};
use crate::{
    collections::linked_list::LinkedListContainerPtr,
    ir::{
        passes::control_flow::CfgCanonicalize,
        passman::{GlobalPassMut, LocalPass, LocalPassMut, PassResult, TransformPass},
        Block,
        Context,
        Func,
        ICmpCond,
        Inst,
        Ty,
    },
    utils::{
        def_use::{Usable, User},
        loop_info::{Loop, LoopWithDepth},
    },
};

pub const DEAD_LOOP_ELIM: &str = "dead-loop-elim";

#[derive(Default)]
pub struct DeadLoopElim {
    /// Scalar evolution analysis.
    scev: ScevAnalysis,
}

impl DeadLoopElim {
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

        if let Some(Scev { repr, .. }) = scevs
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

            let preheader = lp.get_preheader(ctx, &self.scev.loop_ctx).unwrap();
            let header = lp.header(&self.scev.loop_ctx);
            let jump = preheader.tail(ctx).unwrap();

            let br = header.tail(ctx).unwrap();

            assert!(jump.is_jump(ctx));

            if !br.is_br(ctx) {
                return false;
            }

            let initial_arg = jump.succ(ctx, 0).get_arg(*repr).unwrap();

            if initial_arg == *bound && *cmp_cond == ICmpCond::Slt && !reversed {
                // this is a dead loop, replace the compare result to be always
                // false.
                let i1 = Ty::int(ctx, 1);
                let iconst = Inst::iconst(ctx, false, i1);
                header.push_front(ctx, iconst);

                let cond = br.operand(ctx, 0);

                for user in cond.users(ctx) {
                    user.replace(ctx, cond, iconst.result(ctx, 0));
                }

                changed = true;
            }
        }

        changed
    }
}

impl LocalPassMut for DeadLoopElim {
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

impl GlobalPassMut for DeadLoopElim {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        ctx.alloc_all_names();
        println!("{}", ctx.display(true));

        let mut changed = false;
        for func in ctx.funcs() {
            let (_, local_changed) = LocalPassMut::run(self, ctx, func).unwrap();
            changed |= local_changed;
        }
        Ok(((), changed))
    }
}

impl TransformPass for DeadLoopElim {
    fn register(passman: &mut crate::ir::passman::PassManager)
    where
        Self: Sized,
    {
        let pass = Self::default();

        passman.register_transform(
            DEAD_LOOP_ELIM,
            pass,
            vec![
                Box::new(CfgCanonicalize),
                Box::new(LoopSimplify::default()),
                Box::new(Lcssa::default()),
            ],
        );
    }
}
