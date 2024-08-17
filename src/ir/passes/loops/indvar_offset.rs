use super::{scalar_evolution::LoopScevRecord, InductionOp, Lcssa, LoopSimplify, ScevAnalysis};
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passes::{control_flow::CfgCanonicalize, loops::Scev, simple_dce::SimpleDce},
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

pub const INDVAR_OFFSET: &str = "indvar-offset";

#[derive(Default)]
pub struct IndvarOffset {
    scev: ScevAnalysis,
}

impl IndvarOffset {
    fn process_loop(&self, ctx: &mut Context, lp: Loop<Block>, scevs: &LoopScevRecord) -> bool {
        let preheader = lp.get_preheader(ctx, &self.scev.loops).unwrap();
        let header = lp.header(&self.scev.loops);

        let blocks = lp.get_blocks(ctx, &self.scev.loops);

        if blocks.len() != 2 {
            return false;
        }

        let body = if blocks[0] == header {
            blocks[1]
        } else {
            blocks[0]
        };

        let mut cursor = body.cursor();

        let mut changed = false;

        while let Some(inst) = cursor.next(ctx) {
            if let InstKind::Offset = inst.kind(ctx) {
                let ptr = inst.operand(ctx, 0);
                let offset = inst.operand(ctx, 1);

                if let Some(Scev { init, step, op, .. }) = scevs.scevs.get(&offset) {
                    let new_step = match op {
                        InductionOp::Add => *step,
                        InductionOp::Sub => {
                            // negate the step, step is a loop invariant, sub it with 0 in the
                            // preheader
                            let zero = Inst::iconst(ctx, 0, step.ty(ctx));
                            let new_step =
                                Inst::ibinary(ctx, IBinaryOp::Sub, zero.result(ctx, 0), *step);
                            preheader.push_inst_before_terminator(ctx, zero);
                            preheader.push_inst_before_terminator(ctx, new_step);
                            new_step.result(ctx, 0)
                        }
                        InductionOp::Mul | InductionOp::SDiv | InductionOp::Shl => return false,
                    };

                    // the offset is a indvar, we can just induce the pointer,
                    // and maybe the indvar can be removed later.

                    // firstly, create a new block parameter for the pointer
                    let new_ptr = header.new_param(ctx, ptr.ty(ctx));

                    // secondly, create a new `offset` in the preheader, as the initial value
                    let init_ptr = Inst::offset(ctx, ptr, *init);
                    preheader.push_inst_before_terminator(ctx, init_ptr);

                    // replace all the use of the old offset-ed ptr with the block parameter
                    let old = inst.result(ctx, 0);
                    for user in old.users(ctx) {
                        user.replace(ctx, old, new_ptr);
                    }

                    // induce the offset
                    let induced_ptr = Inst::offset(ctx, new_ptr, new_step);
                    inst.insert_before(ctx, induced_ptr);

                    let preds = header.preds(ctx);
                    assert_eq!(preds.len(), 2);

                    for pred in preds {
                        let tail = pred.tail(ctx).unwrap();
                        if pred == preheader {
                            tail.add_succ_arg(ctx, header, new_ptr, init_ptr.result(ctx, 0));
                        } else {
                            tail.add_succ_arg(ctx, header, new_ptr, induced_ptr.result(ctx, 0));
                        }
                    }

                    changed = true;
                }
            }
        }

        changed
    }
}

impl LocalPassMut for IndvarOffset {
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
impl GlobalPassMut for IndvarOffset {
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

impl TransformPass for IndvarOffset {
    fn register(passman: &mut PassManager)
    where
        Self: Sized,
    {
        let pass = Self::default();

        passman.register_transform(
            INDVAR_OFFSET,
            pass,
            vec![
                Box::new(CfgCanonicalize),
                Box::new(LoopSimplify::default()),
                Box::new(Lcssa::default()),
            ],
        );

        passman.add_post_dep(INDVAR_OFFSET, Box::new(SimpleDce::default()));
    }
}
