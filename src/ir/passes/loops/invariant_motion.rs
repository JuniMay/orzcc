use std::collections::VecDeque;

use rustc_hash::FxHashSet;

use super::simplify::LoopSimplify;
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passes::control_flow::CfgCanonicalize,
        passman::{GlobalPassMut, LocalPassMut, PassManager, PassResult, TransformPass},
        Block,
        Context,
        Func,
        InstKind,
        ValueKind,
    },
    utils::{
        cfg::{CfgInfo, CfgNode},
        dominance::Dominance,
        loop_info::{Loop, LoopContext, LoopWithDepth},
    },
};

pub const LOOP_INVARIANT_MOTION: &str = "loop-invariant-motion";

#[derive(Default)]
pub struct LoopInvariantMotion {
    loop_ctx: LoopContext<Block>,
}

impl LoopInvariantMotion {
    fn process_loop(&mut self, ctx: &mut Context, lp: Loop<Block>) -> bool {
        use InstKind as Ik;

        let header = lp.header(&self.loop_ctx);
        let preheader = lp
            .get_preheader(ctx, &self.loop_ctx)
            .expect("preheader should have been created by loop-simplify");

        let mut visited = FxHashSet::default();
        let mut queue = VecDeque::new();
        let mut changed = false;

        queue.push_back(header);

        while let Some(block) = queue.pop_front() {
            if !visited.insert(block) {
                continue;
            }

            let mut next_inst = block.head(ctx);
            while let Some(inst) = next_inst {
                next_inst = inst.next(ctx);

                let mut movable = true;

                for opd in inst.operands(ctx) {
                    match opd.kind(ctx) {
                        ValueKind::BlockParam { block, .. } => {
                            // check if the block is in this loop
                            if self.loop_ctx.is_in_loop(*block, lp) {
                                movable = false;
                                break;
                            }
                        }
                        ValueKind::InstResult { inst, .. } => {
                            // check if the instruction is in this loop
                            let def_block = inst.container(ctx).unwrap();
                            if self.loop_ctx.is_in_loop(def_block, lp) {
                                movable = false;
                                break;
                            }
                        }
                    }
                }

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
                    | Ik::Offset => {
                        // these instructions are movable
                    }
                    Ik::Call(_) | Ik::CallIndirect(_) => {
                        // TODO: we need to decide if there are any side effects
                        movable = false;
                    }
                    Ik::Load => {
                        // we don't know if any store in the loop modifies the
                        // address, so we can't move the load
                        movable = false;
                    }
                    Ik::Store | Ik::Br | Ik::Jump | Ik::Ret => {
                        // these instructions are not movable
                        movable = false;
                    }
                    // stack slot should not appear in the loop
                    Ik::StackSlot(_) => unreachable!(),
                }

                // if the instruction is movable, we can unlink and move it to
                // the preheader
                if movable {
                    inst.unlink(ctx);
                    preheader.push_inst_before_terminator(ctx, inst);
                    changed = true;
                }
            }

            for succ in block.succs(ctx) {
                if self.loop_ctx.is_in_loop(succ, lp) {
                    queue.push_back(succ);
                }
            }
        }

        changed
    }
}

impl LocalPassMut for LoopInvariantMotion {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        // we should iterate loop from inner to outer, so that we can move as many
        // instructions as possible to the preheader of the outermost loop

        let cfg = CfgInfo::new(ctx, func);
        let dominance = Dominance::new(ctx, &cfg);

        self.loop_ctx = LoopContext::new(&cfg, &dominance);

        let mut loops = Vec::new();

        for lp in self.loop_ctx.loops() {
            let depth = lp.depth(&self.loop_ctx);
            loops.push(LoopWithDepth { lp, depth });
        }

        loops.sort();
        loops.reverse();

        let mut changed = false;

        for LoopWithDepth { lp, .. } in loops {
            changed |= self.process_loop(ctx, lp);
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for LoopInvariantMotion {
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

impl TransformPass for LoopInvariantMotion {
    fn register(passman: &mut PassManager) {
        let pass = Self::default();
        passman.register_transform(
            LOOP_INVARIANT_MOTION,
            pass,
            // TODO: auto manage dependencies
            vec![Box::new(CfgCanonicalize), Box::new(LoopSimplify::default())],
        );
    }
}
