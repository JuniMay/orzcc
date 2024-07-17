//! Loop Simplify Pass
//!
//! This pass is similar to `LoopSimplify` in LLVM. It transforms natural loop
//! into a simpler form, which is easier to analyze and optimize.
//!
//! - Preheader insertion: guarantee a single, non-critical entry into the loop.
//! - Dedicated exit block: all exit blocks (blocks outside the loop that have
//!   predecessors inside the loop) only have predecessors from inside the loop.
//!   And all the exits are dominated by the loop header.
//! - Backedge: make sure there is only one backedge to the loop header.

use std::collections::{HashSet, VecDeque};

use crate::{
    collections::linked_list::LinkedListNodePtr,
    ir::{
        passes::control_flow::CfgCanonicalize,
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        Block,
        Context,
        Func,
    },
    utils::{
        cfg::{CfgInfo, CfgNode},
        dominance::Dominance,
        loop_info::{Loop, LoopContext, LoopWithDepth},
    },
};

pub const LOOP_SIMPLIFY: &str = "loop-simplify";

#[derive(Default)]
pub struct LoopSimplify {
    loop_ctx: LoopContext<Block>,
}

impl LoopSimplify {
    fn insert_preheader(&mut self, ctx: &mut Context, lp: Loop<Block>) {
        let header = lp.header(&self.loop_ctx);

        let preds = header
            .preds(ctx)
            .iter()
            .copied()
            .filter(|p| !self.loop_ctx.is_in_loop(*p, lp))
            .collect::<Vec<_>>();

        let preheader = header.split_preds(ctx, preds, ".preheader");

        if let Some(parent) = lp.parent(&self.loop_ctx) {
            // add the preheader to the upper loop if any.
            self.loop_ctx.add_node_to_loop(preheader, parent);
        }

        header.insert_before(ctx, preheader);
    }

    fn form_dedicated_exit(&mut self, ctx: &mut Context, lp: Loop<Block>) -> bool {
        let mut changed = false;

        let header = lp.header(&self.loop_ctx);

        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();

        queue.push_back(header);

        let mut exits = HashSet::new();

        while let Some(block) = queue.pop_front() {
            if !visited.insert(block) {
                continue;
            }

            for succ in block.succs(ctx) {
                if self.loop_ctx.is_in_loop(succ, lp) {
                    queue.push_back(succ);
                } else {
                    exits.insert(succ);
                }
            }
        }

        let mut in_loop_preds = Vec::new();

        for exit in exits.into_iter() {
            in_loop_preds.clear();

            let mut is_dedicated_exit = true;

            for pred in exit.preds(ctx) {
                if self.loop_ctx.is_in_loop(pred, lp) {
                    in_loop_preds.push(pred);
                } else {
                    // the pred is outside the loop, this is not a dedicated exit. we are not break
                    // because we need to collect all the in-loop preds
                    is_dedicated_exit = false;
                }
            }

            if is_dedicated_exit {
                continue;
            }

            let dedicated_exit =
                exit.split_preds(ctx, std::mem::take(&mut in_loop_preds), ".loopexit");

            if let Some(lp) = self.loop_ctx.get_loop(exit) {
                // add the dedicated exit to the same loop as exit (if any)
                self.loop_ctx.add_node_to_loop(dedicated_exit, lp);
            }

            exit.insert_before(ctx, dedicated_exit);

            changed = true;
        }

        changed
    }

    fn process_loop(&mut self, ctx: &mut Context, lp: Loop<Block>) -> bool {
        let mut changed = false;
        let header = lp.header(&self.loop_ctx);

        if lp.get_preheader(ctx, &self.loop_ctx).is_none() {
            self.insert_preheader(ctx, lp);
            changed = true;
        }

        changed |= self.form_dedicated_exit(ctx, lp);

        // we just check the preds length, if one block branches all to the header, we
        // regard it as multiple backedges
        if header.preds(ctx).len() > 2 {
            // there are at least two preds of the header, one is the preheader
            // and the other is the backedge from inside loop. If there are more
            // than two preds, we should create a backedge block and redirect
            // the backedge to the new block.

            let preheader = lp.get_preheader(ctx, &self.loop_ctx).unwrap();

            let in_loop_preds = header
                .preds(ctx)
                .into_iter()
                .filter(|p| *p != preheader)
                .collect::<Vec<_>>();

            let backedge_block = header.split_preds(ctx, in_loop_preds, ".backedge");
            // the backedge block is inside the loop
            self.loop_ctx.add_node_to_loop(backedge_block, lp);

            let mut curr_block = header;

            while self.loop_ctx.is_in_loop(curr_block, lp) && curr_block.next(ctx).is_some() {
                curr_block = curr_block.next(ctx).unwrap();
            }

            curr_block.insert_before(ctx, backedge_block);
        }

        changed
    }
}

impl LocalPassMut for LoopSimplify {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let cfg = CfgInfo::new(ctx, func);
        let dominance = Dominance::new(ctx, &cfg);
        self.loop_ctx = LoopContext::new(&cfg, &dominance);

        // LLVM process the loops from inner to outer, we do the same here.

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

impl GlobalPassMut for LoopSimplify {
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

impl TransformPass for LoopSimplify {
    fn register(passman: &mut crate::ir::passman::PassManager)
    where
        Self: Sized,
    {
        let pass = LoopSimplify::default();
        passman.register_transform(LOOP_SIMPLIFY, pass, vec![Box::new(CfgCanonicalize)]);
    }
}
