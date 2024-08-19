use rustc_hash::FxHashMap;

use super::CfgCanonicalize;
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        Block,
        Context,
        Func,
        Inst,
    },
};

pub const SPLIT_CRITICAL_EDGE: &str = "split-critical-edge";

pub struct SplitCriticalEdge;

impl LocalPassMut for SplitCriticalEdge {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut cursor = func.cursor();

        let mut critical_edges = FxHashMap::default();

        while let Some(block) = cursor.next(ctx) {
            let tail = block.tail(ctx).unwrap();

            if tail.succs(ctx).len() <= 1 {
                continue;
            }

            for (i, successor) in tail.succs(ctx).iter().enumerate() {
                let succ = successor.block();
                if succ.params(ctx).is_empty() {
                    continue;
                }
                if succ.preds(ctx).len() > 1 {
                    critical_edges.entry(tail).or_insert_with(Vec::new).push(i);
                }
            }
        }

        for (inst, edges) in critical_edges {
            let curr_block = inst.container(ctx).unwrap();

            let name = curr_block.name_or_alloc(ctx, "bb").clone();

            for succ_idx in edges {
                let succ = inst.succ(ctx, succ_idx).block();
                let params = succ.params(ctx).to_vec();
                let mut args = Vec::new();

                for param in params {
                    args.push(inst.succ(ctx, succ_idx).get_arg(param).unwrap());
                }

                let new_block = Block::new(ctx);

                new_block.alloc_name(ctx, format!("{}.critical.{}_", name, succ_idx));

                curr_block.insert_after(ctx, new_block);
                inst.replace_single_succ_with_args(ctx, succ_idx, new_block, Vec::new());

                let jump = Inst::jump(ctx, succ, args);
                new_block.push_back(ctx, jump);
            }
        }

        Ok(((), false)) // only run once.
    }
}

impl GlobalPassMut for SplitCriticalEdge {
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

impl TransformPass for SplitCriticalEdge {
    fn register(passman: &mut crate::ir::passman::PassManager) {
        passman.register_transform(
            SPLIT_CRITICAL_EDGE,
            SplitCriticalEdge,
            vec![Box::new(CfgCanonicalize)],
        );
    }
}
