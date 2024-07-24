use std::collections::VecDeque;

use super::CfgCanonicalize;
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        Block,
        Context,
        Func,
    },
    utils::{
        cfg::{CfgInfo, CfgRegion},
        dominance::Dominance,
    },
};

pub const BLOCK_REORDER: &str = "block-reorder";

pub struct BlockReorder;

impl LocalPassMut for BlockReorder {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let cfg = CfgInfo::new(ctx, func);
        let dominance = Dominance::new(ctx, &cfg);

        let blocks: Vec<Block> = func.iter(ctx).collect();

        let entry = func.entry_node(ctx);
        let mut exit = None;

        for block in blocks.iter() {
            block.unlink(ctx);
            if block.tail(ctx).unwrap().is_ret(ctx) {
                exit = Some(*block);
            }
        }

        let exit = exit.unwrap();
        let mut queue = VecDeque::new();

        queue.push_back(entry);
        while let Some(block) = queue.pop_front() {
            if block != exit {
                func.push_back(ctx, block);
            }
            for child in dominance.children(block) {
                queue.push_back(*child);
            }
        }

        func.push_back(ctx, exit);

        Ok(((), false))
    }
}

impl GlobalPassMut for BlockReorder {
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

impl TransformPass for BlockReorder {
    fn register(passman: &mut crate::ir::passman::PassManager)
    where
        Self: Sized,
    {
        passman.register_transform(BLOCK_REORDER, BlockReorder, vec![Box::new(CfgCanonicalize)]);
    }
}
