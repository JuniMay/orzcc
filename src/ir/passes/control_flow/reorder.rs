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
        dfs::DfsContext,
        dominance::Dominance,
    },
};

pub const BLOCK_REORDER: &str = "block-reorder";

pub struct BlockReorder;

impl LocalPassMut for BlockReorder {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        // let cfg = CfgInfo::new(ctx, func);
        let mut dfs = DfsContext::<Block>::default();

        let blocks: Vec<Block> = func.iter(ctx).collect();

        let mut exit = None;

        for block in blocks.iter() {
            if block.tail(ctx).unwrap().is_ret(ctx) {
                exit = Some(*block);
            }
        }

        let exit = exit.unwrap();

        let mut blocks = Vec::new();

        for block in dfs.pre_order_iter(ctx, func) {
            if block != exit {
                blocks.push(block);
            }
        }

        blocks.push(exit);

        for block in blocks.iter() {
            block.unlink(ctx);
        }

        for block in blocks.iter() {
            func.push_back(ctx, *block);
        }

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
