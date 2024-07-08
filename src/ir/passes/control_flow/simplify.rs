//! Simplification of Control Flow Graph
//!
//! This pass performs unreachable block elimination, and merges blocks.
//!
//! Details:
//! - Remove unreachable blocks (has no predecessors)
//! - Merge two blocks (say A and B) if:
//!     - A has only one successor B
//!     - B has only one predecessor A
//!     - Exception: A terminated with a `br` that jumps to B with different
//!       arguments
//! - Remove block parameters if the block has only one predecessor
//!     - Exception: The predecessor terminates with a `br` that jumps to this
//!       block with different arguments
//! - Remove blocks that only contain a jump instruction
//!
//! # See Also
//!
//! - [Inst::num_succ_to](crate::ir::Inst::num_succ_to)

use std::collections::HashSet;

use crate::{
    collections::linked_list::LinkedListContainerPtr,
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        remove_all_insts,
        Context,
        Func,
    },
    utils::cfg::CfgInfo,
};

pub const CFG_SIMPLIFY: &str = "cfg-simplify";

pub struct CfgSimplify;

impl CfgSimplify {
    fn eliminate_unreachable_blocks(&mut self, ctx: &mut Context, func: Func) -> bool {
        let cfg = CfgInfo::new(ctx, func);

        let mut changed = false;

        let reachables = cfg.reachable_nodes(ctx);

        let mut insts_to_remove = Vec::new();
        let mut unreachables = HashSet::new();

        for block in func.iter(ctx) {
            if reachables.contains(&block) {
                continue;
            }

            for inst in block.iter(ctx) {
                insts_to_remove.push(inst);
            }

            unreachables.insert(block);
        }

        // TODO: not best effort but panic, maybe we should return an error
        remove_all_insts(ctx, insts_to_remove, false);

        changed |= !unreachables.is_empty();

        for block in unreachables {
            block.remove(ctx);
        }

        changed
    }
}

impl LocalPassMut for CfgSimplify {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        changed |= self.eliminate_unreachable_blocks(ctx, func);

        Ok(((), changed))
    }
}

impl GlobalPassMut for CfgSimplify {
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

impl TransformPass for CfgSimplify {
    fn register(passman: &mut crate::ir::passman::PassManager)
    where
        Self: Sized,
    {
        passman.register_transform(CFG_SIMPLIFY, CfgSimplify, Vec::new())
    }
}
