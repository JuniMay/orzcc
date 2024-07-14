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

use rustc_hash::{FxHashMap, FxHashSet};

use super::CfgCanonicalize;
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassManager, PassResult, TransformPass},
        remove_all_insts,
        Context,
        Func,
        Inst,
        InstKind,
        ValueKind,
    },
    utils::{
        cfg::CfgInfo,
        def_use::{Usable, User},
    },
};

pub const CFG_SIMPLIFY: &str = "cfg-simplify";

pub struct CfgSimplify;

impl CfgSimplify {
    fn eliminate_unreachable_blocks(&mut self, ctx: &mut Context, func: Func) -> bool {
        let cfg = CfgInfo::new(ctx, func);

        let mut changed = false;

        let reachables = cfg.reachable_nodes(ctx);

        let mut insts_to_remove = Vec::new();
        let mut unreachables = FxHashSet::default();

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

    fn straighten(&mut self, ctx: &mut Context, func: Func) -> bool {
        // merge blocks that has only one succ, and the succ has only one pred
        let mut changed = false;

        let cfg = CfgInfo::new(ctx, func);

        let mut cursor = func.cursor();

        while let Some(block) = cursor.next(ctx) {
            // we need to be careful if the last terminator is a br
            if cfg.succs(block).unwrap().len() != 1 {
                continue;
            }
            let succ = cfg.succs(block).unwrap()[0];
            if cfg.preds(succ).unwrap().len() != 1 {
                continue;
            }
            match block.tail(ctx) {
                Some(inst) if inst.is_terminator(ctx) => {
                    if inst.num_succ_to(ctx, succ) != 1 {
                        // TODO: This should not be reachable anymore, because now the number of
                        // preds of succ is 1. But we need to test it.
                        continue;
                    }
                }
                Some(_) => {
                    panic!("block tail is not a terminator, do canonicalization first");
                }
                None => {
                    panic!("block has no tail, do canonicalization first");
                }
            }

            block.merge(ctx, succ);
            succ.remove(ctx);
            changed = true;

            // TODO: the CFG info is not updated, so we must iterate multiple
            // times to ensure that all blocks are merged
        }

        changed
    }

    fn remove_jump_only_blocks(&mut self, ctx: &mut Context, func: Func) -> bool {
        // if a block is jump only, we can hoist the jump instruction to the
        // predecessor, replace the jump instruction to the block with a jump
        // instruction to the successor of the block

        // if the block has any arguments, we cannot do this optimization.
        // TODO: maybe we can merge the block arguments, but that also requires
        // us to merge the block parameters. an example is the backedge block generated
        // by loop simplify.

        let mut changed = false;

        let mut cursor = func.cursor();

        while let Some(block) = cursor.next(ctx) {
            if block.head(ctx) != block.tail(ctx) {
                continue;
            }

            if !block.params(ctx).is_empty() {
                continue;
            }

            match block.tail(ctx) {
                Some(inst) if inst.is_terminator(ctx) => {
                    if !inst.is_jump(ctx) {
                        continue;
                    }
                    let succ_args = inst.succ(ctx, 0).args();
                    let succ_block = inst.succ(ctx, 0).block();

                    let args = succ_block
                        .params(ctx)
                        .iter()
                        .map(|param| succ_args[param].inner())
                        .collect::<Vec<_>>();

                    for user in block.users(ctx) {
                        user.replace_succ_with_args(ctx, block, succ_block, args.clone());
                        changed = true;
                    }
                    // we don't need to remove the block, because it will become
                    // unreachable in the next iteration
                }
                // XXX: require cfg canonicalization first
                Some(_) => {
                    panic!("block tail is not a terminator, do canonicalization first");
                }
                None => {
                    panic!("block has no tail, do canonicalization first");
                }
            }
        }

        changed
    }

    fn remove_single_pred_params(&mut self, ctx: &mut Context, func: Func) -> bool {
        let mut changed = false;

        let mut cursor = func.cursor();

        while let Some(block) = cursor.next(ctx) {
            if block.total_uses(ctx) != 1 {
                continue;
            }

            let mut args = None;

            for succ in block.users(ctx)[0].succs(ctx) {
                if succ.block() == block {
                    args = Some(
                        succ.args()
                            .iter()
                            .map(|(param, arg)| (*param, arg.inner()))
                            .collect::<FxHashMap<_, _>>(),
                    );
                }
            }

            let args = args.unwrap();

            // there is only one pred, so we can drop the block params safely
            while !block.params(ctx).is_empty() {
                let len = block.params(ctx).len();

                let param = block.params(ctx)[len - 1];

                // replace the uses of the param with the argument
                let arg = args[&param];
                for user in param.users(ctx) {
                    user.replace(ctx, param, arg);
                }

                block.drop_param(ctx, len - 1);
                changed = true;
            }
        }

        changed
    }

    fn reduce_always_jump(&mut self, ctx: &mut Context, func: Func) -> bool {
        let mut changed = false;
        let mut insts_to_remove = Vec::new();

        let mut cursor = func.cursor();
        while let Some(block) = cursor.next(ctx) {
            let mut cursor = block.cursor();
            while let Some(inst) = cursor.next(ctx) {
                if !inst.is_br(ctx) {
                    continue;
                }

                let cond = inst.operand(ctx, 0);
                if let ValueKind::InstResult {
                    inst: cond_inst, ..
                } = cond.kind(ctx)
                {
                    let cond_inst = *cond_inst;

                    if let InstKind::IConst(v) = cond_inst.kind(ctx) {
                        insts_to_remove.push(inst);
                        let (block, args) = if v.is_one() {
                            let succ_block = inst.succ(ctx, 0).block();
                            let succ_args = inst
                                .succ(ctx, 0)
                                .args()
                                .iter()
                                .map(|(param, arg)| (*param, arg.inner()))
                                .collect::<FxHashMap<_, _>>();

                            (succ_block, succ_args)
                        } else {
                            let succ_block = inst.succ(ctx, 1).block();
                            let succ_args = inst
                                .succ(ctx, 1)
                                .args()
                                .iter()
                                .map(|(param, arg)| (*param, arg.inner()))
                                .collect::<FxHashMap<_, _>>();

                            (succ_block, succ_args)
                        };

                        let args = block
                            .params(ctx)
                            .iter()
                            .map(|param| args[param])
                            .collect::<Vec<_>>();

                        let jump = Inst::jump(ctx, block, args);
                        inst.insert_before(ctx, jump);

                        changed = true;
                    }
                }
            }
        }

        remove_all_insts(ctx, insts_to_remove, false);

        changed
    }
}

impl LocalPassMut for CfgSimplify {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        changed |= self.eliminate_unreachable_blocks(ctx, func);
        changed |= self.straighten(ctx, func);
        changed |= self.remove_jump_only_blocks(ctx, func);
        changed |= self.remove_single_pred_params(ctx, func);
        changed |= self.reduce_always_jump(ctx, func);

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
    fn register(passman: &mut PassManager) {
        passman.register_transform(CFG_SIMPLIFY, CfgSimplify, vec![Box::new(CfgCanonicalize)]);
    }
}
