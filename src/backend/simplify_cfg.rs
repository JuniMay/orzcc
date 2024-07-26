use super::{inst::MInst, LowerConfig, MContext};
use crate::collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr};

pub struct SimplifyCfg {}

impl SimplifyCfg {
    pub fn run<I>(mctx: &mut MContext<I>, config: &LowerConfig)
    where
        I: MInst,
    {
        loop {
            let mut changed = false;

            if Self::reorder_branch(mctx, config) {
                changed = true;
            }

            if !changed {
                break;
            }
        }
    }

    /// We predict conditional branches to be not taken.
    /// So we reorder the branches to minimize the number of taken branches.
    /// ...
    ///     blt r1, r2, bb1
    ///     j bb2
    /// .bb1:
    /// ->
    /// ...
    ///     bge r1, r2, bb2
    ///     j bb1
    /// .bb1:
    pub fn reorder_branch<I>(mctx: &mut MContext<I>, _config: &LowerConfig) -> bool
    where
        I: MInst,
    {
        #[allow(unused_mut)] // TODO: fix point on assembly
        let mut changed = false;

        let funcs = mctx
            .funcs
            .iter_mut()
            .map(|(_, func_data)| func_data.self_ptr())
            .collect::<Vec<_>>();

        for func in funcs {
            if func.is_external(mctx) {
                continue;
            }

            let mut cursor = func.cursor();
            while let Some(block) = cursor.next(mctx) {
                if let Some(maybe_j) = block.tail(mctx) {
                    if let Some(maybe_br) = maybe_j.prev(mctx) {
                        if let (Some(j_block), Some(br_block)) = (
                            maybe_j.match_unconditional_branch(mctx),
                            maybe_br.match_conditional_branch(mctx),
                        ) {
                            if block.next(mctx).is_some()
                                && br_block == block.next(mctx).unwrap()
                                && j_block != br_block
                            {
                                maybe_br.inverse_conditional_branch(mctx, j_block);
                                maybe_j.redirect_branch(mctx, br_block);
                            }
                        }
                    }
                }
            }
        }

        changed
    }
}
