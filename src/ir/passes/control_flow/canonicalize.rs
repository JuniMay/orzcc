use thiserror::Error;

use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassError, PassManager, PassResult, TransformPass},
        Block,
        Context,
        Func,
        Inst,
        Value,
    },
    utils::def_use::Usable,
};

pub const CFG_CANONICALIZE: &str = "cfg_canonicalize";

#[derive(Debug, Error)]
pub enum CfgCanonicalizeError {
    #[error("next block has block param, jump cannot be added, block: {0:?}")]
    InvalidBlockParam(Block),

    #[error("next block not found for block: {0:?}")]
    NextBlockNotFound(Block),

    #[error("cannot remove instruction after terminator because value {0:?} is still in use")]
    ValueInUse(Value),
}

impl From<CfgCanonicalizeError> for PassError {
    fn from(value: CfgCanonicalizeError) -> Self {
        PassError::transform_error(CFG_CANONICALIZE, Box::new(value))
    }
}

/// Control flow canonicalization pass.
///
/// This pass adds jump instruction to the end of blocks that do not have a
/// terminator. And removes instructions after the terminator (if its results
/// are not used).
pub struct CfgCanonicalize;

impl LocalPassMut for CfgCanonicalize {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        let mut insts_to_remove = Vec::new();
        let mut blocks_to_modify = Vec::new();

        for block in func.iter(ctx) {
            let mut has_terminator = false;
            for inst in block.iter(ctx) {
                if has_terminator {
                    // there is already a terminator, the rest instructions are unreachable
                    insts_to_remove.push(inst);
                } else if inst.is_terminator(ctx) {
                    has_terminator = true;
                }
            }
            if !has_terminator {
                let next_block = block
                    .next(ctx)
                    .ok_or(CfgCanonicalizeError::NextBlockNotFound(block))?;
                if !next_block.params(ctx).is_empty() {
                    return Err(CfgCanonicalizeError::InvalidBlockParam(next_block).into());
                }
                blocks_to_modify.push((block, next_block));
            }
        }

        for inst in insts_to_remove.into_iter().rev() {
            for result in inst.results(ctx) {
                if !result.users(ctx).is_empty() {
                    // here the instructions are removed in reverse order, so the
                    // value should not be in use
                    return Err(CfgCanonicalizeError::ValueInUse(*result).into());
                }
            }
            inst.remove(ctx);
            changed = true;
        }

        for (block, dst) in blocks_to_modify {
            let jump = Inst::jump(ctx, dst, Vec::new());
            block.push_back(ctx, jump);
            changed = true;
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for CfgCanonicalize {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;
        for func in ctx.funcs() {
            let (_, local_changed) = LocalPassMut::run(self, ctx, func)?;
            changed |= local_changed;
        }
        Ok(((), changed))
    }
}

impl TransformPass for CfgCanonicalize {
    fn register(passman: &mut PassManager) {
        passman.register_transform(CFG_CANONICALIZE, CfgCanonicalize, Vec::new());
    }
}
