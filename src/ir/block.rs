use std::collections::HashSet;

use super::{def_use::Usable, Inst, Ty, Value, ValueData};
use crate::{
    collections::storage::{ArenaLikeAlloc, ArenaLikeDeref, ArenaLikeFree, ArenaPtr, ArenaPtrLike},
    ir::Context,
};

/// The data of a block.
pub struct BlockData {
    /// The self reference of the block.
    this: Block,
    /// The parameters of the block.
    params: Vec<Value>,
    /// The users of the block.
    users: HashSet<Inst>,
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Block(ArenaPtr<BlockData>);

impl Block {
    /// Create a new block with empty parameters.
    pub fn new(ctx: &mut Context) -> Block {
        ctx.alloc_with(|this| BlockData {
            this,
            params: Vec::new(),
            users: HashSet::new(),
        })
    }

    /// Create a new parameter of the block and append it to the block.
    ///
    /// # Parameters
    ///
    /// - `ctx`: The context of IR.
    /// - `ty`: The type of the parameter to create.
    ///
    /// # Returns
    ///
    /// A value, which is the new parameter of the block.
    pub fn new_param(self, ctx: &mut Context, ty: Ty) -> Value {
        let idx = self.deref(ctx).params.len();
        let value = ctx.alloc_with(|this| ValueData::new_block_param(this, ty, self, idx));
        self.deref_mut(ctx).params.push(value);
        value
    }

    pub fn params(self, ctx: &Context) -> &[Value] { self.deref(ctx).params.as_slice() }
}

impl Usable<Inst> for Block {
    fn users(self, ctx: &Context) -> Vec<Inst> { self.deref(ctx).users.iter().copied().collect() }

    fn add_user(self, ctx: &mut Context, user: Inst) { self.deref_mut(ctx).users.insert(user); }

    fn remove_user(self, ctx: &mut Context, user: Inst) { self.deref_mut(ctx).users.remove(&user); }
}

impl ArenaPtrLike for Block {
    type A = Context;
    type T = BlockData;

    fn try_deref(self, ctx: &Self::A) -> Option<&Self::T> { ctx.try_deref(self) }

    fn try_deref_mut(self, ctx: &mut Self::A) -> Option<&mut Self::T> { ctx.try_deref_mut(self) }
}

impl ArenaLikeDeref<BlockData, Block> for Context {
    fn try_deref(&self, block: Block) -> Option<&BlockData> { self.blocks.try_deref(block.0) }

    fn try_deref_mut(&mut self, block: Block) -> Option<&mut BlockData> {
        self.blocks.try_deref_mut(block.0)
    }
}

impl ArenaLikeAlloc<BlockData, Block> for Context {
    fn alloc_with<F>(&mut self, f: F) -> Block
    where
        F: FnOnce(Block) -> BlockData,
    {
        let ptr = self.blocks.alloc_with(|ptr| {
            let block = Block(ptr);
            f(block)
        });
        Block(ptr)
    }
}

impl ArenaLikeFree<BlockData, Block> for Context {
    fn free(&mut self, block: Block) { self.blocks.free(block.0) }
}
