use std::collections::HashSet;

use super::{def_use::Usable, Inst, Ty, Value, ValueData};
use crate::{
    collections::{
        linked_list::LinkedListContainerPtr,
        storage::{ArenaLikeAlloc, ArenaPtr, ArenaPtrLike},
    },
    impl_arena,
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
    /// The first instruction of the block.
    head: Option<Inst>,
    /// The last instruction of the block.
    tail: Option<Inst>,
    /// The next block.
    next: Option<Block>,
    /// The previous block.
    prev: Option<Block>,
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Block(ArenaPtr<BlockData>);

impl_arena!(Context, BlockData, Block, blocks);

impl Block {
    /// Create a new block with empty parameters.
    pub fn new(ctx: &mut Context) -> Block {
        ctx.alloc_with(|this| BlockData {
            this,
            params: Vec::new(),
            users: HashSet::new(),
            head: None,
            tail: None,
            next: None,
            prev: None,
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

impl LinkedListContainerPtr<Inst> for Block {
    fn head(self, ctx: &Context) -> Option<Inst> { self.deref(ctx).head }

    fn tail(self, ctx: &Self::A) -> Option<Inst> { self.deref(ctx).tail }

    fn set_head(self, ctx: &mut Context, head: Option<Inst>) { self.deref_mut(ctx).head = head; }

    fn set_tail(self, ctx: &mut Context, tail: Option<Inst>) { self.deref_mut(ctx).tail = tail; }
}

impl Usable<Inst> for Block {
    fn users(self, ctx: &Context) -> Vec<Inst> { self.deref(ctx).users.iter().copied().collect() }

    fn add_user(self, ctx: &mut Context, user: Inst) { self.deref_mut(ctx).users.insert(user); }

    fn remove_user(self, ctx: &mut Context, user: Inst) { self.deref_mut(ctx).users.remove(&user); }
}
