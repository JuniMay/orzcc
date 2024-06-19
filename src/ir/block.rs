use std::collections::HashSet;

use super::{def_use::Usable, Func, Inst, Ty, Value, ValueData};
use crate::{
    collections::{
        linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
        storage::{ArenaAlloc, ArenaPtr, BaseArenaPtr},
    },
    impl_arena,
    ir::Context,
    utils::CfgNode,
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
    /// The parent function of the block.
    parent: Option<Func>,
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Block(BaseArenaPtr<BlockData>);

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
            parent: None,
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

    pub fn merge(self, ctx: &mut Context, other: Block) {
        LinkedListContainerPtr::merge(self, ctx, other);
        todo!("block merge should also update the def-use and users of block parameters")
    }

    pub fn split(self, ctx: &mut Context, inst: Inst) -> Block {
        let other = Block::new(ctx);
        LinkedListContainerPtr::split(self, ctx, other, inst);
        LinkedListNodePtr::insert_after(self, ctx, other);
        todo!("block split should also update the def-use and users of block parameters")
        // other
    }
}

impl CfgNode for Block {
    type Region = Func;

    fn succs(&self, arena: &Self::A) -> Vec<Self> {
        let tail_inst = self.tail(arena);

        if let Some(tail_inst) = tail_inst {
            if tail_inst.is_terminator(arena) {
                tail_inst.succ_blocks(arena)
            } else {
                // check if next block requires no block params
                let next_block = self
                    .next(arena)
                    .expect("block has neither terminator nor next block");
                if next_block.params(arena).is_empty() {
                    vec![next_block]
                } else {
                    panic!("block has no terminator but next block requires block params")
                }
            }
        } else {
            // check if next block requires no block params
            let next_block = self
                .next(arena)
                .expect("block has no instruction and no next block");
            if next_block.params(arena).is_empty() {
                vec![next_block]
            } else {
                panic!("block has no instruction but next block requires block params")
            }
        }
    }
}

impl LinkedListContainerPtr<Inst> for Block {
    fn head(self, ctx: &Context) -> Option<Inst> { self.deref(ctx).head }

    fn tail(self, ctx: &Self::A) -> Option<Inst> { self.deref(ctx).tail }

    fn set_head(self, ctx: &mut Context, head: Option<Inst>) { self.deref_mut(ctx).head = head; }

    fn set_tail(self, ctx: &mut Context, tail: Option<Inst>) { self.deref_mut(ctx).tail = tail; }
}

impl LinkedListNodePtr for Block {
    type ContainerPtr = Func;

    fn next(self, ctx: &Context) -> Option<Block> { self.deref(ctx).next }

    fn prev(self, ctx: &Context) -> Option<Block> { self.deref(ctx).prev }

    fn set_next(self, ctx: &mut Context, next: Option<Block>) { self.deref_mut(ctx).next = next; }

    fn set_prev(self, ctx: &mut Context, prev: Option<Block>) { self.deref_mut(ctx).prev = prev; }

    fn container(self, ctx: &Context) -> Option<Func> { self.deref(ctx).parent }

    fn set_container(self, ctx: &mut Context, container: Option<Func>) {
        self.deref_mut(ctx).parent = container;
    }
}

impl Usable<Inst> for Block {
    fn users(self, ctx: &Context) -> Vec<Inst> { self.deref(ctx).users.iter().copied().collect() }

    fn add_user(self, ctx: &mut Context, user: Inst) { self.deref_mut(ctx).users.insert(user); }

    fn remove_user(self, ctx: &mut Context, user: Inst) { self.deref_mut(ctx).users.remove(&user); }
}
