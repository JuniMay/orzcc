use std::collections::HashSet;

use super::{debug::CommentPos, Context, Func, Inst, Ty, Value, ValueData};
use crate::{
    collections::{
        linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
        storage::{ArenaAlloc, ArenaFree, ArenaPtr, BaseArenaPtr},
    },
    impl_arena,
    utils::{cfg::CfgNode, def_use::Usable},
};

/// The data of a block.
pub struct BlockData {
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
        ctx.alloc(BlockData {
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
        let value = ctx.alloc(ValueData::new_block_param(ty, self, idx));
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

    /// Remove a parameter of the block and free it.
    ///
    /// This will also modify the instructions that refer to this block, and
    /// update the uses of the passed argument.
    ///
    /// # Panics
    ///
    /// - Panics if the index is out of bounds.
    /// - Panics if any users still use the parameter.
    pub fn drop_param(self, ctx: &mut Context, idx: usize) {
        let param = self.deref(ctx).params[idx];
        if !param.users(ctx).is_empty() {
            panic!("cannot remove block parameter because it is still in use");
        }
        self.deref_mut(ctx).params.remove(idx);
        // check all the users of the block and remove the argument passing.
        // also need to maintain the uses of the passed argument.
        for inst in self.users(ctx) {
            if !inst.is_terminator(ctx) {
                unreachable!("block is used in a non-terminator instruction");
            }
            let mut args_to_modify = HashSet::new();

            for succ in inst.deref_mut(ctx).successors.iter_mut() {
                if succ.block.inner() == self {
                    let arg = succ.args.remove(&param).expect(
                        "block parameter is not passed to the block in the terminator instruction",
                    );
                    args_to_modify.insert(arg);
                }
            }
            // now update the uses of the passed argument
            for arg in args_to_modify {
                arg.drop(ctx);
            }
        }
        // now free the parameter
        param.drop(ctx);
    }

    /// Free the block from the context.
    ///
    /// This will also free all the parameters of the block.
    ///
    /// # Panics
    ///
    /// - Panics if the block is still in use, i.e., some instructions still
    ///  refer to this block and maybe this block still has predecessors.
    /// - Panics if the block is not unlink-ed.
    /// - Panics if any parameters are still in use.
    ///
    /// # See Also
    ///
    /// - [`Block::unlink`]
    /// - [`Block::remove`]
    /// - [`Block::drop_param`]
    pub fn drop(self, ctx: &mut Context) {
        if !self.users(ctx).is_empty() {
            panic!("cannot drop block because it is still in use");
        }
        if self.container(ctx).is_some() {
            panic!("cannot drop block because it is still linked");
        }
        for param in self.params(ctx) {
            if !param.users(ctx).is_empty() {
                panic!("cannot drop block because it has parameters that are still in use");
            }
        }

        let params = self.deref_mut(ctx).params.drain(..).collect::<Vec<_>>();
        for param in params {
            param.drop(ctx);
        }

        ctx.block_name_alloc.remove_by_ptr(self);
        ctx.free(self);
    }

    /// Unlink and drop the block.
    ///
    /// # Panics
    ///
    /// - Panics if the block is still in use.
    /// - Panics if any parameters are still in use.
    ///
    /// # See Also
    ///
    /// - [`Block::drop`]
    /// - [`Block::drop_param`]
    /// - [`Block::unlink`]
    pub fn remove(self, ctx: &mut Context) {
        self.unlink(ctx);
        self.drop(ctx);
    }

    /// Assign a name for the block.
    ///
    /// # Panics
    ///
    /// - Panics if the name is already assigned to another block.
    /// - Panics if the name is empty.
    /// - Panics if this block is already assigned a name.
    pub fn assign_name(self, ctx: &mut Context, name: String) {
        ctx.block_name_alloc.assign_name(self, name);
    }

    /// Allocate a name for the block
    ///
    /// # Parameters
    ///
    /// - `ctx`: The context.
    /// - `prefix`: The prefix of the name.
    ///
    /// # Panics
    ///
    /// - Panics if this block is already assigned a name.
    pub fn alloc_name(self, ctx: &mut Context, prefix: String) -> &String {
        ctx.block_name_alloc.alloc_name(self, prefix)
    }

    /// Get the name of the block.
    ///
    /// # Returns
    ///
    /// - `Some(name)`: The name of the block.
    /// - `None`: The block is not assigned/allocated a name yet.
    pub fn name(self, ctx: &Context) -> Option<&String> { ctx.block_name_alloc.get_name(self) }

    pub fn name_or_alloc(self, ctx: &mut Context, prefix: String) -> &String {
        if self.name(ctx).is_none() {
            self.alloc_name(ctx, prefix)
        } else {
            self.name(ctx).unwrap()
        }
    }

    pub fn comment(self, ctx: &mut Context, pos: CommentPos, content: String) {
        ctx.comment_info.comment_block(self, pos, content);
    }
}

impl CfgNode for Block {
    type Region = Func;

    fn succs(self, arena: &Self::A) -> Vec<Self> {
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

impl Usable for Block {
    type U = Inst;

    fn users(self, ctx: &Context) -> Vec<Inst> { self.deref(ctx).users.iter().copied().collect() }

    fn add_user(self, ctx: &mut Context, user: Inst) { self.deref_mut(ctx).users.insert(user); }

    fn remove_user(self, ctx: &mut Context, user: Inst) { self.deref_mut(ctx).users.remove(&user); }
}
