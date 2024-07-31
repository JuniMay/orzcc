use std::hash::Hash;

use super::{
    context::MContext,
    func::{MFunc, MLabel},
    inst::MInst,
};
use crate::{
    collections::{
        linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
        storage::{ArenaAlloc, ArenaDeref, ArenaFree, ArenaPtr, BaseArenaPtr},
    },
    utils::cfg::CfgNode,
};

pub struct MBlockData<I> {
    label: MLabel,

    head: Option<I>,
    tail: Option<I>,

    next: Option<MBlock<I>>,
    prev: Option<MBlock<I>>,

    parent: Option<MFunc<I>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MBlock<I>(BaseArenaPtr<MBlockData<I>>);

impl<I> MBlock<I>
where
    I: MInst,
{
    pub fn new(mctx: &mut MContext<I>, label: impl Into<MLabel>) -> Self {
        mctx.alloc(MBlockData {
            label: label.into(),
            head: None,
            tail: None,
            next: None,
            prev: None,
            parent: None,
        })
    }

    pub fn label(self, arena: &MContext<I>) -> &MLabel { &self.deref(arena).label }

    pub fn size(self, arena: &MContext<I>) -> usize {
        let mut size = 0;
        for _ in self.iter(arena) {
            size += 1;
        }
        size
    }

    pub fn remove(self, arena: &mut MContext<I>) {
        self.unlink(arena);
        arena.free(self);
    }
}

impl<I> Hash for MBlock<I> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.0.hash(state); }
}

impl<I> CfgNode for MBlock<I>
where
    I: MInst,
{
    type Region = MFunc<I>;

    fn succs(self, arena: &Self::A) -> Vec<Self> {
        let mut succs = Vec::new();
        for inst in self.iter(arena) {
            succs.extend(inst.succs(arena));
        }
        if succs.is_empty() {
            if let Some(next) = self.next(arena) {
                succs.push(next);
            }
        }
        succs
    }
}

impl<I> ArenaPtr for MBlock<I>
where
    I: MInst,
{
    type A = MContext<I>;
    type T = MBlockData<I>;

    fn try_deref(self, arena: &Self::A) -> Option<&Self::T> { ArenaDeref::try_deref(arena, self) }

    fn try_deref_mut(self, arena: &mut Self::A) -> Option<&mut Self::T> {
        ArenaDeref::try_deref_mut(arena, self)
    }
}

impl<I> ArenaAlloc<MBlockData<I>, MBlock<I>> for MContext<I>
where
    I: MInst,
{
    fn alloc_with<F>(&mut self, f: F) -> MBlock<I>
    where
        F: FnOnce(MBlock<I>) -> MBlockData<I>,
    {
        MBlock(self.blocks.alloc_with(|p| f(MBlock(p))))
    }
}

impl<I> ArenaDeref<MBlockData<I>, MBlock<I>> for MContext<I>
where
    I: MInst,
{
    fn try_deref(&self, ptr: MBlock<I>) -> Option<&MBlockData<I>> { self.blocks.try_deref(ptr.0) }

    fn try_deref_mut(&mut self, ptr: MBlock<I>) -> Option<&mut MBlockData<I>> {
        self.blocks.try_deref_mut(ptr.0)
    }
}

impl<I> ArenaFree<MBlockData<I>, MBlock<I>> for MContext<I>
where
    I: MInst,
{
    fn free(&mut self, ptr: MBlock<I>) { self.blocks.free(ptr.0) }
}

impl<I> LinkedListContainerPtr<I> for MBlock<I>
where
    I: MInst,
{
    fn head(self, arena: &Self::A) -> Option<I> { self.deref(arena).head }

    fn tail(self, arena: &Self::A) -> Option<I> { self.deref(arena).tail }

    fn set_head(self, arena: &mut Self::A, head: Option<I>) { self.deref_mut(arena).head = head; }

    fn set_tail(self, arena: &mut Self::A, tail: Option<I>) { self.deref_mut(arena).tail = tail; }
}

impl<I> LinkedListNodePtr for MBlock<I>
where
    I: MInst,
{
    type ContainerPtr = MFunc<I>;

    fn next(self, arena: &Self::A) -> Option<Self> { self.deref(arena).next }

    fn prev(self, arena: &Self::A) -> Option<Self> { self.deref(arena).prev }

    fn set_next(self, arena: &mut Self::A, next: Option<Self>) {
        self.deref_mut(arena).next = next;
    }

    fn set_prev(self, arena: &mut Self::A, prev: Option<Self>) {
        self.deref_mut(arena).prev = prev;
    }

    fn container(self, arena: &Self::A) -> Option<Self::ContainerPtr> { self.deref(arena).parent }

    fn set_container(self, arena: &mut Self::A, container: Option<Self::ContainerPtr>) {
        self.deref_mut(arena).parent = container;
    }
}
