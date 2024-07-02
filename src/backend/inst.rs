use super::{block::MBlock, context::MContext, regs::Reg};
use crate::collections::{
    linked_list::LinkedListNodePtr,
    storage::{ArenaAlloc, ArenaDeref, ArenaFree, ArenaPtr, BaseArenaPtr},
};

pub trait MInst:
    ArenaPtr<A = MContext<Self>> + LinkedListNodePtr<ContainerPtr = MBlock<Self>>
{
    fn from_ptr(ptr: BaseArenaPtr<Self::T>) -> Self;

    fn ptr(&self) -> BaseArenaPtr<Self::T>;

    fn uses(&self, mctx: &MContext<Self>) -> Vec<Reg>;

    fn defs(&self, mctx: &MContext<Self>) -> Vec<Reg>;

    fn is_terminator(&self, mctx: &MContext<Self>) -> bool;

    fn succs(&self, mctx: &MContext<Self>) -> Vec<MBlock<Self>>;
}

pub trait DisplayMInst<'a>: MInst {
    type Display: std::fmt::Display + 'a;

    fn display(self, mctx: &'a MContext<Self>) -> Self::Display;
}

impl<I> ArenaAlloc<I::T, I> for MContext<I>
where
    I: MInst,
{
    fn alloc_with<F>(&mut self, f: F) -> I
    where
        F: FnOnce(I) -> I::T,
    {
        I::from_ptr(self.insts.alloc_with(|p| f(I::from_ptr(p))))
    }
}

impl<I> ArenaDeref<I::T, I> for MContext<I>
where
    I: MInst,
{
    fn try_deref(&self, ptr: I) -> Option<&I::T> { self.insts.try_deref(ptr.ptr()) }

    fn try_deref_mut(&mut self, ptr: I) -> Option<&mut I::T> { self.insts.try_deref_mut(ptr.ptr()) }
}

impl<I> ArenaFree<I::T, I> for MContext<I>
where
    I: MInst,
{
    fn free(&mut self, ptr: I) { self.insts.free(ptr.ptr()) }
}
