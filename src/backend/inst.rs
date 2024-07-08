use super::{
    block::MBlock,
    context::MContext,
    lower::{LowerConfig, MemLoc},
    regs::Reg,
    LowerSpec,
};
use crate::collections::{
    linked_list::LinkedListNodePtr,
    storage::{ArenaAlloc, ArenaDeref, ArenaFree, ArenaPtr, BaseArenaPtr},
};

pub trait MInst:
    ArenaPtr<A = MContext<Self>> + LinkedListNodePtr<ContainerPtr = MBlock<Self>>
{
    type S: LowerSpec<I = Self>;

    fn from_ptr(ptr: BaseArenaPtr<Self::T>) -> Self;

    fn ptr(self) -> BaseArenaPtr<Self::T>;

    fn uses(self, mctx: &MContext<Self>, config: &LowerConfig) -> Vec<Reg>;

    /// Return registers defined by this instruction.
    /// Note for call instruction, all return registers are defined.
    fn defs(self, mctx: &MContext<Self>, config: &LowerConfig) -> Vec<Reg>;

    /// Return registers that may be clobbered (edited) by this instruction.
    /// Note for call instruction, all caller-saved registers are clobbered.
    fn clobbers(self, mctx: &MContext<Self>, config: &LowerConfig) -> Vec<Reg>;

    /// Return all registers that are written in assembly of this instruction.
    fn all_regs(self, mctx: &MContext<Self>, config: &LowerConfig) -> Vec<Reg>;

    fn is_terminator(self, mctx: &MContext<Self>) -> bool;

    fn succs(self, mctx: &MContext<Self>) -> Vec<MBlock<Self>>;

    fn adjust_offset<F>(self, mctx: &mut MContext<Self>, f: F, config: &LowerConfig)
    where
        F: FnOnce(MemLoc) -> Option<MemLoc>;

    fn remove(self, mctx: &mut MContext<Self>) {
        self.unlink(mctx);
        mctx.free(self);
    }

    fn replace_reg(self, mctx: &mut MContext<Self>, from: Reg, to: Reg);
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