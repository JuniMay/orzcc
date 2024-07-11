use core::fmt;
use std::{collections::HashSet, hash::Hash};

use super::{block::MBlock, context::MContext, inst::MInst, PReg};
use crate::{
    collections::{
        linked_list::LinkedListContainerPtr,
        storage::{ArenaAlloc, ArenaDeref, ArenaFree, ArenaPtr, BaseArenaPtr},
    },
    ir,
    utils::cfg::CfgRegion,
};

#[derive(Debug, Clone)]
pub struct MLabel(String);

impl fmt::Display for MLabel {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.0) }
}

impl PartialEq for MLabel {
    fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}

impl Eq for MLabel {}

impl Hash for MLabel {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.0.hash(state) }
}

impl<T> From<T> for MLabel
where
    T: AsRef<str>,
{
    fn from(value: T) -> Self { Self(value.as_ref().to_string()) }
}

/// Data of machine function.
pub struct MFuncData<I> {
    self_ptr: MFunc<I>,
    /// The label of the function
    label: MLabel,
    /// The stack size of local slots/variables.
    storage_stack_size: u64,
    /// The stack size for argument passing in call instructions within the
    /// function.
    outgoing_stack_size: u64,
    /// The saved registers
    saved_regs: HashSet<PReg>,
    /// The signature of the function
    sig: ir::Signature,

    /// If this function is an external function
    is_external: bool,

    head: Option<MBlock<I>>,
    tail: Option<MBlock<I>>,
}

impl<I> MFuncData<I> {
    pub fn self_ptr(&self) -> MFunc<I> { self.self_ptr }
}

#[derive(Debug, PartialEq, Eq)]
pub struct MFunc<I>(BaseArenaPtr<MFuncData<I>>);

impl<I> Clone for MFunc<I> {
    fn clone(&self) -> Self { *self }
}

impl<I> Copy for MFunc<I> {}

impl<I> Hash for MFunc<I> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.0.hash(state) }
}

impl<I> MFunc<I>
where
    I: MInst,
{
    pub fn label(self, arena: &MContext<I>) -> &MLabel { &self.deref(arena).label }

    pub fn new(mctx: &mut MContext<I>, label: impl Into<MLabel>, sig: ir::Signature) -> Self {
        mctx.alloc_with(|self_ptr| MFuncData {
            self_ptr,
            label: label.into(),
            storage_stack_size: 0,
            outgoing_stack_size: 0,
            saved_regs: HashSet::new(),
            sig,
            is_external: false,
            head: None,
            tail: None,
        })
    }

    pub fn new_external(
        mctx: &mut MContext<I>,
        label: impl Into<MLabel>,
        sig: ir::Signature,
    ) -> Self {
        mctx.alloc_with(|self_ptr| MFuncData {
            self_ptr,
            label: label.into(),
            storage_stack_size: 0,
            outgoing_stack_size: 0,
            saved_regs: HashSet::new(),
            sig,
            is_external: true,
            head: None,
            tail: None,
        })
    }

    pub fn sig(self, mctx: &MContext<I>) -> &ir::Signature { &self.deref(mctx).sig }

    pub fn is_external(self, mctx: &MContext<I>) -> bool { self.deref(mctx).is_external }

    pub fn add_storage_stack_size(self, mctx: &mut MContext<I>, size: u64) {
        self.deref_mut(mctx).storage_stack_size += size;
    }

    pub fn storage_stack_size(self, mctx: &MContext<I>) -> u64 {
        self.deref(mctx).storage_stack_size
    }

    pub fn outgoing_stack_size(self, mctx: &MContext<I>) -> u64 {
        self.deref(mctx).outgoing_stack_size
    }

    pub fn update_outgoing_stack_size(self, mctx: &mut MContext<I>, size: u64) {
        if size > self.outgoing_stack_size(mctx) {
            self.deref_mut(mctx).outgoing_stack_size = size;
        }
    }

    /// Get the outgoing stack size and set it to zero.
    pub fn take_outgoing_stack_size(self, mctx: &mut MContext<I>) -> u64 {
        let size = self.outgoing_stack_size(mctx);
        self.deref_mut(mctx).outgoing_stack_size = 0;
        size
    }

    pub fn saved_regs(self, mctx: &MContext<I>) -> Vec<PReg> {
        let mut regs: Vec<PReg> = self.deref(mctx).saved_regs.iter().copied().collect();
        // we need to sort the registers to make the order of saved registers
        // deterministic
        regs.sort();
        regs
    }

    pub fn add_saved_reg(self, mctx: &mut MContext<I>, reg: PReg) {
        self.deref_mut(mctx).saved_regs.insert(reg);
    }
}

impl<I> CfgRegion for MFunc<I>
where
    I: MInst,
{
    type Node = MBlock<I>;

    fn entry_node(self, arena: &Self::A) -> Self::Node {
        self.head(arena)
            .expect("entry block of machine function not found")
    }
}

impl<I> ArenaPtr for MFunc<I>
where
    I: MInst,
{
    type A = MContext<I>;
    type T = MFuncData<I>;

    fn try_deref(self, arena: &Self::A) -> Option<&Self::T> { ArenaDeref::try_deref(arena, self) }

    fn try_deref_mut(self, arena: &mut Self::A) -> Option<&mut Self::T> {
        ArenaDeref::try_deref_mut(arena, self)
    }
}

impl<I> ArenaAlloc<MFuncData<I>, MFunc<I>> for MContext<I>
where
    I: MInst,
{
    fn alloc_with<F>(&mut self, f: F) -> MFunc<I>
    where
        F: FnOnce(MFunc<I>) -> MFuncData<I>,
    {
        MFunc(self.funcs.alloc_with(|p| f(MFunc(p))))
    }
}

impl<I> ArenaDeref<MFuncData<I>, MFunc<I>> for MContext<I>
where
    I: MInst,
{
    fn try_deref(&self, ptr: MFunc<I>) -> Option<&MFuncData<I>> { self.funcs.try_deref(ptr.0) }

    fn try_deref_mut(&mut self, ptr: MFunc<I>) -> Option<&mut MFuncData<I>> {
        self.funcs.try_deref_mut(ptr.0)
    }
}

impl<I> ArenaFree<MFuncData<I>, MFunc<I>> for MContext<I>
where
    I: MInst,
{
    fn free(&mut self, ptr: MFunc<I>) { self.funcs.free(ptr.0) }
}

impl<I> LinkedListContainerPtr<MBlock<I>> for MFunc<I>
where
    I: MInst,
{
    fn head(self, arena: &Self::A) -> Option<MBlock<I>> { self.deref(arena).head }

    fn tail(self, arena: &Self::A) -> Option<MBlock<I>> { self.deref(arena).tail }

    fn set_head(self, arena: &mut Self::A, head: Option<MBlock<I>>) {
        self.deref_mut(arena).head = head;
    }

    fn set_tail(self, arena: &mut Self::A, tail: Option<MBlock<I>>) {
        self.deref_mut(arena).tail = tail;
    }
}
