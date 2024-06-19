use super::{Block, Signature};
use crate::{
    collections::{
        linked_list::LinkedListContainerPtr,
        storage::{ArenaAlloc, ArenaPtr, BaseArenaPtr},
    },
    impl_arena,
    ir::Context,
};

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct Symbol(String);

impl From<&str> for Symbol {
    fn from(s: &str) -> Self { Self(s.to_string()) }
}

impl From<String> for Symbol {
    fn from(s: String) -> Self { Self(s) }
}

pub struct FuncData {
    this: Func,
    name: Symbol,
    sig: Signature,
    /// The head block of the function, also the entry block in control flow.
    head: Option<Block>,
    /// The tail block of the function, not necessarily the exit block in
    /// control flow.
    tail: Option<Block>,
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Func(BaseArenaPtr<FuncData>);

impl_arena!(Context, FuncData, Func, funcs);

impl Func {
    pub fn new(ctx: &mut Context, name: impl Into<Symbol>, sig: Signature) -> Func {
        ctx.alloc_with(|this| FuncData {
            this,
            name: name.into(),
            sig,
            head: None,
            tail: None,
        })
    }

    pub fn name(self, ctx: &Context) -> &str { &self.deref(ctx).name.0 }

    pub fn sig(self, ctx: &Context) -> &Signature { &self.deref(ctx).sig }
}

impl LinkedListContainerPtr<Block> for Func {
    fn head(self, arena: &Self::A) -> Option<Block> { self.deref(arena).head }

    fn tail(self, arena: &Self::A) -> Option<Block> { self.deref(arena).tail }

    fn set_head(self, arena: &mut Self::A, head: Option<Block>) {
        self.deref_mut(arena).head = head;
    }

    fn set_tail(self, arena: &mut Self::A, tail: Option<Block>) {
        self.deref_mut(arena).tail = tail;
    }
}
