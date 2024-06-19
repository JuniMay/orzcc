use super::{Block, Constant, Context, Signature, Ty};
use crate::{
    collections::{
        linked_list::LinkedListContainerPtr,
        storage::{ArenaAlloc, ArenaPtr, BaseArenaPtr},
    },
    impl_arena,
    utils::cfg::CfgRegion,
};

#[derive(Debug, Hash, Clone, PartialEq, Eq)]
pub struct Symbol(String);

impl From<&str> for Symbol {
    fn from(s: &str) -> Self { Self(s.to_string()) }
}

impl From<String> for Symbol {
    fn from(s: String) -> Self { Self(s) }
}

/// The data of a function.
///
/// Cranelift and the old version of OrzIR uses data flow graph to represent
/// local variables and blocks, but it turns out that complicates the design
/// and makes the passes harder to implement. So no separate data flow graph
/// is used in the new version of OrzIR, everything is stored in [Context].
/// The local layout is represented with intrusive linked list of each entity.
///
/// Also, function declaration and definition are two different types in the
/// framework, so one can distinguish them in compile time.
pub struct FuncData {
    /// The name of the function.
    name: Symbol,
    /// The signature of the function.
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
        let func = ctx.alloc(FuncData {
            name: name.into(),
            sig,
            head: None,
            tail: None,
        });
        // establish the mapping from name to function
        ctx.insert_func(func);
        func
    }

    pub fn name(self, ctx: &Context) -> &str { &self.deref(ctx).name.0 }

    pub fn sig(self, ctx: &Context) -> &Signature { &self.deref(ctx).sig }
}

impl CfgRegion for Func {
    type Node = Block;

    fn entry_node(self, arena: &Self::A) -> Self::Node {
        self.head(arena).expect("entry block of function not found")
    }
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

/// Global memory slot.
pub struct GlobalSlotData {
    name: Symbol,
    ty: Ty,
    init: Option<Constant>,
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct GlobalSlot(BaseArenaPtr<GlobalSlotData>);

impl_arena!(Context, GlobalSlotData, GlobalSlot, global_slots);

impl GlobalSlot {
    pub fn new(ctx: &mut Context, name: impl Into<Symbol>, ty: Ty) -> GlobalSlot {
        let slot = ctx.alloc(GlobalSlotData {
            name: name.into(),
            ty,
            init: None,
        });
        ctx.insert_global_slot(slot);
        slot
    }

    pub fn name(self, ctx: &Context) -> &str { &self.deref(ctx).name.0 }

    pub fn ty(self, ctx: &Context) -> Ty { self.deref(ctx).ty }

    pub fn init(self, ctx: &Context) -> Option<&Constant> { self.deref(ctx).init.as_ref() }
}

/// The entity that a symbol defines.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SymbolKind {
    /// A function definition.
    FuncDef(Func),
    /// A global memory slot.
    GlobalSlot(GlobalSlot),
    /// A function declaration.
    FuncDecl(Signature),
}
