use std::{fmt, hash::Hash};

use super::{debug::CommentPos, source_loc::Span, Block, Constant, Context, Signature, Ty};
use crate::{
    collections::{
        linked_list::LinkedListContainerPtr,
        storage::{ArenaAlloc, ArenaPtr, BaseArenaPtr},
    },
    impl_arena,
    utils::cfg::CfgRegion,
};

#[derive(Debug, Clone)]
pub struct Symbol(String, Span);

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}

impl Eq for Symbol {}

impl Hash for Symbol {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.0.hash(state) }
}

impl<T> From<T> for Symbol
where
    T: AsRef<str>,
{
    fn from(value: T) -> Self { Self(value.as_ref().to_string(), Span::default()) }
}

impl Symbol {
    pub fn new(name: impl Into<String>) -> Symbol { Symbol(name.into(), Span::default()) }

    pub fn comment(&self, ctx: &mut Context, pos: CommentPos, content: impl Into<String>) {
        ctx.comment_info
            .comment_symbol(self.clone(), pos, content.into());
    }

    pub fn with_source_span(mut self, span: Span) -> Self {
        self.1 = span;
        self
    }

    pub fn source_span(&self) -> Span { self.1 }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.0) }
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
    self_ptr: Func,
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

impl FuncData {
    pub fn self_ptr(&self) -> Func { self.self_ptr }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Func(BaseArenaPtr<FuncData>);

impl_arena!(Context, FuncData, Func, funcs);

impl Func {
    pub fn new(ctx: &mut Context, name: impl Into<Symbol>, sig: Signature) -> Func {
        let func = ctx.alloc_with(|self_ptr| FuncData {
            self_ptr,
            name: name.into(),
            sig,
            head: None,
            tail: None,
        });
        // establish the mapping from name to function
        ctx.insert_func(func);
        func
    }

    pub fn name(self, ctx: &Context) -> &Symbol { &self.deref(ctx).name }

    pub fn sig(self, ctx: &Context) -> &Signature { &self.deref(ctx).sig }

    pub fn display<'a>(&'a self, ctx: &'a Context, debug: bool) -> DisplayFunc<'a> {
        DisplayFunc {
            ctx,
            data: self.deref(ctx),
            debug,
        }
    }

    pub fn id(self) -> usize { self.0.id() }

    pub fn comment(&self, ctx: &mut Context, pos: CommentPos, content: impl Into<String>) {
        self.deref(ctx).name.clone().comment(ctx, pos, content);
    }
}

impl CfgRegion for Func {
    type Node = Block;

    fn entry_node(self, arena: &Self::A) -> Self::Node {
        self.head(arena).expect("entry block of function not found")
    }
}

pub struct DisplayFunc<'a> {
    ctx: &'a Context,
    data: &'a FuncData,
    debug: bool,
}

impl fmt::Display for DisplayFunc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "func @{}", self.data.name)?;

        if self.debug {
            write!(f, " /* {} */ ", self.data.self_ptr().id())?;
        }

        writeln!(f, "{} {{", self.data.sig.display(self.ctx))?;

        for block in self.data.self_ptr().iter(self.ctx) {
            writeln!(f, "{}", block.display(self.ctx, self.debug))?;
        }

        write!(f, "}}")
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
    self_ptr: GlobalSlot,
    name: Symbol,
    ty: Ty,
    init: Constant,
}

impl GlobalSlotData {
    pub fn self_ptr(&self) -> GlobalSlot { self.self_ptr }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct GlobalSlot(BaseArenaPtr<GlobalSlotData>);

impl_arena!(Context, GlobalSlotData, GlobalSlot, global_slots);

impl GlobalSlot {
    pub fn new(
        ctx: &mut Context,
        name: impl Into<Symbol>,
        ty: Ty,
        init: impl Into<Constant>,
    ) -> GlobalSlot {
        let slot = ctx.alloc_with(|self_ptr| GlobalSlotData {
            self_ptr,
            name: name.into(),
            ty,
            init: init.into(),
        });
        ctx.insert_global_slot(slot);
        slot
    }

    pub fn name(self, ctx: &Context) -> &str { &self.deref(ctx).name.0 }

    pub fn ty(self, ctx: &Context) -> Ty { self.deref(ctx).ty }

    pub fn init(self, ctx: &Context) -> &Constant { &self.deref(ctx).init }

    pub fn id(self) -> usize { self.0.id() }

    pub fn display(self, ctx: &Context, debug: bool) -> DisplayGlobalSlot<'_> {
        DisplayGlobalSlot {
            ctx,
            data: self.deref(ctx),
            debug,
        }
    }

    pub fn comment(&self, ctx: &mut Context, pos: CommentPos, content: impl Into<String>) {
        self.deref(ctx).name.clone().comment(ctx, pos, content);
    }
}

pub struct DisplayGlobalSlot<'a> {
    ctx: &'a Context,
    data: &'a GlobalSlotData,
    debug: bool,
}

impl fmt::Display for DisplayGlobalSlot<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "slot @{}", self.data.name)?;

        if self.debug {
            write!(f, " /* {} */ ", self.data.self_ptr().id())?;
        }

        write!(
            f,
            " : {} = {}",
            self.data.ty.display(self.ctx),
            self.data.init
        )
    }
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

impl Symbol {
    pub fn display<'a>(&'a self, ctx: &'a Context, debug: bool) -> DisplaySymbol<'a> {
        DisplaySymbol {
            ctx,
            symbol: self,
            kind: ctx.lookup_symbol(self).unwrap(),
            debug,
        }
    }
}

pub struct DisplaySymbol<'a> {
    ctx: &'a Context,
    symbol: &'a Symbol,
    kind: &'a SymbolKind,
    debug: bool,
}

impl fmt::Display for DisplaySymbol<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut end_comments = Vec::new();
        let mut after_comments = Vec::new();

        // check comments in the context
        if let Some(comments) = self.ctx.comment_info.get_symbol_comments(self.symbol) {
            for (pos, content) in comments {
                match pos {
                    CommentPos::Before => {
                        writeln!(f, "// {}", content)?;
                    }
                    CommentPos::AtEnd => {
                        end_comments.push(content);
                    }
                    CommentPos::After => {
                        after_comments.push(content);
                    }
                }
            }
        }

        match self.kind {
            SymbolKind::FuncDef(func) => {
                write!(f, "{}", func.display(self.ctx, self.debug))?;
            }
            SymbolKind::GlobalSlot(slot) => {
                write!(f, "{}", slot.display(self.ctx, self.debug))?;
            }
            SymbolKind::FuncDecl(sig) => {
                write!(f, "decl @{}{}", self.symbol, sig.display(self.ctx))?;
            }
        }

        for comment in end_comments.iter() {
            write!(f, " /* {} */", comment)?;
        }

        if !end_comments.is_empty() {
            writeln!(f)?;
        }

        for comment in after_comments.iter() {
            writeln!(f, "// {}", comment)?;
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::Symbol;
    use crate::ir::source_loc::Span;

    #[test]
    fn test_symbol_equality() {
        let sym1 = Symbol::new("symbol").with_source_span(Span::new(1.into(), 6.into()));
        let sym2 = Symbol::new("symbol").with_source_span(Span::new(7.into(), 12.into()));

        assert_ne!(sym1.source_span(), sym2.source_span());
        assert_eq!(sym1, sym2);
    }
}
