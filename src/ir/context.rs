use core::fmt;

use rustc_hash::FxHashMap;

use super::{
    debug::CommentInfo,
    name_alloc::NameAlloc,
    source_loc::Source,
    Block,
    BlockData,
    Func,
    FuncData,
    GlobalSlot,
    GlobalSlotData,
    InstData,
    Signature,
    Symbol,
    SymbolKind,
    TyData,
    Value,
    ValueData,
};
use crate::collections::storage::{BaseArena, UniqueArena};

/// The context of the IR.
///
/// A context can be understood as a container of all the data in the IR, or as
/// the state when creating/modifying the IR.
pub struct Context {
    // +-----------------+
    // |    storages     |
    // +-----------------+
    /// The unique storage of types.
    ///
    /// # Notes
    ///
    /// [ArenaFree](crate::collections::storage::ArenaFree) for types is not
    /// implemented for [Context], once a type is used, it should not be
    /// freed.
    pub(super) tys: UniqueArena<TyData>,
    /// The storage of blocks
    pub(super) blocks: BaseArena<BlockData>,
    /// The storage of instructions
    pub(super) insts: BaseArena<InstData>,
    /// The storage of values.
    pub(super) values: BaseArena<ValueData>,
    /// The storage of functions.
    pub(super) funcs: BaseArena<FuncData>,
    /// The storage of global slots
    pub(super) global_slots: BaseArena<GlobalSlotData>,
    /// The symbol defined in the context.
    pub(super) symbols: FxHashMap<Symbol, SymbolKind>,

    // +-----------------+
    // | name management |
    // +-----------------+
    /// The name of values.
    pub(super) value_name_alloc: NameAlloc<Value>,
    /// The name of blocks.
    pub(super) block_name_alloc: NameAlloc<Block>,

    // +-----------------+
    // | debug interface |
    // +-----------------+
    /// The comment information.
    ///
    /// This is used to add human-readable comment when emitting the IR.
    pub(super) comment_info: CommentInfo,

    // other information
    /// The source of the IR.
    pub(super) source: Source,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            // +-----------------+
            // |    storages     |
            // +-----------------+
            tys: UniqueArena::default(),
            blocks: BaseArena::default(),
            insts: BaseArena::default(),
            values: BaseArena::default(),
            funcs: BaseArena::default(),
            global_slots: BaseArena::default(),
            symbols: FxHashMap::default(),

            // +-----------------+
            // | name management |
            // +-----------------+
            value_name_alloc: NameAlloc::new(),
            block_name_alloc: NameAlloc::new(),

            // +-----------------+
            // | debug interface |
            // +-----------------+
            comment_info: CommentInfo::default(),

            source: Source::default(),
        }
    }
}

impl Context {
    pub fn new_in_memory(name: impl Into<String>) -> Self {
        Self {
            // +-----------------+
            // |    storages     |
            // +-----------------+
            tys: UniqueArena::default(),
            blocks: BaseArena::default(),
            insts: BaseArena::default(),
            values: BaseArena::default(),
            funcs: BaseArena::default(),
            global_slots: BaseArena::default(),
            symbols: FxHashMap::default(),

            // +-----------------+
            // | name management |
            // +-----------------+
            value_name_alloc: NameAlloc::new(),
            block_name_alloc: NameAlloc::new(),

            // +-----------------+
            // | debug interface |
            // +-----------------+
            comment_info: CommentInfo::default(),

            source: Source::in_memory(name),
        }
    }

    /// Insert function definition into the context and map the name.
    ///
    /// # Panics
    ///
    /// Panics if the symbol already exists, one can check if the symbol exists
    /// by calling [lookup_symbol](Self::lookup_symbol).
    ///
    /// # See Also
    ///
    /// - [lookup_symbol](Self::lookup_symbol)
    pub(super) fn insert_func(&mut self, func: Func) {
        let symbol: Symbol = func.name(self).clone();
        if self.symbols.contains_key(&symbol) {
            panic!("symbol {:?} is already defined", symbol);
        }
        self.symbols.insert(symbol, SymbolKind::FuncDef(func));
    }

    pub(super) fn insert_global_slot(&mut self, slot: GlobalSlot) {
        let symbol: Symbol = slot.name(self).clone();
        if self.symbols.contains_key(&symbol) {
            panic!("symbol {:?} is already defined", symbol);
        }
        self.symbols.insert(symbol, SymbolKind::GlobalSlot(slot));
    }

    pub fn add_func_decl(&mut self, symbol: impl Into<Symbol>, sig: Signature) {
        let symbol = symbol.into();

        if self.symbols.contains_key(&symbol) {
            panic!("symbol {:?} is already defined", symbol);
        }
        self.symbols.insert(symbol, SymbolKind::FuncDecl(sig));
    }

    pub fn source(&self) -> &Source { &self.source }

    /// Lookup a symbol in the context.
    ///
    /// # Returns
    ///
    /// - `Some(symbol_kind)` if the symbol exists.
    /// - `None` if the symbol is not defined.
    pub fn lookup_symbol(&self, symbol: &Symbol) -> Option<&SymbolKind> { self.symbols.get(symbol) }

    pub fn lookup_func(&self, symbol: &Symbol) -> Option<Func> {
        match self.lookup_symbol(symbol)? {
            SymbolKind::FuncDef(func) => Some(*func),
            SymbolKind::FuncDecl(_) | SymbolKind::GlobalSlot(_) => None,
        }
    }

    pub fn lookup_value(&self, name: &str) -> Option<Value> { self.value_name_alloc.get_ptr(name) }

    pub fn lookup_block(&self, name: &str) -> Option<Block> { self.block_name_alloc.get_ptr(name) }

    /// Allocate names for all the values and blocks in the context.
    ///
    /// Because we want to immutably emit the IR, we need to allocate names
    /// before doing the emission.
    pub fn alloc_all_names(&mut self) {
        let values: Vec<Value> = self
            .values
            .iter()
            .map(|(_, data)| data.self_ptr())
            .collect();

        for value in values {
            value.name_or_alloc(self, "v");
        }

        let blocks: Vec<Block> = self
            .blocks
            .iter()
            .map(|(_, data)| data.self_ptr())
            .collect();

        for block in blocks {
            block.name_or_alloc(self, "bb");
        }
    }

    pub fn funcs(&self) -> Vec<Func> {
        self.symbols
            .iter()
            .filter_map(|(_, symbol_kind)| match symbol_kind {
                SymbolKind::FuncDef(func) => Some(*func),
                SymbolKind::GlobalSlot(_) | SymbolKind::FuncDecl(_) => None,
            })
            .collect()
    }

    pub fn global_slots(&self) -> Vec<GlobalSlot> {
        self.symbols
            .iter()
            .filter_map(|(_, symbol_kind)| match symbol_kind {
                SymbolKind::GlobalSlot(slot) => Some(*slot),
                SymbolKind::FuncDef(_) | SymbolKind::FuncDecl(_) => None,
            })
            .collect()
    }

    pub fn decls(&self) -> Vec<(Symbol, Signature)> {
        self.symbols
            .iter()
            .filter_map(|(symbol, symbol_kind)| match symbol_kind {
                SymbolKind::FuncDecl(sig) => Some((symbol.clone(), sig.clone())),
                SymbolKind::GlobalSlot(_) | SymbolKind::FuncDef(_) => None,
            })
            .collect()
    }

    pub fn display(&self, debug: bool) -> DisplayContext<'_> {
        let mut slots = Vec::new();
        let mut decls = Vec::new();
        let mut funcs = Vec::new();

        for (symbol, symbol_kind) in &self.symbols {
            match symbol_kind {
                SymbolKind::GlobalSlot(_) => slots.push(symbol),
                SymbolKind::FuncDef(_) => funcs.push(symbol),
                SymbolKind::FuncDecl(_) => decls.push(symbol),
            }
        }

        DisplayContext {
            ctx: self,
            slots,
            decls,
            funcs,
            debug,
        }
    }
}

pub struct DisplayContext<'a> {
    ctx: &'a Context,
    slots: Vec<&'a Symbol>,
    decls: Vec<&'a Symbol>,
    funcs: Vec<&'a Symbol>,
    debug: bool,
}

impl fmt::Display for DisplayContext<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for symbol in self.slots.iter() {
            writeln!(f, "{}", symbol.display(self.ctx, self.debug))?;
        }

        for symbol in self.decls.iter() {
            writeln!(f, "{}", symbol.display(self.ctx, self.debug))?;
        }

        for symbol in self.funcs.iter() {
            writeln!(f, "{}", symbol.display(self.ctx, self.debug))?;
        }

        Ok(())
    }
}
