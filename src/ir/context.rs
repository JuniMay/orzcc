use std::collections::HashMap;

use super::{
    debug::CommentInfo,
    name_alloc::NameAlloc,
    Block,
    BlockData,
    Func,
    FuncData,
    GlobalSlot,
    GlobalSlotData,
    InstData,
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
    pub(super) symbols: HashMap<Symbol, SymbolKind>,

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
            symbols: HashMap::new(),

            // +-----------------+
            // | name management |
            // +-----------------+
            value_name_alloc: NameAlloc::new(),
            block_name_alloc: NameAlloc::new(),

            // +-----------------+
            // | debug interface |
            // +-----------------+
            comment_info: CommentInfo::default(),
        }
    }
}

impl Context {
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
        let symbol: Symbol = func.name(self).into();
        if self.symbols.contains_key(&symbol) {
            panic!("symbol {:?} is already defined", symbol);
        }
        self.symbols.insert(symbol, SymbolKind::FuncDef(func));
    }

    pub(super) fn insert_global_slot(&mut self, slot: GlobalSlot) {
        let symbol: Symbol = slot.name(self).into();
        if self.symbols.contains_key(&symbol) {
            panic!("symbol {:?} is already defined", symbol);
        }
        self.symbols.insert(symbol, SymbolKind::GlobalSlot(slot));
    }

    /// Lookup a symbol in the context.
    ///
    /// # Returns
    ///
    /// - `Some(symbol_kind)` if the symbol exists.
    /// - `None` if the symbol is not defined.
    pub fn lookup_symbol(&mut self, symbol: impl Into<Symbol>) -> Option<&SymbolKind> {
        self.symbols.get(&symbol.into())
    }
}
