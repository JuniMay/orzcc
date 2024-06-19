use std::collections::HashMap;

use super::{
    global::{GlobalSlot, GlobalSlotData, SymbolKind},
    inst::InstData,
    BlockData,
    Func,
    FuncData,
    Symbol,
    ValueData,
};
use crate::{
    collections::storage::{BaseArena, UniqueArena},
    ir::TyData,
};

/// The context of the IR.
///
/// A context can be understood as a container of all the data in the IR, or as
/// the state when creating/modifying the IR.
#[derive(Default)]
pub struct Context {
    /// The unique storage of types.
    ///
    /// # Notes
    ///
    /// [ArenaFree](crate::collections::storage::ArenaFree) for types is not
    /// implemented for [Context], once a type is used, it should not be
    /// freed.
    pub(in crate::ir) tys: UniqueArena<TyData>,
    /// The storage of blocks
    pub(in crate::ir) blocks: BaseArena<BlockData>,
    /// The storage of instructions
    pub(in crate::ir) insts: BaseArena<InstData>,
    /// The storage of values.
    pub(in crate::ir) values: BaseArena<ValueData>,
    /// The storage of functions.
    pub(in crate::ir) funcs: BaseArena<FuncData>,
    /// The storage of global slots
    pub(in crate::ir) global_slots: BaseArena<GlobalSlotData>,
    /// The symbol defined in the context.
    pub(in crate::ir) symbols: HashMap<Symbol, SymbolKind>,
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
    pub(in crate::ir) fn insert_func(&mut self, func: Func) {
        let symbol: Symbol = func.name(self).into();
        if self.symbols.contains_key(&symbol) {
            panic!("symbol {:?} is already defined", symbol);
        }
        self.symbols.insert(symbol, SymbolKind::FuncDef(func));
    }

    pub(in crate::ir) fn insert_global_slot(&mut self, slot: GlobalSlot) {
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
