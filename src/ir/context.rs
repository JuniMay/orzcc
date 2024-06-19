use super::{inst::InstData, BlockData, FuncData, ValueData};
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
    /// [ArenaLikeFree] for types is not implemented for [Context], once a type
    /// is used, it should not be freed.
    pub(in crate::ir) tys: UniqueArena<TyData>,
    /// The storage of blocks
    pub(in crate::ir) blocks: BaseArena<BlockData>,
    /// The storage of instructions
    pub(in crate::ir) insts: BaseArena<InstData>,
    /// The storage of values.
    pub(in crate::ir) values: BaseArena<ValueData>,
    /// The storage of functions.
    pub(in crate::ir) funcs: BaseArena<FuncData>,
}
