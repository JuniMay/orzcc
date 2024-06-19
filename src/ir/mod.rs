mod block;
mod constant;
mod context;
mod fold;
mod global;
mod inst;
mod name_alloc;
mod ty;
mod value;

pub mod passman;

#[cfg(feature = "ir-frontend")]
pub mod frontend;

pub use block::{Block, BlockData};
pub use constant::Constant;
pub use context::Context;
pub use fold::FoldContext;
pub use global::{Func, FuncData, GlobalSlot, GlobalSlotData, Symbol, SymbolKind};
pub use inst::{BinaryOp, Inst, InstData, InstKind, Successor, UnaryOp};
pub use ty::{Signature, Ty, TyData};
pub use value::{Value, ValueData, ValueKind};
