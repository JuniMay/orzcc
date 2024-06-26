mod block;
mod constant;
mod context;
mod fold;
mod global;
mod inst;
mod name_alloc;
mod ty;
mod value;

pub mod debug;
pub mod passman;
pub mod source_loc;
pub use block::{Block, BlockData};
pub use constant::Constant;
pub use context::Context;
pub use fold::FoldContext;
pub use global::{Func, FuncData, GlobalSlot, GlobalSlotData, Symbol, SymbolKind};
pub use inst::{
    CastOp,
    FBinaryOp,
    FCmpCond,
    FUnaryOp,
    IBinaryOp,
    ICmpCond,
    IUnaryOp,
    Inst,
    InstData,
    InstKind,
    Successor,
};
pub use ty::{Signature, Ty, TyData};
pub use value::{Value, ValueData, ValueKind};
