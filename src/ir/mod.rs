mod block;
mod call_graph;
mod constant;
mod context;
mod fold;
mod global;
mod inst;
mod name_alloc;
mod source_loc;
mod ty;
mod value;

pub mod debug;
pub mod passes;
pub mod passman;
pub mod verify;
pub use block::{Block, BlockData};
pub use constant::{Constant, ConstantKind, FloatConstant};
pub use context::Context;
pub use fold::FoldContext;
pub use global::{Func, FuncData, GlobalSlot, GlobalSlotData, Symbol, SymbolKind};
pub use inst::{
    remove_all_insts,
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
pub use source_loc::Span;
pub use ty::{Signature, Ty, TyData};
pub use value::{Value, ValueData, ValueKind};
