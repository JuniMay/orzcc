use thiserror::Error;

use super::{
    types::Type,
    values::{Block, Function, Value},
};

pub mod interpreter;
pub mod vm;

#[derive(Debug, Error)]
pub enum ExecErr {
    #[error("invalid segment")]
    InvalidSegement,

    #[error("invalid type {0}")]
    InvalidType(Type),

    #[error("invalid global init {0:?}")]
    InvalidGlobalInit(Value),

    #[error("invalid global item {0:?}")]
    InvalidGlobalItem(Value),

    #[error("value not found {0:?}")]
    ValueNotFound(Value),

    #[error("function not found {0:?}")]
    FunctionNotFound(Value),

    #[error("entry block not found {0:?}")]
    EntryBlockNotFound(Function),

    #[error("entry inst not found {0:?}")]
    EntryInstNotFound(Block),

    #[error("block not found {0:?}")]
    BlockNotFound(Block),

    #[error("early stop at {0:?}")]
    EarlyStop(Value),
}
