use thiserror::Error;

use super::{
    types::Type,
    values::{Block, Function, Value},
};

pub mod debugger;
pub mod vm;

#[derive(Debug, Error)]
pub enum ExecError {
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

pub struct ExecVReg {
    pub width: usize,
    pub data: Vec<u8>,
}

impl ExecVReg {
    pub fn zero(ty: Type) -> Self {
        let width = ty.bitwidth();
        let data = vec![0; width as usize];
        Self { width, data }
    }

    pub fn from_data(width: usize, data: Vec<u8>) -> Self { Self { width, data } }

    pub fn as_<T>(&self) -> T
    where
        T: Sized,
    {
        let mut data = self.data.clone();
        data.resize(std::mem::size_of::<T>(), 0);
        unsafe { std::ptr::read(data.as_ptr() as *const T) }
    }
}
