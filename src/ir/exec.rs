use std::hash::Hash;

use super::{
    module::Module,
    values::{Block, Function, Inst, Value},
};

pub struct MemorySlot {
    pub(self) bytes: Vec<u8>,
}

impl MemorySlot {
    pub fn new(size: usize) -> Self {
        MemorySlot {
            bytes: vec![0; size],
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VReg(u64);

pub enum ExecutionError {}

pub trait Prepare {
    fn prepare(&mut self) -> Result<(), ExecutionError>;
}

pub trait QueryExecutionState {
    fn curr_inst(&self) -> Inst;
    fn curr_function(&self) -> Function;
    fn curr_block(&self) -> Block;

    fn module(&self) -> &Module;

    fn vreg(&self, value: Value) -> Result<VReg, ExecutionError>;
    fn slot(&self, value: Value) -> Result<&MemorySlot, ExecutionError>;
}

pub trait ModifyExecutionState {
    fn set_curr_inst(&mut self, inst: Inst);
    fn set_curr_function(&mut self, function: Function);
    fn set_curr_block(&mut self, block: Block);

    fn set_vreg(&mut self, value: Value, vreg: VReg) -> Result<(), ExecutionError>;
    fn set_slot(&mut self, value: Value, idx: usize, byte: u8) -> Result<(), ExecutionError>;
}

pub trait ExecuteOnInst: QueryExecutionState + ModifyExecutionState {
    fn exec_inst(&mut self) -> Result<(), ExecutionError>;
}

pub trait ExecuteOnBlock: ExecuteOnInst {
    fn exec_block(&mut self) -> Result<(), ExecutionError>;
}

pub trait ExecuteOnModule: ExecuteOnBlock {
    fn exec_module(&mut self) -> Result<(), ExecutionError>;
}
