use std::hash::Hash;

use super::values::{Block, Function, Inst, Value};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Addr(u64);

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

    fn vreg(&self, value: Value) -> Result<VReg, ExecutionError>;
    fn slot(&self, value: Value) -> Result<Addr, ExecutionError>;

    fn memory(&self, addr: Addr) -> Result<&[u8], ExecutionError>;
}

pub trait ModifyExecutionState {
    fn set_curr_inst(&mut self, inst: Inst);
    fn set_curr_function(&mut self, function: Function);
    fn set_curr_block(&mut self, block: Block);

    fn set_vreg(&mut self, value: Value, vreg: VReg) -> Result<(), ExecutionError>;
    fn set_slot(&mut self, value: Value, addr: Addr) -> Result<(), ExecutionError>;
}

pub trait ExecuteOnInst: QueryExecutionState + ModifyExecutionState {
    fn exec_inst(&mut self) -> Result<(), ExecutionError>;
}
