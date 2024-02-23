use std::hash::Hash;

use super::{
    module::Module,
    values::{Block, Function, Inst, Value},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Addr(u64);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VReg(u64);

pub trait ExecutionError {
    fn message(&self) -> String;
}

pub type ExecutionResult<T> = Result<T, Box<dyn ExecutionError>>;

pub trait Prepare {
    fn prepare(&mut self) -> ExecutionResult<()>;
}

pub trait QueryExecutionState {
    fn curr_inst(&self) -> Inst;
    fn curr_function(&self) -> Function;
    fn curr_block(&self) -> Block;

    fn module(&self) -> &Module;

    fn vreg(&self, value: Value) -> ExecutionResult<VReg>;
    fn slot(&self, value: Value) -> ExecutionResult<Addr>;

    fn memory(&self, addr: Addr) -> ExecutionResult<&[u8]>;
}

pub trait ModifyExecutionState {
    fn set_curr_inst(&mut self, inst: Inst);
    fn set_curr_function(&mut self, function: Function);
    fn set_curr_block(&mut self, block: Block);

    fn set_vreg(&mut self, value: Value, vreg: VReg) -> ExecutionResult<()>;
    fn set_slot(&mut self, value: Value, addr: Addr) -> ExecutionResult<()>;
}

pub trait ExecuteOnInst: QueryExecutionState + ModifyExecutionState {
    fn exec_inst(&mut self) -> ExecutionResult<()>;
}
