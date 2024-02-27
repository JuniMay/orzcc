use super::{entities::FunctionData, module::Module, values::Function};

pub mod control_flow_analysis;
pub mod control_flow_normalization;
pub mod data_flow_analysis;
pub mod dominance_analysis;
pub mod mem2reg;
pub mod printer;
pub mod unreachable_block_elimination;

pub trait GlobalPass {
    type Ok;
    type Err;

    fn run(&mut self, module: &Module) -> Result<Self::Ok, Self::Err>;
}

pub trait LocalPass {
    type Ok;
    type Err;

    fn run(&mut self, function: Function, data: &FunctionData) -> Result<Self::Ok, Self::Err>;
}

pub trait LocalPassMut {
    type Ok;
    type Err;

    fn run(&mut self, function: Function, data: &mut FunctionData) -> Result<Self::Ok, Self::Err>;
}

pub trait GlobalPassMut {
    type Ok;
    type Err;

    fn run(&mut self, module: &mut Module) -> Result<Self::Ok, Self::Err>;
}
