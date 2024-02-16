use super::{entities::FunctionData, module::Module, values::Function};

pub trait GlobalPass {
    fn run(&self, module: &mut Module);
}

pub trait LocalPass {
    fn run(&self, function: Function, data: &mut FunctionData);
}
