use super::{entities::FunctionData, module::Module, values::Function};

pub trait GlobalPass {
    fn run(&mut self, module: &mut Module);
}

pub trait LocalPass {
    fn run(&mut self, function: Function, data: &mut FunctionData);
}
