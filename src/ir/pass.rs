use super::{entities::FunctionData, module::Module, values::Function};

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