use crate::ir::module::Module;

use super::vm::VirtualMachine;


pub struct Interpreter<'a> {
    vm: VirtualMachine<'a>,
    module: &'a Module,
}

impl<'a> Interpreter<'a> {
    pub fn new(module: &'a Module) -> Self {
        let vm = VirtualMachine::new(module);
        Self { vm, module }
    }

    pub fn run(&mut self) {
        todo!()
    }
}