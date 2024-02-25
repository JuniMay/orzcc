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

    fn summary(&self) {
        println!("EXECUTION SUMMARY ======================");
        for function in self.module.function_layout() {
            let function_data = self
                .module
                .function_data(*function)
                .expect("function should exist");
            println!("Function: {}", function_data.name());
            let dfg = function_data.dfg();

            for (value, _data) in dfg.values() {
                let name = dfg.value_name(*value);
                let vreg = self.vm.read_vreg(*value);

                println!("\t{:10} = {:?}", name, vreg);
            }
        }
        println!("========================================");
    }

    pub fn run(&mut self) {
        self.vm.prepare("@main").ok();

        loop {
            let stop = self.vm.step().unwrap_or_else(|e| {
                println!("Error: {}", e);
                true
            });

            if stop {
                break;
            }
        }

        self.summary();
    }
}
