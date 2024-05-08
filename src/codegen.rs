use std::collections::HashMap;

use crate::{
    backend::{MachineBlock, MachineContext, MachineGlobalData, MachineSymbol, VirtualRegister},
    ir::{
        entities::ValueKind,
        module::Module,
        values::{Block, Function, Inst, Value},
    },
};

pub struct CodegenContext {
    machine_ctx: MachineContext,

    value_map: HashMap<Value, VirtualRegister>,
    block_map: HashMap<Block, MachineBlock>,
}

impl CodegenContext {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            machine_ctx: MachineContext::new(),
            value_map: HashMap::new(),
            block_map: HashMap::new(),
        }
    }

    pub fn get_virtual_register(&self, value: Value) -> Option<&VirtualRegister> {
        self.value_map.get(&value)
    }

    pub fn codegen(&mut self, module: &Module) {
        for global in module.global_slot_layout() {
            let name = module.value_name(*global);
            module.with_value_data(*global, |data| todo!());
        }

        todo!()
    }

    pub fn codegen_function(&mut self, module: &Module, function: Function) {
        todo!()
    }

    pub fn codegen_function_prologue(&mut self, module: &Module, function: Function) {
        todo!()
    }

    pub fn codegen_function_epilogue(&mut self, module: &Module, function: Function) {
        todo!()
    }

    pub fn codegen_block(&mut self, module: &Module, block: Block) {
        todo!()
    }

    pub fn codegen_inst(&mut self, module: &Module, inst: Inst) {
        todo!()
    }

    pub fn codegen_value(&mut self, module: &Module, value: Value) {
        todo!()
    }
}
