use std::collections::HashMap;

use crate::{
    backend::{
        MachineBlock,
        MachineContext,
        MachineGlobalData,
        MachineInst,
        MachineSymbol,
        VirtualRegister,
    },
    ir::{
        entities::{FunctionKind, ValueKind},
        module::Module,
        values::{Block, Function, Inst, Value},
    },
};

pub struct CodegenContext {
    machine_ctx: MachineContext,

    value_map: HashMap<Value, VirtualRegister>,
    block_map: HashMap<Block, MachineBlock>,
    global_map: HashMap<Value, MachineSymbol>,
}

impl CodegenContext {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Self {
            machine_ctx: MachineContext::new(),
            value_map: HashMap::new(),
            block_map: HashMap::new(),
            global_map: HashMap::new(),
        }
    }

    pub fn get_virtual_register(&mut self, value: Value) -> &VirtualRegister {
        self.value_map
            .entry(value)
            .or_insert_with(|| self.machine_ctx.new_vreg().as_virtual_register().unwrap())
    }

    pub fn codegen(&mut self, module: &Module) {
        for global in module.global_slot_layout() {
            let machine_symbol: MachineSymbol = module.value_name(*global).into();
            module.with_value_data(*global, |data| {
                if let ValueKind::GlobalSlot(slot) = data.kind() {
                    let init = slot.init();

                    let machine_global = module
                        .with_value_data(init, |data| {
                            let size = data.ty().bytewidth();
                            match data.kind() {
                                ValueKind::Zero | ValueKind::Undef => {
                                    MachineGlobalData::new_bss(size, 2)
                                }
                                ValueKind::Bytes(bytes) => {
                                    MachineGlobalData::new_data(bytes.clone(), 2)
                                }
                                ValueKind::Array(_) => {
                                    let mut bytes: Vec<u8> = Vec::new();
                                    fn rec(module: &Module, value: Value, bytes: &mut Vec<u8>) {
                                        module.with_value_data(value, |data| match data.kind() {
                                            ValueKind::Zero => {
                                                bytes.extend(
                                                    std::iter::repeat(0)
                                                        .take(data.ty().bytewidth()),
                                                );
                                            }
                                            ValueKind::Undef => {
                                                bytes.extend(
                                                    std::iter::repeat(0)
                                                        .take(data.ty().bytewidth()),
                                                );
                                            }
                                            ValueKind::Bytes(inner) => {
                                                assert_eq!(
                                                    inner.len(),
                                                    data.ty().bytewidth(),
                                                    "byte array length mismatch"
                                                );
                                                bytes.extend(inner.clone());
                                            }
                                            ValueKind::Array(elems) => {
                                                for elem in elems {
                                                    rec(module, *elem, bytes);
                                                }
                                            }
                                            ValueKind::Struct(_) => {
                                                unimplemented!("struct in array");
                                            }
                                            _ => {
                                                unreachable!("global slot value is not a constant")
                                            }
                                        });
                                    }
                                    rec(module, init, &mut bytes);
                                    MachineGlobalData::new_data(bytes, 2)
                                }
                                ValueKind::Struct(_) => {
                                    unimplemented!("struct in global slot");
                                }
                                _ => unreachable!("global slot value is not a constant"),
                            }
                        })
                        .expect("init value not found");

                    self.machine_ctx
                        .new_global(machine_symbol.clone(), machine_global);
                    self.global_map.insert(*global, machine_symbol);
                } else {
                    unreachable!("global slot value is not a global slot");
                }
            });
        }

        for function in module.function_layout() {
            if let FunctionKind::Definition = module.function_data(*function).unwrap().kind() {
                self.codegen_function(module, *function);
            }
        }
    }

    pub fn codegen_function(&mut self, module: &Module, function: Function) {
        let function_data = module.function_data(function).unwrap();
        let function_name: MachineSymbol = module.value_name(function.into()).into();

        self.machine_ctx.new_function(function_name.clone());

        for (block, _) in function_data.layout().blocks() {
            let machine_block = self.machine_ctx.new_block();
            self.block_map.insert(block, machine_block);
            self.machine_ctx
                .function_data_mut(&function_name)
                .unwrap()
                .layout_mut()
                .append_block(machine_block)
                .unwrap();
        }
    }

    pub fn codegen_function_prologue(&mut self, module: &Module, function: Function) { todo!() }

    pub fn codegen_function_epilogue(&mut self, module: &Module, function: Function) { todo!() }

    pub fn codegen_block(&mut self, module: &Module, block: Block) -> MachineBlock { todo!() }

    pub fn codegen_inst(&mut self, module: &Module, inst: Inst) -> MachineInst { todo!() }
}
