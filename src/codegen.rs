use std::collections::HashMap;

use crate::{
    backend::{
        BinaryImmOpKind,
        FloatBinaryFmt,
        FloatBinaryOpKind,
        FloatPseudoStoreKind,
        FloatStoreKind,
        InstData,
        MachineBlock,
        MachineContext,
        MachineFunctionData,
        MachineGlobalData,
        MachineInst,
        MachineSymbol,
        PseudoLoadKind,
        PseudoStoreKind,
        Register,
        RiscvFloatingPointRegister,
        RiscvGeneralPurposeRegister,
        StoreKind,
        VirtualRegister,
        VirtualRegisterKind,
    },
    codegen,
    ir::{
        entities::{FunctionKind, ValueKind},
        module::{DataFlowGraph, Module},
        values::{Alloc, Block, Function, Inst, Value},
    },
};

pub enum ValueCodegenResult {
    Register(Register),
    StackSlot { base: Register, offset: usize },
    MachineSymbol(MachineSymbol),
}

pub struct CodegenContext {
    machine_ctx: MachineContext,

    value_map: HashMap<Value, ValueCodegenResult>,
    block_map: HashMap<Block, MachineBlock>,
}

macro_rules! machine_function_layout {
    (mut $ctx:expr, $name:expr) => {
        $ctx.function_data_mut($name).unwrap().layout_mut()
    };
    ($ctx:expr, $name:expr) => {
        $ctx.function_data($name).unwrap().layout()
    };
}

macro_rules! machine_function {
    (mut $ctx:expr, $name:expr) => {
        $ctx.function_data_mut($name).unwrap()
    };
    ($ctx:expr, $name:expr) => {
        $ctx.function_data($name).unwrap()
    };
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

                    self.value_map
                        .insert(*global, ValueCodegenResult::MachineSymbol(machine_symbol));
                } else {
                    unreachable!("global slot value is not a global slot");
                }
            });
        }

        for function in module.function_layout() {
            let symbol: MachineSymbol = module.value_name((*function).into()).into();

            self.value_map.insert(
                (*function).into(),
                ValueCodegenResult::MachineSymbol(symbol.clone()),
            );

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

            machine_function_layout!(mut self.machine_ctx, &function_name)
                .append_block(machine_block)
                .unwrap();
        }

        let entry_block = function_data.layout().entry_block().unwrap();

        let block_args = function_data
            .dfg()
            .block_data(entry_block)
            .unwrap()
            .params();

        let mut integer_arg_count = 0;
        let mut float_arg_count = 0;
        let mut args_passed_by_stack = Vec::new();

        for arg in block_args {
            let value_data = function_data.dfg().local_value_data(*arg).unwrap();
            let ty = value_data.ty();

            if ty.is_float() {
                if float_arg_count <= 7 {
                    let machine_reg = self.machine_ctx.new_fp_reg(
                        (RiscvFloatingPointRegister::Fa0 as u8 + float_arg_count).into(),
                    );
                    let (rd, fmv) = InstData::new_float_binary(
                        &mut self.machine_ctx,
                        FloatBinaryOpKind::Fsgnj,
                        FloatBinaryFmt::S,
                        machine_reg,
                        machine_reg,
                    );

                    machine_function_layout!(mut self.machine_ctx, &function_name)
                        .append_inst(fmv, self.block_map[&entry_block])
                        .unwrap();

                    float_arg_count += 1;

                    self.value_map
                        .insert(*arg, ValueCodegenResult::Register(rd));
                } else {
                    args_passed_by_stack.push(*arg);
                }
            } else if ty.is_int() {
                if integer_arg_count <= 7 {
                    let machine_reg = self.machine_ctx.new_gp_reg(
                        (RiscvGeneralPurposeRegister::A0 as u8 + integer_arg_count).into(),
                    );
                    let (rd, mv) = InstData::new_binary_imm(
                        &mut self.machine_ctx,
                        BinaryImmOpKind::Addi,
                        machine_reg,
                        0.into(),
                    );

                    machine_function_layout!(mut self.machine_ctx, &function_name)
                        .append_inst(mv, self.block_map[&entry_block])
                        .unwrap();

                    integer_arg_count += 1;

                    self.value_map
                        .insert(*arg, ValueCodegenResult::Register(rd));
                } else {
                    args_passed_by_stack.push(*arg);
                }
            } else {
                unimplemented!("non-integer, non-float argument");
            }
        }

        let mut offset = 0;
        let fp = self.machine_ctx.new_gp_reg(RiscvGeneralPurposeRegister::S0);
        for arg in args_passed_by_stack {
            self.value_map
                .insert(arg, ValueCodegenResult::StackSlot { base: fp, offset });
            offset += 8;
        }

        for (block, block_node) in function_data.layout().blocks() {
            for (inst, _inst_node) in block_node.insts() {
                self.codegen_inst(module, inst, function, block);
            }
        }
    }

    pub fn get_value(&self, value: Value) -> &ValueCodegenResult {
        self.value_map.get(&value).unwrap()
    }

    pub fn codegen_function_prologue(&mut self, module: &Module, function: Function) { todo!() }

    pub fn codegen_function_epilogue(&mut self, module: &Module, function: Function) { todo!() }

    pub fn codegen_inst(
        &mut self,
        module: &Module,
        inst: Inst,
        function: Function,
        block: Block,
    ) -> MachineInst {
        let function_data = module.function_data(function).unwrap();
        let function_name: MachineSymbol = module.value_name(function.into()).into();

        let inst_data = function_data.dfg().local_value_data(inst.into()).unwrap();

        match inst_data.kind() {
            ValueKind::Alloc(alloc) => {
                let bytewidth = alloc.ty().bytewidth();
                self.value_map.insert(
                    inst.into(),
                    ValueCodegenResult::StackSlot {
                        base: self.machine_ctx.new_gp_reg(RiscvGeneralPurposeRegister::S0),
                        offset: machine_function!(mut self.machine_ctx, &function_name)
                            .stack_size(),
                    },
                );
                machine_function!(mut self.machine_ctx, &function_name).add_stack_size(bytewidth);
                // not appending here, this should be handled in the prologue
                // (and epilogue for deallocation)
            }
            ValueKind::Store(store) => {
                let val: Register = function_data
                    .dfg()
                    .with_value_data(store.val(), |data| match data.kind() {
                        ValueKind::Zero | ValueKind::Undef => self
                            .machine_ctx
                            .new_gp_reg(RiscvGeneralPurposeRegister::Zero),
                        ValueKind::GlobalSlot(_) | ValueKind::Function => {
                            let codegen_result = self.get_value(store.val());
                            let symbol =
                                if let ValueCodegenResult::MachineSymbol(symbol) = codegen_result {
                                    symbol.clone()
                                } else {
                                    unreachable!();
                                };
                            let (rd, la) = InstData::new_pseudo_load(
                                &mut self.machine_ctx,
                                PseudoLoadKind::Address,
                                symbol,
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(la, self.block_map[&block])
                                .unwrap();
                            rd
                        }
                        ValueKind::Array(_) | ValueKind::Struct(_) => unimplemented!(),
                        ValueKind::Bytes(bytes) => {
                            let imm = bytes.iter().fold(0, |acc, &byte| (acc << 8) | byte as u64);
                            let (rd, li) = InstData::new_li(&mut self.machine_ctx, imm.into());
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(li, self.block_map[&block])
                                .unwrap();
                            rd
                        }
                        ValueKind::Alloc(_) => {
                            let codegen_result = self.get_value(store.val());
                            let (base, offset) =
                                if let ValueCodegenResult::StackSlot { base, offset } =
                                    codegen_result
                                {
                                    (*base, *offset)
                                } else {
                                    unreachable!();
                                };
                            let (rd, addi) = InstData::new_binary_imm(
                                &mut self.machine_ctx,
                                BinaryImmOpKind::Addi,
                                base,
                                offset.into(),
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(addi, self.block_map[&block])
                                .unwrap();
                            rd
                        }
                        ValueKind::Store(_)
                        | ValueKind::Jump(_)
                        | ValueKind::Branch(_)
                        | ValueKind::Return(_) => unreachable!(),
                        ValueKind::Load(_)
                        | ValueKind::Binary(_)
                        | ValueKind::Unary(_)
                        | ValueKind::Call(_)
                        | ValueKind::GetElemPtr(_)
                        | ValueKind::Cast(_)
                        | ValueKind::BlockParam => {
                            let codegen_result = self.get_value(store.val());
                            if let ValueCodegenResult::Register(reg) = codegen_result {
                                *reg
                            } else {
                                unreachable!();
                            }
                        }
                    })
                    .unwrap();

                let ty = function_data
                    .dfg()
                    .with_value_data(store.ptr(), |data| data.ty())
                    .unwrap();

                function_data
                    .dfg()
                    .with_value_data(store.ptr(), |data| match data.kind() {
                        ValueKind::GlobalSlot(_) => {
                            let symbol = if let ValueCodegenResult::MachineSymbol(symbol) =
                                self.get_value(store.ptr())
                            {
                                symbol.clone()
                            } else {
                                unreachable!();
                            };
                            let rt = self
                                .machine_ctx
                                .new_virtual_reg(VirtualRegisterKind::General);

                            let store = if ty.is_float() {
                                InstData::new_float_pseudo_store(
                                    &mut self.machine_ctx,
                                    FloatPseudoStoreKind::Single,
                                    val,
                                    symbol,
                                    rt,
                                )
                            } else if ty.is_int() {
                                InstData::new_pseudo_store(
                                    &mut self.machine_ctx,
                                    PseudoStoreKind::Word,
                                    val,
                                    symbol,
                                    rt,
                                )
                            } else {
                                unreachable!()
                            };

                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(store, self.block_map[&block])
                                .unwrap();
                        }
                        ValueKind::Alloc(_) => {
                            let (base, offset) =
                                if let ValueCodegenResult::StackSlot { base, offset } =
                                    self.get_value(store.ptr())
                                {
                                    (*base, *offset)
                                } else {
                                    unreachable!();
                                };

                            let store = if ty.is_float() {
                                let kind = match ty.bytewidth() {
                                    4 => FloatStoreKind::Single,
                                    8 => FloatStoreKind::Double,
                                    _ => unimplemented!(),
                                };
                                InstData::new_float_store(
                                    &mut self.machine_ctx,
                                    kind,
                                    val,
                                    base,
                                    offset.into(),
                                )
                            } else if ty.is_int() {
                                let kind = match ty.bytewidth() {
                                    1 => StoreKind::Byte,
                                    2 => StoreKind::Half,
                                    4 => StoreKind::Word,
                                    8 => StoreKind::DoubleWord,
                                    _ => unimplemented!(),
                                };
                                InstData::new_store(
                                    &mut self.machine_ctx,
                                    kind,
                                    val,
                                    base,
                                    offset.into(),
                                )
                            } else {
                                unreachable!()
                            };
                        }
                        _ => todo!(),
                    });
            }
            _ => todo!(),
        }

        todo!()
    }
}
