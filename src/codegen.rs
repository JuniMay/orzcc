use std::collections::HashMap;

use crate::{
    backend::{
        FMvFmt,
        FloatLoadKind,
        FloatPseudoLoadKind,
        FloatPseudoStoreKind,
        FloatStoreKind,
        Immediate,
        InstData,
        LoadKind,
        MachineBinaryImmOp,
        MachineBinaryOp,
        MachineBlock,
        MachineBranchOp,
        MachineContext,
        MachineFloatBinaryFmt,
        MachineFloatBinaryOp,
        MachineGlobalData,
        MachineInst,
        MachineSymbol,
        PseudoLoadKind,
        PseudoStoreKind,
        Register,
        RiscvFloatingPointRegister,
        RiscvGeneralPurposeRegister,
        StoreKind,
        VirtualRegisterKind,
    },
    ir::{
        entities::{FunctionKind, ValueKind},
        module::Module,
        types::TypeKind,
        values::{BinaryOp, Block, CastOp, FCmpCond, Function, ICmpCond, Inst, UnaryOp, Value},
    },
};

pub enum ValueCodegenResult {
    Register(Register),
    StackSlot { base: Register, offset: Immediate },
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

        // assign register for all block arguments for argument passing in branch/jump
        // instructions.
        for block in function_data.layout().blocks() {
            let block_args = function_data.dfg().block_data(block.0).unwrap().params();
            for arg in block_args {
                let arg_data = function_data.dfg().local_value_data(*arg).unwrap();

                if arg_data.ty().is_float() {
                    let reg = self
                        .machine_ctx
                        .new_virtual_reg(VirtualRegisterKind::FloatingPoint);
                    self.value_map
                        .insert(*arg, ValueCodegenResult::Register(reg));
                } else if arg_data.ty().is_int() || arg_data.ty().is_ptr() {
                    let reg = self
                        .machine_ctx
                        .new_virtual_reg(VirtualRegisterKind::General);
                    self.value_map
                        .insert(*arg, ValueCodegenResult::Register(reg));
                }
            }
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
                    let rd = if let ValueCodegenResult::Register(rd) = self.get_value(*arg) {
                        *rd
                    } else {
                        unreachable!();
                    };
                    let rs = self.machine_ctx.new_fp_reg(
                        (RiscvFloatingPointRegister::Fa0 as u8 + float_arg_count).into(),
                    );
                    let fmv = InstData::build_fp_move(&mut self.machine_ctx, rd, rs);

                    machine_function_layout!(mut self.machine_ctx, &function_name)
                        .append_inst(fmv, self.block_map[&entry_block])
                        .unwrap();

                    float_arg_count += 1;
                } else {
                    args_passed_by_stack.push(*arg);
                }
            } else if ty.is_int() {
                if integer_arg_count <= 7 {
                    let rd = if let ValueCodegenResult::Register(rd) = self.get_value(*arg) {
                        *rd
                    } else {
                        unreachable!();
                    };

                    let rs = self.machine_ctx.new_gp_reg(
                        (RiscvGeneralPurposeRegister::A0 as u8 + integer_arg_count).into(),
                    );

                    let mv = InstData::build_gp_move(&mut self.machine_ctx, rd, rs);

                    machine_function_layout!(mut self.machine_ctx, &function_name)
                        .append_inst(mv, self.block_map[&entry_block])
                        .unwrap();

                    integer_arg_count += 1;
                } else {
                    args_passed_by_stack.push(*arg);
                }
            } else {
                unimplemented!("non-integer, non-float argument");
            }
        }

        let mut offset = 0;
        let fp = self.machine_ctx.new_gp_reg(RiscvGeneralPurposeRegister::S0);
        // load the rest argument into the registers.
        // the loaded value might be spilled again, but that is a problem for register
        // allocation.
        for arg in args_passed_by_stack {
            let arg_data = function_data.dfg().local_value_data(arg).unwrap();
            let rd = if let ValueCodegenResult::Register(rd) = self.get_value(arg) {
                *rd
            } else {
                unreachable!();
            };

            if arg_data.ty().is_float() {
                let kind = match arg_data.ty().bytewidth() {
                    4 => FloatLoadKind::Single,
                    8 => FloatLoadKind::Double,
                    _ => unimplemented!(),
                };
                let load =
                    InstData::build_float_load(&mut self.machine_ctx, kind, rd, fp, offset.into());
                machine_function_layout!(mut self.machine_ctx, &function_name)
                    .append_inst(load, self.block_map[&entry_block])
                    .unwrap();
            } else if arg_data.ty().is_int() || arg_data.ty().is_ptr() {
                let kind = match arg_data.ty().bytewidth() {
                    1 => LoadKind::Byte,
                    2 => LoadKind::Half,
                    4 => LoadKind::Word,
                    8 => LoadKind::DoubleWord,
                    _ => unimplemented!(),
                };
                let load = InstData::build_load(&mut self.machine_ctx, kind, rd, fp, offset.into());
                machine_function_layout!(mut self.machine_ctx, &function_name)
                    .append_inst(load, self.block_map[&entry_block])
                    .unwrap();
            } else {
                unimplemented!("non-integer, non-float argument");
            }

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
                machine_function!(mut self.machine_ctx, &function_name).add_stack_size(bytewidth);
                let offset =
                    -(machine_function!(mut self.machine_ctx, &function_name).stack_size() as i128);
                self.value_map.insert(
                    inst.into(),
                    ValueCodegenResult::StackSlot {
                        base: self.machine_ctx.new_gp_reg(RiscvGeneralPurposeRegister::S0),
                        offset: offset.into(),
                    },
                );
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
                                MachineBinaryImmOp::Addi,
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
                                let kind = match ty.bytewidth() {
                                    4 => FloatPseudoStoreKind::Single,
                                    _ => unimplemented!(),
                                };
                                InstData::new_float_pseudo_store(
                                    &mut self.machine_ctx,
                                    kind,
                                    val,
                                    symbol,
                                    rt,
                                )
                            } else if ty.is_int() {
                                let kind = match ty.bytewidth() {
                                    4 => PseudoStoreKind::Word,
                                    _ => unimplemented!(),
                                };
                                InstData::new_pseudo_store(
                                    &mut self.machine_ctx,
                                    kind,
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

                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(store, self.block_map[&block])
                                .unwrap();
                        }
                        ValueKind::GetElemPtr(_) => {
                            let reg = if let ValueCodegenResult::Register(reg) =
                                self.get_value(store.ptr())
                            {
                                *reg
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
                                    reg,
                                    0.into(),
                                )
                            } else if ty.is_int() {
                                let kind = match ty.bytewidth() {
                                    1 => StoreKind::Byte,
                                    2 => StoreKind::Half,
                                    4 => StoreKind::Word,
                                    8 => StoreKind::DoubleWord,
                                    _ => unimplemented!(),
                                };
                                InstData::new_store(&mut self.machine_ctx, kind, val, reg, 0.into())
                            } else {
                                unreachable!()
                            };

                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(store, self.block_map[&block])
                                .unwrap();
                        }
                        _ => unimplemented!(),
                    });
            }
            ValueKind::Load(load) => {
                let ptr = load.ptr();
                let ty = function_data
                    .dfg()
                    .local_value_data(inst.into())
                    .unwrap()
                    .ty();

                module.with_value_data(ptr, |data| match data.kind() {
                    ValueKind::GlobalSlot(_) => {
                        let symbol = if let ValueCodegenResult::MachineSymbol(symbol) =
                            self.get_value(ptr)
                        {
                            symbol.clone()
                        } else {
                            unreachable!();
                        };
                        let rt = self
                            .machine_ctx
                            .new_virtual_reg(VirtualRegisterKind::General);

                        let (rd, load) = if ty.is_float() {
                            let kind = match ty.bytewidth() {
                                4 => FloatPseudoLoadKind::Single,
                                _ => unimplemented!(),
                            };
                            InstData::new_float_pseudo_load(&mut self.machine_ctx, kind, symbol, rt)
                        } else if ty.is_int() {
                            let kind = match ty.bytewidth() {
                                4 => PseudoLoadKind::Word,
                                _ => unimplemented!(),
                            };
                            InstData::new_pseudo_load(&mut self.machine_ctx, kind, symbol)
                        } else {
                            unreachable!()
                        };

                        machine_function_layout!(mut self.machine_ctx, &function_name)
                            .append_inst(load, self.block_map[&block])
                            .unwrap();

                        self.value_map
                            .insert(inst.into(), ValueCodegenResult::Register(rd));
                    }
                    ValueKind::Alloc(_) => {
                        let (base, offset) = if let ValueCodegenResult::StackSlot { base, offset } =
                            self.get_value(ptr)
                        {
                            (*base, *offset)
                        } else {
                            unreachable!();
                        };
                        let (rd, load) = if ty.is_float() {
                            let kind = match ty.bytewidth() {
                                4 => FloatLoadKind::Single,
                                8 => FloatLoadKind::Double,
                                _ => unimplemented!(),
                            };
                            InstData::new_float_load(
                                &mut self.machine_ctx,
                                kind,
                                base,
                                offset.into(),
                            )
                        } else if ty.is_int() {
                            let kind = match ty.bytewidth() {
                                1 => LoadKind::Byte,
                                2 => LoadKind::Half,
                                4 => LoadKind::Word,
                                8 => LoadKind::DoubleWord,
                                _ => unimplemented!(),
                            };
                            InstData::new_load(&mut self.machine_ctx, kind, base, offset.into())
                        } else {
                            unreachable!()
                        };

                        machine_function_layout!(mut self.machine_ctx, &function_name)
                            .append_inst(load, self.block_map[&block])
                            .unwrap();

                        self.value_map
                            .insert(inst.into(), ValueCodegenResult::Register(rd));
                    }
                    ValueKind::GetElemPtr(_) => {
                        let reg = if let ValueCodegenResult::Register(reg) = self.get_value(ptr) {
                            *reg
                        } else {
                            unreachable!();
                        };

                        let (rd, load) = if ty.is_float() {
                            let kind = match ty.bytewidth() {
                                4 => FloatLoadKind::Single,
                                8 => FloatLoadKind::Double,
                                _ => unimplemented!(),
                            };
                            InstData::new_float_load(&mut self.machine_ctx, kind, reg, 0.into())
                        } else if ty.is_int() {
                            let kind = match ty.bytewidth() {
                                1 => LoadKind::Byte,
                                2 => LoadKind::Half,
                                4 => LoadKind::Word,
                                8 => LoadKind::DoubleWord,
                                _ => unimplemented!(),
                            };
                            InstData::new_load(&mut self.machine_ctx, kind, reg, 0.into())
                        } else {
                            unreachable!()
                        };

                        machine_function_layout!(mut self.machine_ctx, &function_name)
                            .append_inst(load, self.block_map[&block])
                            .unwrap();

                        self.value_map
                            .insert(inst.into(), ValueCodegenResult::Register(rd));
                    }
                    _ => unimplemented!(),
                });
            }
            ValueKind::Binary(binary) => {
                let lhs = binary.lhs();
                let rhs = binary.rhs();
                let op = binary.op();

                let lhs_data = function_data.dfg().local_value_data(lhs).unwrap();
                let rhs_data = function_data.dfg().local_value_data(rhs).unwrap();

                if lhs_data.ty().is_float() && rhs_data.ty().is_float() {
                    let rs1 = match lhs_data.kind() {
                        ValueKind::GlobalSlot(_)
                        | ValueKind::Array(_)
                        | ValueKind::Struct(_)
                        | ValueKind::Function
                        | ValueKind::Store(_)
                        | ValueKind::Jump(_)
                        | ValueKind::Branch(_)
                        | ValueKind::Return(_)
                        | ValueKind::Alloc(_)
                        | ValueKind::GetElemPtr(_) => unreachable!(),
                        ValueKind::Zero | ValueKind::Undef => {
                            // fmv $rd, zero
                            let dst_fmt = match lhs_data.ty().bytewidth() {
                                2 => FMvFmt::H,
                                4 => FMvFmt::S,
                                8 => FMvFmt::D,
                                _ => unimplemented!(),
                            };
                            let zero = self
                                .machine_ctx
                                .new_gp_reg(RiscvGeneralPurposeRegister::Zero);
                            let (rd, fmv) = InstData::new_float_move(
                                &mut self.machine_ctx,
                                dst_fmt,
                                FMvFmt::X,
                                zero,
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(fmv, self.block_map[&block])
                                .unwrap();
                            rd
                        }
                        ValueKind::Bytes(bytes) => {
                            let imm: Immediate = bytes.into();
                            let (rd, li) = InstData::new_li(&mut self.machine_ctx, imm);
                            let dst_fmt = match lhs_data.ty().bytewidth() {
                                2 => FMvFmt::H,
                                4 => FMvFmt::S,
                                8 => FMvFmt::D,
                                _ => unimplemented!(),
                            };
                            let (rd, fmv) = InstData::new_float_move(
                                &mut self.machine_ctx,
                                dst_fmt,
                                FMvFmt::X,
                                rd,
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(li, self.block_map[&block])
                                .unwrap();
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(fmv, self.block_map[&block])
                                .unwrap();

                            rd
                        }
                        ValueKind::Binary(_)
                        | ValueKind::Unary(_)
                        | ValueKind::Call(_)
                        | ValueKind::Cast(_)
                        | ValueKind::BlockParam
                        | ValueKind::Load(_) => {
                            let codegen_result = self.get_value(lhs);
                            if let ValueCodegenResult::Register(reg) = codegen_result {
                                *reg
                            } else {
                                unreachable!();
                            }
                        }
                    };

                    // same as rs1
                    let rs2 = match lhs_data.kind() {
                        ValueKind::GlobalSlot(_)
                        | ValueKind::Array(_)
                        | ValueKind::Struct(_)
                        | ValueKind::Function
                        | ValueKind::Store(_)
                        | ValueKind::Jump(_)
                        | ValueKind::Branch(_)
                        | ValueKind::Return(_)
                        | ValueKind::Alloc(_)
                        | ValueKind::GetElemPtr(_) => unreachable!(),
                        ValueKind::Zero | ValueKind::Undef => {
                            // fmv $rd, zero
                            let dst_fmt = match lhs_data.ty().bytewidth() {
                                2 => FMvFmt::H,
                                4 => FMvFmt::S,
                                8 => FMvFmt::D,
                                _ => unimplemented!(),
                            };
                            let zero = self
                                .machine_ctx
                                .new_gp_reg(RiscvGeneralPurposeRegister::Zero);
                            let (rd, fmv) = InstData::new_float_move(
                                &mut self.machine_ctx,
                                dst_fmt,
                                FMvFmt::X,
                                zero,
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(fmv, self.block_map[&block])
                                .unwrap();
                            rd
                        }
                        ValueKind::Bytes(bytes) => {
                            let imm: Immediate = bytes.into();
                            let (rd, li) = InstData::new_li(&mut self.machine_ctx, imm);
                            let dst_fmt = match lhs_data.ty().bytewidth() {
                                2 => FMvFmt::H,
                                4 => FMvFmt::S,
                                8 => FMvFmt::D,
                                _ => unimplemented!(),
                            };
                            let (rd, fmv) = InstData::new_float_move(
                                &mut self.machine_ctx,
                                dst_fmt,
                                FMvFmt::X,
                                rd,
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(li, self.block_map[&block])
                                .unwrap();
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(fmv, self.block_map[&block])
                                .unwrap();

                            rd
                        }
                        ValueKind::Binary(_)
                        | ValueKind::Unary(_)
                        | ValueKind::Call(_)
                        | ValueKind::Cast(_)
                        | ValueKind::BlockParam
                        | ValueKind::Load(_) => {
                            let codegen_result = self.get_value(lhs);
                            if let ValueCodegenResult::Register(reg) = codegen_result {
                                *reg
                            } else {
                                unreachable!();
                            }
                        }
                    };

                    let fmt = match lhs_data.ty().bytewidth() {
                        4 => MachineFloatBinaryFmt::S,
                        8 => MachineFloatBinaryFmt::D,
                        _ => unimplemented!(),
                    };

                    let asm_op = match op {
                        BinaryOp::FAdd => MachineFloatBinaryOp::Fadd,
                        BinaryOp::FSub => MachineFloatBinaryOp::Fsub,
                        BinaryOp::FMul => MachineFloatBinaryOp::Fmul,
                        BinaryOp::FDiv => MachineFloatBinaryOp::Fdiv,
                        BinaryOp::FRem => unimplemented!("codegen for frem not implemented"),
                        BinaryOp::FCmp(ref cond) => match cond {
                            FCmpCond::OEq => MachineFloatBinaryOp::Feq,
                            FCmpCond::OLe => MachineFloatBinaryOp::Fle,
                            FCmpCond::OLt => MachineFloatBinaryOp::Flt,
                            FCmpCond::ONe => MachineFloatBinaryOp::Feq, // need to invert the result
                        },
                        _ => unreachable!(),
                    };

                    let (rd, fbin) =
                        InstData::new_float_binary(&mut self.machine_ctx, asm_op, fmt, rs1, rs2);

                    machine_function_layout!(mut self.machine_ctx, &function_name)
                        .append_inst(fbin, self.block_map[&block])
                        .unwrap();

                    if let BinaryOp::FCmp(FCmpCond::ONe) = op {
                        // invert the result of `feq`
                        let (rd, neg) = InstData::new_binary_imm(
                            &mut self.machine_ctx,
                            MachineBinaryImmOp::Xori,
                            rd,
                            (-1).into(),
                        );
                        machine_function_layout!(mut self.machine_ctx, &function_name)
                            .append_inst(neg, self.block_map[&block])
                            .unwrap();

                        self.value_map
                            .insert(inst.into(), ValueCodegenResult::Register(rd));
                    } else {
                        self.value_map
                            .insert(inst.into(), ValueCodegenResult::Register(rd));
                    };
                } else if lhs_data.ty().is_int() && rhs_data.ty().is_int() {
                    enum BinaryOperand {
                        Register(Register),
                        /// For those immediate values that can be encoded in
                        /// the I-type instruction
                        Immediate(Immediate),
                    }
                    let mut rs1: BinaryOperand = match lhs_data.kind() {
                        ValueKind::GlobalSlot(_)
                        | ValueKind::Array(_)
                        | ValueKind::Struct(_)
                        | ValueKind::Function
                        | ValueKind::Store(_)
                        | ValueKind::Jump(_)
                        | ValueKind::Branch(_)
                        | ValueKind::Return(_) => unreachable!(),
                        ValueKind::Zero | ValueKind::Undef => BinaryOperand::Immediate(0.into()),
                        ValueKind::Bytes(bytes) => {
                            let imm: Immediate = bytes.into();
                            if check_itype_imm(imm) {
                                BinaryOperand::Immediate(imm)
                            } else {
                                let (rd, li) = InstData::new_li(&mut self.machine_ctx, imm);
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(li, self.block_map[&block])
                                    .unwrap();
                                BinaryOperand::Register(rd)
                            }
                        }
                        _ => {
                            let codegen_result = self.get_value(lhs);
                            if let ValueCodegenResult::Register(reg) = codegen_result {
                                BinaryOperand::Register(*reg)
                            } else {
                                unreachable!();
                            }
                        }
                    };
                    // same as rs1
                    let mut rs2: BinaryOperand = match rhs_data.kind() {
                        ValueKind::GlobalSlot(_)
                        | ValueKind::Array(_)
                        | ValueKind::Struct(_)
                        | ValueKind::Function
                        | ValueKind::Store(_)
                        | ValueKind::Jump(_)
                        | ValueKind::Branch(_)
                        | ValueKind::Return(_) => unreachable!(),
                        ValueKind::Zero | ValueKind::Undef => BinaryOperand::Immediate(0.into()),
                        ValueKind::Bytes(bytes) => {
                            let imm: Immediate = bytes.into();
                            if check_itype_imm(imm) {
                                BinaryOperand::Immediate(imm)
                            } else {
                                let (rd, li) = InstData::new_li(&mut self.machine_ctx, imm);
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(li, self.block_map[&block])
                                    .unwrap();
                                BinaryOperand::Register(rd)
                            }
                        }
                        _ => {
                            let codegen_result = self.get_value(rhs);
                            if let ValueCodegenResult::Register(reg) = codegen_result {
                                BinaryOperand::Register(*reg)
                            } else {
                                unreachable!();
                            }
                        }
                    };

                    if let BinaryOperand::Immediate(_) = rs1 {
                        // swap rs1 and rs2 so that rs1 is always a register (unless both are
                        // immediates)
                        std::mem::swap(&mut rs1, &mut rs2);
                    }

                    if let BinaryOperand::Immediate(imm) = rs1 {
                        // load immediate into register
                        // this should actually be handled with constant folding, but just in case.
                        let (rd, li) = InstData::new_li(&mut self.machine_ctx, imm);
                        machine_function_layout!(mut self.machine_ctx, &function_name)
                            .append_inst(li, self.block_map[&block])
                            .unwrap();
                        rs1 = BinaryOperand::Register(rd);
                    }

                    match (op, rs1, rs2) {
                        (
                            BinaryOp::Add,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Immediate(imm),
                        ) => {
                            let kind = match lhs_data.ty().bytewidth() {
                                4 => MachineBinaryImmOp::Addiw,
                                8 => MachineBinaryImmOp::Addi,
                                _ => unimplemented!(),
                            };
                            let (rd, addi) =
                                InstData::new_binary_imm(&mut self.machine_ctx, kind, rs1, imm);
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(addi, self.block_map[&block])
                                .unwrap();
                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::Add,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Register(rs2),
                        ) => {
                            let kind = match lhs_data.ty().bytewidth() {
                                4 => MachineBinaryOp::Addw,
                                8 => MachineBinaryOp::Add,
                                _ => unimplemented!(),
                            };
                            let (rd, add) =
                                InstData::new_binary(&mut self.machine_ctx, kind, rs1, rs2);
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(add, self.block_map[&block])
                                .unwrap();
                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::Sub,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Immediate(imm),
                        ) => {
                            let kind = match lhs_data.ty().bytewidth() {
                                4 => MachineBinaryImmOp::Addiw,
                                8 => MachineBinaryImmOp::Addi,
                                _ => unimplemented!(),
                            };
                            let imm = (-imm.0) as i32;
                            let (rd, addi) = InstData::new_binary_imm(
                                &mut self.machine_ctx,
                                kind,
                                rs1,
                                imm.into(),
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(addi, self.block_map[&block])
                                .unwrap();
                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::Sub,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Register(rs2),
                        ) => {
                            let kind = match lhs_data.ty().bytewidth() {
                                4 => MachineBinaryOp::Subw,
                                8 => MachineBinaryOp::Sub,
                                _ => unimplemented!(),
                            };
                            let (rd, sub) =
                                InstData::new_binary(&mut self.machine_ctx, kind, rs1, rs2);
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(sub, self.block_map[&block])
                                .unwrap();
                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::Mul,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Immediate(imm),
                        ) => {
                            let (rs2, li) = InstData::new_li(&mut self.machine_ctx, imm);
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(li, self.block_map[&block])
                                .unwrap();

                            let kind = match lhs_data.ty().bytewidth() {
                                4 => MachineBinaryOp::Mulw,
                                8 => MachineBinaryOp::Mul,
                                _ => unimplemented!(),
                            };

                            let (rd, mul) =
                                InstData::new_binary(&mut self.machine_ctx, kind, rs1, rs2);

                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(mul, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::Mul,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Register(rs2),
                        ) => {
                            let kind = match lhs_data.ty().bytewidth() {
                                4 => MachineBinaryOp::Mulw,
                                8 => MachineBinaryOp::Mul,
                                _ => unimplemented!(),
                            };

                            let (rd, mul) =
                                InstData::new_binary(&mut self.machine_ctx, kind, rs1, rs2);

                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(mul, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::UDiv,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Immediate(imm),
                        ) => {
                            let (rs2, li) = InstData::new_li(&mut self.machine_ctx, imm);
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(li, self.block_map[&block])
                                .unwrap();

                            let kind = match lhs_data.ty().bytewidth() {
                                4 => MachineBinaryOp::Divuw,
                                8 => MachineBinaryOp::Divu,
                                _ => unimplemented!(),
                            };

                            let (rd, div) =
                                InstData::new_binary(&mut self.machine_ctx, kind, rs1, rs2);

                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(div, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::UDiv,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Register(rs2),
                        ) => {
                            let kind = match lhs_data.ty().bytewidth() {
                                4 => MachineBinaryOp::Divuw,
                                8 => MachineBinaryOp::Divu,
                                _ => unimplemented!(),
                            };

                            let (rd, div) =
                                InstData::new_binary(&mut self.machine_ctx, kind, rs1, rs2);

                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(div, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::SDiv,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Immediate(imm),
                        ) => {
                            let (rs2, li) = InstData::new_li(&mut self.machine_ctx, imm);
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(li, self.block_map[&block])
                                .unwrap();

                            let kind = match lhs_data.ty().bytewidth() {
                                4 => MachineBinaryOp::Divw,
                                8 => MachineBinaryOp::Div,
                                _ => unimplemented!(),
                            };

                            let (rd, div) =
                                InstData::new_binary(&mut self.machine_ctx, kind, rs1, rs2);

                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(div, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::SDiv,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Register(rs2),
                        ) => {
                            let kind = match lhs_data.ty().bytewidth() {
                                4 => MachineBinaryOp::Divw,
                                8 => MachineBinaryOp::Div,
                                _ => unimplemented!(),
                            };

                            let (rd, div) =
                                InstData::new_binary(&mut self.machine_ctx, kind, rs1, rs2);

                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(div, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::URem,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Immediate(imm),
                        ) => {
                            let (rs2, li) = InstData::new_li(&mut self.machine_ctx, imm);
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(li, self.block_map[&block])
                                .unwrap();

                            let kind = match lhs_data.ty().bytewidth() {
                                4 => MachineBinaryOp::Remuw,
                                8 => MachineBinaryOp::Remu,
                                _ => unimplemented!(),
                            };

                            let (rd, rem) =
                                InstData::new_binary(&mut self.machine_ctx, kind, rs1, rs2);

                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(rem, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::URem,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Register(rs2),
                        ) => {
                            let kind = match lhs_data.ty().bytewidth() {
                                4 => MachineBinaryOp::Remuw,
                                8 => MachineBinaryOp::Remu,
                                _ => unimplemented!(),
                            };

                            let (rd, rem) =
                                InstData::new_binary(&mut self.machine_ctx, kind, rs1, rs2);

                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(rem, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::SRem,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Immediate(imm),
                        ) => {
                            let (rs2, li) = InstData::new_li(&mut self.machine_ctx, imm);
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(li, self.block_map[&block])
                                .unwrap();

                            let kind = match lhs_data.ty().bytewidth() {
                                4 => MachineBinaryOp::Remw,
                                8 => MachineBinaryOp::Rem,
                                _ => unimplemented!(),
                            };

                            let (rd, rem) =
                                InstData::new_binary(&mut self.machine_ctx, kind, rs1, rs2);

                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(rem, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::SRem,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Register(rs2),
                        ) => {
                            let kind = match lhs_data.ty().bytewidth() {
                                4 => MachineBinaryOp::Remw,
                                8 => MachineBinaryOp::Rem,
                                _ => unimplemented!(),
                            };

                            let (rd, rem) =
                                InstData::new_binary(&mut self.machine_ctx, kind, rs1, rs2);

                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(rem, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::And,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Immediate(imm),
                        ) => {
                            let (rd, andi) = InstData::new_binary_imm(
                                &mut self.machine_ctx,
                                MachineBinaryImmOp::Andi,
                                rs1,
                                imm,
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(andi, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::And,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Register(rs2),
                        ) => {
                            let (rd, and) = InstData::new_binary(
                                &mut self.machine_ctx,
                                MachineBinaryOp::And,
                                rs1,
                                rs2,
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(and, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::Or,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Immediate(imm),
                        ) => {
                            let (rd, ori) = InstData::new_binary_imm(
                                &mut self.machine_ctx,
                                MachineBinaryImmOp::Ori,
                                rs1,
                                imm,
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(ori, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::Or,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Register(rs2),
                        ) => {
                            let (rd, or) = InstData::new_binary(
                                &mut self.machine_ctx,
                                MachineBinaryOp::Or,
                                rs1,
                                rs2,
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(or, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::Xor,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Immediate(imm),
                        ) => {
                            let (rd, xori) = InstData::new_binary_imm(
                                &mut self.machine_ctx,
                                MachineBinaryImmOp::Xori,
                                rs1,
                                imm,
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(xori, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::Xor,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Register(rs2),
                        ) => {
                            let (rd, xor) = InstData::new_binary(
                                &mut self.machine_ctx,
                                MachineBinaryOp::Xor,
                                rs1,
                                rs2,
                            );

                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(xor, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::Shl,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Immediate(imm),
                        ) => {
                            let (rd, slli) = InstData::new_binary_imm(
                                &mut self.machine_ctx,
                                MachineBinaryImmOp::Slli,
                                rs1,
                                imm,
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(slli, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::Shl,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Register(rs2),
                        ) => {
                            let (rd, sll) = InstData::new_binary(
                                &mut self.machine_ctx,
                                MachineBinaryOp::Sll,
                                rs1,
                                rs2,
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(sll, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::LShr,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Immediate(imm),
                        ) => {
                            let (rd, srli) = InstData::new_binary_imm(
                                &mut self.machine_ctx,
                                MachineBinaryImmOp::Srli,
                                rs1,
                                imm,
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(srli, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::LShr,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Register(rs2),
                        ) => {
                            let (rd, srl) = InstData::new_binary(
                                &mut self.machine_ctx,
                                MachineBinaryOp::Srl,
                                rs1,
                                rs2,
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(srl, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::AShr,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Immediate(imm),
                        ) => {
                            let (rd, srai) = InstData::new_binary_imm(
                                &mut self.machine_ctx,
                                MachineBinaryImmOp::Srai,
                                rs1,
                                imm,
                            );

                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(srai, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::AShr,
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Register(rs2),
                        ) => {
                            let (rd, sra) = InstData::new_binary(
                                &mut self.machine_ctx,
                                MachineBinaryOp::Sra,
                                rs1,
                                rs2,
                            );

                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(sra, self.block_map[&block])
                                .unwrap();

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        (
                            BinaryOp::ICmp(cond),
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Immediate(imm),
                        ) => match cond {
                            ICmpCond::Eq | ICmpCond::Ne => {
                                let kind = match lhs_data.ty().bytewidth() {
                                    4 => MachineBinaryImmOp::Addiw,
                                    8 => MachineBinaryImmOp::Addi,
                                    _ => unimplemented!(),
                                };
                                let imm = (-imm.0) as i32;
                                let (rd, addi) = InstData::new_binary_imm(
                                    &mut self.machine_ctx,
                                    kind,
                                    rs1,
                                    imm.into(),
                                );

                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(addi, self.block_map[&block])
                                    .unwrap();

                                if let ICmpCond::Eq = cond {
                                    let (rd, seqz) = InstData::new_binary_imm(
                                        &mut self.machine_ctx,
                                        MachineBinaryImmOp::Sltiu,
                                        rd,
                                        1.into(),
                                    );
                                    machine_function_layout!(mut self.machine_ctx, &function_name)
                                        .append_inst(seqz, self.block_map[&block])
                                        .unwrap();

                                    self.value_map
                                        .insert(inst.into(), ValueCodegenResult::Register(rd));
                                } else {
                                    let zero = self
                                        .machine_ctx
                                        .new_gp_reg(RiscvGeneralPurposeRegister::Zero);
                                    let (rd, snez) = InstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Sltu,
                                        zero,
                                        rd,
                                    );
                                    machine_function_layout!(mut self.machine_ctx, &function_name)
                                        .append_inst(snez, self.block_map[&block])
                                        .unwrap();

                                    self.value_map
                                        .insert(inst.into(), ValueCodegenResult::Register(rd));
                                }
                            }
                            ICmpCond::Slt => {
                                let (rd, slti) = InstData::new_binary_imm(
                                    &mut self.machine_ctx,
                                    MachineBinaryImmOp::Slti,
                                    rs1,
                                    imm,
                                );
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(slti, self.block_map[&block])
                                    .unwrap();
                                self.value_map
                                    .insert(inst.into(), ValueCodegenResult::Register(rd));
                            }
                            ICmpCond::Sle => {
                                // lhs <= rhs <=> !(lhs > rhs) <=> !(rhs < lhs)
                                // needs to be loaded into a register
                                let (rs2, li) = InstData::new_li(&mut self.machine_ctx, imm);
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(li, self.block_map[&block])
                                    .unwrap();

                                let (rd, slt) = InstData::new_binary(
                                    &mut self.machine_ctx,
                                    MachineBinaryOp::Slt,
                                    rs2,
                                    rs1,
                                );
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(slt, self.block_map[&block])
                                    .unwrap();

                                let (rd, xori) = InstData::new_binary_imm(
                                    &mut self.machine_ctx,
                                    MachineBinaryImmOp::Xori,
                                    rd,
                                    (-1).into(),
                                );

                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(xori, self.block_map[&block])
                                    .unwrap();

                                self.value_map
                                    .insert(inst.into(), ValueCodegenResult::Register(rd));
                            }
                        },
                        (
                            BinaryOp::ICmp(cond),
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Register(rs2),
                        ) => match cond {
                            ICmpCond::Eq | ICmpCond::Ne => {
                                let (rd, sub) = InstData::new_binary(
                                    &mut self.machine_ctx,
                                    MachineBinaryOp::Sub,
                                    rs1,
                                    rs2,
                                );

                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(sub, self.block_map[&block])
                                    .unwrap();

                                if let ICmpCond::Eq = cond {
                                    let (rd, seqz) = InstData::new_binary_imm(
                                        &mut self.machine_ctx,
                                        MachineBinaryImmOp::Sltiu,
                                        rd,
                                        1.into(),
                                    );
                                    machine_function_layout!(mut self.machine_ctx, &function_name)
                                        .append_inst(seqz, self.block_map[&block])
                                        .unwrap();

                                    self.value_map
                                        .insert(inst.into(), ValueCodegenResult::Register(rd));
                                } else {
                                    let zero = self
                                        .machine_ctx
                                        .new_gp_reg(RiscvGeneralPurposeRegister::Zero);
                                    let (rd, snez) = InstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Sltu,
                                        zero,
                                        rd,
                                    );
                                    machine_function_layout!(mut self.machine_ctx, &function_name)
                                        .append_inst(snez, self.block_map[&block])
                                        .unwrap();

                                    self.value_map
                                        .insert(inst.into(), ValueCodegenResult::Register(rd));
                                }
                            }
                            ICmpCond::Slt => {
                                let (rd, slt) = InstData::new_binary(
                                    &mut self.machine_ctx,
                                    MachineBinaryOp::Slt,
                                    rs1,
                                    rs2,
                                );
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(slt, self.block_map[&block])
                                    .unwrap();
                                self.value_map
                                    .insert(inst.into(), ValueCodegenResult::Register(rd));
                            }
                            ICmpCond::Sle => {
                                let (rd, slt) = InstData::new_binary(
                                    &mut self.machine_ctx,
                                    MachineBinaryOp::Slt,
                                    rs2,
                                    rs1,
                                );
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(slt, self.block_map[&block])
                                    .unwrap();

                                let (rd, xori) = InstData::new_binary_imm(
                                    &mut self.machine_ctx,
                                    MachineBinaryImmOp::Xori,
                                    rd,
                                    (-1).into(),
                                );

                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(xori, self.block_map[&block])
                                    .unwrap();

                                self.value_map
                                    .insert(inst.into(), ValueCodegenResult::Register(rd));
                            }
                        },
                        _ => unreachable!(),
                    }
                } else {
                    unreachable!()
                }
            }
            ValueKind::Unary(unary) => {
                let op = unary.op();
                let val = unary.val();

                let val_data = function_data.dfg().local_value_data(val).unwrap();

                let operand = match val_data.kind() {
                    ValueKind::Alloc(_)
                    | ValueKind::Array(_)
                    | ValueKind::Struct(_)
                    | ValueKind::Function
                    | ValueKind::Store(_)
                    | ValueKind::Jump(_)
                    | ValueKind::Branch(_)
                    | ValueKind::Return(_) => unreachable!(),
                    ValueKind::Zero | ValueKind::Undef => {
                        let zero = self
                            .machine_ctx
                            .new_gp_reg(RiscvGeneralPurposeRegister::Zero);
                        self.value_map
                            .insert(inst.into(), ValueCodegenResult::Register(zero));
                        zero
                    }
                    ValueKind::Bytes(bytes) => {
                        let imm: Immediate = bytes.into();
                        let (rd, li) = InstData::new_li(&mut self.machine_ctx, imm);
                        machine_function_layout!(mut self.machine_ctx, &function_name)
                            .append_inst(li, self.block_map[&block])
                            .unwrap();
                        rd
                    }
                    _ => {
                        let codegen_result = self.get_value(val);
                        if let ValueCodegenResult::Register(reg) = codegen_result {
                            *reg
                        } else {
                            unreachable!();
                        }
                    }
                };

                match op {
                    UnaryOp::FNeg => {
                        // fmv
                        let dst_fmt = match val_data.ty().bytewidth() {
                            2 => FMvFmt::H,
                            4 => FMvFmt::S,
                            8 => FMvFmt::D,
                            _ => unimplemented!(),
                        };

                        let (rd, fmv) = InstData::new_float_move(
                            &mut self.machine_ctx,
                            dst_fmt,
                            FMvFmt::X,
                            operand,
                        );

                        machine_function_layout!(mut self.machine_ctx, &function_name)
                            .append_inst(fmv, self.block_map[&block])
                            .unwrap();

                        // fneg => fsgnjn
                        let fmt = match val_data.ty().bytewidth() {
                            4 => MachineFloatBinaryFmt::S,
                            8 => MachineFloatBinaryFmt::D,
                            _ => unimplemented!(),
                        };

                        let (rd, fsgnjn) = InstData::new_float_binary(
                            &mut self.machine_ctx,
                            MachineFloatBinaryOp::Fsgnjn,
                            fmt,
                            rd,
                            rd,
                        );

                        machine_function_layout!(mut self.machine_ctx, &function_name)
                            .append_inst(fsgnjn, self.block_map[&block])
                            .unwrap();

                        self.value_map
                            .insert(inst.into(), ValueCodegenResult::Register(rd));
                    }
                    UnaryOp::Not => {
                        let (rd, xori) = InstData::new_binary_imm(
                            &mut self.machine_ctx,
                            MachineBinaryImmOp::Xori,
                            operand,
                            (-1).into(),
                        );

                        machine_function_layout!(mut self.machine_ctx, &function_name)
                            .append_inst(xori, self.block_map[&block])
                            .unwrap();

                        self.value_map
                            .insert(inst.into(), ValueCodegenResult::Register(rd));
                    }
                }
            }
            ValueKind::Cast(cast) => {
                let op = cast.op();
                let val = cast.val();

                let val_data = function_data.dfg().local_value_data(val).unwrap();

                let rs = match val_data.kind() {
                    ValueKind::GlobalSlot(_)
                    | ValueKind::Array(_)
                    | ValueKind::Struct(_)
                    | ValueKind::Function
                    | ValueKind::Store(_)
                    | ValueKind::Jump(_)
                    | ValueKind::Branch(_)
                    | ValueKind::Return(_) => unreachable!(),
                    ValueKind::Zero | ValueKind::Undef => {
                        // fmv $rd, zero
                        let fmt = match val_data.ty().bytewidth() {
                            2 => FMvFmt::H,
                            4 => FMvFmt::S,
                            8 => FMvFmt::D,
                            _ => unimplemented!(),
                        };
                        let zero = self
                            .machine_ctx
                            .new_gp_reg(RiscvGeneralPurposeRegister::Zero);
                        let (rd, fmv) =
                            InstData::new_float_move(&mut self.machine_ctx, fmt, FMvFmt::X, zero);
                        machine_function_layout!(mut self.machine_ctx, &function_name)
                            .append_inst(fmv, self.block_map[&block])
                            .unwrap();
                        rd
                    }
                    ValueKind::Bytes(bytes) => {
                        let imm: Immediate = bytes.into();
                        let (rd, li) = InstData::new_li(&mut self.machine_ctx, imm);
                        let fmt = match val_data.ty().bytewidth() {
                            2 => FMvFmt::H,
                            4 => FMvFmt::S,
                            8 => FMvFmt::D,
                            _ => unimplemented!(),
                        };
                        let (rd, fmv) =
                            InstData::new_float_move(&mut self.machine_ctx, fmt, FMvFmt::X, rd);
                        machine_function_layout!(mut self.machine_ctx, &function_name)
                            .append_inst(li, self.block_map[&block])
                            .unwrap();
                        machine_function_layout!(mut self.machine_ctx, &function_name)
                            .append_inst(fmv, self.block_map[&block])
                            .unwrap();
                        rd
                    }
                    _ => {
                        let codegen_result = self.get_value(val);
                        if let ValueCodegenResult::Register(reg) = codegen_result {
                            *reg
                        } else {
                            unreachable!();
                        }
                    }
                };

                let this_ty = function_data
                    .dfg()
                    .local_value_data(inst.into())
                    .unwrap()
                    .ty();
                match op {
                    CastOp::FpToSI => {
                        // TODO
                    }
                    _ => unimplemented!(),
                }
            }
            ValueKind::Jump(jump) => {
                let block = jump.dst();
                let args = jump.args();

                let params = function_data.dfg().block_data(block).unwrap().params();

                assert_eq!(args.len(), params.len());

                for (arg, param) in args.iter().zip(params.iter()) {
                    // the register should have been assigned to the block argument.
                    let rd = if let ValueCodegenResult::Register(reg) = self.get_value(*param) {
                        *reg
                    } else {
                        unreachable!();
                    };
                    let arg_data = function_data.dfg().local_value_data(*arg).unwrap();
                    match arg_data.kind() {
                        ValueKind::GlobalSlot(_)
                        | ValueKind::Array(_)
                        | ValueKind::Struct(_)
                        | ValueKind::Function
                        | ValueKind::Store(_)
                        | ValueKind::Jump(_)
                        | ValueKind::Branch(_)
                        | ValueKind::Return(_) => unreachable!(),
                        ValueKind::Zero | ValueKind::Undef => {
                            let zero = self
                                .machine_ctx
                                .new_gp_reg(RiscvGeneralPurposeRegister::Zero);

                            if arg_data.ty().is_float() {
                                let dst_fmt = match arg_data.ty().bytewidth() {
                                    2 => FMvFmt::H,
                                    4 => FMvFmt::S,
                                    8 => FMvFmt::D,
                                    _ => unimplemented!(),
                                };
                                let fmv = InstData::build_fmv(
                                    &mut self.machine_ctx,
                                    dst_fmt,
                                    FMvFmt::X,
                                    rd,
                                    zero,
                                );
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(fmv, self.block_map[&block])
                                    .unwrap();
                            } else if arg_data.ty().is_int() || arg_data.ty().is_ptr() {
                                let mv = InstData::build_gp_move(&mut self.machine_ctx, rd, zero);
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(mv, self.block_map[&block])
                                    .unwrap();
                            } else {
                                unreachable!()
                            }
                        }
                        ValueKind::Bytes(bytes) => {
                            let imm: Immediate = bytes.into();
                            if arg_data.ty().is_float() {
                                let (tmp, li) = InstData::new_li(&mut self.machine_ctx, imm);
                                let mv = InstData::build_fp_move(&mut self.machine_ctx, rd, tmp);
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(li, self.block_map[&block])
                                    .unwrap();
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(mv, self.block_map[&block])
                                    .unwrap();
                            } else if arg_data.ty().is_int() || arg_data.ty().is_ptr() {
                                let li = InstData::build_li(&mut self.machine_ctx, rd, imm);
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(li, self.block_map[&block])
                                    .unwrap();
                            } else {
                                unreachable!()
                            }
                        }
                        _ => {
                            let codegen_result = self.get_value(*arg);
                            let rs = if let ValueCodegenResult::Register(reg) = codegen_result {
                                *reg
                            } else {
                                unreachable!();
                            };
                            let mv = if arg_data.ty().is_float() {
                                InstData::build_fp_move(&mut self.machine_ctx, rd, rs)
                            } else if arg_data.ty().is_int() || arg_data.ty().is_ptr() {
                                InstData::build_gp_move(&mut self.machine_ctx, rd, rs)
                            } else {
                                unreachable!();
                            };
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(mv, self.block_map[&block])
                                .unwrap();
                        }
                    }
                }

                let j = InstData::new_j(&mut self.machine_ctx, self.block_map[&block]);
                machine_function_layout!(mut self.machine_ctx, &function_name)
                    .append_inst(j, self.block_map[&block])
                    .unwrap();
            }
            ValueKind::Branch(branch) => {
                let cond = branch.cond();
                let then_block = branch.then_dst();
                let else_block = branch.else_dst();

                let cond_data = function_data.dfg().local_value_data(cond).unwrap();

                let cond_reg = match cond_data.kind() {
                    ValueKind::GlobalSlot(_)
                    | ValueKind::Array(_)
                    | ValueKind::Struct(_)
                    | ValueKind::Function
                    | ValueKind::Store(_)
                    | ValueKind::Jump(_)
                    | ValueKind::Branch(_)
                    | ValueKind::Return(_)
                    | ValueKind::Zero
                    | ValueKind::Undef => unreachable!(),
                    ValueKind::Bytes(bytes) => {
                        let imm: Immediate = bytes.into();
                        let (rd, li) = InstData::new_li(&mut self.machine_ctx, imm);
                        machine_function_layout!(mut self.machine_ctx, &function_name)
                            .append_inst(li, self.block_map[&block])
                            .unwrap();
                        rd
                    }
                    _ => {
                        let codegen_result = self.get_value(cond);
                        if let ValueCodegenResult::Register(reg) = codegen_result {
                            *reg
                        } else {
                            unreachable!();
                        }
                    }
                };

                // params <- args
                let params = function_data.dfg().block_data(then_block).unwrap().params();
                let args = branch.then_args();

                assert_eq!(params.len(), args.len());

                // same as jump
                for (arg, param) in args.iter().zip(params.iter()) {
                    // the register should have been assigned to the block argument.
                    let rd = if let ValueCodegenResult::Register(reg) = self.get_value(*param) {
                        *reg
                    } else {
                        unreachable!();
                    };
                    let arg_data = function_data.dfg().local_value_data(*arg).unwrap();
                    match arg_data.kind() {
                        ValueKind::GlobalSlot(_)
                        | ValueKind::Array(_)
                        | ValueKind::Struct(_)
                        | ValueKind::Function
                        | ValueKind::Store(_)
                        | ValueKind::Jump(_)
                        | ValueKind::Branch(_)
                        | ValueKind::Return(_) => unreachable!(),
                        ValueKind::Zero | ValueKind::Undef => {
                            let zero = self
                                .machine_ctx
                                .new_gp_reg(RiscvGeneralPurposeRegister::Zero);

                            if arg_data.ty().is_float() {
                                let dst_fmt = match arg_data.ty().bytewidth() {
                                    2 => FMvFmt::H,
                                    4 => FMvFmt::S,
                                    8 => FMvFmt::D,
                                    _ => unimplemented!(),
                                };
                                let fmv = InstData::build_fmv(
                                    &mut self.machine_ctx,
                                    dst_fmt,
                                    FMvFmt::X,
                                    rd,
                                    zero,
                                );
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(fmv, self.block_map[&block])
                                    .unwrap();
                            } else if arg_data.ty().is_int() || arg_data.ty().is_ptr() {
                                let mv = InstData::build_gp_move(&mut self.machine_ctx, rd, zero);
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(mv, self.block_map[&block])
                                    .unwrap();
                            } else {
                                unreachable!()
                            }
                        }
                        ValueKind::Bytes(bytes) => {
                            let imm: Immediate = bytes.into();
                            if arg_data.ty().is_float() {
                                let (tmp, li) = InstData::new_li(&mut self.machine_ctx, imm);
                                let mv = InstData::build_fp_move(&mut self.machine_ctx, rd, tmp);
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(li, self.block_map[&block])
                                    .unwrap();
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(mv, self.block_map[&block])
                                    .unwrap();
                            } else if arg_data.ty().is_int() || arg_data.ty().is_ptr() {
                                let li = InstData::build_li(&mut self.machine_ctx, rd, imm);
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(li, self.block_map[&block])
                                    .unwrap();
                            } else {
                                unreachable!()
                            }
                        }
                        _ => {
                            let codegen_result = self.get_value(*arg);
                            let rs = if let ValueCodegenResult::Register(reg) = codegen_result {
                                *reg
                            } else {
                                unreachable!();
                            };
                            let mv = if arg_data.ty().is_float() {
                                InstData::build_fp_move(&mut self.machine_ctx, rd, rs)
                            } else if arg_data.ty().is_int() || arg_data.ty().is_ptr() {
                                InstData::build_gp_move(&mut self.machine_ctx, rd, rs)
                            } else {
                                unreachable!();
                            };
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(mv, self.block_map[&block])
                                .unwrap();
                        }
                    }
                }

                // bnez $cond, $then_block
                let zero = self
                    .machine_ctx
                    .new_gp_reg(RiscvGeneralPurposeRegister::Zero);
                let bne = InstData::new_branch(
                    &mut self.machine_ctx,
                    MachineBranchOp::Bne,
                    cond_reg,
                    zero,
                    self.block_map[&then_block],
                );

                machine_function_layout!(mut self.machine_ctx, &function_name)
                    .append_inst(bne, self.block_map[&block])
                    .unwrap();

                // params <- args
                let params = function_data.dfg().block_data(else_block).unwrap().params();
                let args = branch.else_args();

                assert_eq!(params.len(), args.len());

                for (arg, param) in args.iter().zip(params.iter()) {
                    // the register should have been assigned to the block argument.
                    let rd = if let ValueCodegenResult::Register(reg) = self.get_value(*param) {
                        *reg
                    } else {
                        unreachable!();
                    };
                    let arg_data = function_data.dfg().local_value_data(*arg).unwrap();
                    match arg_data.kind() {
                        ValueKind::GlobalSlot(_)
                        | ValueKind::Array(_)
                        | ValueKind::Struct(_)
                        | ValueKind::Function
                        | ValueKind::Store(_)
                        | ValueKind::Jump(_)
                        | ValueKind::Branch(_)
                        | ValueKind::Return(_) => unreachable!(),
                        ValueKind::Zero | ValueKind::Undef => {
                            let zero = self
                                .machine_ctx
                                .new_gp_reg(RiscvGeneralPurposeRegister::Zero);

                            if arg_data.ty().is_float() {
                                let dst_fmt = match arg_data.ty().bytewidth() {
                                    2 => FMvFmt::H,
                                    4 => FMvFmt::S,
                                    8 => FMvFmt::D,
                                    _ => unimplemented!(),
                                };
                                let fmv = InstData::build_fmv(
                                    &mut self.machine_ctx,
                                    dst_fmt,
                                    FMvFmt::X,
                                    rd,
                                    zero,
                                );
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(fmv, self.block_map[&block])
                                    .unwrap();
                            } else if arg_data.ty().is_int() || arg_data.ty().is_ptr() {
                                let mv = InstData::build_gp_move(&mut self.machine_ctx, rd, zero);
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(mv, self.block_map[&block])
                                    .unwrap();
                            } else {
                                unreachable!()
                            }
                        }
                        ValueKind::Bytes(bytes) => {
                            let imm: Immediate = bytes.into();
                            if arg_data.ty().is_float() {
                                let (tmp, li) = InstData::new_li(&mut self.machine_ctx, imm);
                                let mv = InstData::build_fp_move(&mut self.machine_ctx, rd, tmp);
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(li, self.block_map[&block])
                                    .unwrap();
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(mv, self.block_map[&block])
                                    .unwrap();
                            } else if arg_data.ty().is_int() || arg_data.ty().is_ptr() {
                                let li = InstData::build_li(&mut self.machine_ctx, rd, imm);
                                machine_function_layout!(mut self.machine_ctx, &function_name)
                                    .append_inst(li, self.block_map[&block])
                                    .unwrap();
                            } else {
                                unreachable!()
                            }
                        }
                        _ => {
                            let codegen_result = self.get_value(*arg);
                            let rs = if let ValueCodegenResult::Register(reg) = codegen_result {
                                *reg
                            } else {
                                unreachable!();
                            };
                            let mv = if arg_data.ty().is_float() {
                                InstData::build_fp_move(&mut self.machine_ctx, rd, rs)
                            } else if arg_data.ty().is_int() || arg_data.ty().is_ptr() {
                                InstData::build_gp_move(&mut self.machine_ctx, rd, rs)
                            } else {
                                unreachable!();
                            };
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(mv, self.block_map[&block])
                                .unwrap();
                        }
                    }
                }

                // j $else_block
                let j = InstData::new_j(&mut self.machine_ctx, self.block_map[&else_block]);
                machine_function_layout!(mut self.machine_ctx, &function_name)
                    .append_inst(j, self.block_map[&block])
                    .unwrap();
            }
            ValueKind::GetElemPtr(gep) => {
                let ptr = gep.ptr();
                let reg = match self.get_value(ptr) {
                    ValueCodegenResult::Register(reg) => *reg,
                    ValueCodegenResult::StackSlot { base, offset } => {
                        // add base and offset
                        let base = *base;
                        let offset = *offset;
                        if check_itype_imm(offset) {
                            let (rd, addi) = InstData::new_binary_imm(
                                &mut self.machine_ctx,
                                MachineBinaryImmOp::Addi,
                                base,
                                offset,
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(addi, self.block_map[&block])
                                .unwrap();
                            rd
                        } else {
                            // li
                            let (rd, li) = InstData::new_li(&mut self.machine_ctx, offset);
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(li, self.block_map[&block])
                                .unwrap();
                            // add
                            let (rd, add) = InstData::new_binary(
                                &mut self.machine_ctx,
                                MachineBinaryOp::Add,
                                base,
                                rd,
                            );
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(add, self.block_map[&block])
                                .unwrap();
                            rd
                        }
                    }
                    ValueCodegenResult::MachineSymbol(symbol) => {
                        // la
                        let symbol = symbol.clone();
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
                };

                let indices = gep.indices();
                let mut basis_ty = gep.ty();
                let mut ptr_reg = reg;

                for index in indices {
                    let bytewidth = basis_ty.bytewidth();
                    let index_data = function_data.dfg().local_value_data(*index).unwrap();

                    let shamt = if bytewidth.is_power_of_two() {
                        Some(bytewidth.trailing_zeros())
                    } else {
                        None
                    };

                    match index_data.kind() {
                        ValueKind::GlobalSlot(_)
                        | ValueKind::Array(_)
                        | ValueKind::Struct(_)
                        | ValueKind::Function
                        | ValueKind::Store(_)
                        | ValueKind::Jump(_)
                        | ValueKind::Branch(_)
                        | ValueKind::Return(_) => unreachable!(),
                        ValueKind::Zero | ValueKind::Undef => {
                            // $ptr + 0 * bytewidth = $ptr
                            // no need to do anything
                        }
                        ValueKind::Bytes(bytes) => {
                            let (rd, li) = InstData::new_li(&mut self.machine_ctx, bytes.into());
                            machine_function_layout!(mut self.machine_ctx, &function_name)
                                .append_inst(li, self.block_map[&block])
                                .unwrap();
                            match shamt {
                                Some(shamt) => {
                                    // mul can be reduced to slli
                                    let (rd, slli) = InstData::new_binary_imm(
                                        &mut self.machine_ctx,
                                        MachineBinaryImmOp::Slli,
                                        rd,
                                        shamt.into(),
                                    );
                                    machine_function_layout!(mut self.machine_ctx, &function_name)
                                        .append_inst(slli, self.block_map[&block])
                                        .unwrap();
                                    // add
                                    let (rd, add) = InstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Add,
                                        ptr_reg,
                                        rd,
                                    );
                                    machine_function_layout!(mut self.machine_ctx, &function_name)
                                        .append_inst(add, self.block_map[&block])
                                        .unwrap();
                                    ptr_reg = rd;
                                }
                                None => {
                                    // li + mul
                                    let (rd, li) =
                                        InstData::new_li(&mut self.machine_ctx, bytewidth.into());
                                    machine_function_layout!(mut self.machine_ctx, &function_name)
                                        .append_inst(li, self.block_map[&block])
                                        .unwrap();
                                    let (rd, mul) = InstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Mul,
                                        rd,
                                        rd,
                                    );
                                    machine_function_layout!(mut self.machine_ctx, &function_name)
                                        .append_inst(mul, self.block_map[&block])
                                        .unwrap();
                                    // add
                                    let (rd, add) = InstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Add,
                                        ptr_reg,
                                        rd,
                                    );
                                    machine_function_layout!(mut self.machine_ctx, &function_name)
                                        .append_inst(add, self.block_map[&block])
                                        .unwrap();
                                    ptr_reg = rd;
                                }
                            }
                        }
                        _ => {
                            let codegen_result = self.get_value(*index);
                            let rs = if let ValueCodegenResult::Register(reg) = codegen_result {
                                *reg
                            } else {
                                unreachable!();
                            };
                            match shamt {
                                Some(shamt) => {
                                    // slli
                                    let (rs, slli) = InstData::new_binary_imm(
                                        &mut self.machine_ctx,
                                        MachineBinaryImmOp::Slli,
                                        rs,
                                        shamt.into(),
                                    );
                                    machine_function_layout!(mut self.machine_ctx, &function_name)
                                        .append_inst(slli, self.block_map[&block])
                                        .unwrap();
                                    // add
                                    let (rd, add) = InstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Add,
                                        ptr_reg,
                                        rs,
                                    );
                                    machine_function_layout!(mut self.machine_ctx, &function_name)
                                        .append_inst(add, self.block_map[&block])
                                        .unwrap();
                                    ptr_reg = rd;
                                }
                                None => {
                                    // mul
                                    let (rs, mul) = InstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Mul,
                                        rs,
                                        rs,
                                    );
                                    machine_function_layout!(mut self.machine_ctx, &function_name)
                                        .append_inst(mul, self.block_map[&block])
                                        .unwrap();
                                    // add
                                    let (rd, add) = InstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Add,
                                        ptr_reg,
                                        rs,
                                    );
                                    machine_function_layout!(mut self.machine_ctx, &function_name)
                                        .append_inst(add, self.block_map[&block])
                                        .unwrap();
                                    ptr_reg = rd;
                                }
                            }
                        }
                    }

                    basis_ty = match basis_ty.kind() {
                        TypeKind::Array(_, elem_ty) => elem_ty.clone(),
                        TypeKind::Struct(_) => unimplemented!(),
                        TypeKind::Ptr => unreachable!(), // there is no bound type anymore
                        _ => unreachable!(),
                    };
                }

                self.value_map
                    .insert(inst.into(), ValueCodegenResult::Register(ptr_reg));
            }
            ValueKind::Call(call) => {
                todo!()
            }
            ValueKind::Return(ret) => {
                let val = ret.val();
                if let Some(val) = val {
                    let val_data = function_data.dfg().local_value_data(val).unwrap();

                    let reg = if let ValueCodegenResult::Register(reg) = self.get_value(val) {
                        *reg
                    } else {
                        unreachable!();
                    };

                    if val_data.ty().is_float() {
                        let fa0 = self.machine_ctx.new_fp_reg(RiscvFloatingPointRegister::Fa0);
                        let mv = InstData::build_fp_move(&mut self.machine_ctx, fa0, reg);
                        machine_function_layout!(mut self.machine_ctx, &function_name)
                            .append_inst(mv, self.block_map[&block])
                            .unwrap();
                    } else if val_data.ty().is_int() || val_data.ty().is_ptr() {
                        let a0 = self.machine_ctx.new_gp_reg(RiscvGeneralPurposeRegister::A0);
                        let mv = InstData::build_gp_move(&mut self.machine_ctx, a0, reg);
                        machine_function_layout!(mut self.machine_ctx, &function_name)
                            .append_inst(mv, self.block_map[&block])
                            .unwrap();
                    } else {
                        unreachable!()
                    }
                }
                let ret = InstData::new_ret(&mut self.machine_ctx);
                machine_function_layout!(mut self.machine_ctx, &function_name)
                    .append_inst(ret, self.block_map[&block])
                    .unwrap();
            }
            ValueKind::BlockParam
            | ValueKind::Function
            | ValueKind::Zero
            | ValueKind::Undef
            | ValueKind::Bytes(_)
            | ValueKind::GlobalSlot(_)
            | ValueKind::Struct(_)
            | ValueKind::Array(_) => unreachable!(),
        }

        todo!()
    }
}

fn check_itype_imm(imm: Immediate) -> bool {
    if imm.0 > i32::MAX as i128 || imm.0 < i32::MIN as i128 {
        return false;
    }
    let imm = imm.0 as i32;
    // -0x800 is excluded for the correctness negation
    (-0x7ff..=0x7ff).contains(&imm)
}
