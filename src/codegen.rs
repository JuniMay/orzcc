use std::collections::HashMap;

use crate::{
    backend::{
        FCvtFmt,
        FMvFmt,
        FloatLoadKind,
        FloatPseudoLoadKind,
        FloatPseudoStoreKind,
        FloatStoreKind,
        Immediate,
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
        MachineInstData,
        MachineSymbol,
        PseudoLoadKind,
        PseudoStoreKind,
        Register,
        RiscvFpReg,
        RiscvGpReg,
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
    pub machine_ctx: MachineContext,

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

    pub fn finish(self) -> MachineContext { self.machine_ctx }

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
                                                // zero fill inner to match the bytewidth
                                                for i in 0..data.ty().bytewidth() {
                                                    bytes.push(inner.get(i).copied().unwrap_or(0));
                                                }
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
                    unreachable!("global slot value is not a global slot: {:?}", data.kind());
                }
            });
        }

        for function in module.function_layout() {
            let symbol: MachineSymbol = module.value_name((*function).into()).into();
            self.value_map.insert(
                (*function).into(),
                ValueCodegenResult::MachineSymbol(symbol.clone()),
            );
            // insert first to make sure the function symbol is available
            if let FunctionKind::Definition = module.function_data(*function).unwrap().kind() {
                self.machine_ctx.new_function(symbol);
            }
        }

        for function in module.function_layout() {
            if let FunctionKind::Definition = module.function_data(*function).unwrap().kind() {
                self.codegen_function(module, *function);
            }
        }
    }

    pub fn codegen_rest(&mut self, module: &Module) {
        for function in module.function_layout() {
            if let FunctionKind::Definition = module.function_data(*function).unwrap().kind() {
                self.codegen_function_prologue(module, *function);
                self.codegen_function_epilogue(module, *function);
            }
        }
    }

    pub fn codegen_function(&mut self, module: &Module, function: Function) {
        let function_data = module.function_data(function).unwrap();
        let function_name: MachineSymbol = module.value_name(function.into()).into();

        for (block, _) in function_data.layout.blocks() {
            let machine_block = self.machine_ctx.new_block();
            dbg!(machine_block);
            self.block_map.insert(block, machine_block);

            machine_function_layout!(mut self.machine_ctx, &function_name)
                .append_block(machine_block)
                .unwrap();
        }

        // assign register for all block arguments for argument passing in branch/jump
        // instructions.
        for block in function_data.layout.blocks() {
            let block_args = function_data.dfg.block_data(block.0).unwrap().params();
            for arg in block_args {
                let arg_data = function_data.dfg.local_value_data(*arg).unwrap();

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

        let entry_block = function_data.layout.entry_block().unwrap();

        let block_args = function_data.dfg.block_data(entry_block).unwrap().params();

        let mut integer_arg_count = 0;
        let mut float_arg_count = 0;
        let mut args_passed_by_stack = Vec::new();

        for arg in block_args {
            let value_data = function_data.dfg.local_value_data(*arg).unwrap();
            let ty = value_data.ty();

            if ty.is_float() {
                if float_arg_count <= 7 {
                    let rd = self.get_value_as_register(*arg);
                    let rs = self
                        .machine_ctx
                        .new_fp_reg((RiscvFpReg::Fa0 as u8 + float_arg_count).into());
                    let fmv = MachineInstData::build_fp_move(&mut self.machine_ctx, rd, rs);

                    self.append_inst(&function_name, entry_block, fmv);

                    float_arg_count += 1;
                } else {
                    args_passed_by_stack.push(*arg);
                }
            } else if ty.is_int() || ty.is_ptr() {
                if integer_arg_count <= 7 {
                    let rd = self.get_value_as_register(*arg);

                    let rs = self
                        .machine_ctx
                        .new_gp_reg((RiscvGpReg::A0 as u8 + integer_arg_count).into());

                    let mv = MachineInstData::build_gp_move(&mut self.machine_ctx, rd, rs);

                    self.append_inst(&function_name, entry_block, mv);

                    integer_arg_count += 1;
                } else {
                    args_passed_by_stack.push(*arg);
                }
            } else {
                unimplemented!("non-integer, non-float argument");
            }
        }

        let mut offset = 0;
        // according to the calling convention, the sp points to the first parameter not
        // passed by register, before use the frame pointer/s0, fp should be
        // saved and assign to the original sp. clang might generate the code
        // below:
        // ```
        // addi sp, sp, -(aligned_frame_size)
        // sd ra, (aligned_frame_size - 8)(sp)
        // sd s0, (aligned_frame_size - 16)(sp)
        // addi fp, sp, aligned_frame_size
        // ```
        // and then the fp can be used to index the parameters passed by stack from
        // offset 0.
        let fp = self.machine_ctx.new_gp_reg(RiscvGpReg::S0);
        // load the rest argument into the registers.
        // the loaded value might be spilled again, but that is a problem for register
        // allocation.
        for arg in args_passed_by_stack {
            let arg_data = function_data.dfg.local_value_data(arg).unwrap();
            let rd = self.get_value_as_register(arg);

            if arg_data.ty().is_float() {
                let kind = match arg_data.ty().bytewidth() {
                    4 => FloatLoadKind::Single,
                    8 => FloatLoadKind::Double,
                    _ => unimplemented!(),
                };
                let load = MachineInstData::build_float_load(
                    &mut self.machine_ctx,
                    kind,
                    rd,
                    fp,
                    offset.into(),
                );
                self.append_inst(&function_name, entry_block, load)
            } else if arg_data.ty().is_int() || arg_data.ty().is_ptr() {
                let kind = match arg_data.ty().bytewidth() {
                    1 => LoadKind::Byte,
                    2 => LoadKind::Half,
                    4 => LoadKind::Word,
                    8 => LoadKind::DoubleWord,
                    _ => unimplemented!(),
                };
                let load =
                    MachineInstData::build_load(&mut self.machine_ctx, kind, rd, fp, offset.into());
                self.append_inst(&function_name, entry_block, load)
            } else {
                unimplemented!("non-integer, non-float argument");
            }

            offset += 8;
        }

        for (block, block_node) in function_data.layout.blocks() {
            for (inst, _inst_node) in block_node.insts() {
                self.codegen_inst(module, inst, function, block);
            }
        }
    }

    pub fn get_value(&self, value: Value) -> &ValueCodegenResult {
        self.value_map.get(&value).unwrap()
    }

    pub fn get_value_as_register(&self, value: Value) -> Register {
        if let ValueCodegenResult::Register(reg) = self.get_value(value) {
            *reg
        } else {
            panic!("value is not a register");
        }
    }

    pub fn get_value_as_symbol(&self, value: Value) -> MachineSymbol {
        if let ValueCodegenResult::MachineSymbol(symbol) = self.get_value(value) {
            symbol.clone()
        } else {
            panic!("value is not a symbol");
        }
    }

    pub fn get_value_as_stack_slot(&self, value: Value) -> (Register, Immediate) {
        if let ValueCodegenResult::StackSlot { base, offset } = self.get_value(value) {
            (*base, *offset)
        } else {
            panic!("value is not a stack slot");
        }
    }

    pub fn append_inst(&mut self, function_name: &MachineSymbol, block: Block, inst: MachineInst) {
        dbg!(self.machine_ctx.inst_data(inst));
        machine_function_layout!(mut self.machine_ctx, function_name)
            .append_inst(inst, self.block_map[&block])
            .unwrap();
    }

    pub fn prepend_inst(&mut self, function_name: &MachineSymbol, block: Block, inst: MachineInst) {
        machine_function_layout!(mut self.machine_ctx, function_name)
            .prepend_inst(inst, self.block_map[&block])
            .unwrap();
    }

    pub fn codegen_function_prologue(&mut self, module: &Module, function: Function) {
        let function_data = module.function_data(function).unwrap();

        let entry_block = function_data.layout.entry_block().unwrap();

        let function_name = self.get_value_as_symbol(function.into());

        let fp = self.machine_ctx.new_gp_reg(RiscvGpReg::S0);
        let sp = self.machine_ctx.new_gp_reg(RiscvGpReg::Sp);
        let t2 = self.machine_ctx.new_gp_reg(RiscvGpReg::T2);
        let ra = self.machine_ctx.new_gp_reg(RiscvGpReg::Ra);

        machine_function!(mut self.machine_ctx, &function_name).add_saved_reg(fp);
        // ra will be handled separately

        let saved_regs_num = machine_function!(self.machine_ctx, &function_name)
            .saved_regs()
            .len();

        // +1 for ra
        machine_function!(mut self.machine_ctx, &function_name)
            .add_stack_size((saved_regs_num + 1) * 8);
        let aligned_stack_frame_size =
            machine_function!(mut self.machine_ctx, &function_name).aligned_stack_size();
        dbg!(aligned_stack_frame_size);
        // fp is used for parameter passing, later.
        if check_itype_imm(aligned_stack_frame_size.into()) {
            let addi_fp = MachineInstData::build_binary_imm(
                &mut self.machine_ctx,
                MachineBinaryImmOp::Addi,
                fp,
                sp,
                aligned_stack_frame_size.into(),
            );
            self.prepend_inst(&function_name, entry_block, addi_fp);
        } else {
            let li = MachineInstData::build_li(
                &mut self.machine_ctx,
                t2,
                aligned_stack_frame_size.into(),
            );
            let add_fp = MachineInstData::build_binary(
                &mut self.machine_ctx,
                MachineBinaryOp::Add,
                fp,
                sp,
                t2,
            );
            self.prepend_inst(&function_name, entry_block, add_fp);
            self.prepend_inst(&function_name, entry_block, li);
        }

        let mut curr_frame_pos = aligned_stack_frame_size - 8;

        let mut insts = Vec::new();
        let saved_regs = machine_function!(self.machine_ctx, &function_name).saved_regs();
        // add ra to the start
        // idk if this is necessary, but looks pretty
        let saved_regs = vec![ra]
            .into_iter()
            .chain(saved_regs.iter().copied())
            .collect::<Vec<_>>();

        for reg in saved_regs {
            let (base, offset): (_, Immediate) = if check_itype_imm(curr_frame_pos.into()) {
                (sp, curr_frame_pos.into())
            } else {
                let li =
                    MachineInstData::build_li(&mut self.machine_ctx, t2, curr_frame_pos.into());
                let add = MachineInstData::build_binary(
                    &mut self.machine_ctx,
                    MachineBinaryOp::Add,
                    t2,
                    sp,
                    t2,
                );
                insts.push(li);
                insts.push(add);
                (t2, 0.into())
            };
            if reg.is_gp() {
                let sd = MachineInstData::new_store(
                    &mut self.machine_ctx,
                    StoreKind::DoubleWord,
                    reg,
                    base,
                    offset,
                );
                insts.push(sd);
            } else if reg.is_fp() {
                let sd = MachineInstData::new_float_store(
                    &mut self.machine_ctx,
                    FloatStoreKind::Double,
                    reg,
                    base,
                    offset,
                );
                insts.push(sd);
            } else {
                unimplemented!("save register of type {:?}", reg);
            }
            if curr_frame_pos != 0 {
                curr_frame_pos -= 8;
            } else {
                break;
            }
        }
        for inst in insts.into_iter().rev() {
            self.prepend_inst(&function_name, entry_block, inst);
        }

        if check_itype_imm(aligned_stack_frame_size.into()) {
            let addi = MachineInstData::build_binary_imm(
                &mut self.machine_ctx,
                MachineBinaryImmOp::Addi,
                sp,
                sp,
                (-(aligned_stack_frame_size as i32)).into(),
            );
            self.prepend_inst(&function_name, entry_block, addi);
        } else {
            let li = MachineInstData::build_li(
                &mut self.machine_ctx,
                t2,
                (-(aligned_stack_frame_size as i32)).into(),
            );
            let add = MachineInstData::build_binary(
                &mut self.machine_ctx,
                MachineBinaryOp::Add,
                sp,
                sp,
                t2,
            );
            self.prepend_inst(&function_name, entry_block, add);
            self.prepend_inst(&function_name, entry_block, li);
        }
    }

    pub fn codegen_function_epilogue(&mut self, module: &Module, function: Function) {
        let function_data = module.function_data(function).unwrap();

        let exit_block = function_data.layout.exit_block().unwrap();

        let function_name = self.get_value_as_symbol(function.into());

        let sp = self.machine_ctx.new_gp_reg(RiscvGpReg::Sp);
        let t2 = self.machine_ctx.new_gp_reg(RiscvGpReg::T2);
        let ra = self.machine_ctx.new_gp_reg(RiscvGpReg::Ra);

        let aligned_stack_frame_size =
            machine_function!(mut self.machine_ctx, &function_name).aligned_stack_size();
        let mut curr_frame_pos = aligned_stack_frame_size - 8;

        let saved_regs = machine_function!(self.machine_ctx, &function_name).saved_regs();
        // add ra to the start
        // idk if this is necessary, but looks pretty
        let saved_regs = vec![ra]
            .into_iter()
            .chain(saved_regs.iter().copied())
            .collect::<Vec<_>>();

        // restore all saved registers
        for reg in saved_regs {
            let (base, offset): (_, Immediate) = if check_itype_imm(curr_frame_pos.into()) {
                (sp, curr_frame_pos.into())
            } else {
                let li =
                    MachineInstData::build_li(&mut self.machine_ctx, t2, curr_frame_pos.into());
                let add = MachineInstData::build_binary(
                    &mut self.machine_ctx,
                    MachineBinaryOp::Add,
                    t2,
                    sp,
                    t2,
                );
                self.append_inst(&function_name, exit_block, li);
                self.append_inst(&function_name, exit_block, add);
                (t2, 0.into())
            };
            if reg.is_gp() {
                let ld = MachineInstData::build_load(
                    &mut self.machine_ctx,
                    LoadKind::DoubleWord,
                    reg,
                    base,
                    offset,
                );
                self.append_inst(&function_name, exit_block, ld);
            } else if reg.is_fp() {
                let ld = MachineInstData::build_float_load(
                    &mut self.machine_ctx,
                    FloatLoadKind::Double,
                    reg,
                    base,
                    offset,
                );
                self.append_inst(&function_name, exit_block, ld);
            } else {
                unimplemented!("restore register of type {:?}", reg);
            }
            if curr_frame_pos != 0 {
                curr_frame_pos -= 8;
            } else {
                break;
            }
        }

        // ret
        let ret = MachineInstData::new_ret(&mut self.machine_ctx);
        self.append_inst(&function_name, exit_block, ret);
    }

    pub fn codegen_inst(&mut self, module: &Module, inst: Inst, function: Function, block: Block) {
        let function_data = module.function_data(function).unwrap();
        let function_name: MachineSymbol = module.value_name(function.into()).into();

        let inst_data = function_data.dfg.local_value_data(inst.into()).unwrap();

        match inst_data.kind() {
            ValueKind::Alloc(alloc) => {
                let bytewidth = alloc.ty().bytewidth();
                let offset =
                    machine_function!(mut self.machine_ctx, &function_name).stack_size() as i128;
                // clang/gcc index local variables by fp/s0, but here just use sp as the base
                // pointer.
                //
                // if we use fp/s0, the offset of local variables will be changed after
                // the register allocation (beacuse some registers might be saved on the stack)
                // so we use sp as the base pointer, and the offset is positive. and by doing
                // so, the fp/s0 can be used to allocate after the argument passing.
                //
                // before codegen insts, the stack size should be zero. and when generating the
                // prologue, the stack frame size should be increased by the number of saved
                // regsiters and aligned to 16 bytes
                self.value_map.insert(
                    inst.into(),
                    ValueCodegenResult::StackSlot {
                        base: self.machine_ctx.new_gp_reg(RiscvGpReg::Sp),
                        offset: offset.into(),
                    },
                );
                machine_function!(mut self.machine_ctx, &function_name).add_stack_size(bytewidth);
                // not appending here, this should be handled in the prologue
                // (and epilogue for deallocation)
            }
            ValueKind::Store(store) => {
                let val: Register = function_data
                    .dfg
                    .with_value_data(store.val(), |data| match data.kind() {
                        ValueKind::Zero | ValueKind::Undef => {
                            self.machine_ctx.new_gp_reg(RiscvGpReg::Zero)
                        }
                        ValueKind::GlobalSlot(_) | ValueKind::Function => {
                            let symbol = self.get_value_as_symbol(store.val());
                            let (rd, la) = MachineInstData::new_pseudo_load(
                                &mut self.machine_ctx,
                                PseudoLoadKind::Address,
                                symbol,
                            );
                            self.append_inst(&function_name, block, la);
                            rd
                        }
                        ValueKind::Array(_) | ValueKind::Struct(_) => unimplemented!(),
                        ValueKind::Bytes(bytes) => {
                            let imm = bytes.iter().fold(0, |acc, &byte| (acc << 8) | byte as u64);
                            let (rd, li) =
                                MachineInstData::new_li(&mut self.machine_ctx, imm.into());
                            self.append_inst(&function_name, block, li);
                            rd
                        }
                        ValueKind::Alloc(_) => {
                            let (base, offset) = self.get_value_as_stack_slot(store.val());
                            let (rd, addi) = MachineInstData::new_binary_imm(
                                &mut self.machine_ctx,
                                MachineBinaryImmOp::Addi,
                                base,
                                offset,
                            );
                            self.append_inst(&function_name, block, addi);
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
                    .dfg
                    .with_value_data(store.ptr(), |data| data.ty())
                    .unwrap();

                function_data
                    .dfg
                    .with_value_data(store.ptr(), |data| match data.kind() {
                        ValueKind::GlobalSlot(_) => {
                            let symbol = self.get_value_as_symbol(store.ptr());

                            let rt = self
                                .machine_ctx
                                .new_virtual_reg(VirtualRegisterKind::General);

                            let store = if ty.is_float() {
                                let kind = match ty.bytewidth() {
                                    4 => FloatPseudoStoreKind::Single,
                                    _ => unimplemented!(),
                                };
                                MachineInstData::new_float_pseudo_store(
                                    &mut self.machine_ctx,
                                    kind,
                                    val,
                                    symbol,
                                    rt,
                                )
                            } else if ty.is_int() || ty.is_ptr() {
                                let kind = match ty.bytewidth() {
                                    4 => PseudoStoreKind::Word,
                                    8 => PseudoStoreKind::DoubleWord,
                                    _ => unimplemented!(),
                                };
                                MachineInstData::new_pseudo_store(
                                    &mut self.machine_ctx,
                                    kind,
                                    val,
                                    symbol,
                                    rt,
                                )
                            } else {
                                unreachable!()
                            };

                            self.append_inst(&function_name, block, store);
                        }
                        ValueKind::Alloc(_) => {
                            let (base, offset) = self.get_value_as_stack_slot(store.ptr());
                            let store = if ty.is_float() {
                                let kind = match ty.bytewidth() {
                                    4 => FloatStoreKind::Single,
                                    8 => FloatStoreKind::Double,
                                    _ => unimplemented!(),
                                };
                                MachineInstData::new_float_store(
                                    &mut self.machine_ctx,
                                    kind,
                                    val,
                                    base,
                                    offset,
                                )
                            } else if ty.is_int() || ty.is_ptr() {
                                let kind = match ty.bytewidth() {
                                    1 => StoreKind::Byte,
                                    2 => StoreKind::Half,
                                    4 => StoreKind::Word,
                                    8 => StoreKind::DoubleWord,
                                    _ => unimplemented!(),
                                };
                                MachineInstData::new_store(
                                    &mut self.machine_ctx,
                                    kind,
                                    val,
                                    base,
                                    offset,
                                )
                            } else {
                                unreachable!()
                            };

                            self.append_inst(&function_name, block, store);
                        }
                        ValueKind::GetElemPtr(_) => {
                            let reg = self.get_value_as_register(store.ptr());

                            let store = if ty.is_float() {
                                let kind = match ty.bytewidth() {
                                    4 => FloatStoreKind::Single,
                                    8 => FloatStoreKind::Double,
                                    _ => unimplemented!(),
                                };
                                MachineInstData::new_float_store(
                                    &mut self.machine_ctx,
                                    kind,
                                    val,
                                    reg,
                                    0.into(),
                                )
                            } else if ty.is_int() || ty.is_ptr() {
                                let kind = match ty.bytewidth() {
                                    1 => StoreKind::Byte,
                                    2 => StoreKind::Half,
                                    4 => StoreKind::Word,
                                    8 => StoreKind::DoubleWord,
                                    _ => unimplemented!(),
                                };
                                MachineInstData::new_store(
                                    &mut self.machine_ctx,
                                    kind,
                                    val,
                                    reg,
                                    0.into(),
                                )
                            } else {
                                unreachable!()
                            };

                            self.append_inst(&function_name, block, store);
                        }
                        _ => unimplemented!(),
                    });
            }
            ValueKind::Load(load) => {
                let ptr = load.ptr();
                let ty = function_data
                    .dfg
                    .local_value_data(inst.into())
                    .unwrap()
                    .ty();

                function_data
                    .dfg
                    .with_value_data(ptr, |data| match data.kind() {
                        ValueKind::GlobalSlot(_) => {
                            let symbol = self.get_value_as_symbol(ptr);
                            let rt = self
                                .machine_ctx
                                .new_virtual_reg(VirtualRegisterKind::General);

                            let (rd, load) = if ty.is_float() {
                                let kind = match ty.bytewidth() {
                                    4 => FloatPseudoLoadKind::Single,
                                    _ => unimplemented!(),
                                };
                                MachineInstData::new_float_pseudo_load(
                                    &mut self.machine_ctx,
                                    kind,
                                    symbol,
                                    rt,
                                )
                            } else if ty.is_int() || ty.is_ptr() {
                                let kind = match ty.bytewidth() {
                                    4 => PseudoLoadKind::Word,
                                    _ => unimplemented!(),
                                };
                                MachineInstData::new_pseudo_load(
                                    &mut self.machine_ctx,
                                    kind,
                                    symbol,
                                )
                            } else {
                                unreachable!()
                            };

                            self.append_inst(&function_name, block, load);

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        ValueKind::Alloc(_) => {
                            let (base, offset) = self.get_value_as_stack_slot(ptr);
                            let (rd, load) = if ty.is_float() {
                                let kind = match ty.bytewidth() {
                                    4 => FloatLoadKind::Single,
                                    8 => FloatLoadKind::Double,
                                    _ => unimplemented!(),
                                };
                                MachineInstData::new_float_load(
                                    &mut self.machine_ctx,
                                    kind,
                                    base,
                                    offset,
                                )
                            } else if ty.is_int() || ty.is_ptr() {
                                let kind = match ty.bytewidth() {
                                    1 => LoadKind::Byte,
                                    2 => LoadKind::Half,
                                    4 => LoadKind::Word,
                                    8 => LoadKind::DoubleWord,
                                    _ => unimplemented!(),
                                };
                                MachineInstData::new_load(&mut self.machine_ctx, kind, base, offset)
                            } else {
                                unreachable!()
                            };

                            self.append_inst(&function_name, block, load);

                            self.value_map
                                .insert(inst.into(), ValueCodegenResult::Register(rd));
                        }
                        ValueKind::GetElemPtr(_) => {
                            let reg = self.get_value_as_register(ptr);

                            let (rd, load) = if ty.is_float() {
                                let kind = match ty.bytewidth() {
                                    4 => FloatLoadKind::Single,
                                    8 => FloatLoadKind::Double,
                                    _ => unimplemented!(),
                                };
                                MachineInstData::new_float_load(
                                    &mut self.machine_ctx,
                                    kind,
                                    reg,
                                    0.into(),
                                )
                            } else if ty.is_int() || ty.is_ptr() {
                                let kind = match ty.bytewidth() {
                                    1 => LoadKind::Byte,
                                    2 => LoadKind::Half,
                                    4 => LoadKind::Word,
                                    8 => LoadKind::DoubleWord,
                                    _ => unimplemented!(),
                                };
                                MachineInstData::new_load(
                                    &mut self.machine_ctx,
                                    kind,
                                    reg,
                                    0.into(),
                                )
                            } else {
                                unreachable!()
                            };

                            self.append_inst(&function_name, block, load);

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

                let lhs_data = function_data.dfg.local_value_data(lhs).unwrap();
                let rhs_data = function_data.dfg.local_value_data(rhs).unwrap();

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
                            let dst_fmt = FMvFmt::from_byte_width(lhs_data.ty().bytewidth());
                            let zero = self.machine_ctx.new_gp_reg(RiscvGpReg::Zero);
                            let (rd, fmv) = MachineInstData::new_float_move(
                                &mut self.machine_ctx,
                                dst_fmt,
                                FMvFmt::X,
                                zero,
                            );
                            self.append_inst(&function_name, block, fmv);
                            rd
                        }
                        ValueKind::Bytes(bytes) => {
                            let imm: Immediate = bytes.into();
                            let (rd, li) = MachineInstData::new_li(&mut self.machine_ctx, imm);
                            let dst_fmt = FMvFmt::from_byte_width(lhs_data.ty().bytewidth());
                            let (rd, fmv) = MachineInstData::new_float_move(
                                &mut self.machine_ctx,
                                dst_fmt,
                                FMvFmt::X,
                                rd,
                            );
                            self.append_inst(&function_name, block, li);
                            self.append_inst(&function_name, block, fmv);

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
                            let dst_fmt = FMvFmt::from_byte_width(lhs_data.ty().bytewidth());
                            let zero = self.machine_ctx.new_gp_reg(RiscvGpReg::Zero);
                            let (rd, fmv) = MachineInstData::new_float_move(
                                &mut self.machine_ctx,
                                dst_fmt,
                                FMvFmt::X,
                                zero,
                            );
                            self.append_inst(&function_name, block, fmv);
                            rd
                        }
                        ValueKind::Bytes(bytes) => {
                            let imm: Immediate = bytes.into();
                            let (rd, li) = MachineInstData::new_li(&mut self.machine_ctx, imm);
                            let dst_fmt = FMvFmt::from_byte_width(lhs_data.ty().bytewidth());
                            let (rd, fmv) = MachineInstData::new_float_move(
                                &mut self.machine_ctx,
                                dst_fmt,
                                FMvFmt::X,
                                rd,
                            );
                            self.append_inst(&function_name, block, li);
                            self.append_inst(&function_name, block, fmv);

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

                    let (rd, fbin) = MachineInstData::new_float_binary(
                        &mut self.machine_ctx,
                        asm_op,
                        fmt,
                        rs1,
                        rs2,
                    );

                    self.append_inst(&function_name, block, fbin);

                    if let BinaryOp::FCmp(FCmpCond::ONe) = op {
                        // invert the result of `feq`
                        let (rd, neg) = MachineInstData::new_binary_imm(
                            &mut self.machine_ctx,
                            MachineBinaryImmOp::Xori,
                            rd,
                            (-1).into(),
                        );
                        self.append_inst(&function_name, block, neg);

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
                                let (rd, li) = MachineInstData::new_li(&mut self.machine_ctx, imm);
                                self.append_inst(&function_name, block, li);
                                BinaryOperand::Register(rd)
                            }
                        }
                        _ => BinaryOperand::Register(self.get_value_as_register(lhs)),
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
                                let (rd, li) = MachineInstData::new_li(&mut self.machine_ctx, imm);
                                self.append_inst(&function_name, block, li);
                                BinaryOperand::Register(rd)
                            }
                        }
                        _ => BinaryOperand::Register(self.get_value_as_register(rhs)),
                    };

                    if let BinaryOperand::Immediate(_) = rs1 {
                        // swap rs1 and rs2 so that rs1 is always a register (unless both are
                        // immediates)
                        std::mem::swap(&mut rs1, &mut rs2);
                    }

                    if let BinaryOperand::Immediate(imm) = rs1 {
                        // load immediate into register
                        // this should actually be handled with constant folding, but just in case.
                        let (rd, li) = MachineInstData::new_li(&mut self.machine_ctx, imm);
                        self.append_inst(&function_name, block, li);
                        rs1 = BinaryOperand::Register(rd);
                    }

                    match (op, rs1, rs2) {
                        (
                            op @ (BinaryOp::Add
                            | BinaryOp::Sub
                            | BinaryOp::Or
                            | BinaryOp::And
                            | BinaryOp::Xor
                            | BinaryOp::Shl
                            | BinaryOp::LShr
                            | BinaryOp::AShr),
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Immediate(imm),
                        ) => {
                            self.codegen_binary_imm(
                                module,
                                function,
                                block,
                                inst,
                                op,
                                rs1,
                                imm,
                                lhs_data.ty().bytewidth(),
                            );
                        }
                        (
                            op @ (BinaryOp::Add
                            | BinaryOp::Sub
                            | BinaryOp::Mul
                            | BinaryOp::UDiv
                            | BinaryOp::SDiv
                            | BinaryOp::URem
                            | BinaryOp::SRem
                            | BinaryOp::And
                            | BinaryOp::Or
                            | BinaryOp::Xor
                            | BinaryOp::Shl
                            | BinaryOp::LShr
                            | BinaryOp::AShr),
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Register(rs2),
                        ) => {
                            self.codegen_bianry_reg(
                                module,
                                function,
                                block,
                                inst,
                                op,
                                rs1,
                                rs2,
                                lhs_data.ty().bytewidth(),
                            );
                        }
                        (
                            op @ (BinaryOp::Mul
                            | BinaryOp::UDiv
                            | BinaryOp::SDiv
                            | BinaryOp::URem
                            | BinaryOp::SRem),
                            BinaryOperand::Register(rs1),
                            BinaryOperand::Immediate(imm),
                        ) => {
                            self.codegen_binary_imm_li(
                                module,
                                function,
                                block,
                                inst,
                                op,
                                rs1,
                                imm,
                                lhs_data.ty().bytewidth(),
                            );
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
                                let (rd, addi) = MachineInstData::new_binary_imm(
                                    &mut self.machine_ctx,
                                    kind,
                                    rs1,
                                    imm.into(),
                                );

                                self.append_inst(&function_name, block, addi);

                                if let ICmpCond::Eq = cond {
                                    let (rd, seqz) = MachineInstData::new_binary_imm(
                                        &mut self.machine_ctx,
                                        MachineBinaryImmOp::Sltiu,
                                        rd,
                                        1.into(),
                                    );
                                    self.append_inst(&function_name, block, seqz);

                                    self.value_map
                                        .insert(inst.into(), ValueCodegenResult::Register(rd));
                                } else {
                                    let zero = self.machine_ctx.new_gp_reg(RiscvGpReg::Zero);
                                    let (rd, snez) = MachineInstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Sltu,
                                        zero,
                                        rd,
                                    );
                                    self.append_inst(&function_name, block, snez);

                                    self.value_map
                                        .insert(inst.into(), ValueCodegenResult::Register(rd));
                                }
                            }
                            ICmpCond::Slt => {
                                let (rd, slti) = MachineInstData::new_binary_imm(
                                    &mut self.machine_ctx,
                                    MachineBinaryImmOp::Slti,
                                    rs1,
                                    imm,
                                );
                                self.append_inst(&function_name, block, slti);
                                self.value_map
                                    .insert(inst.into(), ValueCodegenResult::Register(rd));
                            }
                            ICmpCond::Sle => {
                                // lhs <= rhs <=> !(lhs > rhs) <=> !(rhs < lhs)
                                // needs to be loaded into a register
                                let (rs2, li) = MachineInstData::new_li(&mut self.machine_ctx, imm);
                                self.append_inst(&function_name, block, li);

                                let (rd, slt) = MachineInstData::new_binary(
                                    &mut self.machine_ctx,
                                    MachineBinaryOp::Slt,
                                    rs2,
                                    rs1,
                                );
                                self.append_inst(&function_name, block, slt);

                                let (rd, xori) = MachineInstData::new_binary_imm(
                                    &mut self.machine_ctx,
                                    MachineBinaryImmOp::Xori,
                                    rd,
                                    (-1).into(),
                                );

                                self.append_inst(&function_name, block, xori);

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
                                let (rd, sub) = MachineInstData::new_binary(
                                    &mut self.machine_ctx,
                                    MachineBinaryOp::Sub,
                                    rs1,
                                    rs2,
                                );

                                self.append_inst(&function_name, block, sub);

                                if let ICmpCond::Eq = cond {
                                    let (rd, seqz) = MachineInstData::new_binary_imm(
                                        &mut self.machine_ctx,
                                        MachineBinaryImmOp::Sltiu,
                                        rd,
                                        1.into(),
                                    );
                                    self.append_inst(&function_name, block, seqz);

                                    self.value_map
                                        .insert(inst.into(), ValueCodegenResult::Register(rd));
                                } else {
                                    let zero = self.machine_ctx.new_gp_reg(RiscvGpReg::Zero);
                                    let (rd, snez) = MachineInstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Sltu,
                                        zero,
                                        rd,
                                    );
                                    self.append_inst(&function_name, block, snez);

                                    self.value_map
                                        .insert(inst.into(), ValueCodegenResult::Register(rd));
                                }
                            }
                            ICmpCond::Slt => {
                                let (rd, slt) = MachineInstData::new_binary(
                                    &mut self.machine_ctx,
                                    MachineBinaryOp::Slt,
                                    rs1,
                                    rs2,
                                );
                                self.append_inst(&function_name, block, slt);
                                self.value_map
                                    .insert(inst.into(), ValueCodegenResult::Register(rd));
                            }
                            ICmpCond::Sle => {
                                let (rd, slt) = MachineInstData::new_binary(
                                    &mut self.machine_ctx,
                                    MachineBinaryOp::Slt,
                                    rs2,
                                    rs1,
                                );
                                self.append_inst(&function_name, block, slt);

                                let (rd, xori) = MachineInstData::new_binary_imm(
                                    &mut self.machine_ctx,
                                    MachineBinaryImmOp::Xori,
                                    rd,
                                    (-1).into(),
                                );

                                self.append_inst(&function_name, block, xori);

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

                let val_data = function_data.dfg.local_value_data(val).unwrap();

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
                        let zero = self.machine_ctx.new_gp_reg(RiscvGpReg::Zero);
                        self.value_map
                            .insert(inst.into(), ValueCodegenResult::Register(zero));
                        zero
                    }
                    ValueKind::Bytes(bytes) => {
                        let imm: Immediate = bytes.into();
                        let (rd, li) = MachineInstData::new_li(&mut self.machine_ctx, imm);
                        self.append_inst(&function_name, block, li);
                        rd
                    }
                    _ => self.get_value_as_register(val),
                };

                match op {
                    UnaryOp::FNeg => {
                        // fmv
                        let dst_fmt = FMvFmt::from_byte_width(val_data.ty().bytewidth());

                        let (rd, fmv) = MachineInstData::new_float_move(
                            &mut self.machine_ctx,
                            dst_fmt,
                            FMvFmt::X,
                            operand,
                        );

                        self.append_inst(&function_name, block, fmv);

                        // fneg => fsgnjn
                        let fmt = match val_data.ty().bytewidth() {
                            4 => MachineFloatBinaryFmt::S,
                            8 => MachineFloatBinaryFmt::D,
                            _ => unimplemented!(),
                        };

                        let (rd, fsgnjn) = MachineInstData::new_float_binary(
                            &mut self.machine_ctx,
                            MachineFloatBinaryOp::Fsgnjn,
                            fmt,
                            rd,
                            rd,
                        );

                        self.append_inst(&function_name, block, fsgnjn);

                        self.value_map
                            .insert(inst.into(), ValueCodegenResult::Register(rd));
                    }
                    UnaryOp::Not => {
                        let bitwidth = val_data.ty().bitwidth();
                        // - bitwidth <= 11: exactly the number of 1s.
                        // - bitwidth >= 12: signext will lead to all 1s, so a correction is needed

                        let mask = (1 << bitwidth) - 1;

                        match bitwidth {
                            0..=11 => {
                                let (rd, xori) = MachineInstData::new_binary_imm(
                                    &mut self.machine_ctx,
                                    MachineBinaryImmOp::Xori,
                                    operand,
                                    mask.into(),
                                );

                                self.append_inst(&function_name, block, xori);

                                self.value_map
                                    .insert(inst.into(), ValueCodegenResult::Register(rd));
                            }
                            12.. => {
                                // XXX: not sure if this is the optimal solution, but in the IR
                                // generated from SysY, only i1 will be used in Not, so the compiler
                                // should not reach here.
                                let (rd, xori) = MachineInstData::new_binary_imm(
                                    &mut self.machine_ctx,
                                    MachineBinaryImmOp::Xori,
                                    operand,
                                    (-1).into(),
                                );

                                self.append_inst(&function_name, block, xori);

                                // li + and
                                let (mask, li) =
                                    MachineInstData::new_li(&mut self.machine_ctx, mask.into());
                                self.append_inst(&function_name, block, li);

                                let (rd, and) = MachineInstData::new_binary(
                                    &mut self.machine_ctx,
                                    MachineBinaryOp::And,
                                    rd,
                                    mask,
                                );
                                self.append_inst(&function_name, block, and);

                                self.value_map
                                    .insert(inst.into(), ValueCodegenResult::Register(rd));
                            }
                        }
                    }
                }
            }
            ValueKind::Cast(cast) => {
                // TODO
                let op = cast.op();
                let val = cast.val();

                let val_data = function_data.dfg.local_value_data(val).unwrap();

                // converting zero to anything is still zero, so we can just return zero
                if let ValueKind::Zero | ValueKind::Undef = val_data.kind() {
                    let zero = self.machine_ctx.new_gp_reg(RiscvGpReg::Zero);
                    self.value_map
                        .insert(inst.into(), ValueCodegenResult::Register(zero));
                }

                let rs = match val_data.kind() {
                    ValueKind::GlobalSlot(_)
                    | ValueKind::Array(_)
                    | ValueKind::Struct(_)
                    | ValueKind::Function
                    | ValueKind::Store(_)
                    | ValueKind::Jump(_)
                    | ValueKind::Branch(_)
                    | ValueKind::Return(_)
                    | ValueKind::Alloc(_) => unreachable!(),
                    ValueKind::Zero | ValueKind::Undef => {
                        let zero = self.machine_ctx.new_gp_reg(RiscvGpReg::Zero);
                        self.value_map
                            .insert(inst.into(), ValueCodegenResult::Register(zero));
                        zero
                    }
                    ValueKind::Bytes(bytes) => {
                        let imm: Immediate = bytes.into();
                        match val_data.ty() {
                            ty if ty.is_int() => {
                                let (rd, li) = MachineInstData::new_li(&mut self.machine_ctx, imm);
                                self.append_inst(&function_name, block, li);
                                rd
                            }
                            ty if ty.is_float() => {
                                // li and fmv
                                let (rd, li) = MachineInstData::new_li(&mut self.machine_ctx, imm);
                                self.append_inst(&function_name, block, li);

                                let dst_fmt = FMvFmt::from_byte_width(val_data.ty().bytewidth());
                                let (rd, fmv) = MachineInstData::new_float_move(
                                    &mut self.machine_ctx,
                                    dst_fmt,
                                    FMvFmt::X,
                                    rd,
                                );
                                self.append_inst(&function_name, block, fmv);
                                rd
                            }
                            _ => unimplemented!(),
                        }
                    }
                    _ => self.get_value_as_register(val),
                };

                match op {
                    CastOp::ZExt => {
                        // make sure its int
                        assert!(val_data.ty().is_int());
                        // addi $rd, $rs, 0
                        let (rd, addi) = MachineInstData::new_binary_imm(
                            &mut self.machine_ctx,
                            MachineBinaryImmOp::Addi,
                            rs,
                            0.into(),
                        );
                        self.append_inst(&function_name, block, addi);
                        self.value_map
                            .insert(inst.into(), ValueCodegenResult::Register(rd));
                    }
                    CastOp::FpToSI => {
                        // make sure its float
                        assert!(val_data.ty().is_float());
                        // make sure result is int
                        assert!(inst_data.ty().is_int());
                        // fcvt.[w/l].[s/d] $rd, $rs
                        let fmt = match val_data.ty().bytewidth() {
                            4 => FCvtFmt::W,
                            8 => FCvtFmt::L,
                            _ => unimplemented!(),
                        };
                        let dst_fmt = match inst_data.ty().bytewidth() {
                            4 => FCvtFmt::S,
                            8 => FCvtFmt::D,
                            _ => unimplemented!(),
                        };
                        let (rd, fcvt) = MachineInstData::new_float_convert(
                            &mut self.machine_ctx,
                            fmt,
                            dst_fmt,
                            rs,
                        );
                        self.append_inst(&function_name, block, fcvt);
                        self.value_map
                            .insert(inst.into(), ValueCodegenResult::Register(rd));
                    }
                    CastOp::SIToFp => {
                        // make sure its int
                        assert!(val_data.ty().is_int());
                        // make sure result is float
                        assert!(inst_data.ty().is_float());
                        // fcvt.[s/d].[w/l] $rd, $rs
                        let fmt = match val_data.ty().bytewidth() {
                            4 => FCvtFmt::W,
                            8 => FCvtFmt::L,
                            _ => unimplemented!(),
                        };
                        let dst_fmt = match inst_data.ty().bytewidth() {
                            4 => FCvtFmt::S,
                            8 => FCvtFmt::D,
                            _ => unimplemented!(),
                        };
                        let (rd, fcvt) = MachineInstData::new_float_convert(
                            &mut self.machine_ctx,
                            fmt,
                            dst_fmt,
                            rs,
                        );
                        self.append_inst(&function_name, block, fcvt);
                        self.value_map
                            .insert(inst.into(), ValueCodegenResult::Register(rd));
                    }
                    CastOp::Bitcast => {
                        // just return the register
                        self.value_map
                            .insert(inst.into(), ValueCodegenResult::Register(rs));
                    }
                    _ => unimplemented!(),
                }
            }
            ValueKind::Jump(jump) => {
                let dst_block = jump.dst();
                let args = jump.args();
                let params = function_data.dfg.block_data(dst_block).unwrap().params();

                self.codegen_block_arg_pass(module, function, block, params, args);

                let j = MachineInstData::new_j(&mut self.machine_ctx, self.block_map[&dst_block]);
                self.append_inst(&function_name, block, j);
            }
            ValueKind::Branch(branch) => {
                let cond = branch.cond();
                let then_block = branch.then_dst();
                let else_block = branch.else_dst();

                let cond_data = function_data.dfg.local_value_data(cond).unwrap();

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
                        let (rd, li) = MachineInstData::new_li(&mut self.machine_ctx, imm);
                        self.append_inst(&function_name, block, li);
                        rd
                    }
                    _ => self.get_value_as_register(cond),
                };

                let params = function_data.dfg.block_data(then_block).unwrap().params();
                let args = branch.then_args();
                self.codegen_block_arg_pass(module, function, block, params, args);

                // bnez $cond, $then_block
                let zero = self.machine_ctx.new_gp_reg(RiscvGpReg::Zero);
                let bne = MachineInstData::new_branch(
                    &mut self.machine_ctx,
                    MachineBranchOp::Bne,
                    cond_reg,
                    zero,
                    self.block_map[&then_block],
                );

                self.append_inst(&function_name, block, bne);

                let params = function_data.dfg.block_data(else_block).unwrap().params();
                let args = branch.else_args();
                self.codegen_block_arg_pass(module, function, block, params, args);

                // j $else_block
                let j = MachineInstData::new_j(&mut self.machine_ctx, self.block_map[&else_block]);
                self.append_inst(&function_name, block, j);
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
                            let (rd, addi) = MachineInstData::new_binary_imm(
                                &mut self.machine_ctx,
                                MachineBinaryImmOp::Addi,
                                base,
                                offset,
                            );
                            self.append_inst(&function_name, block, addi);
                            rd
                        } else {
                            // li
                            let (rd, li) = MachineInstData::new_li(&mut self.machine_ctx, offset);
                            self.append_inst(&function_name, block, li);
                            // add
                            let (rd, add) = MachineInstData::new_binary(
                                &mut self.machine_ctx,
                                MachineBinaryOp::Add,
                                base,
                                rd,
                            );
                            self.append_inst(&function_name, block, add);
                            rd
                        }
                    }
                    ValueCodegenResult::MachineSymbol(symbol) => {
                        // la
                        let symbol = symbol.clone();
                        let (rd, la) = MachineInstData::new_pseudo_load(
                            &mut self.machine_ctx,
                            PseudoLoadKind::Address,
                            symbol,
                        );
                        self.append_inst(&function_name, block, la);
                        rd
                    }
                };

                let indices = gep.indices();
                let mut basis_ty = gep.ty();
                let mut ptr_reg = reg;

                for index in indices {
                    let bytewidth = basis_ty.bytewidth();
                    let index_data = function_data.dfg.local_value_data(*index).unwrap();

                    let shamt = if bytewidth.is_power_of_two() {
                        Some(bytewidth.trailing_zeros())
                    } else {
                        None
                    };

                    match index_data.kind() {
                        ValueKind::GlobalSlot(_)
                        | ValueKind::Alloc(_)
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
                            let (rd, li) =
                                MachineInstData::new_li(&mut self.machine_ctx, bytes.into());
                            self.append_inst(&function_name, block, li);
                            match shamt {
                                Some(shamt) => {
                                    // mul can be reduced to slli
                                    let (rd, slli) = MachineInstData::new_binary_imm(
                                        &mut self.machine_ctx,
                                        MachineBinaryImmOp::Slli,
                                        rd,
                                        shamt.into(),
                                    );
                                    self.append_inst(&function_name, block, slli);
                                    // add
                                    let (rd, add) = MachineInstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Add,
                                        ptr_reg,
                                        rd,
                                    );
                                    self.append_inst(&function_name, block, add);
                                    ptr_reg = rd;
                                }
                                None => {
                                    // li + mul
                                    let (bw, li) = MachineInstData::new_li(
                                        &mut self.machine_ctx,
                                        bytewidth.into(),
                                    );
                                    self.append_inst(&function_name, block, li);
                                    let (rd, mul) = MachineInstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Mul,
                                        bw,
                                        rd,
                                    );
                                    self.append_inst(&function_name, block, mul);
                                    // add
                                    let (rd, add) = MachineInstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Add,
                                        ptr_reg,
                                        rd,
                                    );
                                    self.append_inst(&function_name, block, add);
                                    ptr_reg = rd;
                                }
                            }
                        }
                        _ => {
                            let rs = self.get_value_as_register(*index);
                            match shamt {
                                Some(shamt) => {
                                    // slli
                                    let (rs, slli) = MachineInstData::new_binary_imm(
                                        &mut self.machine_ctx,
                                        MachineBinaryImmOp::Slli,
                                        rs,
                                        shamt.into(),
                                    );
                                    self.append_inst(&function_name, block, slli);
                                    // add
                                    let (rd, add) = MachineInstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Add,
                                        ptr_reg,
                                        rs,
                                    );
                                    self.append_inst(&function_name, block, add);
                                    ptr_reg = rd;
                                }
                                None => {
                                    // mul
                                    let (rs, mul) = MachineInstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Mul,
                                        rs,
                                        rs,
                                    );
                                    self.append_inst(&function_name, block, mul);
                                    // add
                                    let (rd, add) = MachineInstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Add,
                                        ptr_reg,
                                        rs,
                                    );
                                    self.append_inst(&function_name, block, add);
                                    ptr_reg = rd;
                                }
                            }
                        }
                    }

                    dbg!(&basis_ty);

                    basis_ty = match basis_ty.kind() {
                        TypeKind::Array(_, elem_ty) => elem_ty.clone(),
                        TypeKind::Struct(_) => unimplemented!(),
                        TypeKind::Ptr => break, // there is no bound type anymore
                        _ => break,
                    };
                }

                self.value_map
                    .insert(inst.into(), ValueCodegenResult::Register(ptr_reg));
            }
            ValueKind::Call(call) => {
                let callee = call.callee();
                let args = call.args();

                let ret: Value = inst.into();

                let mut gp_reg_count = 0;
                let mut fp_reg_count = 0;
                let mut args_passed_by_stack: Vec<Value> = Vec::new();

                // heuristic: reg passing after the stack pushing, so that the a0~a7 and fa0~fa7
                // can be used for the stack arguments
                let mut reg_passing_insts: Vec<MachineInst> = Vec::new();

                for arg in args {
                    function_data.dfg.with_value_data(*arg, |arg_data| {
                        let ty = arg_data.ty();

                        if ty.is_float() {
                            if fp_reg_count <= 7 {
                                let fa = self
                                    .machine_ctx
                                    .new_fp_reg((RiscvFpReg::Fa0 as u8 + fp_reg_count).into());
                                let rd = match arg_data.kind() {
                                    ValueKind::GlobalSlot(_)
                                    | ValueKind::Alloc(_)
                                    | ValueKind::Array(_)
                                    | ValueKind::Struct(_)
                                    | ValueKind::Function
                                    | ValueKind::Store(_)
                                    | ValueKind::Jump(_)
                                    | ValueKind::Branch(_)
                                    | ValueKind::Return(_) => unreachable!(),
                                    ValueKind::Zero | ValueKind::Undef => {
                                        // fmv
                                        let dst_fmt = FMvFmt::from_byte_width(ty.bytewidth());
                                        let zero = self.machine_ctx.new_gp_reg(RiscvGpReg::Zero);
                                        let (rd, fmv) = MachineInstData::new_float_move(
                                            &mut self.machine_ctx,
                                            dst_fmt,
                                            FMvFmt::X,
                                            zero,
                                        );
                                        reg_passing_insts.push(fmv);
                                        rd
                                    }
                                    ValueKind::Bytes(bytes) => {
                                        // load into reg
                                        let imm: Immediate = bytes.into();
                                        // li
                                        let (rd, li) =
                                            MachineInstData::new_li(&mut self.machine_ctx, imm);
                                        reg_passing_insts.push(li);
                                        // fmv
                                        let dst_fmt = FMvFmt::from_byte_width(ty.bytewidth());
                                        let (rd, fmv) = MachineInstData::new_float_move(
                                            &mut self.machine_ctx,
                                            dst_fmt,
                                            FMvFmt::X,
                                            rd,
                                        );
                                        reg_passing_insts.push(fmv);
                                        rd
                                    }
                                    _ => self.get_value_as_register(*arg),
                                };
                                assert!(rd.is_fp_virtual());

                                let mv =
                                    MachineInstData::build_fp_move(&mut self.machine_ctx, fa, rd);
                                reg_passing_insts.push(mv);

                                fp_reg_count += 1;
                            } else {
                                args_passed_by_stack.push(*arg);
                            }
                        } else if ty.is_int() || ty.is_ptr() {
                            if gp_reg_count <= 7 {
                                let a = self
                                    .machine_ctx
                                    .new_gp_reg((RiscvGpReg::A0 as u8 + gp_reg_count).into());
                                match arg_data.kind() {
                                    ValueKind::Array(_)
                                    | ValueKind::Struct(_)
                                    | ValueKind::Store(_)
                                    | ValueKind::Jump(_)
                                    | ValueKind::Branch(_)
                                    | ValueKind::Return(_) => unreachable!(),
                                    ValueKind::GlobalSlot(_) | ValueKind::Function => {
                                        // la
                                        let symbol = self.get_value_as_symbol(*arg);
                                        let la = MachineInstData::build_pseudo_load(
                                            &mut self.machine_ctx,
                                            PseudoLoadKind::Address,
                                            a,
                                            symbol,
                                        );
                                        reg_passing_insts.push(la);
                                    }
                                    ValueKind::Zero | ValueKind::Undef => {
                                        // mv
                                        let zero = self.machine_ctx.new_gp_reg(RiscvGpReg::Zero);
                                        let mv = MachineInstData::build_gp_move(
                                            &mut self.machine_ctx,
                                            a,
                                            zero,
                                        );
                                        reg_passing_insts.push(mv);
                                    }
                                    ValueKind::Bytes(bytes) => {
                                        // load into reg
                                        let imm: Immediate = bytes.into();
                                        // li
                                        let li = MachineInstData::build_li(
                                            &mut self.machine_ctx,
                                            a,
                                            imm,
                                        );
                                        reg_passing_insts.push(li);
                                    }
                                    ValueKind::Alloc(_) => {
                                        let (rs, offset) = self.get_value_as_stack_slot(*arg);
                                        if check_itype_imm(offset) {
                                            // addi
                                            let addi = MachineInstData::build_binary_imm(
                                                &mut self.machine_ctx,
                                                MachineBinaryImmOp::Addi,
                                                a,
                                                rs,
                                                offset,
                                            );
                                            reg_passing_insts.push(addi);
                                        } else {
                                            // li
                                            let li = MachineInstData::build_li(
                                                &mut self.machine_ctx,
                                                a,
                                                offset,
                                            );
                                            reg_passing_insts.push(li);
                                            // add
                                            let add = MachineInstData::build_binary(
                                                &mut self.machine_ctx,
                                                MachineBinaryOp::Add,
                                                a,
                                                a,
                                                rs,
                                            );
                                            reg_passing_insts.push(add);
                                        }
                                    }
                                    _ => {
                                        let rs = self.get_value_as_register(*arg);
                                        let mv = MachineInstData::build_gp_move(
                                            &mut self.machine_ctx,
                                            a,
                                            rs,
                                        );
                                        reg_passing_insts.push(mv);
                                    }
                                }

                                gp_reg_count += 1;
                            } else {
                                args_passed_by_stack.push(*arg);
                            }
                        } else {
                            unreachable!()
                        }
                    });
                }

                let mut stack_offset = 0usize;
                for arg in args_passed_by_stack.iter() {
                    let bytewidth = function_data
                        .dfg
                        .with_value_data(*arg, |arg_data| arg_data.ty().bytewidth())
                        .unwrap();
                    stack_offset += bytewidth;
                }

                // align to 16 bytes
                stack_offset = (stack_offset + 15) & !15;
                // passing the arguments steps:
                // 1. tmp <- sp - stack_offset(aligned)
                // 2. *tmp <- arg#0
                // 3. *(tmp + sizeof(arg#0)) <- arg#1
                // ...
                // final: sp <- tmp
                // possible optimizations: sw/d arg, offset(sp)

                if stack_offset != 0 {
                    // sub $tmp, $sp, stack_offset
                    let sp = self.machine_ctx.new_gp_reg(RiscvGpReg::Sp);
                    let (rd, inst) = if check_itype_imm(stack_offset.into()) {
                        // addi, $dst, sp, -stack_offset
                        MachineInstData::new_binary_imm(
                            &mut self.machine_ctx,
                            MachineBinaryImmOp::Addi,
                            sp,
                            (-(stack_offset as i32)).into(),
                        )
                    } else {
                        // li, $tmp, stack_offset
                        let (rd, li) =
                            MachineInstData::new_li(&mut self.machine_ctx, stack_offset.into());
                        self.append_inst(&function_name, block, li);
                        // sub, $dst, sp, $tmp
                        MachineInstData::new_binary(
                            &mut self.machine_ctx,
                            MachineBinaryOp::Sub,
                            sp,
                            rd,
                        )
                    };
                    self.append_inst(&function_name, block, inst);

                    let mut curr_offset = 0;

                    for arg in args_passed_by_stack.iter() {
                        function_data.dfg.with_value_data(*arg, |arg_data| {
                            let bytewidth = arg_data.ty().bytewidth();
                            let (offset_reg, offset): (_, Immediate) =
                                if check_itype_imm(curr_offset.into()) {
                                    (rd, curr_offset.into())
                                } else {
                                    let (imm_reg, li) = MachineInstData::new_li(
                                        &mut self.machine_ctx,
                                        curr_offset.into(),
                                    );
                                    self.append_inst(&function_name, block, li);
                                    // add
                                    let (added, add) = MachineInstData::new_binary(
                                        &mut self.machine_ctx,
                                        MachineBinaryOp::Add,
                                        rd,
                                        imm_reg,
                                    );
                                    self.append_inst(&function_name, block, add);
                                    (added, 0.into())
                                };
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
                                    // sw/d zero, offset(offset_reg)
                                    let zero = self.machine_ctx.new_gp_reg(RiscvGpReg::Zero);
                                    let kind = match bytewidth {
                                        1 => StoreKind::Byte,
                                        2 => StoreKind::Half,
                                        4 => StoreKind::Word,
                                        8 => StoreKind::DoubleWord,
                                        _ => unimplemented!(),
                                    };
                                    let store = MachineInstData::new_store(
                                        &mut self.machine_ctx,
                                        kind,
                                        zero,
                                        offset_reg,
                                        offset,
                                    );
                                    self.append_inst(&function_name, block, store);
                                }
                                ValueKind::Bytes(bytes) => {
                                    let imm: Immediate = bytes.into();
                                    let (rd, li) =
                                        MachineInstData::new_li(&mut self.machine_ctx, imm);
                                    self.append_inst(&function_name, block, li);
                                    if arg_data.ty().is_float() {
                                        let dst_fmt = match bytewidth {
                                            2 => FMvFmt::H,
                                            4 => FMvFmt::S,
                                            8 => FMvFmt::D,
                                            _ => unimplemented!(),
                                        };
                                        let (rd, fmv) = MachineInstData::new_float_move(
                                            &mut self.machine_ctx,
                                            dst_fmt,
                                            FMvFmt::X,
                                            rd,
                                        );
                                        self.append_inst(&function_name, block, fmv);
                                        // fsd
                                        let kind = match bytewidth {
                                            4 => FloatStoreKind::Single,
                                            8 => FloatStoreKind::Double,
                                            _ => unimplemented!(),
                                        };
                                        let store = MachineInstData::new_float_store(
                                            &mut self.machine_ctx,
                                            kind,
                                            rd,
                                            offset_reg,
                                            offset,
                                        );
                                        self.append_inst(&function_name, block, store);
                                    } else {
                                        // sw/d
                                        let kind = match bytewidth {
                                            1 => StoreKind::Byte,
                                            2 => StoreKind::Half,
                                            4 => StoreKind::Word,
                                            8 => StoreKind::DoubleWord,
                                            _ => unimplemented!(),
                                        };
                                        let store = MachineInstData::new_store(
                                            &mut self.machine_ctx,
                                            kind,
                                            rd,
                                            offset_reg,
                                            offset,
                                        );
                                        self.append_inst(&function_name, block, store);
                                    }
                                }
                                ValueKind::BlockParam
                                | ValueKind::Call(_)
                                | ValueKind::Unary(_)
                                | ValueKind::Binary(_)
                                | ValueKind::GetElemPtr(_)
                                | ValueKind::Cast(_)
                                | ValueKind::Load(_) => {
                                    let rs = self.get_value_as_register(*arg);
                                    if arg_data.ty().is_float() {
                                        // fsd
                                        let kind = match bytewidth {
                                            4 => FloatStoreKind::Single,
                                            8 => FloatStoreKind::Double,
                                            _ => unimplemented!(),
                                        };
                                        let store = MachineInstData::new_float_store(
                                            &mut self.machine_ctx,
                                            kind,
                                            rs,
                                            offset_reg,
                                            offset,
                                        );
                                        self.append_inst(&function_name, block, store);
                                    } else {
                                        // sw/d
                                        let kind = match bytewidth {
                                            1 => StoreKind::Byte,
                                            2 => StoreKind::Half,
                                            4 => StoreKind::Word,
                                            8 => StoreKind::DoubleWord,
                                            _ => unimplemented!(),
                                        };
                                        let store = MachineInstData::new_store(
                                            &mut self.machine_ctx,
                                            kind,
                                            rs,
                                            offset_reg,
                                            offset,
                                        );
                                        self.append_inst(&function_name, block, store);
                                    }
                                }
                                ValueKind::Alloc(_) => {
                                    let (rs, offset) = self.get_value_as_stack_slot(*arg);
                                    if check_itype_imm(offset) {
                                        // addi
                                        let (rd, addi) = MachineInstData::new_binary_imm(
                                            &mut self.machine_ctx,
                                            MachineBinaryImmOp::Addi,
                                            rs,
                                            offset,
                                        );
                                        self.append_inst(&function_name, block, addi);
                                        // sd
                                        let kind = match bytewidth {
                                            4 => StoreKind::Word,
                                            8 => StoreKind::DoubleWord,
                                            _ => unimplemented!(),
                                        };
                                        let store = MachineInstData::new_store(
                                            &mut self.machine_ctx,
                                            kind,
                                            rd,
                                            offset_reg,
                                            offset,
                                        );
                                        self.append_inst(&function_name, block, store);
                                    } else {
                                        // li
                                        let (rd, li) =
                                            MachineInstData::new_li(&mut self.machine_ctx, offset);
                                        self.append_inst(&function_name, block, li);
                                        // add
                                        let (rd, add) = MachineInstData::new_binary(
                                            &mut self.machine_ctx,
                                            MachineBinaryOp::Add,
                                            rs,
                                            rd,
                                        );
                                        self.append_inst(&function_name, block, add);
                                        // sd
                                        let kind = match bytewidth {
                                            4 => StoreKind::Word,
                                            8 => StoreKind::DoubleWord,
                                            _ => unimplemented!(),
                                        };
                                        let store = MachineInstData::new_store(
                                            &mut self.machine_ctx,
                                            kind,
                                            rd,
                                            offset_reg,
                                            offset,
                                        );
                                        self.append_inst(&function_name, block, store);
                                    }
                                }
                            }

                            curr_offset += bytewidth;
                        });
                    }

                    for inst in reg_passing_insts {
                        self.append_inst(&function_name, block, inst);
                    }

                    // sp <- tmp
                    let mv = MachineInstData::build_gp_move(&mut self.machine_ctx, sp, rd);
                    self.append_inst(&function_name, block, mv);
                } else {
                    // no stack arguments
                    for inst in reg_passing_insts {
                        self.append_inst(&function_name, block, inst);
                    }
                }
                // call
                // XXX: function pointer is not supported.
                let callee_symbol = self.get_value_as_symbol(callee);
                let call = MachineInstData::new_call(&mut self.machine_ctx, callee_symbol);
                self.append_inst(&function_name, block, call);

                // recover the stack
                if stack_offset != 0 {
                    if check_itype_imm(stack_offset.into()) {
                        // addi sp, sp, stack_offset
                        let sp = self.machine_ctx.new_gp_reg(RiscvGpReg::Sp);
                        let addi = MachineInstData::build_binary_imm(
                            &mut self.machine_ctx,
                            MachineBinaryImmOp::Addi,
                            sp,
                            sp,
                            stack_offset.into(),
                        );

                        self.append_inst(&function_name, block, addi);
                    } else {
                        // li
                        let (rd, li) =
                            MachineInstData::new_li(&mut self.machine_ctx, stack_offset.into());
                        self.append_inst(&function_name, block, li);
                        // add
                        let sp = self.machine_ctx.new_gp_reg(RiscvGpReg::Sp);
                        let add = MachineInstData::build_binary(
                            &mut self.machine_ctx,
                            MachineBinaryOp::Add,
                            sp,
                            sp,
                            rd,
                        );
                        self.append_inst(&function_name, block, add);
                    }
                }

                // return value
                let ret_ty = function_data.dfg.local_value_data(ret).unwrap().ty();

                if ret_ty.is_void() {
                    // do nothing
                } else if ret_ty.is_float() {
                    let fa0 = self.machine_ctx.new_fp_reg(RiscvFpReg::Fa0);
                    let (rd, fmv) = MachineInstData::new_float_move(
                        &mut self.machine_ctx,
                        FMvFmt::from_byte_width(ret_ty.bytewidth()),
                        FMvFmt::X,
                        fa0,
                    );
                    self.append_inst(&function_name, block, fmv);
                    self.value_map.insert(ret, ValueCodegenResult::Register(rd));
                } else if ret_ty.is_int() | ret_ty.is_ptr() {
                    let a0 = self.machine_ctx.new_gp_reg(RiscvGpReg::A0);
                    // addi
                    let (rd, addi) = MachineInstData::new_binary_imm(
                        &mut self.machine_ctx,
                        MachineBinaryImmOp::Addi,
                        a0,
                        0.into(),
                    );
                    self.append_inst(&function_name, block, addi);
                    self.value_map.insert(ret, ValueCodegenResult::Register(rd));
                } else {
                    unreachable!()
                }
            }
            ValueKind::Return(ret) => {
                let val = ret.val();
                if let Some(val) = val {
                    let val_data = function_data.dfg.local_value_data(val).unwrap();

                    // let reg = self.get_value_as_register(val);
                    match val_data.kind() {
                        ValueKind::GlobalSlot(_)
                        | ValueKind::Alloc(_)
                        | ValueKind::Array(_)
                        | ValueKind::Struct(_)
                        | ValueKind::Function
                        | ValueKind::Store(_)
                        | ValueKind::Jump(_)
                        | ValueKind::Branch(_)
                        | ValueKind::Return(_) => unreachable!(),
                        ValueKind::Zero | ValueKind::Undef => {
                            let zero = self.machine_ctx.new_gp_reg(RiscvGpReg::Zero);
                            if val_data.ty().is_float() {
                                let fa0 = self.machine_ctx.new_fp_reg(RiscvFpReg::Fa0);
                                let fmv = MachineInstData::build_fp_move(
                                    &mut self.machine_ctx,
                                    fa0,
                                    zero,
                                );
                                self.append_inst(&function_name, block, fmv);
                            } else if val_data.ty().is_int() || val_data.ty().is_ptr() {
                                let a0 = self.machine_ctx.new_gp_reg(RiscvGpReg::A0);
                                let mv =
                                    MachineInstData::build_gp_move(&mut self.machine_ctx, a0, zero);
                                self.append_inst(&function_name, block, mv);
                            } else {
                                unreachable!()
                            }
                        }
                        ValueKind::Bytes(bytes) => {
                            if val_data.ty().is_float() {
                                let imm: Immediate = bytes.into();
                                let (rd, li) = MachineInstData::new_li(&mut self.machine_ctx, imm);
                                self.append_inst(&function_name, block, li);
                                let fa0 = self.machine_ctx.new_fp_reg(RiscvFpReg::Fa0);
                                let fmv =
                                    MachineInstData::build_fp_move(&mut self.machine_ctx, fa0, rd);
                                self.append_inst(&function_name, block, fmv);
                            } else if val_data.ty().is_int() || val_data.ty().is_ptr() {
                                let imm: Immediate = bytes.into();
                                let a0 = self.machine_ctx.new_gp_reg(RiscvGpReg::A0);
                                let li = MachineInstData::build_li(&mut self.machine_ctx, a0, imm);
                                self.append_inst(&function_name, block, li);
                            } else {
                                unreachable!()
                            }
                        }
                        _ => {
                            let reg = self.get_value_as_register(val);
                            if val_data.ty().is_float() {
                                let fa0 = self.machine_ctx.new_fp_reg(RiscvFpReg::Fa0);
                                let mv =
                                    MachineInstData::build_fp_move(&mut self.machine_ctx, fa0, reg);
                                self.append_inst(&function_name, block, mv);
                            } else if val_data.ty().is_int() || val_data.ty().is_ptr() {
                                let a0 = self.machine_ctx.new_gp_reg(RiscvGpReg::A0);
                                let mv =
                                    MachineInstData::build_gp_move(&mut self.machine_ctx, a0, reg);
                                self.append_inst(&function_name, block, mv);
                            } else {
                                unreachable!()
                            }
                        }
                    };
                }
                // ret will be handled by the function epilogue
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
    }

    fn codegen_block_arg_pass(
        &mut self,
        module: &Module,
        function: Function,
        block: Block,
        params: &[Value],
        args: &[Value],
    ) {
        let function_data = module.function_data(function).unwrap();
        let function_name = module.value_name(function.into()).into();

        assert_eq!(args.len(), params.len());
        for (arg, param) in args.iter().zip(params.iter()) {
            // the register should have been assigned to the block argument.
            let rd = self.get_value_as_register(*param);
            function_data.dfg.with_value_data(*arg, |arg_data| {
                match arg_data.kind() {
                    ValueKind::Array(_)
                    | ValueKind::Struct(_)
                    | ValueKind::Function
                    | ValueKind::Store(_)
                    | ValueKind::Jump(_)
                    | ValueKind::Branch(_)
                    | ValueKind::Return(_) => unreachable!(),
                    ValueKind::GlobalSlot(_) => {
                        // la
                        let symbol = self.get_value_as_symbol(*arg);
                        let la = MachineInstData::build_pseudo_load(
                            &mut self.machine_ctx,
                            PseudoLoadKind::Address,
                            rd,
                            symbol,
                        );
                        self.append_inst(&function_name, block, la);
                    }
                    ValueKind::Alloc(_) => {
                        let (rs, offset) = self.get_value_as_stack_slot(*arg);
                        if check_itype_imm(offset) {
                            // addi
                            let addi = MachineInstData::build_binary_imm(
                                &mut self.machine_ctx,
                                MachineBinaryImmOp::Addi,
                                rd,
                                rs,
                                offset,
                            );
                            self.append_inst(&function_name, block, addi);
                        } else {
                            // li
                            let li = MachineInstData::build_li(&mut self.machine_ctx, rd, offset);
                            self.append_inst(&function_name, block, li);
                            // add
                            let add = MachineInstData::build_binary(
                                &mut self.machine_ctx,
                                MachineBinaryOp::Add,
                                rd,
                                rd,
                                rs,
                            );
                            self.append_inst(&function_name, block, add);
                        }
                    }
                    ValueKind::Zero | ValueKind::Undef => {
                        let zero = self.machine_ctx.new_gp_reg(RiscvGpReg::Zero);

                        if arg_data.ty().is_float() {
                            let dst_fmt = match arg_data.ty().bytewidth() {
                                2 => FMvFmt::H,
                                4 => FMvFmt::S,
                                8 => FMvFmt::D,
                                _ => unimplemented!(),
                            };
                            let fmv = MachineInstData::build_fmv(
                                &mut self.machine_ctx,
                                dst_fmt,
                                FMvFmt::X,
                                rd,
                                zero,
                            );
                            self.append_inst(&function_name, block, fmv);
                        } else if arg_data.ty().is_int() || arg_data.ty().is_ptr() {
                            let mv =
                                MachineInstData::build_gp_move(&mut self.machine_ctx, rd, zero);
                            self.append_inst(&function_name, block, mv);
                        } else {
                            unreachable!()
                        }
                    }
                    ValueKind::Bytes(bytes) => {
                        let imm: Immediate = bytes.into();
                        if arg_data.ty().is_float() {
                            let (tmp, li) = MachineInstData::new_li(&mut self.machine_ctx, imm);
                            let mv = MachineInstData::build_fp_move(&mut self.machine_ctx, rd, tmp);
                            self.append_inst(&function_name, block, li);
                            self.append_inst(&function_name, block, mv);
                        } else if arg_data.ty().is_int() || arg_data.ty().is_ptr() {
                            let li = MachineInstData::build_li(&mut self.machine_ctx, rd, imm);
                            self.append_inst(&function_name, block, li);
                        } else {
                            unreachable!()
                        }
                    }
                    _ => {
                        let rs = self.get_value_as_register(*arg);
                        let mv = if arg_data.ty().is_float() {
                            MachineInstData::build_fp_move(&mut self.machine_ctx, rd, rs)
                        } else if arg_data.ty().is_int() || arg_data.ty().is_ptr() {
                            MachineInstData::build_gp_move(&mut self.machine_ctx, rd, rs)
                        } else {
                            unreachable!();
                        };
                        self.append_inst(&function_name, block, mv);
                    }
                }
            });
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn codegen_bianry_reg(
        &mut self,
        module: &Module,
        function: Function,
        block: Block,
        inst: Inst,
        op: BinaryOp,
        lhs: Register,
        rhs: Register,
        bytewidth: usize,
    ) {
        let function_name = module.value_name(function.into()).into();
        let op = match (op, bytewidth) {
            (BinaryOp::Add, 4) => MachineBinaryOp::Addw,
            (BinaryOp::Add, 8) => MachineBinaryOp::Add,
            (BinaryOp::Add, _) => unimplemented!(),
            (BinaryOp::Sub, 4) => MachineBinaryOp::Subw,
            (BinaryOp::Sub, 8) => MachineBinaryOp::Sub,
            (BinaryOp::Sub, _) => unimplemented!(),
            (BinaryOp::Mul, 4) => MachineBinaryOp::Mulw,
            (BinaryOp::Mul, 8) => MachineBinaryOp::Mul,
            (BinaryOp::Mul, _) => unimplemented!(),
            (BinaryOp::UDiv, 4) => MachineBinaryOp::Divuw,
            (BinaryOp::UDiv, 8) => MachineBinaryOp::Divu,
            (BinaryOp::UDiv, _) => unimplemented!(),
            (BinaryOp::SDiv, 4) => MachineBinaryOp::Divw,
            (BinaryOp::SDiv, 8) => MachineBinaryOp::Div,
            (BinaryOp::SDiv, _) => unimplemented!(),
            (BinaryOp::URem, 4) => MachineBinaryOp::Remuw,
            (BinaryOp::URem, 8) => MachineBinaryOp::Remu,
            (BinaryOp::URem, _) => unimplemented!(),
            (BinaryOp::SRem, 4) => MachineBinaryOp::Remw,
            (BinaryOp::SRem, 8) => MachineBinaryOp::Rem,
            (BinaryOp::SRem, _) => unimplemented!(),
            (BinaryOp::And, _) => MachineBinaryOp::And,
            (BinaryOp::Or, _) => MachineBinaryOp::Or,
            (BinaryOp::Xor, _) => MachineBinaryOp::Xor,
            (BinaryOp::Shl, _) => MachineBinaryOp::Sll,
            (BinaryOp::LShr, _) => MachineBinaryOp::Srl,
            (BinaryOp::AShr, _) => MachineBinaryOp::Sra,
            _ => unreachable!(),
        };
        let (rd, machine_inst) = MachineInstData::new_binary(&mut self.machine_ctx, op, lhs, rhs);
        self.append_inst(&function_name, block, machine_inst);
        self.value_map
            .insert(inst.into(), ValueCodegenResult::Register(rd));
    }

    #[allow(clippy::too_many_arguments)]
    fn codegen_binary_imm(
        &mut self,
        module: &Module,
        function: Function,
        block: Block,
        inst: Inst,
        op: BinaryOp,
        lhs: Register,
        rhs: Immediate,
        bytewidth: usize,
    ) {
        let function_name = module.value_name(function.into()).into();
        let rhs: Immediate = if let BinaryOp::Sub = op {
            // using addi to sub, so negate the immediate
            (-rhs.0).into()
        } else {
            rhs.0.into()
        };
        let op = match (op, bytewidth) {
            (BinaryOp::Add | BinaryOp::Sub, 4) => MachineBinaryImmOp::Addiw,
            (BinaryOp::Add | BinaryOp::Sub, 8) => MachineBinaryImmOp::Addi,
            (BinaryOp::Add | BinaryOp::Sub, _) => unreachable!(),
            (BinaryOp::Or, _) => MachineBinaryImmOp::Ori,
            (BinaryOp::And, _) => MachineBinaryImmOp::Andi,
            (BinaryOp::Xor, _) => MachineBinaryImmOp::Xori,
            (BinaryOp::Shl, _) => MachineBinaryImmOp::Slli,
            (BinaryOp::LShr, _) => MachineBinaryImmOp::Srli,
            (BinaryOp::AShr, _) => MachineBinaryImmOp::Srai,
            _ => unreachable!(),
        };
        let (rd, machine_inst) =
            MachineInstData::new_binary_imm(&mut self.machine_ctx, op, lhs, rhs);
        self.append_inst(&function_name, block, machine_inst);
        self.value_map
            .insert(inst.into(), ValueCodegenResult::Register(rd));
    }

    #[allow(clippy::too_many_arguments)]
    fn codegen_binary_imm_li(
        &mut self,
        module: &Module,
        function: Function,
        block: Block,
        inst: Inst,
        op: BinaryOp,
        lhs: Register,
        rhs: Immediate,
        bytewidth: usize,
    ) {
        let function_name = module.value_name(function.into()).into();
        let (rhs, li) = MachineInstData::new_li(&mut self.machine_ctx, rhs);
        self.append_inst(&function_name, block, li);
        let op = match (op, bytewidth) {
            (BinaryOp::Mul, 4) => MachineBinaryOp::Mulw,
            (BinaryOp::Mul, 8) => MachineBinaryOp::Mul,
            (BinaryOp::Mul, _) => unimplemented!(),
            (BinaryOp::UDiv, 4) => MachineBinaryOp::Divuw,
            (BinaryOp::UDiv, 8) => MachineBinaryOp::Divu,
            (BinaryOp::UDiv, _) => unimplemented!(),
            (BinaryOp::SDiv, 4) => MachineBinaryOp::Divw,
            (BinaryOp::SDiv, 8) => MachineBinaryOp::Div,
            (BinaryOp::SDiv, _) => unimplemented!(),
            (BinaryOp::URem, 4) => MachineBinaryOp::Remuw,
            (BinaryOp::URem, 8) => MachineBinaryOp::Remu,
            (BinaryOp::URem, _) => unimplemented!(),
            (BinaryOp::SRem, 4) => MachineBinaryOp::Remw,
            (BinaryOp::SRem, 8) => MachineBinaryOp::Rem,
            (BinaryOp::SRem, _) => unimplemented!(),
            _ => unreachable!(),
        };
        let (rd, machine_inst) = MachineInstData::new_binary(&mut self.machine_ctx, op, lhs, rhs);
        self.append_inst(&function_name, block, machine_inst);
        self.value_map
            .insert(inst.into(), ValueCodegenResult::Register(rd));
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
