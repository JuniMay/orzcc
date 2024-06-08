//! # Virtual Machine of OrzIR
//!
//! The virtual machine is used to execute the Orz IR. The virtual machine is
//! represented as a 64-bit  little-endian machine using a segmented memory
//! model.
//!
//! Note that a virtual machine is just a approximation of the real behavior of
//! the IR. The virtual machine is not designed to be a full-featured runtime,
//! but a tool to help debug and optimize the IR.
//!
//! The address format in the VM is as below
//! ```text
//! +-------------+--------------------------------------------------+
//! | Segment (8) |                   Offset (56)                    |
//! +-------------+--------------------------------------------------+
//! ```
//!
//! The program counter or instruction pointer in the VM is represented using a
//! combination of current function and current instruction.

use std::collections::HashMap;

use super::ExecError;
use crate::{
    collections::BiMap,
    ir::{
        entities::ValueKind,
        module::Module,
        types::TypeKind,
        values::{BinaryOp, CastOp, FCmpCond, Function, ICmpCond, Inst, UnaryOp, Value},
    },
};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Segment {
    Code = 0,
    Data = 1,
    Constant = 2,
    Stack = 3,
}

impl Segment {
    pub fn to_addr(&self, offset: usize) -> Addr { Addr(((*self as u64) << 56) | (offset as u64)) }
}

pub type ExecResult<T> = Result<T, ExecError>;

pub struct Memory {
    pub(super) data: Vec<u8>,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Addr(pub(super) u64);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct VReg(pub(super) u64);

impl From<Addr> for VReg {
    fn from(addr: Addr) -> Self { Self(addr.0) }
}

impl From<VReg> for Addr {
    fn from(vreg: VReg) -> Self { Self(vreg.0) }
}

impl Addr {
    pub fn new(addr: u64) -> Self { Self(addr) }

    pub fn segment(&self) -> ExecResult<Segment> {
        // high 8 bits
        match self.0 >> 56 {
            0 => Ok(Segment::Code),
            1 => Ok(Segment::Data),
            2 => Ok(Segment::Constant),
            3 => Ok(Segment::Stack),
            _ => Err(ExecError::InvalidSegement),
        }
    }

    pub fn offset(&self) -> usize {
        // low 56 bits
        (self.0 & 0x00ff_ffff_ffff_ffff) as usize
    }
}

impl Default for VReg {
    fn default() -> Self { Self::new() }
}

impl VReg {
    pub fn new() -> Self { Self(0) }

    pub fn to_le_bytes(&self) -> [u8; 8] { self.0.to_le_bytes() }

    pub fn from_le_bytes(bytes: [u8; 8]) -> Self { Self(u64::from_le_bytes(bytes)) }

    pub fn to_float(&self) -> f32 { f32::from_le_bytes((self.0 as u32).to_le_bytes()) }

    pub fn from_float(f: f32) -> Self { Self(u32::from_le_bytes(f.to_le_bytes()) as u64) }

    pub fn to_double(&self) -> f64 { f64::from_le_bytes(self.0.to_le_bytes()) }

    pub fn from_double(f: f64) -> Self { Self(u64::from_le_bytes(f.to_le_bytes())) }
}

pub struct VirtualMachine<'a> {
    /// Module
    module: &'a Module,

    /// Memory
    memory: HashMap<Segment, Memory>,

    /// Global address bi-directional map
    addrs: BiMap<Value, Addr>,

    /// Virtual registers
    vregs: HashMap<Value, VReg>,

    /// Current instruction
    curr_inst: Inst,

    /// Current function
    curr_function: Function,

    /// Stack for return value
    stack: Vec<(Function, Value)>,

    /// If the vm has stopped
    stopped: bool,
}

impl<'a> VirtualMachine<'a> {
    pub fn new(module: &'a Module) -> Self {
        let mut memory = HashMap::new();
        memory.insert(Segment::Code, Memory { data: vec![] });
        memory.insert(Segment::Data, Memory { data: vec![] });
        memory.insert(Segment::Constant, Memory { data: vec![] });
        memory.insert(Segment::Stack, Memory { data: vec![] });

        Self {
            module,
            memory,

            addrs: BiMap::default(),

            vregs: HashMap::new(),

            curr_inst: Value::new(0).into(),
            curr_function: Value::new(0).into(),

            stack: vec![],

            stopped: false,
        }
    }

    pub fn stopped(&self) -> bool { self.stopped }

    pub(super) fn curr_function(&self) -> Function { self.curr_function }

    pub(super) fn curr_inst(&self) -> Inst { self.curr_inst }

    fn alloc_memory(&mut self, segment: Segment, size: usize) -> ExecResult<Addr> {
        let offset = self.memory[&segment].data.len() as u64;
        let addr = segment.to_addr(offset as usize);
        let new_len = self.memory[&segment].data.len() + size;
        self.memory
            .get_mut(&segment)
            .unwrap()
            .data
            .resize(new_len, 0);
        Ok(addr)
    }

    fn alloc_vreg(&mut self, value: Value) -> VReg {
        let vreg = VReg::new();
        self.vregs.insert(value, vreg);
        vreg
    }

    fn write_vreg(&mut self, value: Value, vreg: VReg) { self.vregs.insert(value, vreg); }

    pub(super) fn read_vreg(&self, value: Value) -> VReg { *self.vregs.get(&value).unwrap() }

    fn write_memory(&mut self, addr: Addr, data: &[u8]) -> ExecResult<()> {
        let segment = addr.segment()?;
        let offset = addr.offset();
        let memory = &mut self.memory.get_mut(&segment).unwrap().data;
        memory[offset..offset + data.len()].copy_from_slice(data);
        Ok(())
    }

    pub(super) fn read_memory(&self, addr: Addr, size: usize) -> ExecResult<&[u8]> {
        let segment = addr.segment()?;
        let offset = addr.offset();
        Ok(&self.memory[&segment].data[offset..offset + size])
    }

    pub(super) fn memory(&self) -> &HashMap<Segment, Memory> { &self.memory }

    fn write_global_init(&mut self, addr: Addr, init: Value) -> ExecResult<()> {
        self.module
            .with_value_data(init, |data| match data.kind() {
                ValueKind::Zero => {
                    let size = data.ty().bytewidth();
                    self.write_memory(addr, &vec![0; size])
                }
                ValueKind::Bytes(bytes) => {
                    let size = data.ty().bytewidth();
                    let pad = size - bytes.len();
                    let mut data = Vec::new();
                    data.extend_from_slice(bytes);
                    data.extend_from_slice(&vec![0; pad]);
                    self.write_memory(addr, &data)
                }
                ValueKind::Undef => {
                    let size = data.ty().bytewidth();
                    self.write_memory(addr, &vec![0; size])
                }
                ValueKind::Array(elems) => {
                    let (_len, ty) = data
                        .ty()
                        .as_array()
                        .ok_or_else(|| ExecError::InvalidType(data.ty()))?;
                    let elem_size = ty.bytewidth();

                    let mut offset = 0;
                    for elem in elems {
                        let elem_addr = Addr(addr.0 + offset);
                        self.write_global_init(elem_addr, *elem)?;
                        offset += elem_size as u64;
                    }
                    Ok(())
                }
                ValueKind::Struct(fields) => {
                    let field_types = data
                        .ty()
                        .as_struct()
                        .ok_or_else(|| ExecError::InvalidType(data.ty()))?;
                    let mut offset = 0;
                    for (field, ty) in fields.iter().zip(field_types) {
                        let field_addr = Addr(addr.0 + offset);
                        self.write_global_init(field_addr, *field)?;
                        offset += ty.bytewidth() as u64;
                    }
                    Ok(())
                }
                _ => Err(ExecError::InvalidGlobalInit(init)),
            })
            .unwrap_or(Err(ExecError::ValueNotFound(init)))
    }

    pub fn prepare(&mut self, entry_function: Function) -> ExecResult<()> {
        for value in self.module.global_slot_layout() {
            self.module
                .with_value_data(*value, |data| match data.kind() {
                    ValueKind::GlobalSlot(slot) => {
                        let segment = if slot.mutable() {
                            Segment::Data
                        } else {
                            Segment::Constant
                        };
                        let ty = self
                            .module
                            .with_value_data(slot.init(), |data| data.ty())
                            .unwrap();
                        let addr = self.alloc_memory(segment, ty.bytewidth())?;
                        self.addrs.insert(*value, addr);
                        self.write_global_init(addr, slot.init())?;
                        self.alloc_vreg(*value);
                        self.write_vreg(*value, addr.into());
                        Ok(())
                    }
                    _ => Ok(()),
                })
                .unwrap_or(Err(ExecError::ValueNotFound(*value)))?;
        }

        for function in self.module.function_layout() {
            self.module
                .with_value_data((*function).into(), |data| match data.kind() {
                    ValueKind::Function => {
                        let addr = self.alloc_memory(Segment::Code, 1)?;
                        self.addrs.insert((*function).into(), addr);
                        self.alloc_vreg((*function).into());
                        self.write_vreg((*function).into(), addr.into());
                        Ok(())
                    }
                    _ => Err(ExecError::InvalidGlobalItem((*function).into())),
                })
                .unwrap_or_else(|| Err(ExecError::ValueNotFound((*function).into())))?;

            // allocate vreg for values
            let dfg = &self
                .module
                .function_data(*function)
                .ok_or_else(|| ExecError::FunctionNotFound((*function).into()))?
                .dfg;

            for (value, data) in dfg.values() {
                self.alloc_vreg(*value);
                match data.kind() {
                    ValueKind::Zero => {
                        self.write_vreg(*value, VReg(0));
                    }
                    ValueKind::Bytes(bytes) => {
                        let mut vreg_val = 0;
                        for (i, byte) in bytes.iter().enumerate() {
                            vreg_val |= (*byte as u64) << (i * 8);
                        }
                        self.write_vreg(*value, VReg(vreg_val));
                    }
                    _ => {}
                }
            }
        }

        self.curr_function = entry_function;

        let layout = &self
            .module
            .function_data(self.curr_function)
            .ok_or_else(|| ExecError::FunctionNotFound(self.curr_function.into()))?
            .layout;

        self.curr_inst = layout.entry_inst().expect("entry inst should exist");
        Ok(())
    }

    pub fn run(&mut self, steps: Option<usize>) -> ExecResult<()> {
        let mut steps = steps.unwrap_or(usize::MAX);
        while steps > 0 {
            if self.stopped() {
                break;
            }
            self.step()?;
            steps -= 1;
        }
        Ok(())
    }

    pub fn step(&mut self) -> ExecResult<()> {
        let dfg = &self
            .module
            .function_data(self.curr_function)
            .ok_or_else(|| ExecError::FunctionNotFound(self.curr_function.into()))?
            .dfg;
        let layout = &self
            .module
            .function_data(self.curr_function)
            .ok_or_else(|| ExecError::FunctionNotFound(self.curr_function.into()))?
            .layout;

        let value_data = dfg
            .local_value_data(self.curr_inst.into())
            .ok_or_else(|| ExecError::ValueNotFound(self.curr_inst.into()))?;

        let mut next_function = self.curr_function;
        let mut next_inst = layout.next_inst(self.curr_inst);

        match value_data.kind() {
            ValueKind::Alloc(alloc) => {
                let addr = self.alloc_memory(Segment::Stack, alloc.ty().bytewidth())?;
                self.write_vreg(self.curr_inst.into(), addr.into());
            }
            ValueKind::Load(load) => {
                let size = value_data.ty().bytewidth();
                let ptr_vreg = self.read_vreg(load.ptr());
                let bytes = self.read_memory(ptr_vreg.into(), size)?;
                let mut vreg_val = 0;
                for (i, byte) in bytes.iter().enumerate().take(size) {
                    vreg_val |= (*byte as u64) << (i * 8);
                }
                self.write_vreg(self.curr_inst.into(), VReg(vreg_val));
            }
            ValueKind::Store(store) => {
                let val = store.val();
                let ptr = store.ptr();
                let size = dfg
                    .with_value_data(val, |data| data.ty().bytewidth())
                    .expect("val to store should exist");
                let ptr_vreg = self.read_vreg(ptr);
                let val_vreg = self.read_vreg(val);
                let bytes = val_vreg.to_le_bytes();
                self.write_memory(ptr_vreg.into(), &bytes[0..size])?;
            }
            ValueKind::Binary(binary) => {
                let op = binary.op();
                let lhs = binary.lhs();
                let rhs = binary.rhs();
                let lhs_ty = dfg.with_value_data(lhs, |data| data.ty()).unwrap();
                let rhs_ty = dfg.with_value_data(rhs, |data| data.ty()).unwrap();
                let lhs_vreg = self.read_vreg(lhs);
                let rhs_vreg = self.read_vreg(rhs);

                let ty = value_data.ty();
                let size = ty.bytewidth();
                let bitwidth = ty.bitwidth();

                let lhs_val = lhs_vreg.0;
                let rhs_val = rhs_vreg.0;

                let lhs_val = lhs_val & ((1 << lhs_ty.bitwidth()) - 1);
                let rhs_val = rhs_val & ((1 << rhs_ty.bitwidth()) - 1);

                match op {
                    BinaryOp::Add => {
                        let result_val = lhs_val.wrapping_add(rhs_val);
                        self.write_vreg(self.curr_inst.into(), VReg(result_val));
                    }
                    BinaryOp::Sub => {
                        let result_val = lhs_val.wrapping_sub(rhs_val);
                        self.write_vreg(self.curr_inst.into(), VReg(result_val));
                    }
                    BinaryOp::Mul => {
                        let result_val = lhs_val.wrapping_mul(rhs_val);
                        self.write_vreg(self.curr_inst.into(), VReg(result_val));
                    }
                    BinaryOp::SDiv => {
                        let result_val = match size {
                            1 => (lhs_val as i8).wrapping_div(rhs_val as i8) as u64,
                            2 => (lhs_val as i16).wrapping_div(rhs_val as i16) as u64,
                            4 => (lhs_val as i32).wrapping_div(rhs_val as i32) as u64,
                            8 => lhs_val.wrapping_div(rhs_val),
                            _ => unimplemented!("unsupported int size for sdiv"),
                        };
                        self.write_vreg(self.curr_inst.into(), VReg(result_val));
                    }
                    BinaryOp::UDiv => {
                        let result_val = lhs_val.wrapping_div(rhs_val);
                        self.write_vreg(self.curr_inst.into(), VReg(result_val));
                    }
                    BinaryOp::SRem => {
                        let result_val = match size {
                            1 => (lhs_val as i8).wrapping_rem(rhs_val as i8) as u64,
                            2 => (lhs_val as i16).wrapping_rem(rhs_val as i16) as u64,
                            4 => (lhs_val as i32).wrapping_rem(rhs_val as i32) as u64,
                            8 => lhs_val.wrapping_rem(rhs_val),
                            _ => unimplemented!("unsupported int size for srem"),
                        };
                        self.write_vreg(self.curr_inst.into(), VReg(result_val));
                    }
                    BinaryOp::URem => {
                        let result_val = lhs_val.wrapping_rem(rhs_val);
                        self.write_vreg(self.curr_inst.into(), VReg(result_val));
                    }
                    BinaryOp::And => {
                        let result_val = lhs_val & rhs_val;
                        let result_val = result_val & ((1 << bitwidth) - 1);
                        self.write_vreg(self.curr_inst.into(), VReg(result_val));
                    }
                    BinaryOp::Or => {
                        let result_val = lhs_val | rhs_val;
                        let result_val = result_val & ((1 << bitwidth) - 1);
                        self.write_vreg(self.curr_inst.into(), VReg(result_val));
                    }
                    BinaryOp::Xor => {
                        let result_val = lhs_val ^ rhs_val;
                        let result_val = result_val & ((1 << bitwidth) - 1);
                        self.write_vreg(self.curr_inst.into(), VReg(result_val));
                    }
                    BinaryOp::Shl => {
                        let result_val = lhs_val.wrapping_shl(rhs_val as u32);
                        let result_val = result_val & ((1 << bitwidth) - 1);
                        self.write_vreg(self.curr_inst.into(), VReg(result_val));
                    }
                    BinaryOp::LShr => {
                        let result_val = lhs_val.wrapping_shr(rhs_val as u32);
                        let result_val = result_val & ((1 << bitwidth) - 1);
                        self.write_vreg(self.curr_inst.into(), VReg(result_val));
                    }
                    BinaryOp::AShr => {
                        let result_val = (lhs_val as i64).wrapping_shr(rhs_val as u32) as u64;
                        let result_val = result_val & ((1 << bitwidth) - 1);
                        self.write_vreg(self.curr_inst.into(), VReg(result_val));
                    }
                    BinaryOp::FAdd => {
                        let vreg = match size {
                            4 => VReg::from_float(lhs_vreg.to_float() + rhs_vreg.to_float()),
                            8 => VReg::from_double(lhs_vreg.to_double() + rhs_vreg.to_double()),
                            _ => unimplemented!("unsupported float size for fadd"),
                        };
                        self.write_vreg(self.curr_inst.into(), vreg);
                    }
                    BinaryOp::FSub => {
                        let vreg = match size {
                            4 => VReg::from_float(lhs_vreg.to_float() - rhs_vreg.to_float()),
                            8 => VReg::from_double(lhs_vreg.to_double() - rhs_vreg.to_double()),
                            _ => unimplemented!("unsupported float size for fsub"),
                        };
                        self.write_vreg(self.curr_inst.into(), vreg);
                    }
                    BinaryOp::FMul => {
                        let vreg = match size {
                            4 => VReg::from_float(lhs_vreg.to_float() * rhs_vreg.to_float()),
                            8 => VReg::from_double(lhs_vreg.to_double() * rhs_vreg.to_double()),
                            _ => unimplemented!("unsupported float size for fmul"),
                        };
                        self.write_vreg(self.curr_inst.into(), vreg);
                    }
                    BinaryOp::FDiv => {
                        let vreg = match size {
                            4 => VReg::from_float(lhs_vreg.to_float() / rhs_vreg.to_float()),
                            8 => VReg::from_double(lhs_vreg.to_double() / rhs_vreg.to_double()),
                            _ => unimplemented!("unsupported float size for fdiv"),
                        };
                        self.write_vreg(self.curr_inst.into(), vreg);
                    }
                    BinaryOp::FRem => {
                        let vreg = match size {
                            4 => VReg::from_float(lhs_vreg.to_float() % rhs_vreg.to_float()),
                            8 => VReg::from_double(lhs_vreg.to_double() % rhs_vreg.to_double()),
                            _ => unimplemented!("unsupported float size for frem"),
                        };
                        self.write_vreg(self.curr_inst.into(), vreg);
                    }
                    BinaryOp::ICmp(cond) => match cond {
                        ICmpCond::Eq => {
                            let result_val = (lhs_val == rhs_val).into();
                            self.write_vreg(self.curr_inst.into(), VReg(result_val));
                        }
                        ICmpCond::Ne => {
                            let result_val = (lhs_val != rhs_val).into();
                            self.write_vreg(self.curr_inst.into(), VReg(result_val));
                        }
                        ICmpCond::Slt => {
                            let result_val = match lhs_ty.bytewidth() {
                                1 => (lhs_val as i8) < (rhs_val as i8),
                                2 => (lhs_val as i16) < (rhs_val as i16),
                                4 => (lhs_val as i32) < (rhs_val as i32),
                                8 => (lhs_val as i64) < (rhs_val as i64),
                                _ => unimplemented!("unsupported int size for slt"),
                            } as u64;

                            self.write_vreg(self.curr_inst.into(), VReg(result_val));
                        }
                        ICmpCond::Sle => {
                            let result_val = match lhs_ty.bytewidth() {
                                1 => (lhs_val as i8) <= (rhs_val as i8),
                                2 => (lhs_val as i16) <= (rhs_val as i16),
                                4 => (lhs_val as i32) <= (rhs_val as i32),
                                8 => (lhs_val as i64) <= (rhs_val as i64),
                                _ => unimplemented!("unsupported int size for sle"),
                            } as u64;
                            self.write_vreg(self.curr_inst.into(), VReg(result_val));
                        }
                    },
                    BinaryOp::FCmp(cond) => {
                        // trim leading zero
                        let size = lhs_ty.bytewidth();
                        let mask = (1 << (size * 8)) - 1;
                        let lhs_val = lhs_val & mask;
                        let rhs_val = rhs_val & mask;
                        match cond {
                            FCmpCond::OEq => {
                                let result_val = (lhs_val == rhs_val).into();
                                self.write_vreg(self.curr_inst.into(), VReg(result_val));
                            }
                            FCmpCond::ONe => {
                                let result_val = (lhs_val != rhs_val).into();
                                self.write_vreg(self.curr_inst.into(), VReg(result_val));
                            }
                            FCmpCond::OLt => {
                                let result_val = (lhs_val < rhs_val).into();
                                self.write_vreg(self.curr_inst.into(), VReg(result_val));
                            }
                            FCmpCond::OLe => {
                                let result_val = (lhs_val <= rhs_val).into();
                                self.write_vreg(self.curr_inst.into(), VReg(result_val));
                            }
                        }
                    }
                }
            }
            ValueKind::Unary(unary) => {
                let op = unary.op();
                let operand = unary.val();
                let operand_vreg = self.read_vreg(operand);
                let ty = value_data.ty();
                let size = ty.bytewidth();
                let bitwidth = ty.bitwidth();
                let operand_val = operand_vreg.0;

                let mask = (1 << bitwidth) - 1;
                let operand_val = operand_val & mask;

                match op {
                    UnaryOp::Not => {
                        let result_val = !operand_val;
                        let result_val = result_val & ((1 << bitwidth) - 1);
                        self.write_vreg(self.curr_inst.into(), VReg(result_val));
                    }
                    UnaryOp::FNeg => {
                        let vreg = match size {
                            4 => VReg::from_float(-operand_vreg.to_float()),
                            8 => VReg::from_double(-operand_vreg.to_double()),
                            _ => unimplemented!("unsupported float size for fneg"),
                        };
                        self.write_vreg(self.curr_inst.into(), vreg);
                    }
                }
            }
            ValueKind::Jump(jump) => {
                let block = jump.dst();
                let block_data = dfg.block_data(block).expect("block should exist");
                assert_eq!(jump.args().len(), block_data.params().len());
                for (arg, param) in jump.args().iter().zip(block_data.params()) {
                    let arg_vreg = self.read_vreg(*arg);
                    self.write_vreg(*param, arg_vreg);
                }
                next_inst = layout.entry_inst_of_block(block);
            }
            ValueKind::Branch(branch) => {
                let cond = branch.cond();
                let then_block = branch.then_dst();
                let else_block = branch.else_dst();
                let cond_vreg = self.read_vreg(cond);
                let cond_val = cond_vreg.0;

                let block = if cond_val != 0 {
                    then_block
                } else {
                    else_block
                };

                let args = if block == then_block {
                    branch.then_args()
                } else {
                    branch.else_args()
                };

                let block_data = dfg.block_data(block).expect("block should exist");

                assert_eq!(args.len(), block_data.params().len());

                for (arg, param) in args.iter().zip(block_data.params()) {
                    let arg_vreg = self.read_vreg(*arg);
                    self.write_vreg(*param, arg_vreg);
                }

                next_inst = layout.entry_inst_of_block(block);
            }
            ValueKind::Call(call) => {
                let mut callee = call.callee();
                let args = call.args();

                let callee_data = dfg
                    .with_value_data(callee, |data| {
                        assert!(data.ty().is_ptr(), "callee's type should be a pointer");
                        match data.kind() {
                            ValueKind::Function => self
                                .module
                                .function_data(callee.into())
                                .expect("function should exist"),
                            _ => {
                                let addr = self.read_vreg(callee).into();
                                let function =
                                    self.addrs.get_rev(&addr).expect("function should exist");
                                callee = *function;
                                self.module
                                    .function_data((*function).into())
                                    .expect("function should exist")
                            }
                        }
                    })
                    .expect("callee should exist");

                let entry_block = callee_data
                    .layout
                    .entry_block()
                    .expect("entry block should exist in the layout");
                let entry_block_data = callee_data
                    .dfg
                    .block_data(entry_block)
                    .expect("entry block should exist in the dfg");
                let params = entry_block_data.params();

                assert!(
                    args.len() == params.len(),
                    "args and params should have the same length"
                );

                for (arg, param) in args.iter().zip(params) {
                    let arg_vreg = self.read_vreg(*arg);
                    self.write_vreg(*param, arg_vreg);
                }
                self.stack.push((self.curr_function, self.curr_inst.into()));

                next_function = callee.into();
                next_inst = callee_data
                    .layout
                    .blocks()
                    .node(entry_block)
                    .expect("block should exist")
                    .insts()
                    .front();
            }
            ValueKind::GetElemPtr(gep) => {
                let bound_type = gep.ty();
                let ptr = gep.ptr();

                let addr: Addr = self
                    .module
                    .function_data(self.curr_function)
                    .unwrap()
                    .dfg
                    .with_value_data(ptr, |data| match data.kind() {
                        ValueKind::GlobalSlot(_slot) => {
                            *self.addrs.get_fwd(&ptr).expect("addr should exist")
                        }
                        _ => {
                            let ptr_vreg = self.read_vreg(ptr);
                            ptr_vreg.into()
                        }
                    })
                    .expect("ptr should exist");

                let indices = gep.indices();

                let mut offset = 0u64;
                let mut curr_type = bound_type;

                for (i, index) in indices.iter().enumerate() {
                    let index_val = self.read_vreg(*index).0;

                    if i == 0 {
                        let size = curr_type.bytewidth();
                        offset += index_val * size as u64;
                        continue;
                    } else {
                        match curr_type.kind() {
                            TypeKind::Array(_len, elem_type) => {
                                let elem_size = elem_type.bytewidth();
                                offset += index_val * elem_size as u64;
                                curr_type = elem_type.clone();
                            }
                            TypeKind::Struct(field_types) => {
                                let field_idx = index_val as usize;
                                let field_size = if field_idx == 0 {
                                    0
                                } else {
                                    field_types[field_idx - 1].bytewidth()
                                };
                                offset += field_size as u64;
                                curr_type = field_types[field_idx].clone();
                            }
                            _ => {
                                let size = curr_type.bytewidth();
                                offset += index_val * size as u64;
                                break;
                            }
                        }
                    }
                }

                let new_addr = Addr(addr.0 + offset);
                self.write_vreg(self.curr_inst.into(), new_addr.into());
            }
            ValueKind::Cast(cast) => {
                let op = cast.op();
                let operand = cast.val();

                let operand_vreg = self.read_vreg(operand);

                let ty = value_data.ty();
                let operand_ty = dfg
                    .with_value_data(operand, |data| data.ty())
                    .ok_or(ExecError::ValueNotFound(operand))?;

                let dest_size = ty.bytewidth();
                let operand_size = operand_ty.bytewidth();

                match op {
                    CastOp::Trunc => match dest_size {
                        1 => self
                            .write_vreg(self.curr_inst.into(), VReg(operand_vreg.0 as u8 as u64)),
                        2 => self
                            .write_vreg(self.curr_inst.into(), VReg(operand_vreg.0 as u16 as u64)),
                        4 => self
                            .write_vreg(self.curr_inst.into(), VReg(operand_vreg.0 as u32 as u64)),
                        8 => self.write_vreg(self.curr_inst.into(), VReg(operand_vreg.0)),
                        _ => unimplemented!("unsupported int size for trunc"),
                    },
                    CastOp::ZExt => {
                        let mask = (1 << (operand_size * 8)) - 1;
                        let result_val = operand_vreg.0 & mask;
                        self.write_vreg(self.curr_inst.into(), VReg(result_val));
                    }
                    CastOp::SExt => {
                        let mask = (1 << (operand_size * 8)) - 1;
                        let sign_bit = 1 << (operand_size * 8 - 1);
                        let result_val = if operand_vreg.0 & sign_bit != 0 {
                            operand_vreg.0 | !mask
                        } else {
                            operand_vreg.0 & mask
                        };
                        self.write_vreg(self.curr_inst.into(), VReg(result_val));
                    }
                    CastOp::FpToUI => match operand_size {
                        4 => match dest_size {
                            1 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg(operand_vreg.to_float() as u8 as u64),
                            ),
                            2 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg(operand_vreg.to_float() as u16 as u64),
                            ),
                            4 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg(operand_vreg.to_float() as u32 as u64),
                            ),
                            8 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg(operand_vreg.to_float() as u64),
                            ),
                            _ => unimplemented!("unsupported int size for fptoui"),
                        },
                        8 => match dest_size {
                            1 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg(operand_vreg.to_double() as u8 as u64),
                            ),
                            2 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg(operand_vreg.to_double() as u16 as u64),
                            ),
                            4 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg(operand_vreg.to_double() as u32 as u64),
                            ),
                            8 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg(operand_vreg.to_double() as u64),
                            ),
                            _ => unimplemented!("unsupported int size for fptoui"),
                        },
                        _ => unimplemented!("unsupported float size for fptoui"),
                    },
                    CastOp::FpToSI => match operand_size {
                        4 => match dest_size {
                            1 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg(operand_vreg.to_float() as i8 as u64),
                            ),
                            2 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg(operand_vreg.to_float() as i16 as u64),
                            ),
                            4 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg(operand_vreg.to_float() as i32 as u64),
                            ),
                            8 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg(operand_vreg.to_float() as i64 as u64),
                            ),
                            _ => unimplemented!("unsupported int size for fptosi"),
                        },
                        8 => match dest_size {
                            1 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg(operand_vreg.to_double() as i8 as u64),
                            ),
                            2 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg(operand_vreg.to_double() as i16 as u64),
                            ),
                            4 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg(operand_vreg.to_double() as i32 as u64),
                            ),
                            8 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg(operand_vreg.to_double() as i64 as u64),
                            ),
                            _ => unimplemented!("unsupported int size for fptosi"),
                        },
                        _ => unimplemented!("unsupported float size for fptosi"),
                    },
                    CastOp::UIToFp => match dest_size {
                        4 => match operand_size {
                            1 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_float(operand_vreg.0 as u8 as f32),
                            ),
                            2 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_float(operand_vreg.0 as u16 as f32),
                            ),
                            4 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_float(operand_vreg.0 as u32 as f32),
                            ),
                            8 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_float(operand_vreg.0 as f32),
                            ),
                            _ => unimplemented!("unsupported int size for uitofp"),
                        },
                        8 => match operand_size {
                            1 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_double(operand_vreg.0 as u8 as f64),
                            ),
                            2 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_double(operand_vreg.0 as u16 as f64),
                            ),
                            4 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_double(operand_vreg.0 as u32 as f64),
                            ),
                            8 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_double(operand_vreg.0 as f64),
                            ),
                            _ => unimplemented!("unsupported int size for uitofp"),
                        },
                        _ => unimplemented!("unsupported float size for uitofp"),
                    },
                    CastOp::SIToFp => match dest_size {
                        4 => match operand_size {
                            1 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_float(operand_vreg.0 as i8 as f32),
                            ),
                            2 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_float(operand_vreg.0 as i16 as f32),
                            ),
                            4 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_float(operand_vreg.0 as i32 as f32),
                            ),
                            8 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_float(operand_vreg.0 as f32),
                            ),
                            _ => unimplemented!("unsupported int size for sitofp"),
                        },
                        8 => match operand_size {
                            1 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_double(operand_vreg.0 as i8 as f64),
                            ),
                            2 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_double(operand_vreg.0 as i16 as f64),
                            ),
                            4 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_double(operand_vreg.0 as i32 as f64),
                            ),
                            8 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_double(operand_vreg.0 as f64),
                            ),
                            _ => unimplemented!("unsupported int size for sitofp"),
                        },
                        _ => unimplemented!("unsupported float size for sitofp"),
                    },
                    CastOp::FpTrunc => match dest_size {
                        4 => match operand_size {
                            4 => self.write_vreg(self.curr_inst.into(), operand_vreg),
                            _ => unimplemented!("unsupported float size for fptrunc"),
                        },
                        8 => match operand_size {
                            4 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_double(operand_vreg.to_float() as f64),
                            ),
                            8 => self.write_vreg(self.curr_inst.into(), operand_vreg),
                            _ => unimplemented!("unsupported float size for fptrunc"),
                        },
                        _ => unimplemented!("unsupported float size for fptrunc"),
                    },
                    CastOp::FpExt => match dest_size {
                        4 => match operand_size {
                            4 => self.write_vreg(self.curr_inst.into(), operand_vreg),
                            8 => self.write_vreg(
                                self.curr_inst.into(),
                                VReg::from_double(operand_vreg.to_float() as f64),
                            ),
                            _ => unimplemented!("unsupported float size for fpext"),
                        },
                        8 => match operand_size {
                            8 => self.write_vreg(self.curr_inst.into(), operand_vreg),
                            _ => unimplemented!("unsupported float size for fpext"),
                        },
                        _ => unimplemented!("unsupported float size for fpext"),
                    },
                    CastOp::Bitcast => {
                        self.write_vreg(self.curr_inst.into(), operand_vreg);
                    }
                }
            }
            ValueKind::Return(ret) => {
                let val = ret.val();
                if self.stack.is_empty() {
                    self.stopped = true;
                } else {
                    let (caller, prev) = if let Some(val) = val {
                        let val_vreg = self.read_vreg(val);
                        let (caller, prev) = self.stack.pop().unwrap();
                        self.write_vreg(prev, val_vreg);
                        (caller, prev)
                    } else {
                        self.stack.pop().unwrap()
                    };

                    let layout = &self.module.function_data(caller).unwrap().layout;
                    next_inst = layout.next_inst(prev.into());
                    next_function = caller;
                }
            }
            _ => unreachable!("invalid local instruction"),
        }

        if !self.stopped() {
            self.curr_inst =
                next_inst.ok_or_else(|| ExecError::EarlyStop(self.curr_inst.into()))?;
            self.curr_function = next_function;
        }

        Ok(())
    }
}
