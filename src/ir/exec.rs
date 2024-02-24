use std::{collections::HashMap, result};

use crate::collections::{BiLinkedNode, BiMap};

use super::{
    entities::ValueKind,
    layout,
    module::{self, Module},
    types::DataLayout,
    values::{BinaryOp, Block, FCmpCond, Function, ICmpCond, Inst, UnaryOp, Value},
};

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum Segment {
    Code,
    Data,
    Constant,
    Stack,
}

pub struct Memory {
    data: Vec<u8>,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct Addr(u64);

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct VReg(u64);

impl From<Addr> for VReg {
    fn from(addr: Addr) -> Self {
        Self(addr.0)
    }
}

impl From<VReg> for Addr {
    fn from(vreg: VReg) -> Self {
        Self(vreg.0)
    }
}

impl Addr {
    pub fn segment(&self) -> Segment {
        // high 8 bits
        match self.0 >> 56 {
            0 => Segment::Code,
            1 => Segment::Data,
            2 => Segment::Constant,
            3 => Segment::Stack,
            _ => panic!("invalid segment"),
        }
    }

    pub fn offset(&self) -> usize {
        // low 56 bits
        (self.0 & 0x00ff_ffff_ffff_ffff) as usize
    }
}

impl VReg {
    pub fn new() -> Self {
        Self(0)
    }

    pub fn to_le_bytes(&self) -> [u8; 8] {
        self.0.to_le_bytes()
    }

    pub fn from_le_bytes(bytes: [u8; 8]) -> Self {
        Self(u64::from_le_bytes(bytes))
    }

    pub fn to_float(&self) -> f32 {
        f32::from_le_bytes((self.0 as u32).to_le_bytes())
    }

    pub fn from_float(f: f32) -> Self {
        Self(u32::from_le_bytes(f.to_le_bytes()) as u64)
    }

    pub fn to_double(&self) -> f64 {
        f64::from_le_bytes(self.0.to_le_bytes())
    }

    pub fn from_double(f: f64) -> Self {
        Self(u64::from_le_bytes(f.to_le_bytes()))
    }
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

    /// Current block
    curr_block: Block,

    /// Function name to value map
    function_names: HashMap<String, Function>,
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

            addrs: BiMap::new(),

            vregs: HashMap::new(),

            curr_inst: Value::new(0).into(),
            curr_function: Value::new(0).into(),
            curr_block: Block::new(0),

            function_names: HashMap::new(),
        }
    }

    fn data_layout(&self) -> DataLayout {
        DataLayout { pointer_size: 4 }
    }

    fn alloc_memory(&mut self, segment: Segment, size: usize) -> Addr {
        let addr = Addr(self.memory[&segment].data.len() as u64);
        let new_len = self.memory[&segment].data.len() + size;
        self.memory
            .get_mut(&segment)
            .unwrap()
            .data
            .resize(new_len, 0);
        addr
    }

    fn alloc_vreg(&mut self, value: Value) -> VReg {
        let vreg = VReg::new();
        self.vregs.insert(value, vreg);
        vreg
    }

    fn write_vreg(&mut self, value: Value, vreg: VReg) {
        self.vregs.insert(value, vreg);
    }

    fn write_memory(&mut self, addr: Addr, data: &[u8]) {
        let segment = addr.segment();
        let offset = addr.offset();
        let memory = &mut self.memory.get_mut(&segment).unwrap().data;
        memory[offset..offset + data.len()].copy_from_slice(data);
    }

    fn read_memory(&self, addr: Addr, size: usize) -> &[u8] {
        let segment = addr.segment();
        let offset = addr.offset();
        &self.memory[&segment].data[offset..offset + size]
    }

    fn function(&self, name: &str) -> Function {
        *self.function_names.get(name).unwrap()
    }

    fn add_function(&mut self, name: String, function: Function) {
        self.function_names.insert(name, function);
    }

    pub fn prepare(&mut self, entry_function_name: &str, vregs: &HashMap<Value, VReg>) {
        for value in self.module.global_slot_layout() {
            self.module
                .with_value_data(*value, |data| match data.kind() {
                    ValueKind::GlobalSlot(slot) => {
                        let segment = if slot.mutable() {
                            Segment::Data
                        } else {
                            Segment::Constant
                        };
                        let data_layout = self.data_layout();
                        let addr = self.alloc_memory(segment, data.ty().size(Some(&data_layout)));
                        self.addrs.insert(*value, addr);
                    }
                    _ => unreachable!("unexpected item"),
                });
        }

        for function in self.module.function_layout() {
            self.module
                .with_value_data((*function).into(), |data| match data.kind() {
                    ValueKind::Function => {
                        let addr = self.alloc_memory(Segment::Code, 1);
                        self.addrs.insert((*function).into(), addr);
                    }
                    _ => unreachable!("unexpected item"),
                });

            // map name of the function
            let name = self
                .module
                .function_data(*function)
                .expect("function should exist")
                .name()
                .to_string();
            self.add_function(name, *function);

            // allocate vreg for values
            let dfg = self
                .module
                .function_data(*function)
                .expect("function should exist")
                .dfg();

            for (value, _data) in dfg.values() {
                self.alloc_vreg(*value);
                if vregs.contains_key(value) {
                    self.vregs.insert(*value, vregs[value]);
                }
            }
        }

        self.curr_function = self.function(entry_function_name);

        let layout = self
            .module
            .function_data(self.curr_function)
            .expect("function should exist")
            .layout();

        self.curr_block = layout.entry_block().expect("entry block should exist");
        self.curr_inst = layout
            .blocks()
            .node(self.curr_block)
            .expect("block should exist")
            .insts()
            .front()
            .expect("inst should exist")
            .into();
    }

    pub fn step(&mut self) {
        let dfg = self
            .module
            .function_data(self.curr_function)
            .expect("function should exist")
            .dfg();
        let layout = self
            .module
            .function_data(self.curr_function)
            .expect("function should exist")
            .layout();

        let value_data = dfg
            .local_value_data(self.curr_inst.into())
            .expect("inst should exist");

        let mut next_inst = layout
            .blocks()
            .node(self.curr_block)
            .expect("block should exist")
            .insts()
            .node(self.curr_inst)
            .expect("inst should exist")
            .next()
            .expect("next inst should exist");

        let dest = self.curr_inst.into();
        let data_layout = self.data_layout();

        match value_data.kind() {
            ValueKind::Alloc(alloc) => {
                let addr = self.alloc_memory(Segment::Stack, alloc.ty().size(Some(&data_layout)));
                self.write_vreg(dest, addr.into());
            }
            ValueKind::Load(load) => {
                let size = value_data.ty().size(Some(&data_layout));
                let ptr_vreg = self.vregs[&load.ptr()];
                let bytes = self.read_memory(ptr_vreg.into(), size);
                let mut vreg_val = 0;
                for i in 0..size {
                    vreg_val |= (bytes[i] as u64) << (i * 8);
                }
                self.write_vreg(dest, VReg(vreg_val));
            }
            ValueKind::Store(store) => {
                let val = store.val();
                let ptr = store.val();
                let size = dfg
                    .with_value_data(val, |data| data.ty().size(Some(&data_layout)))
                    .expect("val to store should exist");
                let ptr_vreg = self.vregs[&ptr];
                let val_vreg = self.vregs[&val];
                let bytes = val_vreg.to_le_bytes();
                self.write_memory(ptr_vreg.into(), &bytes[0..size]);
            }
            ValueKind::Binary(binary) => {
                let op = binary.op();
                let lhs = binary.lhs();
                let rhs = binary.rhs();
                let lhs_vreg = self.vregs[&lhs];
                let rhs_vreg = self.vregs[&rhs];

                let ty = value_data.ty();
                let size = ty.size(Some(&data_layout));

                let lhs_val = lhs_vreg.0;
                let rhs_val = rhs_vreg.0;

                match op {
                    BinaryOp::Add => {
                        let result_val = match size {
                            1 => (lhs_val as u8).wrapping_add(rhs_val as u8) as u64,
                            2 => (lhs_val as u16).wrapping_add(rhs_val as u16) as u64,
                            4 => (lhs_val as u32).wrapping_add(rhs_val as u32) as u64,
                            8 => lhs_val.wrapping_add(rhs_val),
                            _ => unimplemented!("unsupported int size for add"),
                        };
                        self.write_vreg(dest, VReg(result_val));
                    }
                    BinaryOp::Sub => {
                        let result_val = match size {
                            1 => (lhs_val as u8).wrapping_sub(rhs_val as u8) as u64,
                            2 => (lhs_val as u16).wrapping_sub(rhs_val as u16) as u64,
                            4 => (lhs_val as u32).wrapping_sub(rhs_val as u32) as u64,
                            8 => lhs_val.wrapping_sub(rhs_val),
                            _ => unimplemented!("unsupported int size for sub"),
                        };
                        self.write_vreg(dest, VReg(result_val));
                    }
                    BinaryOp::Mul => {
                        let result_val = match size {
                            1 => (lhs_val as u8).wrapping_mul(rhs_val as u8) as u64,
                            2 => (lhs_val as u16).wrapping_mul(rhs_val as u16) as u64,
                            4 => (lhs_val as u32).wrapping_mul(rhs_val as u32) as u64,
                            8 => lhs_val.wrapping_mul(rhs_val),
                            _ => unimplemented!("unsupported int size for mul"),
                        };
                        self.write_vreg(dest, VReg(result_val));
                    }
                    BinaryOp::SDiv => {
                        let result_val = match size {
                            1 => (lhs_val as i8).wrapping_div(rhs_val as i8) as u64,
                            2 => (lhs_val as i16).wrapping_div(rhs_val as i16) as u64,
                            4 => (lhs_val as i32).wrapping_div(rhs_val as i32) as u64,
                            8 => lhs_val.wrapping_div(rhs_val),
                            _ => unimplemented!("unsupported int size for sdiv"),
                        };
                        self.write_vreg(dest, VReg(result_val));
                    }
                    BinaryOp::UDiv => {
                        let result_val = match size {
                            1 => (lhs_val as u8).wrapping_div(rhs_val as u8) as u64,
                            2 => (lhs_val as u16).wrapping_div(rhs_val as u16) as u64,
                            4 => (lhs_val as u32).wrapping_div(rhs_val as u32) as u64,
                            8 => lhs_val.wrapping_div(rhs_val),
                            _ => unimplemented!("unsupported int size for udiv"),
                        };
                        self.write_vreg(dest, VReg(result_val));
                    }
                    BinaryOp::SRem => {
                        let result_val = match size {
                            1 => (lhs_val as i8).wrapping_rem(rhs_val as i8) as u64,
                            2 => (lhs_val as i16).wrapping_rem(rhs_val as i16) as u64,
                            4 => (lhs_val as i32).wrapping_rem(rhs_val as i32) as u64,
                            8 => lhs_val.wrapping_rem(rhs_val),
                            _ => unimplemented!("unsupported int size for srem"),
                        };
                        self.write_vreg(dest, VReg(result_val));
                    }
                    BinaryOp::URem => {
                        let result_val = match size {
                            1 => (lhs_val as u8).wrapping_rem(rhs_val as u8) as u64,
                            2 => (lhs_val as u16).wrapping_rem(rhs_val as u16) as u64,
                            4 => (lhs_val as u32).wrapping_rem(rhs_val as u32) as u64,
                            8 => lhs_val.wrapping_rem(rhs_val),
                            _ => unimplemented!("unsupported int size for urem"),
                        };
                        self.write_vreg(dest, VReg(result_val));
                    }
                    BinaryOp::And => {
                        let result_val = lhs_val & rhs_val;
                        let result_val = result_val & ((1 << size * 8) - 1);
                        self.write_vreg(dest, VReg(result_val));
                    }
                    BinaryOp::Or => {
                        let result_val = lhs_val | rhs_val;
                        let result_val = result_val & ((1 << size * 8) - 1);
                        self.write_vreg(dest, VReg(result_val));
                    }
                    BinaryOp::Xor => {
                        let result_val = lhs_val ^ rhs_val;
                        let result_val = result_val & ((1 << size * 8) - 1);
                        self.write_vreg(dest, VReg(result_val));
                    }
                    BinaryOp::Shl => {
                        let result_val = lhs_val.wrapping_shl(rhs_val as u32);
                        let result_val = result_val & ((1 << size * 8) - 1);
                        self.write_vreg(dest, VReg(result_val));
                    }
                    BinaryOp::LShr => {
                        let result_val = lhs_val.wrapping_shr(rhs_val as u32);
                        let result_val = result_val & ((1 << size * 8) - 1);
                        self.write_vreg(dest, VReg(result_val));
                    }
                    BinaryOp::AShr => {
                        let result_val = (lhs_val as i64).wrapping_shr(rhs_val as u32) as u64;
                        let result_val = result_val & ((1 << size * 8) - 1);
                        self.write_vreg(dest, VReg(result_val));
                    }
                    BinaryOp::FAdd => {
                        let vreg = match size {
                            4 => VReg::from_float(lhs_vreg.to_float() + rhs_vreg.to_float()),
                            8 => VReg::from_double(lhs_vreg.to_double() + rhs_vreg.to_double()),
                            _ => unimplemented!("unsupported float size for fadd"),
                        };
                        self.write_vreg(dest, vreg);
                    }
                    BinaryOp::FSub => {
                        let vreg = match size {
                            4 => VReg::from_float(lhs_vreg.to_float() - rhs_vreg.to_float()),
                            8 => VReg::from_double(lhs_vreg.to_double() - rhs_vreg.to_double()),
                            _ => unimplemented!("unsupported float size for fsub"),
                        };
                        self.write_vreg(dest, vreg);
                    }
                    BinaryOp::FMul => {
                        let vreg = match size {
                            4 => VReg::from_float(lhs_vreg.to_float() * rhs_vreg.to_float()),
                            8 => VReg::from_double(lhs_vreg.to_double() * rhs_vreg.to_double()),
                            _ => unimplemented!("unsupported float size for fmul"),
                        };
                        self.write_vreg(dest, vreg);
                    }
                    BinaryOp::FDiv => {
                        let vreg = match size {
                            4 => VReg::from_float(lhs_vreg.to_float() / rhs_vreg.to_float()),
                            8 => VReg::from_double(lhs_vreg.to_double() / rhs_vreg.to_double()),
                            _ => unimplemented!("unsupported float size for fdiv"),
                        };
                        self.write_vreg(dest, vreg);
                    }
                    BinaryOp::FRem => {
                        let vreg = match size {
                            4 => VReg::from_float(lhs_vreg.to_float() % rhs_vreg.to_float()),
                            8 => VReg::from_double(lhs_vreg.to_double() % rhs_vreg.to_double()),
                            _ => unimplemented!("unsupported float size for frem"),
                        };
                        self.write_vreg(dest, vreg);
                    }
                    BinaryOp::ICmp(cond) => match cond {
                        ICmpCond::Eq => {
                            let result_val = if lhs_val == rhs_val { 1 } else { 0 };
                            self.write_vreg(dest, VReg(result_val));
                        }
                        ICmpCond::Ne => {
                            let result_val = if lhs_val != rhs_val { 1 } else { 0 };
                            self.write_vreg(dest, VReg(result_val));
                        }
                        ICmpCond::Slt => {
                            let result_val = if (lhs_val as i64) < (rhs_val as i64) {
                                1
                            } else {
                                0
                            };
                            self.write_vreg(dest, VReg(result_val));
                        }
                        ICmpCond::Sle => {
                            let result_val = if (lhs_val as i64) <= (rhs_val as i64) {
                                1
                            } else {
                                0
                            };
                            self.write_vreg(dest, VReg(result_val));
                        }
                    },
                    BinaryOp::FCmp(cond) => match cond {
                        FCmpCond::OEq => {
                            let result_val = if lhs_val == rhs_val { 1 } else { 0 };
                            self.write_vreg(dest, VReg(result_val));
                        }
                        FCmpCond::ONe => {
                            let result_val = if lhs_val != rhs_val { 1 } else { 0 };
                            self.write_vreg(dest, VReg(result_val));
                        }
                        FCmpCond::OLt => {
                            let result_val = if lhs_val < rhs_val { 1 } else { 0 };
                            self.write_vreg(dest, VReg(result_val));
                        }
                        FCmpCond::OLe => {
                            let result_val = if lhs_val <= rhs_val { 1 } else { 0 };
                            self.write_vreg(dest, VReg(result_val));
                        }
                    },
                }
            }
            ValueKind::Unary(unary) => {
                let op = unary.op();
                let operand = unary.val();
                let operand_vreg = self.vregs[&operand];
                let ty = value_data.ty();
                let size = ty.size(Some(&data_layout));
                let operand_val = operand_vreg.0;

                match op {
                    UnaryOp::Not => {
                        let result_val = !operand_val;
                        let result_val = result_val & ((1 << size * 8) - 1);
                        self.write_vreg(dest, VReg(result_val));
                    }
                    UnaryOp::FNeg => {
                        let vreg = match size {
                            4 => VReg::from_float(-operand_vreg.to_float()),
                            8 => VReg::from_double(-operand_vreg.to_double()),
                            _ => unimplemented!("unsupported float size for fneg"),
                        };
                        self.write_vreg(dest, vreg);
                    }
                }
            }
            ValueKind::Jump(jump) => {
                let block = jump.dst();
                let block_data = dfg.block_data(block).expect("block should exist");
                assert_eq!(jump.args().len(), block_data.params().len());
                for (arg, param) in jump.args().iter().zip(block_data.params()) {
                    let arg_vreg = self.vregs[arg];
                    self.write_vreg(*param, arg_vreg);
                }
                self.curr_block = block;
                next_inst = layout
                    .blocks()
                    .node(self.curr_block)
                    .expect("block should exist")
                    .insts()
                    .front()
                    .expect("inst should exist")
                    .into();
            }
            ValueKind::Branch(branch) => {
                let cond = branch.cond();
                let then_block = branch.then_dst();
                let else_block = branch.else_dst();
                let cond_vreg = self.vregs[&cond];
                let cond_val = cond_vreg.0;
                
                let next_block = if cond_val != 0 {
                    then_block
                } else {
                    else_block
                };

                let args = if next_block == then_block {
                    branch.then_args()
                } else {
                    branch.else_args()
                };

                let block_data = dfg.block_data(next_block).expect("block should exist");

                assert_eq!(args.len(), block_data.params().len());

                for (arg, param) in args.iter().zip(block_data.params()) {
                    let arg_vreg = self.vregs[arg];
                    self.write_vreg(*param, arg_vreg);
                }

                self.curr_block = next_block;
                next_inst = layout
                    .blocks()
                    .node(self.curr_block)
                    .expect("block should exist")
                    .insts()
                    .front()
                    .expect("inst should exist")
                    .into();
            }
            
            _ => unimplemented!(),
        }

        self.curr_inst = next_inst;
    }
}
