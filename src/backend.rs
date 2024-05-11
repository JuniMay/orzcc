use core::fmt;
use std::collections::HashMap;

use thiserror::Error;

use crate::{
    collections::{List, ListError, ListNode},
    ir::module::IdAllocator,
};

/// The layout in a machine function/procedure.
#[derive(Default)]
pub struct MachineLayout {
    /// The block list, including the instruction layout.
    blocks: MachineBlockList,
    /// The mapping from machinr instructions to the blocks.
    inst_blocks: HashMap<MachineInst, MachineBlock>,
}

/// Errors in layout operations
#[derive(Debug, Error)]
pub enum MachineLayoutOpErr {
    /// Duplicated instruction in inertion.
    #[error("duplicated instruction: {0:?}")]
    InstDuplicated(MachineInst),

    /// Duplicated block in insertion.
    #[error("duplicated block: {0:?}")]
    BlockDuplicated(MachineBlock),

    /// Parent block cannot be indexed in the layout mapping
    ///
    /// This is usually caused by removing an instruction.
    #[error("parent block not found for: {0:?}")]
    ParentBlockNotFound(MachineInst),

    /// Block cannot be found in a local layout (in the linked list).
    #[error("block node not found: {0:?}")]
    BlockNodeNotFound(MachineBlock),

    /// Instruction cannot be found in a local layout (in the linked list).
    #[error("instruction node not found for: {0:?}")]
    InstNodeNotFound(MachineInst),
}

impl MachineLayout {
    pub fn new() -> Self {
        Self::default()
    }

    /// Get the entry block for the current function.
    ///
    /// The entry block is by default the first block in the layout.
    pub fn entry_block(&self) -> Option<MachineBlock> {
        self.blocks.front()
    }

    /// Get the list of blocks.
    pub fn blocks(&self) -> &MachineBlockList {
        &self.blocks
    }

    /// Get the parent block of an instruction
    pub fn parent_block(&self, inst: MachineInst) -> Option<MachineBlock> {
        self.inst_blocks.get(&inst).copied()
    }

    /// Get the next block
    pub fn next_block(&self, block: MachineBlock) -> Option<MachineBlock> {
        self.blocks.node(block)?.next()
    }

    /// Get the next non-empty block
    pub fn next_non_empty_block(&self, block: MachineBlock) -> Option<MachineBlock> {
        let mut next_block = self.blocks.node(block)?.next();
        loop {
            if let Some(block) = next_block {
                let node = self.blocks.node(block).unwrap();
                if node.insts().is_empty() {
                    // try next one
                    next_block = node.next();
                } else {
                    return Some(block);
                }
            } else {
                // there is no next block
                return None;
            }
        }
    }

    /// Get the next instruction in the layout
    ///
    /// If this is the end of the block, return the first instruction of the
    /// next block. Note that this is not the execution order, but the
    /// layout order.
    pub fn next_inst(&self, inst: MachineInst) -> Option<MachineInst> {
        let parent_block = self.parent_block(inst)?;
        let node = self.blocks.node(parent_block).unwrap();

        node.insts().node(inst)?.next().or_else(|| {
            // this is the end of the block
            self.next_non_empty_block(parent_block)
                .and_then(|next_block| self.blocks.node(next_block).unwrap().insts().front())
        })
    }

    /// Get the entry instruction in the layout
    pub fn entry_inst(&self) -> Option<MachineInst> {
        self.entry_block()
            .and_then(|entry_block| self.blocks.node(entry_block).unwrap().insts().front())
    }

    /// Get the entry instruction of a block
    pub fn entry_inst_of_block(&self, block: MachineBlock) -> Option<MachineInst> {
        self.blocks
            .node(block)
            .and_then(|node| node.insts().front())
    }

    /// Get the exit/last instruction of a block
    pub fn exit_inst_of_block(&self, block: MachineBlock) -> Option<MachineInst> {
        self.blocks.node(block).and_then(|node| node.insts().back())
    }

    /// Append a block to the layout
    pub fn append_block(&mut self, block: MachineBlock) -> Result<(), MachineLayoutOpErr> {
        self.blocks
            .append(block)
            .map_err(|_| MachineLayoutOpErr::BlockDuplicated(block))
    }

    /// Append an instruction to a block
    pub fn append_inst(
        &mut self,
        inst: MachineInst,
        block: MachineBlock,
    ) -> Result<(), MachineLayoutOpErr> {
        if self.inst_blocks.contains_key(&inst) {
            return Err(MachineLayoutOpErr::InstDuplicated(inst));
        }
        self.blocks
            .node_mut(block)
            .ok_or(MachineLayoutOpErr::BlockNodeNotFound(block))?
            .insts_mut()
            .append(inst)
            .map_err(|_| MachineLayoutOpErr::InstDuplicated(inst))?;

        self.inst_blocks.insert(inst, block);

        Ok(())
    }

    /// Remove a block from the layout
    pub(super) fn remove_block(&mut self, block: MachineBlock) -> Result<(), MachineLayoutOpErr> {
        for (inst, _) in self
            .blocks
            .node(block)
            .ok_or(MachineLayoutOpErr::BlockNodeNotFound(block))?
            .insts()
            .into_iter()
        {
            self.inst_blocks.remove(&inst);
        }

        self.blocks
            .remove(block)
            .map_err(|_| MachineLayoutOpErr::BlockNodeNotFound(block))
    }

    pub(super) fn remove_inst(&mut self, inst: MachineInst) -> Result<(), MachineLayoutOpErr> {
        let block = self
            .inst_blocks
            .remove(&inst)
            .ok_or(MachineLayoutOpErr::ParentBlockNotFound(inst))?;

        self.blocks
            .node_mut(block)
            .ok_or(MachineLayoutOpErr::BlockNodeNotFound(block))?
            .insts_mut()
            .remove(inst)
            .map_err(|_| MachineLayoutOpErr::InstNodeNotFound(inst))
    }

    pub fn insert_block_before(
        &mut self,
        block: MachineBlock,
        before: MachineBlock,
    ) -> Result<(), MachineLayoutOpErr> {
        self.blocks
            .insert_before(block, before)
            .map_err(|err| match err {
                ListError::KeyDuplicated(block) => MachineLayoutOpErr::BlockDuplicated(block),
                ListError::NodeNotFound(before) => MachineLayoutOpErr::BlockNodeNotFound(before),
            })
    }

    pub fn insert_inst_before(
        &mut self,
        inst: MachineInst,
        before: MachineInst,
    ) -> Result<(), MachineLayoutOpErr> {
        if self.inst_blocks.contains_key(&inst) {
            return Err(MachineLayoutOpErr::InstDuplicated(inst));
        }

        let block = self
            .inst_blocks
            .get(&before)
            .ok_or(MachineLayoutOpErr::ParentBlockNotFound(before))?;

        self.blocks
            .node_mut(*block)
            .ok_or(MachineLayoutOpErr::BlockNodeNotFound(*block))?
            .insts_mut()
            .insert_before(inst, before)
            .map_err(|err| match err {
                ListError::KeyDuplicated(inst) => MachineLayoutOpErr::InstDuplicated(inst),
                // this is actually unreachable
                ListError::NodeNotFound(before) => MachineLayoutOpErr::InstNodeNotFound(before),
            })?;

        self.inst_blocks.insert(inst, *block);

        Ok(())
    }
}

/// The machine function data.
pub struct MachineFunctionData {
    /// The layout inside this function.
    layout: MachineLayout,
}

impl MachineFunctionData {
    pub fn layout(&self) -> &MachineLayout {
        &self.layout
    }

    pub fn layout_mut(&mut self) -> &mut MachineLayout {
        &mut self.layout
    }

    pub fn to_asm(&self, ctx: &MachineContext) -> String {
        let mut asm = String::new();
        // go through the blocks
        for (block_id, block) in self.layout.blocks().into_iter() {
            // .bb_<id>:
            asm.push_str(&format!(".bb_{}:\n", block_id.0));
            asm.push_str(&block.to_asm(ctx));
        }
        asm
    }
}

/// An instruction node in the list.
#[derive(Default)]
pub struct MachineInstNode {
    prev: Option<MachineInst>,
    next: Option<MachineInst>,
}

impl ListNode<MachineInst> for MachineInstNode {
    fn prev(&self) -> Option<MachineInst> {
        self.prev
    }

    fn next(&self) -> Option<MachineInst> {
        self.next
    }

    fn set_prev(&mut self, prev: Option<MachineInst>) {
        self.prev = prev;
    }

    fn set_next(&mut self, next: Option<MachineInst>) {
        self.next = next;
    }
}

pub type MachineInstList = List<MachineInst, MachineInstNode>;

/// The block node in the list.
#[derive(Default)]
pub struct MachineBlockNode {
    prev: Option<MachineBlock>,
    next: Option<MachineBlock>,
    /// The instructions in the block.
    insts: MachineInstList,
}

pub type MachineBlockList = List<MachineBlock, MachineBlockNode>;

impl MachineBlockNode {
    pub fn insts(&self) -> &MachineInstList {
        &self.insts
    }

    pub fn insts_mut(&mut self) -> &mut MachineInstList {
        &mut self.insts
    }

    pub fn to_asm(&self, ctx: &MachineContext) -> String {
        let mut asm = String::new();
        for (inst_id, _) in self.insts.into_iter() {
            let data = ctx.inst_data(inst_id).unwrap();
            asm.push_str(&format!("\t{}\n", data));
        }
        asm
    }
}

impl ListNode<MachineBlock> for MachineBlockNode {
    fn prev(&self) -> Option<MachineBlock> {
        self.prev
    }

    fn next(&self) -> Option<MachineBlock> {
        self.next
    }

    fn set_prev(&mut self, prev: Option<MachineBlock>) {
        self.prev = prev;
    }

    fn set_next(&mut self, next: Option<MachineBlock>) {
        self.next = next;
    }
}

#[derive(Default)]
pub struct MachineContext {
    /// The allocator for virtual register ids.
    vreg_allocator: IdAllocator,
    /// The allocator for block ids.
    block_allocator: IdAllocator,
    /// The allocator for instruction ids.
    inst_allocator: IdAllocator,
    /// The functions in the context.
    functions: HashMap<MachineSymbol, MachineFunctionData>,
    /// The globals in the context.
    globals: HashMap<MachineSymbol, MachineGlobalData>,
    /// The instructions in the context.
    ///
    /// TODO: This arena-style storage can be reimplemented with a simple [Vec]
    insts: HashMap<MachineInst, InstData>,
}

impl MachineContext {
    pub fn new() -> Self {
        Self::default()
    }

    /// Create a new virtual register.
    pub fn new_vreg(&mut self) -> Register {
        Register::Virtual(VirtualRegister(self.vreg_allocator.allocate()))
    }

    /// Create a new block.
    pub fn new_block(&mut self) -> MachineBlock {
        MachineBlock(self.block_allocator.allocate())
    }

    /// Create a new instruction.
    fn new_inst(&mut self, data: InstData) -> MachineInst {
        let inst = MachineInst(self.inst_allocator.allocate());
        self.insts.insert(inst, data);
        inst
    }

    /// Get the instruction data.
    fn inst_data(&self, inst: MachineInst) -> Option<&InstData> {
        self.insts.get(&inst)
    }

    /// Get the mutable instruction data.
    fn inst_data_mut(&mut self, inst: MachineInst) -> Option<&mut InstData> {
        self.insts.get_mut(&inst)
    }

    /// Get the function data.
    pub fn function_data(&self, symbol: &MachineSymbol) -> Option<&MachineFunctionData> {
        self.functions.get(symbol)
    }

    /// Get the mutable function data.
    pub fn function_data_mut(
        &mut self,
        symbol: &MachineSymbol,
    ) -> Option<&mut MachineFunctionData> {
        self.functions.get_mut(symbol)
    }

    /// Create a new function.
    pub fn new_function(&mut self, name: MachineSymbol) {
        self.functions.insert(
            name,
            MachineFunctionData {
                layout: MachineLayout::new(),
            },
        );
    }

    /// Create a new global.
    pub fn new_global(&mut self, name: MachineSymbol, data: MachineGlobalData) {
        self.globals.insert(name, data);
    }
}

impl fmt::Display for MachineContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // .option pic
        writeln!(f, "\t.option pic")?;
        // .text
        writeln!(f, "\t.text")?;
        for (func_name, func_data) in self.functions.iter() {
            // .global <name>
            writeln!(f, "\t.global {}", func_name)?;
            // .align 1
            writeln!(f, "\t.align 1")?;
            // .type <name>, @function
            writeln!(f, "\t.type {}, @function", func_name)?;
            // <name>:
            writeln!(f, "{}:", func_name)?;
            // instructions
            writeln!(f, "{}", func_data.to_asm(self))?;
        }

        for (global_name, global_data) in self.globals.iter() {
            // .type <name>, @object
            writeln!(f, "\t.type {}, @object", global_name)?;
            match global_data.kind() {
                MachineGlobalKind::Bss { size } => {
                    // .bss
                    writeln!(f, "\t.bss")?;
                    // .global <name>
                    writeln!(f, "\t.global {}", global_name)?;
                    // .align 2
                    writeln!(f, "\t.align 2")?;
                    // <name>:
                    writeln!(f, "{}:", global_name)?;
                    // .zero <size>
                    writeln!(f, "\t.zero {}", size)?;
                }
                MachineGlobalKind::Data { data } => {
                    // .data
                    writeln!(f, "\t.data")?;
                    // .global <name>
                    writeln!(f, "\t.global {}", global_name)?;
                    // .align 2
                    writeln!(f, "\t.align 2")?;
                    // <name>:
                    writeln!(f, "{}:", global_name)?;
                    // .byte <data>
                    write!(f, "\t.byte")?;
                    for byte in data.iter() {
                        write!(f, " {}", byte)?;
                    }
                    writeln!(f)?;
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MachineBlock(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MachineInst(usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VirtualRegister(usize);

pub enum RiscvGeneralPurposeRegister {
    Zero = 0,
    Ra = 1,
    Sp = 2,
    Gp = 3,
    Tp = 4,
    T0 = 5,
    T1 = 6,
    T2 = 7,
    S0 = 8,
    S1 = 9,
    A0 = 10,
    A1 = 11,
    A2 = 12,
    A3 = 13,
    A4 = 14,
    A5 = 15,
    A6 = 16,
    A7 = 17,
    S2 = 18,
    S3 = 19,
    S4 = 20,
    S5 = 21,
    S6 = 22,
    S7 = 23,
    S8 = 24,
    S9 = 25,
    S10 = 26,
    S11 = 27,
    T3 = 28,
    T4 = 29,
    T5 = 30,
    T6 = 31,
}

impl fmt::Display for RiscvGeneralPurposeRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RiscvGeneralPurposeRegister::Zero => write!(f, "zero"),
            RiscvGeneralPurposeRegister::Ra => write!(f, "ra"),
            RiscvGeneralPurposeRegister::Sp => write!(f, "sp"),
            RiscvGeneralPurposeRegister::Gp => write!(f, "gp"),
            RiscvGeneralPurposeRegister::Tp => write!(f, "tp"),
            RiscvGeneralPurposeRegister::T0 => write!(f, "t0"),
            RiscvGeneralPurposeRegister::T1 => write!(f, "t1"),
            RiscvGeneralPurposeRegister::T2 => write!(f, "t2"),
            RiscvGeneralPurposeRegister::S0 => write!(f, "s0"),
            RiscvGeneralPurposeRegister::S1 => write!(f, "s1"),
            RiscvGeneralPurposeRegister::A0 => write!(f, "a0"),
            RiscvGeneralPurposeRegister::A1 => write!(f, "a1"),
            RiscvGeneralPurposeRegister::A2 => write!(f, "a2"),
            RiscvGeneralPurposeRegister::A3 => write!(f, "a3"),
            RiscvGeneralPurposeRegister::A4 => write!(f, "a4"),
            RiscvGeneralPurposeRegister::A5 => write!(f, "a5"),
            RiscvGeneralPurposeRegister::A6 => write!(f, "a6"),
            RiscvGeneralPurposeRegister::A7 => write!(f, "a7"),
            RiscvGeneralPurposeRegister::S2 => write!(f, "s2"),
            RiscvGeneralPurposeRegister::S3 => write!(f, "s3"),
            RiscvGeneralPurposeRegister::S4 => write!(f, "s4"),
            RiscvGeneralPurposeRegister::S5 => write!(f, "s5"),
            RiscvGeneralPurposeRegister::S6 => write!(f, "s6"),
            RiscvGeneralPurposeRegister::S7 => write!(f, "s7"),
            RiscvGeneralPurposeRegister::S8 => write!(f, "s8"),
            RiscvGeneralPurposeRegister::S9 => write!(f, "s9"),
            RiscvGeneralPurposeRegister::S10 => write!(f, "s10"),
            RiscvGeneralPurposeRegister::S11 => write!(f, "s11"),
            RiscvGeneralPurposeRegister::T3 => write!(f, "t3"),
            RiscvGeneralPurposeRegister::T4 => write!(f, "t4"),
            RiscvGeneralPurposeRegister::T5 => write!(f, "t5"),
            RiscvGeneralPurposeRegister::T6 => write!(f, "t6"),
        }
    }
}

pub enum RiscvFloatingPointRegister {
    Ft0 = 0,
    Ft1 = 1,
    Ft2 = 2,
    Ft3 = 3,
    Ft4 = 4,
    Ft5 = 5,
    Ft6 = 6,
    Ft7 = 7,
    Fs0 = 8,
    Fs1 = 9,
    Fa0 = 10,
    Fa1 = 11,
    Fa2 = 12,
    Fa3 = 13,
    Fa4 = 14,
    Fa5 = 15,
    Fa6 = 16,
    Fa7 = 17,
    Fs2 = 18,
    Fs3 = 19,
    Fs4 = 20,
    Fs5 = 21,
    Fs6 = 22,
    Fs7 = 23,
    Fs8 = 24,
    Fs9 = 25,
    Fs10 = 26,
    Fs11 = 27,
    Ft8 = 28,
    Ft9 = 29,
    Ft10 = 30,
    Ft11 = 31,
}

impl fmt::Display for RiscvFloatingPointRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RiscvFloatingPointRegister::Ft0 => write!(f, "ft0"),
            RiscvFloatingPointRegister::Ft1 => write!(f, "ft1"),
            RiscvFloatingPointRegister::Ft2 => write!(f, "ft2"),
            RiscvFloatingPointRegister::Ft3 => write!(f, "ft3"),
            RiscvFloatingPointRegister::Ft4 => write!(f, "ft4"),
            RiscvFloatingPointRegister::Ft5 => write!(f, "ft5"),
            RiscvFloatingPointRegister::Ft6 => write!(f, "ft6"),
            RiscvFloatingPointRegister::Ft7 => write!(f, "ft7"),
            RiscvFloatingPointRegister::Fs0 => write!(f, "fs0"),
            RiscvFloatingPointRegister::Fs1 => write!(f, "fs1"),
            RiscvFloatingPointRegister::Fa0 => write!(f, "fa0"),
            RiscvFloatingPointRegister::Fa1 => write!(f, "fa1"),
            RiscvFloatingPointRegister::Fa2 => write!(f, "fa2"),
            RiscvFloatingPointRegister::Fa3 => write!(f, "fa3"),
            RiscvFloatingPointRegister::Fa4 => write!(f, "fa4"),
            RiscvFloatingPointRegister::Fa5 => write!(f, "fa5"),
            RiscvFloatingPointRegister::Fa6 => write!(f, "fa6"),
            RiscvFloatingPointRegister::Fa7 => write!(f, "fa7"),
            RiscvFloatingPointRegister::Fs2 => write!(f, "fs2"),
            RiscvFloatingPointRegister::Fs3 => write!(f, "fs3"),
            RiscvFloatingPointRegister::Fs4 => write!(f, "fs4"),
            RiscvFloatingPointRegister::Fs5 => write!(f, "fs5"),
            RiscvFloatingPointRegister::Fs6 => write!(f, "fs6"),
            RiscvFloatingPointRegister::Fs7 => write!(f, "fs7"),
            RiscvFloatingPointRegister::Fs8 => write!(f, "fs8"),
            RiscvFloatingPointRegister::Fs9 => write!(f, "fs9"),
            RiscvFloatingPointRegister::Fs10 => write!(f, "fs10"),
            RiscvFloatingPointRegister::Fs11 => write!(f, "fs11"),
            RiscvFloatingPointRegister::Ft8 => write!(f, "ft8"),
            RiscvFloatingPointRegister::Ft9 => write!(f, "ft9"),
            RiscvFloatingPointRegister::Ft10 => write!(f, "ft10"),
            RiscvFloatingPointRegister::Ft11 => write!(f, "ft11"),
        }
    }
}

impl fmt::Display for VirtualRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "$v{}", self.0)
    }
}

pub enum Register {
    General(RiscvGeneralPurposeRegister),
    FloatingPoint(RiscvFloatingPointRegister),
    Virtual(VirtualRegister),
}

impl Register {
    pub fn as_virtual_register(&self) -> Option<VirtualRegister> {
        match self {
            Register::Virtual(vreg) => Some(*vreg),
            _ => None,
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Register::General(reg) => write!(f, "{}", reg),
            Register::FloatingPoint(reg) => write!(f, "{}", reg),
            Register::Virtual(reg) => write!(f, "{}", reg),
        }
    }
}

pub struct Immediate(i128);

impl fmt::Display for Immediate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl From<i128> for Immediate {
    fn from(value: i128) -> Self {
        Immediate(value)
    }
}

impl From<u64> for Immediate {
    fn from(value: u64) -> Self {
        Immediate(value as i128)
    }
}

impl From<u32> for Immediate {
    fn from(value: u32) -> Self {
        Immediate(value as i128)
    }
}

impl From<u16> for Immediate {
    fn from(value: u16) -> Self {
        Immediate(value as i128)
    }
}

impl From<u8> for Immediate {
    fn from(value: u8) -> Self {
        Immediate(value as i128)
    }
}

impl From<i64> for Immediate {
    fn from(value: i64) -> Self {
        Immediate(value as i128)
    }
}

impl From<i32> for Immediate {
    fn from(value: i32) -> Self {
        Immediate(value as i128)
    }
}

impl From<i16> for Immediate {
    fn from(value: i16) -> Self {
        Immediate(value as i128)
    }
}

impl From<i8> for Immediate {
    fn from(value: i8) -> Self {
        Immediate(value as i128)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MachineSymbol(String);

impl fmt::Display for MachineSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct MachineGlobalData {
    align: usize,
    kind: MachineGlobalKind,
}

pub enum MachineGlobalKind {
    Bss { size: usize },
    Data { data: Vec<u8> },
}

impl MachineGlobalData {
    pub fn new_bss(size: usize, align: usize) -> Self {
        Self {
            align,
            kind: MachineGlobalKind::Bss { size },
        }
    }

    pub fn new_data(data: Vec<u8>, align: usize) -> Self {
        Self {
            align,
            kind: MachineGlobalKind::Data { data },
        }
    }

    pub fn align(&self) -> usize {
        self.align
    }

    pub fn kind(&self) -> &MachineGlobalKind {
        &self.kind
    }

    pub fn set_kind(&mut self, kind: MachineGlobalKind) {
        self.kind = kind;
    }
}

impl From<String> for MachineSymbol {
    fn from(name: String) -> Self {
        // TODO: check the name, the global names in IR starts with `@`, the validity
        // needs to be checked.
        MachineSymbol(name)
    }
}

pub enum InstData {
    Load {
        kind: LoadKind,
        dest: Register,
        base: Register,
        offset: Immediate,
    },
    FloatLoad {
        kind: FloatLoadKind,
        dest: Register,
        base: Register,
        offset: Immediate,
    },
    PseudoLoad {
        kind: PseudoLoadKind,
        dest: Register,
        symbol: MachineSymbol,
    },
    PseudoStore {
        kind: PseudoStoreKind,
        value: Register,
        symbol: MachineSymbol,
        rt: Register,
    },
    FloatPseudoLoad {
        kind: FloatPseudoLoad,
        dest: Register,
        symbol: MachineSymbol,
        rt: Register,
    },
    FloatPseudoStore {
        kind: FloatPseudoStore,
        value: Register,
        symbol: MachineSymbol,
        rt: Register,
    },
    Store {
        kind: StoreKind,
        value: Register,
        base: Register,
        offset: Immediate,
    },
    FloatStore {
        kind: FloatStoreKind,
        value: Register,
        base: Register,
        offset: Immediate,
    },
    FloatMove {
        dst_fmt: FloatMoveFmt,
        src_fmt: FloatMoveFmt,

        rd: Register,
        rs: Register,
    },
    FloatConvert {
        dst_fmt: FloatConvertFmt,
        src_fmt: FloatConvertFmt,
        rd: Register,
        rs: Register,
    },
    Binary {
        kind: BinaryOpKind,
        rd: Register,
        rs1: Register,
        rs2: Register,
    },
    BinaryImm {
        kind: BinaryImmOpKind,
        rd: Register,
        rs1: Register,
        imm: Immediate,
    },
    FloatBinary {
        kind: FloatBinaryOpKind,
        fmt: FloatBinaryFmt,
        rd: Register,
        rs1: Register,
        rs2: Register,
    },
    FloatMulAdd {
        kind: FloatMulAdd,
        fmt: FloatMulAddFmt,
        rd: Register,
        rs1: Register,
        rs2: Register,
        rs3: Register,
    },
    FloatUnary {
        kind: FloatUnary,
        fmt: FloatUnaryFmt,
        rd: Register,
        rs: Register,
    },
    Li {
        rd: Register,
        imm: Immediate,
    },
    Ret,
    Call {
        symbol: MachineSymbol,
    },
    Branch {
        kind: BranchOpKind,
        rs1: Register,
        rs2: Register,
        block: MachineBlock,
    },
    J {
        block: MachineBlock,
    },
}

impl InstData {
    pub fn new_load(
        ctx: &mut MachineContext,
        kind: LoadKind,
        base: Register,
        offset: Immediate,
    ) -> MachineInst {
        let dest = ctx.new_vreg();
        let data = InstData::Load {
            kind,
            dest,
            base,
            offset,
        };
        ctx.new_inst(data)
    }

    pub fn new_float_load(
        ctx: &mut MachineContext,
        kind: FloatLoadKind,
        base: Register,
        offset: Immediate,
    ) -> MachineInst {
        let dest = ctx.new_vreg();
        let data = InstData::FloatLoad {
            kind,
            dest,
            base,
            offset,
        };
        ctx.new_inst(data)
    }

    pub fn new_pseudo_load(
        ctx: &mut MachineContext,
        kind: PseudoLoadKind,
        symbol: MachineSymbol,
    ) -> MachineInst {
        let dest = ctx.new_vreg();
        let data = InstData::PseudoLoad { kind, dest, symbol };
        ctx.new_inst(data)
    }

    pub fn new_pseudo_store(
        ctx: &mut MachineContext,
        kind: PseudoStoreKind,
        value: Register,
        symbol: MachineSymbol,
        rt: Register,
    ) -> MachineInst {
        let data = InstData::PseudoStore {
            kind,
            value,
            symbol,
            rt,
        };
        ctx.new_inst(data)
    }

    pub fn new_float_pseudo_load(
        ctx: &mut MachineContext,
        kind: FloatPseudoLoad,
        symbol: MachineSymbol,
        rt: Register,
    ) -> MachineInst {
        let dest = ctx.new_vreg();
        let data = InstData::FloatPseudoLoad {
            kind,
            dest,
            symbol,
            rt,
        };
        ctx.new_inst(data)
    }

    pub fn new_float_pseudo_store(
        ctx: &mut MachineContext,
        kind: FloatPseudoStore,
        value: Register,
        symbol: MachineSymbol,
        rt: Register,
    ) -> MachineInst {
        let data = InstData::FloatPseudoStore {
            kind,
            value,
            symbol,
            rt,
        };
        ctx.new_inst(data)
    }

    pub fn new_store(
        ctx: &mut MachineContext,
        kind: StoreKind,
        value: Register,
        base: Register,
        offset: Immediate,
    ) -> MachineInst {
        let data = InstData::Store {
            kind,
            value,
            base,
            offset,
        };
        ctx.new_inst(data)
    }

    pub fn new_float_store(
        ctx: &mut MachineContext,
        kind: FloatStoreKind,
        value: Register,
        base: Register,
        offset: Immediate,
    ) -> MachineInst {
        let data = InstData::FloatStore {
            kind,
            value,
            base,
            offset,
        };
        ctx.new_inst(data)
    }

    pub fn new_float_move(
        ctx: &mut MachineContext,
        dst_fmt: FloatMoveFmt,
        src_fmt: FloatMoveFmt,
        rd: Register,
        rs: Register,
    ) -> MachineInst {
        let data = InstData::FloatMove {
            dst_fmt,
            src_fmt,
            rd,
            rs,
        };
        ctx.new_inst(data)
    }

    pub fn new_float_convert(
        ctx: &mut MachineContext,
        dst_fmt: FloatConvertFmt,
        src_fmt: FloatConvertFmt,
        rd: Register,
        rs: Register,
    ) -> MachineInst {
        let data = InstData::FloatConvert {
            dst_fmt,
            src_fmt,
            rd,
            rs,
        };
        ctx.new_inst(data)
    }

    pub fn new_binary(
        ctx: &mut MachineContext,
        kind: BinaryOpKind,
        rd: Register,
        rs1: Register,
        rs2: Register,
    ) -> MachineInst {
        let data = InstData::Binary { kind, rd, rs1, rs2 };
        ctx.new_inst(data)
    }

    pub fn new_binary_imm(
        ctx: &mut MachineContext,
        kind: BinaryImmOpKind,
        rd: Register,
        rs1: Register,
        imm: Immediate,
    ) -> MachineInst {
        let data = InstData::BinaryImm { kind, rd, rs1, imm };
        ctx.new_inst(data)
    }

    pub fn new_float_binary(
        ctx: &mut MachineContext,
        kind: FloatBinaryOpKind,
        fmt: FloatBinaryFmt,
        rd: Register,
        rs1: Register,
        rs2: Register,
    ) -> MachineInst {
        let data = InstData::FloatBinary {
            kind,
            fmt,
            rd,
            rs1,
            rs2,
        };
        ctx.new_inst(data)
    }

    pub fn new_float_mul_add(
        ctx: &mut MachineContext,
        kind: FloatMulAdd,
        fmt: FloatMulAddFmt,
        rd: Register,
        rs1: Register,
        rs2: Register,
        rs3: Register,
    ) -> MachineInst {
        let data = InstData::FloatMulAdd {
            kind,
            fmt,
            rd,
            rs1,
            rs2,
            rs3,
        };
        ctx.new_inst(data)
    }

    pub fn new_float_unary(
        ctx: &mut MachineContext,
        kind: FloatUnary,
        fmt: FloatUnaryFmt,
        rd: Register,
        rs: Register,
    ) -> MachineInst {
        let data = InstData::FloatUnary { kind, fmt, rd, rs };
        ctx.new_inst(data)
    }

    pub fn new_li(ctx: &mut MachineContext, rd: Register, imm: Immediate) -> MachineInst {
        let data = InstData::Li { rd, imm };
        ctx.new_inst(data)
    }

    pub fn new_ret(ctx: &mut MachineContext) -> MachineInst {
        let data = InstData::Ret;
        ctx.new_inst(data)
    }

    pub fn new_call(ctx: &mut MachineContext, symbol: MachineSymbol) -> MachineInst {
        let data = InstData::Call { symbol };
        ctx.new_inst(data)
    }

    pub fn new_branch(
        ctx: &mut MachineContext,
        kind: BranchOpKind,
        rs1: Register,
        rs2: Register,
        block: MachineBlock,
    ) -> MachineInst {
        let data = InstData::Branch {
            kind,
            rs1,
            rs2,
            block,
        };
        ctx.new_inst(data)
    }

    pub fn new_j(ctx: &mut MachineContext, block: MachineBlock) -> MachineInst {
        let data = InstData::J { block };
        ctx.new_inst(data)
    }

    pub fn is_load(&self) -> bool {
        matches!(
            self,
            InstData::Load { .. }
                | InstData::FloatLoad { .. }
                | InstData::PseudoLoad { .. }
                | InstData::FloatPseudoLoad { .. }
        )
    }

    pub fn is_store(&self) -> bool {
        matches!(
            self,
            InstData::Store { .. }
                | InstData::FloatStore { .. }
                | InstData::PseudoStore { .. }
                | InstData::FloatPseudoStore { .. }
        )
    }

    pub fn is_branch(&self) -> bool {
        matches!(self, InstData::Branch { .. } | InstData::J { .. })
    }

    pub fn is_call(&self) -> bool {
        matches!(self, InstData::Call { .. })
    }

    pub fn is_ret(&self) -> bool {
        matches!(self, InstData::Ret)
    }

    pub fn is_move(&self) -> bool {
        matches!(self, InstData::FloatMove { .. })
    }

    pub fn is_convert(&self) -> bool {
        matches!(self, InstData::FloatConvert { .. })
    }

    pub fn is_binary(&self) -> bool {
        matches!(self, InstData::Binary { .. } | InstData::BinaryImm { .. })
    }

    pub fn is_float_binary(&self) -> bool {
        matches!(self, InstData::FloatBinary { .. })
    }
}

impl fmt::Display for InstData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InstData::Load {
                kind,
                dest,
                base,
                offset,
            } => write!(f, "{} {}, {}({})", kind, dest, offset, base)?,
            InstData::FloatLoad {
                kind,
                dest,
                base,
                offset,
            } => write!(f, "{} {}, {}({})", kind, dest, offset, base)?,
            InstData::PseudoLoad { kind, dest, symbol } => {
                write!(f, "{} {}, {}", kind, dest, symbol)?
            }
            InstData::PseudoStore {
                kind,
                value,
                symbol,
                rt,
            } => write!(f, "{} {}, {}, {}", kind, value, symbol, rt)?,
            InstData::FloatPseudoLoad {
                kind,
                dest,
                symbol,
                rt,
            } => write!(f, "{} {}, {}, {}", kind, dest, symbol, rt)?,
            InstData::FloatPseudoStore {
                kind,
                value,
                symbol,
                rt,
            } => write!(f, "{} {}, {}, {}", kind, value, symbol, rt)?,
            InstData::Store {
                kind,
                value,
                base,
                offset,
            } => write!(f, "{} {}, {}({})", kind, value, offset, base)?,
            InstData::FloatStore {
                kind,
                value,
                base,
                offset,
            } => write!(f, "{} {}, {}({})", kind, value, offset, base)?,
            InstData::FloatMove {
                dst_fmt,
                src_fmt,
                rd,
                rs,
            } => write!(f, "fmv.{}.{} {}, {}", dst_fmt, src_fmt, rd, rs)?,
            InstData::FloatConvert {
                dst_fmt,
                src_fmt,
                rd,
                rs,
            } => write!(f, "fcvt.{}.{} {}, {}", dst_fmt, src_fmt, rd, rs)?,
            InstData::Binary { kind, rd, rs1, rs2 } => {
                write!(f, "{} {}, {}, {}", kind, rd, rs1, rs2)?
            }
            InstData::BinaryImm { kind, rd, rs1, imm } => {
                write!(f, "{} {}, {}, {}", kind, rd, rs1, imm)?
            }
            InstData::FloatBinary {
                kind,
                fmt,
                rd,
                rs1,
                rs2,
            } => write!(f, "{}.{} {}, {}, {}", kind, fmt, rd, rs1, rs2)?,
            InstData::FloatMulAdd {
                kind,
                fmt,
                rd,
                rs1,
                rs2,
                rs3,
            } => write!(f, "{}.{} {}, {}, {}, {}", kind, fmt, rd, rs1, rs2, rs3)?,
            InstData::FloatUnary { kind, fmt, rd, rs } => {
                write!(f, "{}.{} {}, {}", kind, fmt, rd, rs)?
            }
            InstData::Li { rd, imm } => write!(f, "li {}, {}", rd, imm)?,
            InstData::Ret => write!(f, "ret")?,
            InstData::Call { symbol } => write!(f, "call {}", symbol)?,
            InstData::Branch {
                kind,
                rs1,
                rs2,
                block,
            } => write!(f, "{} {}, {}, .bb_{}", kind, rs1, rs2, block.0)?,
            InstData::J { block } => write!(f, "j .bb_{}", block.0)?,
        }

        Ok(())
    }
}

pub enum FloatConvertFmt {
    H,
    S,
    D,
    W,
    Wu,
    L,
    Lu,
}

impl fmt::Display for FloatConvertFmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatConvertFmt::H => write!(f, "h"),
            FloatConvertFmt::S => write!(f, "s"),
            FloatConvertFmt::D => write!(f, "d"),
            FloatConvertFmt::W => write!(f, "w"),
            FloatConvertFmt::Wu => write!(f, "wu"),
            FloatConvertFmt::L => write!(f, "l"),
            FloatConvertFmt::Lu => write!(f, "lu"),
        }
    }
}

pub enum FloatMoveFmt {
    H,
    S,
    D,
    X,
}

impl fmt::Display for FloatMoveFmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatMoveFmt::H => write!(f, "h"),
            FloatMoveFmt::S => write!(f, "s"),
            FloatMoveFmt::D => write!(f, "d"),
            FloatMoveFmt::X => write!(f, "x"),
        }
    }
}

pub enum LoadKind {
    Byte,
    Half,
    Word,
    ByteUnsigned,
    HalfUnsigned,
    WordUnsigned,
    DoubleWord,
}

impl fmt::Display for LoadKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LoadKind::Byte => write!(f, "lb"),
            LoadKind::Half => write!(f, "lh"),
            LoadKind::Word => write!(f, "lw"),
            LoadKind::ByteUnsigned => write!(f, "lbu"),
            LoadKind::HalfUnsigned => write!(f, "lhu"),
            LoadKind::WordUnsigned => write!(f, "lwu"),
            LoadKind::DoubleWord => write!(f, "ld"),
        }
    }
}

pub enum StoreKind {
    Byte,
    Half,
    Word,
    DoubleWord,
}

impl fmt::Display for StoreKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StoreKind::Byte => write!(f, "sb"),
            StoreKind::Half => write!(f, "sh"),
            StoreKind::Word => write!(f, "sw"),
            StoreKind::DoubleWord => write!(f, "sd"),
        }
    }
}

pub enum FloatStoreKind {
    /// Fsw
    Single,
    /// Fsd
    Double,
}

impl fmt::Display for FloatStoreKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatStoreKind::Single => write!(f, "fsw"),
            FloatStoreKind::Double => write!(f, "fsd"),
        }
    }
}

pub enum FloatLoadKind {
    /// Flw
    Single,
    /// Fld
    Double,
}

impl fmt::Display for FloatLoadKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatLoadKind::Single => write!(f, "flw"),
            FloatLoadKind::Double => write!(f, "fld"),
        }
    }
}

pub enum PseudoLoadKind {
    /// LA
    Address,
    /// LW
    Word,
}

impl fmt::Display for PseudoLoadKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PseudoLoadKind::Address => write!(f, "la"),
            PseudoLoadKind::Word => write!(f, "lw"),
        }
    }
}

pub enum PseudoStoreKind {
    /// SW
    Word,
}

impl fmt::Display for PseudoStoreKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PseudoStoreKind::Word => write!(f, "sw"),
        }
    }
}

pub enum FloatPseudoLoad {
    /// FLW
    Single,
}

impl fmt::Display for FloatPseudoLoad {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatPseudoLoad::Single => write!(f, "flw"),
        }
    }
}

pub enum FloatPseudoStore {
    /// FSW
    Single,
}

impl fmt::Display for FloatPseudoStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatPseudoStore::Single => write!(f, "fsw"),
        }
    }
}

pub enum BinaryOpKind {
    Add,
    Addw,
    Sub,
    Subw,
    Sll,
    Sllw,
    Srl,
    Srlw,
    Sra,
    Sraw,
    Xor,
    Or,
    And,
    Slt,
    Sltu,
    Mul,
    Mulw,
    Mulh,
    Mulhsu,
    Mulhu,
    Div,
    Divw,
    Divu,
    Rem,
    Remw,
    Remu,
    Rew,
}

impl fmt::Display for BinaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOpKind::Add => write!(f, "add"),
            BinaryOpKind::Addw => write!(f, "addw"),
            BinaryOpKind::Sub => write!(f, "sub"),
            BinaryOpKind::Subw => write!(f, "subw"),
            BinaryOpKind::Sll => write!(f, "sll"),
            BinaryOpKind::Sllw => write!(f, "sllw"),
            BinaryOpKind::Srl => write!(f, "srl"),
            BinaryOpKind::Srlw => write!(f, "srlw"),
            BinaryOpKind::Sra => write!(f, "sra"),
            BinaryOpKind::Sraw => write!(f, "sraw"),
            BinaryOpKind::Xor => write!(f, "xor"),
            BinaryOpKind::Or => write!(f, "or"),
            BinaryOpKind::And => write!(f, "and"),
            BinaryOpKind::Slt => write!(f, "slt"),
            BinaryOpKind::Sltu => write!(f, "sltu"),
            BinaryOpKind::Mul => write!(f, "mul"),
            BinaryOpKind::Mulw => write!(f, "mulw"),
            BinaryOpKind::Mulh => write!(f, "mulh"),
            BinaryOpKind::Mulhsu => write!(f, "mulhsu"),
            BinaryOpKind::Mulhu => write!(f, "mulhu"),
            BinaryOpKind::Div => write!(f, "div"),
            BinaryOpKind::Divw => write!(f, "divw"),
            BinaryOpKind::Divu => write!(f, "divu"),
            BinaryOpKind::Rem => write!(f, "rem"),
            BinaryOpKind::Remw => write!(f, "remw"),
            BinaryOpKind::Remu => write!(f, "remu"),
            BinaryOpKind::Rew => write!(f, "rew"),
        }
    }
}

pub enum BranchOpKind {
    Beq,
    Bne,
    Blt,
    Bge,
    Bltu,
    Bgeu,
}

impl fmt::Display for BranchOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BranchOpKind::Beq => write!(f, "beq"),
            BranchOpKind::Bne => write!(f, "bne"),
            BranchOpKind::Blt => write!(f, "blt"),
            BranchOpKind::Bge => write!(f, "bge"),
            BranchOpKind::Bltu => write!(f, "bltu"),
            BranchOpKind::Bgeu => write!(f, "bgeu"),
        }
    }
}

pub enum BinaryImmOpKind {
    Addi,
    Addiw,
    Slli,
    Slliw,
    Srli,
    Srliw,
    Srai,
    Sraiw,
    Xori,
    Ori,
    Andi,
    Slti,
    Sltiu,
}

impl fmt::Display for BinaryImmOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryImmOpKind::Addi => write!(f, "addi"),
            BinaryImmOpKind::Addiw => write!(f, "addiw"),
            BinaryImmOpKind::Slli => write!(f, "slli"),
            BinaryImmOpKind::Slliw => write!(f, "slliw"),
            BinaryImmOpKind::Srli => write!(f, "srli"),
            BinaryImmOpKind::Srliw => write!(f, "srliw"),
            BinaryImmOpKind::Srai => write!(f, "srai"),
            BinaryImmOpKind::Sraiw => write!(f, "sraiw"),
            BinaryImmOpKind::Xori => write!(f, "xori"),
            BinaryImmOpKind::Ori => write!(f, "ori"),
            BinaryImmOpKind::Andi => write!(f, "andi"),
            BinaryImmOpKind::Slti => write!(f, "slti"),
            BinaryImmOpKind::Sltiu => write!(f, "sltiu"),
        }
    }
}

pub enum FloatBinaryOpKind {
    Fadd,
    Fsub,
    Fmul,
    Fdiv,
    Fsgnj,
    Fsgnjn,
    Fsgnjx,
    Fmin,
    Fmax,
    Feq,
    Flt,
    Fle,
}

impl fmt::Display for FloatBinaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatBinaryOpKind::Fadd => write!(f, "fadd"),
            FloatBinaryOpKind::Fsub => write!(f, "fsub"),
            FloatBinaryOpKind::Fmul => write!(f, "fmul"),
            FloatBinaryOpKind::Fdiv => write!(f, "fdiv"),
            FloatBinaryOpKind::Fsgnj => write!(f, "fsgnj"),
            FloatBinaryOpKind::Fsgnjn => write!(f, "fsgnjn"),
            FloatBinaryOpKind::Fsgnjx => write!(f, "fsgnjx"),
            FloatBinaryOpKind::Fmin => write!(f, "fmin"),
            FloatBinaryOpKind::Fmax => write!(f, "fmax"),
            FloatBinaryOpKind::Feq => write!(f, "feq"),
            FloatBinaryOpKind::Flt => write!(f, "flt"),
            FloatBinaryOpKind::Fle => write!(f, "fle"),
        }
    }
}

pub enum FloatBinaryFmt {
    S,
    D,
}

impl fmt::Display for FloatBinaryFmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatBinaryFmt::S => write!(f, "s"),
            FloatBinaryFmt::D => write!(f, "d"),
        }
    }
}

pub enum FloatMulAdd {
    Fmadd,
    Fmsub,
    Fnmsub,
    Fnmadd,
}

impl fmt::Display for FloatMulAdd {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatMulAdd::Fmadd => write!(f, "fmadd"),
            FloatMulAdd::Fmsub => write!(f, "fmsub"),
            FloatMulAdd::Fnmsub => write!(f, "fnmsub"),
            FloatMulAdd::Fnmadd => write!(f, "fnmadd"),
        }
    }
}

pub enum FloatMulAddFmt {
    S,
    D,
}

impl fmt::Display for FloatMulAddFmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatMulAddFmt::S => write!(f, "s"),
            FloatMulAddFmt::D => write!(f, "d"),
        }
    }
}

pub enum FloatUnary {
    FSqrt,
    FClass,
}

impl fmt::Display for FloatUnary {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatUnary::FSqrt => write!(f, "fsqrt"),
            FloatUnary::FClass => write!(f, "fclass"),
        }
    }
}

pub enum FloatUnaryFmt {
    S,
    D,
}

impl fmt::Display for FloatUnaryFmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatUnaryFmt::S => write!(f, "s"),
            FloatUnaryFmt::D => write!(f, "d"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_asm() {
        let mut ctx = MachineContext::new();
        // main func
        let main_func = MachineSymbol("main".to_string());
        ctx.new_function(main_func);
        let bb0 = MachineBlock(0);
        ctx.function_data_mut(&MachineSymbol("main".to_string()))
            .unwrap()
            .layout_mut()
            .append_block(bb0)
            .unwrap();
        let inst0 = InstData::new_load(
            &mut ctx,
            LoadKind::Word,
            Register::Virtual(VirtualRegister(0)),
            Immediate(0),
        );
        ctx.function_data_mut(&MachineSymbol("main".to_string()))
            .unwrap()
            .layout_mut()
            .append_inst(inst0, bb0)
            .unwrap();
        let inst1 = InstData::new_binary_imm(
            &mut ctx,
            BinaryImmOpKind::Addi,
            Register::Virtual(VirtualRegister(1)),
            Register::Virtual(VirtualRegister(0)),
            Immediate(1),
        );
        ctx.function_data_mut(&MachineSymbol("main".to_string()))
            .unwrap()
            .layout_mut()
            .append_inst(inst1, bb0)
            .unwrap();

        let bb1 = MachineBlock(1);
        ctx.function_data_mut(&MachineSymbol("main".to_string()))
            .unwrap()
            .layout_mut()
            .append_block(bb1)
            .unwrap();
        let inst2 = InstData::new_binary(
            &mut ctx,
            BinaryOpKind::Add,
            Register::Virtual(VirtualRegister(2)),
            Register::Virtual(VirtualRegister(1)),
            Register::Virtual(VirtualRegister(0)),
        );
        ctx.function_data_mut(&MachineSymbol("main".to_string()))
            .unwrap()
            .layout_mut()
            .append_inst(inst2, bb1)
            .unwrap();

        // print the ctx
        println!("{}", ctx);

        assert_eq!(format!("{}", ctx), "\t.option pic\n\t.text\n\t.global main\n\t.align 1\n\t.type main, @function\nmain:\n.bb_0:\n\tlw $v0, 0($v0)\n\taddi $v1, $v0, 1\n.bb_1:\n\tadd $v2, $v1, $v0\n\n");
    }
}
