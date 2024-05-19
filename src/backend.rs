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
    pub fn new() -> Self { Self::default() }

    /// Get the entry block for the current function.
    ///
    /// The entry block is by default the first block in the layout.
    pub fn entry_block(&self) -> Option<MachineBlock> { self.blocks.front() }

    /// Get the list of blocks.
    pub fn blocks(&self) -> &MachineBlockList { &self.blocks }

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
    pub fn remove_block(&mut self, block: MachineBlock) -> Result<(), MachineLayoutOpErr> {
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

    pub fn remove_inst(&mut self, inst: MachineInst) -> Result<(), MachineLayoutOpErr> {
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
    /// The size of the stack frame.
    stack_size: usize,
    /// The aligned portion of the stack frame.
    stack_align: Option<usize>,
}

impl MachineFunctionData {
    pub fn layout(&self) -> &MachineLayout { &self.layout }

    pub fn layout_mut(&mut self) -> &mut MachineLayout { &mut self.layout }

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

    pub fn add_stack_size(&mut self, size: usize) { self.stack_size += size; }

    /// Get the unaligned stack size.
    pub fn stack_size(&self) -> usize { self.stack_size }

    /// Shrink the stack size.
    pub fn shrink_stack(&mut self, size: usize) { self.stack_size -= size; }

    /// Calculate the alignment of the stack.
    pub fn calculate_alignment(&mut self) {
        // align to 16 bytes
        let aigned_stack_size = (self.stack_size + 15) & !15;
        self.stack_align = Some(aigned_stack_size - self.stack_size);
    }

    /// Get the aligned stack size.
    pub fn aligned_stack_size(&mut self) -> usize {
        if self.stack_align.is_none() {
            self.calculate_alignment();
        }
        self.stack_size + self.stack_align.unwrap()
    }
}

/// An instruction node in the list.
#[derive(Default)]
pub struct MachineInstNode {
    prev: Option<MachineInst>,
    next: Option<MachineInst>,
}

impl ListNode<MachineInst> for MachineInstNode {
    fn prev(&self) -> Option<MachineInst> { self.prev }

    fn next(&self) -> Option<MachineInst> { self.next }

    fn set_prev(&mut self, prev: Option<MachineInst>) { self.prev = prev; }

    fn set_next(&mut self, next: Option<MachineInst>) { self.next = next; }
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
    pub fn insts(&self) -> &MachineInstList { &self.insts }

    pub fn insts_mut(&mut self) -> &mut MachineInstList { &mut self.insts }

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
    fn prev(&self) -> Option<MachineBlock> { self.prev }

    fn next(&self) -> Option<MachineBlock> { self.next }

    fn set_prev(&mut self, prev: Option<MachineBlock>) { self.prev = prev; }

    fn set_next(&mut self, next: Option<MachineBlock>) { self.next = next; }
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
    insts: HashMap<MachineInst, MachineInstData>,
}

impl MachineContext {
    pub fn new() -> Self { Self::default() }

    /// Create a new virtual register.
    pub fn new_virtual_reg(&mut self, kind: VirtualRegisterKind) -> Register {
        Register::Virtual(VirtualRegister {
            id: self.vreg_allocator.allocate(),
            kind,
        })
    }

    pub fn new_gp_reg(&mut self, kind: RiscvGpReg) -> Register { Register::General(kind) }

    pub fn new_fp_reg(&mut self, kind: RiscvFpReg) -> Register { Register::FloatingPoint(kind) }

    /// Create a new block.
    pub fn new_block(&mut self) -> MachineBlock { MachineBlock(self.block_allocator.allocate()) }

    /// Create a new instruction.
    fn new_inst(&mut self, data: MachineInstData) -> MachineInst {
        let inst = MachineInst(self.inst_allocator.allocate());
        self.insts.insert(inst, data);
        inst
    }

    /// Get the instruction data.
    pub fn inst_data(&self, inst: MachineInst) -> Option<&MachineInstData> { self.insts.get(&inst) }

    /// Get the mutable instruction data.
    pub fn inst_data_mut(&mut self, inst: MachineInst) -> Option<&mut MachineInstData> {
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
                stack_size: 0,
                stack_align: None,
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
pub enum VirtualRegisterKind {
    General,
    FloatingPoint,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VirtualRegister {
    id: usize,
    kind: VirtualRegisterKind,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum RiscvGpReg {
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

impl From<u8> for RiscvGpReg {
    fn from(val: u8) -> Self {
        match val {
            0 => RiscvGpReg::Zero,
            1 => RiscvGpReg::Ra,
            2 => RiscvGpReg::Sp,
            3 => RiscvGpReg::Gp,
            4 => RiscvGpReg::Tp,
            5 => RiscvGpReg::T0,
            6 => RiscvGpReg::T1,
            7 => RiscvGpReg::T2,
            8 => RiscvGpReg::S0,
            9 => RiscvGpReg::S1,
            10 => RiscvGpReg::A0,
            11 => RiscvGpReg::A1,
            12 => RiscvGpReg::A2,
            13 => RiscvGpReg::A3,
            14 => RiscvGpReg::A4,
            15 => RiscvGpReg::A5,
            16 => RiscvGpReg::A6,
            17 => RiscvGpReg::A7,
            18 => RiscvGpReg::S2,
            19 => RiscvGpReg::S3,
            20 => RiscvGpReg::S4,
            21 => RiscvGpReg::S5,
            22 => RiscvGpReg::S6,
            23 => RiscvGpReg::S7,
            24 => RiscvGpReg::S8,
            25 => RiscvGpReg::S9,
            26 => RiscvGpReg::S10,
            27 => RiscvGpReg::S11,
            28 => RiscvGpReg::T3,
            29 => RiscvGpReg::T4,
            30 => RiscvGpReg::T5,
            31 => RiscvGpReg::T6,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for RiscvGpReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RiscvGpReg::Zero => write!(f, "zero"),
            RiscvGpReg::Ra => write!(f, "ra"),
            RiscvGpReg::Sp => write!(f, "sp"),
            RiscvGpReg::Gp => write!(f, "gp"),
            RiscvGpReg::Tp => write!(f, "tp"),
            RiscvGpReg::T0 => write!(f, "t0"),
            RiscvGpReg::T1 => write!(f, "t1"),
            RiscvGpReg::T2 => write!(f, "t2"),
            RiscvGpReg::S0 => write!(f, "s0"),
            RiscvGpReg::S1 => write!(f, "s1"),
            RiscvGpReg::A0 => write!(f, "a0"),
            RiscvGpReg::A1 => write!(f, "a1"),
            RiscvGpReg::A2 => write!(f, "a2"),
            RiscvGpReg::A3 => write!(f, "a3"),
            RiscvGpReg::A4 => write!(f, "a4"),
            RiscvGpReg::A5 => write!(f, "a5"),
            RiscvGpReg::A6 => write!(f, "a6"),
            RiscvGpReg::A7 => write!(f, "a7"),
            RiscvGpReg::S2 => write!(f, "s2"),
            RiscvGpReg::S3 => write!(f, "s3"),
            RiscvGpReg::S4 => write!(f, "s4"),
            RiscvGpReg::S5 => write!(f, "s5"),
            RiscvGpReg::S6 => write!(f, "s6"),
            RiscvGpReg::S7 => write!(f, "s7"),
            RiscvGpReg::S8 => write!(f, "s8"),
            RiscvGpReg::S9 => write!(f, "s9"),
            RiscvGpReg::S10 => write!(f, "s10"),
            RiscvGpReg::S11 => write!(f, "s11"),
            RiscvGpReg::T3 => write!(f, "t3"),
            RiscvGpReg::T4 => write!(f, "t4"),
            RiscvGpReg::T5 => write!(f, "t5"),
            RiscvGpReg::T6 => write!(f, "t6"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(u8)]
pub enum RiscvFpReg {
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

impl From<u8> for RiscvFpReg {
    fn from(value: u8) -> Self {
        match value {
            0 => RiscvFpReg::Ft0,
            1 => RiscvFpReg::Ft1,
            2 => RiscvFpReg::Ft2,
            3 => RiscvFpReg::Ft3,
            4 => RiscvFpReg::Ft4,
            5 => RiscvFpReg::Ft5,
            6 => RiscvFpReg::Ft6,
            7 => RiscvFpReg::Ft7,
            8 => RiscvFpReg::Fs0,
            9 => RiscvFpReg::Fs1,
            10 => RiscvFpReg::Fa0,
            11 => RiscvFpReg::Fa1,
            12 => RiscvFpReg::Fa2,
            13 => RiscvFpReg::Fa3,
            14 => RiscvFpReg::Fa4,
            15 => RiscvFpReg::Fa5,
            16 => RiscvFpReg::Fa6,
            17 => RiscvFpReg::Fa7,
            18 => RiscvFpReg::Fs2,
            19 => RiscvFpReg::Fs3,
            20 => RiscvFpReg::Fs4,
            21 => RiscvFpReg::Fs5,
            22 => RiscvFpReg::Fs6,
            23 => RiscvFpReg::Fs7,
            24 => RiscvFpReg::Fs8,
            25 => RiscvFpReg::Fs9,
            26 => RiscvFpReg::Fs10,
            27 => RiscvFpReg::Fs11,
            28 => RiscvFpReg::Ft8,
            29 => RiscvFpReg::Ft9,
            30 => RiscvFpReg::Ft10,
            31 => RiscvFpReg::Ft11,
            _ => unreachable!(),
        }
    }
}

impl fmt::Display for RiscvFpReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RiscvFpReg::Ft0 => write!(f, "ft0"),
            RiscvFpReg::Ft1 => write!(f, "ft1"),
            RiscvFpReg::Ft2 => write!(f, "ft2"),
            RiscvFpReg::Ft3 => write!(f, "ft3"),
            RiscvFpReg::Ft4 => write!(f, "ft4"),
            RiscvFpReg::Ft5 => write!(f, "ft5"),
            RiscvFpReg::Ft6 => write!(f, "ft6"),
            RiscvFpReg::Ft7 => write!(f, "ft7"),
            RiscvFpReg::Fs0 => write!(f, "fs0"),
            RiscvFpReg::Fs1 => write!(f, "fs1"),
            RiscvFpReg::Fa0 => write!(f, "fa0"),
            RiscvFpReg::Fa1 => write!(f, "fa1"),
            RiscvFpReg::Fa2 => write!(f, "fa2"),
            RiscvFpReg::Fa3 => write!(f, "fa3"),
            RiscvFpReg::Fa4 => write!(f, "fa4"),
            RiscvFpReg::Fa5 => write!(f, "fa5"),
            RiscvFpReg::Fa6 => write!(f, "fa6"),
            RiscvFpReg::Fa7 => write!(f, "fa7"),
            RiscvFpReg::Fs2 => write!(f, "fs2"),
            RiscvFpReg::Fs3 => write!(f, "fs3"),
            RiscvFpReg::Fs4 => write!(f, "fs4"),
            RiscvFpReg::Fs5 => write!(f, "fs5"),
            RiscvFpReg::Fs6 => write!(f, "fs6"),
            RiscvFpReg::Fs7 => write!(f, "fs7"),
            RiscvFpReg::Fs8 => write!(f, "fs8"),
            RiscvFpReg::Fs9 => write!(f, "fs9"),
            RiscvFpReg::Fs10 => write!(f, "fs10"),
            RiscvFpReg::Fs11 => write!(f, "fs11"),
            RiscvFpReg::Ft8 => write!(f, "ft8"),
            RiscvFpReg::Ft9 => write!(f, "ft9"),
            RiscvFpReg::Ft10 => write!(f, "ft10"),
            RiscvFpReg::Ft11 => write!(f, "ft11"),
        }
    }
}

impl fmt::Display for VirtualRegister {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.kind {
            VirtualRegisterKind::General => write!(f, "$v{}", self.id),
            VirtualRegisterKind::FloatingPoint => write!(f, "$vf{}", self.id),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Register {
    General(RiscvGpReg),
    FloatingPoint(RiscvFpReg),
    Virtual(VirtualRegister),
}

impl Register {
    pub fn as_virtual_register(&self) -> Option<VirtualRegister> {
        match self {
            Register::Virtual(vreg) => Some(*vreg),
            _ => None,
        }
    }

    pub fn is_fp_virtual(&self) -> bool {
        match self {
            Register::Virtual(vreg) => matches!(vreg.kind, VirtualRegisterKind::FloatingPoint),
            _ => false,
        }
    }

    pub fn is_gp_virtual(&self) -> bool {
        match self {
            Register::Virtual(vreg) => matches!(vreg.kind, VirtualRegisterKind::General),
            _ => false,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Immediate(pub i128);

impl fmt::Display for Immediate {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.0) }
}

impl From<&Vec<u8>> for Immediate {
    fn from(data: &Vec<u8>) -> Self {
        let mut value = 0;
        for byte in data.iter().rev() {
            value <<= 8;
            value |= *byte as i128;
        }
        Immediate(value)
    }
}

impl From<i128> for Immediate {
    fn from(value: i128) -> Self { Immediate(value) }
}

impl From<u64> for Immediate {
    fn from(value: u64) -> Self { Immediate(value as i128) }
}

impl From<u32> for Immediate {
    fn from(value: u32) -> Self { Immediate(value as i128) }
}

impl From<u16> for Immediate {
    fn from(value: u16) -> Self { Immediate(value as i128) }
}

impl From<u8> for Immediate {
    fn from(value: u8) -> Self { Immediate(value as i128) }
}

impl From<i64> for Immediate {
    fn from(value: i64) -> Self { Immediate(value as i128) }
}

impl From<i32> for Immediate {
    fn from(value: i32) -> Self { Immediate(value as i128) }
}

impl From<i16> for Immediate {
    fn from(value: i16) -> Self { Immediate(value as i128) }
}

impl From<i8> for Immediate {
    fn from(value: i8) -> Self { Immediate(value as i128) }
}

impl From<usize> for Immediate {
    fn from(value: usize) -> Self { Immediate(value as i128) }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MachineSymbol(String);

impl fmt::Display for MachineSymbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}", self.0) }
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

    pub fn align(&self) -> usize { self.align }

    pub fn kind(&self) -> &MachineGlobalKind { &self.kind }

    pub fn set_kind(&mut self, kind: MachineGlobalKind) { self.kind = kind; }
}

impl From<String> for MachineSymbol {
    fn from(name: String) -> Self {
        // TODO: check the name, the global names in IR starts with `@`, the validity
        // needs to be checked.
        MachineSymbol(name)
    }
}

pub enum MachineInstData {
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
        kind: FloatPseudoLoadKind,
        dest: Register,
        symbol: MachineSymbol,
        rt: Register,
    },
    FloatPseudoStore {
        kind: FloatPseudoStoreKind,
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
    FMv {
        dst_fmt: FMvFmt,
        src_fmt: FMvFmt,

        rd: Register,
        rs: Register,
    },
    FCvt {
        dst_fmt: FCvtFmt,
        src_fmt: FCvtFmt,
        rd: Register,
        rs: Register,
    },
    Binary {
        kind: MachineBinaryOp,
        rd: Register,
        rs1: Register,
        rs2: Register,
    },
    BinaryImm {
        kind: MachineBinaryImmOp,
        rd: Register,
        rs1: Register,
        imm: Immediate,
    },
    FloatBinary {
        kind: MachineFloatBinaryOp,
        fmt: MachineFloatBinaryFmt,
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
        kind: MachineFloatUnaryOp,
        fmt: MachineFloatUnaryFmt,
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
        kind: MachineBranchOp,
        rs1: Register,
        rs2: Register,
        block: MachineBlock,
    },
    J {
        block: MachineBlock,
    },
}

impl MachineInstData {
    pub fn new_load(
        ctx: &mut MachineContext,
        kind: LoadKind,
        base: Register,
        offset: Immediate,
    ) -> (Register, MachineInst) {
        let dest = ctx.new_virtual_reg(VirtualRegisterKind::General);
        let data = MachineInstData::Load {
            kind,
            dest,
            base,
            offset,
        };
        (dest, ctx.new_inst(data))
    }

    pub fn new_float_load(
        ctx: &mut MachineContext,
        kind: FloatLoadKind,
        base: Register,
        offset: Immediate,
    ) -> (Register, MachineInst) {
        let dest = ctx.new_virtual_reg(VirtualRegisterKind::FloatingPoint);
        let data = MachineInstData::FloatLoad {
            kind,
            dest,
            base,
            offset,
        };
        (dest, ctx.new_inst(data))
    }

    pub fn new_pseudo_load(
        ctx: &mut MachineContext,
        kind: PseudoLoadKind,
        symbol: MachineSymbol,
    ) -> (Register, MachineInst) {
        let dest = ctx.new_virtual_reg(VirtualRegisterKind::General);
        let data = MachineInstData::PseudoLoad { kind, dest, symbol };
        (dest, ctx.new_inst(data))
    }

    pub fn new_pseudo_store(
        ctx: &mut MachineContext,
        kind: PseudoStoreKind,
        value: Register,
        symbol: MachineSymbol,
        rt: Register,
    ) -> MachineInst {
        let data = MachineInstData::PseudoStore {
            kind,
            value,
            symbol,
            rt,
        };
        ctx.new_inst(data)
    }

    pub fn new_float_pseudo_load(
        ctx: &mut MachineContext,
        kind: FloatPseudoLoadKind,
        symbol: MachineSymbol,
        rt: Register,
    ) -> (Register, MachineInst) {
        let dest = ctx.new_virtual_reg(VirtualRegisterKind::FloatingPoint);
        let data = MachineInstData::FloatPseudoLoad {
            kind,
            dest,
            symbol,
            rt,
        };
        (dest, ctx.new_inst(data))
    }

    pub fn new_float_pseudo_store(
        ctx: &mut MachineContext,
        kind: FloatPseudoStoreKind,
        value: Register,
        symbol: MachineSymbol,
        rt: Register,
    ) -> MachineInst {
        let data = MachineInstData::FloatPseudoStore {
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
        let data = MachineInstData::Store {
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
        let data = MachineInstData::FloatStore {
            kind,
            value,
            base,
            offset,
        };
        ctx.new_inst(data)
    }

    pub fn new_float_move(
        ctx: &mut MachineContext,
        dst_fmt: FMvFmt,
        src_fmt: FMvFmt,
        rs: Register,
    ) -> (Register, MachineInst) {
        let rd = ctx.new_virtual_reg(match dst_fmt {
            FMvFmt::X => VirtualRegisterKind::General,
            FMvFmt::S | FMvFmt::D | FMvFmt::H => VirtualRegisterKind::FloatingPoint,
        });
        let data = MachineInstData::FMv {
            dst_fmt,
            src_fmt,
            rd,
            rs,
        };
        (rd, ctx.new_inst(data))
    }

    pub fn new_float_convert(
        ctx: &mut MachineContext,
        dst_fmt: FCvtFmt,
        src_fmt: FCvtFmt,
        rs: Register,
    ) -> (Register, MachineInst) {
        let rd = ctx.new_virtual_reg(VirtualRegisterKind::FloatingPoint);
        let data = MachineInstData::FCvt {
            dst_fmt,
            src_fmt,
            rd,
            rs,
        };
        (rd, ctx.new_inst(data))
    }

    pub fn new_binary(
        ctx: &mut MachineContext,
        kind: MachineBinaryOp,
        rs1: Register,
        rs2: Register,
    ) -> (Register, MachineInst) {
        let rd = ctx.new_virtual_reg(VirtualRegisterKind::General);
        let data = MachineInstData::Binary { kind, rd, rs1, rs2 };
        (rd, ctx.new_inst(data))
    }

    pub fn new_binary_imm(
        ctx: &mut MachineContext,
        kind: MachineBinaryImmOp,
        rs1: Register,
        imm: Immediate,
    ) -> (Register, MachineInst) {
        let rd = ctx.new_virtual_reg(VirtualRegisterKind::General);
        let data = MachineInstData::BinaryImm { kind, rd, rs1, imm };
        (rd, ctx.new_inst(data))
    }

    pub fn new_float_binary(
        ctx: &mut MachineContext,
        kind: MachineFloatBinaryOp,
        fmt: MachineFloatBinaryFmt,
        rs1: Register,
        rs2: Register,
    ) -> (Register, MachineInst) {
        let rd = ctx.new_virtual_reg(match kind {
            MachineFloatBinaryOp::Feq | MachineFloatBinaryOp::Fle | MachineFloatBinaryOp::Flt => {
                VirtualRegisterKind::General
            }
            _ => VirtualRegisterKind::FloatingPoint,
        });
        let data = MachineInstData::FloatBinary {
            kind,
            fmt,
            rd,
            rs1,
            rs2,
        };
        (rd, ctx.new_inst(data))
    }

    pub fn new_float_mul_add(
        ctx: &mut MachineContext,
        kind: FloatMulAdd,
        fmt: FloatMulAddFmt,
        rs1: Register,
        rs2: Register,
        rs3: Register,
    ) -> MachineInst {
        let rd = ctx.new_virtual_reg(VirtualRegisterKind::FloatingPoint);
        let data = MachineInstData::FloatMulAdd {
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
        kind: MachineFloatUnaryOp,
        fmt: MachineFloatUnaryFmt,
        rs: Register,
    ) -> MachineInst {
        let rd = ctx.new_virtual_reg(VirtualRegisterKind::FloatingPoint);
        let data = MachineInstData::FloatUnary { kind, fmt, rd, rs };
        ctx.new_inst(data)
    }

    pub fn new_li(ctx: &mut MachineContext, imm: Immediate) -> (Register, MachineInst) {
        let rd = ctx.new_virtual_reg(VirtualRegisterKind::General);
        let data = MachineInstData::Li { rd, imm };
        (rd, ctx.new_inst(data))
    }

    pub fn new_ret(ctx: &mut MachineContext) -> MachineInst {
        let data = MachineInstData::Ret;
        ctx.new_inst(data)
    }

    pub fn new_call(ctx: &mut MachineContext, symbol: MachineSymbol) -> MachineInst {
        let data = MachineInstData::Call { symbol };
        ctx.new_inst(data)
    }

    pub fn new_branch(
        ctx: &mut MachineContext,
        kind: MachineBranchOp,
        rs1: Register,
        rs2: Register,
        block: MachineBlock,
    ) -> MachineInst {
        let data = MachineInstData::Branch {
            kind,
            rs1,
            rs2,
            block,
        };
        ctx.new_inst(data)
    }

    pub fn new_j(ctx: &mut MachineContext, block: MachineBlock) -> MachineInst {
        let data = MachineInstData::J { block };
        ctx.new_inst(data)
    }

    pub fn is_load(&self) -> bool {
        matches!(
            self,
            MachineInstData::Load { .. }
                | MachineInstData::FloatLoad { .. }
                | MachineInstData::PseudoLoad { .. }
                | MachineInstData::FloatPseudoLoad { .. }
        )
    }

    pub fn is_store(&self) -> bool {
        matches!(
            self,
            MachineInstData::Store { .. }
                | MachineInstData::FloatStore { .. }
                | MachineInstData::PseudoStore { .. }
                | MachineInstData::FloatPseudoStore { .. }
        )
    }

    pub fn is_branch(&self) -> bool {
        matches!(
            self,
            MachineInstData::Branch { .. } | MachineInstData::J { .. }
        )
    }

    pub fn is_call(&self) -> bool { matches!(self, MachineInstData::Call { .. }) }

    pub fn is_ret(&self) -> bool { matches!(self, MachineInstData::Ret) }

    pub fn is_convert(&self) -> bool { matches!(self, MachineInstData::FCvt { .. }) }

    pub fn is_binary(&self) -> bool {
        matches!(
            self,
            MachineInstData::Binary { .. } | MachineInstData::BinaryImm { .. }
        )
    }

    pub fn is_float_binary(&self) -> bool { matches!(self, MachineInstData::FloatBinary { .. }) }

    /// Build an `addi $rd, $rs, 0`
    ///
    /// This is used for block argument, which requires multiple definition of
    /// the same register, and thus the destination register should be
    /// assigned.
    pub fn build_gp_move(ctx: &mut MachineContext, rd: Register, rs: Register) -> MachineInst {
        let data = MachineInstData::BinaryImm {
            kind: MachineBinaryImmOp::Addi,
            rd,
            rs1: rs,
            imm: Immediate(0),
        };
        ctx.new_inst(data)
    }

    /// Build an `fsgnj.s $rd, $rs, $rs`
    ///
    /// This is used for block argument, which requires multiple definition of
    /// the same register, and thus the destination register should be
    /// assigned.
    pub fn build_fp_move(ctx: &mut MachineContext, rd: Register, rs: Register) -> MachineInst {
        let data = MachineInstData::FloatBinary {
            kind: MachineFloatBinaryOp::Fsgnj,
            fmt: MachineFloatBinaryFmt::S,
            rd,
            rs1: rs,
            rs2: rs,
        };
        ctx.new_inst(data)
    }

    pub fn build_fmv(
        ctx: &mut MachineContext,
        dst_fmt: FMvFmt,
        src_fmt: FMvFmt,
        rd: Register,
        rs: Register,
    ) -> MachineInst {
        let data = MachineInstData::FMv {
            dst_fmt,
            src_fmt,
            rd,
            rs,
        };
        ctx.new_inst(data)
    }

    /// Build an load instruction for function argument (entry block arguments)
    pub fn build_load(
        ctx: &mut MachineContext,
        kind: LoadKind,
        dest: Register,
        base: Register,
        offset: Immediate,
    ) -> MachineInst {
        let data = MachineInstData::Load {
            kind,
            dest,
            base,
            offset,
        };
        ctx.new_inst(data)
    }

    /// Build and float load instruction for function argument (entry block
    /// arguments)
    pub fn build_float_load(
        ctx: &mut MachineContext,
        kind: FloatLoadKind,
        dest: Register,
        base: Register,
        offset: Immediate,
    ) -> MachineInst {
        let data = MachineInstData::FloatLoad {
            kind,
            dest,
            base,
            offset,
        };
        ctx.new_inst(data)
    }

    pub fn build_li(ctx: &mut MachineContext, rd: Register, imm: Immediate) -> MachineInst {
        let data = MachineInstData::Li { rd, imm };
        ctx.new_inst(data)
    }

    pub fn build_pseudo_load(
        ctx: &mut MachineContext,
        kind: PseudoLoadKind,
        dest: Register,
        symbol: MachineSymbol,
    ) -> MachineInst {
        let data = MachineInstData::PseudoLoad { kind, dest, symbol };
        ctx.new_inst(data)
    }

    pub fn build_binary(
        ctx: &mut MachineContext,
        kind: MachineBinaryOp,
        rd: Register,
        rs1: Register,
        rs2: Register,
    ) -> MachineInst {
        let data = MachineInstData::Binary { kind, rd, rs1, rs2 };
        ctx.new_inst(data)
    }

    pub fn build_binary_imm(
        ctx: &mut MachineContext,
        kind: MachineBinaryImmOp,
        rd: Register,
        rs1: Register,
        imm: Immediate,
    ) -> MachineInst {
        let data = MachineInstData::BinaryImm { kind, rd, rs1, imm };
        ctx.new_inst(data)
    }
}

impl fmt::Display for MachineInstData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MachineInstData::Load {
                kind,
                dest,
                base,
                offset,
            } => write!(f, "{} {}, {}({})", kind, dest, offset, base)?,
            MachineInstData::FloatLoad {
                kind,
                dest,
                base,
                offset,
            } => write!(f, "{} {}, {}({})", kind, dest, offset, base)?,
            MachineInstData::PseudoLoad { kind, dest, symbol } => {
                write!(f, "{} {}, {}", kind, dest, symbol)?
            }
            MachineInstData::PseudoStore {
                kind,
                value,
                symbol,
                rt,
            } => write!(f, "{} {}, {}, {}", kind, value, symbol, rt)?,
            MachineInstData::FloatPseudoLoad {
                kind,
                dest,
                symbol,
                rt,
            } => write!(f, "{} {}, {}, {}", kind, dest, symbol, rt)?,
            MachineInstData::FloatPseudoStore {
                kind,
                value,
                symbol,
                rt,
            } => write!(f, "{} {}, {}, {}", kind, value, symbol, rt)?,
            MachineInstData::Store {
                kind,
                value,
                base,
                offset,
            } => write!(f, "{} {}, {}({})", kind, value, offset, base)?,
            MachineInstData::FloatStore {
                kind,
                value,
                base,
                offset,
            } => write!(f, "{} {}, {}({})", kind, value, offset, base)?,
            MachineInstData::FMv {
                dst_fmt,
                src_fmt,
                rd,
                rs,
            } => write!(f, "fmv.{}.{} {}, {}", dst_fmt, src_fmt, rd, rs)?,
            MachineInstData::FCvt {
                dst_fmt,
                src_fmt,
                rd,
                rs,
            } => write!(f, "fcvt.{}.{} {}, {}", dst_fmt, src_fmt, rd, rs)?,
            MachineInstData::Binary { kind, rd, rs1, rs2 } => {
                write!(f, "{} {}, {}, {}", kind, rd, rs1, rs2)?
            }
            MachineInstData::BinaryImm { kind, rd, rs1, imm } => {
                write!(f, "{} {}, {}, {}", kind, rd, rs1, imm)?
            }
            MachineInstData::FloatBinary {
                kind,
                fmt,
                rd,
                rs1,
                rs2,
            } => write!(f, "{}.{} {}, {}, {}", kind, fmt, rd, rs1, rs2)?,
            MachineInstData::FloatMulAdd {
                kind,
                fmt,
                rd,
                rs1,
                rs2,
                rs3,
            } => write!(f, "{}.{} {}, {}, {}, {}", kind, fmt, rd, rs1, rs2, rs3)?,
            MachineInstData::FloatUnary { kind, fmt, rd, rs } => {
                write!(f, "{}.{} {}, {}", kind, fmt, rd, rs)?
            }
            MachineInstData::Li { rd, imm } => write!(f, "li {}, {}", rd, imm)?,
            MachineInstData::Ret => write!(f, "ret")?,
            MachineInstData::Call { symbol } => write!(f, "call {}", symbol)?,
            MachineInstData::Branch {
                kind,
                rs1,
                rs2,
                block,
            } => write!(f, "{} {}, {}, .bb_{}", kind, rs1, rs2, block.0)?,
            MachineInstData::J { block } => write!(f, "j .bb_{}", block.0)?,
        }

        Ok(())
    }
}

pub enum FCvtFmt {
    H,
    S,
    D,
    W,
    Wu,
    L,
    Lu,
}

impl fmt::Display for FCvtFmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FCvtFmt::H => write!(f, "h"),
            FCvtFmt::S => write!(f, "s"),
            FCvtFmt::D => write!(f, "d"),
            FCvtFmt::W => write!(f, "w"),
            FCvtFmt::Wu => write!(f, "wu"),
            FCvtFmt::L => write!(f, "l"),
            FCvtFmt::Lu => write!(f, "lu"),
        }
    }
}

pub enum FMvFmt {
    // half floating point
    H,
    // single floating point
    S,
    // double floating point
    D,
    // general purpose register
    X,
}

impl fmt::Display for FMvFmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FMvFmt::H => write!(f, "h"),
            FMvFmt::S => write!(f, "s"),
            FMvFmt::D => write!(f, "d"),
            FMvFmt::X => write!(f, "x"),
        }
    }
}

impl FMvFmt {
    pub fn from_byte_width(width: usize) -> Self {
        match width {
            2 => FMvFmt::H,
            4 => FMvFmt::S,
            8 => FMvFmt::D,
            _ => unimplemented!(),
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
    /// SD
    DoubleWord,
}

impl fmt::Display for PseudoStoreKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PseudoStoreKind::Word => write!(f, "sw"),
            PseudoStoreKind::DoubleWord => write!(f, "sd"),
        }
    }
}

pub enum FloatPseudoLoadKind {
    /// FLW
    Single,
}

impl fmt::Display for FloatPseudoLoadKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatPseudoLoadKind::Single => write!(f, "flw"),
        }
    }
}

pub enum FloatPseudoStoreKind {
    /// FSW
    Single,
}

impl fmt::Display for FloatPseudoStoreKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FloatPseudoStoreKind::Single => write!(f, "fsw"),
        }
    }
}

pub enum MachineBinaryOp {
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
    Divuw,
    Rem,
    Remw,
    Remu,
    Remuw,
    Rew,
}

impl fmt::Display for MachineBinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MachineBinaryOp::Add => write!(f, "add"),
            MachineBinaryOp::Addw => write!(f, "addw"),
            MachineBinaryOp::Sub => write!(f, "sub"),
            MachineBinaryOp::Subw => write!(f, "subw"),
            MachineBinaryOp::Sll => write!(f, "sll"),
            MachineBinaryOp::Sllw => write!(f, "sllw"),
            MachineBinaryOp::Srl => write!(f, "srl"),
            MachineBinaryOp::Srlw => write!(f, "srlw"),
            MachineBinaryOp::Sra => write!(f, "sra"),
            MachineBinaryOp::Sraw => write!(f, "sraw"),
            MachineBinaryOp::Xor => write!(f, "xor"),
            MachineBinaryOp::Or => write!(f, "or"),
            MachineBinaryOp::And => write!(f, "and"),
            MachineBinaryOp::Slt => write!(f, "slt"),
            MachineBinaryOp::Sltu => write!(f, "sltu"),
            MachineBinaryOp::Mul => write!(f, "mul"),
            MachineBinaryOp::Mulw => write!(f, "mulw"),
            MachineBinaryOp::Mulh => write!(f, "mulh"),
            MachineBinaryOp::Mulhsu => write!(f, "mulhsu"),
            MachineBinaryOp::Mulhu => write!(f, "mulhu"),
            MachineBinaryOp::Div => write!(f, "div"),
            MachineBinaryOp::Divw => write!(f, "divw"),
            MachineBinaryOp::Divu => write!(f, "divu"),
            MachineBinaryOp::Divuw => write!(f, "divuw"),
            MachineBinaryOp::Rem => write!(f, "rem"),
            MachineBinaryOp::Remw => write!(f, "remw"),
            MachineBinaryOp::Remu => write!(f, "remu"),
            MachineBinaryOp::Remuw => write!(f, "remuw"),
            MachineBinaryOp::Rew => write!(f, "rew"),
        }
    }
}

pub enum MachineBranchOp {
    Beq,
    Bne,
    Blt,
    Bge,
    Bltu,
    Bgeu,
}

impl fmt::Display for MachineBranchOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MachineBranchOp::Beq => write!(f, "beq"),
            MachineBranchOp::Bne => write!(f, "bne"),
            MachineBranchOp::Blt => write!(f, "blt"),
            MachineBranchOp::Bge => write!(f, "bge"),
            MachineBranchOp::Bltu => write!(f, "bltu"),
            MachineBranchOp::Bgeu => write!(f, "bgeu"),
        }
    }
}

pub enum MachineBinaryImmOp {
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

impl fmt::Display for MachineBinaryImmOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MachineBinaryImmOp::Addi => write!(f, "addi"),
            MachineBinaryImmOp::Addiw => write!(f, "addiw"),
            MachineBinaryImmOp::Slli => write!(f, "slli"),
            MachineBinaryImmOp::Slliw => write!(f, "slliw"),
            MachineBinaryImmOp::Srli => write!(f, "srli"),
            MachineBinaryImmOp::Srliw => write!(f, "srliw"),
            MachineBinaryImmOp::Srai => write!(f, "srai"),
            MachineBinaryImmOp::Sraiw => write!(f, "sraiw"),
            MachineBinaryImmOp::Xori => write!(f, "xori"),
            MachineBinaryImmOp::Ori => write!(f, "ori"),
            MachineBinaryImmOp::Andi => write!(f, "andi"),
            MachineBinaryImmOp::Slti => write!(f, "slti"),
            MachineBinaryImmOp::Sltiu => write!(f, "sltiu"),
        }
    }
}

pub enum MachineFloatBinaryOp {
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

impl fmt::Display for MachineFloatBinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MachineFloatBinaryOp::Fadd => write!(f, "fadd"),
            MachineFloatBinaryOp::Fsub => write!(f, "fsub"),
            MachineFloatBinaryOp::Fmul => write!(f, "fmul"),
            MachineFloatBinaryOp::Fdiv => write!(f, "fdiv"),
            MachineFloatBinaryOp::Fsgnj => write!(f, "fsgnj"),
            MachineFloatBinaryOp::Fsgnjn => write!(f, "fsgnjn"),
            MachineFloatBinaryOp::Fsgnjx => write!(f, "fsgnjx"),
            MachineFloatBinaryOp::Fmin => write!(f, "fmin"),
            MachineFloatBinaryOp::Fmax => write!(f, "fmax"),
            MachineFloatBinaryOp::Feq => write!(f, "feq"),
            MachineFloatBinaryOp::Flt => write!(f, "flt"),
            MachineFloatBinaryOp::Fle => write!(f, "fle"),
        }
    }
}

pub enum MachineFloatBinaryFmt {
    S,
    D,
}

impl fmt::Display for MachineFloatBinaryFmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MachineFloatBinaryFmt::S => write!(f, "s"),
            MachineFloatBinaryFmt::D => write!(f, "d"),
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

pub enum MachineFloatUnaryOp {
    FSqrt,
    FClass,
}

impl fmt::Display for MachineFloatUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MachineFloatUnaryOp::FSqrt => write!(f, "fsqrt"),
            MachineFloatUnaryOp::FClass => write!(f, "fclass"),
        }
    }
}

pub enum MachineFloatUnaryFmt {
    S,
    D,
}

impl fmt::Display for MachineFloatUnaryFmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MachineFloatUnaryFmt::S => write!(f, "s"),
            MachineFloatUnaryFmt::D => write!(f, "d"),
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

        let v0 = ctx.new_virtual_reg(VirtualRegisterKind::General);

        let (_v1, inst0) = MachineInstData::new_load(&mut ctx, LoadKind::Word, v0, Immediate(0));
        ctx.function_data_mut(&MachineSymbol("main".to_string()))
            .unwrap()
            .layout_mut()
            .append_inst(inst0, bb0)
            .unwrap();

        let (v2, inst1) =
            MachineInstData::new_binary_imm(&mut ctx, MachineBinaryImmOp::Addi, v0, Immediate(1));
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
        let (_v3, inst2) = MachineInstData::new_binary(&mut ctx, MachineBinaryOp::Add, v2, v0);
        ctx.function_data_mut(&MachineSymbol("main".to_string()))
            .unwrap()
            .layout_mut()
            .append_inst(inst2, bb1)
            .unwrap();

        // print the ctx
        println!("{}", ctx);

        assert_eq!(format!("{}", ctx), "\t.option pic\n\t.text\n\t.global main\n\t.align 1\n\t.type main, @function\nmain:\n.bb_0:\n\tlw $v1, 0($v0)\n\taddi $v2, $v0, 1\n.bb_1:\n\tadd $v3, $v2, $v0\n\n");
    }
}
