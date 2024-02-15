use std::collections::HashMap;

use crate::collections::{BiLinkedList, BiLinkedNode};

use super::values::{Block, Function, Inst, Value};

/// Instruction node
pub struct InstNode {
    prev: Option<Inst>,
    next: Option<Inst>,
}

impl BiLinkedNode<Inst> for InstNode {
    fn new() -> Self {
        InstNode {
            prev: None,
            next: None,
        }
    }

    fn next(&self) -> Option<Inst> {
        self.next
    }

    fn prev(&self) -> Option<Inst> {
        self.prev
    }

    fn set_next(&mut self, next: Option<Inst>) {
        self.next = next;
    }

    fn set_prev(&mut self, prev: Option<Inst>) {
        self.prev = prev;
    }
}

pub type InstList = BiLinkedList<Inst, InstNode>;

/// Basic block node
pub struct BlockNode {
    prev: Option<Block>,
    next: Option<Block>,

    insts: InstList,
}

impl BiLinkedNode<Block> for BlockNode {
    fn new() -> Self {
        Self {
            prev: None,
            next: None,
            insts: InstList::new(),
        }
    }

    fn prev(&self) -> Option<Block> {
        self.prev
    }

    fn next(&self) -> Option<Block> {
        self.next
    }

    fn set_prev(&mut self, prev: Option<Block>) {
        self.prev = prev;
    }

    fn set_next(&mut self, next: Option<Block>) {
        self.next = next;
    }
}

impl BlockNode {
    pub fn insts(&self) -> &InstList {
        &self.insts
    }

    pub fn insts_mut(&mut self) -> &mut InstList {
        &mut self.insts
    }
}

pub type BlockList = BiLinkedList<Block, BlockNode>;

/// A layout of a function.
pub struct Layout {
    /// The basic blocks
    blocks: BlockList,

    /// Instruction to block mapping
    inst_blocks: HashMap<Inst, Block>,
}

/// Errors in layout operations
#[derive(Debug)]
pub enum LayoutOpErr {
    /// Duplicated instruction in inertion.
    InstDuplicate(Inst),

    /// Duplicated block in insertion.
    BlockDuplicate(Block),

    /// Parent block cannot be indexed in the layout mapping
    ///
    /// This is usually caused by removing an instruction.
    ParentBlockNotFound(Inst),

    /// Block cannot be found in a local layout (in the linked list).
    BlockNodeNotFound(Block),

    /// Instruction cannot be found in a local layout (in the linked list).
    InstNodeNotFound(Inst),
}

impl Layout {
    pub fn new() -> Self {
        Self {
            blocks: BlockList::new(),
            inst_blocks: HashMap::new(),
        }
    }

    pub fn blocks(&self) -> &BlockList {
        &self.blocks
    }

    pub fn blocks_mut(&mut self) -> &mut BlockList {
        &mut self.blocks
    }

    pub fn inst_blocks(&self) -> &HashMap<Inst, Block> {
        &self.inst_blocks
    }

    pub fn inst_blocks_mut(&mut self) -> &mut HashMap<Inst, Block> {
        &mut self.inst_blocks
    }
}
