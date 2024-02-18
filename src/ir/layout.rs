use std::{collections::HashMap, error::Error, fmt};

use crate::collections::{BiLinkedList, BiLinkedListErr, BiLinkedNode};

use super::values::{Block, Inst};

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

/// A layout inside a function.
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
    InstDuplicated(Inst),

    /// Duplicated block in insertion.
    BlockDuplicated(Block),

    /// Parent block cannot be indexed in the layout mapping
    ///
    /// This is usually caused by removing an instruction.
    ParentBlockNotFound(Inst),

    /// Block cannot be found in a local layout (in the linked list).
    BlockNodeNotFound(Block),

    /// Instruction cannot be found in a local layout (in the linked list).
    InstNodeNotFound(Inst),
}

impl fmt::Display for LayoutOpErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LayoutOpErr::InstDuplicated(inst) => write!(f, "duplicated instruction: {:?}", inst),
            LayoutOpErr::BlockDuplicated(block) => write!(f, "duplicated block: {:?}", block),
            LayoutOpErr::ParentBlockNotFound(inst) => {
                write!(f, "parent block not found for instruction: {:?}", inst)
            }
            LayoutOpErr::BlockNodeNotFound(block) => write!(f, "block node not found: {:?}", block),
            LayoutOpErr::InstNodeNotFound(inst) => {
                write!(f, "instruction node not found: {:?}", inst)
            }
        }
    }
}

impl Error for LayoutOpErr {}

impl Layout {
    pub fn new() -> Self {
        Self {
            blocks: BlockList::new(),
            inst_blocks: HashMap::new(),
        }
    }

    pub fn entry_block(&self) -> Option<Block> {
        self.blocks.front()
    }

    pub fn blocks(&self) -> &BlockList {
        &self.blocks
    }

    pub fn inst_blocks(&self) -> &HashMap<Inst, Block> {
        &self.inst_blocks
    }

    pub fn append_block(&mut self, block: Block) -> Result<(), LayoutOpErr> {
        self.blocks
            .append(block)
            .map_err(|_| LayoutOpErr::BlockDuplicated(block))
    }

    pub fn append_inst(&mut self, inst: Inst, block: Block) -> Result<(), LayoutOpErr> {
        if self.inst_blocks.contains_key(&inst) {
            return Err(LayoutOpErr::InstDuplicated(inst));
        }
        self.blocks
            .node_mut(block)
            .ok_or(LayoutOpErr::BlockNodeNotFound(block))?
            .insts_mut()
            .append(inst)
            .map_err(|_| LayoutOpErr::InstDuplicated(inst))
    }

    pub fn remove_block(&mut self, block: Block) -> Result<(), LayoutOpErr> {
        for (inst, _) in self
            .blocks
            .node(block)
            .ok_or(LayoutOpErr::BlockNodeNotFound(block))?
            .insts()
            .iter()
        {
            self.inst_blocks.remove(&inst);
        }

        self.blocks
            .remove(block)
            .map_err(|_| LayoutOpErr::BlockNodeNotFound(block))
    }

    pub fn remove_inst(&mut self, inst: Inst) -> Result<(), LayoutOpErr> {
        let block = self
            .inst_blocks
            .remove(&inst)
            .ok_or(LayoutOpErr::ParentBlockNotFound(inst))?;

        self.blocks
            .node_mut(block)
            .ok_or(LayoutOpErr::BlockNodeNotFound(block))?
            .insts_mut()
            .remove(inst)
            .map_err(|_| LayoutOpErr::InstNodeNotFound(inst))
    }

    pub fn insert_block_before(&mut self, block: Block, before: Block) -> Result<(), LayoutOpErr> {
        self.blocks
            .insert_before(block, before)
            .map_err(|err| match err {
                BiLinkedListErr::KeyDuplicated(block) => LayoutOpErr::BlockDuplicated(block),
                BiLinkedListErr::NodeNotFound(before) => LayoutOpErr::BlockNodeNotFound(before),
            })
    }

    pub fn insert_inst_before(&mut self, inst: Inst, before: Inst) -> Result<(), LayoutOpErr> {
        if self.inst_blocks.contains_key(&inst) {
            return Err(LayoutOpErr::InstDuplicated(inst));
        }

        let block = self
            .inst_blocks
            .get(&before)
            .ok_or(LayoutOpErr::ParentBlockNotFound(before))?;

        self.blocks
            .node_mut(*block)
            .ok_or(LayoutOpErr::BlockNodeNotFound(*block))?
            .insts_mut()
            .insert_before(inst, before)
            .map_err(|err| match err {
                BiLinkedListErr::KeyDuplicated(inst) => LayoutOpErr::InstDuplicated(inst),
                BiLinkedListErr::NodeNotFound(before) => LayoutOpErr::InstNodeNotFound(before),
            })?;

        self.inst_blocks.insert(inst, *block);

        Ok(())
    }
}
