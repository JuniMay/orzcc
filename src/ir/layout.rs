use std::collections::HashMap;

use crate::adt::{BiLinkedList, BiLinkedNode};

use super::{block::Block, inst::Inst};

/// Instruction node
pub struct InstNode {
    inst: Inst,
    prev: Option<Inst>,
    next: Option<Inst>,
}

impl BiLinkedNode<Inst> for InstNode {
    fn new(key: Inst) -> Self {
        InstNode {
            inst: key,
            prev: None,
            next: None,
        }
    }

    fn curr(&self) -> &Inst {
        return &self.inst;
    }

    fn next(&self) -> Option<&Inst> {
        return self.next.as_ref();
    }

    fn prev(&self) -> Option<&Inst> {
        return self.prev.as_ref();
    }

    fn set_next(&mut self, next: Option<Inst>) {
        self.next = next;
    }

    fn set_prev(&mut self, prev: Option<Inst>) {
        self.prev = prev;
    }
}

pub type InstList = BiLinkedList<Inst, InstNode>;

/// Block node in the linked list
pub struct BlockNode {
    block: Block,

    prev: Option<Block>,
    next: Option<Block>,

    pub insts: InstList,
}

impl BiLinkedNode<Block> for BlockNode {
    fn new(key: Block) -> Self {
        Self {
            block: key,
            prev: None,
            next: None,
            insts: InstList::new(),
        }
    }

    fn curr(&self) -> &Block {
        &self.block
    }

    fn prev(&self) -> Option<&Block> {
        self.prev.as_ref()
    }

    fn next(&self) -> Option<&Block> {
        self.next.as_ref()
    }

    fn set_prev(&mut self, prev: Option<Block>) {
        self.prev = prev;
    }

    fn set_next(&mut self, next: Option<Block>) {
        self.next = next;
    }
}

pub type BlockList = BiLinkedList<Block, BlockNode>;

/// Layout of instruction and blocks inside a certain function.
pub struct Layout {
    /// Blocks.
    pub blocks: BlockList,
    /// Instruction to block mapping.
    pub inst_blocks: HashMap<Inst, Block>,
}

impl Layout {
    pub fn new() -> Self {
        Self {
            blocks: BlockList::new(),
            inst_blocks: HashMap::new(),
        }
    }
}
