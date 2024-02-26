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
                write!(f, "parent block not found for: {:?}", inst)
            }
            LayoutOpErr::BlockNodeNotFound(block) => write!(f, "block node not found: {:?}", block),
            LayoutOpErr::InstNodeNotFound(inst) => {
                write!(f, "instruction node not found for: {:?}", inst)
            }
        }
    }
}

impl Error for LayoutOpErr {}

impl Default for Layout {
    fn default() -> Self {
        Self::new()
    }
}

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

    /// Get the parent block of an instruction
    pub fn parent_block(&self, inst: Inst) -> Option<Block> {
        self.inst_blocks.get(&inst).copied()
    }

    /// Get the next block
    pub fn next_block(&self, block: Block) -> Option<Block> {
        self.blocks.node(block)?.next()
    }

    /// Get the next non-empty block
    pub fn next_non_empty_block(&self, block: Block) -> Option<Block> {
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
    /// If this is the end of the block, return the first instruction of the next block.
    /// Note that this is not the execution order, but the layout order.
    pub fn next_inst(&self, inst: Inst) -> Option<Inst> {
        let parent_block = self.parent_block(inst)?;
        let node = self.blocks.node(parent_block).unwrap();
        let next_inst = node.insts().node(inst)?.next().or_else(|| {
            // this is the end of the block
            self.next_non_empty_block(parent_block)
                .and_then(|next_block| self.blocks.node(next_block).unwrap().insts().front())
        });
        next_inst
    }

    /// Get the entry instruction in the layout
    pub fn entry_inst(&self) -> Option<Inst> {
        self.entry_block()
            .and_then(|entry_block| self.blocks.node(entry_block).unwrap().insts().front())
    }

    /// Get the entry instruction of a block
    pub fn entry_inst_of_block(&self, block: Block) -> Option<Inst> {
        self.blocks
            .node(block)
            .and_then(|node| node.insts().front())
    }

    /// Get the exit/last instruction of a block
    pub fn exit_inst_of_block(&self, block: Block) -> Option<Inst> {
        self.blocks
            .node(block)
            .and_then(|node| node.insts().back())
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
            .map_err(|_| LayoutOpErr::InstDuplicated(inst))?;

        self.inst_blocks.insert(inst, block);

        Ok(())
    }

    pub fn remove_block(&mut self, block: Block) -> Result<(), LayoutOpErr> {
        for (inst, _) in self
            .blocks
            .node(block)
            .ok_or(LayoutOpErr::BlockNodeNotFound(block))?
            .insts()
            .into_iter()
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
                // this is actually unreachable
                BiLinkedListErr::NodeNotFound(before) => LayoutOpErr::InstNodeNotFound(before),
            })?;

        self.inst_blocks.insert(inst, *block);

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::ir::{
        module::IdAllocator,
        values::{Block, Inst, Value},
    };
    use std::cell::RefCell;

    thread_local! {
        static ID_ALLOCATOR: RefCell<IdAllocator> = RefCell::new(IdAllocator::new());
    }

    fn allocate_test_inst() -> Inst {
        let id = ID_ALLOCATOR.with(|id_allocator| id_allocator.borrow_mut().allocate());
        Value::new(id).into()
    }

    fn allocate_test_block() -> Block {
        let id = ID_ALLOCATOR.with(|id_allocator| id_allocator.borrow_mut().allocate());
        Block::new(id)
    }

    #[test]
    fn test_append_block() {
        let mut layout = super::Layout::new();
        let block0 = allocate_test_block();
        let block1 = allocate_test_block();
        let block2 = allocate_test_block();

        layout.append_block(block0).unwrap();
        layout.append_block(block1).unwrap();
        layout.append_block(block2).unwrap();

        assert_eq!(layout.blocks().front(), Some(block0));
        assert_eq!(layout.blocks().back(), Some(block2));
        assert_eq!(layout.entry_block(), Some(block0));

        layout.remove_block(block0).unwrap();
        assert_eq!(layout.blocks().front(), Some(block1));
        assert_eq!(layout.blocks().back(), Some(block2));
        assert_eq!(layout.entry_block(), Some(block1));

        layout.remove_block(block1).unwrap();
        assert_eq!(layout.blocks().front(), Some(block2));
        assert_eq!(layout.blocks().back(), Some(block2));
        assert_eq!(layout.entry_block(), Some(block2));

        layout.remove_block(block2).unwrap();
        assert_eq!(layout.blocks().front(), None);
        assert_eq!(layout.blocks().back(), None);
        assert_eq!(layout.entry_block(), None);
    }

    #[test]
    fn test_insert_block_before() {
        let mut layout = super::Layout::new();
        let block0 = allocate_test_block();
        let block1 = allocate_test_block();
        let block2 = allocate_test_block();
        let block3 = allocate_test_block();

        // 0 -> 1 -> 2
        layout.append_block(block0).unwrap();
        layout.append_block(block1).unwrap();
        layout.append_block(block2).unwrap();

        // 0 -> 1 -> 3 -> 2
        layout.insert_block_before(block3, block2).unwrap();

        assert_eq!(layout.blocks().front(), Some(block0));
        assert_eq!(layout.blocks().back(), Some(block2));
        assert_eq!(layout.entry_block(), Some(block0));

        assert_eq!(layout.blocks().node(block0).unwrap().next, Some(block1));
        assert_eq!(layout.blocks().node(block1).unwrap().next, Some(block3));
        assert_eq!(layout.blocks().node(block3).unwrap().next, Some(block2));
        assert_eq!(layout.blocks().node(block2).unwrap().next, None);

        assert_eq!(layout.blocks().node(block0).unwrap().prev, None);
        assert_eq!(layout.blocks().node(block1).unwrap().prev, Some(block0));
        assert_eq!(layout.blocks().node(block3).unwrap().prev, Some(block1));
        assert_eq!(layout.blocks().node(block2).unwrap().prev, Some(block3));
    }

    #[test]
    fn test_append_inst() {
        let mut layout = super::Layout::new();
        let block = allocate_test_block();
        let inst0 = allocate_test_inst();
        let inst1 = allocate_test_inst();
        let inst2 = allocate_test_inst();

        layout.append_block(block).unwrap();
        layout.append_inst(inst0, block).unwrap();
        layout.append_inst(inst1, block).unwrap();
        layout.append_inst(inst2, block).unwrap();

        assert_eq!(
            layout.blocks().node(block).unwrap().insts().front(),
            Some(inst0)
        );
        assert_eq!(
            layout.blocks().node(block).unwrap().insts().back(),
            Some(inst2)
        );

        assert_eq!(layout.parent_block(inst0), Some(block));
        assert_eq!(layout.parent_block(inst1), Some(block));
        assert_eq!(layout.parent_block(inst2), Some(block));

        assert_eq!(
            layout
                .blocks()
                .node(block)
                .unwrap()
                .insts()
                .node(inst0)
                .unwrap()
                .next,
            Some(inst1)
        );
        assert_eq!(
            layout
                .blocks()
                .node(block)
                .unwrap()
                .insts()
                .node(inst1)
                .unwrap()
                .next,
            Some(inst2)
        );
        assert_eq!(
            layout
                .blocks()
                .node(block)
                .unwrap()
                .insts()
                .node(inst2)
                .unwrap()
                .next,
            None
        );

        layout.remove_inst(inst1).unwrap();
        assert_eq!(
            layout.blocks().node(block).unwrap().insts().front(),
            Some(inst0)
        );
        assert_eq!(
            layout.blocks().node(block).unwrap().insts().back(),
            Some(inst2)
        );

        assert_eq!(layout.parent_block(inst0), Some(block));
        assert_eq!(layout.parent_block(inst1), None);
        assert_eq!(layout.parent_block(inst2), Some(block));

        layout.remove_inst(inst0).unwrap();
        assert_eq!(
            layout.blocks().node(block).unwrap().insts().front(),
            Some(inst2)
        );
        assert_eq!(
            layout.blocks().node(block).unwrap().insts().back(),
            Some(inst2)
        );

        assert_eq!(layout.parent_block(inst0), None);
        assert_eq!(layout.parent_block(inst1), None);
        assert_eq!(layout.parent_block(inst2), Some(block));

        layout.remove_inst(inst2).unwrap();
        assert_eq!(layout.blocks().node(block).unwrap().insts().front(), None);
    }

    #[test]
    fn test_insert_inst_before() {
        let mut layout = super::Layout::new();
        let block = allocate_test_block();
        let inst0 = allocate_test_inst();
        let inst1 = allocate_test_inst();
        let inst2 = allocate_test_inst();
        let inst3 = allocate_test_inst();

        layout.append_block(block).unwrap();

        // 0 -> 1 -> 2
        layout.append_inst(inst0, block).unwrap();
        layout.append_inst(inst1, block).unwrap();
        layout.append_inst(inst2, block).unwrap();

        // 0 -> 1 -> 3 -> 2
        layout.insert_inst_before(inst3, inst2).unwrap();

        assert_eq!(
            layout.blocks().node(block).unwrap().insts().front(),
            Some(inst0)
        );
        assert_eq!(
            layout.blocks().node(block).unwrap().insts().back(),
            Some(inst2)
        );

        assert_eq!(layout.parent_block(inst0), Some(block));
        assert_eq!(layout.parent_block(inst1), Some(block));
        assert_eq!(layout.parent_block(inst2), Some(block));
        assert_eq!(layout.parent_block(inst3), Some(block));

        assert_eq!(
            layout
                .blocks()
                .node(block)
                .unwrap()
                .insts()
                .node(inst0)
                .unwrap()
                .next,
            Some(inst1)
        );
        assert_eq!(
            layout
                .blocks()
                .node(block)
                .unwrap()
                .insts()
                .node(inst1)
                .unwrap()
                .next,
            Some(inst3)
        );
        assert_eq!(
            layout
                .blocks()
                .node(block)
                .unwrap()
                .insts()
                .node(inst3)
                .unwrap()
                .next,
            Some(inst2)
        );
        assert_eq!(
            layout
                .blocks()
                .node(block)
                .unwrap()
                .insts()
                .node(inst2)
                .unwrap()
                .next,
            None
        );
    }

    #[test]
    fn test_errors() {
        let mut layout = super::Layout::new();
        let block0 = allocate_test_block();
        let block1 = allocate_test_block();
        let block2 = allocate_test_block();
        let block3 = allocate_test_block();
        let inst0 = allocate_test_inst();
        let inst1 = allocate_test_inst();
        let inst2 = allocate_test_inst();
        let inst3 = allocate_test_inst();
        let inst4 = allocate_test_inst();
        let inst5 = allocate_test_inst();

        layout.append_block(block0).unwrap();
        layout.append_block(block1).unwrap();
        layout.append_block(block2).unwrap();
        layout.append_block(block3).unwrap();

        layout.append_inst(inst0, block0).unwrap();
        layout.append_inst(inst1, block1).unwrap();
        layout.append_inst(inst2, block2).unwrap();
        layout.append_inst(inst3, block3).unwrap();
        layout.append_inst(inst4, block3).unwrap();
        layout.append_inst(inst5, block3).unwrap();

        assert_eq!(layout.entry_inst_of_block(block0), Some(inst0));
        assert_eq!(layout.entry_inst_of_block(block1), Some(inst1));
        assert_eq!(layout.entry_inst_of_block(block2), Some(inst2));
        assert_eq!(layout.entry_inst_of_block(block3), Some(inst3));

        assert_eq!(layout.exit_inst_of_block(block0), Some(inst0));
        assert_eq!(layout.exit_inst_of_block(block1), Some(inst1));
        assert_eq!(layout.exit_inst_of_block(block2), Some(inst2));
        assert_eq!(layout.exit_inst_of_block(block3), Some(inst5));

        assert_eq!(
            format!("{}", layout.append_block(block0).unwrap_err()),
            format!("duplicated block: {:?}", block0)
        );

        assert_eq!(
            format!("{}", layout.append_inst(inst0, block0).unwrap_err()),
            format!("duplicated instruction: {:?}", inst0)
        );

        let new_inst0 = allocate_test_inst();
        let new_inst1 = allocate_test_inst();
        let new_block0 = allocate_test_block();
        let new_block1 = allocate_test_block();

        assert_eq!(
            format!("{}", layout.remove_block(new_block0).unwrap_err()),
            format!("block node not found: {:?}", new_block0)
        );

        assert_eq!(
            format!("{}", layout.remove_inst(new_inst0).unwrap_err()),
            format!("parent block not found for: {:?}", new_inst0)
        );

        assert_eq!(
            format!(
                "{}",
                layout.insert_block_before(block0, new_block0).unwrap_err()
            ),
            format!("duplicated block: {:?}", block0)
        );

        assert_eq!(
            format!(
                "{}",
                layout
                    .insert_block_before(new_block1, new_block0)
                    .unwrap_err()
            ),
            format!("block node not found: {:?}", new_block0)
        );

        assert_eq!(
            format!(
                "{}",
                layout.insert_inst_before(inst0, new_inst0).unwrap_err()
            ),
            format!("duplicated instruction: {:?}", inst0)
        );
        assert_eq!(
            format!(
                "{}",
                layout.insert_inst_before(new_inst1, new_inst0).unwrap_err()
            ),
            format!("parent block not found for: {:?}", new_inst0)
        );

        layout.remove_block(block0).unwrap();

        assert_eq!(
            format!("{}", layout.remove_inst(inst0).unwrap_err()),
            format!("parent block not found for: {:?}", inst0)
        );
    }
}
