use core::panic;
use std::collections::HashMap;

use crate::collections::{BiLinkedList, BiLinkedListErr, BiLinkedNode};

use super::value::{Block, Function, Global, Inst};

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

/// Block node in the linked list
pub struct BlockNode {
    prev: Option<Block>,
    next: Option<Block>,

    pub insts: InstList,
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

pub type BlockList = BiLinkedList<Block, BlockNode>;

/// Layout of globals, functions, blocks and instructions.
pub struct Layout {
    /// Layout (sequence) of global values.
    pub globals: Vec<Global>,

    /// Layout (sequence) of identified types.
    ///
    /// This is indexed by the name/identifier, the actual type should be fetched from module.
    pub identified_types: Vec<String>,

    /// Layout (sequence) of functions.
    pub functions: Vec<Function>,

    /// Local layouts, indexed by function.
    pub local_layouts: HashMap<Function, BlockList>,

    /// Instruction to block mapping.
    pub inst_blocks: HashMap<Inst, Block>,

    /// Block to function mapping.
    pub block_functions: HashMap<Block, Function>,
}

/// Errors in layout operations
#[derive(Debug)]
pub enum LayoutOpErr {
    /// Duplicated global value in insertion.
    GlobalDuplicate(Global),

    /// Duplicated function value in insertion.
    FunctionDuplicate(Function),

    /// Duplicated identified type in insertion.
    TypeDuplicate(String),

    /// Duplicated instruction in inertion.
    InstDuplicate(Inst),

    /// Duplicated block in insertion.
    BlockDuplicate(Block),

    /// Parent function cannot indexed in the layout mapping
    ///
    /// This is usually caused by removing an block or inserting an instruction.
    ParentFunctionNotFound(Block),

    /// Parent block cannot be indexed in the layout mapping
    ///
    /// This is usually caused by removing an instruction.
    ParentBlockNotFound(Inst),

    /// Block cannot be found in a local layout (in the linked list).
    BlockNodeNotFound(Block),

    /// Instruction cannot be found in a local layout (in the linked list).
    InstNodeNotFound(Inst),

    /// Local layout cannot be found in `local_layouts`
    LocalLayoutNotFound(Function),
}

impl Layout {
    pub fn new() -> Self {
        Self {
            globals: Vec::new(),
            identified_types: Vec::new(),
            functions: Vec::new(),
            local_layouts: HashMap::new(),
            inst_blocks: HashMap::new(),
            block_functions: HashMap::new(),
        }
    }

    /// Reset the layout to be empty.
    ///
    /// Note that the data of functions, instructions, blocks, etc., will remains.
    pub fn clear(&mut self) {
        self.globals.clear();
        self.identified_types.clear();
        self.functions.clear();
        self.local_layouts.clear();
        self.inst_blocks.clear();
        self.block_functions.clear();
    }

    /// Append a global to the layout
    ///
    /// If `global` already exists, return `LayoutOpErr::GlobalDuplicate(gloabl)`.
    pub fn append_global(&mut self, global: Global) -> Result<(), LayoutOpErr> {
        if self.globals.iter().any(|g| g == &global) {
            return Err(LayoutOpErr::GlobalDuplicate(global));
        }
        self.globals.push(global);
        Ok(())
    }

    /// Append an identified type to the layout
    ///
    /// If `name` already exiest, return `LayoutOpErr::TypeDuplicate(name)`.
    pub fn append_identified_type(&mut self, name: String) -> Result<(), LayoutOpErr> {
        if self.identified_types.iter().any(|t| t == &name) {
            return Err(LayoutOpErr::TypeDuplicate(name));
        }
        self.identified_types.push(name);
        Ok(())
    }

    /// Init the local layout of `function`.
    ///
    /// This will replace the existed layout (if any).
    fn init_local_layout(&mut self, function: Function) -> Result<(), LayoutOpErr> {
        self.local_layouts.insert(function, BlockList::new());
        Ok(())
    }

    /// Append function to the layout.
    ///
    /// If `function` exists, return `LayoutOpErr::FunctionDuplicate(function)`.
    ///
    /// This will also (re-)initialize the corresponding local layout.
    pub fn append_function(&mut self, function: Function) -> Result<(), LayoutOpErr> {
        if self.functions.iter().any(|f| f == &function) {
            return Err(LayoutOpErr::FunctionDuplicate(function));
        }
        self.init_local_layout(function)?;
        self.functions.push(function);
        Ok(())
    }

    /// Get the blocks (`BlockList`) by `function` as mutable..
    ///
    /// If `function` is not a key of `local_layouts`,
    /// return `LayoutOpErr::LocalLayoutNotFound(function)`.
    fn get_blocks_mut(&mut self, function: Function) -> Result<&mut BlockList, LayoutOpErr> {
        self.local_layouts
            .get_mut(&function)
            .ok_or(LayoutOpErr::LocalLayoutNotFound(function))
    }

    /// Get the blocks (`BlockList`) by `function` as immutable..
    ///
    /// If `function` is not a key of `local_layouts`,
    /// return `LayoutOpErr::LocalLayoutNotFound(function)`.
    pub fn get_blocks(&self, function: Function) -> Result<&BlockList, LayoutOpErr> {
        self.local_layouts
            .get(&function)
            .ok_or(LayoutOpErr::LocalLayoutNotFound(function))
    }

    /// Append `block` to `function`.
    pub fn append_block(&mut self, block: Block, function: Function) -> Result<(), LayoutOpErr> {
        self.get_blocks_mut(function)?
            .append(block)
            .map_err(|e| match e {
                BiLinkedListErr::KeyDuplicated(_) => LayoutOpErr::BlockDuplicate(block),
                _ => panic!("unexpected error: {:?}", e),
            })?;

        self.block_functions.insert(block, function);

        Ok(())
    }

    /// Insert `block` at `before` in `function`.
    pub fn insert_block_before(
        &mut self,
        block: Block,
        before: Block,
        function: Function,
    ) -> Result<(), LayoutOpErr> {
        self.get_blocks_mut(function)?
            .insert_before(block, before)
            .map_err(|e| match e {
                BiLinkedListErr::KeyDuplicated(_) => LayoutOpErr::BlockDuplicate(block),
                BiLinkedListErr::NodeNotFound(_) => LayoutOpErr::BlockNodeNotFound(before),
            })?;

        self.block_functions.insert(block, function);

        Ok(())
    }

    /// Get the parent function of a block.
    fn get_parent_function(&self, block: Block) -> Result<&Function, LayoutOpErr> {
        self.block_functions
            .get(&block)
            .ok_or(LayoutOpErr::ParentFunctionNotFound(block))
    }

    /// Get the parent block of an instruction.
    fn get_parent_block(&self, inst: Inst) -> Result<&Block, LayoutOpErr> {
        self.inst_blocks
            .get(&inst)
            .ok_or(LayoutOpErr::ParentBlockNotFound(inst))
    }

    /// Remove `block` from the layout
    ///
    /// This does not change the data of instructions and the block itself.
    pub fn remove_block(&mut self, block: Block) -> Result<(), LayoutOpErr> {
        let function = self.get_parent_function(block)?;

        self.get_blocks_mut(*function)?
            .remove(block)
            .map_err(|e| match e {
                BiLinkedListErr::NodeNotFound(_) => LayoutOpErr::BlockNodeNotFound(block),
                _ => panic!("unexpected error: {:?}", e),
            })?;

        self.block_functions.remove(&block);

        Ok(())
    }

    /// Get the insts (`InstList`) by `block` as mutable
    fn get_insts_mut(&mut self, block: Block) -> Result<&mut InstList, LayoutOpErr> {
        let function = self
            .block_functions
            .get(&block)
            .ok_or(LayoutOpErr::ParentFunctionNotFound(block))?;

        let blocks = self
            .local_layouts
            .get_mut(function)
            .ok_or(LayoutOpErr::LocalLayoutNotFound(*function))?;

        Ok(&mut blocks
            .nodes
            .get_mut(&block)
            .ok_or(LayoutOpErr::BlockNodeNotFound(block))?
            .insts)
    }

    /// Get the insts (`InstList`) by `block` as immutable.
    pub fn get_insts(&self, block: Block) -> Result<&InstList, LayoutOpErr> {
        let function = self
            .block_functions
            .get(&block)
            .ok_or(LayoutOpErr::ParentFunctionNotFound(block))?;

        let blocks = self
            .local_layouts
            .get(function)
            .ok_or(LayoutOpErr::LocalLayoutNotFound(*function))?;

        Ok(&blocks
            .nodes
            .get(&block)
            .ok_or(LayoutOpErr::BlockNodeNotFound(block))?
            .insts)
    }

    /// Append `inst` to `block`.
    pub fn append_inst(&mut self, inst: Inst, block: Block) -> Result<(), LayoutOpErr> {
        self.get_insts_mut(block)?
            .append(inst)
            .map_err(|e| match e {
                BiLinkedListErr::KeyDuplicated(_) => LayoutOpErr::InstDuplicate(inst),
                _ => panic!("unexpected error: {:?}", e),
            })?;

        self.inst_blocks.insert(inst, block);

        Ok(())
    }

    /// Insert `inst` at `before` in `block`.
    pub fn insert_inst_before(
        &mut self,
        inst: Inst,
        before: Inst,
        block: Block,
    ) -> Result<(), LayoutOpErr> {
        self.get_insts_mut(block)?
            .insert_before(inst, before)
            .map_err(|e| match e {
                BiLinkedListErr::KeyDuplicated(_) => LayoutOpErr::InstDuplicate(inst),
                BiLinkedListErr::NodeNotFound(_) => LayoutOpErr::InstNodeNotFound(before),
            })?;

        self.inst_blocks.insert(inst, block);

        Ok(())
    }

    /// Remove `inst` from the layout.
    ///
    /// This does not change the data of the instruction.
    pub fn remove_inst(&mut self, inst: Inst) -> Result<(), LayoutOpErr> {
        let block = self.get_parent_block(inst)?;

        self.get_insts_mut(*block)?
            .remove(inst)
            .map_err(|e| match e {
                BiLinkedListErr::NodeNotFound(_) => LayoutOpErr::InstNodeNotFound(inst),
                _ => panic!("unexpected error: {:?}", e),
            })?;

        self.inst_blocks.remove(&inst);

        Ok(())
    }
}
