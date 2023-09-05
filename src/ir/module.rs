use super::entities::{BlockData, ConstantData, FunctionData, GlobalData, InstData};
use super::layout::Layout;
use super::types::Type;
use super::value::{Block, Constant, Function, Global, Inst, Value, ValueData};
use crate::ir::types::TyKind;
use std::collections::{HashMap, HashSet};
use std::hash::Hash;

/// Allocator for names and blocks in IR.
pub struct NameAllocator<T>
where
    T: Hash + Eq,
{
    counter: usize,
    predefined_set: HashSet<String>,
    predefined_map: HashMap<T, String>,
    allocated_map: HashMap<T, String>,
}

#[derive(Debug)]
pub enum NameAllocErr {
    NameDuplicated,
}

impl<T> NameAllocator<T>
where
    T: Hash + Eq + Copy,
{
    pub fn new() -> NameAllocator<T> {
        NameAllocator {
            counter: 0,
            predefined_set: HashSet::new(),
            predefined_map: HashMap::new(),
            allocated_map: HashMap::new(),
        }
    }

    /// Allocate a new name or return the existed one.
    pub fn allocate(&mut self, key: T) -> String {
        if self.predefined_map.contains_key(&key) {
            self.predefined_map.get(&key).unwrap().clone()
        } else if self.allocated_map.contains_key(&key) {
            self.allocated_map.get(&key).unwrap().clone()
        } else {
            let mut name = format!("{}", self.counter);
            while self.predefined_set.contains(&name) {
                self.counter += 1;
                name = format!("{}", self.counter);
            }
            self.allocated_map.insert(key, name.clone());
            self.counter += 1;
            name
        }
    }

    /// Manually assign a name.
    pub fn assign(&mut self, key: T, name: String) -> Result<(), NameAllocErr> {
        if self.predefined_set.contains(&name)
            || self.predefined_map.contains_key(&key)
            || self.predefined_map.contains_key(&key)
        {
            Err(NameAllocErr::NameDuplicated)
        } else {
            self.predefined_set.insert(name.clone());
            self.predefined_map.insert(key, name.clone());
            Ok(())
        }
    }

    /// Clear all auto-allocated names.
    pub fn clear(&mut self) {
        self.allocated_map.clear();
        self.counter = 0;
    }

    pub fn get_name(&self, key: T) -> Option<String> {
        self.predefined_map
            .get(&key)
            .cloned()
            .or_else(|| self.allocated_map.get(&key).cloned())
    }
}

/// Allocator for id of values.
pub struct IdAllocator {
    counter: usize,
    free: Vec<usize>,
}

impl IdAllocator {
    pub fn new() -> IdAllocator {
        IdAllocator {
            counter: 0,
            free: Vec::new(),
        }
    }

    /// Allocate a new id
    pub fn allocate(&mut self) -> usize {
        // first check the free
        if self.free.len() > 0 {
            self.free.pop().unwrap()
        } else {
            self.counter += 1;
            self.counter - 1
        }
    }

    /// Make a certain id free.
    pub fn free(&mut self, id: usize) {
        self.free.push(id);
    }
}

/// Commenter for instructions
///
/// This is used to put useful information after the instruction when dumping the IR.
pub struct InstCommenter {
    pub comments: HashMap<Inst, String>,
}

impl InstCommenter {
    pub fn new() -> InstCommenter {
        InstCommenter {
            comments: HashMap::new(),
        }
    }

    pub fn comment(&mut self, inst: Inst, comment: String) {
        self.comments.insert(inst, comment);
    }
}

/// Module of IR.
///
/// Module contains the entity information of each references.
/// The information in a module can be used to (re-)initialize a builder
///
/// A module does not represent any special structure by itself though
/// implicitly the data-flow-graph is stored. The sequence of instructions
/// and blocks are stored inside the corresponding function. Special
/// graph or other representations should be introduced by independent
/// passes and even the implicit dfg should not be used directly from
/// the module.
///
/// Though being SSA form, the module does not store explicit the def-use chains.
/// The use-def chain can be queried from instruction by the `value` because of SSA.
///
/// If execution is needed, just use the sequential representation in the
/// module (control-flow relation might needs to be analyzed during execution).
///
/// All the indexer (Value, Inst, Block, etc) can be converted to
/// represent a value. and the actual data is stored in the corresponding maps.
///
/// Value name assignment will be done when dumping the ir.
pub struct Module {
    /// The value and corresponding data in the module.
    pub values: HashMap<Value, ValueData>,

    /// The instructions
    pub insts: HashMap<Inst, InstData>,

    /// The blocks
    pub blocks: HashMap<Block, BlockData>,

    /// The constants
    pub constants: HashMap<Constant, ConstantData>,

    /// The globals
    pub globals: HashMap<Global, GlobalData>,

    /// The functions
    pub functions: HashMap<Function, FunctionData>,

    /// Identified types
    pub identified_types: HashMap<String, Type>,

    /// The id allocator
    pub(super) id_allocator: IdAllocator,

    /// The name allocator for values
    pub(super) value_name_allocator: NameAllocator<Value>,

    /// The name allocator for blocks
    pub(super) block_name_allocator: NameAllocator<Block>,

    /// Instruction commenter
    pub(super) inst_commenter: InstCommenter,
}

impl Module {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            insts: HashMap::new(),
            blocks: HashMap::new(),
            constants: HashMap::new(),
            globals: HashMap::new(),
            functions: HashMap::new(),
            identified_types: HashMap::new(),
            id_allocator: IdAllocator::new(),
            value_name_allocator: NameAllocator::new(),
            block_name_allocator: NameAllocator::new(),
            inst_commenter: InstCommenter::new(),
        }
    }

    /// Allocate index as a value
    pub fn allocate_id(&mut self) -> Value {
        Value::new(self.id_allocator.allocate())
    }

    /// Assign a custom name to value.
    ///
    /// This can be used in optimization passes.
    pub fn assign_value_name(&mut self, value: Value, name: String) -> Result<(), NameAllocErr> {
        self.value_name_allocator.assign(value, name)
    }

    /// Assign a custom name to block.
    ///
    /// This can be used in some control-flow-related optimizations.
    pub fn assign_block_name(&mut self, block: Block, name: String) -> Result<(), NameAllocErr> {
        self.block_name_allocator.assign(block, name)
    }

    /// Comment at instruction.
    ///
    /// This can be used to put some debug/necessary information to IR when dumping.
    pub fn comment_at_inst(&mut self, inst: Inst, comment: String) {
        self.inst_commenter.comment(inst, comment)
    }

    /// Allocate names for block, params and instruction results.
    pub fn allocate_name(&mut self, layout: &Layout) {
        for (_function, blocks) in &layout.local_layouts {
            for (block, block_node) in blocks.iter() {
                self.block_name_allocator.allocate(block);
                let block_data = self.blocks.get(&block).unwrap();

                for param in &block_data.params {
                    self.value_name_allocator.allocate(*param);
                }

                for (inst, _inst_node) in block_node.insts.iter() {
                    if let TyKind::Void = self.values.get(&inst.into()).unwrap().ty.kind() {
                        // Skip assignment for void-typed value.
                        continue;
                    }
                    self.value_name_allocator.allocate(inst.into());
                }
            }
        }
    }
}
