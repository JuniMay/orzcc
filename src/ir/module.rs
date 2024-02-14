use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    hash::Hash,
    rc::{Rc, Weak},
};

use super::{
    builder::{BlockBuilder, CustomTypeBuilder, GlobalValueBuilder, LocalValueBuilder},
    entities::{BlockData, FunctionData, ValueData},
    types::Type,
    values::{Block, Function, Value},
};

pub struct DataFlowGraph {
    /// Values in the dfg
    values: HashMap<Value, ValueData>,

    /// Blocks in the dfg
    blocks: HashMap<Block, BlockData>,

    /// Pointer to global values
    pub(super) globals: Weak<RefCell<GlobalValueMap>>,

    /// Pointer to functions
    pub(super) functions: Weak<RefCell<FunctionMap>>,

    /// Pointer to custom types
    pub(super) custom_tys: Weak<RefCell<CustomTypeMap>>,

    /// Pointer to id allocator
    pub(super) id_allocator: Weak<RefCell<IdAllocator>>,
}

impl DataFlowGraph {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            blocks: HashMap::new(),
            globals: Weak::new(),
            functions: Weak::new(),
            custom_tys: Weak::new(),
            id_allocator: Weak::new(),
        }
    }

    fn allocate_id(&self) -> usize {
        self.id_allocator
            .upgrade()
            .expect("id allocator should be alive.")
            .borrow_mut()
            .allocate()
    }

    pub(super) fn add_value(&mut self, data: ValueData) -> Value {
        let value = Value::new(self.allocate_id());
        self.values.insert(value, data);
        value
    }

    pub(super) fn add_block(&mut self, data: BlockData) -> Block {
        let block = Value::new(self.allocate_id());
        let block = Block::from(block);
        self.blocks.insert(block, data);
        block
    }

    pub fn value_builder(&mut self) -> LocalValueBuilder {
        LocalValueBuilder::new(self)
    }

    pub fn block_builder(&mut self) -> BlockBuilder {
        BlockBuilder::new(self)
    }

    pub(super) fn value_data(&self, value: Value) -> Option<&ValueData> {
        self.values.get(&value)
    }

    pub(super) fn block_data(&self, block: Block) -> Option<&BlockData> {
        self.blocks.get(&block)
    }
}

pub type GlobalValueMap = HashMap<Value, ValueData>;
pub type FunctionMap = HashMap<Function, FunctionData>;
pub type CustomTypeMap = HashMap<String, Type>;

/// A module.
///
/// Module is the top-level container of the IR. It contains all the global values, 
/// functions, and user defined types.
pub struct Module {
    /// Module name
    name: String,

    /// Globals
    globals: Rc<RefCell<GlobalValueMap>>,

    /// Layout of globals
    global_layout: Vec<Value>,

    /// Functions
    functions: Rc<RefCell<FunctionMap>>,

    /// Layout of functions
    function_layout: Vec<Function>,

    /// User defined types
    custom_tys: Rc<RefCell<CustomTypeMap>>,

    /// Layout of user defined types
    custom_ty_layout: Vec<String>,

    /// Id allocator
    id_allocator: Rc<RefCell<IdAllocator>>,
}

impl Module {
    pub fn new(name: String) -> Self {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let functions = Rc::new(RefCell::new(HashMap::new()));
        let custom_tys = Rc::new(RefCell::new(HashMap::new()));
        let id_allocator = Rc::new(RefCell::new(IdAllocator::new()));

        Self {
            name,
            globals,
            global_layout: Vec::new(),
            functions,
            function_layout: Vec::new(),
            custom_tys,
            custom_ty_layout: Vec::new(),
            id_allocator,
        }
    }

    pub fn global_layout(&self) -> &[Value] {
        &self.global_layout
    }

    pub fn global_layout_mut(&mut self) -> &mut Vec<Value> {
        &mut self.global_layout
    }

    pub fn function_layout(&self) -> &[Function] {
        &self.function_layout
    }

    pub fn function_layout_mut(&mut self) -> &mut Vec<Function> {
        &mut self.function_layout
    }

    pub fn custom_ty_layout(&self) -> &[String] {
        &self.custom_ty_layout
    }

    pub fn custom_ty_layout_mut(&mut self) -> &mut Vec<String> {
        &mut self.custom_ty_layout
    }

    fn allocate_id(&self) -> usize {
        self.id_allocator.borrow_mut().allocate()
    }

    pub fn value_builder(&mut self) -> GlobalValueBuilder {
        GlobalValueBuilder::new(self)
    }

    pub fn custom_ty_builder(&mut self) -> CustomTypeBuilder {
        CustomTypeBuilder::new(self)
    }
}

pub struct IdAllocator {
    counter: usize,
}

impl IdAllocator {
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    pub fn allocate(&mut self) -> usize {
        let id = self.counter;
        self.counter += 1;
        id
    }
}

pub struct NameAllocator<T>
where
    T: Hash + Eq,
{
    counter: usize,
    assigned_map: HashMap<T, String>,
    assigned_set: HashSet<String>,
    allocated: HashMap<T, String>,
}

#[derive(Debug)]
pub enum NameAllocErr {
    NameDuplicated,
}

impl<T> NameAllocator<T>
where
    T: Hash + Eq + Copy,
{
    pub fn new() -> Self {
        Self {
            counter: 0,
            assigned_map: HashMap::new(),
            assigned_set: HashSet::new(),
            allocated: HashMap::new(),
        }
    }

    pub fn allocate(&mut self, key: T) -> Result<(), NameAllocErr> {
        if self.assigned_map.contains_key(&key) || self.allocated.contains_key(&key) {
            return Err(NameAllocErr::NameDuplicated);
        }

        self.allocated.insert(key, format!("{}", self.counter));
        self.counter += 1;

        Ok(())
    }

    pub fn assign(&mut self, key: T, name: String) -> Result<(), NameAllocErr> {
        if self.assigned_set.contains(&name)
            || self.assigned_map.contains_key(&key)
            || self.allocated.contains_key(&key)
        {
            return Err(NameAllocErr::NameDuplicated);
        }

        self.assigned_set.insert(name.clone());
        self.assigned_map.insert(key, name);

        Ok(())
    }

    pub fn get(&mut self, key: T) -> String {
        let name = self
            .assigned_map
            .get(&key)
            .or_else(|| self.allocated.get(&key))
            .cloned()
            .or_else(|| {
                self.allocate(key.clone())
                    .expect("allocation should be successful for non-existed key.");
                self.allocated.get(&key).cloned()
            });

        name.unwrap()
    }
}
