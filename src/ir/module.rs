use std::{
    cell::RefCell,
    collections::HashMap,
    error::Error,
    fmt,
    hash::Hash,
    rc::{Rc, Weak},
};

use crate::collections::BiMap;

use super::{
    builder::{GlobalBuilder, LocalBuilder},
    entities::{BlockData, FunctionData, ValueData},
    types::Type,
    values::{Block, Function, Value},
    GLOBAL_PREFIX, LABEL_PREFIX, LOCAL_PREFIX,
};

/// The data flow graph.
pub struct DataFlowGraph {
    /// Values in the dfg
    values: HashMap<Value, ValueData>,

    /// Blocks in the dfg
    blocks: HashMap<Block, BlockData>,

    /// Local name allocator of values
    value_name_allocator: RefCell<ValueNameAllocator>,

    /// Name allocator of blocks
    block_name_allocator: RefCell<BlockNameAllocator>,

    /// Pointer to global values (including functions)
    globals: Weak<RefCell<GlobalValueMap>>,

    /// Pointer to id allocator
    id_allocator: Weak<RefCell<IdAllocator>>,

    /// Global name allocator
    global_name_allocator: Weak<RefCell<ValueNameAllocator>>,
}

impl Default for DataFlowGraph {
    fn default() -> Self {
        Self::new()
    }
}

impl DataFlowGraph {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            blocks: HashMap::new(),
            value_name_allocator: RefCell::new(NameAllocator::new(LOCAL_PREFIX)),
            block_name_allocator: RefCell::new(NameAllocator::new(LABEL_PREFIX)),
            globals: Weak::new(),
            id_allocator: Weak::new(),
            global_name_allocator: Weak::new(),
        }
    }

    pub fn values(&self) -> &HashMap<Value, ValueData> {
        &self.values
    }

    /// Allocate an id using [`IdAllocator`].
    fn allocate_id(&self) -> usize {
        self.id_allocator
            .upgrade()
            .expect("id allocator should be alive.")
            .borrow_mut()
            .allocate()
    }

    /// Add a value to the dfg.
    pub(super) fn add_value(&mut self, data: ValueData) -> Value {
        let value = Value::new(self.allocate_id());
        self.values.insert(value, data);
        value
    }

    /// Add a block to the dfg.
    pub(super) fn add_block(&mut self, data: BlockData) -> Block {
        let block = Block::new(self.allocate_id());
        self.blocks.insert(block, data);
        block
    }

    /// Get the local builder of the dfg.
    pub fn builder(&mut self) -> LocalBuilder {
        LocalBuilder::new(self)
    }

    /// Get the value data of a local value.
    pub fn local_value_data(&self, value: Value) -> Option<&ValueData> {
        self.values.get(&value)
    }

    /// Apply a function to the value data of a local or global value.
    pub fn with_value_data<F, R>(&self, value: Value, f: F) -> Option<R>
    where
        F: FnOnce(&ValueData) -> R,
    {
        self.globals
            .upgrade()
            .expect("global value map should be alive.")
            .borrow()
            .get(&value)
            .or_else(|| self.values.get(&value))
            .map(f)
    }

    /// Get the name of a local or global value
    pub fn value_name(&self, value: Value) -> String {
        if self.local_value_data(value).is_some() {
            self.value_name_allocator.borrow_mut().get(value)
        } else if self
            .globals
            .upgrade()
            .expect("global value map should be alive.")
            .borrow()
            .contains_key(&value)
        {
            self.global_name_allocator
                .upgrade()
                .expect("global name allocator should be alive.")
                .borrow_mut()
                .get(value)
        } else {
            panic!("value should be either local or global.")
        }
    }

    pub fn get_local_value_by_name(&self, name: &String) -> Option<Value> {
        self.value_name_allocator.borrow().try_get_by_name(name)
    }

    pub fn get_block_by_name(&self, name: &String) -> Option<Block> {
        self.block_name_allocator.borrow().try_get_by_name(name)
    }

    /// Get the name of a block
    pub fn block_name(&self, block: Block) -> String {
        if self.blocks.contains_key(&block) {
            self.block_name_allocator.borrow_mut().get(block)
        } else {
            panic!("block should be in the dfg.")
        }
    }

    pub fn assign_local_value_name(&self, value: Value, name: String) -> Result<(), NameAllocErr> {
        self.value_name_allocator.borrow_mut().assign(value, name)
    }

    pub fn assign_block_name(&self, block: Block, name: String) -> Result<(), NameAllocErr> {
        self.block_name_allocator.borrow_mut().assign(block, name)
    }

    pub fn block_data(&self, block: Block) -> Option<&BlockData> {
        self.blocks.get(&block)
    }
}

pub type GlobalValueMap = HashMap<Value, ValueData>;

pub type IdentifiedTypeMap = HashMap<String, Type>;

pub type ValueNameAllocator = NameAllocator<Value>;

pub type BlockNameAllocator = NameAllocator<Block>;

/// A module.
///
/// Module is the top-level container of the IR. It contains all the global values,
/// functions, and user defined types.
pub struct Module {
    /// Module name
    name: String,

    /// Globals
    ///
    /// This includes global memory slots and functions. Note that the [`ValueData`] and
    /// [`FunctionData`] of a function are different.
    globals: Rc<RefCell<GlobalValueMap>>,

    /// Layout of global slots
    global_slot_layout: Vec<Value>,

    /// Functions
    functions: HashMap<Function, FunctionData>,

    /// Layout of functions
    function_layout: Vec<Function>,

    /// Layout of identified types
    identified_type_layout: Vec<String>,

    /// Id allocator
    id_allocator: Rc<RefCell<IdAllocator>>,

    /// Global name allocator
    name_allocator: Rc<RefCell<ValueNameAllocator>>,
}

impl Module {
    pub fn new(name: String) -> Self {
        let globals = Rc::new(RefCell::new(HashMap::new()));
        let functions = HashMap::new();
        let id_allocator = Rc::new(RefCell::new(IdAllocator::new()));
        let name_allocator = Rc::new(RefCell::new(NameAllocator::new(GLOBAL_PREFIX)));

        Self {
            name,
            globals,
            global_slot_layout: Vec::new(),
            functions,
            function_layout: Vec::new(),
            identified_type_layout: Vec::new(),
            id_allocator,
            name_allocator,
        }
    }

    pub fn with_value_data<F, R>(&self, value: Value, f: F) -> Option<R>
    where
        F: FnOnce(&ValueData) -> R,
    {
        self.globals.borrow().get(&value).map(f)
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn global_slot_layout(&self) -> &[Value] {
        &self.global_slot_layout
    }

    pub fn function_layout(&self) -> &[Function] {
        &self.function_layout
    }

    pub fn identified_type_layout(&self) -> &[String] {
        &self.identified_type_layout
    }

    fn allocate_id(&self) -> usize {
        self.id_allocator.borrow_mut().allocate()
    }

    pub fn builder(&mut self) -> GlobalBuilder {
        GlobalBuilder::new(self)
    }

    pub fn function_data(&self, function: Function) -> Option<&FunctionData> {
        self.functions.get(&function)
    }

    pub fn function_data_mut(&mut self, function: Function) -> Option<&mut FunctionData> {
        self.functions.get_mut(&function)
    }

    pub(super) fn add_global_slot(&mut self, data: ValueData) -> Value {
        let value = Value::new(self.allocate_id());
        self.globals.borrow_mut().insert(value, data);
        self.global_slot_layout.push(value);
        value
    }

    pub(super) fn add_function(
        &mut self,
        value_data: ValueData,
        function_data: FunctionData,
    ) -> Function {
        let function = Value::new(self.allocate_id());
        let function_name = function_data.name().to_string();

        assert!(function_name.starts_with(GLOBAL_PREFIX));
        self.name_allocator
            .borrow_mut()
            .assign(function, function_name)
            .expect("function name should be unique.");

        self.globals.borrow_mut().insert(function, value_data);
        self.functions.insert(function.into(), function_data);

        let weak_globals = Rc::downgrade(&self.globals);
        let weak_id_allocator = Rc::downgrade(&self.id_allocator);
        let weak_name_allocator = Rc::downgrade(&self.name_allocator);

        let dfg = self.function_data_mut(function.into()).unwrap().dfg_mut();

        dfg.globals = weak_globals;
        dfg.id_allocator = weak_id_allocator;
        dfg.global_name_allocator = weak_name_allocator;

        self.function_layout.push(function.into());

        function.into()
    }

    pub fn value_name(&self, value: Value) -> String {
        self.name_allocator.borrow_mut().get(value)
    }

    pub fn assign_name(&mut self, value: Value, name: String) -> Result<(), NameAllocErr> {
        self.name_allocator.borrow_mut().assign(value, name)
    }

    pub fn get_value_by_name(&self, name: &String) -> Option<Value> {
        self.name_allocator.borrow().try_get_by_name(name)
    }

    pub fn add_identified_type(&mut self, name: String) {
        self.identified_type_layout.push(name);
    }
}

/// Allocator of ids.
///
/// This is the allocator of [`Value`], [`Block`], and
/// other entities that need an id to identify them uniquely in the IR.
pub struct IdAllocator {
    counter: usize,
}

impl Default for IdAllocator {
    fn default() -> Self {
        Self::new()
    }
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

/// Manager and allocator of names.
pub struct NameAllocator<T>
where
    T: Hash + Eq + Clone,
{
    counter: usize,

    map: BiMap<T, String>,

    prefix: &'static str,
}

#[derive(Debug)]
pub enum NameAllocErr {
    /// The key is already assigned or allocated.
    KeyDuplicated,
    /// The name is already assigned or allocated.
    NameDuplicated,
}

impl fmt::Display for NameAllocErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::KeyDuplicated => write!(f, "key is already assigned or allocated"),
            Self::NameDuplicated => write!(f, "name is already assigned or allocated"),
        }
    }
}

impl Error for NameAllocErr {}

impl<T> NameAllocator<T>
where
    T: Hash + Eq + Copy,
{
    /// Create a new name allocator.
    ///
    /// The `prefix` is the prefix of the name, e.g. in `%value`, `^block`, `@global`, the prefix
    /// is `%`, `^`, `@` respectively.
    pub fn new(prefix: &'static str) -> Self {
        Self {
            counter: 0,
            map: BiMap::new(),
            prefix,
        }
    }

    /// Allocate a name for the key.
    ///
    /// If the key is already allocated, return [`NameAllocErr`].
    pub fn allocate(&mut self, key: T) -> Result<(), NameAllocErr> {
        if self.map.contains(&key) {
            return Err(NameAllocErr::KeyDuplicated);
        }

        loop {
            let name = format!("{}{}", self.prefix, self.counter);
            if self.map.contains_rev(&name) {
                self.counter += 1;
            } else {
                self.map.insert(key, name);
                self.counter += 1;
                break;
            }
        }

        Ok(())
    }

    /// Manually assign a name for the key.
    pub fn assign(&mut self, key: T, name: String) -> Result<(), NameAllocErr> {
        if self.map.contains_rev(&name) {
            return Err(NameAllocErr::NameDuplicated);
        }

        if self.map.contains(&key) {
            return Err(NameAllocErr::KeyDuplicated);
        }

        let name = if name.starts_with(self.prefix) {
            name
        } else {
            format!("{}{}", self.prefix, name)
        };

        self.map.insert(key, name);

        Ok(())
    }

    /// Get the name of the key.
    ///
    /// If the name is not assigned, allocate a new name.
    pub fn get(&mut self, key: T) -> String {
        let name = self.map.get(&key).cloned().or_else(|| {
            self.allocate(key)
                .expect("allocation should be successful for non-existed key.");
            self.map.get(&key).cloned()
        });

        name.unwrap()
    }

    /// Try to get the name of the key.
    ///
    /// This will not allocate a new name if the name is not assigned.
    pub fn try_get(&self, key: T) -> Option<String> {
        self.map.get(&key).cloned()
    }

    pub fn try_get_by_name(&self, name: &String) -> Option<T> {
        self.map.get_rev(name).copied()
    }
}
