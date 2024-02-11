use std::{cell::RefCell, collections::HashMap, rc::Weak};

use super::{
    entities::{BlockData, FunctionData, ValueData},
    types::Type,
    values::{Block, Function, Value},
};

pub struct DataFlowGraph {
    values: HashMap<Value, ValueData>,
    blocks: HashMap<Block, BlockData>,
}

pub struct Module {
    /// Module name
    name: String,
    /// Globals
    globals: HashMap<Value, ValueData>,
    /// Layout of globals
    global_layout: Vec<Value>,
    /// Functions
    functions: HashMap<Function, FunctionData>,
    /// Layout of functions
    function_layout: Vec<Function>,
    /// User defined types
    custom_tys: HashMap<String, Type>,
    /// Layout of user defined types
    custom_ty_layout: Vec<String>,
}
