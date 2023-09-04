use super::value::{Block, Value};

/// A block call with arguments
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockCall {
    /// The callee block
    pub block: Block,
    /// The arguments
    pub args: Vec<Value>,
}

impl BlockCall {
    pub fn new(block: Block, args: Vec<Value>) -> Self {
        Self { block, args }
    }
}

/// Data of the block
pub struct BlockData {
    /// Params of the block
    pub params: Vec<Value>,
}

impl BlockData {
    pub fn new(params: Vec<Value>) -> Self {
        Self { params }
    }
}
