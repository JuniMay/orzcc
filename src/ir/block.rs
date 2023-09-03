use super::value::Value;

/// Reference to the block
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Block(usize);

impl Block {
    pub fn new(id: usize) -> Self {
        Block(id)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

/// A block call with arguments
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BlockCall {
    /// The callee block
    block: Block,
    /// The arguments
    args: Vec<Value>,
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
