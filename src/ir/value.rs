use super::{block::Block, constant::Constant, function::Function, inst::Inst, ty::Type};

/// Value reference
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Value(usize);

impl Value {
    pub fn new(id: usize) -> Self {
        Value(id)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

/// Kind of value
pub enum ValueKind {
    /// Constant
    Constant,
    /// Block parameter
    BlockParam,
    /// Function
    Function,
    /// Block
    Block,
    /// Instruction or the value produced by an instruction.
    Inst,
}

/// Data of a value
pub struct ValueData {
    /// Type of the value
    pub ty: Type,
    /// Kind of the value
    pub kind: ValueKind,
}

impl ValueData {
    pub fn new(ty: Type, kind: ValueKind) -> Self {
        ValueData { ty, kind }
    }
}

impl From<Inst> for Value {
    fn from(inst: Inst) -> Self {
        Value::new(inst.index())
    }
}

impl Into<Inst> for Value {
    fn into(self) -> Inst {
        Inst::new(self.index())
    }
}

impl From<Block> for Value {
    fn from(block: Block) -> Self {
        Value::new(block.index())
    }
}

impl Into<Block> for Value {
    fn into(self) -> Block {
        Block::new(self.index())
    }
}

impl From<Function> for Value {
    fn from(function: Function) -> Self {
        Value::new(function.index())
    }
}

impl Into<Function> for Value {
    fn into(self) -> Function {
        Function::new(self.index())
    }
}

impl From<Constant> for Value {
    fn from(constant: Constant) -> Self {
        Value::new(constant.index())
    }
}

impl Into<Constant> for Value {
    fn into(self) -> Constant {
        Constant::new(self.index())
    }
}
