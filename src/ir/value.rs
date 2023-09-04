use super::types::Type;

/// Value reference
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Value(usize);

impl Value {
    pub fn new(index: usize) -> Self {
        Self(index)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

/// Value indexer indicates an indexer for a value and can be converted from/into a Value.
trait ValueIndexer: From<Value> {
    fn new(index: usize) -> Self;
    fn index(&self) -> usize;
}

/// Reference to an instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Inst(usize);

/// Reference to a function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Function(usize);

/// Reference to the block
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Block(usize);

/// Reference to a global value
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Global(usize);

/// Reference to a constant.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Constant(usize);

/// Implement the value indexer trait for given indexer.
macro_rules! impl_value_indexer {
    ($indexer:ident) => {
        impl ValueIndexer for $indexer {
            fn new(id: usize) -> Self {
                Self(id)
            }

            fn index(&self) -> usize {
                self.0
            }
        }

        impl From<Value> for $indexer {
            fn from(value: Value) -> Self {
                Self::new(value.index())
            }
        }
    };
}

impl_value_indexer!(Inst);

impl_value_indexer!(Function);

impl_value_indexer!(Block);

impl_value_indexer!(Global);

impl_value_indexer!(Constant);

impl<T> From<T> for Value
where
    T: ValueIndexer,
{
    fn from(indexer: T) -> Self {
        Self::new(indexer.index())
    }
}

/// Kinds of value.
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
    /// A global value
    Global,
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
