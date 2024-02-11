use super::types::Type;
use std::fmt;

/// Value reference
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
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

impl<T> From<T> for Value
where
    T: ValueIndexer,
{
    fn from(indexer: T) -> Self {
        Self::new(indexer.index())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ICmpCond {
    Eq,
    Ne,
    Slt,
    Sle,
}

impl fmt::Display for ICmpCond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ICmpCond::Eq => write!(f, "eq"),
            ICmpCond::Ne => write!(f, "ne"),
            ICmpCond::Slt => write!(f, "slt"),
            ICmpCond::Sle => write!(f, "sle"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FCmpCond {
    OEq,
    ONe,
    OLt,
    OLe,
}

impl fmt::Display for FCmpCond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FCmpCond::OEq => write!(f, "oeq"),
            FCmpCond::ONe => write!(f, "one"),
            FCmpCond::OLt => write!(f, "olt"),
            FCmpCond::OLe => write!(f, "ole"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    /// Integer addition
    Add,
    /// Floating point addition
    FAdd,
    /// Integer subtraction
    Sub,
    /// Floating point subtraction
    FSub,
    /// Integer multiplication
    Mul,
    /// Floating point multiplication
    FMul,
    /// Unsigned division
    UDiv,
    /// Signed division
    SDiv,
    /// Floating point division
    FDiv,
    /// Unsigned remainder
    URem,
    /// Signed remainder
    SRem,
    /// Floating point remainder
    FRem,
    /// Bitwise and
    And,
    /// Bitwise or
    Or,
    /// Bitwise xor
    Xor,
    /// Shift left
    Shl,
    /// Logical shift right
    LShr,
    /// Arithmetic shift right
    AShr,
    /// Integer comparison
    ICmp(ICmpCond),
    /// Floating point comparison
    FCmp(FCmpCond),
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "add"),
            BinaryOp::FAdd => write!(f, "fadd"),
            BinaryOp::Sub => write!(f, "sub"),
            BinaryOp::FSub => write!(f, "fsub"),
            BinaryOp::Mul => write!(f, "mul"),
            BinaryOp::FMul => write!(f, "fmul"),
            BinaryOp::UDiv => write!(f, "udiv"),
            BinaryOp::SDiv => write!(f, "sdiv"),
            BinaryOp::FDiv => write!(f, "fdiv"),
            BinaryOp::URem => write!(f, "urem"),
            BinaryOp::SRem => write!(f, "srem"),
            BinaryOp::FRem => write!(f, "frem"),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Or => write!(f, "or"),
            BinaryOp::Xor => write!(f, "xor"),
            BinaryOp::Shl => write!(f, "shl"),
            BinaryOp::LShr => write!(f, "lshr"),
            BinaryOp::AShr => write!(f, "ashr"),
            BinaryOp::ICmp(cond) => write!(f, "icmp.{}", cond),
            BinaryOp::FCmp(cond) => write!(f, "fcmp.{}", cond),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    FNeg,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::FNeg => write!(f, "fneg"),
        }
    }
}

pub struct Binary {
    op: BinaryOp,
    lhs: Value,
    rhs: Value,
}

pub struct Unary {
    op: UnaryOp,
    val: Value,
}

pub struct Store {
    val: Value,
    ptr: Value,
}

pub struct Load {
    ptr: Value,
}

/// Allocation instruction internals.
///
/// Allocation instruction always return a pointer to the allocated value.
/// Therefore, the type of the allocated value is required.
pub struct Alloc {
    /// Type of the allocated value
    ty: Type,
}

/// A global value
pub struct Global {
    /// The initial value of the global
    ///
    /// The initial value should be a constant.
    init: Value,
    /// If or not the global is mutable
    mutable: bool,
}

pub struct Jump {
    dst: Block,
    args: Vec<Value>,
}

pub struct Branch {
    cond: Value,
    then_dst: Block,
    else_dst: Block,
    then_args: Vec<Value>,
    else_args: Vec<Value>,
}

pub struct Return {
    val: Option<Value>,
}

/// A function call.
///
/// The function type can be inferred from the args and the value type.
pub struct Call {
    /// The callee can be a function or a function pointer.
    callee: Value,
    /// Arguments
    args: Vec<Value>,
}

pub struct GetElemPtr {
    /// The pointer
    ptr: Value,
    /// Bound type
    ty: Type,
    /// Indices
    indices: Vec<Value>,
}
