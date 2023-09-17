use std::fmt;

use super::{
    types::Type,
    value::{Block, Constant, Value},
};

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

/// Kind of constants
pub enum ConstantKind {
    /// Zero (initializer)
    Zero,
    /// Undefined
    Undef,
    /// Bytes of non-aggregated
    Bytes(Vec<u8>),
    /// An array
    Array(Vec<Constant>),
    /// A struct
    Struct(Vec<Constant>),
}

/// Data of the constant
pub struct ConstantData {
    /// Type of the constant
    pub ty: Type,
    /// Kind of the constant
    pub kind: ConstantKind,
}

impl ConstantData {
    pub fn mk_zero(ty: Type) -> ConstantData {
        Self {
            ty,
            kind: ConstantKind::Zero,
        }
    }

    pub fn mk_undef(ty: Type) -> ConstantData {
        Self {
            ty,
            kind: ConstantKind::Undef,
        }
    }

    pub fn mk_bytes(ty: Type, bytes: Vec<u8>) -> ConstantData {
        Self {
            ty,
            kind: ConstantKind::Bytes(bytes),
        }
    }

    pub fn mk_array(ty: Type, elems: Vec<Constant>) -> ConstantData {
        Self {
            ty,
            kind: ConstantKind::Array(elems),
        }
    }

    pub fn mk_struct(ty: Type, fields: Vec<Constant>) -> ConstantData {
        Self {
            ty,
            kind: ConstantKind::Struct(fields),
        }
    }
}

pub enum FunctionKind {
    /// Function with definition
    Definition,
    /// Declaration
    Declaration,
    /// Intrinsic
    Intrinsic,
}

/// Data of function.
pub struct FunctionData {
    /// Name of the function.
    pub name: String,
    /// Type of the function.
    pub ty: Type,
    /// The kind of the function
    pub kind: FunctionKind,
}

/// Data of a global value
#[derive(Debug, Clone)]
pub struct GlobalData {
    /// Name of the value
    pub name: String,
    /// Type of the value
    pub ty: Type,
    /// Initializer of the value
    pub init: Constant,
    /// If the value is a constant
    pub mutable: bool,
}

impl GlobalData {
    pub fn new(name: String, ty: Type, init: Constant, mutable: bool) -> GlobalData {
        GlobalData {
            name,
            ty,
            init,
            mutable,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add,
    FAdd,
    Sub,
    FSub,
    Mul,
    FMul,
    UDiv,
    SDiv,
    FDiv,
    URem,
    SRem,
    FRem,
    And,
    Or,
    Xor,
    Shl,
    LShr,
    AShr,
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

/// Instruction data
pub enum InstData {
    /// Allocation of stack slot.
    Alloc { ty: Type },
    /// Load
    Load { ty: Type, addr: Value },
    /// Store
    Store { val: Value, addr: Value },
    /// Binary
    Binary {
        op: BinaryOp,
        lhs: Value,
        rhs: Value,
    },
    /// Integer compare
    ICmp {
        cond: ICmpCond,
        lhs: Value,
        rhs: Value,
    },
    /// Floating-point compare
    FCmp {
        cond: FCmpCond,
        lhs: Value,
        rhs: Value,
    },
    /// Unary
    Unary { op: UnaryOp, val: Value },
    /// Jump
    Jmp { dst: BlockCall },
    /// Conditional branch
    Br {
        cond: Value,
        dst_then: BlockCall,
        dst_else: BlockCall,
    },
    /// Return
    Ret { val: Option<Value> },
    /// Call
    ///
    /// Format: %v0 = call <ret_type> <fn_val> (args...)
    Call {
        /// Function type
        /// Call can also be applied to function pointer, so type is required.
        fn_ty: Type,
        /// Function (ptr)
        fn_val: Value,
        /// Function call arguments
        args: Vec<Value>,
    },
}

impl InstData {
    /// If the instruction is a terminator.
    pub fn is_terminator(&self) -> bool {
        match self {
            InstData::Ret { .. } | InstData::Br { .. } | InstData::Jmp { .. } => true,
            _ => false,
        }
    }
}
