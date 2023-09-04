use crate::ir::value::Value;
use std::fmt::{self};

use super::{block::BlockCall, types::Type};

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

/// Instruction data
pub enum InstData {
    /// Alloca
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
    /// Unary
    Unary { op: UnaryOp, val: Value },
    /// Branch
    Br { dst: BlockCall },
    /// Conditional branch
    CondBr {
        cond: Value,
        dst_then: BlockCall,
        dst_else: BlockCall,
    },
    /// Return
    Ret { val: Option<Value> },
    /// Call
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
