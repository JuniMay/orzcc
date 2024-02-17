//! # Value Reference and Internals
//!
//! There are four references in the IR: [`Value`], [`Inst`], [`Function`] and [`Block`].
//!
//! The [`Value`], [`Inst`], [`Function`] can be converted from one to another, but [`Block`] cannot.
//!
//! In the current implementation, all the references share the same [`IdAllocator`](super::module::IdAllocator).
//!

use super::{
    entities::{ValueData, ValueKind},
    types::Type,
};
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
///
/// Indexer is actually a wrapper of usize, representing a reference to the data.
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

/// Reference to a block
///
/// Blocks are independent from values.
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

impl Block {
    pub fn new(index: usize) -> Self {
        Self(index)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

impl<T> From<T> for Value
where
    T: ValueIndexer,
{
    fn from(indexer: T) -> Self {
        Self::new(indexer.index())
    }
}

/// Condition for integer comparison
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

/// Condition for floating-point comparison
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

/// Binary operations.
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

impl BinaryOp {
    /// If the operation requires integer operands
    pub(super) fn require_int(&self) -> bool {
        match self {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::UDiv
            | BinaryOp::SDiv
            | BinaryOp::URem
            | BinaryOp::SRem
            | BinaryOp::And
            | BinaryOp::Or
            | BinaryOp::Xor
            | BinaryOp::Shl
            | BinaryOp::LShr
            | BinaryOp::AShr
            | BinaryOp::ICmp(_) => true,
            _ => false,
        }
    }

    /// If the operation requires floating-point operands
    pub(super) fn require_float(&self) -> bool {
        match self {
            BinaryOp::FAdd
            | BinaryOp::FSub
            | BinaryOp::FMul
            | BinaryOp::FDiv
            | BinaryOp::FRem
            | BinaryOp::FCmp(_) => true,
            _ => false,
        }
    }

    /// If the operation requires the same type of operands
    pub(super) fn require_same_type(&self) -> bool {
        match self {
            BinaryOp::Shl | BinaryOp::LShr | BinaryOp::AShr => false,
            _ => true,
        }
    }
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

/// Unary operations.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    /// Floating point negation
    FNeg,
}

impl UnaryOp {
    /// If the operation requires integer operands
    pub(super) fn require_int(&self) -> bool {
        false
    }

    /// If the operation requires floating-point operands
    pub(super) fn require_float(&self) -> bool {
        match self {
            UnaryOp::FNeg => true,
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::FNeg => write!(f, "fneg"),
        }
    }
}

/// Binary instruction internals.
pub struct Binary {
    op: BinaryOp,
    lhs: Value,
    rhs: Value,
}

impl Binary {
    pub(super) fn new_value_data(ty: Type, op: BinaryOp, lhs: Value, rhs: Value) -> ValueData {
        ValueData::new(ty, ValueKind::Binary(Binary { op, lhs, rhs }))
    }

    pub fn op(&self) -> BinaryOp {
        self.op.clone()
    }

    pub fn lhs(&self) -> Value {
        self.lhs
    }

    pub fn rhs(&self) -> Value {
        self.rhs
    }
}

/// Unary instruction internals.
pub struct Unary {
    op: UnaryOp,
    val: Value,
}

impl Unary {
    pub(super) fn new_value_data(ty: Type, op: UnaryOp, val: Value) -> ValueData {
        ValueData::new(ty, ValueKind::Unary(Unary { op, val }))
    }

    pub fn op(&self) -> UnaryOp {
        self.op.clone()
    }

    pub fn val(&self) -> Value {
        self.val
    }
}

/// Store instruction internals.
pub struct Store {
    val: Value,
    ptr: Value,
}

impl Store {
    pub(super) fn new_value_data(val: Value, ptr: Value) -> ValueData {
        ValueData::new(Type::mk_void(), ValueKind::Store(Store { val, ptr }))
    }

    pub fn val(&self) -> Value {
        self.val
    }

    pub fn ptr(&self) -> Value {
        self.ptr
    }
}

/// Load instruction internals.
///
/// The type of the loaded value is available in [`ValueData`](super::entities::ValueData).
pub struct Load {
    ptr: Value,
}

impl Load {
    pub(super) fn new_value_data(ty: Type, ptr: Value) -> ValueData {
        ValueData::new(ty, ValueKind::Load(Load { ptr }))
    }

    pub fn ptr(&self) -> Value {
        self.ptr
    }
}

/// Allocation instruction internals.
///
/// Allocation instruction always return a pointer to the allocated value.
/// Therefore, the type of the allocated value is required.
pub struct Alloc {
    /// Type of the allocated value
    ty: Type,
}

impl Alloc {
    pub(super) fn new_value_data(ty: Type) -> ValueData {
        ValueData::new(Type::mk_ptr(), ValueKind::Alloc(Alloc { ty }))
    }

    pub fn ty(&self) -> Type {
        self.ty.clone()
    }
}

/// A global memory slot.
///
/// The initial value of the global should be a constant.
/// The type of the global is available in [`ValueData`](super::entities::ValueData).
pub struct GlobalSlot {
    /// The initial value of the global
    ///
    /// The initial value should be a constant.
    init: Value,
    /// If or not the global is mutable
    mutable: bool,
}

impl GlobalSlot {
    pub(super) fn new_value_data(ty: Type, init: Value, mutable: bool) -> ValueData {
        ValueData::new(ty, ValueKind::GlobalSlot(GlobalSlot { init, mutable }))
    }

    pub fn init(&self) -> Value {
        self.init
    }

    pub fn mutable(&self) -> bool {
        self.mutable
    }
}

/// Jump instruction internals.
pub struct Jump {
    dst: Block,
    args: Vec<Value>,
}

impl Jump {
    pub(super) fn new_value_data(dst: Block, args: Vec<Value>) -> ValueData {
        ValueData::new(Type::mk_void(), ValueKind::Jump(Jump { dst, args }))
    }

    pub fn dst(&self) -> Block {
        self.dst
    }

    pub fn args(&self) -> &[Value] {
        &self.args
    }
}

/// Branch instruction internals.
pub struct Branch {
    cond: Value,
    then_dst: Block,
    else_dst: Block,
    then_args: Vec<Value>,
    else_args: Vec<Value>,
}

impl Branch {
    pub(super) fn new_value_data(
        cond: Value,
        then_dst: Block,
        else_dst: Block,
        then_args: Vec<Value>,
        else_args: Vec<Value>,
    ) -> ValueData {
        ValueData::new(
            Type::mk_void(),
            ValueKind::Branch(Branch {
                cond,
                then_dst,
                else_dst,
                then_args,
                else_args,
            }),
        )
    }

    pub fn cond(&self) -> Value {
        self.cond
    }

    pub fn then_dst(&self) -> Block {
        self.then_dst
    }

    pub fn else_dst(&self) -> Block {
        self.else_dst
    }

    pub fn then_args(&self) -> &[Value] {
        &self.then_args
    }

    pub fn else_args(&self) -> &[Value] {
        &self.else_args
    }
}

/// Return instruction internals.
pub struct Return {
    val: Option<Value>,
}

impl Return {
    pub(super) fn new_value_data(val: Option<Value>) -> ValueData {
        ValueData::new(Type::mk_void(), ValueKind::Return(Return { val }))
    }

    pub fn val(&self) -> Option<Value> {
        self.val
    }
}

/// A function call.
///
/// The function type can be inferred from the args and the [`ValueData`](super::entities::ValueData).
pub struct Call {
    /// The callee can be a function or a function pointer.
    callee: Value,
    /// Arguments
    args: Vec<Value>,
}

impl Call {
    pub(super) fn new_value_data(ret_ty: Type, callee: Value, args: Vec<Value>) -> ValueData {
        ValueData::new(ret_ty, ValueKind::Call(Call { callee, args }))
    }

    pub fn callee(&self) -> Value {
        self.callee
    }

    pub fn args(&self) -> &[Value] {
        &self.args
    }
}

/// Get element pointer instruction internals.
pub struct GetElemPtr {
    /// The pointer
    ptr: Value,
    /// Bound type
    ty: Type,
    /// Indices
    indices: Vec<Value>,
}

impl GetElemPtr {
    pub(super) fn new_value_data(ptr: Value, ty: Type, indices: Vec<Value>) -> ValueData {
        ValueData::new(
            Type::mk_ptr(),
            ValueKind::GetElemPtr(GetElemPtr { ptr, ty, indices }),
        )
    }

    /// Get the pointer
    pub fn ptr(&self) -> Value {
        self.ptr
    }

    /// Get the bound type
    pub fn ty(&self) -> Type {
        self.ty.clone()
    }

    /// Get the indices
    pub fn indices(&self) -> &[Value] {
        &self.indices
    }
}
