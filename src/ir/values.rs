//! # Value Reference and Internals
//!
//! There are four references in the IR: [`Value`], [`Inst`], [`Function`] and
//! [`Block`].
//!
//! The [`Value`], [`Inst`], [`Function`] can be converted from one to another,
//! but [`Block`] cannot.
//!
//! In the current implementation, all the references share the same
//! [`IdAllocator`](super::module::IdAllocator).

use super::{
    entities::{ValueData, ValueKind},
    types::Type,
};

/// Value reference
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Value(usize);

impl Value {
    pub fn new(index: usize) -> Self { Self(index) }

    pub fn index(&self) -> usize { self.0 }
}

/// Value indexer indicates an indexer for a value and can be converted
/// from/into a Value.
///
/// Indexer is actually a wrapper of usize, representing a reference to the
/// data.
pub trait ValueIndexer: From<Value> {
    fn new(index: usize) -> Self;
    fn index(&self) -> usize;
}

/// Reference to an instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Inst(usize);

/// Reference to a function.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Function(usize);

/// Reference to a block
///
/// Blocks are independent from values.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[repr(transparent)]
pub struct Block(usize);

/// Implement the value indexer trait for given indexer.
macro_rules! impl_value_indexer {
    ($indexer:ident) => {
        impl ValueIndexer for $indexer {
            fn new(id: usize) -> Self { Self(id) }

            fn index(&self) -> usize { self.0 }
        }

        impl From<Value> for $indexer {
            fn from(value: Value) -> Self { Self::new(value.index()) }
        }
    };
}

impl_value_indexer!(Inst);

impl_value_indexer!(Function);

impl Block {
    pub fn new(index: usize) -> Self { Self(index) }

    pub fn index(&self) -> usize { self.0 }
}

impl<T> From<T> for Value
where
    T: ValueIndexer,
{
    fn from(indexer: T) -> Self { Self::new(indexer.index()) }
}

/// Condition for integer comparison
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ICmpCond {
    Eq,
    Ne,
    Slt,
    Sle,
}

/// Condition for floating-point comparison
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FCmpCond {
    OEq,
    ONe,
    OLt,
    OLe,
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
        matches!(
            self,
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
                | BinaryOp::ICmp(_)
        )
    }

    /// If the operation requires floating-point operands
    pub(super) fn require_float(&self) -> bool {
        matches!(
            self,
            BinaryOp::FAdd
                | BinaryOp::FSub
                | BinaryOp::FMul
                | BinaryOp::FDiv
                | BinaryOp::FRem
                | BinaryOp::FCmp(_)
        )
    }

    /// If the operation requires the same type of operands
    pub(super) fn require_same_type(&self) -> bool {
        !matches!(self, BinaryOp::Shl | BinaryOp::LShr | BinaryOp::AShr)
    }
}

/// Unary operations.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    /// Floating point negation
    FNeg,
    /// Not
    Not,
}

impl UnaryOp {
    /// If the operation requires integer operands
    pub(super) fn require_int(&self) -> bool { false }

    /// If the operation requires floating-point operands
    pub(super) fn require_float(&self) -> bool { matches!(self, UnaryOp::FNeg) }
}

pub trait ReplaceUse {
    fn replace_use(&mut self, old: Value, new: Value);
}

/// Binary instruction internals.
#[derive(Debug)]
pub struct Binary {
    op: BinaryOp,
    lhs: Value,
    rhs: Value,
}

impl Binary {
    pub(super) fn new_value_data(ty: Type, op: BinaryOp, lhs: Value, rhs: Value) -> ValueData {
        ValueData::new(ty, ValueKind::Binary(Binary { op, lhs, rhs }))
    }

    pub fn op(&self) -> BinaryOp { self.op.clone() }

    pub fn lhs(&self) -> Value { self.lhs }

    pub fn rhs(&self) -> Value { self.rhs }

    pub fn set_op(&mut self, op: BinaryOp) -> BinaryOp { std::mem::replace(&mut self.op, op) }

    pub fn set_lhs(&mut self, lhs: Value) -> Value { std::mem::replace(&mut self.lhs, lhs) }

    pub fn set_rhs(&mut self, rhs: Value) -> Value { std::mem::replace(&mut self.rhs, rhs) }
}

impl ReplaceUse for Binary {
    fn replace_use(&mut self, old: Value, new: Value) {
        if self.lhs == old {
            self.lhs = new;
        }
        if self.rhs == old {
            self.rhs = new;
        }
    }
}

/// Unary instruction internals.
#[derive(Debug)]
pub struct Unary {
    op: UnaryOp,
    val: Value,
}

impl Unary {
    pub(super) fn new_value_data(ty: Type, op: UnaryOp, val: Value) -> ValueData {
        ValueData::new(ty, ValueKind::Unary(Unary { op, val }))
    }

    pub fn op(&self) -> UnaryOp { self.op.clone() }

    pub fn val(&self) -> Value { self.val }

    pub fn set_op(&mut self, op: UnaryOp) -> UnaryOp { std::mem::replace(&mut self.op, op) }

    pub fn set_val(&mut self, val: Value) -> Value { std::mem::replace(&mut self.val, val) }
}

impl ReplaceUse for Unary {
    fn replace_use(&mut self, old: Value, new: Value) {
        if self.val == old {
            self.val = new;
        }
    }
}

/// Store instruction internals.
#[derive(Debug)]
pub struct Store {
    val: Value,
    ptr: Value,
}

impl Store {
    pub(super) fn new_value_data(val: Value, ptr: Value) -> ValueData {
        ValueData::new(Type::void(), ValueKind::Store(Store { val, ptr }))
    }

    pub fn val(&self) -> Value { self.val }

    pub fn ptr(&self) -> Value { self.ptr }

    pub fn set_val(&mut self, val: Value) -> Value { std::mem::replace(&mut self.val, val) }

    pub fn set_ptr(&mut self, ptr: Value) -> Value { std::mem::replace(&mut self.ptr, ptr) }
}

impl ReplaceUse for Store {
    fn replace_use(&mut self, old: Value, new: Value) {
        if self.val == old {
            self.val = new;
        }
        if self.ptr == old {
            self.ptr = new;
        }
    }
}

/// Load instruction internals.
///
/// The type of the loaded value is available in [`ValueData`].
#[derive(Debug)]
pub struct Load {
    ptr: Value,
}

impl Load {
    pub(super) fn new_value_data(ty: Type, ptr: Value) -> ValueData {
        ValueData::new(ty, ValueKind::Load(Load { ptr }))
    }

    pub fn ptr(&self) -> Value { self.ptr }

    pub fn set_ptr(&mut self, ptr: Value) -> Value { std::mem::replace(&mut self.ptr, ptr) }
}

impl ReplaceUse for Load {
    fn replace_use(&mut self, old: Value, new: Value) {
        if self.ptr == old {
            self.ptr = new;
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum CastOp {
    /// Truncate
    Trunc,

    /// Zero extend
    ZExt,

    /// Sign extend
    SExt,

    /// Float to unsigned int
    FpToUI,

    /// Float to signed int
    FpToSI,

    /// Unsigned int to float
    UIToFp,

    /// Signed int to float
    SIToFp,

    /// Float truncation
    FpTrunc,

    /// Float extension
    FpExt,

    /// Bitcast
    Bitcast,
}

/// Type cast
///
/// Perform bitcast operation, the type is available in [`ValueData`].
#[derive(Debug)]
pub struct Cast {
    op: CastOp,
    val: Value,
}

impl Cast {
    pub(super) fn new_value_data(op: CastOp, ty: Type, val: Value) -> ValueData {
        ValueData::new(ty, ValueKind::Cast(Cast { op, val }))
    }

    pub fn val(&self) -> Value { self.val }

    pub fn op(&self) -> CastOp { self.op.clone() }

    pub fn set_val(&mut self, val: Value) -> Value { std::mem::replace(&mut self.val, val) }

    pub fn set_op(&mut self, op: CastOp) -> CastOp { std::mem::replace(&mut self.op, op) }
}

impl ReplaceUse for Cast {
    fn replace_use(&mut self, old: Value, new: Value) {
        if self.val == old {
            self.val = new;
        }
    }
}

/// Allocation instruction internals.
///
/// Allocation instruction always return a pointer to the allocated value.
/// Therefore, the type of the allocated value is required.
#[derive(Debug)]
pub struct Alloc {
    /// Type of the allocated value
    ty: Type,
}

impl Alloc {
    pub(super) fn new_value_data(ty: Type) -> ValueData {
        ValueData::new(Type::ptr(), ValueKind::Alloc(Alloc { ty }))
    }

    pub fn ty(&self) -> Type { self.ty.clone() }
}

/// A global memory slot.
///
/// The initial value of the global should be a constant.
/// The type of the global is available in [`ValueData`].
#[derive(Debug)]
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

    pub fn init(&self) -> Value { self.init }

    pub fn mutable(&self) -> bool { self.mutable }

    pub fn set_init(&mut self, init: Value) -> Value { std::mem::replace(&mut self.init, init) }

    pub fn set_mutable(&mut self, mutable: bool) -> bool {
        std::mem::replace(&mut self.mutable, mutable)
    }
}

impl ReplaceUse for GlobalSlot {
    fn replace_use(&mut self, old: Value, new: Value) {
        if self.init == old {
            self.init = new;
        }
    }
}

/// Jump instruction internals.
#[derive(Debug)]
pub struct Jump {
    dst: Block,
    args: Vec<Value>,
}

impl Jump {
    pub(super) fn new_value_data(dst: Block, args: Vec<Value>) -> ValueData {
        ValueData::new(Type::void(), ValueKind::Jump(Jump { dst, args }))
    }

    pub fn dst(&self) -> Block { self.dst }

    pub fn args(&self) -> &[Value] { &self.args }

    pub fn set_dst(&mut self, dst: Block) -> Block { std::mem::replace(&mut self.dst, dst) }

    pub fn set_arg(&mut self, index: usize, arg: Value) -> Value {
        std::mem::replace(&mut self.args[index], arg)
    }

    pub fn append_arg(&mut self, arg: Value) { self.args.push(arg); }

    pub fn extend_args(&mut self, args: Vec<Value>) { self.args.extend(args); }

    pub fn set_args(&mut self, args: Vec<Value>) -> Vec<Value> {
        std::mem::replace(&mut self.args, args)
    }
}

impl ReplaceUse for Jump {
    fn replace_use(&mut self, old: Value, new: Value) {
        for arg in &mut self.args {
            if *arg == old {
                *arg = new;
            }
        }
    }
}

/// Branch instruction internals.
#[derive(Debug)]
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
            Type::void(),
            ValueKind::Branch(Branch {
                cond,
                then_dst,
                else_dst,
                then_args,
                else_args,
            }),
        )
    }

    pub fn cond(&self) -> Value { self.cond }

    pub fn then_dst(&self) -> Block { self.then_dst }

    pub fn else_dst(&self) -> Block { self.else_dst }

    pub fn append_then_arg(&mut self, arg: Value) { self.then_args.push(arg); }

    pub fn append_else_arg(&mut self, arg: Value) { self.else_args.push(arg); }

    pub fn extend_then_args(&mut self, args: Vec<Value>) { self.then_args.extend(args); }

    pub fn extend_else_args(&mut self, args: Vec<Value>) { self.else_args.extend(args); }

    pub fn then_args(&self) -> &[Value] { &self.then_args }

    pub fn else_args(&self) -> &[Value] { &self.else_args }

    pub fn set_cond(&mut self, cond: Value) -> Value { std::mem::replace(&mut self.cond, cond) }

    pub fn set_then_dst(&mut self, then_dst: Block) -> Block {
        std::mem::replace(&mut self.then_dst, then_dst)
    }

    pub fn set_else_dst(&mut self, else_dst: Block) -> Block {
        std::mem::replace(&mut self.else_dst, else_dst)
    }

    pub fn set_then_arg(&mut self, index: usize, arg: Value) -> Value {
        std::mem::replace(&mut self.then_args[index], arg)
    }

    pub fn set_else_arg(&mut self, index: usize, arg: Value) -> Value {
        std::mem::replace(&mut self.else_args[index], arg)
    }

    pub fn set_then_args(&mut self, args: Vec<Value>) -> Vec<Value> {
        std::mem::replace(&mut self.then_args, args)
    }

    pub fn set_else_args(&mut self, args: Vec<Value>) -> Vec<Value> {
        std::mem::replace(&mut self.else_args, args)
    }
}

impl ReplaceUse for Branch {
    fn replace_use(&mut self, old: Value, new: Value) {
        if self.cond == old {
            self.cond = new;
        }
        for arg in &mut self.then_args {
            if *arg == old {
                *arg = new;
            }
        }
        for arg in &mut self.else_args {
            if *arg == old {
                *arg = new;
            }
        }
    }
}

/// Return instruction internals.
#[derive(Debug)]
pub struct Return {
    val: Option<Value>,
}

impl Return {
    pub(super) fn new_value_data(val: Option<Value>) -> ValueData {
        ValueData::new(Type::void(), ValueKind::Return(Return { val }))
    }

    pub fn val(&self) -> Option<Value> { self.val }

    pub fn set_val(&mut self, val: Option<Value>) -> Option<Value> {
        std::mem::replace(&mut self.val, val)
    }
}

impl ReplaceUse for Return {
    fn replace_use(&mut self, old: Value, new: Value) {
        if let Some(val) = &mut self.val {
            if *val == old {
                *val = new;
            }
        }
    }
}

/// A function call.
///
/// The function type can be inferred from the args and the [`ValueData`].
#[derive(Debug)]
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

    pub fn callee(&self) -> Value { self.callee }

    pub fn args(&self) -> &[Value] { &self.args }

    pub fn set_callee(&mut self, callee: Value) -> Value {
        std::mem::replace(&mut self.callee, callee)
    }

    pub fn set_arg(&mut self, index: usize, arg: Value) -> Value {
        std::mem::replace(&mut self.args[index], arg)
    }

    pub fn set_args(&mut self, args: Vec<Value>) -> Vec<Value> {
        std::mem::replace(&mut self.args, args)
    }
}

impl ReplaceUse for Call {
    fn replace_use(&mut self, old: Value, new: Value) {
        if self.callee == old {
            self.callee = new;
        }
        for arg in &mut self.args {
            if *arg == old {
                *arg = new;
            }
        }
    }
}

/// Get element pointer instruction internals.
#[derive(Debug)]
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
            Type::ptr(),
            ValueKind::GetElemPtr(GetElemPtr { ptr, ty, indices }),
        )
    }

    /// Get the pointer
    pub fn ptr(&self) -> Value { self.ptr }

    /// Get the bound type
    pub fn ty(&self) -> Type { self.ty.clone() }

    /// Get the indices
    pub fn indices(&self) -> &[Value] { &self.indices }

    /// Set the pointer
    pub fn set_ptr(&mut self, ptr: Value) -> Value { std::mem::replace(&mut self.ptr, ptr) }

    pub fn set_index(&mut self, index: usize, value: Value) -> Value {
        std::mem::replace(&mut self.indices[index], value)
    }

    pub fn set_indices(&mut self, indices: Vec<Value>) -> Vec<Value> {
        std::mem::replace(&mut self.indices, indices)
    }
}

impl ReplaceUse for GetElemPtr {
    fn replace_use(&mut self, old: Value, new: Value) {
        if self.ptr == old {
            self.ptr = new;
        }
        for index in &mut self.indices {
            if *index == old {
                *index = new;
            }
        }
    }
}
