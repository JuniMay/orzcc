use core::fmt;
use std::{collections::HashMap, vec};

use super::{
    constant::FloatConstant,
    debug::CommentPos,
    source_loc::Span,
    Block,
    Context,
    Signature,
    Symbol,
    SymbolKind,
    Ty,
    Value,
    ValueData,
};
use crate::{
    collections::{
        apint::ApInt,
        linked_list::LinkedListNodePtr,
        storage::{ArenaAlloc, ArenaFree, ArenaPtr, BaseArenaPtr},
    },
    impl_arena,
    utils::def_use::{Operand, Usable, User},
};

/// The integer comparison condition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ICmpCond {
    /// Equal.
    Eq,
    /// Not equal.
    Ne,
    /// Signed less than.
    Slt,
    /// Signed less than or equal.
    Sle,
    /// Unsigned less than.
    Ult,
    /// Unsigned less than or equal.
    Ule,
}

impl fmt::Display for ICmpCond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Eq => write!(f, "eq"),
            Self::Ne => write!(f, "ne"),
            Self::Slt => write!(f, "slt"),
            Self::Sle => write!(f, "sle"),
            Self::Ult => write!(f, "ult"),
            Self::Ule => write!(f, "ule"),
        }
    }
}

impl From<&str> for ICmpCond {
    fn from(s: &str) -> Self {
        match s {
            "eq" => Self::Eq,
            "ne" => Self::Ne,
            "slt" => Self::Slt,
            "sle" => Self::Sle,
            "ult" => Self::Ult,
            "ule" => Self::Ule,
            _ => panic!("invalid integer comparison condition: {}", s),
        }
    }
}

/// The floating-point comparison condition.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FCmpCond {
    /// Ordered and equal.
    OEq,
    /// Ordered and not equal.
    ONe,
    /// Ordered and less than.
    OLt,
    /// Ordered and less than or equal.
    OLe,
    /// Unordered or equal.
    UEq,
    /// Unordered or not equal.
    UNe,
    /// Unordered or less than.
    ULt,
    /// Unordered or less than or equal.
    ULe,
}

impl fmt::Display for FCmpCond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::OEq => write!(f, "oeq"),
            Self::ONe => write!(f, "one"),
            Self::OLt => write!(f, "olt"),
            Self::OLe => write!(f, "ole"),
            Self::UEq => write!(f, "ueq"),
            Self::UNe => write!(f, "une"),
            Self::ULt => write!(f, "ult"),
            Self::ULe => write!(f, "ule"),
        }
    }
}

impl From<&str> for FCmpCond {
    fn from(s: &str) -> Self {
        match s {
            "oeq" => Self::OEq,
            "one" => Self::ONe,
            "olt" => Self::OLt,
            "ole" => Self::OLe,
            "ueq" => Self::UEq,
            "une" => Self::UNe,
            "ult" => Self::ULt,
            "ule" => Self::ULe,
            _ => panic!("invalid floating-point comparison condition: {}", s),
        }
    }
}

/// Integer binary operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IBinaryOp {
    /// Add.
    Add,
    /// Subtract.
    Sub,
    /// Multiply.
    Mul,
    /// Unsigned division.
    UDiv,
    /// Signed division.
    SDiv,
    /// Unsigned remainder.
    URem,
    /// Signed remainder.
    SRem,
    /// Bitwise and.
    And,
    /// Bitwise or.
    Or,
    /// Bitwise xor.
    Xor,
    /// Shift left.
    Shl,
    /// Logical shift right.
    LShr,
    /// Arithmetic shift right.
    AShr,
    /// Comparison.
    Cmp(ICmpCond),
}

impl fmt::Display for IBinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "add"),
            Self::Sub => write!(f, "sub"),
            Self::Mul => write!(f, "mul"),
            Self::UDiv => write!(f, "udiv"),
            Self::SDiv => write!(f, "sdiv"),
            Self::URem => write!(f, "urem"),
            Self::SRem => write!(f, "srem"),
            Self::And => write!(f, "and"),
            Self::Or => write!(f, "or"),
            Self::Xor => write!(f, "xor"),
            Self::Shl => write!(f, "shl"),
            Self::LShr => write!(f, "lshr"),
            Self::AShr => write!(f, "ashr"),
            Self::Cmp(cond) => write!(f, "icmp.{}", cond),
        }
    }
}

/// Floating-point binary operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FBinaryOp {
    /// Add.
    Add,
    /// Subtract.
    Sub,
    /// Multiply.
    Mul,
    /// Divide.
    Div,
    /// Remainder.
    Rem,
    /// Comparison.
    Cmp(FCmpCond),
}

impl fmt::Display for FBinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "fadd"),
            Self::Sub => write!(f, "fsub"),
            Self::Mul => write!(f, "fmul"),
            Self::Div => write!(f, "fdiv"),
            Self::Rem => write!(f, "frem"),
            Self::Cmp(cond) => write!(f, "fcmp.{}", cond),
        }
    }
}

/// Integer unary operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IUnaryOp {
    /// Bitwise not.
    Not,
}

impl fmt::Display for IUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Not => write!(f, "not"),
        }
    }
}

impl From<&str> for IUnaryOp {
    fn from(s: &str) -> Self {
        match s {
            "not" => Self::Not,
            _ => panic!("invalid integer unary operation: {}", s),
        }
    }
}

/// Floating-point unary operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FUnaryOp {
    /// Negation.
    Neg,
}

impl fmt::Display for FUnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Neg => write!(f, "fneg"),
        }
    }
}

impl From<&str> for FUnaryOp {
    fn from(s: &str) -> Self {
        match s {
            "fneg" => Self::Neg,
            _ => panic!("invalid floating-point unary operation: {}", s),
        }
    }
}

/// Cast operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CastOp {
    /// Truncate.
    Trunc,
    /// Floating point truncate.
    FpTrunc,
    /// Zero-extend.
    ZExt,
    /// Sign-extend.
    SExt,
    /// Float to unsigned integer.
    FpToUi,
    /// Float to signed integer.
    FpToSi,
    /// Unsigned integer to float.
    UiToFp,
    /// Signed integer to float.
    SiToFp,
    /// Bitcast.
    ///
    /// This does not apply to pointer-int conversion, use [CastOp::PtrToInt] or
    /// [CastOp::IntToPtr] instead.
    Bitcast,
    /// Float extension.
    FpExt,
    /// Pointer to integer.
    PtrToInt,
    /// Integer to pointer.
    IntToPtr,
}

impl fmt::Display for CastOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Trunc => write!(f, "trunc"),
            Self::FpTrunc => write!(f, "fptrunc"),
            Self::ZExt => write!(f, "zext"),
            Self::SExt => write!(f, "sext"),
            Self::FpToUi => write!(f, "fptoui"),
            Self::FpToSi => write!(f, "fptosi"),
            Self::UiToFp => write!(f, "uitofp"),
            Self::SiToFp => write!(f, "sitofp"),
            Self::Bitcast => write!(f, "bitcast"),
            Self::FpExt => write!(f, "fpext"),
            Self::PtrToInt => write!(f, "ptrtoint"),
            Self::IntToPtr => write!(f, "inttoptr"),
        }
    }
}

impl From<&str> for CastOp {
    fn from(s: &str) -> Self {
        match s {
            "trunc" => Self::Trunc,
            "fptrunc" => Self::FpTrunc,
            "zext" => Self::ZExt,
            "sext" => Self::SExt,
            "fptoui" => Self::FpToUi,
            "fptosi" => Self::FpToSi,
            "uitofp" => Self::UiToFp,
            "sitofp" => Self::SiToFp,
            "bitcast" => Self::Bitcast,
            "fpext" => Self::FpExt,
            "ptrtoint" => Self::PtrToInt,
            "inttoptr" => Self::IntToPtr,
            _ => panic!("invalid cast operation: {}", s),
        }
    }
}

/// Successor of a branch instruction.
///
/// Successor describes the destination block and the argument passing.
pub struct Successor {
    /// The destination block.
    pub(super) block: Operand<Block>,
    /// Argument mapping.
    ///
    /// This represents the parameter to argument mapping, the keys represent
    /// parameters, and the values represent arguments.
    pub(super) args: HashMap<Value, Operand<Value>>,
}

impl Successor {
    pub fn new(block: Operand<Block>) -> Self {
        Self {
            block,
            args: HashMap::new(),
        }
    }

    /// Add an argument mapping.
    pub fn add_arg(&mut self, param: Value, arg: Operand<Value>) { self.args.insert(param, arg); }

    pub fn display<'a>(&'a self, ctx: &'a Context) -> DisplaySuccessor<'a> {
        DisplaySuccessor { ctx, succ: self }
    }
}

pub struct DisplaySuccessor<'a> {
    ctx: &'a Context,
    succ: &'a Successor,
}

impl<'a> fmt::Display for DisplaySuccessor<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "^{}", self.succ.block.inner().name(self.ctx).unwrap())?;
        if !self.succ.args.is_empty() {
            write!(f, "(")?;

            let params = self.succ.block.inner().params(self.ctx);

            for (i, param) in params.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }

                write!(
                    f,
                    "{}",
                    self.succ
                        .args
                        .get(param)
                        .unwrap()
                        .inner()
                        .name(self.ctx)
                        .unwrap()
                )?;
            }

            write!(f, ")")?;
        }
        Ok(())
    }
}

pub enum InstKind {
    /// Create a new value from a constant.
    ///
    /// Using an individual constant creation instruction is similar to MLIR and
    /// Cranelift IR.
    IConst(ApInt),
    /// Create a new value from a constant.
    ///
    /// Currently, only f32 and f64 are supported.
    FConst(FloatConstant),
    /// Create a new value as a stack slot.
    ///
    /// The result type is a pointer.
    StackSlot(u32),
    /// Integer binary instruction.
    IBinary(IBinaryOp),
    /// Floating-point binary instruction.
    FBinary(FBinaryOp),
    /// Integer unary instruction.
    IUnary(IUnaryOp),
    /// Floating-point unary instruction.
    FUnary(FUnaryOp),
    /// The cast instruction.
    ///
    /// The type of the destination value is determined by the type of the
    /// result value.
    Cast(CastOp),
    /// The offset instruction.
    ///
    /// This instruction is to calculate an index from a base pointer and a
    /// calculated offset (must be integer).
    ///
    /// `offset` is equivalent to `getelementptr inbounds i8, %base, %offset` in
    /// LLVM IR. The only difference is that `offset` makes the calculation of
    /// the offset independent from the base pointer.
    Offset,
    /// Jump instruction.
    Jump,
    /// Branch instruction.
    Br,
    /// Switch instruction.
    Switch { labels: Vec<ApInt> },
    /// Call instruction.
    Call(Symbol),
    /// Call indirect instruction.
    ///
    /// The first operand is the function pointer, and the rest are arguments.
    CallIndirect(Signature),
    /// Return instruction.
    Ret,
    /// Get global symbol as local pinter.
    ///
    /// This applies to function and global slot. The corresponding instruction
    /// in RISC-V is `LA`, i.e., load address (`LA` a pseudo instruction)
    GetGlobal(Symbol),
    /// Load instruction.
    ///
    /// The loaded type can be retrieved from the result value.
    Load,
    /// Store instruction.
    ///
    /// The first operand is the value to store, the second operand is the
    /// address to store. This can be interpreted as `store value (to) address`.
    Store,
}

pub struct InstData {
    self_ptr: Inst,

    /// Result of this instruction.
    results: Vec<Value>,
    /// The instruction kind.
    pub(super) kind: InstKind,
    /// The operands
    pub(super) operands: Vec<Operand<Value>>,
    /// The successors
    pub(super) successors: Vec<Successor>,

    /// The next instruction.
    next: Option<Inst>,
    /// The previous instruction.
    prev: Option<Inst>,
    /// The parent block.
    parent: Option<Block>,

    source_span: Span,
}

impl InstData {
    pub fn self_ptr(&self) -> Inst { self.self_ptr }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Inst(BaseArenaPtr<InstData>);

impl_arena!(Context, InstData, Inst, insts);

impl Inst {
    fn new(ctx: &mut Context, kind: InstKind, result_tys: Vec<Ty>, operands: Vec<Value>) -> Inst {
        let inst = ctx.alloc_with(|self_ptr| InstData {
            self_ptr,
            results: Vec::new(),
            kind,

            operands: Vec::new(),
            successors: Vec::new(),

            next: None,
            prev: None,
            parent: None,

            source_span: Span::default(),
        });

        let operands = operands
            .into_iter()
            .map(|val| Operand::new(ctx, val, inst))
            .collect::<Vec<_>>();

        for (i, ty) in result_tys.into_iter().enumerate() {
            let value =
                ctx.alloc_with(|self_ptr| ValueData::new_inst_result(self_ptr, ty, inst, i));
            inst.deref_mut(ctx).results.push(value);
        }

        inst.deref_mut(ctx).operands = operands;

        inst
    }

    pub fn set_source_span(&self, ctx: &mut Context, span: impl Into<Span>) {
        self.deref_mut(ctx).source_span = span.into();
    }

    pub fn source_span(self, ctx: &Context) -> Span { self.deref(ctx).source_span }

    /// Create a new iconst instruction.
    pub fn iconst(ctx: &mut Context, constant: impl Into<ApInt>, ty: Ty) -> Inst {
        Self::new(ctx, InstKind::IConst(constant.into()), vec![ty], vec![])
    }

    /// Create a new fconst instruction for float32
    pub fn fconst32(ctx: &mut Context, constant: f32, ty: Ty) -> Inst {
        Self::new(ctx, InstKind::FConst(constant.into()), vec![ty], vec![])
    }

    /// Create a new fconst instruction for float64
    pub fn fconst64(ctx: &mut Context, constant: f64, ty: Ty) -> Inst {
        Self::new(ctx, InstKind::FConst(constant.into()), vec![ty], vec![])
    }

    /// Create a new stack slot instruction.
    pub fn stack_slot(ctx: &mut Context, size: u32) -> Inst {
        let ty = Ty::ptr(ctx);
        Self::new(ctx, InstKind::StackSlot(size), vec![ty], vec![])
    }

    pub fn ibinary(ctx: &mut Context, op: IBinaryOp, lhs: Value, rhs: Value) -> Inst {
        use IBinaryOp as Op;

        if !lhs.ty(ctx).is_integer(ctx) || !rhs.ty(ctx).is_integer(ctx) {
            // ibinary only supports integer-like types
            panic!("lhs and rhs must be integer-like types");
        }

        if lhs.ty(ctx) != rhs.ty(ctx) && !matches!(op, Op::Shl | Op::LShr | Op::AShr) {
            // for non-shift operations, lhs and rhs must have the same type
            panic!("lhs and rhs must have the same type");
        }

        let ty = match op {
            Op::Add
            | Op::Sub
            | Op::Mul
            | Op::UDiv
            | Op::SDiv
            | Op::URem
            | Op::SRem
            | Op::And
            | Op::Or
            | Op::Xor
            | Op::Shl
            | Op::LShr
            | Op::AShr => lhs.ty(ctx),
            Op::Cmp(_) => Ty::int(ctx, 1),
        };

        Self::new(ctx, InstKind::IBinary(op), vec![ty], vec![lhs, rhs])
    }

    pub fn fbinary(ctx: &mut Context, op: FBinaryOp, lhs: Value, rhs: Value) -> Inst {
        use FBinaryOp as Op;

        if !lhs.ty(ctx).is_float(ctx) || !rhs.ty(ctx).is_float(ctx) {
            // fbinary only supports float-like types
            panic!("lhs and rhs must be float-like types");
        }

        if lhs.ty(ctx) != rhs.ty(ctx) {
            // lhs and rhs must have the same type
            panic!("lhs and rhs must have the same type");
        }

        let ty = match op {
            Op::Add | Op::Sub | Op::Mul | Op::Div | Op::Rem => lhs.ty(ctx),
            Op::Cmp(_) => Ty::int(ctx, 1),
        };

        Self::new(ctx, InstKind::FBinary(op), vec![ty], vec![lhs, rhs])
    }

    pub fn iunary(ctx: &mut Context, op: IUnaryOp, val: Value) -> Inst {
        use IUnaryOp as Op;

        if !val.ty(ctx).is_integer(ctx) {
            // iunary only supports integer-like types
            panic!("val must be an integer-like type");
        }

        let ty = match op {
            Op::Not => val.ty(ctx),
        };

        Self::new(ctx, InstKind::IUnary(op), vec![ty], vec![val])
    }

    pub fn funary(ctx: &mut Context, op: FUnaryOp, val: Value) -> Inst {
        use FUnaryOp as Op;

        if !val.ty(ctx).is_float(ctx) {
            // funary only supports float-like types
            panic!("val must be a float-like type");
        }

        let ty = match op {
            Op::Neg => val.ty(ctx),
        };

        Self::new(ctx, InstKind::FUnary(op), vec![ty], vec![val])
    }

    pub fn cast(ctx: &mut Context, op: CastOp, val: Value, ty: Ty) -> Inst {
        let val_ty = val.ty(ctx);

        // TODO: support SIMD types
        match op {
            CastOp::Trunc => {
                if !val_ty.is_integer(ctx) || !ty.is_integer(ctx) {
                    panic!("trunc only supports integer-like types");
                }
                if val_ty.bitwidth(ctx).unwrap() <= ty.bitwidth(ctx).unwrap() {
                    panic!("trunc only supports truncating to a smaller type");
                }
            }
            CastOp::ZExt | CastOp::SExt => {
                if !val_ty.is_integer(ctx) || !ty.is_integer(ctx) {
                    panic!("zext only supports integer-like types");
                }
                if val_ty.bitwidth(ctx).unwrap() >= ty.bitwidth(ctx).unwrap() {
                    panic!("zext only supports extending to a larger type");
                }
            }
            CastOp::FpToUi | CastOp::FpToSi => {
                if !val_ty.is_float(ctx) || !ty.is_integer(ctx) {
                    panic!("fptoui only supports float-like to integer-like types");
                }
            }
            CastOp::UiToFp | CastOp::SiToFp => {
                if !val_ty.is_integer(ctx) || !ty.is_float(ctx) {
                    panic!("uitofp only supports integer-like to float-like types");
                }
            }
            CastOp::Bitcast => {
                if val_ty.is_ptr(ctx) || ty.is_ptr(ctx) {
                    panic!("bitcast does not support pointer types");
                }
                if val_ty.bitwidth(ctx).unwrap() != ty.bitwidth(ctx).unwrap() {
                    panic!("bitcast only supports types with the same size");
                }
            }
            CastOp::FpExt => {
                if !val_ty.is_float(ctx) || !ty.is_float(ctx) {
                    panic!("fpext only supports float-like types");
                }
                if val_ty.bitwidth(ctx).unwrap() >= ty.bitwidth(ctx).unwrap() {
                    panic!("fpext only supports extending to a larger type");
                }
            }
            CastOp::FpTrunc => {
                if !val_ty.is_float(ctx) || !ty.is_float(ctx) {
                    panic!("fptrunc only supports float-like types");
                }
                if val_ty.bitwidth(ctx).unwrap() <= ty.bitwidth(ctx).unwrap() {
                    panic!("fptrunc only supports truncating to a smaller type");
                }
            }
            CastOp::PtrToInt => {
                if !val_ty.is_ptr(ctx) || !ty.is_integer(ctx) {
                    panic!("ptrtoint only supports pointer to integer-like types");
                }
            }
            CastOp::IntToPtr => {
                if !val_ty.is_integer(ctx) || !ty.is_ptr(ctx) {
                    panic!("inttoptr only supports integer-like to pointer types");
                }
            }
        }

        Self::new(ctx, InstKind::Cast(op), vec![ty], vec![val])
    }

    pub fn offset(ctx: &mut Context, base: Value, offset: Value) -> Inst {
        if !base.ty(ctx).is_ptr(ctx) {
            panic!("base must be an pointer type");
        }

        if !offset.ty(ctx).is_integer(ctx) {
            panic!("offset must be an integer-like type");
        }

        Self::new(
            ctx,
            InstKind::Offset,
            vec![base.ty(ctx)],
            vec![base, offset],
        )
    }

    pub fn jump(ctx: &mut Context, target: Block, args: Vec<Value>) -> Inst {
        let params = target.params(ctx).to_vec();
        if params.len() != args.len() {
            panic!("number of arguments does not match the target block's parameters");
        }
        for (arg, param) in args.iter().zip(params.iter()) {
            if arg.ty(ctx) != param.ty(ctx) {
                panic!("argument type does not match the target block's parameters");
            }
        }

        let inst = Self::new(ctx, InstKind::Jump, vec![], vec![]);

        let mut succ = Successor::new(Operand::new(ctx, target, inst));
        for (param, arg) in params.into_iter().zip(args) {
            let arg = Operand::new(ctx, arg, inst);
            succ.add_arg(param, arg);
        }
        inst.deref_mut(ctx).successors = vec![succ];

        inst
    }

    pub fn br(
        ctx: &mut Context,
        cond: Value,
        then_block: Block,
        then_args: Vec<Value>,
        else_block: Block,
        else_args: Vec<Value>,
    ) -> Inst {
        if cond.ty(ctx) != Ty::int(ctx, 1) {
            panic!("condition must be an i1 type");
        }

        let then_params = then_block.params(ctx).to_vec();
        if then_params.len() != then_args.len() {
            panic!("number of arguments does not match the then block's parameters");
        }
        for (arg, param) in then_args.iter().zip(then_params.iter()) {
            if arg.ty(ctx) != param.ty(ctx) {
                panic!("argument type does not match the then block's parameters");
            }
        }

        let else_params = else_block.params(ctx).to_vec();
        if else_params.len() != else_args.len() {
            panic!("number of arguments does not match the else block's parameters");
        }
        for (arg, param) in else_args.iter().zip(else_params.iter()) {
            if arg.ty(ctx) != param.ty(ctx) {
                panic!("argument type does not match the else block's parameters");
            }
        }

        let inst = Self::new(ctx, InstKind::Br, vec![], vec![cond]);

        let mut then_succ = Successor::new(Operand::new(ctx, then_block, inst));
        for (param, arg) in then_params.into_iter().zip(then_args) {
            let arg = Operand::new(ctx, arg, inst);
            then_succ.add_arg(param, arg);
        }

        let mut else_succ = Successor::new(Operand::new(ctx, else_block, inst));
        for (param, arg) in else_params.into_iter().zip(else_args) {
            let arg = Operand::new(ctx, arg, inst);
            else_succ.add_arg(param, arg);
        }

        inst.deref_mut(ctx).successors = vec![then_succ, else_succ];

        inst
    }

    pub fn switch(
        ctx: &mut Context,
        cond: Value,
        mut branches: Vec<(Option<ApInt>, Block, Vec<Value>)>,
    ) -> Inst {
        for (_, dest, args) in branches.iter() {
            let params = dest.params(ctx).to_vec();
            if params.len() != args.len() {
                panic!("number of arguments does not match the target block's parameters");
            }
            for (arg, param) in args.iter().zip(params.iter()) {
                if arg.ty(ctx) != param.ty(ctx) {
                    panic!("argument type does not match the target block's parameters");
                }
            }
        }

        for (i, (label, _, _)) in branches.iter().enumerate() {
            match label {
                None => {
                    if i != branches.len() - 1 {
                        panic!("default label must be the last branch");
                    }
                }
                Some(_) => {}
            }
        }

        let mut labels = Vec::new();
        let mut block_args = Vec::new();

        for (label, dest, args) in branches.drain(..) {
            if let Some(label) = label {
                labels.push(label);
            }
            block_args.push((dest, args));
        }

        let inst = Self::new(ctx, InstKind::Switch { labels }, vec![], vec![cond]);

        let mut succs = Vec::new();
        for (dest, args) in block_args {
            let params = dest.params(ctx).to_vec();
            let mut succ = Successor::new(Operand::new(ctx, dest, inst));
            for (param, arg) in params.into_iter().zip(args) {
                let arg = Operand::new(ctx, arg, inst);
                succ.add_arg(param, arg);
            }
            succs.push(succ);
        }

        inst.deref_mut(ctx).successors = succs;

        inst
    }

    pub fn call(ctx: &mut Context, symbol: Symbol, args: Vec<Value>, result_tys: Vec<Ty>) -> Inst {
        if let Some(SymbolKind::FuncDef(func)) = ctx.lookup_symbol(&symbol) {
            let sig = func.sig(ctx);
            if sig.params.len() != args.len() {
                panic!("number of arguments does not match the function signature");
            }
            if sig.ret.len() != result_tys.len() {
                panic!("number of results does not match the function signature");
            }

            for (arg, param) in args.iter().zip(sig.params.iter()) {
                if arg.ty(ctx) != *param {
                    panic!("argument type does not match the function signature");
                }
            }

            for (result, ret) in result_tys.iter().zip(sig.ret.iter()) {
                if *result != *ret {
                    panic!("result type does not match the function signature");
                }
            }
        } else {
            panic!("symbol is not valid");
        }

        Self::new(ctx, InstKind::Call(symbol), result_tys, args)
    }

    pub fn call_indirect(
        ctx: &mut Context,
        sig: Signature,
        func: Value,
        args: Vec<Value>,
        result_tys: Vec<Ty>,
    ) -> Inst {
        if sig.params.len() != args.len() {
            panic!("number of arguments does not match the function signature");
        }
        if sig.ret.len() != result_tys.len() {
            panic!("number of results does not match the function signature");
        }

        for (arg, param) in args.iter().zip(sig.params.iter()) {
            if arg.ty(ctx) != *param {
                panic!("argument type does not match the function signature");
            }
        }

        for (result, ret) in result_tys.iter().zip(sig.ret.iter()) {
            if *result != *ret {
                panic!("result type does not match the function signature");
            }
        }

        if !func.ty(ctx).is_ptr(ctx) {
            panic!("function must be an pointer type");
        }

        let mut operands = vec![func];
        operands.extend(args);

        Self::new(ctx, InstKind::CallIndirect(sig), result_tys, operands)
    }

    pub fn ret(ctx: &mut Context, vals: Vec<Value>) -> Inst {
        Self::new(ctx, InstKind::Ret, vec![], vals)
    }

    pub fn load(ctx: &mut Context, ptr: Value, ty: Ty) -> Inst {
        if !ptr.ty(ctx).is_ptr(ctx) {
            panic!("ptr must be a pointer type");
        }
        Self::new(ctx, InstKind::Load, vec![ty], vec![ptr])
    }

    pub fn store(ctx: &mut Context, val: Value, ptr: Value) -> Inst {
        if !ptr.ty(ctx).is_ptr(ctx) {
            panic!("ptr must be a pointer type");
        }
        Self::new(ctx, InstKind::Store, vec![], vec![val, ptr])
    }

    pub fn get_global(ctx: &mut Context, symbol: Symbol) -> Inst {
        let ptr = Ty::ptr(ctx);
        Self::new(ctx, InstKind::GetGlobal(symbol), vec![ptr], vec![])
    }

    pub fn is_terminator(self, ctx: &Context) -> bool {
        matches!(
            self.deref(ctx).kind,
            InstKind::Jump | InstKind::Br | InstKind::Switch { .. } | InstKind::Ret
        )
    }

    /// Get the successor blocks of the branch instruction.
    ///
    /// # Panics
    ///
    /// Panics if the instruction is not a branch instruction. Check with
    /// [Inst::is_terminator] before calling this method.
    ///
    /// # See Also
    ///
    /// - [Inst::is_terminator]
    pub fn succ_blocks(self, ctx: &Context) -> Vec<Block> {
        if !self.is_terminator(ctx) {
            panic!("instruction is not a terminator");
        }
        self.deref(ctx)
            .successors
            .iter()
            .map(|s| s.block.inner())
            .collect()
    }

    /// Free the instruction from the context.
    ///
    /// This method will also free all the result values of the instruction.
    ///
    /// # Panics
    ///
    /// - Panics if any result value is used by other instructions.
    /// - Panics if the instruction is not unlink-ed.
    ///
    /// # See Also
    ///
    /// - [Inst::unlink]
    /// - [Inst::remove]
    pub fn drop(self, ctx: &mut Context) {
        // check the uses of result values
        for result in self.deref(ctx).results.iter() {
            if !result.users(ctx).is_empty() {
                panic!("cannot remove instruction because some result values are still in use");
            }
        }
        // check if the instruction is unlinked
        if self.container(ctx).is_some() {
            panic!("cannot remove instruction because it is still linked");
        }
        // free all the result values
        let results = self.deref_mut(ctx).results.drain(..).collect::<Vec<_>>();
        for result in results {
            result.drop(ctx);
        }
        // update the uses of blocks
        let succs = self.deref_mut(ctx).successors.drain(..).collect::<Vec<_>>();
        for mut succ in succs {
            succ.args.drain().for_each(|(_, arg)| arg.drop(ctx));
            succ.block.drop(ctx);
        }

        // update the uses of operands
        let opds = self.deref_mut(ctx).operands.drain(..).collect::<Vec<_>>();
        for opd in opds {
            opd.drop(ctx);
        }

        // free the instruction
        ctx.free(self);
    }

    /// Unlink and drop the instruction.
    ///
    /// This method will unlink the instruction from the parent block and then
    /// drop the instruction.
    ///
    /// # Panics
    ///
    /// Panics if any result value is used by other instructions.
    ///
    /// # See Also
    ///
    /// - [Inst::unlink]
    /// - [Inst::drop]
    pub fn remove(self, ctx: &mut Context) {
        self.unlink(ctx);
        self.drop(ctx);
    }

    pub fn results(self, ctx: &Context) -> &[Value] { &self.deref(ctx).results }

    pub fn comment(self, ctx: &mut Context, pos: CommentPos, content: impl Into<String>) {
        ctx.comment_info.comment_inst(self, pos, content.into());
    }

    pub fn result(self, ctx: &Context, idx: usize) -> Value { self.deref(ctx).results[idx] }

    pub fn display(self, ctx: &Context, debug: bool) -> DisplayInst<'_> {
        DisplayInst {
            ctx,
            data: self.deref(ctx),
            debug,
        }
    }
}

impl LinkedListNodePtr for Inst {
    type ContainerPtr = Block;

    fn next(self, ctx: &Context) -> Option<Inst> { self.deref(ctx).next }

    fn prev(self, ctx: &Context) -> Option<Inst> { self.deref(ctx).prev }

    fn set_next(self, ctx: &mut Context, next: Option<Inst>) { self.deref_mut(ctx).next = next; }

    fn set_prev(self, ctx: &mut Context, prev: Option<Inst>) { self.deref_mut(ctx).prev = prev; }

    fn container(self, ctx: &Context) -> Option<Block> { self.deref(ctx).parent }

    fn set_container(self, ctx: &mut Context, container: Option<Block>) {
        self.deref_mut(ctx).parent = container;
    }
}

impl User<Block> for Inst {
    fn all_uses(self, ctx: &Context) -> Vec<Block> {
        self.deref(ctx)
            .successors
            .iter()
            .map(|s| s.block.inner())
            .collect()
    }

    fn replace(self, ctx: &mut Context, old: Block, new: Block) {
        // we need to be very careful, because the block parameters might be
        // different. So we need to check the argument mapping, the number of
        // arguments and the type of the arguments.

        // firstly, check if the old block and new block have compatible
        // parameters, i.e., same number and same types

        // the number of parameters must be the same
        if old.params(ctx).len() != new.params(ctx).len() {
            panic!("incompatible block parameter count");
        }

        // the type of the parameters must be the same
        for (old_param, new_param) in old.params(ctx).iter().zip(new.params(ctx).iter()) {
            if old_param.ty(ctx) != new_param.ty(ctx) {
                panic!("incompatible block parameter type");
            }
        }

        if !self.is_terminator(ctx) {
            // if the instruction is not a terminator, we don't need to do
            // anything
            return;
        }

        // the mapping from old block parameters to new block parameters according to
        // the position/index in the parameter list.
        let param_mapping = old
            .params(ctx)
            .iter()
            .copied()
            .zip(new.params(ctx).iter().copied())
            .collect::<HashMap<_, _>>();

        let mut replaced = false;

        // now we can replace the block and the argument mapping params (keys)

        for succ in self.deref_mut(ctx).successors.iter_mut() {
            if succ.block.set_inner_if_eq(old, new) {
                succ.args = succ
                    .args
                    .drain()
                    .map(|(old_param, arg)| {
                        let new_param = param_mapping.get(&old_param).unwrap();
                        (*new_param, arg)
                    })
                    .collect();

                replaced = true;
            }
        }

        if replaced {
            // after all the replacements, we need to update the uses of the old and
            // new blocks

            // remove this instruction from the users of the old block
            old.remove_user(ctx, self);
            // add this instruction to the users of the new block
            new.add_user(ctx, self);
        }
    }
}

impl User<Value> for Inst {
    fn all_uses(self, ctx: &Context) -> Vec<Value> {
        // in operands and successors
        let mut uses = self
            .deref(ctx)
            .operands
            .iter()
            .map(|opd| opd.inner())
            .collect::<Vec<_>>();

        for succ in self.deref(ctx).successors.iter() {
            uses.extend(succ.args.values().map(|arg| arg.inner()));
        }

        uses
    }

    fn replace(self, ctx: &mut Context, old: Value, new: Value) {
        if old.ty(ctx) != new.ty(ctx) {
            panic!("incompatible value type for replacement");
        }

        let mut replaced = false;

        for operand in self.deref_mut(ctx).operands.iter_mut() {
            if operand.set_inner_if_eq(old, new) {
                replaced = true;
            }
        }

        for succ in self.deref_mut(ctx).successors.iter_mut() {
            for arg in succ.args.values_mut() {
                if arg.set_inner_if_eq(old, new) {
                    replaced = true;
                }
            }
        }

        if replaced {
            // remove this instruction from the users of the old value
            old.remove_user(ctx, self);
            // add this instruction to the users of the new value
            new.add_user(ctx, self);
        }
    }
}

/// A temporary display instance.
///
/// This is used to make the arena pointer usable in [format!] macro.
pub struct DisplayInst<'a> {
    ctx: &'a Context,
    data: &'a InstData,
    debug: bool,
}

impl<'a> fmt::Display for DisplayInst<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use InstKind as Ik;

        let mut end_comments = Vec::new();
        let mut after_comments = Vec::new();

        // check comments in the context
        if let Some(comments) = self
            .ctx
            .comment_info
            .get_inst_comments(self.data.self_ptr())
        {
            for (pos, content) in comments {
                match pos {
                    CommentPos::Before => {
                        writeln!(f, "// {}", content)?;
                    }
                    CommentPos::AtEnd => {
                        end_comments.push(content);
                    }
                    CommentPos::After => {
                        after_comments.push(content);
                    }
                }
            }
        }

        // first, print the results
        // %v1, %v2, ... = ...
        // if debug mode is on, print the internal arena id of each values
        // %v1 /* 1 */, %v2 /* 2 */, ... = ...

        if self.data.results.is_empty() {
            if self.debug {
                write!(f, "/* no result */ ")?;
            }
        } else {
            for (i, result) in self.data.results.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "%{}", result.name(self.ctx).unwrap())?;
                if self.debug {
                    write!(f, " /* {} */", result.id())?;
                }
            }
            write!(f, " = ")?;
        }

        match &self.data.kind {
            Ik::IConst(constant) => {
                assert_eq!(self.data.results.len(), 1);
                write!(f, "iconst {:x}", constant)?;
            }
            Ik::FConst(constant) => {
                assert_eq!(self.data.results.len(), 1);
                write!(f, "fconst {:x}", constant)?;
            }
            Ik::StackSlot(size) => {
                assert_eq!(self.data.results.len(), 1);
                let result_ty = self.data.results[0].ty(self.ctx);
                assert!(result_ty.is_ptr(self.ctx));
                write!(f, "stack_slot {}", size)?;
            }
            Ik::IBinary(op) => {
                assert_eq!(self.data.results.len(), 1);
                assert_eq!(self.data.operands.len(), 2);
                write!(
                    f,
                    "{} %{}, %{}",
                    op,
                    self.data.operands[0].inner().name(self.ctx).unwrap(),
                    self.data.operands[1].inner().name(self.ctx).unwrap(),
                )?;
            }
            Ik::FBinary(op) => {
                assert_eq!(self.data.results.len(), 1);
                assert_eq!(self.data.operands.len(), 2);
                write!(
                    f,
                    "{} %{}, %{}",
                    op,
                    self.data.operands[0].inner().name(self.ctx).unwrap(),
                    self.data.operands[1].inner().name(self.ctx).unwrap(),
                )?;
            }
            Ik::IUnary(op) => {
                assert_eq!(self.data.results.len(), 1);
                assert_eq!(self.data.operands.len(), 1);

                write!(
                    f,
                    "{} %{}",
                    op,
                    self.data.operands[0].inner().name(self.ctx).unwrap(),
                )?;
            }
            Ik::FUnary(op) => {
                assert_eq!(self.data.results.len(), 1);
                assert_eq!(self.data.operands.len(), 1);
                write!(
                    f,
                    "{} %{}",
                    op,
                    self.data.operands[0].inner().name(self.ctx).unwrap(),
                )?;
            }
            Ik::Cast(op) => {
                assert_eq!(self.data.results.len(), 1);
                assert_eq!(self.data.operands.len(), 1);
                write!(
                    f,
                    "{} %{}",
                    op,
                    self.data.operands[0].inner().name(self.ctx).unwrap(),
                )?;
            }
            Ik::Offset => {
                assert_eq!(self.data.results.len(), 1);
                assert_eq!(self.data.operands.len(), 2);
                write!(
                    f,
                    "offset %{}, %{}",
                    self.data.operands[0].inner().name(self.ctx).unwrap(),
                    self.data.operands[1].inner().name(self.ctx).unwrap(),
                )?;
            }
            Ik::Jump => {
                assert_eq!(self.data.results.len(), 0);
                assert_eq!(self.data.operands.len(), 0);
                assert_eq!(self.data.successors.len(), 1);
                write!(f, "jump {}", self.data.successors[0].display(self.ctx))?;
            }
            Ik::Br => {
                assert_eq!(self.data.results.len(), 0);
                assert_eq!(self.data.operands.len(), 1);
                assert_eq!(self.data.successors.len(), 2);
                write!(
                    f,
                    "br %{}, {}, {}",
                    self.data.operands[0].inner().name(self.ctx).unwrap(),
                    self.data.successors[0].display(self.ctx),
                    self.data.successors[1].display(self.ctx)
                )?;
            }
            Ik::Switch { labels } => {
                // TODO: too many assertions, maybe an independent validation pass?
                assert_eq!(self.data.results.len(), 0);
                assert_eq!(self.data.operands.len(), 1);
                assert!(!self.data.successors.is_empty());
                assert!(
                    self.data.successors.len() == labels.len()
                        || self.data.successors.len() == labels.len() + 1
                );

                write!(
                    f,
                    "switch %{}, ",
                    self.data.operands[0].inner().name(self.ctx).unwrap()
                )?;
                for (i, (label, succ)) in labels.iter().zip(self.data.successors.iter()).enumerate()
                {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}: {}", label, succ.display(self.ctx))?;
                }

                if self.data.successors.len() == labels.len() + 1 {
                    write!(
                        f,
                        ", default: {}",
                        self.data.successors.last().unwrap().display(self.ctx)
                    )?;
                }
            }
            Ik::Call(callee) => {
                write!(f, "call @{}(", callee)?;
                for (i, arg) in self.data.operands.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "%{}", arg.inner().name(self.ctx).unwrap())?;
                }
                write!(f, ")")?;
            }
            Ik::CallIndirect(sig) => {
                assert!(!self.data.operands.is_empty());
                write!(
                    f,
                    "call_indirect {}, %{}(",
                    sig.display(self.ctx),
                    self.data.operands[0].inner().name(self.ctx).unwrap()
                )?;
                for (i, arg) in self.data.operands.iter().skip(1).enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "%{}", arg.inner().name(self.ctx).unwrap())?;
                }
                write!(f, ")")?;
            }
            Ik::Ret => {
                write!(f, "ret")?;

                if self.data.operands.is_empty() {
                    write!(f, " void")?;
                } else if self.data.operands.len() == 1 {
                    write!(
                        f,
                        " %{}",
                        self.data.operands[0].inner().name(self.ctx).unwrap()
                    )?;
                } else {
                    write!(f, " (")?;
                    for (i, opd) in self.data.operands.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "%{}", opd.inner().name(self.ctx).unwrap())?;
                    }
                    write!(f, ")")?;
                }
            }
            Ik::GetGlobal(symbol) => {
                write!(f, "get_global @{}", symbol)?;
            }
            Ik::Load => {
                assert_eq!(self.data.results.len(), 1);
                assert_eq!(self.data.operands.len(), 1);
                write!(
                    f,
                    "load %{}",
                    self.data.operands[0].inner().name(self.ctx).unwrap()
                )?;
            }
            Ik::Store => {
                assert_eq!(self.data.results.len(), 0);
                assert_eq!(self.data.operands.len(), 2);
                write!(
                    f,
                    "store %{}, %{}",
                    self.data.operands[0].inner().name(self.ctx).unwrap(),
                    self.data.operands[1].inner().name(self.ctx).unwrap()
                )?;
            }
        }

        // print the result types
        if !self.data.results.is_empty() {
            write!(f, " : ")?;
            if self.data.results.len() == 1 {
                write!(f, "{}", self.data.results[0].ty(self.ctx).display(self.ctx))?;
            } else {
                write!(f, "(")?;
                for (i, value) in self.data.results.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value.ty(self.ctx).display(self.ctx))?;
                }
                write!(f, ")")?;
            }
        }

        for comment in end_comments.iter() {
            write!(f, " /* {} */", comment)?;
        }

        if !end_comments.is_empty() {
            writeln!(f)?;
        }

        for comment in after_comments {
            writeln!(f, "// {}", comment)?;
        }

        Ok(())
    }
}
