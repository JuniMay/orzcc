use core::fmt;

use rustc_hash::FxHashMap;

use super::{
    constant::FloatConstant,
    debug::CommentPos,
    deep_clone::DeepCloneMap,
    source_loc::Span,
    Block,
    Context,
    IntConstant,
    Signature,
    Symbol,
    SymbolKind,
    Ty,
    Value,
    ValueData,
    ValueKind,
};
use crate::{
    collections::{
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
    /// Min signed
    Min,
    /// Max signed
    Max,
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
            Self::Min => write!(f, "min"),
            Self::Max => write!(f, "max"),
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
    pub(super) args: FxHashMap<Value, Operand<Value>>,
}

impl Successor {
    pub fn new(block: Operand<Block>) -> Self {
        Self {
            block,
            args: FxHashMap::default(),
        }
    }

    /// Add an argument mapping.
    pub fn add_arg(&mut self, param: Value, arg: Operand<Value>) { self.args.insert(param, arg); }

    pub fn display<'a>(&'a self, ctx: &'a Context) -> DisplaySuccessor<'a> {
        DisplaySuccessor { ctx, succ: self }
    }

    pub fn block(&self) -> Block { self.block.inner() }

    pub fn args(&self) -> &FxHashMap<Value, Operand<Value>> { &self.args }

    pub fn get_arg(&self, param: Value) -> Option<Value> {
        self.args.get(&param).map(|arg| arg.inner())
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
                    "/* %{} <- */ %{}",
                    param.name(self.ctx).unwrap(),
                    self.succ
                        .get_arg(*param)
                        .map(|arg| arg.name(self.ctx).unwrap())
                        .unwrap_or(&"???".to_string())
                )?;
            }

            write!(f, ")")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub enum InstKind {
    Undef,
    /// Create a new value from a constant.
    ///
    /// Using an individual constant creation instruction is similar to MLIR and
    /// Cranelift IR.
    IConst(IntConstant),
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
    /// the offset independent of the base pointer.
    Offset,
    /// Jump instruction.
    Jump,
    /// Branch instruction.
    Br,
    /// Call instruction.
    Call(Symbol),
    /// Call indirect instruction.
    ///
    /// The first operand is the function pointer, and the rest are arguments.
    CallIndirect(Signature),
    /// Return instruction.
    Ret,
    /// Get a global symbol as a pointer.
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
    /// High level store element representation.
    ///
    /// `store_elem [_, dim1, dim2, ...], %val, %addr[idx0, idx1, idx2, ...]`
    StoreElem {
        /// The shape of the memory region
        shape: Vec<u32>,
    },
    /// High level load element representation.
    ///
    /// `load_elem [_, dim1, dim2, ...], %addr[idx0, idx1, idx2, ...]: ty`
    LoadElem {
        /// The shape of the memory region
        shape: Vec<u32>,
    },
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

    /// The span of this instruction.
    ///
    /// This covers from the start of the instruction to the end of the
    /// instruction.
    source_span: Span,
}

impl InstData {
    pub fn self_ptr(&self) -> Inst { self.self_ptr }
}

impl PartialEq for InstData {
    fn eq(&self, other: &Self) -> bool {
        self.results.is_empty()
            && other.results.is_empty()
            && self.kind == other.kind
            && self
                .operands
                .iter()
                .map(|op| op.inner())
                .eq(other.operands.iter().map(|op| op.inner()))
            && self
                .successors
                .iter()
                .map(|succ| succ.block())
                .eq(other.successors.iter().map(|succ| succ.block()))
            // TODO: this is conservative.
            && self.successors.iter().all(|succ| succ.args().is_empty())
            && other.successors.iter().all(|succ| succ.args().is_empty())
    }
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Inst(BaseArenaPtr<InstData>);

impl_arena!(Context, InstData, Inst, insts);

impl Inst {
    /// Create an instruction directly.
    ///
    /// This is a low-level function and should be **USED WITH CAUTION**.
    pub(crate) fn new(
        ctx: &mut Context,
        kind: InstKind,
        result_tys: Vec<Ty>,
        operands: Vec<Value>,
    ) -> Inst {
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

        if result_tys.len() == 1 && result_tys[0].is_void(ctx) {
            // not creating any result for void type
        } else {
            for (i, ty) in result_tys.into_iter().enumerate() {
                if ty.is_void(ctx) {
                    panic!("multiple results must not contain void type");
                }
                let value =
                    ctx.alloc_with(|self_ptr| ValueData::new_inst_result(self_ptr, ty, inst, i));
                inst.deref_mut(ctx).results.push(value);
            }
        }

        inst.deref_mut(ctx).operands = operands;
        inst
    }

    /// Add successor to the instruction.
    ///
    /// This is a low-level function and should be **USED WITH CAUTION**.
    pub(crate) fn add_successor(&self, ctx: &mut Context, succ: Successor) {
        self.deref_mut(ctx).successors.push(succ);
    }

    pub fn set_source_span(&self, ctx: &mut Context, span: impl Into<Span>) {
        self.deref_mut(ctx).source_span = span.into();
    }

    pub fn source_span(self, ctx: &Context) -> Span { self.deref(ctx).source_span }

    /// Create a new undef instruction.
    pub fn undef(ctx: &mut Context, ty: Ty) -> Inst {
        Self::new(ctx, InstKind::Undef, vec![ty], vec![])
    }

    /// Create a new iconst instruction.
    ///
    /// If the width of the constant does not match the width of the type,
    /// [IntConstant::resize] will be called to resize the constant.
    pub fn iconst(ctx: &mut Context, constant: impl Into<IntConstant>, ty: Ty) -> Inst {
        let width = ty.bitwidth(ctx);
        let constant: IntConstant = constant.into();
        let constant = constant.resize(width as u8);
        Self::new(
            ctx,
            // resize the constant to the width of the type
            InstKind::IConst(constant),
            vec![ty],
            vec![],
        )
    }

    /// Create a new fconst instruction for float32
    ///
    /// If the type is float64, and the constant is float32, the constant will
    /// be promoted to float64.
    ///
    /// # Panics
    ///
    /// Panics if the constant is a float64 and the type is float32.
    pub fn fconst(ctx: &mut Context, constant: impl Into<FloatConstant>, ty: Ty) -> Inst {
        let mut constant: FloatConstant = constant.into();

        if ty.is_float32(ctx) {
            if let FloatConstant::Float64(_) = constant {
                panic!("fconst: expected f32, got f64");
            }
        } else if let FloatConstant::Float32(_) = constant {
            constant = constant.promote();
        }

        Self::new(ctx, InstKind::FConst(constant), vec![ty], vec![])
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
            panic!(
                "lhs and rhs must be integer-like types, got {} and {}",
                lhs.ty(ctx).display(ctx),
                rhs.ty(ctx).display(ctx)
            );
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
            | Op::Max
            | Op::Min
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
                if val_ty.bitwidth(ctx) <= ty.bitwidth(ctx) {
                    panic!("trunc only supports truncating to a smaller type");
                }
            }
            CastOp::ZExt | CastOp::SExt => {
                if !val_ty.is_integer(ctx) || !ty.is_integer(ctx) {
                    panic!("zext only supports integer-like types");
                }
                if val_ty.bitwidth(ctx) >= ty.bitwidth(ctx) {
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
                if val_ty.bitwidth(ctx) != ty.bitwidth(ctx) {
                    panic!("bitcast only supports types with the same size");
                }
            }
            CastOp::FpExt => {
                if !val_ty.is_float(ctx) || !ty.is_float(ctx) {
                    panic!("fpext only supports float-like types");
                }
                if val_ty.bitwidth(ctx) >= ty.bitwidth(ctx) {
                    panic!("fpext only supports extending to a larger type");
                }
            }
            CastOp::FpTrunc => {
                if !val_ty.is_float(ctx) || !ty.is_float(ctx) {
                    panic!("fptrunc only supports float-like types");
                }
                if val_ty.bitwidth(ctx) <= ty.bitwidth(ctx) {
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
                panic!(
                    "types of arguments and parameters not match, arg: {}, param (expected): {}",
                    arg.ty(ctx).display(ctx),
                    param.ty(ctx).display(ctx)
                );
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

    pub fn call(
        ctx: &mut Context,
        symbol: impl Into<Symbol>,
        args: Vec<Value>,
        result_tys: Vec<Ty>,
    ) -> Inst {
        let symbol = symbol.into();
        let sig = if let Some(SymbolKind::FuncDef(func)) = ctx.lookup_symbol(&symbol) {
            func.sig(ctx)
        } else if let Some(SymbolKind::FuncDecl(sig)) = ctx.lookup_symbol(&symbol) {
            sig
        } else {
            panic!("symbol is not valid");
        };

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

    pub fn load_elem(
        ctx: &mut Context,
        shape: Vec<u32>,
        addr: Value,
        indices: Vec<Value>,
        ty: Ty,
    ) -> Inst {
        if !addr.ty(ctx).is_ptr(ctx) {
            panic!("addr must be a pointer type");
        }

        assert_eq!(shape.len() + 1, indices.len());

        let mut operands = vec![addr];
        operands.extend(indices);

        Self::new(ctx, InstKind::LoadElem { shape }, vec![ty], operands)
    }

    pub fn store_elem(
        ctx: &mut Context,
        shape: Vec<u32>,
        val: Value,
        addr: Value,
        indices: Vec<Value>,
    ) -> Inst {
        if !addr.ty(ctx).is_ptr(ctx) {
            panic!("addr must be a pointer type");
        }

        assert_eq!(shape.len() + 1, indices.len());

        let mut operands = vec![val, addr];
        operands.extend(indices);

        Self::new(ctx, InstKind::StoreElem { shape }, vec![], operands)
    }

    pub fn get_global(ctx: &mut Context, symbol: impl Into<Symbol>) -> Inst {
        let ptr = Ty::ptr(ctx);
        Self::new(ctx, InstKind::GetGlobal(symbol.into()), vec![ptr], vec![])
    }

    pub fn is_terminator(self, ctx: &Context) -> bool {
        matches!(
            self.deref(ctx).kind,
            InstKind::Jump | InstKind::Br | InstKind::Ret
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

    /// Add an argument to the successor block of the branch instruction.
    ///
    /// # Parameters
    ///
    /// - `ctx`: the context
    /// - `block`: the block to pass the argument to
    /// - `param`: the block parameter to pass the argument to
    /// - `arg`: the argument to pass
    ///
    /// # Panics
    ///
    /// Panics if the instruction is not a branch instruction. Check with
    /// [Inst::is_terminator] before calling this method.
    pub fn add_succ_arg(self, ctx: &mut Context, block: Block, param: Value, arg: Value) {
        if !self.is_terminator(ctx) {
            panic!("instruction is not a terminator");
        }

        let mut count = 0;
        for succ in self.deref(ctx).successors.iter() {
            if succ.block.inner() == block {
                count += 1; // borrow checker! we cannot create operands here
            }
        }
        let mut operands = Vec::new();
        for _ in 0..count {
            let arg = Operand::new(ctx, arg, self);
            operands.push(arg);
        }
        for succ in self.deref_mut(ctx).successors.iter_mut() {
            if succ.block.inner() == block {
                succ.add_arg(param, operands.pop().unwrap());
            }
        }
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

    /// Get all the results of this instruction,
    pub fn results(self, ctx: &Context) -> &[Value] { &self.deref(ctx).results }

    /// Comment at this instruction.
    pub fn comment(self, ctx: &mut Context, pos: CommentPos, content: impl Into<String>) {
        ctx.comment_info.comment_inst(self, pos, content.into());
    }

    /// Get the result at the given index.
    pub fn result(self, ctx: &Context, idx: usize) -> Value { self.deref(ctx).results[idx] }

    /// Get the operand as value at the given index.
    pub fn operand(self, ctx: &Context, idx: usize) -> Value {
        self.deref(ctx).operands[idx].inner()
    }

    /// Get all the operands.
    ///
    /// Note that this does not include the block arguments.
    pub fn operands(self, ctx: &Context) -> Vec<Value> {
        self.deref(ctx)
            .operands
            .iter()
            .map(|opd| opd.inner())
            .collect()
    }

    /// Get the successor at the given index.
    pub fn succ(self, ctx: &Context, idx: usize) -> &Successor { &self.deref(ctx).successors[idx] }

    pub fn succs(self, ctx: &Context) -> &[Successor] { &self.deref(ctx).successors }

    /// Get the number of successors that goes to the given block.
    ///
    /// The [CfgInfo](crate::utils::cfg::CfgInfo) does not model the
    /// control-flow-dependent argument passing. When simplifying CFG, we need
    /// to know how many successors goes to a certain successor, and decide if
    /// two blocks can be merged. For example, the IR below cannot be simplified
    /// even though the two blocks has only one pred/succ.
    ///
    /// ```orzir
    /// ^bb0:
    ///     ...
    ///     br %cond, ^bb1(%0), ^bb1(%1)
    /// ^bb1: // only one pred `^bb0` in `CfgInfo`
    ///     ...
    /// ```
    ///
    /// Note that sometimes a branch instruction might pass the same argument to
    /// the same block multiple times. That kinds of branch instruction can be
    /// simplified. But here we only consider the number of successors that goes
    /// to the given block, which is a conservative approach.
    ///
    /// TODO: simplify branch instruction with the same argument passing to the
    /// same block multiple times.
    ///
    /// # Parameters
    ///
    /// - `ctx`: the context
    /// - `block`: the target block
    ///
    /// # Returns
    ///
    /// The number of successors that goes to the given block.
    pub fn num_succ_to(self, ctx: &Context, block: Block) -> usize {
        self.deref(ctx)
            .successors
            .iter()
            .filter(|s| s.block.inner() == block)
            .count()
    }

    pub fn succ_to(self, ctx: &Context, block: Block) -> impl Iterator<Item = &Successor> {
        self.deref(ctx)
            .successors
            .iter()
            .filter(move |s| s.block.inner() == block)
    }

    /// Remove all the arguments passed to a certain block parameter.
    ///
    /// Used in aggressive DCE to remove dead block parameters
    pub fn remove_args_passing_to_param(self, ctx: &mut Context, param: Value) {
        let mut args_to_drop = Vec::new();
        for succ in self.deref_mut(ctx).successors.iter_mut() {
            let mut new_args = FxHashMap::default();
            for (p, arg) in succ.args.drain() {
                if p != param {
                    new_args.insert(p, arg);
                } else {
                    args_to_drop.push(arg);
                }
            }
            succ.args = new_args;
        }

        for arg in args_to_drop {
            // drop the use of the arguments
            arg.drop(ctx);
        }
    }

    pub fn replace_succ_with_args(
        self,
        ctx: &mut Context,
        old: Block,
        new: Block,
        args: Vec<Value>,
    ) {
        if !self.is_terminator(ctx) {
            panic!("instruction is not a terminator");
        }

        if !old.params(ctx).is_empty() {
            // remove all the previously passed arguments.
            let params = old.params(ctx).to_vec();
            for param in params {
                self.remove_args_passing_to_param(ctx, param);
            }
        }

        let mut num_blocks_to_replace = 0;

        for succ in self.deref(ctx).successors.iter() {
            if succ.block.inner() == old {
                num_blocks_to_replace += 1;
            }
        }

        let params = new.params(ctx).to_vec();

        // we need to create num_block_to_replace * len(args) operands, which is vector
        // of vector of operands
        let mut args = (0..num_blocks_to_replace)
            .map(|_| {
                args.iter()
                    .zip(params.iter())
                    .map(|(arg, param)| (*param, Operand::new(ctx, *arg, self)))
                    .collect::<FxHashMap<_, _>>()
            })
            .collect::<Vec<_>>();

        for succ in self.deref_mut(ctx).successors.iter_mut() {
            if succ.block.set_inner_if_eq(old, new) {
                // because we asserted that the old block has no parameters, we
                // can just drop the old vec
                succ.args = args.pop().unwrap();
            }
        }

        for _ in 0..num_blocks_to_replace {
            old.remove_user(ctx, self);
            new.add_user(ctx, self);
        }
    }

    /// Replace all the arguments passed to the `param` with the `arg`.
    pub fn replace_args(self, ctx: &mut Context, param: Value, arg: Value) {
        let mut operands_to_drop = Vec::new();

        let mut param_count = 0;
        for succ in self.deref_mut(ctx).successors.iter_mut() {
            if succ.args.contains_key(&param) {
                param_count += 1;
            }
        }

        let mut new_arg_opds = Vec::new();
        for _ in 0..param_count {
            new_arg_opds.push(Operand::new(ctx, arg, self));
        }

        for succ in self.deref_mut(ctx).successors.iter_mut() {
            let old = succ
                .args
                .insert(param, new_arg_opds.pop().unwrap())
                .unwrap();
            operands_to_drop.push(old);
        }

        for opd in operands_to_drop {
            opd.drop(ctx);
        }
    }

    /// Get the kind of the instruction.
    pub fn kind(self, ctx: &Context) -> &InstKind { &self.deref(ctx).kind }

    pub fn display(self, ctx: &Context, debug: bool) -> DisplayInst<'_> {
        DisplayInst {
            ctx,
            data: self.deref(ctx),
            debug,
        }
    }

    pub fn deep_clone(self, ctx: &mut Context, map: &mut DeepCloneMap) -> Inst {
        let kind = self.kind(ctx).clone();

        let opds = self
            .operands(ctx)
            .into_iter()
            .map(|opd| map.get_value_or_old(opd))
            .collect::<Vec<_>>();

        let result_tys = self
            .results(ctx)
            .iter()
            .map(|r| r.ty(ctx))
            .collect::<Vec<_>>();

        let inst = Self::new(ctx, kind, result_tys, opds);

        // succs
        let blocks = self
            .deref(ctx)
            .successors
            .iter()
            .map(|succ| {
                let old_block = succ.block();
                let new_block = map.get_block_or_old(old_block);

                let args = old_block
                    .params(ctx)
                    .iter()
                    .zip(new_block.params(ctx).iter())
                    .map(|(old_param, new_param)| {
                        let old_arg = succ.get_arg(*old_param).unwrap();
                        let new_arg = map.get_value_or_old(old_arg);
                        (*new_param, new_arg)
                    })
                    .collect::<FxHashMap<_, _>>();

                (new_block, args)
            })
            .collect::<Vec<_>>();

        for (block, args) in blocks {
            let mut new_succ = Successor::new(Operand::new(ctx, block, inst));
            for (param, arg) in args {
                new_succ.add_arg(param, Operand::new(ctx, arg, inst));
            }
            inst.add_successor(ctx, new_succ);
        }

        for (old_result, new_result) in self.results(ctx).iter().zip(inst.results(ctx).iter()) {
            map.insert_value(*old_result, *new_result);
        }

        inst
    }

    pub fn is_stack_slot(self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).kind, InstKind::StackSlot(_))
    }

    pub fn is_iconst(self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).kind, InstKind::IConst(_))
    }

    pub fn is_fconst(self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).kind, InstKind::FConst(_))
    }

    pub fn is_undef(self, ctx: &Context) -> bool { matches!(self.deref(ctx).kind, InstKind::Undef) }

    pub fn is_ibinary(self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).kind, InstKind::IBinary(_))
    }

    pub fn is_fbinary(self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).kind, InstKind::FBinary(_))
    }

    pub fn is_iunary(self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).kind, InstKind::IUnary(_))
    }

    pub fn is_funary(self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).kind, InstKind::FUnary(_))
    }

    pub fn is_cast(self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).kind, InstKind::Cast(_))
    }

    pub fn is_offset(self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).kind, InstKind::Offset)
    }

    pub fn is_jump(self, ctx: &Context) -> bool { matches!(self.deref(ctx).kind, InstKind::Jump) }

    pub fn is_br(self, ctx: &Context) -> bool { matches!(self.deref(ctx).kind, InstKind::Br) }

    pub fn is_call(self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).kind, InstKind::Call(_))
    }

    pub fn is_call_indirect(self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).kind, InstKind::CallIndirect(_))
    }

    pub fn is_ret(self, ctx: &Context) -> bool { matches!(self.deref(ctx).kind, InstKind::Ret) }

    pub fn is_get_global(self, ctx: &Context) -> bool {
        matches!(self.deref(ctx).kind, InstKind::GetGlobal(_))
    }

    pub fn is_load(self, ctx: &Context) -> bool { matches!(self.deref(ctx).kind, InstKind::Load) }

    pub fn is_store(self, ctx: &Context) -> bool { matches!(self.deref(ctx).kind, InstKind::Store) }

    pub fn is_commutative(self, ctx: &Context) -> bool {
        use InstKind as Ik;

        matches!(
            self.kind(ctx),
            Ik::IBinary(IBinaryOp::Add)
                | Ik::IBinary(IBinaryOp::Mul)
                | Ik::IBinary(IBinaryOp::And)
                | Ik::IBinary(IBinaryOp::Or)
                | Ik::IBinary(IBinaryOp::Xor)
                | Ik::FBinary(FBinaryOp::Add)
                | Ik::FBinary(FBinaryOp::Mul)
        )
    }

    /// AGGRESSIVE: FBinary associations can result in precision and rounding
    /// errors
    pub fn is_associative(self, ctx: &Context) -> bool {
        use InstKind as Ik;

        matches!(
            self.kind(ctx),
            Ik::IBinary(IBinaryOp::Add)
                | Ik::IBinary(IBinaryOp::Mul)
                | Ik::IBinary(IBinaryOp::And)
                | Ik::IBinary(IBinaryOp::Or)
                | Ik::IBinary(IBinaryOp::Xor)
                | Ik::IBinary(IBinaryOp::Min) // TODO: aggressive option
                | Ik::IBinary(IBinaryOp::Max) /* | Ik::FBinary(FBinaryOp::Add)  // aggressive
                                               * | Ik::FBinary(FBinaryOp::Mul) // aggressive */
        )
    }

    pub fn commute_operands(self, ctx: &mut Context) {
        if !self.is_commutative(ctx) {
            panic!("instruction is not commutative");
        }

        let operands = self.deref(ctx).operands.len();
        if operands != 2 {
            panic!("commutative instruction must have 2 operands");
        }

        self.deref_mut(ctx).operands.swap(0, 1);
    }

    pub fn is_used(self, ctx: &Context) -> bool {
        let mut is_used = false;
        for result in self.results(ctx) {
            if !result.users(ctx).is_empty() {
                is_used = true;
                break;
            }
        }
        is_used
    }

    pub fn get_iconst_value(self, ctx: &Context) -> Option<IntConstant> {
        if let InstKind::IConst(value) = self.kind(ctx) {
            Some(*value)
        } else {
            None
        }
    }

    pub fn inverse_br(self, ctx: &mut Context) {
        if !self.is_br(ctx) {
            panic!("instruction is not a branch instruction");
        }

        self.deref_mut(ctx).successors.swap(0, 1);
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
            .collect::<FxHashMap<_, _>>();

        let mut num_replaced = 0;

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

                num_replaced += 1;
            }
        }

        for _ in 0..num_replaced {
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

        let mut num_replaced = 0;

        for operand in self.deref_mut(ctx).operands.iter_mut() {
            if operand.set_inner_if_eq(old, new) {
                num_replaced += 1;
            }
        }

        for succ in self.deref_mut(ctx).successors.iter_mut() {
            for arg in succ.args.values_mut() {
                if arg.set_inner_if_eq(old, new) {
                    num_replaced += 1;
                }
            }
        }

        for _ in 0..num_replaced {
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
                    write!(
                        f,
                        " /* {}, uses: {} */",
                        result.id(),
                        result.total_uses(self.ctx)
                    )?;
                }
            }
            write!(f, " = ")?;
        }

        match &self.data.kind {
            Ik::Undef => {
                assert_eq!(self.data.results.len(), 1);
                assert_eq!(self.data.operands.len(), 0);
                write!(f, "undef")?;
            }
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
            Ik::StoreElem { shape } => {
                write!(f, "store_elem [_")?;
                for dim in shape.iter() {
                    write!(f, ", {}", dim)?;
                }
                write!(f, "]")?;

                write!(
                    f,
                    ", %{}",
                    self.data.operands[0].inner().name(self.ctx).unwrap()
                )?; // val
                write!(
                    f,
                    ", %{}[",
                    self.data.operands[1].inner().name(self.ctx).unwrap()
                )?; // ptr

                for (i, idx) in self.data.operands.iter().skip(2).enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "%{}", idx.inner().name(self.ctx).unwrap())?;
                }

                write!(f, "]")?;
            }
            Ik::LoadElem { shape } => {
                write!(f, "load_elem [_")?;
                for dim in shape.iter() {
                    write!(f, ", {}", dim)?;
                }
                write!(f, "]")?;

                write!(
                    f,
                    ", %{}[",
                    self.data.operands[0].inner().name(self.ctx).unwrap()
                )?; // ptr

                for (i, idx) in self.data.operands.iter().skip(1).enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "%{}", idx.inner().name(self.ctx).unwrap())?;
                }

                write!(f, "]")?;
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

/// Remove all the given instructions.
///
/// The instructions might have complicated def-use chains, we need to remove
/// those with no users first, i.e., a topological order.
///
/// # Parameters
///
/// - `ctx`: the context.
/// - `insts`: the instructions to remove.
/// - `best_effort`: if `true`, this function will try to remove as many
///   instructions as possible without panicking. If `false`, this function will
///   panic if it cannot remove all the instructions.
///
/// # Panics
///
/// Panics only if `best_effort` is `false`:
///
/// - Panics if the result is used by other instructions.
/// - Panics if there are cycles (which is a bug).
pub fn remove_all_insts(ctx: &mut Context, insts: Vec<Inst>, best_effort: bool) {
    let mut inst_set = insts
        .into_iter()
        .map(|i| {
            let num_users: usize = i
                .results(ctx)
                .iter()
                .copied()
                // if one instruction uses one value multiple times, `users` will only count once.
                .map(|v| v.total_uses(ctx))
                .sum();
            (i, num_users)
        })
        .collect::<FxHashMap<_, _>>();

    let mut worklist = inst_set
        .iter()
        .filter(|(_, num_users)| **num_users == 0)
        .map(|(inst, _)| *inst)
        .collect::<Vec<_>>();

    while let Some(inst) = worklist.pop() {
        for opd in inst.operands(ctx) {
            match opd.kind(ctx) {
                ValueKind::BlockParam { .. } => {
                    // block parameter will not be removed
                }
                ValueKind::InstResult { inst, .. } => {
                    // only update those in the inst_set
                    if let Some(num_users) = inst_set.get_mut(inst) {
                        *num_users -= 1;
                        if *num_users == 0 {
                            worklist.push(*inst);
                        }
                    }
                }
            }
        }

        // operands and successors are mutually exclusive, so we need to check both
        for succ in inst.deref(ctx).successors.iter() {
            for (_, arg) in succ.args().iter() {
                match arg.inner().kind(ctx) {
                    ValueKind::BlockParam { .. } => {
                        // block parameter will not be removed
                    }
                    ValueKind::InstResult { inst, .. } => {
                        // only update those in the inst_set
                        if let Some(num_users) = inst_set.get_mut(inst) {
                            *num_users -= 1;
                            if *num_users == 0 {
                                worklist.push(*inst);
                            }
                        }
                    }
                }
            }
        }

        // the instruction is not used by any other instructions, remove it
        inst.remove(ctx);
    }

    // check if there are cycles, or used by other instructions
    if !inst_set.is_empty() && !best_effort {
        let mut panic = false;
        for (inst, num_users) in inst_set {
            if num_users > 0 {
                eprintln!(
                    "inst: {}, num_users: {}",
                    inst.display(ctx, true),
                    num_users
                );
                panic = true;
            }
        }
        if panic {
            panic!("remove_insts with best_effort=false: there are cycles or used by other instructions.");
        }
    }
}
