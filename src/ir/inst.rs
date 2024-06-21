use core::fmt;
use std::{collections::HashMap, vec};

use super::{
    debug::CommentPos,
    Block,
    Constant,
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

/// Cast operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CastOp {
    /// Truncate.
    Trunc,
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
    /// This also applies to index and integer cast.
    Bitcast,
    /// Float extension.
    FpExt,
    /// Index to integer.
    IndexToInt,
    /// Integer to index.
    IntToIndex,
}

impl fmt::Display for CastOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Trunc => write!(f, "trunc"),
            Self::ZExt => write!(f, "zext"),
            Self::SExt => write!(f, "sext"),
            Self::FpToUi => write!(f, "fptoui"),
            Self::FpToSi => write!(f, "fptosi"),
            Self::UiToFp => write!(f, "uitofp"),
            Self::SiToFp => write!(f, "sitofp"),
            Self::Bitcast => write!(f, "bitcast"),
            Self::FpExt => write!(f, "fpext"),
            Self::IndexToInt => write!(f, "indextoint"),
            Self::IntToIndex => write!(f, "inttoindex"),
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

    pub fn add_arg(&mut self, param: Value, arg: Operand<Value>) { self.args.insert(param, arg); }
}

pub enum InstKind {
    /// Create a new value from a constant.
    ///
    /// Using an individual constant creation instruction is similar to MLIR and
    /// Cranelift IR.
    IConst(Constant),
    /// Create a new value from a constant.
    FConst(Constant),
    /// Create a new value as a stack slot.
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
    /// Jump instruction.
    Jump,
    /// Branch instruction.
    Br,
    /// Switch instruction.
    Switch { labels: Vec<Constant> },
    /// Call instruction.
    Call(Symbol),
    /// Call indirect instruction.
    CallIndirect(Signature),
    /// Return instruction.
    Return,
    /// Load instruction.
    ///
    /// The loaded type can be retrieved from the result value.
    Load,
    /// Store instruction.
    Store,
}

pub struct InstData {
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
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Inst(BaseArenaPtr<InstData>);

impl_arena!(Context, InstData, Inst, insts);

impl Inst {
    fn new(ctx: &mut Context, kind: InstKind, result_tys: Vec<Ty>, operands: Vec<Value>) -> Inst {
        let inst = ctx.alloc(InstData {
            results: Vec::new(),
            kind,

            operands: Vec::new(),
            successors: Vec::new(),

            next: None,
            prev: None,
            parent: None,
        });

        let operands = operands
            .into_iter()
            .map(|val| Operand::new(ctx, val, inst))
            .collect::<Vec<_>>();

        for (i, ty) in result_tys.into_iter().enumerate() {
            let value = ctx.alloc(ValueData::new_inst_result(ty, inst, i));
            inst.deref_mut(ctx).results.push(value);
        }

        inst.deref_mut(ctx).operands = operands;

        inst
    }

    /// Create a new iconst instruction.
    pub fn iconst(ctx: &mut Context, constant: impl Into<Constant>, ty: Ty) -> Inst {
        Self::new(ctx, InstKind::IConst(constant.into()), vec![ty], vec![])
    }

    /// Create a new fconst instruction.
    pub fn fconst(ctx: &mut Context, constant: impl Into<Constant>, ty: Ty) -> Inst {
        Self::new(ctx, InstKind::FConst(constant.into()), vec![ty], vec![])
    }

    /// Create a new stack slot instruction.
    pub fn stack_slot(ctx: &mut Context, size: u32) -> Inst {
        let ty = Ty::index(ctx);
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
                    panic!("fp_to_ui only supports float-like to integer-like types");
                }
            }
            CastOp::UiToFp | CastOp::SiToFp => {
                if !val_ty.is_integer(ctx) || !ty.is_float(ctx) {
                    panic!("ui_to_fp only supports integer-like to float-like types");
                }
            }
            CastOp::Bitcast => {
                if val_ty.is_index(ctx) || ty.is_index(ctx) {
                    panic!("bitcast does not support index types");
                }
                if val_ty.bitwidth(ctx).unwrap() != ty.bitwidth(ctx).unwrap() {
                    panic!("bitcast only supports types with the same size");
                }
            }
            CastOp::FpExt => {
                if !val_ty.is_float(ctx) || !ty.is_float(ctx) {
                    panic!("fp_ext only supports float-like types");
                }
                if val_ty.bitwidth(ctx).unwrap() >= ty.bitwidth(ctx).unwrap() {
                    panic!("fp_ext only supports extending to a larger type");
                }
            }
            CastOp::IndexToInt => {
                if !val_ty.is_index(ctx) || !ty.is_integer(ctx) {
                    panic!("index_to_int only supports index to integer-like types");
                }
            }
            CastOp::IntToIndex => {
                if !val_ty.is_integer(ctx) || !ty.is_index(ctx) {
                    panic!("int_to_index only supports integer-like to index types");
                }
            }
        }

        Self::new(ctx, InstKind::Cast(op), vec![ty], vec![val])
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
        mut branches: Vec<(Option<Constant>, Block, Vec<Value>)>,
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
                Some(constant) => {
                    if !constant.is_integer() {
                        panic!("switch label must be an integer constant");
                    }
                }
                None => {
                    if i != branches.len() - 1 {
                        panic!("default label must be the last branch");
                    }
                }
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
        if let Some(SymbolKind::FuncDef(func)) = ctx.lookup_symbol(symbol.clone()) {
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

        if func.ty(ctx) != Ty::index(ctx) {
            panic!("function must be an index type");
        }

        let mut operands = vec![func];
        operands.extend(args);

        Self::new(ctx, InstKind::CallIndirect(sig), result_tys, operands)
    }

    pub fn return_(ctx: &mut Context, vals: Vec<Value>) -> Inst {
        Self::new(ctx, InstKind::Return, vec![], vals)
    }

    pub fn load(ctx: &mut Context, ptr: Value, ty: Ty) -> Inst {
        if !ptr.ty(ctx).is_index(ctx) {
            panic!("ptr must be a pointer type");
        }
        Self::new(ctx, InstKind::Load, vec![ty], vec![ptr])
    }

    pub fn store(ctx: &mut Context, val: Value, ptr: Value) -> Inst {
        if !ptr.ty(ctx).is_index(ctx) {
            panic!("ptr must be a pointer type");
        }
        Self::new(ctx, InstKind::Store, vec![], vec![val, ptr])
    }

    pub fn is_terminator(self, ctx: &Context) -> bool {
        matches!(
            self.deref(ctx).kind,
            InstKind::Jump | InstKind::Br | InstKind::Switch { .. } | InstKind::Return
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

    pub fn comment(self, ctx: &mut Context, pos: CommentPos, content: String) {
        ctx.comment_info.comment_inst(self, pos, content);
    }

    pub fn result(self, ctx: &Context, idx: usize) -> Value { self.deref(ctx).results[idx] }
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
