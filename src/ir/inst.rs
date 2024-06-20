use std::collections::HashMap;

use super::{debug::CommentPos, Block, Constant, Context, Signature, Symbol, Ty, Value, ValueData};
use crate::{
    collections::{
        linked_list::LinkedListNodePtr,
        storage::{ArenaAlloc, ArenaFree, ArenaPtr, BaseArenaPtr},
    },
    impl_arena,
    utils::def_use::{Usable, User},
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

/// Integer unary operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IUnaryOp {
    /// Bitwise not.
    Not,
}

/// Floating-point unary operation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FUnaryOp {
    /// Negation.
    Neg,
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
}

/// Successor of a branch instruction.
///
/// Successor describes the destination block and the argument passing.
pub struct Successor {
    /// The destination block.
    pub(super) block: Block,
    /// Argument mapping.
    ///
    /// This represents the parameter to argument mapping, the keys represent
    /// parameters, and the values represent arguments.
    pub(super) args: HashMap<Value, Value>,
}

impl Successor {
    pub fn new(block: Block) -> Self {
        Self {
            block,
            args: HashMap::new(),
        }
    }

    pub fn add_arg(&mut self, param: Value, arg: Value) { self.args.insert(param, arg); }
}

pub enum InstKind {
    /// Create a new value from a constant.
    ///
    /// Using an individual constant creation instruction is similar to MLIR and
    /// Cranelift IR.
    IConst {
        /// The constant value, expect to be [Constant::Integer]
        constant: Constant,
    },
    /// Create a new value from a constant.
    FConst {
        /// The constant value, expect to be [Constant::Float32] or
        /// [Constant::Float64]
        constant: Constant,
    },
    /// Create a new value as a stack slot.
    StackSlot {
        /// The size of the stack slot.
        ///
        /// We don't represent the slot with type, but with size, as the size
        /// is more important than the type in the stack allocation. The type
        /// can be decided when loading or storing the value.
        size: u32,
    },
    /// Integer binary instruction.
    IBinary {
        /// The opcode of the binary operation.
        op: IBinaryOp,
        /// The left-hand side value.
        lhs: Value,
        /// The right-hand side value.
        rhs: Value,
    },
    /// Floating-point binary instruction.
    FBinary {
        /// The opcode of the binary operation.
        op: FBinaryOp,
        /// The left-hand side value.
        lhs: Value,
        /// The right-hand side value.
        rhs: Value,
    },
    /// Integer unary instruction.
    IUnary {
        /// The opcode of the unary operation.
        op: IUnaryOp,
        /// The value to apply the operation.
        val: Value,
    },
    /// Floating-point unary instruction.
    FUnary {
        /// The opcode of the unary operation.
        op: FUnaryOp,
        /// The value to apply the operation.
        val: Value,
    },
    /// The cast instruction.
    ///
    /// The type of the destination value is determined by the type of the
    /// result value.
    Cast {
        /// The opcode of the cast operation.
        op: CastOp,
        /// The value to cast.
        val: Value,
    },
    /// Branch instruction.
    ///
    /// This instruction unifies the jump, br, and even switch.
    ///
    /// # Notes
    ///
    /// The `br` mostly jumps to the first successor, when the condition is
    /// true. However, when using this instruction, the first successor
    /// corresponds to the false branch (cond is `0`).
    Branch {
        /// The condition of the branch.
        ///
        /// The bit width of condition value must be equal or greater than
        /// the number of successors.
        cond: Option<Value>,
        /// The successors of the branch.
        succs: Vec<Successor>,
        /// The default successor.
        default: Option<Successor>,
    },
    /// Call instruction.
    Call {
        /// The callee symbol.
        ///
        /// This symbol can be used to lookup in the [Context] using
        /// [Context::lookup_symbol].
        callee: Symbol,
        /// The arguments to the callee.
        args: Vec<Value>,
    },
    /// Call indirect instruction.
    CallIndirect {
        /// The signature of the callee.
        sig: Signature,
        /// The callee value, should have a type of
        /// [TyData::Index](crate::ir::TyData::Index).
        callee: Value,
        /// The arguments to the callee.
        args: Vec<Value>,
    },
    /// Return instruction.
    Return {
        /// The optional return value.
        val: Option<Value>,
    },
    /// Load instruction.
    Load {
        /// The type of the loaded value.
        ty: Ty,
        /// The memory to load from.
        ///
        /// This value should have a [TyData::Index](crate::ir::TyData::Index)
        /// type.
        mem: Value,
    },
    /// Store instruction.
    Store {
        /// The value to store.
        val: Value,
        /// The memory to store to.
        mem: Value,
    },
}

pub struct InstData {
    /// Result of this instruction.
    results: Vec<Value>,
    /// The instruction kind.
    pub(super) kind: InstKind,
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
    pub fn new(ctx: &mut Context, kind: InstKind, result_tys: Vec<Ty>) -> Inst {
        let inst = ctx.alloc(InstData {
            results: Vec::new(),
            kind,
            next: None,
            prev: None,
            parent: None,
        });

        for (i, ty) in result_tys.into_iter().enumerate() {
            let value = ctx.alloc(ValueData::new_inst_result(ty, inst, i));
            inst.deref_mut(ctx).results.push(value);
        }

        inst
    }

    /// Create a new iconst instruction.
    pub fn iconst(ctx: &mut Context, constant: impl Into<Constant>, ty: Ty) -> Inst {
        Self::new(
            ctx,
            InstKind::IConst {
                constant: constant.into(),
            },
            vec![ty],
        )
    }

    /// Create a new fconst instruction.
    pub fn fconst(ctx: &mut Context, constant: impl Into<Constant>, ty: Ty) -> Inst {
        Self::new(
            ctx,
            InstKind::FConst {
                constant: constant.into(),
            },
            vec![ty],
        )
    }

    /// Create a new stack slot instruction.
    pub fn stack_slot(ctx: &mut Context, size: u32) -> Inst {
        let ty = Ty::index(ctx);
        Self::new(ctx, InstKind::StackSlot { size }, vec![ty])
    }

    pub fn ibinary(ctx: &mut Context, op: IBinaryOp, lhs: Value, rhs: Value) -> Inst {
        use IBinaryOp as Op;

        if !lhs.ty(ctx).is_integer_like(ctx) || !rhs.ty(ctx).is_integer_like(ctx) {
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

        Self::new(ctx, InstKind::IBinary { op, lhs, rhs }, vec![ty])
    }

    pub fn fbinary(ctx: &mut Context, op: FBinaryOp, lhs: Value, rhs: Value) -> Inst {
        use FBinaryOp as Op;

        if !lhs.ty(ctx).is_float_like(ctx) || !rhs.ty(ctx).is_float_like(ctx) {
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

        Self::new(ctx, InstKind::FBinary { op, lhs, rhs }, vec![ty])
    }

    pub fn iunary(ctx: &mut Context, op: IUnaryOp, val: Value) -> Inst {
        use IUnaryOp as Op;

        if !val.ty(ctx).is_integer_like(ctx) {
            // iunary only supports integer-like types
            panic!("val must be an integer-like type");
        }

        let ty = match op {
            Op::Not => val.ty(ctx),
        };

        Self::new(ctx, InstKind::IUnary { op, val }, vec![ty])
    }

    pub fn funary(ctx: &mut Context, op: FUnaryOp, val: Value) -> Inst {
        use FUnaryOp as Op;

        if !val.ty(ctx).is_float_like(ctx) {
            // funary only supports float-like types
            panic!("val must be a float-like type");
        }

        let ty = match op {
            Op::Neg => val.ty(ctx),
        };

        Self::new(ctx, InstKind::FUnary { op, val }, vec![ty])
    }

    pub fn cast(ctx: &mut Context, op: CastOp, val: Value, ty: Ty) -> Inst {
        // TODO check if the cast is valid
        Self::new(ctx, InstKind::Cast { op, val }, vec![ty])
    }

    pub fn is_terminator(self, ctx: &Context) -> bool {
        matches!(
            self.deref(ctx).kind,
            InstKind::Branch { .. } | InstKind::Return { .. }
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
        if let InstKind::Branch {
            ref succs,
            ref default,
            ..
        } = self.deref(ctx).kind
        {
            let mut blocks = succs.iter().map(|s| s.block).collect::<Vec<_>>();
            if let Some(default) = default {
                blocks.push(default.block);
            }
            blocks
        } else if let InstKind::Return { .. } = self.deref(ctx).kind {
            // return instruction has no successors
            Vec::new()
        } else {
            panic!("not a branch instruction")
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
        for block in User::<Block>::all_uses(self, ctx) {
            block.remove_user(ctx, self);
        }
        // update the uses of operands
        for operand in User::<Value>::all_uses(self, ctx) {
            operand.remove_user(ctx, self);
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
        // only append the block if this is a branch instruction
        let mut uses = vec![];
        if let InstKind::Branch {
            ref succs,
            ref default,
            ..
        } = self.deref(ctx).kind
        {
            uses.extend(succs.iter().map(|s| s.block));
            if let Some(default) = default {
                uses.push(default.block);
            }
        }
        uses
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
        if let InstKind::Branch {
            ref mut succs,
            ref mut default,
            ..
        } = self.deref_mut(ctx).kind
        {
            for succ in succs {
                if succ.block == old {
                    succ.block = new;
                    succ.args = succ
                        .args
                        .iter()
                        .map(|(old_param, arg)| {
                            let new_param = param_mapping.get(old_param).unwrap();
                            (*new_param, *arg)
                        })
                        .collect();

                    replaced = true;
                }
            }
            if let Some(ref mut succ) = default {
                if succ.block == old {
                    succ.block = new;
                    succ.args = succ
                        .args
                        .iter()
                        .map(|(old_param, arg)| {
                            let new_param = param_mapping.get(old_param).unwrap();
                            (*new_param, *arg)
                        })
                        .collect();

                    replaced = true;
                }
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
        use InstKind as Ik;
        match self.deref(ctx).kind {
            Ik::IConst { .. } | Ik::FConst { .. } | Ik::StackSlot { .. } => vec![],
            Ik::IBinary { lhs, rhs, .. } | Ik::FBinary { lhs, rhs, .. } => vec![lhs, rhs],
            Ik::IUnary { val, .. } | Ik::FUnary { val, .. } => vec![val],
            Ik::Cast { val, .. } => vec![val],
            Ik::Branch {
                cond,
                ref succs,
                ref default,
            } => {
                let mut uses = vec![];
                if let Some(cond) = cond {
                    uses.push(cond);
                }
                // now append the arguments (values of the mapping)
                for succ in succs {
                    uses.extend(succ.args.values().cloned());
                }
                if let Some(default) = default {
                    uses.extend(default.args.values().cloned());
                }
                uses
            }
            Ik::Call { ref args, .. } => args.clone(),
            Ik::CallIndirect {
                callee, ref args, ..
            } => {
                let mut uses = vec![callee];
                uses.extend(args);
                uses
            }
            Ik::Return { val } => val.into_iter().collect(),
            Ik::Load { mem, .. } => vec![mem],
            Ik::Store { val, mem } => vec![val, mem],
        }
    }

    fn replace(self, ctx: &mut Context, old: Value, new: Value) {
        use InstKind as Ik;

        let mut replaced = false;

        match self.deref_mut(ctx).kind {
            Ik::IConst { .. } | Ik::FConst { .. } | Ik::StackSlot { .. } => {}
            Ik::IBinary {
                ref mut lhs,
                ref mut rhs,
                ..
            }
            | Ik::FBinary {
                ref mut lhs,
                ref mut rhs,
                ..
            } => {
                if *lhs == old {
                    *lhs = new;
                    replaced = true;
                }
                if *rhs == old {
                    *rhs = new;
                    replaced = true;
                }
            }
            Ik::IUnary { ref mut val, .. } | Ik::FUnary { ref mut val, .. } => {
                if *val == old {
                    *val = new;
                    replaced = true;
                }
            }
            Ik::Cast { ref mut val, .. } => {
                if *val == old {
                    *val = new;
                    replaced = true;
                }
            }
            Ik::Branch {
                ref mut cond,
                ref mut succs,
                ref mut default,
            } => {
                if let Some(ref mut cond) = cond {
                    if *cond == old {
                        *cond = new;
                        replaced = true;
                    }
                }
                for succ in succs {
                    // remember that we need to replace the args, not the
                    // params, i.e., replace the value of the mapping
                    for arg in succ.args.values_mut() {
                        if *arg == old {
                            *arg = new;
                            replaced = true;
                        }
                    }
                }
                if let Some(ref mut succ) = default {
                    for arg in succ.args.values_mut() {
                        if *arg == old {
                            *arg = new;
                            replaced = true;
                        }
                    }
                }
            }
            Ik::Call { ref mut args, .. } => {
                for arg in args {
                    if *arg == old {
                        *arg = new;
                        replaced = true;
                    }
                }
            }
            Ik::CallIndirect {
                ref mut callee,
                ref mut args,
                ..
            } => {
                if *callee == old {
                    *callee = new;
                    replaced = true;
                }
                for arg in args {
                    if *arg == old {
                        *arg = new;
                        replaced = true;
                    }
                }
            }
            Ik::Return { ref mut val } => {
                if let Some(val) = val {
                    if *val == old {
                        *val = new;
                        replaced = true;
                    }
                }
            }
            Ik::Load { ref mut mem, .. } => {
                if *mem == old {
                    *mem = new;
                    replaced = true;
                }
            }
            Ik::Store {
                ref mut val,
                ref mut mem,
            } => {
                if *val == old {
                    *val = new;
                    replaced = true;
                }
                if *mem == old {
                    *mem = new;
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
