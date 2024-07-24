//! # Infrastructure for Constant Folding
//!
//! The constant folding is built as an infrastructure, because it can be used
//! not only in optimization passes, but also helpful to build an execution
//! engine for the IR.

use rustc_hash::FxHashMap;

use super::{constant::FloatConstant, Context, IUnaryOp, Inst, InstKind, Value};
use crate::{
    collections::apint::ApInt,
    ir::{CastOp, FBinaryOp, FCmpCond, FUnaryOp, IBinaryOp, ICmpCond},
};

#[derive(Debug, Clone)]
pub enum FoldedConstant {
    Integer(ApInt),
    Float(FloatConstant),
    Undef,
    // TODO: SIMD & struct support
}

impl FoldedConstant {
    pub fn unwrap_integer(&self) -> &ApInt {
        if let FoldedConstant::Integer(value) = self {
            value
        } else {
            panic!("unwrap_integer: not an integer constant");
        }
    }

    pub fn unwrap_float(&self) -> &FloatConstant {
        if let FoldedConstant::Float(value) = self {
            value
        } else {
            panic!("unwrap_float: not a float constant");
        }
    }
}

/// The context of the constant folding.
///
/// The context is used to store the corresponding [Constant] for the values in
/// the IR.
#[derive(Default)]
pub struct FoldContext {
    /// The folded constant values.
    values: FxHashMap<Value, FoldedConstant>,
}

impl FoldContext {
    /// Lookup the constant value of the accepted value.
    pub fn lookup(&self, value: Value) -> Option<&FoldedConstant> { self.values.get(&value) }

    /// Set the constant value of the accepted value.
    pub fn set(&mut self, value: Value, constant: FoldedConstant) {
        self.values.insert(value, constant);
    }

    pub fn clear(&mut self) { self.values.clear(); }
}

impl Inst {
    /// Fold the instruction with a given constant folding context.
    pub fn fold(self, ctx: &Context, fold_ctx: &mut FoldContext) -> Option<FoldedConstant> {
        match self.kind(ctx) {
            InstKind::Undef => Some(FoldedConstant::Undef),
            InstKind::IConst(value) => {
                let width = self.result(ctx, 0).ty(ctx).bitwidth(ctx).unwrap();
                let apint = value.resize(width as u8).into_apint();
                Some(FoldedConstant::Integer(apint))
            }
            InstKind::FConst(value) => Some(FoldedConstant::Float(*value)),
            InstKind::IBinary(op) => {
                let lhs = self.operand(ctx, 0);
                let rhs = self.operand(ctx, 1);

                let lhs_width = lhs.ty(ctx).bitwidth(ctx).unwrap();
                let rhs_width = rhs.ty(ctx).bitwidth(ctx).unwrap();

                // TODO: we really should verify the IR

                assert_eq!(lhs_width, rhs_width);

                let mut lhs = fold_ctx.lookup(lhs)?.unwrap_integer().clone();
                let rhs = fold_ctx.lookup(rhs)?.unwrap_integer();

                assert_eq!(lhs.width(), lhs_width);
                assert_eq!(rhs.width(), rhs_width);

                match op {
                    IBinaryOp::Add => {
                        lhs.inplace_add(rhs);
                    }
                    IBinaryOp::Sub => {
                        lhs.inplace_sub(rhs);
                    }
                    IBinaryOp::Mul => {
                        lhs.inplace_carrying_umul(rhs);
                    }
                    IBinaryOp::UDiv => {
                        lhs.inplace_udiv(rhs);
                    }
                    IBinaryOp::SDiv => {
                        lhs.inplace_sdiv(rhs);
                    }
                    IBinaryOp::URem => {
                        let rem = lhs.inplace_udiv(rhs);
                        lhs = rem;
                    }
                    IBinaryOp::SRem => {
                        let rem = lhs.inplace_sdiv(rhs);
                        lhs = rem;
                    }
                    IBinaryOp::And => {
                        lhs.inplace_bitand(rhs);
                    }
                    IBinaryOp::Or => {
                        lhs.inplace_bitor(rhs);
                    }
                    IBinaryOp::Xor => {
                        lhs.inplace_bitxor(rhs);
                    }
                    IBinaryOp::Shl => {
                        if rhs.width() > 64 {
                            return None;
                        }
                        let shamt = u64::from(rhs.clone());
                        lhs.inplace_carrying_shl(shamt as usize);
                    }
                    IBinaryOp::LShr => {
                        if rhs.width() > 64 {
                            return None;
                        }
                        let shamt = u64::from(rhs.clone());
                        lhs.inplace_lshr(shamt as usize);
                    }
                    IBinaryOp::AShr => {
                        if rhs.width() > 64 {
                            return None;
                        }
                        let shamt = u64::from(rhs.clone());
                        lhs.inplace_ashr(shamt as usize);
                    }
                    IBinaryOp::Cmp(cond) => {
                        let result = match cond {
                            ICmpCond::Eq => &lhs == rhs,
                            ICmpCond::Ne => &lhs != rhs,
                            ICmpCond::Sle => lhs.slt(rhs) || &lhs == rhs,
                            ICmpCond::Slt => lhs.slt(rhs),
                            ICmpCond::Ule => &lhs <= rhs,
                            // the default comparison is unsigned for ApInt
                            ICmpCond::Ult => &lhs < rhs,
                        };

                        lhs = ApInt::from(result);
                    }
                }

                Some(FoldedConstant::Integer(lhs))
            }
            // TODO
            InstKind::FBinary(op) => {
                let lhs = self.operand(ctx, 0);
                let rhs = self.operand(ctx, 1);

                let lhs_width = lhs.ty(ctx).bitwidth(ctx).unwrap();
                let rhs_width = rhs.ty(ctx).bitwidth(ctx).unwrap();

                assert_eq!(lhs_width, rhs_width);

                let mut lhs = *fold_ctx.lookup(lhs)?.unwrap_float();
                let rhs = fold_ctx.lookup(rhs)?.unwrap_float();

                assert_eq!(lhs.width(), lhs_width);
                assert_eq!(rhs.width(), rhs_width);
                
                match op {
                    FBinaryOp::Add => {
                        lhs.inplace_add(rhs);
                    }
                    FBinaryOp::Sub => {
                        lhs.inplace_sub(rhs);
                    }
                    FBinaryOp::Mul => {
                        lhs.inplace_mul(rhs);
                    }
                    FBinaryOp::Div => {
                        lhs.inplace_div(rhs);
                    }
                    FBinaryOp::Rem => {
                        lhs.inplace_rem(rhs);
                    }
                    FBinaryOp::Cmp(cond) => {
                        let result = match cond {
                            FCmpCond::OEq => &lhs == rhs,
                            FCmpCond::ONe => &lhs != rhs,
                            FCmpCond::OLt => lhs.lt(rhs),
                            FCmpCond::OLe => lhs.le(rhs),
                            FCmpCond::UEq => lhs.is_nan() || rhs.is_nan() || &lhs == rhs,
                            FCmpCond::UNe => lhs.is_nan() || rhs.is_nan() || &lhs != rhs,
                            FCmpCond::ULt => lhs.is_nan() || rhs.is_nan() || lhs.lt(rhs),
                            FCmpCond::ULe => lhs.is_nan() || rhs.is_nan() || lhs.le(rhs),
                        };

                        lhs = FloatConstant::from(if result { 1.0 } else { 0.0 });
                    }
                }

                Some(FoldedConstant::Float(lhs))
            },
            InstKind::IUnary(op) => {
                let val = self.operand(ctx, 0);

                let width = val.ty(ctx).bitwidth(ctx).unwrap();

                let mut val = fold_ctx.lookup(val)?.unwrap_integer().clone();

                assert_eq!(val.width(), width);

                match op {
                    IUnaryOp::Not => {
                        val.inplace_bitnot();
                    }
                }

                Some(FoldedConstant::Integer(val))
            },
            InstKind::FUnary(op) => {
                let val = self.operand(ctx, 0);

                let width = val.ty(ctx).bitwidth(ctx).unwrap();

                let mut val = *fold_ctx.lookup(val)?.unwrap_float();

                assert_eq!(val.width(), width);

                match op {
                    FUnaryOp::Neg => {
                        val.inplace_neg();
                    }
                }

                Some(FoldedConstant::Float(val))
            },
            InstKind::Cast(op) => None,
            InstKind::StackSlot(_)
            | InstKind::Load
            | InstKind::Store
            | InstKind::Br
            | InstKind::Jump
            | InstKind::Call(_)
            | InstKind::CallIndirect(_)
            | InstKind::Ret
            | InstKind::GetGlobal(_)
            | InstKind::Offset => None,
        }
    }
}
