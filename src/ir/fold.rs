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

    pub fn is_undef(&self) -> bool { matches!(self, FoldedConstant::Undef) }
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
    pub fn fold(
        self,
        ctx: &Context,
        fold_ctx: &mut FoldContext,
        aggressive: bool,
    ) -> Option<FoldedConstant> {
        match self.kind(ctx) {
            // XXX: SysY undefined value is not defined accurately.
            InstKind::Undef => Some(FoldedConstant::Undef),
            InstKind::IConst(value) => {
                let width = self.result(ctx, 0).ty(ctx).bitwidth(ctx);
                let apint = value.resize(width as u8).into_apint();
                Some(FoldedConstant::Integer(apint))
            }
            InstKind::FConst(value) => Some(FoldedConstant::Float(*value)),
            InstKind::IBinary(op) => {
                let lhs = self.operand(ctx, 0);
                let rhs = self.operand(ctx, 1);

                let lhs_width = lhs.ty(ctx).bitwidth(ctx);
                let rhs_width = rhs.ty(ctx).bitwidth(ctx);

                // TODO: we really should verify the IR

                assert_eq!(lhs_width, rhs_width);

                let mut lhs = fold_ctx.lookup(lhs)?.unwrap_integer().clone();
                let rhs = fold_ctx.lookup(rhs)?.unwrap_integer();

                // 在下述情况中，可被折叠的value和其被记录在fold_ctx中的被折叠成的constant的宽度不一致：
                // 由于fold_ctx仅被set函数改变（clear不算），而set仅在下面被调用：
                // if let Some(constant) = inst.fold(ctx, &mut self.fold_ctx) {
                //     let value = inst.result(ctx, 0);
                //     self.fold_ctx.set(value, constant);
                //     folded_insts.push(inst);
                // }
                // 导致 value 与 constant 总是被同时加入。
                // 所以只可能是inst.fold(ctx, &mut self.fold_ctx)返回的constant与value =
                // inst.result(ctx, 0);返回的value的宽度不一致。
                // 只需检查fold函数中每个match的case的返回constant的宽度是否与value的宽度一致即可。
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
                    IBinaryOp::Max | IBinaryOp::Min => return None,
                }

                Some(FoldedConstant::Integer(lhs))
            }
            InstKind::FBinary(op) => {
                let lhs = self.operand(ctx, 0);
                let rhs = self.operand(ctx, 1);

                let lhs_width = lhs.ty(ctx).bitwidth(ctx);
                let rhs_width = rhs.ty(ctx).bitwidth(ctx);

                assert_eq!(lhs_width, rhs_width);

                let mut lhs = *fold_ctx.lookup(lhs)?.unwrap_float();
                let rhs = fold_ctx.lookup(rhs)?.unwrap_float();

                assert_eq!(lhs.width(), lhs_width);
                assert_eq!(rhs.width(), rhs_width);

                match op {
                    FBinaryOp::Add => {
                        let result = lhs.add(rhs);
                        Some(FoldedConstant::Float(result))
                    }
                    FBinaryOp::Sub => {
                        let result = lhs.sub(rhs);
                        Some(FoldedConstant::Float(result))
                    }
                    FBinaryOp::Mul => {
                        let result = lhs.mul(rhs);
                        Some(FoldedConstant::Float(result))
                    }
                    FBinaryOp::Div => {
                        let result = lhs.div(rhs);
                        Some(FoldedConstant::Float(result))
                    }
                    FBinaryOp::Rem => {
                        let result = lhs.rem(rhs);
                        Some(FoldedConstant::Float(result))
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

                        Some(FoldedConstant::Integer(ApInt::from(result)))
                    }
                }
            }
            InstKind::IUnary(op) => {
                let val = self.operand(ctx, 0);

                let width = val.ty(ctx).bitwidth(ctx);

                let mut val = fold_ctx.lookup(val)?.unwrap_integer().clone();

                assert_eq!(val.width(), width);

                match op {
                    IUnaryOp::Not => {
                        val.inplace_bitnot();
                    }
                }

                Some(FoldedConstant::Integer(val))
            }
            InstKind::FUnary(op) => {
                let val = self.operand(ctx, 0);

                let width = val.ty(ctx).bitwidth(ctx);

                let mut val = *fold_ctx.lookup(val)?.unwrap_float();

                assert_eq!(val.width(), width);

                match op {
                    FUnaryOp::Neg => {
                        val = val.neg();
                    }
                }

                Some(FoldedConstant::Float(val))
            }
            InstKind::Cast(op) => {
                let val = self.operand(ctx, 0);

                let width = val.ty(ctx).bitwidth(ctx);

                match op {
                    // FIXME: 这几个CastOp并不会被折叠，如果折叠了则会报错，
                    // 届时请把仇科文找来让他自裁（）
                    CastOp::IntToPtr | CastOp::PtrToInt | CastOp::Bitcast => {
                        panic!("FOLD: CastInst can be folded but not written yet. Please check here and contact Kevin Chou to do it.")
                    }
                    CastOp::Trunc
                    | CastOp::FpTrunc
                    | CastOp::ZExt
                    | CastOp::SExt
                    | CastOp::FpToUi
                    | CastOp::FpToSi
                    | CastOp::UiToFp
                    | CastOp::SiToFp
                    | CastOp::FpExt => {}
                }

                // 由于Cast的操作数类型不确定，这里需要按照其类型进行分类处理。
                match fold_ctx.lookup(val)? {
                    FoldedConstant::Integer(int_val) => {
                        let mut mut_int_val = int_val.clone();
                        let val = fold_ctx.lookup(val)?.unwrap_integer().clone();
                        assert_eq!(val.width(), width);
                        match op {
                            CastOp::Trunc => {
                                let target_width = self.result(ctx, 0).ty(ctx).bitwidth(ctx);
                                mut_int_val.truncate(target_width);
                                Some(FoldedConstant::Integer(mut_int_val))
                            }
                            CastOp::ZExt => {
                                let target_width = self.result(ctx, 0).ty(ctx).bitwidth(ctx);
                                mut_int_val.zeroext(target_width);
                                Some(FoldedConstant::Integer(mut_int_val))
                            }
                            CastOp::SExt => {
                                let target_width = self.result(ctx, 0).ty(ctx).bitwidth(ctx);
                                mut_int_val.signext(target_width);
                                Some(FoldedConstant::Integer(mut_int_val))
                            }
                            CastOp::UiToFp if aggressive => {
                                // AGGRESSIVE: 在38_light2d中，存在cast int 100000006 to float
                                // 100000010的情况。
                                let target_ty = self.result(ctx, 0).ty(ctx);
                                let u64_val: u64 = mut_int_val.into();
                                let float_val = if target_ty.is_float32(ctx) {
                                    FloatConstant::from(u64_val as f32)
                                } else if target_ty.is_float64(ctx) {
                                    FloatConstant::from(u64_val as f64)
                                } else {
                                    return None;
                                };
                                Some(FoldedConstant::Float(float_val))
                            }
                            CastOp::SiToFp if aggressive => {
                                let target_ty = self.result(ctx, 0).ty(ctx);
                                let u64_val: u64 = mut_int_val.into();
                                let i64_val: i64 =
                                    (u64_val as i64) * if int_val.highest_bit() { -1 } else { 1 };
                                let float_val = if target_ty.is_float32(ctx) {
                                    FloatConstant::from(i64_val as f32)
                                } else if target_ty.is_float64(ctx) {
                                    FloatConstant::from(i64_val as f64)
                                } else {
                                    return None;
                                };
                                Some(FoldedConstant::Float(float_val))
                            }
                            CastOp::FpTrunc
                            | CastOp::FpToUi
                            | CastOp::FpToSi
                            | CastOp::Bitcast
                            | CastOp::FpExt
                            | CastOp::UiToFp
                            | CastOp::SiToFp
                            | CastOp::PtrToInt
                            | CastOp::IntToPtr => None,
                        }
                    }
                    FoldedConstant::Float(float_val) => {
                        let val = *fold_ctx.lookup(val)?.unwrap_float();
                        assert_eq!(val.width(), width);
                        match op {
                            CastOp::FpTrunc => Some(FoldedConstant::Float(float_val.truncate())),
                            CastOp::FpExt => Some(FoldedConstant::Float(float_val.promote())),
                            CastOp::FpToUi | CastOp::FpToSi => {
                                let apint: ApInt = match float_val {
                                    FloatConstant::Float32(val) => {
                                        (f32::from_bits(*val) as i32).into() // FIXME: 这里“as i32” Rust默认采用Round to Zero (RTZ)的舍入方式，与后端相同。
                                    }
                                    FloatConstant::Float64(val) => {
                                        // FIXME: 如果后端默认的FRM不是RTZ的话，记得把这里换一下。
                                        (f64::from_bits(*val) as i64).into()
                                    }
                                };
                                Some(FoldedConstant::Integer(apint))
                            }
                            CastOp::Trunc
                            | CastOp::ZExt
                            | CastOp::SExt
                            | CastOp::UiToFp
                            | CastOp::SiToFp
                            | CastOp::Bitcast
                            | CastOp::PtrToInt
                            | CastOp::IntToPtr => None,
                        }
                    }
                    _ => None,
                }
            }
            InstKind::StackSlot(_)
            | InstKind::Load
            | InstKind::Store
            | InstKind::Br
            | InstKind::Jump
            | InstKind::Call(_)
            | InstKind::CallIndirect(_)
            | InstKind::Ret
            | InstKind::GetGlobal(_)
            | InstKind::Offset
            | InstKind::LoadElem { .. }
            | InstKind::StoreElem { .. } => None,
        }
    }
}
