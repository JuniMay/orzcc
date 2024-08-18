//! Instruction Combining Pass
//!
//! The instruction combining is extensible with [Rule]s. Each rule is a
//! function that takes an instruction and returns a boolean. If the rule
//! matches, the instruction is modified in place and the function returns
//! `true`. The rule should not remove the instruction, but can modify the
//! def-use chain of the instruction result.
//!
//! The rules include:
//！
//! - `c @ x => x @ c` ^[1] [`mv_const_rhs`]
//!
//! - `x +- 0 => x` [`add_zero_elim`] [`sub_zero_elim`]
//! - `x -+ (0 - y) => x +- y` [`assoc_sub_zero`]
//! - `(0 - x) + y => y - x`
//!
//! - `x - x => 0` [`sub_identity_to_zero`]
//! - `x - (x + y)` or `x - (y + x)` => `0 - y`
//! - `(x + y) - x` or `(y + x) - x` => `y`
//! - `(x +- c) + d` or `d + (x +- c)` => `x +- (c +- d)` [`assoc_const`]
//! - `(x +- c) - d => x +- (c -+ d)`
//! - `d - (c - x) => x + (d - c)`
//!
//! - `x + x => x * 2` [`add_to_mul`]
//! - `(x * c) + x` or `x + (x * c)` => `x * (c + 1)` [`distributive_one`]
//! - `(x * c) - x` => `x * (c - 1)
//! - `x - (x * c)` => `x * (1 - c)`
//! - `x * y +- x * z => x * (y +- z)` [`distributive`]
//! - `x * y +- z * y => (x +- z) * y`
//! - `x / y +- z / y => (x +- z) / y`
//!
//! - `v * 1 => v` [`mul_one_elim`]
//! - `v * 0 => 0` [`mul_zero_elim`]
//! - `v / 1 => v` [`div_one_elim`]
//! - `v / -1 => -v` [`div_neg_one_elim`]
//! - `v % 1 => 0` [`rem_one_elim`]
//!
//! - `(x * c) * d => x * (c * d)` [`assoc_const`]
//! - `d * (x * c) => x * (c * d)`
//!
//! - `v << 0 => v` [`shl_zero_elim`]
//! - `v >> 0 => v` [`shr_zero_elim`]
//!
//! - `v / (2^n)` => `v >> n` [`div_to_shl`]
//! - `v * (2^n)` => `v << n` [`mul_to_shl`]
//! - `v % (2^n)` => `v & (2^n - 1)` [`rem_to_shl`]
//! - `v / c` => `(v * magic) >> disp + sign` [`div_rem_to_mul`]
//! - `v % c` => `v - (v / c) * c`
//!
//! - `p[0] => *p` [`offset_zero_elim`]
//!
//! [1]: where @ is a commutative binary operator.

use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        CastOp,
        Context,
        Func,
        IBinaryOp,
        Inst,
        InstKind as Ik,
        IntConstant,
        Ty,
        ValueKind,
    },
    utils::def_use::{Usable, User},
};

pub const INSTCOMBINE: &str = "instcombine";

pub const AGGRESSIVE_INSTCOMBINE: &str = "aggressive-instcombine";

pub const ADVANCED_INSTCOMBINE: &str = "advanced-instcombine";

/// A rule for instcombine.
struct Rule {
    /// The rewriter function.
    ///
    /// If this rule matches, this function should modify the instruction in
    /// place and return `true`.
    rewriter: fn(&mut Context, Inst) -> bool,
}

pub struct Instcombine {
    rules: Vec<Rule>,
}

pub struct AggressiveInstcombine {
    rules: Vec<Rule>,
}

pub struct AdvancedInstcombine {
    rules: Vec<Rule>,
}

impl Default for Instcombine {
    fn default() -> Self {
        Self {
            rules: vec![
                mv_const_rhs(),
                mul_zero_elim(),
                mul_one_elim(),
                add_zero_elim(),
                assoc_sub_zero(),
                sub_zero_elim(),
                offset_zero_elim(),
                add_to_mul(),
                div_one_elim(),
                div_neg_one_elim(),
                rem_one_elim(),
                shl_zero_elim(), // not tested
                shr_zero_elim(), // not tested
                redistribute_const(), // aggressive
                // reassociate() // 
            ],
        }
    }
}

impl LocalPassMut for Instcombine {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        let mut cursor = func.cursor();
        while let Some(block) = cursor.next(ctx) {
            let mut cursor = block.cursor();
            while let Some(inst) = cursor.next(ctx) {
                if !inst.is_used(ctx) {
                    // if the instruction's results have no users, we can move to the next
                    // instruction.
                    continue;
                }
                for rule in &self.rules {
                    if (rule.rewriter)(ctx, inst) {
                        changed = true;
                        if !inst.is_used(ctx) {
                            // if the instruction's results have no users, we can move to the next
                            // instruction without applying other rules
                            break;
                        }
                    }
                }
            }
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for Instcombine {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        for func in ctx.funcs() {
            let ((), local_changed) = LocalPassMut::run(self, ctx, func)?;
            changed |= local_changed;
        }

        Ok(((), changed))
    }
}

impl TransformPass for Instcombine {
    fn register(passman: &mut crate::ir::passman::PassManager) {
        let pass = Self::default();
        passman.register_transform(INSTCOMBINE, pass, Vec::new());
    }
}

impl Default for AdvancedInstcombine {
    fn default() -> Self {
        Self {
            rules: vec![
                div_to_shift(),
                rem_to_shift(),
                div_rem_to_mul(),
                mul_to_shl(),
            ],
        }
    }
}

impl LocalPassMut for AdvancedInstcombine {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        let mut cursor = func.cursor();
        while let Some(block) = cursor.next(ctx) {
            let mut cursor = block.cursor();
            while let Some(inst) = cursor.next(ctx) {
                if !inst.is_used(ctx) {
                    // if the instruction's results have no users, we can move to the next
                    // instruction.
                    continue;
                }
                for rule in &self.rules {
                    if (rule.rewriter)(ctx, inst) {
                        changed = true;
                        if !inst.is_used(ctx) {
                            // if the instruction's results have no users, we can move to the next
                            // instruction without applying other rules
                            break;
                        }
                    }
                }
            }
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for AdvancedInstcombine {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        for func in ctx.funcs() {
            let ((), local_changed) = LocalPassMut::run(self, ctx, func)?;
            changed |= local_changed;
        }

        Ok(((), changed))
    }
}

impl TransformPass for AdvancedInstcombine {
    fn register(passman: &mut crate::ir::passman::PassManager) {
        let pass = Self::default();
        passman.register_transform(ADVANCED_INSTCOMBINE, pass, Vec::new());
    }
}

impl Default for AggressiveInstcombine {
    fn default() -> Self {
        Self {
            rules: vec![
                mv_same_together(),     // aggressive
                sub_identity_to_zero(), // aggressive
                assoc_const(),          // aggressive
                distributive_one(),     // aggressive
                distributive(),         // aggressive
            ],
        }
    }
}

impl LocalPassMut for AggressiveInstcombine {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        let mut cursor = func.cursor();
        while let Some(block) = cursor.next(ctx) {
            let mut cursor = block.cursor();
            while let Some(inst) = cursor.next(ctx) {
                if !inst.is_used(ctx) {
                    // if the instruction's results have no users, we can move to the next
                    // instruction.
                    continue;
                }
                for rule in &self.rules {
                    if (rule.rewriter)(ctx, inst) {
                        changed = true;
                        if !inst.is_used(ctx) {
                            // if the instruction's results have no users, we can move to the next
                            // instruction without applying other rules
                            break;
                        }
                    }
                }
            }
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for AggressiveInstcombine {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        for func in ctx.funcs() {
            let ((), local_changed) = LocalPassMut::run(self, ctx, func)?;
            changed |= local_changed;
        }

        Ok(((), changed))
    }
}

impl TransformPass for AggressiveInstcombine {
    fn register(passman: &mut crate::ir::passman::PassManager) {
        let pass = Self::default();
        passman.register_transform(AGGRESSIVE_INSTCOMBINE, pass, Vec::new());
    }
}

/// Move constant to the right hand side.
///
/// This applies to commutative instructions, when the lhs is a constant and the
/// rhs is not.
const fn mv_const_rhs() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if inst.is_commutative(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);

                // lhs is const and rhs is not, than we commute
                if let ValueKind::InstResult { inst: lhs_inst, .. } = lhs.kind(ctx) {
                    if let Ik::IConst(_) = lhs_inst.kind(ctx) {
                    } else if let Ik::FConst(_) = lhs_inst.kind(ctx) {
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }

                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IConst(_) = rhs_inst.kind(ctx) {
                        return false;
                    } else if let Ik::FConst(_) = rhs_inst.kind(ctx) {
                        return false;
                    }
                }

                // now we know that lhs is const and rhs is not
                inst.commute_operands(ctx);
                true
            } else {
                false
            }
        },
    }
}

/// Eliminate addition with zero.
const fn add_zero_elim() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(IBinaryOp::Add) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: lhs_inst, .. } = lhs.kind(ctx) {
                    if let Ik::IConst(v) = lhs_inst.kind(ctx) {
                        if v.is_zero() {
                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, rhs);
                            }
                            return true;
                        }
                    }
                }
                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IConst(v) = rhs_inst.kind(ctx) {
                        if v.is_zero() {
                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, lhs);
                            }
                            return true;
                        }
                    }
                }
            }
            false
        },
    }
}

const fn sub_zero_elim() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(IBinaryOp::Sub) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IConst(v) = rhs_inst.kind(ctx) {
                        if v.is_zero() {
                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, lhs);
                            }
                            return true;
                        }
                    }
                }
            }
            false
        },
    }
}

/// Eliminate offset with zero.
///
/// Similar to `add_zero_elim`, but for offset instructions.
const fn offset_zero_elim() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::Offset = inst.kind(ctx) {
                let base = inst.operand(ctx, 0);
                let offset = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult {
                    inst: offset_inst, ..
                } = offset.kind(ctx)
                {
                    if let Ik::IConst(v) = offset_inst.kind(ctx) {
                        if v.is_zero() {
                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, base);
                            }
                            return true;
                        }
                    }
                }
            }
            false
        },
    }
}

/// Replace multiplication with shift.
const fn mul_to_shl() -> Rule {
    Rule {
        rewriter: |ctx: &mut Context, inst: Inst| {
            if let Ik::IBinary(IBinaryOp::Mul) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);

                let bitwidth = lhs.ty(ctx).bitwidth(ctx);

                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IConst(mut v) = rhs_inst.kind(ctx) {
                        let is_v_neg = if v.as_signed() < 0 {
                            v = IntConstant::from(-v.as_signed());
                            true
                        } else {
                            false
                        };
                        if v.is_power_of_two() {
                            let shamt =
                                IntConstant::from(v.trailing_zeros()).resize(bitwidth as u8);
                            let i_shamt = Inst::iconst(ctx, shamt, dst.ty(ctx));
                            let i_shl =
                                Inst::ibinary(ctx, IBinaryOp::Shl, lhs, i_shamt.result(ctx, 0));
                            inst.insert_after(ctx, i_shamt);
                            i_shamt.insert_after(ctx, i_shl);
                            let dst_new = if is_v_neg {
                                let i_zero = Inst::iconst(ctx, 0, dst.ty(ctx));
                                let i_neg = Inst::ibinary(
                                    ctx,
                                    IBinaryOp::Sub,
                                    i_zero.result(ctx, 0),
                                    i_shl.result(ctx, 0),
                                );
                                i_shl.insert_after(ctx, i_zero);
                                i_zero.insert_after(ctx, i_neg);
                                i_neg.result(ctx, 0)
                            } else {
                                i_shl.result(ctx, 0)
                            };

                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, dst_new);
                            }
                            // we cannot remove the instruction here because we are iterating
                            return true;
                        }
                    }
                }
            }
            false
        },
    }
}

/// Eliminate multiplication by one.
const fn mul_one_elim() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(IBinaryOp::Mul) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: lhs_inst, .. } = lhs.kind(ctx) {
                    if let Ik::IConst(v) = lhs_inst.kind(ctx) {
                        if v.is_one() {
                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, rhs);
                            }
                            return true;
                        }
                    }
                }
                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IConst(v) = rhs_inst.kind(ctx) {
                        if v.is_one() {
                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, lhs);
                            }
                            return true;
                        }
                    }
                }
            }
            false
        },
    }
}

const fn mul_zero_elim() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(IBinaryOp::Mul) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: lhs_inst, .. } = lhs.kind(ctx) {
                    if let Ik::IConst(v) = lhs_inst.kind(ctx) {
                        if v.is_zero() {
                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, lhs);
                            }
                            return true;
                        }
                    }
                }
                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IConst(v) = rhs_inst.kind(ctx) {
                        if v.is_zero() {
                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, rhs);
                            }
                            return true;
                        }
                    }
                }
            }
            false
        },
    }
}

/// Add identity to multiplication.
///
/// - `x + x => x * 2`
///
/// The result will be optimized by the `mul_to_shl` rule. Also, if there are
/// more additions/subtractions with the same operand, they will be optimized by
/// the `distributive_const` rule.
const fn add_to_mul() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(IBinaryOp::Add) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                let width = dst.ty(ctx).bitwidth(ctx) as u8;

                if lhs == rhs {
                    let two = Inst::iconst(ctx, IntConstant::from(2).resize(width), dst.ty(ctx));
                    let mul = Inst::ibinary(ctx, IBinaryOp::Mul, lhs, two.result(ctx, 0));
                    inst.insert_after(ctx, two);
                    two.insert_after(ctx, mul);
                    for user in dst.users(ctx) {
                        user.replace(ctx, dst, mul.result(ctx, 0));
                    }
                    return true;
                }
            }
            false
        },
    }
}

/// Associativity of subtraction with zero.
///
/// - `x - (0 - y) => x + y`
/// - `x + (0 - y) => x - y`
/// - `(0 - x) + y => y - x`
const fn assoc_sub_zero() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(IBinaryOp::Sub) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IBinary(IBinaryOp::Sub) = rhs_inst.kind(ctx) {
                        let rhs_lhs = rhs_inst.operand(ctx, 0);
                        let rhs_rhs = rhs_inst.operand(ctx, 1);
                        if let ValueKind::InstResult {
                            inst: rhs_lhs_inst, ..
                        } = rhs_lhs.kind(ctx)
                        {
                            if let Ik::IConst(v) = rhs_lhs_inst.kind(ctx) {
                                if v.is_zero() {
                                    // x - (0 - y) => x + y
                                    let add = Inst::ibinary(ctx, IBinaryOp::Add, lhs, rhs_rhs);
                                    inst.insert_after(ctx, add);
                                    for user in dst.users(ctx) {
                                        user.replace(ctx, dst, add.result(ctx, 0));
                                    }
                                    return true;
                                }
                            }
                        }
                    }
                }
            } else if let Ik::IBinary(IBinaryOp::Add) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IBinary(IBinaryOp::Sub) = rhs_inst.kind(ctx) {
                        let rhs_lhs = rhs_inst.operand(ctx, 0);
                        let rhs_rhs = rhs_inst.operand(ctx, 1);
                        if let ValueKind::InstResult {
                            inst: rhs_lhs_inst, ..
                        } = rhs_lhs.kind(ctx)
                        {
                            if let Ik::IConst(v) = rhs_lhs_inst.kind(ctx) {
                                if v.is_zero() {
                                    // x + (0 - y) => x - y
                                    let sub = Inst::ibinary(ctx, IBinaryOp::Sub, lhs, rhs_rhs);
                                    inst.insert_after(ctx, sub);
                                    for user in dst.users(ctx) {
                                        user.replace(ctx, dst, sub.result(ctx, 0));
                                    }
                                    return true;
                                }
                            }
                        }
                    }
                }

                if let ValueKind::InstResult { inst: lhs_inst, .. } = lhs.kind(ctx) {
                    if let Ik::IBinary(IBinaryOp::Sub) = lhs_inst.kind(ctx) {
                        let lhs_lhs = lhs_inst.operand(ctx, 0);
                        let lhs_rhs = lhs_inst.operand(ctx, 1);
                        if let ValueKind::InstResult {
                            inst: lhs_lhs_inst, ..
                        } = lhs_lhs.kind(ctx)
                        {
                            if let Ik::IConst(v) = lhs_lhs_inst.kind(ctx) {
                                if v.is_zero() {
                                    // (0 - x) + y => y - x
                                    let sub = Inst::ibinary(ctx, IBinaryOp::Sub, rhs, lhs_rhs);
                                    inst.insert_after(ctx, sub);
                                    for user in dst.users(ctx) {
                                        user.replace(ctx, dst, sub.result(ctx, 0));
                                    }
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
            false
        },
    }
}

// Eliminate division by one
const fn div_one_elim() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(IBinaryOp::SDiv) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IConst(v) = rhs_inst.kind(ctx) {
                        if v.is_one() {
                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, lhs);
                            }
                            return true;
                        }
                    }
                }
            }
            false
        },
    }
}

// Eliminate division by negative one
const fn div_neg_one_elim() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(IBinaryOp::SDiv) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IConst(v) = rhs_inst.kind(ctx) {
                        if v.as_signed() == -1 {
                            let zero = Inst::iconst(ctx, 0, dst.ty(ctx));
                            let neg = Inst::ibinary(ctx, IBinaryOp::Sub, zero.result(ctx, 0), lhs);
                            inst.insert_after(ctx, zero);
                            zero.insert_after(ctx, neg);
                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, neg.result(ctx, 0));
                            }
                            return true;
                        }
                    }
                }
            }
            false
        },
    }
}

/// Replace division with shift (and add).
const fn div_to_shift() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(IBinaryOp::SDiv) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IConst(mut v) = rhs_inst.kind(ctx) {
                        let is_v_neg = if v.as_signed() < 0 {
                            v = IntConstant::from(-v.as_signed());
                            true
                        } else {
                            false
                        };
                        if v.is_power_of_two() {
                            let k = v.trailing_zeros();
                            if k == 0 {
                                return false;
                            }
                            let bitwidth = lhs.ty(ctx).bitwidth(ctx) as u8;
                            let shamt_ks1 = IntConstant::from(k - 1);
                            let shamt_wsk = IntConstant::from(bitwidth as u32 - k);
                            let shamt_k: IntConstant = IntConstant::from(k);

                            let temp0 = Inst::iconst(ctx, shamt_ks1, lhs.ty(ctx));
                            let temp1 =
                                Inst::ibinary(ctx, IBinaryOp::AShr, lhs, temp0.result(ctx, 0));
                            let temp2 = Inst::iconst(ctx, shamt_wsk, lhs.ty(ctx));
                            let temp3 = Inst::ibinary(
                                ctx,
                                IBinaryOp::LShr,
                                temp1.result(ctx, 0),
                                temp2.result(ctx, 0),
                            );
                            let temp4 =
                                Inst::ibinary(ctx, IBinaryOp::Add, lhs, temp3.result(ctx, 0));
                            let temp5 = Inst::iconst(ctx, shamt_k, dst.ty(ctx));
                            let final_inst = Inst::ibinary(
                                ctx,
                                IBinaryOp::AShr,
                                temp4.result(ctx, 0),
                                temp5.result(ctx, 0),
                            );

                            inst.insert_after(ctx, temp0);
                            temp0.insert_after(ctx, temp1);
                            temp1.insert_after(ctx, temp2);
                            temp2.insert_after(ctx, temp3);
                            temp3.insert_after(ctx, temp4);
                            temp4.insert_after(ctx, temp5);
                            temp5.insert_after(ctx, final_inst);
                            let dst_new = if is_v_neg {
                                let i_zero = Inst::iconst(ctx, 0, dst.ty(ctx));
                                let i_neg = Inst::ibinary(
                                    ctx,
                                    IBinaryOp::Sub,
                                    i_zero.result(ctx, 0),
                                    final_inst.result(ctx, 0),
                                );
                                final_inst.insert_after(ctx, i_zero);
                                i_zero.insert_after(ctx, i_neg);
                                i_neg.result(ctx, 0)
                            } else {
                                final_inst.result(ctx, 0)
                            };

                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, dst_new);
                            }
                            return true;
                        }
                    }
                }
            }
            false
        },
    }
}

// Eliminate modulo by (negative) one
const fn rem_one_elim() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(IBinaryOp::SRem) = inst.kind(ctx) {
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IConst(mut v) = rhs_inst.kind(ctx) {
                        if v.as_signed() < 0 {
                            v = IntConstant::from(-v.as_signed())
                        }
                        if v.is_one() {
                            let zero = Inst::iconst(ctx, 0, dst.ty(ctx));
                            inst.insert_after(ctx, zero);

                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, zero.result(ctx, 0));
                            }
                            return true;
                        }
                    }
                }
            }
            false
        },
    }
}

/// Replace modulo with shift (and sub).
const fn rem_to_shift() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(IBinaryOp::SRem) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IConst(mut v) = rhs_inst.kind(ctx) {
                        if v.as_signed() < 0 {
                            v = IntConstant::from(-v.as_signed())
                        }
                        if v.is_power_of_two() {
                            let k = v.trailing_zeros();
                            if k == 0 {
                                return false;
                            }
                            let bitwidth = lhs.ty(ctx).bitwidth(ctx) as u8;
                            let shamt_wsk = IntConstant::from(bitwidth as u32 - k);
                            let andwith = IntConstant::from(v.as_signed() as u32 - 1);

                            let tmp0 =
                                Inst::iconst(ctx, IntConstant::from(bitwidth - 1), lhs.ty(ctx));
                            let tmp1 =
                                Inst::ibinary(ctx, IBinaryOp::AShr, lhs, tmp0.result(ctx, 0));
                            let tmp2 = Inst::iconst(ctx, shamt_wsk, dst.ty(ctx));
                            let tmp3 = Inst::ibinary(
                                ctx,
                                IBinaryOp::LShr,
                                tmp1.result(ctx, 0),
                                tmp2.result(ctx, 0),
                            );
                            let tmp4 = Inst::ibinary(ctx, IBinaryOp::Add, lhs, tmp3.result(ctx, 0));
                            let tmp5 = Inst::iconst(ctx, andwith, dst.ty(ctx));
                            let tmp6 = Inst::ibinary(
                                ctx,
                                IBinaryOp::And,
                                tmp4.result(ctx, 0),
                                tmp5.result(ctx, 0),
                            );
                            let final_inst = Inst::ibinary(
                                ctx,
                                IBinaryOp::Sub,
                                tmp6.result(ctx, 0),
                                tmp3.result(ctx, 0),
                            );

                            inst.insert_after(ctx, tmp0);
                            tmp0.insert_after(ctx, tmp1);
                            tmp1.insert_after(ctx, tmp2);
                            tmp2.insert_after(ctx, tmp3);
                            tmp3.insert_after(ctx, tmp4);
                            tmp4.insert_after(ctx, tmp5);
                            tmp5.insert_after(ctx, tmp6);
                            tmp6.insert_after(ctx, final_inst);

                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, final_inst.result(ctx, 0));
                            }
                            return true;
                        }
                    }
                }
            }
            false
        },
    }
}

const fn div_rem_to_mul() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(op) = inst.kind(ctx) {
                let is_div = matches!(op, IBinaryOp::SDiv);
                let is_rem = matches!(op, IBinaryOp::SRem);
                if !is_div && !is_rem {
                    return false;
                }

                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IConst(mut v) = rhs_inst.kind(ctx) {
                        if v.is_zero() {
                            return false;
                        }
                        let is_v_neg = if v.as_signed() < 0 {
                            v = IntConstant::from(-v.as_signed());
                            true
                        } else {
                            false
                        };
                        if !v.is_power_of_two() {
                            let bitwidth = lhs.ty(ctx).bitwidth(ctx);
                            if bitwidth != 32 {
                                return false;
                            }
                            // TODO:
                            // (magi, disp) = magic(rhs);
                            // bitwidth = 32
                            //
                            // // mulh v1= lhs, magi
                            // let v2 = i64 (Ty::int(64))
                            //
                            // srai v2= v1, (disp - bitwidth)
                            // srli v3= lhs, (bitwidth - 1)
                            // add ans= v2, v3
                            //
                            let int64 = Ty::int(ctx, 64);
                            let (magi, disp) = magic(bitwidth as u64, v.as_signed() as u64);

                            // temp0-temp5: 使用64位整数的乘法和右移来模拟32位整数与魔数的高位乘法。
                            let temp0 = Inst::cast(ctx, CastOp::SExt, lhs, int64);
                            let temp1 = Inst::iconst(ctx, magi, int64);
                            let temp2 = Inst::ibinary(
                                ctx,
                                IBinaryOp::Mul,
                                temp0.result(ctx, 0),
                                temp1.result(ctx, 0),
                            );
                            let temp3 = Inst::iconst(ctx, IntConstant::from(disp), int64);
                            let temp4 = Inst::ibinary(
                                ctx,
                                IBinaryOp::AShr,
                                temp2.result(ctx, 0),
                                temp3.result(ctx, 0),
                            );
                            let temp5 =
                                Inst::cast(ctx, CastOp::Trunc, temp4.result(ctx, 0), lhs.ty(ctx));
                            // temp6-temp7: 获得符号位用于修正
                            let temp6 = Inst::iconst(
                                ctx,
                                IntConstant::from(bitwidth as u64 - 1),
                                lhs.ty(ctx),
                            );
                            let temp7 =
                                Inst::ibinary(ctx, IBinaryOp::LShr, lhs, temp6.result(ctx, 0));
                            // final_inst: 使用符号位修正结果，得到除法结果。
                            let temp8 = Inst::ibinary(
                                ctx,
                                IBinaryOp::Add,
                                temp5.result(ctx, 0),
                                temp7.result(ctx, 0),
                            );

                            inst.insert_after(ctx, temp0);
                            temp0.insert_after(ctx, temp1);
                            temp1.insert_after(ctx, temp2);
                            temp2.insert_after(ctx, temp3);
                            temp3.insert_after(ctx, temp4);
                            temp4.insert_after(ctx, temp5);
                            temp5.insert_after(ctx, temp6);
                            temp6.insert_after(ctx, temp7);
                            temp7.insert_after(ctx, temp8);

                            // 处理取模的情况，并确定final_inst。
                            let final_inst = if is_div {
                                temp8
                            } else if is_rem {
                                let temp9 = Inst::iconst(ctx, v, lhs.ty(ctx));
                                let temp10 = Inst::ibinary(
                                    ctx,
                                    IBinaryOp::Mul,
                                    temp8.result(ctx, 0),
                                    temp9.result(ctx, 0),
                                );
                                let temp11 =
                                    Inst::ibinary(ctx, IBinaryOp::Sub, lhs, temp10.result(ctx, 0));

                                temp8.insert_after(ctx, temp9);
                                temp9.insert_after(ctx, temp10);
                                temp10.insert_after(ctx, temp11);

                                temp11
                            } else {
                                panic!("unreachable")
                            };

                            // 处理结果的符号，并确定dst_new。
                            let dst_new = if is_v_neg {
                                let i_zero = Inst::iconst(ctx, 0, dst.ty(ctx));
                                let i_neg = Inst::ibinary(
                                    ctx,
                                    IBinaryOp::Sub,
                                    i_zero.result(ctx, 0),
                                    final_inst.result(ctx, 0),
                                );
                                final_inst.insert_after(ctx, i_zero);
                                i_zero.insert_after(ctx, i_neg);
                                i_neg.result(ctx, 0)
                            } else {
                                final_inst.result(ctx, 0)
                            };

                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, dst_new);
                            }
                            return true;
                        }
                    }
                }
            }
            false
        },
    }
}

// 仅用于div_rem_to_mul。
fn magic(w: u64, d: u64) -> (u64, u64) {
    // w = bitwidth
    // d = divisor
    let nc = (1 << (w - 1)) - (1 << (w - 1)) % d - 1; // FIXME: 93_nested_call.sy div 0
    let mut p = w;
    while 1 << p <= nc * (d - (1 << p) % d) {
        p += 1;
    }
    let s = p;
    let m = ((1 << p) + d - (1 << p) % d) / d;

    // m = magi(c number)
    // s = disp(lacement)
    (m, s)
}

// Eliminate shift by zero
const fn shl_zero_elim() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(IBinaryOp::Shl) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IConst(v) = rhs_inst.kind(ctx) {
                        if v.is_zero() {
                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, lhs);
                            }
                            return true;
                        }
                    }
                }
            }
            false
        },
    }
}

// Eliminate shift by zero
const fn shr_zero_elim() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(IBinaryOp::LShr | IBinaryOp::AShr) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IConst(v) = rhs_inst.kind(ctx) {
                        if v.is_zero() {
                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, lhs);
                            }
                            return true;
                        }
                    }
                }
            }
            false
        },
    }
}

// ---------------------------- AGGRESSIVE RULES ---------------------------- //
// TODO: We might need to consider the overflow.

/// Eliminate subtraction same operand.
///
/// - `x - x => 0`
/// - `x - (x + y) => 0 - y`
/// - `x - (y + x) => 0 - y`
/// - `(x + y) - x => y`
/// - `(y + x) - x => y`
const fn sub_identity_to_zero() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(IBinaryOp::Sub) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                let bitwidth = dst.ty(ctx).bitwidth(ctx);

                if lhs == rhs {
                    let zero = Inst::iconst(
                        ctx,
                        IntConstant::zero(dst.ty(ctx).bitwidth(ctx) as u8),
                        dst.ty(ctx),
                    );
                    inst.insert_after(ctx, zero);
                    for user in dst.users(ctx) {
                        user.replace(ctx, dst, zero.result(ctx, 0));
                    }
                    return true;
                }

                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IBinary(IBinaryOp::Add) = rhs_inst.kind(ctx) {
                        let rhs_lhs = rhs_inst.operand(ctx, 0);
                        let rhs_rhs = rhs_inst.operand(ctx, 1);
                        if lhs == rhs_lhs {
                            // x - (x + y) => 0 - y
                            let zero =
                                Inst::iconst(ctx, IntConstant::zero(bitwidth as u8), dst.ty(ctx));
                            let neg_rhs =
                                Inst::ibinary(ctx, IBinaryOp::Sub, zero.result(ctx, 0), rhs_rhs);

                            inst.insert_after(ctx, zero);
                            zero.insert_after(ctx, neg_rhs);

                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, neg_rhs.result(ctx, 0));
                            }
                            return true;
                        } else if lhs == rhs_rhs {
                            // x - (y + x) => 0 - y
                            let zero =
                                Inst::iconst(ctx, IntConstant::zero(bitwidth as u8), dst.ty(ctx));
                            let neg_rhs =
                                Inst::ibinary(ctx, IBinaryOp::Sub, zero.result(ctx, 0), rhs_lhs);

                            inst.insert_after(ctx, zero);
                            zero.insert_after(ctx, neg_rhs);

                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, neg_rhs.result(ctx, 0));
                            }
                            return true;
                        }
                    }
                }

                if let ValueKind::InstResult { inst: lhs_inst, .. } = lhs.kind(ctx) {
                    if let Ik::IBinary(IBinaryOp::Add) = lhs_inst.kind(ctx) {
                        let lhs_lhs = lhs_inst.operand(ctx, 0);
                        let lhs_rhs = lhs_inst.operand(ctx, 1);
                        if rhs == lhs_lhs {
                            // (x + y) - x => y
                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, lhs_rhs);
                            }
                            return true;
                        } else if rhs == lhs_rhs {
                            // (y + x) - x => y
                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, lhs_lhs);
                            }
                            return true;
                        }
                    }
                }
            }
            false
        },
    }
}

/// Associativity to combine constants.
///
/// Add:
///
/// - `(x + c) + d => x + (c + d)`
/// - `(x - c) + d => x - (c - d)`
/// - `d + (x + c) => x + (c + d)`
/// - `d + (x - c) => x + (d - c)`
///
/// Sub:
///
/// - `(x + c) - d => x + (c - d)`
/// - `(x - c) - d => x - (c + d)`
/// - `d - (x + c)`: NOT COMBINABLE, we need an additional `0 - x`, not
///   profitable
/// - `d - (c - x) => x + (d - c)`
///
/// Mul:
///
/// - `(x * c) * d => x * (c * d)`
/// - `d * (x * c) => x * (c * d)`
///
/// The result can be constant folded, we don't need to fold the constant here,
/// we just create new instructions and replace def-use. However, we should
/// check constants to make the transformation profitable.
///
/// We should also note the commutative property of addition and multiplication.
///
/// TODO: Can integer division be combined? Signed or unsigned?
const fn assoc_const() -> Rule {
    use IBinaryOp as Op;

    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(op) = inst.kind(ctx) {
                if !matches!(op, Op::Add | Op::Sub | Op::Mul) {
                    return false;
                }

                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                // consider the situation when lhs is a binary
                if let ValueKind::InstResult { inst: lhs_inst, .. } = lhs.kind(ctx) {
                    if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                        match (lhs_inst.kind(ctx), rhs_inst.kind(ctx)) {
                            (Ik::IBinary(lhs_op), Ik::IConst(_)) => {
                                // `(x + c) + d => x + (c + d)`
                                // `(x - c) + d => x - (c - d)`
                                // `(x + c) - d => x + (c - d)`
                                // `(x - c) - d => x - (c + d)`
                                // `(x * c) * d => x * (c * d)`

                                let lhs_lhs = lhs_inst.operand(ctx, 0);
                                let lhs_rhs = lhs_inst.operand(ctx, 1);

                                match (lhs_op, op) {
                                    (Op::Add, Op::Add) => {
                                        if let ValueKind::InstResult {
                                            inst: lhs_lhs_inst, ..
                                        } = lhs_lhs.kind(ctx)
                                        {
                                            if let Ik::IConst(_) = lhs_lhs_inst.kind(ctx) {
                                                // (c + x) + d => x + (c + d)
                                                let new_rhs = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Add,
                                                    lhs_lhs,
                                                    rhs,
                                                );
                                                let new_add = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Add,
                                                    lhs_rhs,
                                                    new_rhs.result(ctx, 0),
                                                );
                                                inst.insert_after(ctx, new_rhs);
                                                new_rhs.insert_after(ctx, new_add);
                                                for user in dst.users(ctx) {
                                                    user.replace(ctx, dst, new_add.result(ctx, 0));
                                                }
                                                return true;
                                            }
                                        }
                                        if let ValueKind::InstResult {
                                            inst: lhs_rhs_inst, ..
                                        } = lhs_rhs.kind(ctx)
                                        {
                                            if let Ik::IConst(_) = lhs_rhs_inst.kind(ctx) {
                                                // (x + c) + d => x + (c + d)
                                                let new_rhs = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Add,
                                                    lhs_rhs,
                                                    rhs,
                                                );
                                                let new_add = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Add,
                                                    lhs_lhs,
                                                    new_rhs.result(ctx, 0),
                                                );
                                                inst.insert_after(ctx, new_rhs);
                                                new_rhs.insert_after(ctx, new_add);
                                                for user in dst.users(ctx) {
                                                    user.replace(ctx, dst, new_add.result(ctx, 0));
                                                }
                                                return true;
                                            }
                                        }
                                    }
                                    (Op::Sub, Op::Add) => {
                                        if let ValueKind::InstResult {
                                            inst: lhs_rhs_inst, ..
                                        } = lhs_rhs.kind(ctx)
                                        {
                                            if let Ik::IConst(_) = lhs_rhs_inst.kind(ctx) {
                                                // (x - c) + d => x - (c - d)
                                                let new_rhs = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Sub,
                                                    lhs_rhs,
                                                    rhs,
                                                );
                                                let new_sub = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Sub,
                                                    lhs_lhs,
                                                    new_rhs.result(ctx, 0),
                                                );
                                                inst.insert_after(ctx, new_rhs);
                                                new_rhs.insert_after(ctx, new_sub);
                                                for user in dst.users(ctx) {
                                                    user.replace(ctx, dst, new_sub.result(ctx, 0));
                                                }
                                                return true;
                                            }
                                        }
                                    }
                                    (Op::Add, Op::Sub) => {
                                        if let ValueKind::InstResult {
                                            inst: lhs_lhs_inst, ..
                                        } = lhs_lhs.kind(ctx)
                                        {
                                            if let Ik::IConst(_) = lhs_lhs_inst.kind(ctx) {
                                                // (c + x) - d => x + (c - d)
                                                let new_rhs = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Sub,
                                                    lhs_lhs,
                                                    rhs,
                                                );
                                                let new_add = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Add,
                                                    lhs_rhs,
                                                    new_rhs.result(ctx, 0),
                                                );
                                                inst.insert_after(ctx, new_rhs);
                                                new_rhs.insert_after(ctx, new_add);
                                                for user in dst.users(ctx) {
                                                    user.replace(ctx, dst, new_add.result(ctx, 0));
                                                }
                                                return true;
                                            }
                                        }
                                        if let ValueKind::InstResult {
                                            inst: lhs_rhs_inst, ..
                                        } = lhs_rhs.kind(ctx)
                                        {
                                            if let Ik::IConst(_) = lhs_rhs_inst.kind(ctx) {
                                                // (x + c) - d => x + (c - d)
                                                let new_rhs = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Sub,
                                                    lhs_rhs,
                                                    rhs,
                                                );
                                                let new_add = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Add,
                                                    lhs_lhs,
                                                    new_rhs.result(ctx, 0),
                                                );
                                                inst.insert_after(ctx, new_rhs);
                                                new_rhs.insert_after(ctx, new_add);
                                                for user in dst.users(ctx) {
                                                    user.replace(ctx, dst, new_add.result(ctx, 0));
                                                }
                                                return true;
                                            }
                                        }
                                    }
                                    (Op::Sub, Op::Sub) => {
                                        if let ValueKind::InstResult {
                                            inst: lhs_rhs_inst, ..
                                        } = lhs_rhs.kind(ctx)
                                        {
                                            if let Ik::IConst(_) = lhs_rhs_inst.kind(ctx) {
                                                // (x - c) - d => x - (c + d)
                                                let new_rhs = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Add,
                                                    lhs_rhs,
                                                    rhs,
                                                );
                                                let new_sub = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Sub,
                                                    lhs_lhs,
                                                    new_rhs.result(ctx, 0),
                                                );
                                                inst.insert_after(ctx, new_rhs);
                                                new_rhs.insert_after(ctx, new_sub);
                                                for user in dst.users(ctx) {
                                                    user.replace(ctx, dst, new_sub.result(ctx, 0));
                                                }
                                                return true;
                                            }
                                        }
                                    }
                                    (Op::Mul, Op::Mul) => {
                                        if let ValueKind::InstResult {
                                            inst: lhs_lhs_inst, ..
                                        } = lhs_lhs.kind(ctx)
                                        {
                                            if let Ik::IConst(_) = lhs_lhs_inst.kind(ctx) {
                                                // (c * x) * d => x * (c * d)
                                                let new_rhs = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Mul,
                                                    lhs_lhs,
                                                    rhs,
                                                );
                                                let new_mul = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Mul,
                                                    lhs_rhs,
                                                    new_rhs.result(ctx, 0),
                                                );
                                                inst.insert_after(ctx, new_rhs);
                                                new_rhs.insert_after(ctx, new_mul);
                                                for user in dst.users(ctx) {
                                                    user.replace(ctx, dst, new_mul.result(ctx, 0));
                                                }
                                                return true;
                                            }
                                        }
                                        if let ValueKind::InstResult {
                                            inst: lhs_rhs_inst, ..
                                        } = lhs_rhs.kind(ctx)
                                        {
                                            if let Ik::IConst(_) = lhs_rhs_inst.kind(ctx) {
                                                // (x * c) * d => x * (c * d)
                                                let new_rhs = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Mul,
                                                    lhs_rhs,
                                                    rhs,
                                                );
                                                let new_mul = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Mul,
                                                    lhs_lhs,
                                                    new_rhs.result(ctx, 0),
                                                );
                                                inst.insert_after(ctx, new_rhs);
                                                new_rhs.insert_after(ctx, new_mul);
                                                for user in dst.users(ctx) {
                                                    user.replace(ctx, dst, new_mul.result(ctx, 0));
                                                }
                                                return true;
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            (Ik::IConst(_), Ik::IBinary(rhs_op)) => {
                                // - `d + (x + c) => x + (c + d)`
                                // - `d + (x - c) => x + (d - c)`
                                // - `d - (x + c)`: NOT COMBINABLE, we need an additional `0 - x`,
                                //   not profitable
                                // - `d - (c - x) => x + (d - c)`
                                // - `d * (x * c) => x * (c * d)`

                                let rhs_lhs = rhs_inst.operand(ctx, 0);
                                let rhs_rhs = rhs_inst.operand(ctx, 1);

                                match (op, rhs_op) {
                                    (Op::Add, Op::Add) => {
                                        if let ValueKind::InstResult {
                                            inst: rhs_lhs_inst, ..
                                        } = rhs_lhs.kind(ctx)
                                        {
                                            if let Ik::IConst(_) = rhs_lhs_inst.kind(ctx) {
                                                // d + (c + x) => x + (c + d)
                                                let new_rhs = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Add,
                                                    rhs_lhs,
                                                    lhs,
                                                );
                                                let new_add = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Add,
                                                    rhs_rhs,
                                                    new_rhs.result(ctx, 0),
                                                );
                                                inst.insert_after(ctx, new_rhs);
                                                new_rhs.insert_after(ctx, new_add);
                                                for user in dst.users(ctx) {
                                                    user.replace(ctx, dst, new_add.result(ctx, 0));
                                                }
                                                return true;
                                            }
                                        }
                                        if let ValueKind::InstResult {
                                            inst: rhs_rhs_inst, ..
                                        } = rhs_rhs.kind(ctx)
                                        {
                                            if let Ik::IConst(_) = rhs_rhs_inst.kind(ctx) {
                                                // d + (x + c) => x + (c + d)
                                                let new_rhs = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Add,
                                                    rhs_rhs,
                                                    lhs,
                                                );
                                                let new_add = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Add,
                                                    rhs_lhs,
                                                    new_rhs.result(ctx, 0),
                                                );
                                                inst.insert_after(ctx, new_rhs);
                                                new_rhs.insert_after(ctx, new_add);
                                                for user in dst.users(ctx) {
                                                    user.replace(ctx, dst, new_add.result(ctx, 0));
                                                }
                                                return true;
                                            }
                                        }
                                    }
                                    (Op::Add, Op::Sub) => {
                                        if let ValueKind::InstResult {
                                            inst: rhs_rhs_inst, ..
                                        } = rhs_rhs.kind(ctx)
                                        {
                                            if let Ik::IConst(_) = rhs_rhs_inst.kind(ctx) {
                                                // d + (x - c) => x + (d - c)
                                                let new_rhs = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Sub,
                                                    lhs,
                                                    rhs_rhs,
                                                );
                                                let new_add = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Add,
                                                    rhs_lhs,
                                                    new_rhs.result(ctx, 0),
                                                );
                                                inst.insert_after(ctx, new_rhs);
                                                new_rhs.insert_after(ctx, new_add);
                                                for user in dst.users(ctx) {
                                                    user.replace(ctx, dst, new_add.result(ctx, 0));
                                                }
                                                return true;
                                            }
                                        }
                                    }
                                    (Op::Sub, Op::Sub) => {
                                        if let ValueKind::InstResult {
                                            inst: rhs_lhs_inst, ..
                                        } = rhs_lhs.kind(ctx)
                                        {
                                            if let Ik::IConst(_) = rhs_lhs_inst.kind(ctx) {
                                                // d - (c - x) => x + (d - c)
                                                let new_rhs = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Sub,
                                                    lhs,
                                                    rhs_lhs,
                                                );
                                                let new_add = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Add,
                                                    rhs_rhs,
                                                    new_rhs.result(ctx, 0),
                                                );
                                                inst.insert_after(ctx, new_rhs);
                                                new_rhs.insert_after(ctx, new_add);
                                                for user in dst.users(ctx) {
                                                    user.replace(ctx, dst, new_add.result(ctx, 0));
                                                }
                                                return true;
                                            }
                                        }
                                    }
                                    (Op::Mul, Op::Mul) => {
                                        if let ValueKind::InstResult {
                                            inst: rhs_lhs_inst, ..
                                        } = rhs_lhs.kind(ctx)
                                        {
                                            if let Ik::IConst(_) = rhs_lhs_inst.kind(ctx) {
                                                // d * (c * x) => x * (c * d)
                                                let new_rhs = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Mul,
                                                    rhs_lhs,
                                                    lhs,
                                                );
                                                let new_mul = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Mul,
                                                    rhs_rhs,
                                                    new_rhs.result(ctx, 0),
                                                );
                                                inst.insert_after(ctx, new_rhs);
                                                new_rhs.insert_after(ctx, new_mul);
                                                for user in dst.users(ctx) {
                                                    user.replace(ctx, dst, new_mul.result(ctx, 0));
                                                }
                                                return true;
                                            }
                                        }
                                        if let ValueKind::InstResult {
                                            inst: rhs_rhs_inst, ..
                                        } = rhs_rhs.kind(ctx)
                                        {
                                            if let Ik::IConst(_) = rhs_rhs_inst.kind(ctx) {
                                                // d * (x * c) => x * (c * d)
                                                let new_rhs = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Mul,
                                                    rhs_rhs,
                                                    lhs,
                                                );
                                                let new_mul = Inst::ibinary(
                                                    ctx,
                                                    IBinaryOp::Mul,
                                                    rhs_lhs,
                                                    new_rhs.result(ctx, 0),
                                                );
                                                inst.insert_after(ctx, new_rhs);
                                                new_rhs.insert_after(ctx, new_mul);
                                                for user in dst.users(ctx) {
                                                    user.replace(ctx, dst, new_mul.result(ctx, 0));
                                                }
                                                return true;
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
            false
        },
    }
}

/// Distributive property special case.
///
/// - `(x * c) + x` => `x * (c + 1)`
/// - `x + (x * c)` => `x * (c + 1)`
/// - `(x * c) - x` => `x * (c - 1)
/// - `x - (x * c)` => `x * (1 - c)`
///
/// c should be a constant, to reduce the complexity.
const fn distributive_one() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(op) = inst.kind(ctx) {
                if !matches!(op, IBinaryOp::Add | IBinaryOp::Sub) {
                    return false;
                }
                let op = *op;

                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                let bitwidth = dst.ty(ctx).bitwidth(ctx);

                if let ValueKind::InstResult { inst: lhs_inst, .. } = lhs.kind(ctx) {
                    if let Ik::IBinary(IBinaryOp::Mul) = lhs_inst.kind(ctx) {
                        let lhs_lhs = lhs_inst.operand(ctx, 0);
                        let lhs_rhs = lhs_inst.operand(ctx, 1);

                        if lhs_lhs == rhs {
                            // (x * c) +- x => x * (c +- 1)
                            if let ValueKind::InstResult {
                                inst: lhs_rhs_inst, ..
                            } = lhs_rhs.kind(ctx)
                            {
                                if let Ik::IConst(_) = lhs_rhs_inst.kind(ctx) {
                                    let one = Inst::iconst(
                                        ctx,
                                        IntConstant::one(bitwidth as u8),
                                        dst.ty(ctx),
                                    );
                                    let new_rhs =
                                        Inst::ibinary(ctx, op, lhs_rhs, one.result(ctx, 0));
                                    let new_mul = Inst::ibinary(
                                        ctx,
                                        IBinaryOp::Mul,
                                        lhs_lhs,
                                        new_rhs.result(ctx, 0),
                                    );
                                    inst.insert_after(ctx, one);
                                    one.insert_after(ctx, new_rhs);
                                    new_rhs.insert_after(ctx, new_mul);
                                    for user in dst.users(ctx) {
                                        user.replace(ctx, dst, new_mul.result(ctx, 0));
                                    }

                                    return true;
                                }
                            }
                        }
                        if lhs_rhs == rhs {
                            // (c * x) +- x => x * (c +- 1)
                            if let ValueKind::InstResult {
                                inst: lhs_lhs_inst, ..
                            } = lhs_lhs.kind(ctx)
                            {
                                if let Ik::IConst(_) = lhs_lhs_inst.kind(ctx) {
                                    let one = Inst::iconst(
                                        ctx,
                                        IntConstant::one(bitwidth as u8),
                                        dst.ty(ctx),
                                    );
                                    let new_rhs =
                                        Inst::ibinary(ctx, op, lhs_lhs, one.result(ctx, 0));
                                    let new_mul = Inst::ibinary(
                                        ctx,
                                        IBinaryOp::Mul,
                                        lhs_rhs,
                                        new_rhs.result(ctx, 0),
                                    );
                                    inst.insert_after(ctx, one);
                                    one.insert_after(ctx, new_rhs);
                                    new_rhs.insert_after(ctx, new_mul);
                                    for user in dst.users(ctx) {
                                        user.replace(ctx, dst, new_mul.result(ctx, 0));
                                    }

                                    return true;
                                }
                            }
                        }
                    }
                }

                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IBinary(IBinaryOp::Mul) = rhs_inst.kind(ctx) {
                        let rhs_lhs = rhs_inst.operand(ctx, 0);
                        let rhs_rhs = rhs_inst.operand(ctx, 1);

                        if rhs_lhs == lhs {
                            // x +- (x * c) => x * (1 +- c)
                            if let ValueKind::InstResult {
                                inst: rhs_rhs_inst, ..
                            } = rhs_rhs.kind(ctx)
                            {
                                if let Ik::IConst(_) = rhs_rhs_inst.kind(ctx) {
                                    let one = Inst::iconst(
                                        ctx,
                                        IntConstant::one(bitwidth as u8),
                                        dst.ty(ctx),
                                    );
                                    let new_rhs =
                                        Inst::ibinary(ctx, op, one.result(ctx, 0), rhs_rhs);
                                    let new_mul = Inst::ibinary(
                                        ctx,
                                        IBinaryOp::Mul,
                                        lhs,
                                        new_rhs.result(ctx, 0),
                                    );
                                    inst.insert_after(ctx, one);
                                    one.insert_after(ctx, new_rhs);
                                    new_rhs.insert_after(ctx, new_mul);
                                    for user in dst.users(ctx) {
                                        user.replace(ctx, dst, new_mul.result(ctx, 0));
                                    }

                                    return true;
                                }
                            }
                        }
                        if rhs_rhs == lhs {
                            // x +- (c * x) => x * (1 +- c)
                            if let ValueKind::InstResult {
                                inst: rhs_lhs_inst, ..
                            } = rhs_lhs.kind(ctx)
                            {
                                if let Ik::IConst(_) = rhs_lhs_inst.kind(ctx) {
                                    let one = Inst::iconst(
                                        ctx,
                                        IntConstant::one(bitwidth as u8),
                                        dst.ty(ctx),
                                    );
                                    let new_rhs =
                                        Inst::ibinary(ctx, op, one.result(ctx, 0), rhs_lhs);
                                    let new_mul = Inst::ibinary(
                                        ctx,
                                        IBinaryOp::Mul,
                                        lhs,
                                        new_rhs.result(ctx, 0),
                                    );
                                    inst.insert_after(ctx, one);
                                    one.insert_after(ctx, new_rhs);
                                    new_rhs.insert_after(ctx, new_mul);
                                    for user in dst.users(ctx) {
                                        user.replace(ctx, dst, new_mul.result(ctx, 0));
                                    }

                                    return true;
                                }
                            }
                        }
                    }
                }
            }

            false
        },
    }
}

/// Distributive property for strength reduction.
///
/// - `x * y + x * z => x * (y + z)`
/// - `x * y - z * y => (x - z) * y`
/// - `x * y - x * z => x * (y - z)`
/// - `x * y + z * y => (x + z) * y`
/// - `x / y + z / y => (x + z) / y`
/// - `x / y - z / y => (x - z) / y`
///
/// For constants, we should compare the underlying value, but it can actually
/// be done by global value numbering.
///
/// TODO: SDiv is considered, but how about UDiv?
const fn distributive() -> Rule {
    use IBinaryOp as Op;

    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(op) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if !matches!(op, Op::Add | Op::Sub) {
                    return false;
                }

                let op = *op;

                if let (
                    ValueKind::InstResult { inst: lhs_inst, .. },
                    ValueKind::InstResult { inst: rhs_inst, .. },
                ) = (lhs.kind(ctx), rhs.kind(ctx))
                {
                    if let (Ik::IBinary(lhs_op), Ik::IBinary(rhs_op)) =
                        (lhs_inst.kind(ctx), rhs_inst.kind(ctx))
                    {
                        if lhs_op == rhs_op && matches!(lhs_op, Op::Mul | Op::SDiv) {
                            let lhs_lhs = lhs_inst.operand(ctx, 0);
                            let lhs_rhs = lhs_inst.operand(ctx, 1);
                            let rhs_lhs = rhs_inst.operand(ctx, 0);
                            let rhs_rhs = rhs_inst.operand(ctx, 1);

                            if let Op::Mul = lhs_op {
                                if lhs_lhs == rhs_lhs {
                                    // x * y + x * z => x * (y + z)
                                    // x * y - x * z => x * (y - z)
                                    let new_rhs = Inst::ibinary(ctx, op, lhs_rhs, rhs_rhs);
                                    let new_mul = Inst::ibinary(
                                        ctx,
                                        Op::Mul,
                                        lhs_lhs,
                                        new_rhs.result(ctx, 0),
                                    );
                                    inst.insert_after(ctx, new_rhs);
                                    new_rhs.insert_after(ctx, new_mul);
                                    for user in dst.users(ctx) {
                                        user.replace(ctx, dst, new_mul.result(ctx, 0));
                                    }
                                    return true;
                                } else if lhs_lhs == rhs_rhs {
                                    // x * y + z * x => x * (y + z)
                                    // x * y - z * x => x * (y - z)
                                    let new_rhs = Inst::ibinary(ctx, op, lhs_rhs, rhs_lhs);
                                    let new_mul = Inst::ibinary(
                                        ctx,
                                        Op::Mul,
                                        lhs_lhs,
                                        new_rhs.result(ctx, 0),
                                    );
                                    inst.insert_after(ctx, new_rhs);
                                    new_rhs.insert_after(ctx, new_mul);
                                    for user in dst.users(ctx) {
                                        user.replace(ctx, dst, new_mul.result(ctx, 0));
                                    }
                                    return true;
                                } else if lhs_rhs == rhs_lhs {
                                    // y * x + x * z => x * (y + z)
                                    // y * x - x * z => x * (y - z)
                                    let new_rhs = Inst::ibinary(ctx, op, lhs_lhs, rhs_rhs);
                                    let new_mul = Inst::ibinary(
                                        ctx,
                                        Op::Mul,
                                        lhs_rhs,
                                        new_rhs.result(ctx, 0),
                                    );
                                    inst.insert_after(ctx, new_rhs);
                                    new_rhs.insert_after(ctx, new_mul);
                                    for user in dst.users(ctx) {
                                        user.replace(ctx, dst, new_mul.result(ctx, 0));
                                    }
                                    return true;
                                } else if lhs_rhs == rhs_rhs {
                                    // y * x + z * x => x * (y + z)
                                    // y * x - z * x => x * (y - z)
                                    let new_rhs = Inst::ibinary(ctx, op, lhs_lhs, rhs_lhs);
                                    let new_mul = Inst::ibinary(
                                        ctx,
                                        Op::Mul,
                                        lhs_rhs,
                                        new_rhs.result(ctx, 0),
                                    );
                                    inst.insert_after(ctx, new_rhs);
                                    new_rhs.insert_after(ctx, new_mul);
                                    for user in dst.users(ctx) {
                                        user.replace(ctx, dst, new_mul.result(ctx, 0));
                                    }
                                    return true;
                                }
                            }

                            if let Op::SDiv = lhs_op {
                                if lhs_rhs == rhs_rhs {
                                    // x / y + z / y => (x + z) / y
                                    // x / y - z / y => (x - z) / y
                                    let new_lhs = Inst::ibinary(ctx, op, lhs_lhs, rhs_lhs);
                                    let new_div = Inst::ibinary(
                                        ctx,
                                        Op::SDiv,
                                        new_lhs.result(ctx, 0),
                                        lhs_rhs,
                                    );
                                    inst.insert_after(ctx, new_lhs);
                                    new_lhs.insert_after(ctx, new_div);
                                    for user in dst.users(ctx) {
                                        user.replace(ctx, dst, new_div.result(ctx, 0));
                                    }
                                    return true;
                                }
                            }
                        }
                    }
                }
            }
            false
        },
    }
}

/// Move same instructions' same operands together,
/// instruction that can be moved together must be associative.
///
/// `(x @ y) @ x` or `x @ (x @ y)` or `x @ (y @ x)` or `(y @ x) @ x` => `(x @ x)
/// @ y`
///
/// note: this rule better to run after add_to_mul
const fn mv_same_together() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if inst.is_associative(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);
                let inst_kind = inst.kind(ctx).clone();
                let ty = dst.ty(ctx);

                let mut insts_to_move =
                    if let ValueKind::InstResult { inst: lhs_inst, .. } = lhs.kind(ctx) {
                        if lhs_inst.kind(ctx) == &inst_kind {
                            let lhs_lhs = lhs_inst.operand(ctx, 0);
                            let lhs_rhs = lhs_inst.operand(ctx, 1);
                            if lhs_lhs == rhs {
                                Some((rhs, lhs_rhs))
                            } else if lhs_rhs == rhs {
                                Some((rhs, lhs_lhs))
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    } else {
                        None
                    };
                if insts_to_move.is_none() {
                    insts_to_move =
                        if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                            if rhs_inst.kind(ctx) == &inst_kind {
                                let rhs_lhs = rhs_inst.operand(ctx, 0);
                                let rhs_rhs = rhs_inst.operand(ctx, 1);
                                if rhs_lhs == lhs {
                                    Some((lhs, rhs_rhs))
                                } else if rhs_rhs == lhs {
                                    Some((lhs, rhs_lhs))
                                } else {
                                    None
                                }
                            } else {
                                None
                            }
                        } else {
                            None
                        };
                }
                if let Some((twice, other)) = insts_to_move {
                    let inst_inner =
                        Inst::new(ctx, inst_kind.clone(), vec![ty], vec![twice, twice]);
                    let inst_outer = Inst::new(
                        ctx,
                        inst_kind.clone(),
                        vec![ty],
                        vec![inst_inner.result(ctx, 0), other],
                    );
                    inst.insert_after(ctx, inst_inner);
                    inst_inner.insert_after(ctx, inst_outer);
                    for user in dst.users(ctx) {
                        user.replace(ctx, dst, inst_outer.result(ctx, 0));
                    }
                    return true;
                } else {
                    return false;
                }
            }
            false
        },
    }
}

/// Distribute add/sub & mul with constants, create more opportunities for
/// induction variables.
///
/// `(x +- c1) * c2` => x * c2 +- c1 * c2
const fn redistribute_const() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(IBinaryOp::Mul) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: lhs_inst, .. } = lhs.kind(ctx) {
                    if let Ik::IBinary(lhs_op) = lhs_inst.kind(ctx) {
                        let lhs_lhs = lhs_inst.operand(ctx, 0);
                        let lhs_rhs = lhs_inst.operand(ctx, 1);
                        let lhs_op = *lhs_op;

                        if lhs_op != IBinaryOp::Add && lhs_op != IBinaryOp::Sub {
                            return false;
                        }

                        if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                            if let Ik::IConst(_) = rhs_inst.kind(ctx) {
                                if let ValueKind::InstResult {
                                    inst: lhs_rhs_inst, ..
                                } = lhs_rhs.kind(ctx)
                                {
                                    if let Ik::IConst(_) = lhs_rhs_inst.kind(ctx) {
                                        let new_lhs =
                                            Inst::ibinary(ctx, IBinaryOp::Mul, lhs_lhs, rhs);
                                        let new_rhs =
                                            Inst::ibinary(ctx, IBinaryOp::Mul, lhs_rhs, rhs);
                                        let new_add = Inst::ibinary(
                                            ctx,
                                            lhs_op,
                                            new_lhs.result(ctx, 0),
                                            new_rhs.result(ctx, 0),
                                        );
                                        inst.insert_after(ctx, new_lhs);
                                        new_lhs.insert_after(ctx, new_rhs);
                                        new_rhs.insert_after(ctx, new_add);
                                        for user in dst.users(ctx) {
                                            user.replace(ctx, dst, new_add.result(ctx, 0));
                                        }
                                        return true;
                                    }
                                }
                            }
                        }
                    }
                }
            }
            false
        },
    }
}

/// Associate more.
///
/// (x +- c1) +- (y +- c2) => (x +- y) +- (c1 +- c2)
const fn reassociate() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            if let Ik::IBinary(op) = inst.kind(ctx) {
                let lhs = inst.operand(ctx, 0);
                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);
                let op = *op;

                if let ValueKind::InstResult { inst: lhs_inst, .. } = lhs.kind(ctx) {
                    if let Ik::IBinary(lhs_op) = lhs_inst.kind(ctx) {
                        let lhs_lhs = lhs_inst.operand(ctx, 0);
                        let lhs_rhs = lhs_inst.operand(ctx, 1);
                        let lhs_op = *lhs_op;

                        if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                            if let Ik::IBinary(rhs_op) = rhs_inst.kind(ctx) {
                                let rhs_lhs = rhs_inst.operand(ctx, 0);
                                let rhs_rhs = rhs_inst.operand(ctx, 1);
                                let rhs_op = *rhs_op;

                                if let ValueKind::InstResult {
                                    inst: lhs_rhs_inst, ..
                                } = lhs_rhs.kind(ctx)
                                {
                                    if let ValueKind::InstResult {
                                        inst: rhs_rhs_inst, ..
                                    } = rhs_rhs.kind(ctx)
                                    {
                                        if let Ik::IConst(_) = lhs_rhs_inst.kind(ctx) {
                                            if let Ik::IConst(_) = rhs_rhs_inst.kind(ctx) {
                                                // (x + c1) + (y + c2) => (x + y) + (c1 + c2)
                                                // (x + c1) + (y - c2) => (x + y) + (c1 - c2)
                                                // (x - c1) + (y - c2) => (x + y) - (c1 + c2)
                                                // (x - c1) + (y + c2) => (x + y) - (c1 - c2)
                                                // (x + c1) - (y + c2) => (x - y) + (c1 - c2)
                                                // (x + c1) - (y - c2) => (x - y) + (c1 + c2)
                                                // (x - c1) - (y - c2) => (x - y) - (c1 - c2)
                                                // (x - c1) - (y + c2) => (x - y) - (c1 + c2)

                                                let new_lhs_op = op;
                                                let new_op = lhs_op;
                                                let new_rhs_op = match (lhs_op, op, rhs_op) {
                                                    (
                                                        IBinaryOp::Add,
                                                        IBinaryOp::Add,
                                                        IBinaryOp::Add,
                                                    ) => IBinaryOp::Add,
                                                    (
                                                        IBinaryOp::Add,
                                                        IBinaryOp::Add,
                                                        IBinaryOp::Sub,
                                                    ) => IBinaryOp::Sub,
                                                    (
                                                        IBinaryOp::Sub,
                                                        IBinaryOp::Add,
                                                        IBinaryOp::Sub,
                                                    ) => IBinaryOp::Add,
                                                    (
                                                        IBinaryOp::Sub,
                                                        IBinaryOp::Add,
                                                        IBinaryOp::Add,
                                                    ) => IBinaryOp::Sub,
                                                    (
                                                        IBinaryOp::Add,
                                                        IBinaryOp::Sub,
                                                        IBinaryOp::Add,
                                                    ) => IBinaryOp::Sub,
                                                    (
                                                        IBinaryOp::Add,
                                                        IBinaryOp::Sub,
                                                        IBinaryOp::Sub,
                                                    ) => IBinaryOp::Add,
                                                    (
                                                        IBinaryOp::Sub,
                                                        IBinaryOp::Sub,
                                                        IBinaryOp::Sub,
                                                    ) => IBinaryOp::Sub,
                                                    (
                                                        IBinaryOp::Sub,
                                                        IBinaryOp::Sub,
                                                        IBinaryOp::Add,
                                                    ) => IBinaryOp::Add,
                                                    _ => return false,
                                                };

                                                let new_lhs = Inst::ibinary(
                                                    ctx, new_lhs_op, lhs_lhs, rhs_lhs,
                                                );
                                                let new_rhs = Inst::ibinary(
                                                    ctx, new_rhs_op, lhs_rhs, rhs_rhs,
                                                );
                                                let new_inst = Inst::ibinary(
                                                    ctx,
                                                    new_op,
                                                    new_lhs.result(ctx, 0),
                                                    new_rhs.result(ctx, 0),
                                                );

                                                inst.insert_after(ctx, new_lhs);
                                                new_lhs.insert_after(ctx, new_rhs);
                                                new_rhs.insert_after(ctx, new_inst);
                                                for user in dst.users(ctx) {
                                                    user.replace(ctx, dst, new_inst.result(ctx, 0));
                                                }

                                                return true;
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
            false
        },
    }
}
