//! Instruction Combining Pass
//!
//! The instruction combining is extensible with [Rule]s. Each rule is a
//! function that takes an instruction and returns a boolean. If the rule
//! matches, the instruction is modified in place and the function returns
//! `true`. The rule should not remove the instruction, but can modify the
//! def-use chain of the instruction result.

// TODO: Now this pass only contains a few simple rules, we need to add more
// rules to make it more powerful.
//
// TODO: Some rules **MIGHT** be applicable to floating-point instructions.
//
// TODO: Some simplification might extend the liverange of the value, which can
// potentially increase the register pressure. Maybe we need a `sink` pass to
// sink the instructions.
//
// TODO: We are not sure about the sequence of the rules. Theoretically,
// because of the iterative feature of the pass manager, the sequence of the
// rules should not matter, but we need to test it.
//
// TODO: There are aggressive rules in this pass, maybe we should separate them
// from the non-aggressive ones.
//
// TODO: Find a way to test these rules one by one.

use crate::{
    collections::{
        apint::ApInt,
        linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    },
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        Context,
        Func,
        IBinaryOp,
        Inst,
        InstKind as Ik,
        ValueKind,
    },
    utils::def_use::{Usable, User},
};

pub const INSTCOMBINE: &str = "instcombine";

/// A rule for instcombine.
struct Rule {
    /// The rewriter function.
    ///
    /// If this rule matches, this function should modify the instruction in
    /// place and return `true`.
    rewriter: fn(&mut Context, Inst) -> bool,
}

pub struct InstCombine {
    rules: Vec<Rule>,
}

impl Default for InstCombine {
    fn default() -> Self {
        Self {
            rules: vec![
                sub_identity_to_zero(), // aggressive
                mul_zero_elim(),
                mul_one_elim(),
                mv_const_rhs(),
                add_zero_elim(),
                assoc_sub_zero(),
                sub_zero_elim(),
                offset_zero_elim(),
                add_to_mul(),
                mul_to_shl(),
                combine_const(), // aggressive
                distributive(),  // aggressive
            ],
        }
    }
}

impl LocalPassMut for InstCombine {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        let mut cursor = func.cursor();
        while let Some(block) = cursor.next(ctx) {
            let mut cursor = block.cursor();
            while let Some(inst) = cursor.next(ctx) {
                if !inst.is_used(ctx) {
                    // if the instruction has no uses, we can move to the next instruction.
                    continue;
                }
                for rule in &self.rules {
                    if (rule.rewriter)(ctx, inst) {
                        changed = true;
                        if !inst.is_used(ctx) {
                            // if the modified instruction has no uses, we can move to the next
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

impl GlobalPassMut for InstCombine {
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

impl TransformPass for InstCombine {
    fn register(passman: &mut crate::ir::passman::PassManager) {
        let pass = Self::default();
        passman.register_transform(INSTCOMBINE, pass, Vec::new());
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

                let bitwidth = lhs.ty(ctx).bitwidth(ctx).unwrap();

                let rhs = inst.operand(ctx, 1);
                let dst = inst.result(ctx, 0);

                if let ValueKind::InstResult { inst: rhs_inst, .. } = rhs.kind(ctx) {
                    if let Ik::IConst(v) = rhs_inst.kind(ctx) {
                        if v.is_power_of_two() {
                            let shamt = ApInt::from(v.trailing_zeros())
                                .into_shrunk()
                                .into_zeroext(bitwidth);
                            let i_shamt = Inst::iconst(ctx, shamt, dst.ty(ctx));
                            let i_shl =
                                Inst::ibinary(ctx, IBinaryOp::Shl, lhs, i_shamt.result(ctx, 0));
                            inst.insert_after(ctx, i_shamt);
                            i_shamt.insert_after(ctx, i_shl);
                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, i_shl.result(ctx, 0));
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

                if lhs == rhs {
                    let two = Inst::iconst(ctx, ApInt::from(2), dst.ty(ctx));
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

                let bitwidth = dst.ty(ctx).bitwidth(ctx).unwrap();

                if lhs == rhs {
                    let zero = Inst::iconst(
                        ctx,
                        ApInt::zero(dst.ty(ctx).bitwidth(ctx).unwrap()),
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
                            let zero = Inst::iconst(ctx, ApInt::zero(bitwidth), dst.ty(ctx));
                            let neg_rhs =
                                Inst::ibinary(ctx, IBinaryOp::Sub, zero.result(ctx, 0), rhs_rhs);

                            inst.insert_after(ctx, zero);
                            zero.insert_after(ctx, neg_rhs);

                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, neg_rhs.result(ctx, 0));
                            }
                        } else if lhs == rhs_rhs {
                            // x - (y + x) => 0 - y
                            let zero = Inst::iconst(ctx, ApInt::zero(bitwidth), dst.ty(ctx));
                            let neg_rhs =
                                Inst::ibinary(ctx, IBinaryOp::Sub, zero.result(ctx, 0), rhs_lhs);

                            inst.insert_after(ctx, zero);
                            zero.insert_after(ctx, neg_rhs);

                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, neg_rhs.result(ctx, 0));
                            }
                        }
                        return true;
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
                        } else if rhs == lhs_rhs {
                            // (y + x) - x => y
                            for user in dst.users(ctx) {
                                user.replace(ctx, dst, lhs_lhs);
                            }
                        }
                        return true;
                    }
                }
            }
            false
        },
    }
}

/// Combine constants.
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
const fn combine_const() -> Rule {
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

/// Distributive property for strength reduction.
///
/// - `x * y + x * z => x * (y + z)`
/// - `x * y - z * y => (x + z) * y`
/// - `x * y - x * z => x * (y - z)`
/// - `x * y + z * y => (x + z) * y`
/// - `x / y + z / y => (x + z) / y`
/// - `x / y - z / y => (x - z) / y`
///
/// Special case:
///
/// - `(x * y) + x` => `x * (y + 1)`
/// - `x + (x * y)` => `x * (y + 1)`
///
/// For constants, we should compare the underlying value.
const fn distributive() -> Rule {
    Rule {
        rewriter: |ctx, inst| {
            // TODO: implement this
            false
        },
    }
}