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
                mv_const_rhs(),
                add_zero_elim(),
                offset_zero_elim(),
                mul_to_shl(),
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
