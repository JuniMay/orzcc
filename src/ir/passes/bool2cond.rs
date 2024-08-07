//! Convert 0/1 to condition value.
//!
//! ```orzir
//! ^bb0:
//!     br %cond, ^bb1, ^bb2
//!
//! ^bb1:
//!     // use 1
//!
//! ^bb2:
//!     // use 0
//! ```
//!
//! We can zero-extend the condition to be the same type of 1/0, and replace the
//! use of 1/0 with the condition, so that ^bb1 and ^bb2 might be exactly the
//! same and we can merge them later.

use std::collections::{HashMap, HashSet};

use super::control_flow::CfgCanonicalize;
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassManager, PassResult, TransformPass},
        CastOp,
        Context,
        Func,
        Inst,
    },
    utils::def_use::User,
};

pub const BOOL2COND: &str = "bool2cond";

pub struct Bool2Cond;

impl LocalPassMut for Bool2Cond {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        let mut cursor = func.cursor();

        while let Some(block) = cursor.next(ctx) {
            let tail = block.tail(ctx).unwrap();

            if !tail.is_br(ctx) {
                continue;
            }

            let cond = tail.operand(ctx, 0);

            let succ_then = tail.succ(ctx, 0).block();
            let succ_else = tail.succ(ctx, 1).block();

            // share only one predecessor, the current block
            if succ_then.preds(ctx).len() != 1 || succ_else.preds(ctx).len() != 1 {
                continue;
            }

            if succ_then.insn(ctx) != succ_else.insn(ctx) {
                // different instruction number, even if we replace with cond, we cannot merge.
                continue;
            }

            let mut ty2zext = HashMap::new();

            let mut cursor = succ_then.cursor();
            while let Some(inst) = cursor.next(ctx) {
                let operands = inst.operands(ctx);

                let mut replaced = HashSet::new();

                // check all the operands' def inst, see if it is one.
                for operand in operands {
                    if replaced.contains(&operand) {
                        continue;
                    }
                    let constant = operand
                        .def_inst(ctx)
                        .and_then(|inst| inst.get_iconst_value(ctx));
                    if let Some(iconst) = constant {
                        if iconst.is_one() {
                            let ty = operand.ty(ctx);
                            if ty == cond.ty(ctx) {
                                inst.replace(ctx, operand, cond);
                            } else {
                                // zero-extend the condition to be the same type of 1/0.
                                let zext = ty2zext.entry(ty).or_insert_with(|| {
                                    let zext = Inst::cast(ctx, CastOp::ZExt, cond, ty);
                                    tail.insert_before(ctx, zext);
                                    zext.result(ctx, 0)
                                });
                                inst.replace(ctx, operand, *zext);
                            }
                            // `replace` will replace all, so only call once for each `one`
                            // operand.
                            replaced.insert(operand);
                            changed = true;
                        }
                    }
                }
            }

            let mut cursor = succ_else.cursor();
            while let Some(inst) = cursor.next(ctx) {
                let operands = inst.operands(ctx);

                let mut replaced = HashSet::new();

                // check all the operands' def inst, see if it is one.
                for operand in operands {
                    if replaced.contains(&operand) {
                        continue;
                    }
                    let constant = operand
                        .def_inst(ctx)
                        .and_then(|inst| inst.get_iconst_value(ctx));
                    if let Some(iconst) = constant {
                        if iconst.is_zero() {
                            let ty = operand.ty(ctx);
                            if ty == cond.ty(ctx) {
                                inst.replace(ctx, operand, cond);
                            } else {
                                // zero-extend the condition to be the same type of 1/0.
                                let zext = ty2zext.entry(ty).or_insert_with(|| {
                                    let zext = Inst::cast(ctx, CastOp::ZExt, cond, ty);
                                    tail.insert_before(ctx, zext);
                                    zext.result(ctx, 0)
                                });
                                inst.replace(ctx, operand, *zext);
                            }
                            // `replace` will replace all, so only call once for each `one`
                            // operand.
                            replaced.insert(operand);
                            changed = true;
                        }
                    }
                }
            }
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for Bool2Cond {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;
        for func in ctx.funcs() {
            let (_, local_changed) = LocalPassMut::run(self, ctx, func)?;
            changed |= local_changed;
        }
        Ok(((), changed))
    }
}

impl TransformPass for Bool2Cond {
    fn register(passman: &mut PassManager) {
        passman.register_transform(BOOL2COND, Bool2Cond, vec![Box::new(CfgCanonicalize)]);
    }
}
