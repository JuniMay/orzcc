use rustc_hash::FxHashMap;

use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passman::{GlobalPass, LocalPass, PassResult},
        Block,
        Context,
        Func,
        IBinaryOp,
        ICmpCond,
        Inst,
        InstKind,
        Value,
    },
    utils::{
        cfg::{CfgInfo, CfgNode},
        def_use::Usable,
        dominance::Dominance,
        loop_info::{Loop, LoopContext},
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum InductionOp {
    Add,
    Sub,
    Mul,
    SDiv,
    Shl,
}

impl TryFrom<IBinaryOp> for InductionOp {
    type Error = ();

    fn try_from(value: IBinaryOp) -> Result<Self, Self::Error> {
        #[allow(clippy::wildcard_enum_match_arm)]
        match value {
            IBinaryOp::Add => Ok(Self::Add),
            IBinaryOp::Sub => Ok(Self::Sub),
            IBinaryOp::Mul => Ok(Self::Mul),
            IBinaryOp::SDiv => Ok(Self::SDiv),
            IBinaryOp::Shl => Ok(Self::Shl),
            _ => Err(()),
        }
    }
}

/// Basic recurrence record.
#[derive(Debug, Clone)]
pub struct Scev {
    /// The representative value of this induction variable, typically the loop
    /// parameter.
    pub block_param: Value,
    /// The start value of this induction variable.
    pub init: Value,
    /// The evolving step of this induction variable.
    pub step: Value,
    /// The evolution method of this induction variable.
    pub op: InductionOp,
    /// The modulus of this induction variable.
    pub modulus: Option<Value>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum LoopBoundCond {
    Slt,
    Sle,
    Sgt,
    Sge,
}

/// A record for loop bound.
pub struct LoopBound {
    /// The block parameter representing the basic recurrence.
    pub block_param: Value,
    /// The instruction for comparison
    pub cmp_inst: Inst,
    /// The comparison condition
    pub cond: LoopBoundCond,
    /// The bound value
    pub bound: Value,
    /// If this is a reversed comparison, i.e., bound is the lhs in the
    /// instruction.
    pub reversed: bool,
}

pub struct DisplayScev<'a> {
    ctx: &'a Context,
    scev: &'a Scev,
}

impl<'a> std::fmt::Display for DisplayScev<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let start = self.scev.init.name(self.ctx).unwrap();
        let step = self.scev.step.name(self.ctx).unwrap();

        let modulus = self
            .scev
            .modulus
            .map(|v| v.name(self.ctx).unwrap().clone())
            .unwrap_or("none".to_string());

        let op = match self.scev.op {
            InductionOp::Add => "add",
            InductionOp::Sub => "sub",
            InductionOp::Mul => "mul",
            InductionOp::SDiv => "sdiv",
            InductionOp::Shl => "shl",
        };

        write!(f, "{} {} {} (mod {})", start, op, step, modulus)
    }
}

impl Scev {
    pub fn display<'a>(&'a self, ctx: &'a Context) -> DisplayScev<'a> {
        DisplayScev { ctx, scev: self }
    }
}

/// The analysis pass of scalar evolution.
///
/// This analysis currently only do the most basic detection of induction
/// variables. No chain of recurrences, no nested SCEVs, just simple binary
/// operations.
#[derive(Default)]
pub struct ScevAnalysis {
    pub(super) loops: LoopContext<Block>,
    pub(super) dominance: Dominance<Block>,
    indvars: FxHashMap<Value, Scev>,
}

impl ScevAnalysis {
    fn process_loop(&mut self, ctx: &Context, lp: Loop<Block>) {
        // after loop-simplify, the header should have exactly two predecessors. one is
        // the preheader, the other is the latch.
        let header = lp.header(&self.loops);

        for param in header.params(ctx) {
            // all the block params in the header are suspected to be induction variables.

            // firstly, determine the start and evolving value of the induction variable.
            let mut start = None;
            let mut evolving = None;

            // should be exactly two users, preheader and dedicated exit.
            for pred_inst in header.users(ctx) {
                let incomings = pred_inst
                    .succ_to(ctx, header)
                    // expect this succ to be legal, so unwrap the incoming argument.
                    .map(|succ| succ.get_arg(*param).unwrap())
                    .collect::<Vec<_>>();

                if incomings.len() != 1 {
                    // if there are multiple incoming values, i.e., `br %cond, %header(%1),
                    // %header(%2)`, we cannot decide which one is the desired induction result.
                    // this should not happen if loop-simplify is done correctly, but check it
                    // anyway.
                    continue;
                }

                let incoming = incomings[0];

                let pred_block = pred_inst.container(ctx).unwrap();

                if self.loops.is_in_loop(pred_block, lp) {
                    // the incoming value is from inside the loop, it should be the evolving
                    // value of the induction variable.
                    evolving = Some(incoming);
                } else {
                    // the incoming value is from outside the loop, it should be the start
                    // value of the induction variable.
                    start = Some(incoming);
                }
            }

            if start.is_none() || evolving.is_none() {
                // if we cannot find both the start and evolving value, we cannot determine if
                // this is an induction variable.
                continue;
            }

            let init = start.unwrap();
            let evolving = evolving.unwrap();

            // secondly, find the induction operation.
            //
            // we only care about the evolution of the induction variable, not the upper
            // bound. the upper bound should be handled when running indvar simplification.
            //
            // The instruction should be `%evolving = <op> %param, %step`, where `%step` is
            // an loop-invariant value. Here we simply check if the `%step` is defined
            // outside the loop, and let LICM/GCM to do the preparation
            let def_inst = if let Some(def_inst) = evolving.def_inst(ctx) {
                def_inst
            } else {
                // the evolution is not defined by an instruction, this is not an induction
                // variable.
                continue;
            };

            if let InstKind::IBinary(op) = def_inst.kind(ctx) {
                let lhs = def_inst.operand(ctx, 0);
                let rhs = def_inst.operand(ctx, 1);

                let step = if lhs == *param {
                    rhs
                } else if rhs == *param {
                    lhs
                } else {
                    // the evolution is not in the form of `%evolving = <op> %param, %step`
                    continue;
                };

                if self.loops.is_in_loop(step.def_block(ctx), lp) {
                    // the step is not defined outside the loop, this is not an induction variable.
                    // again, this is a conservative approach to check, we should let LICM to do the
                    // preparation.
                    continue;
                }

                let ind_op = if let Ok(ind_op) = InductionOp::try_from(*op) {
                    ind_op
                } else if let IBinaryOp::SRem = op {
                    // TODO maybe we want to support the arithmetic on some abstract domain. but
                    // that requires the integer range analysis to be done first.
                    continue;
                } else {
                    // the operation is not supported, even if it is a valid binary operation.
                    println!("[ scev-analysis ] unsupported induction operation: {}", op);
                    continue;
                };

                let indvar = Scev {
                    block_param: *param,
                    init,
                    step,
                    op: ind_op,
                    modulus: None,
                };

                self.indvars.insert(*param, indvar);
            }
        }
    }

    pub fn find_loop_bound(&mut self, ctx: &Context, lp: Loop<Block>) -> Option<LoopBound> {
        let exits = lp.get_exit_blocks(ctx, &self.loops);

        if exits.len() != 1 {
            // we only support single exit loop.
            println!("[ scev-analysis ] multiple exits in loop, not supported.");
            return None;
        }

        let header = lp.header(&self.loops);

        let mut pre_exit = false;
        for succ in header.succs(ctx) {
            if exits.contains(&succ) {
                pre_exit = true;
                break;
            }
        }
        if !pre_exit {
            println!("[ scev-analysis ] header is not pre-exit block, not supported");
            return None;
        }

        // this should be a branch instruction.
        let tail = header.tail(ctx).unwrap();

        let mut loop_bound = None;

        if let InstKind::Br = tail.kind(ctx) {
            let cond = tail.operand(ctx, 0);

            if let Some(inst) = cond.def_inst(ctx) {
                if let InstKind::IBinary(IBinaryOp::Cmp(cond @ (ICmpCond::Slt | ICmpCond::Sle))) =
                    inst.kind(ctx)
                {
                    let lhs = inst.operand(ctx, 0);
                    let rhs = inst.operand(ctx, 1);

                    // the compare form should be:
                    // 1. indvar <op> invariant
                    // 2. invariant <op> indvar (revsersed)
                    let (indvar, invariant, reversed) = if self.indvars.contains_key(&lhs) {
                        // also check if the bound is loop-invariant.
                        let def_block = rhs.def_block(ctx);
                        if self.loops.is_in_loop(def_block, lp) {
                            return None;
                        }
                        (lhs, rhs, false)
                    } else if self.indvars.contains_key(&rhs) {
                        let def_block = lhs.def_block(ctx);
                        if self.loops.is_in_loop(def_block, lp) {
                            return None;
                        }
                        (rhs, lhs, true)
                    } else {
                        // the compare is not related to the loop parameter.
                        return None;
                    };

                    let cond = if *cond == ICmpCond::Slt && !reversed {
                        LoopBoundCond::Slt
                    } else if *cond == ICmpCond::Sle && !reversed {
                        LoopBoundCond::Sle
                    } else if *cond == ICmpCond::Slt && reversed {
                        LoopBoundCond::Sgt
                    } else if *cond == ICmpCond::Sle && reversed {
                        LoopBoundCond::Sge
                    } else {
                        unreachable!()
                    };

                    loop_bound = Some(LoopBound {
                        block_param: indvar,
                        cmp_inst: inst,
                        cond,
                        bound: invariant,
                        reversed,
                    });
                }
            }
        }

        loop_bound
    }
}

#[derive(Default)]
pub struct LoopScevRecord {
    /// The loop parameter.
    pub loop_bounds: FxHashMap<Loop<Block>, Option<LoopBound>>,
    /// The detected loop induction variables.
    pub scevs: FxHashMap<Value, Scev>,
}

pub struct DisplayLoopScevRecord<'a> {
    ctx: &'a Context,
    record: &'a LoopScevRecord,
}

impl<'a> std::fmt::Display for DisplayLoopScevRecord<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (_, bound) in self.record.loop_bounds.iter() {
            if let Some(LoopBound {
                block_param,
                cond,
                bound,
                reversed,
                ..
            }) = bound
            {
                let param_name = block_param.name(self.ctx).unwrap();
                let bound_name = bound.name(self.ctx).unwrap();

                let cond = match cond {
                    LoopBoundCond::Slt => "slt",
                    LoopBoundCond::Sle => "sle",
                    LoopBoundCond::Sgt => "sgt",
                    LoopBoundCond::Sge => "sge",
                };

                writeln!(
                    f,
                    "{} {} {} {}",
                    param_name,
                    cond,
                    bound_name,
                    if *reversed { "reversed" } else { "" }
                )?;
            }
        }

        for (param, scev) in self.record.scevs.iter() {
            let param_name = param.name(self.ctx).unwrap();
            writeln!(f, "{}: {}", param_name, scev.display(self.ctx))?;
        }

        Ok(())
    }
}

impl LoopScevRecord {
    pub fn display<'a>(&'a self, ctx: &'a Context) -> DisplayLoopScevRecord<'a> {
        DisplayLoopScevRecord { ctx, record: self }
    }
}

impl LocalPass for ScevAnalysis {
    type Output = LoopScevRecord;

    fn run(&mut self, ctx: &Context, func: Func) -> PassResult<Self::Output> {
        let cfg = CfgInfo::new(ctx, func);

        self.dominance = Dominance::new(ctx, &cfg);
        self.loops = LoopContext::new(&cfg, &self.dominance);
        self.indvars.clear();

        let mut result = LoopScevRecord::default();

        for lp in self.loops.loops() {
            self.process_loop(ctx, lp);
            let bound = self.find_loop_bound(ctx, lp);
            result.loop_bounds.insert(lp, bound);
        }

        result.scevs = self.indvars.drain().collect();

        Ok(result)
    }
}

impl GlobalPass for ScevAnalysis {
    type Output = FxHashMap<Func, LoopScevRecord>;

    fn run(&mut self, ctx: &Context) -> PassResult<Self::Output> {
        let mut result = FxHashMap::default();
        for func in ctx.funcs() {
            result.insert(func, LocalPass::run(self, ctx, func)?);
        }
        Ok(result)
    }
}
