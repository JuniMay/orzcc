use rustc_hash::FxHashMap;

use crate::{
    collections::{linked_list::LinkedListNodePtr, union_find::UnionFind},
    ir::{
        passman::{GlobalPass, LocalPass, PassResult},
        Block,
        Context,
        Func,
        IBinaryOp,
        InstKind,
        Value,
    },
    utils::{
        cfg::CfgInfo,
        def_use::Usable,
        dominance::Dominance,
        loop_info::{Loop, LoopContext},
    },
};

pub enum InductionOp {
    Add,
    Sub,
    Mul,
    SDiv,
    UDiv,
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
            IBinaryOp::UDiv => Ok(Self::UDiv),
            IBinaryOp::Shl => Ok(Self::Shl),
            _ => Err(()),
        }
    }
}

/// A record of an induction variable.
pub struct Scev {
    /// The start value of this induction variable.
    pub start: Value,
    /// The evolving step of this induction variable.
    pub step: Value,
    /// The evolution method of this induction variable.
    pub op: InductionOp,
    /// The modulus of this induction variable.
    pub modulus: Option<Value>,
}

pub struct DisplayScev<'a> {
    ctx: &'a Context,
    scev: &'a Scev,
}

impl<'a> std::fmt::Display for DisplayScev<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let start = self.scev.start.name(self.ctx).unwrap();
        let step = self.scev.step.name(self.ctx).unwrap();

        let modulus = self
            .scev
            .modulus
            .map(|v| v.name(self.ctx).unwrap().clone())
            .unwrap_or("none".to_string());

        let op = match self.scev.op {
            InductionOp::Add => "+",
            InductionOp::Sub => "-",
            InductionOp::Mul => "*",
            InductionOp::SDiv => "/",
            InductionOp::UDiv => "/",
            InductionOp::Shl => "<<",
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
#[derive(Default)]
pub struct ScevAnalysis {
    loop_ctx: LoopContext<Block>,
    dominance: Dominance<Block>,
    // TODO: union-find might be useful for some complex tree-shaped operations, and maybe we can
    // re-associate the operations to get a indvar expression.
    _indvars: UnionFind<Value>,

    indvars: Vec<Scev>,
}

impl ScevAnalysis {
    fn process_loop(&mut self, ctx: &Context, lp: Loop<Block>) {
        // after loop-simplify, the header should have exactly two predecessors. one is
        // the preheader, the other is the latch.
        let header = lp.header(&self.loop_ctx);

        for param in header.params(ctx) {
            // all the block params in the header are suspected to be induction variables.

            // firstly, determine the start and evolving value of the induction variable.
            let mut start = None;
            let mut evolving = None;

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

                if self.loop_ctx.is_in_loop(pred_block, lp) {
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

            let start = start.unwrap();
            let evolving = evolving.unwrap();

            // secondly, find the induction operation.
            //
            // we only care about the evolution of the induction variable, not the upper
            // bound. the upper bound should be handled when running indvar simplification.
            //
            // The instruction should be `%evolving = <op> %param, %step`, where `%step` is
            // an loop-invariant value. Here we simply check if the `%step` is defined
            // outside the loop, and let LICM to do the preparation
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

                if self.loop_ctx.is_in_loop(step.def_block(ctx), lp) {
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
                    start,
                    step,
                    op: ind_op,
                    modulus: None,
                };

                self.indvars.push(indvar);
            }
        }
    }
}

impl LocalPass for ScevAnalysis {
    type Output = Vec<Scev>;

    fn run(&mut self, ctx: &Context, func: Func) -> PassResult<Self::Output> {
        let cfg = CfgInfo::new(ctx, func);

        self.dominance = Dominance::new(ctx, &cfg);
        self.loop_ctx = LoopContext::new(&cfg, &self.dominance);

        for lp in self.loop_ctx.loops() {
            self.process_loop(ctx, lp);
        }

        Ok(self.indvars.drain(..).collect())
    }
}

impl GlobalPass for ScevAnalysis {
    type Output = FxHashMap<Func, Vec<Scev>>;

    fn run(&mut self, ctx: &Context) -> PassResult<Self::Output> {
        let mut result = FxHashMap::default();
        for func in ctx.funcs() {
            result.insert(func, LocalPass::run(self, ctx, func)?);
        }
        Ok(result)
    }
}
