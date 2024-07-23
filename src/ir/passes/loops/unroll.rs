use super::{scalar_evolution::LoopScevRecord, LoopSimplify, Scev, ScevAnalysis};
use crate::{
    ir::{
        deep_clone::DeepCloneMap,
        passes::{control_flow::CfgCanonicalize, loops::InductionOp},
        passman::{
            GlobalPassMut,
            LocalPass,
            LocalPassMut,
            ParamStorage,
            PassResult,
            TransformPass,
        },
        Block,
        Context,
        Func,
        ICmpCond,
        InstKind,
    },
    utils::loop_info::Loop,
};

pub const LOOP_UNROLL: &str = "loop-unroll";

#[derive(Default)]
pub struct LoopUnroll {
    /// Scalar evolution analysis.
    scev: ScevAnalysis,
    /// Deep clone map for cloning instructions.
    deep_clone_map: DeepCloneMap,

    /// Unroll factor.
    unroll_factor: usize,
    /// If unroll the entire loop when the trip count is a determined constant.
    unroll_constant_all: bool,
}

impl LoopUnroll {
    fn process_loop(&mut self, ctx: &mut Context, lp: Loop<Block>, scevs: &LoopScevRecord) -> bool {
        let mut changed = false;

        let header = lp.header(&self.scev.loop_ctx);
        let preheader = lp.get_preheader(ctx, &self.scev.loop_ctx).unwrap();

        let (lhs, cmp_cond, rhs) = if let Some(bound) = scevs.loop_bounds.get(&lp).unwrap() {
            bound
        } else {
            return false;
        };

        match cmp_cond {
            ICmpCond::Eq | ICmpCond::Ne => return false,
            // TODO: Support unsigned bounds
            ICmpCond::Ule | ICmpCond::Ult => return false,
            ICmpCond::Sle | ICmpCond::Slt => {}
        }

        // trip count calculation (for sle and addition)
        // let trip count as k, init = a, upper = b
        // a + (k - 1) * step <= b, minus 1 for a itself
        // k - 1 <= (b - 1) / step, so take floor and add 1
        // k = floor((b - a) / step) + 1
        //
        // for slt, just minus the upper bound by 1

        // iterate the indvars, check if we can unroll the loop
        for Scev {
            repr,
            start,
            step,
            op,
            modulus,
        } in scevs.scevs.get(&lp).unwrap().iter()
        {
            let (bound, cmp_cond, reversed) = if repr == lhs {
                (rhs, cmp_cond, false)
            } else if repr == rhs {
                (lhs, cmp_cond, true)
            } else {
                return false;
            };

            println!("[ loop-unroll ] start: {:?}", start);

            let mut start_const = None;
            if let Some(inst) = start.def_inst(ctx) {
                if let InstKind::IConst(start) = inst.kind(ctx) {
                    start_const = Some(start.as_signed());
                }
            }

            let mut step_const = None;
            if let Some(inst) = step.def_inst(ctx) {
                if let InstKind::IConst(step) = inst.kind(ctx) {
                    step_const = Some(step.as_signed());
                }
            }

            let mut bound_const = None;
            if let Some(inst) = bound.def_inst(ctx) {
                if let InstKind::IConst(bound) = inst.kind(ctx) {
                    bound_const = Some(bound.as_signed());
                }
            }

            // two layers of option, outer means if there is a modulus, inner means if the
            // modulus is a constant
            let modulus_const = modulus.map(|v| {
                let mut modulus_const = None;
                if let Some(inst) = v.def_inst(ctx) {
                    if let InstKind::IConst(modulus) = inst.kind(ctx) {
                        modulus_const = Some(modulus.as_signed());
                    }
                }
                modulus_const
            });

            let cmp_fn = |a: i64, b: i64| match (cmp_cond, reversed) {
                (ICmpCond::Sle, false) => a <= b,
                (ICmpCond::Slt, false) => a < b,
                (ICmpCond::Sle, true) => a >= b,
                (ICmpCond::Slt, true) => a > b,
                _ => unreachable!(),
            };

            let induce_fn = |a: i64, b: i64, m: Option<i64>| {
                let val = match op {
                    InductionOp::Add => a + b,
                    InductionOp::Sub => a - b,
                    InductionOp::Mul => a * b,
                    InductionOp::SDiv => a / b,
                    InductionOp::Shl => a << b,
                };

                if let Some(m) = m {
                    val % m
                } else {
                    val
                }
            };

            let trip_count_const = if let (Some(start), Some(step), Some(bound), None) =
                (start_const, step_const, bound_const, modulus_const)
            {
                let mut trip_count = 0usize;
                let mut curr_indvar = start;

                while cmp_fn(curr_indvar, bound) {
                    trip_count += 1;
                    curr_indvar = induce_fn(curr_indvar, step, None);
                }

                Some(trip_count)
            } else if let (Some(start), Some(step), Some(bound), Some(Some(m))) =
                (start_const, step_const, bound_const, modulus_const)
            {
                // if the modulus exists, it must be a constant so we can compute the trip count
                let mut trip_count = 0usize;
                let mut curr_indvar = start;

                while cmp_fn(curr_indvar, bound) {
                    trip_count += 1;
                    curr_indvar = induce_fn(curr_indvar, step, Some(m));
                }

                Some(trip_count)
            } else {
                None
            };

            println!("[ loop-unroll (const) ] start: {:?}", start_const);
            println!("[ loop-unroll (const) ] step: {:?}", step_const);
            println!("[ loop-unroll (const) ] bound: {:?}", bound_const);
            println!("[ loop-unroll (const) ] trip count: {:?}", trip_count_const);
        }

        return changed;
    }
}

impl LocalPassMut for LoopUnroll {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let scevs = LocalPass::run(&mut self.scev, ctx, func)?;

        let mut changed = false;

        for lp in self.scev.loop_ctx.loops() {
            changed |= self.process_loop(ctx, lp, &scevs);
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for LoopUnroll {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;
        for func in ctx.funcs() {
            let (_, local_changed) = LocalPassMut::run(self, ctx, func).unwrap();
            changed |= local_changed;
        }
        Ok(((), changed))
    }

    fn fetch_params(&mut self, params: &ParamStorage) {
        self.unroll_factor = params.get("unroll-factor").unwrap_or(self.unroll_factor);
        self.unroll_constant_all = params
            .get("unroll-constant-all")
            .unwrap_or(self.unroll_constant_all);

        println!(
            "[ loop-unroll config ] factor={}, constant_all={}",
            self.unroll_factor, self.unroll_constant_all
        );
    }
}

impl TransformPass for LoopUnroll {
    fn register(passman: &mut crate::ir::passman::PassManager)
    where
        Self: Sized,
    {
        let pass = Self::default();

        passman.register_transform(
            LOOP_UNROLL,
            pass,
            vec![Box::new(CfgCanonicalize), Box::new(LoopSimplify::default())],
        );

        passman.add_parameter("unroll-factor", 8);
        passman.add_parameter("unroll-constant-all", true);
    }
}
