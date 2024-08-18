use std::collections::HashMap;

use super::{scalar_evolution::LoopScevRecord, Lcssa, LoopSimplify, ScevAnalysis};
use crate::{
    ir::{
        passes::control_flow::CfgCanonicalize,
        passman::{GlobalPassMut, LocalPass, LocalPassMut, PassManager, PassResult, TransformPass},
        Block,
        Context,
        Func,
    },
    utils::{
        def_use::{Usable, User},
        loop_info::{Loop, LoopWithDepth},
    },
};

pub const INDVAR_REDUCE: &str = "indvar-reduce";

#[derive(Default)]
pub struct IndvarReduce {
    scev: ScevAnalysis,
}

impl IndvarReduce {
    fn process_loop(&self, ctx: &mut Context, lp: Loop<Block>, scevs: &LoopScevRecord) -> bool {
        // remove redundant induction variables. If two indvars share the same step, op,
        // and init, we can replace one with the other.

        let header = lp.header(&self.scev.loops);

        let params = header.params(ctx).to_vec();

        let mut indvar_to_replace = HashMap::new();

        for (i, param) in params.iter().enumerate() {
            if indvar_to_replace.contains_key(param) {
                continue;
            }

            let indvar = if let Some(indvar) = scevs.scevs.get(param) {
                indvar
            } else {
                continue;
            };

            for &other in params.iter().skip(i + 1) {
                let other = if let Some(other) = scevs.scevs.get(&other) {
                    other
                } else {
                    continue;
                };

                if indvar.init == other.init
                    && indvar.step == other.step
                    && indvar.op == other.op
                    && indvar.modulus == other.modulus
                {
                    indvar_to_replace.insert(other.block_param, *param);
                }
            }
        }

        println!(
            "[ indvar-reduce ] reduced {} redundant indvars",
            indvar_to_replace.len()
        );

        if indvar_to_replace.is_empty() {
            return false;
        }

        for (from, to) in indvar_to_replace {
            for user in from.users(ctx) {
                user.replace(ctx, from, to);
            }
        }

        true
    }
}

impl LocalPassMut for IndvarReduce {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        let scevs = LocalPass::run(&mut self.scev, ctx, func)?;

        let mut loops = Vec::new();

        for lp in self.scev.loops.loops() {
            let depth = lp.depth(&self.scev.loops);
            loops.push(LoopWithDepth { lp, depth });
        }

        loops.sort();

        for LoopWithDepth { lp, .. } in loops {
            changed |= self.process_loop(ctx, lp, &scevs);
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for IndvarReduce {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;
        for func in ctx.funcs() {
            let (_, local_changed) = LocalPassMut::run(self, ctx, func).unwrap();
            changed |= local_changed;
        }
        Ok(((), changed))
    }
}

impl TransformPass for IndvarReduce {
    fn register(passman: &mut PassManager)
    where
        Self: Sized,
    {
        let pass = Self::default();

        passman.register_transform(
            INDVAR_REDUCE,
            pass,
            vec![
                Box::new(CfgCanonicalize),
                Box::new(LoopSimplify::default()),
                Box::new(Lcssa::default()),
            ],
        );
    }
}
