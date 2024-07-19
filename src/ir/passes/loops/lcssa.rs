use rustc_hash::FxHashMap;

use super::LoopSimplify;
use crate::{
    collections::linked_list::LinkedListNodePtr,
    ir::{
        passes::control_flow::CfgCanonicalize,
        passman::{GlobalPassMut, LocalPassMut, PassManager, PassResult, TransformPass},
        Block,
        Context,
        Func,
        Value,
    },
    utils::{
        cfg::CfgInfo,
        def_use::{Usable, User},
        dominance::Dominance,
        loop_info::{Loop, LoopContext, LoopWithDepth},
    },
};

pub const LCSSA: &str = "lcssa";

#[derive(Default)]
pub struct Lcssa {
    loop_ctx: LoopContext<Block>,
    dominance: Dominance<Block>,
}

impl Lcssa {
    fn process_loop(&self, ctx: &mut Context, lp: Loop<Block>) -> bool {
        let unclosed = lp.get_unclosed_values(ctx, &self.loop_ctx);

        if unclosed.is_empty() {
            return false;
        }

        let exits = lp.get_exit_blocks(ctx, &self.loop_ctx);

        println!("[ lcssa ] found {} unclosed values", unclosed.len());

        // exit block -> block param mapping, for each unclosed value.
        let mut lcssa_params = FxHashMap::default();

        // create block parameters in each exit block for each value.
        for value in unclosed.iter().copied() {
            // for each unclosed value, create a new mapping.
            lcssa_params.clear();

            let def_block = value.def_block(ctx);

            for exit_block in exits.iter().copied() {
                if !self.dominance.dominates(def_block, exit_block) {
                    // to insert block param, we must make sure the definition block dominates the
                    // exit block. this requires loop-simplify to be run before lcssa. and if the
                    // def block does not dominate the exit block, we can just skip this exit block.
                    continue;
                }
                let ty = value.ty(ctx);
                // create a new receiver parameter for the incoming unclosed value.
                let param = exit_block.new_param(ctx, ty);

                let name = Self::alloc_name_for_lcssa_param(ctx, value, param);
                println!("[ lcssa ] created {} in an exit block", name);

                for pred_inst in exit_block.users(ctx) {
                    // add arguments for all predecessors that jumps to the exit block.
                    pred_inst.add_succ_arg(ctx, exit_block, param, value);
                }

                // record exit -> param
                lcssa_params.insert(exit_block, param);
            }

            for user in value.users(ctx) {
                let user_block = user.container(ctx).unwrap();
                if self.loop_ctx.is_in_loop(user_block, lp) {
                    continue;
                }
                // replace uses outside the loop with the block parameter.
                let param = self.get_value_in_block(ctx, user_block, value, &mut lcssa_params, lp);
                user.replace(ctx, value, param);
            }
        }

        // unclosed set is not empty, must have changed something.
        true
    }

    fn alloc_name_for_lcssa_param(ctx: &mut Context, unclosed: Value, param: Value) -> &String {
        if param.name(ctx).is_some() {
            return param.name(ctx).unwrap();
        }
        let name = unclosed.name_or_alloc(ctx, "v").clone();
        param.alloc_name(ctx, format!("{}.lcssa_", name))
    }

    fn get_value_in_block(
        &self,
        ctx: &mut Context,
        user_block: Block,
        unclosed_value: Value,
        lcssa_params: &mut FxHashMap<Block, Value>,
        lp: Loop<Block>,
    ) -> Value {
        if let Some(param) = lcssa_params.get(&user_block) {
            return *param;
        }

        // the user block is not an exit block (and not visited), two cases:
        // 1. the user block is dominated by exactly one exit block.
        // 2. the user block is not dominated by any exit block.

        let idom = self.dominance.idom(user_block).unwrap();

        if self.loop_ctx.is_in_loop(idom, lp) {
            // if the idom is in the loop, the user block is not dominated by any exit
            // block.

            // create a new block parameter for the value.
            let ty = unclosed_value.ty(ctx);
            let param = user_block.new_param(ctx, ty);

            let name = Self::alloc_name_for_lcssa_param(ctx, unclosed_value, param);
            println!("[ lcssa ] created {} in a non-exit block", name);

            for pred_inst in user_block.users(ctx) {
                let pred_block = pred_inst.container(ctx).unwrap();
                let val =
                    self.get_value_in_block(ctx, pred_block, unclosed_value, lcssa_params, lp);
                pred_inst.add_succ_arg(ctx, user_block, param, val);
            }

            // update the mapping, so that we can use the value in the future.
            lcssa_params.insert(user_block, param);
            param
        } else {
            // not sure, because this is just the idom, so recursively find the value.
            let param = self.get_value_in_block(ctx, idom, unclosed_value, lcssa_params, lp);
            // update the mapping, so that we can use the value in the future.
            lcssa_params.insert(user_block, param);
            param
        }
    }
}

impl LocalPassMut for Lcssa {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let cfg = CfgInfo::new(ctx, func);

        self.dominance = Dominance::new(ctx, &cfg);
        self.loop_ctx = LoopContext::new(&cfg, &self.dominance);

        let mut loops = Vec::new();

        for lp in self.loop_ctx.loops() {
            let depth = lp.depth(&self.loop_ctx);
            loops.push(LoopWithDepth { lp, depth });
        }

        loops.sort();
        loops.reverse();

        let mut changed = false;

        for LoopWithDepth { lp, .. } in loops {
            changed |= self.process_loop(ctx, lp);
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for Lcssa {
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

impl TransformPass for Lcssa {
    fn register(passman: &mut PassManager) {
        let pass = Self::default();
        passman.register_transform(
            LCSSA,
            pass,
            // TODO: auto manage dependencies
            vec![Box::new(CfgCanonicalize), Box::new(LoopSimplify::default())],
        );
    }
}
