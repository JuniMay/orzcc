use super::{scalar_evolution::LoopScevRecord, Lcssa, LoopSimplify, Scev, ScevAnalysis};
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
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
        remove_all_insts,
        Block,
        Context,
        Func,
        ICmpCond,
        Inst,
        InstKind,
        Value,
    },
    utils::{
        cfg::CfgNode,
        def_use::User,
        loop_info::{Loop, LoopWithDepth},
    },
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
        if let Some(Scev {
            repr,
            start,
            step,
            op,
            modulus,
        }) = scevs
            .scevs
            .get(&lp)
            .unwrap()
            .iter()
            .find(|s| s.repr == *lhs || s.repr == *rhs)
        {
            let (bound, cmp_cond, reversed) = if repr == lhs {
                (rhs, cmp_cond, false)
            } else if repr == rhs {
                (lhs, cmp_cond, true)
            } else {
                unreachable!()
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

            let trip_count_const = if let (Some(start), Some(step), Some(bound), None) =
                (start_const, step_const, bound_const, modulus_const)
            {
                match (op, cmp_cond, reversed) {
                    (InductionOp::Add, ICmpCond::Sle, false) => {
                        let trip_count = (bound - start) / step + 1;
                        Some(trip_count as usize)
                    }
                    (InductionOp::Add, ICmpCond::Slt, false) => {
                        let trip_count = (bound - 1 - start) / step + 1;
                        Some(trip_count as usize)
                    }
                    // TODO: support more
                    _ => None,
                }
            } else {
                None
            };

            println!("[ loop-unroll (const) ] start: {:?}", start_const);
            println!("[ loop-unroll (const) ] step: {:?}", step_const);
            println!("[ loop-unroll (const) ] bound: {:?}", bound_const);
            println!("[ loop-unroll (const) ] trip count: {:?}", trip_count_const);

            if let Some(_trip_count) = trip_count_const {
                changed |= self.unroll_const(
                    ctx,
                    lp,
                    *repr,
                    start_const.unwrap(),
                    step_const.unwrap(),
                    bound_const.unwrap(),
                    trip_count_const.unwrap(),
                );
            }
        }

        changed
    }

    #[allow(clippy::too_many_arguments)]
    fn unroll_const(
        &mut self,
        ctx: &mut Context,
        lp: Loop<Block>,
        repr: Value,
        start: i64,
        step: i64,
        bound: i64,
        trip_count: usize,
    ) -> bool {
        self.deep_clone_map.clear();

        let header = lp.header(&self.scev.loop_ctx);
        let mut curr_header = header;

        let preheader = lp.get_preheader(ctx, &self.scev.loop_ctx).unwrap();

        let blocks = lp.get_blocks(ctx, &self.scev.loop_ctx);

        let insn: usize = blocks.iter().map(|bb| bb.insn(ctx)).sum();

        if insn * trip_count > 4096 {
            return false;
        }

        // we preserve the original loop body first. after cloning and
        // unrolling, we will remove the original loop body.

        // firstly, we need to decide an insertion point, just after the
        // preheader.
        let mut insertion_point = preheader;

        let mut loop_param = start;

        // the instructions that need to be modified with the new header.
        let mut jumps_to_modify = Vec::new();

        jumps_to_modify.push(preheader.tail(ctx).unwrap());

        while loop_param <= bound {
            println!("[ loop-unroll (const) ] loop param: {}", loop_param);

            let iconst = Inst::iconst(ctx, loop_param, repr.ty(ctx));

            self.deep_clone_map
                .insert_value(repr, iconst.result(ctx, 0));

            // map blocks and block params
            for block in blocks.iter() {
                let new_block = Block::new(ctx);
                self.deep_clone_map.insert_block(*block, new_block);

                #[allow(clippy::unnecessary_to_owned)]
                for param in block.params(ctx).to_vec() {
                    let new_param = new_block.new_param(ctx, param.ty(ctx));
                    if param != repr {
                        // we need the param to replace block, so just map if it's not the loop
                        // param
                        self.deep_clone_map.insert_value(param, new_param);
                    }
                }

                insertion_point.insert_after(ctx, new_block);
                insertion_point = new_block;

                if block == &header {
                    new_block.push_back(ctx, iconst);

                    // modify the jumps
                    for jump in jumps_to_modify.drain(..) {
                        jump.replace(ctx, curr_header, new_block);
                    }
                    // update the current header, so we can replace in the next iteration
                    curr_header = new_block;
                }
            }

            // clone instructions
            for block in blocks.iter() {
                let new_block = self.deep_clone_map.get_block(*block).unwrap();

                let mut cursor = block.cursor();
                while let Some(inst) = cursor.next(ctx) {
                    let new_inst = inst.deep_clone(ctx, &mut self.deep_clone_map);
                    new_block.push_back(ctx, new_inst);
                }

                for succ in block.succs(ctx) {
                    if succ == header {
                        jumps_to_modify.push(new_block.tail(ctx).unwrap());
                        break;
                    }
                }
            }

            loop_param += step;
        }

        // one last header
        let new_header = Block::new(ctx);
        insertion_point.insert_after(ctx, new_header);

        let iconst = Inst::iconst(ctx, loop_param, repr.ty(ctx));
        new_header.push_back(ctx, iconst);
        self.deep_clone_map
            .insert_value(repr, iconst.result(ctx, 0));

        #[allow(clippy::unnecessary_to_owned)]
        for param in header.params(ctx).to_vec() {
            let new_param = new_header.new_param(ctx, param.ty(ctx));
            if param != repr {
                self.deep_clone_map.insert_value(param, new_param);
            }
        }
        for jump in jumps_to_modify {
            jump.replace(ctx, curr_header, new_header);
        }

        // clone instructions
        let mut cursor = header.cursor();
        while let Some(inst) = cursor.next(ctx) {
            let new_inst = inst.deep_clone(ctx, &mut self.deep_clone_map);
            new_header.push_back(ctx, new_inst);
        }

        // remove all instructions in the original loop body
        let mut insts_to_remove = Vec::new();
        for block in blocks.iter() {
            for inst in block.iter(ctx) {
                insts_to_remove.push(inst);
            }
        }

        // FIXME: h_functional 18 prim
        remove_all_insts(ctx, insts_to_remove, false);

        for block in blocks {
            block.remove(ctx);
        }

        true
    }
}

impl LocalPassMut for LoopUnroll {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let scevs = LocalPass::run(&mut self.scev, ctx, func)?;

        let mut loops = Vec::new();

        for lp in self.scev.loop_ctx.loops() {
            let depth = lp.depth(&self.scev.loop_ctx);
            loops.push(LoopWithDepth { lp, depth });
        }

        loops.sort();
        loops.reverse();

        for LoopWithDepth { lp, .. } in loops {
            if self.process_loop(ctx, lp, &scevs) {
                return Ok(((), true));
            }
        }

        Ok(((), false))
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
            vec![
                Box::new(CfgCanonicalize),
                Box::new(LoopSimplify::default()),
                Box::new(Lcssa::default()),
            ],
        );

        passman.add_parameter("unroll-factor", 8);
        passman.add_parameter("unroll-constant-all", true);
    }
}
