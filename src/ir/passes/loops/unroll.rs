use super::{scalar_evolution::LoopScevRecord, Lcssa, LoopSimplify, Scev, ScevAnalysis};
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        debug::CommentPos,
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
        IBinaryOp,
        ICmpCond,
        Inst,
        InstKind,
        Ty,
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
            } else {
                // dynamic unrolling
                #[allow(clippy::single_match)]
                match (op, cmp_cond, reversed) {
                    (InductionOp::Add, ICmpCond::Sle | ICmpCond::Slt, false) => {
                        changed |=
                            self.unroll_dynamic(ctx, lp, *repr, *start, *step, *bound, *cmp_cond);
                    }
                    _ => {}
                }
            }
        }

        changed
    }

    #[allow(clippy::too_many_arguments)]
    fn unroll_dynamic(
        &mut self,
        ctx: &mut Context,
        lp: Loop<Block>,
        repr: Value,
        start: Value,
        step: Value,
        bound: Value,
        cmp_cond: ICmpCond,
    ) -> bool {
        let bound_def_block = bound.def_block(ctx);
        if self.scev.loop_ctx.is_in_loop(bound_def_block, lp) {
            // not loop invariant, cannot unroll
            return false;
        }

        let step_def_block = step.def_block(ctx);
        if self.scev.loop_ctx.is_in_loop(step_def_block, lp) {
            // not loop invariant, cannot unroll
            return false;
        }

        let header = lp.header(&self.scev.loop_ctx);

        let mut is_pre_exit = false;
        let exits = lp.get_exit_blocks(ctx, &self.scev.loop_ctx);

        for succ in header.succs(ctx) {
            if !self.scev.loop_ctx.is_in_loop(succ, lp) {
                is_pre_exit = true;
                break;
            }
        }

        if exits.len() != 1 || !is_pre_exit {
            // multiple exits or the header does not control the exit, do not unroll
            return false;
        }

        self.deep_clone_map.clear();

        let unroll_factor = self.unroll_factor as i32;

        // while (i + k * (factor - 1) < n) {
        //      i += k;
        //      i += k; .. unroll factor
        // }
        // ... same as before
        // this may cause overflow.

        // alternative:
        // sle: bound -= 1
        // slt:
        //  count = (bound - start) / step + 1
        //  rem = count % factor
        //  unrolled = count - rem
        //  unrolled_bound = start + step * unrolled
        //  while (i < unrolled_bound) {
        //      // unroll body * factor
        //  }
        //  ... old loop body with new i

        let preheader = lp.get_preheader(ctx, &self.scev.loop_ctx).unwrap();
        let blocks = lp.get_blocks(ctx, &self.scev.loop_ctx);

        let insn: usize = blocks.iter().map(|bb| bb.insn(ctx)).sum();
        if insn * self.unroll_factor > 512 {
            return false;
        }

        let ty = repr.ty(ctx);

        // calculate the trip count and unroll count
        let bound = match cmp_cond {
            ICmpCond::Slt => {
                let iconst = Inst::iconst(ctx, 1, ty);
                let sub = Inst::ibinary(ctx, IBinaryOp::Sub, bound, iconst.result(ctx, 0));
                preheader.push_inst_before_terminator(ctx, iconst);
                preheader.push_inst_before_terminator(ctx, sub);
                sub.result(ctx, 0)
            }
            ICmpCond::Sle => bound,
            ICmpCond::Eq | ICmpCond::Ne | ICmpCond::Ult | ICmpCond::Ule => unimplemented!(),
        };
        // calculate the trip count
        let sub = Inst::ibinary(ctx, IBinaryOp::Sub, bound, start);
        let one = Inst::iconst(ctx, 1, ty);
        let div = Inst::ibinary(ctx, IBinaryOp::SDiv, sub.result(ctx, 0), step);
        let add = Inst::ibinary(ctx, IBinaryOp::Add, div.result(ctx, 0), one.result(ctx, 0));
        let factor = Inst::iconst(ctx, unroll_factor, ty);
        let rem = Inst::ibinary(
            ctx,
            IBinaryOp::SRem,
            add.result(ctx, 0),
            factor.result(ctx, 0),
        );
        let unrolled = Inst::ibinary(ctx, IBinaryOp::Sub, add.result(ctx, 0), rem.result(ctx, 0));

        let mul = Inst::ibinary(ctx, IBinaryOp::Mul, step, unrolled.result(ctx, 0));
        let unrolled_bound = Inst::ibinary(ctx, IBinaryOp::Add, start, mul.result(ctx, 0));

        preheader.push_inst_before_terminator(ctx, sub);
        preheader.push_inst_before_terminator(ctx, one);
        preheader.push_inst_before_terminator(ctx, div);
        preheader.push_inst_before_terminator(ctx, add);
        preheader.push_inst_before_terminator(ctx, factor);
        preheader.push_inst_before_terminator(ctx, rem);
        preheader.push_inst_before_terminator(ctx, unrolled);
        preheader.push_inst_before_terminator(ctx, mul);
        preheader.push_inst_before_terminator(ctx, unrolled_bound);

        let unrolled_bound = unrolled_bound.result(ctx, 0);

        let mut insertion_point = preheader;

        // copy header
        let new_header = Block::new(ctx);
        self.deep_clone_map.insert_block(header, new_header);

        // the condition of jump, we need to modify it.
        let cond = header.tail(ctx).unwrap().operand(ctx, 0);

        // also, we need to map the exit to be the entry.
        let exit = header.tail(ctx).unwrap().succ(ctx, 1).block();

        insertion_point.insert_after(ctx, new_header);
        insertion_point = new_header;

        #[allow(clippy::unnecessary_to_owned)]
        for param in header.params(ctx).to_vec() {
            let ty = param.ty(ctx);
            let new_param = new_header.new_param(ctx, ty);
            self.deep_clone_map.insert_value(param, new_param);
        }

        // the new indvar in the parameter list.
        let new_repr = self.deep_clone_map.get_value(repr).unwrap();

        let i1 = Ty::int(ctx, 1);

        let mut jumps_to_modify: Vec<Inst> = Vec::new();

        // replace the first jump instruction.
        preheader
            .tail(ctx)
            .unwrap()
            .replace(ctx, header, new_header);

        let mut curr_header = new_header;

        for i in 0..unroll_factor {
            for block in blocks.iter() {
                if block == &header && i == 0 {
                    // the first header is already cloned
                    continue;
                }

                let new_block = Block::new(ctx);
                self.deep_clone_map.insert_block(*block, new_block);

                #[allow(clippy::unnecessary_to_owned)]
                for param in block.params(ctx).to_vec() {
                    let ty = param.ty(ctx);
                    let new_param = new_block.new_param(ctx, ty);
                    self.deep_clone_map.insert_value(param, new_param);
                }

                insertion_point.insert_after(ctx, new_block);
                insertion_point = new_block;

                if block == &header {
                    for jump in jumps_to_modify.drain(..) {
                        jump.replace(ctx, curr_header, new_block);
                    }
                    curr_header = new_block;
                }
            }

            for block in blocks.iter() {
                let new_block = self.deep_clone_map.get_block(*block).unwrap();
                let mut cursor = block.cursor();
                while let Some(inst) = cursor.next(ctx) {
                    if !inst.results(ctx).is_empty() && inst.result(ctx, 0) == cond {
                        if i != 0 {
                            // no need to clone the compare instruction, we just create true.
                            // use true to replace the old condition, because we know it must jump
                            // now.
                            // TODO: is this right?
                            let true_ = Inst::iconst(ctx, true, i1);
                            self.deep_clone_map.insert_value(cond, true_.result(ctx, 0));
                            new_block.push_back(ctx, true_);
                        } else {
                            // use the unroll bound as the new condition, also as slt
                            let new_cond = Inst::ibinary(
                                ctx,
                                IBinaryOp::Cmp(ICmpCond::Slt),
                                new_repr,
                                unrolled_bound,
                            );
                            self.deep_clone_map
                                .insert_value(cond, new_cond.result(ctx, 0));
                            new_block.push_back(ctx, new_cond);
                        }
                    } else {
                        let new_inst = inst.deep_clone(ctx, &mut self.deep_clone_map);
                        new_block.push_back(ctx, new_inst);
                    }
                }

                for succ in block.succs(ctx) {
                    if succ == header {
                        jumps_to_modify.push(new_block.tail(ctx).unwrap());
                        break;
                    }
                }

                if *block == header {
                    // we need to replace the exit to be the old header
                    let tail = new_block.tail(ctx).unwrap();
                    let params = new_block.params(ctx).to_vec();
                    tail.replace_succ_with_args(ctx, exit, header, params);
                }
            }
        }

        for jump in jumps_to_modify {
            // jump to the old header
            jump.replace(ctx, curr_header, header);
        }

        true
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
        if trip_count == 0 {
            return false;
        }

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
                    // the header dominates all the loop bodies, so insert the loop param here
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

        // one last header, should jump outside the loop
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

        // old loop body should be unreachable
        for block in blocks {
            // FIXME: 96_matrix_add.sy
            block.comment(ctx, CommentPos::Before, "should be unreachable");
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

        passman.add_parameter("unroll-factor", 4);
        passman.add_parameter("unroll-constant-all", true);

        passman.add_post_dep(LOOP_UNROLL, Box::new(CfgCanonicalize));
    }
}
