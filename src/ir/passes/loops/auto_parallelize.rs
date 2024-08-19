use rustc_hash::{FxHashMap, FxHashSet};

use super::{
    scalar_evolution::LoopScevRecord,
    InductionOp,
    Lcssa,
    LoopSimplify,
    Scev,
    ScevAnalysis,
};
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        deep_clone::DeepCloneMap,
        function_analysis::FunctionAnalysis,
        passes::{
            control_flow::CfgCanonicalize,
            loops::scalar_evolution::{LoopBound, LoopBoundCond},
        },
        passman::{GlobalPassMut, LocalPass, LocalPassMut, PassManager, PassResult, TransformPass},
        Block,
        Context,
        FBinaryOp,
        Func,
        IBinaryOp,
        ICmpCond,
        Inst,
        InstKind,
        Signature,
        Ty,
        Value,
    },
    utils::{
        cfg::CfgNode,
        def_use::{Usable, User},
        loop_info::{Loop, LoopWithDepth},
    },
};

pub const AUTO_PARALLELIZE: &str = "auto-parallelize";

#[derive(Default)]
pub struct AutoParallelize {
    scev: ScevAnalysis,
    function_analysis: FunctionAnalysis,
    func_counter: usize,
    generated_payloads: FxHashSet<Func>,
    // mark all the parallelized loops's repr to avoid infinite loop
    parallelized_loop: FxHashSet<Block>,
}

#[derive(Debug)]
pub struct MemoryAccess {
    pub ptr: Value,
    pub offsets: Vec<Value>,
}

impl AutoParallelize {
    fn process_loop(&mut self, ctx: &mut Context, lp: Loop<Block>, scevs: &LoopScevRecord) -> bool {
        let mut changed = false;

        let LoopBound {
            block_param,
            cond: cmp_cond,
            bound,
            reversed,
            ..
        } = if let Some(bound) = scevs.loop_bounds.get(&lp).unwrap() {
            bound
        } else {
            println!(
                "[ auto_parallelize ] loop bound not found for loop: {:?}",
                lp.header(&self.scev.loops)
            );
            return false;
        };

        println!(
            "[ auto_parallelize ] lhs: {:?}, cmp_cond: {:?}, rhs: {:?}",
            block_param, cmp_cond, bound
        );

        if *reversed {
            return false;
        }

        match cmp_cond {
            LoopBoundCond::Sge | LoopBoundCond::Sgt => return false, // TODO: support Sge, Sgt
            LoopBoundCond::Sle => return false,                      // TODO: support Sle
            LoopBoundCond::Slt => {}
        }

        if let Some(Scev {
            block_param: repr,
            init: start,
            step,
            op,
            modulus,
        }) = scevs.scevs.get(block_param)
        {
            println!(
                "[ auto_parallelize ] repr: {:?}, start: {:?}, step: {:?}, op: {:?}, modulus: {:?}, lhs: {:?}, cmp_cond: {:?}, rhs: {:?}",
                repr, start, step, op, modulus, block_param, cmp_cond, bound
            );

            if self.parallelized_loop.contains(&repr.def_block(ctx)) {
                return false;
            }

            match op {
                InductionOp::Add => {}
                // TODO: support more induction operations
                InductionOp::Sub | InductionOp::Mul | InductionOp::SDiv | InductionOp::Shl => {
                    println!(
                        "[ auto_parallelize ] unsupported induction operation: {:?}",
                        op
                    );
                    return false;
                }
            }

            // only support add 1
            if let Some(inst) = step.def_inst(ctx) {
                if let InstKind::IConst(step) = inst.kind(ctx) {
                    if !step.is_one() {
                        println!("[ auto_parallelize ] step is not 1: {:?}, {:?}", step, inst);
                        return false;
                    }
                } else {
                    println!(
                        "[ auto_parallelize ] step is not a constant: {:?}, {:?}",
                        step, inst
                    );
                    return false;
                }
            } else {
                println!("[ auto_parallelize ] step is not defined: {:?}", step);
                return false;
            }

            // only support for (lhs = start; lhs < rhs; lhs++)
            if block_param != repr {
                println!(
                    "[ auto_parallelize ] lhs is not repr: {:?}, {:?}",
                    block_param, repr
                );
                return false;
            }

            let loop_blocks = lp.get_blocks(ctx, &self.scev.loops);

            if self
                .generated_payloads
                .contains(&loop_blocks.first().unwrap().container(ctx).unwrap())
            {
                return false;
            }

            // // rule 0: do not parallelize if the loop is in self-recursion function
            // let header_func = loop_blocks.first().unwrap().container(ctx).unwrap();

            // for block in header_func.iter(ctx) {
            //     for inst in block.iter(ctx) {
            //         if let InstKind::Call(symbol) = inst.kind(ctx) {
            //             if let Some(func) = ctx.lookup_func(symbol) {
            //                 if func == header_func {
            //                     return false;
            //                 }
            //             }
            //         }
            //     }
            // }

            // rule 1: no call of impure function in the loop
            for block in loop_blocks.iter() {
                for inst in block.iter(ctx) {
                    if let InstKind::Call(symbol) = inst.kind(ctx) {
                        if let Some(func) = ctx.lookup_func(symbol) {
                            if !self.function_analysis.analyze_func(ctx, func).is_pure() {
                                println!(
                                    "[ auto_parallelize ] call to impure function: {:?}",
                                    func
                                );
                                return false;
                            }
                        } else {
                            println!("[ auto_parallelize ] function not found: {:?}", symbol);
                            return false;
                        }
                    }
                }
            }

            let mut stores = Vec::new();
            let mut loads = Vec::new();

            // get store ptrs and load ptrs
            for block in loop_blocks.iter() {
                for inst in block.iter(ctx) {
                    match inst.kind(ctx) {
                        InstKind::Store => {
                            // stores.push(inst.operand(ctx, 1));
                            // panic!("parallelize should run before legalize")
                        }
                        InstKind::Load => {
                            // loads.push(inst.operand(ctx, 0));
                            // panic!("parallelize should run before legalize")
                        }
                        InstKind::Undef
                        | InstKind::IConst(_)
                        | InstKind::FConst(_)
                        | InstKind::StackSlot(_)
                        | InstKind::IBinary(_)
                        | InstKind::FBinary(_)
                        | InstKind::IUnary(_)
                        | InstKind::FUnary(_)
                        | InstKind::Cast(_)
                        | InstKind::Offset
                        | InstKind::Jump
                        | InstKind::Br
                        | InstKind::Call(_)
                        | InstKind::CallIndirect(_)
                        | InstKind::Ret
                        | InstKind::GetGlobal(_) => {}
                        InstKind::LoadElem { .. } => {
                            let ptr = inst.operand(ctx, 0);
                            let offsets = inst
                                .operands(ctx)
                                .iter()
                                .skip(1)
                                .copied()
                                .collect::<Vec<_>>();
                            loads.push(MemoryAccess { ptr, offsets });
                        }
                        InstKind::StoreElem { .. } => {
                            let ptr = inst.operand(ctx, 1);
                            let offsets = inst
                                .operands(ctx)
                                .iter()
                                .skip(2)
                                .copied()
                                .collect::<Vec<_>>();
                            stores.push(MemoryAccess { ptr, offsets });
                        }
                    }
                }
            }

            // rule 2: store should either NoAlias or MustAlias with each other
            for i in 0..stores.len() {
                for j in i + 1..stores.len() {
                    if stores[i].ptr == stores[j].ptr && stores[i].offsets != stores[j].offsets {
                        return false;
                    }
                }
            }

            // rule 3: store should NoAlias with load
            for store in &stores {
                for load in &loads {
                    if store.ptr == load.ptr && store.offsets != load.offsets {
                        return false;
                    }
                }
            }

            // all offsets are indvars
            for store in &stores {
                for &offset in store.offsets.iter() {
                    if !scevs.scevs.contains_key(&offset) {
                        println!(
                            "[ auto_parallelize ] offset not found in scevs: {:?}",
                            offset
                        );
                        return false;
                    }
                }
            }
            for load in &loads {
                for &offset in load.offsets.iter() {
                    if !scevs.scevs.contains_key(&offset) {
                        println!(
                            "[ auto_parallelize ] offset not found in scevs: {:?}",
                            offset
                        );
                        return false;
                    }
                }
            }

            // store need constrained by repr
            for store in &stores {
                if !store.offsets.contains(repr) {
                    println!(
                        "[ auto_parallelize ] store not constrained by repr: {:?}",
                        store
                    );
                    return false;
                }
            }

            // rule 4: support at most 1 reduction
            let header = lp.header(&self.scev.loops);
            if header.params(ctx).len() > 2 {
                return false;
            }

            let mut reduction_index = 0;
            let reduction_val = if header.params(ctx).len() == 2 {
                let a = header.params(ctx)[0];
                let b = header.params(ctx)[1];
                if a == *repr {
                    reduction_index = 1;
                    Some(b)
                } else if b == *repr {
                    reduction_index = 0;
                    Some(a)
                } else {
                    println!("[ auto_parallelize ] no repr value found in header.params(ctx)");
                    return false;
                }
            } else {
                None
            };

            if reduction_val.is_some() && reduction_val.unwrap().ty(ctx).is_float(ctx) {
                println!("[ auto_parallelize ] float unsupported for reduction");
                return false;
            }

            if reduction_val.is_some() {
                let mut found = false;
                // rule 4.1: the reduction value should be used in an add inst
                for user in reduction_val.unwrap().users(ctx) {
                    if let InstKind::IBinary(_) | InstKind::FBinary(_) = user.kind(ctx) {
                        let reduction_result = user.result(ctx, 0);
                        if let Some(reduction_user_block) = user.container(ctx) {
                            if let Some(reduction_user_block_tail_jump) =
                                reduction_user_block.tail(ctx)
                            {
                                if reduction_user_block_tail_jump.is_jump(ctx)
                                    && reduction_user_block_tail_jump
                                        .succ(ctx, 0)
                                        .get_arg(reduction_val.unwrap())
                                        .is_some()
                                    && reduction_user_block_tail_jump
                                        .succ(ctx, 0)
                                        .get_arg(reduction_val.unwrap())
                                        .unwrap()
                                        == reduction_result
                                {
                                    if matches!(
                                        user.kind(ctx),
                                        InstKind::IBinary(IBinaryOp::Add)
                                            | InstKind::FBinary(FBinaryOp::Add)
                                    ) {
                                        found = true;
                                    } else {
                                        println!(
                                            "[ auto_parallelize ] reduction value used in inst other than add: {:?}",
                                            reduction_val
                                        );
                                        return false;
                                    }
                                }
                            }
                        }
                    }
                }

                if !found {
                    println!(
                        "[ auto_parallelize ] can't find reduction value used in add inst: {:?}",
                        reduction_val
                    );
                    return false;
                }
            }

            // rule 5: only one exit block
            if lp.get_exit_blocks(ctx, &self.scev.loops).len() != 1 {
                return false;
            }

            let exit_block = lp
                .get_exit_blocks(ctx, &self.scev.loops)
                .into_iter()
                .next()
                .unwrap();

            // no break in loop
            for block in loop_blocks
                .iter()
                .filter(|block| **block != lp.header(&self.scev.loops))
            {
                if block.succs(ctx).contains(&exit_block) {
                    return false;
                }
            }

            // search for used values that are defined outside the loop
            let mut defined_inside = FxHashSet::default();
            for block in loop_blocks.iter() {
                for param in block.params(ctx) {
                    defined_inside.insert(param);
                }
                for inst in block.iter(ctx) {
                    for operand in inst.results(ctx) {
                        defined_inside.insert(operand);
                    }
                }
            }

            let mut defined_outside = FxHashSet::default();
            for block in loop_blocks.iter() {
                for inst in block.iter(ctx) {
                    for operand in User::<Value>::all_uses(inst, ctx) {
                        if operand != *repr
                            && !(*block == header && operand == *bound)
                            && (reduction_val.is_none() || operand != reduction_val.unwrap())
                            && !defined_inside.contains(&operand)
                        {
                            defined_outside.insert(operand);
                        }
                    }
                }
            }

            // rule 6: the rhs is defined outside the loop
            if defined_inside.contains(bound) {
                println!("[ auto_parallelize ] rhs is defined inside the loop");
                return false;
            }

            println!(
                "[ auto_parallelize ] pending to be parallelized. stores: {:?}, reduction_val: {:?}",
                stores, reduction_val
            );

            // type checks
            if let Some(reduction_val) = reduction_val {
                if !reduction_val.ty(ctx).is_int32(ctx) && !reduction_val.ty(ctx).is_float32(ctx) {
                    println!("[ auto_parallelize ] unsupported type in reduction_val");
                    return false;
                }
            }

            // params passing
            let mut constants = FxHashMap::default();
            let mut params_to_pass = Vec::new();
            for operand in defined_outside {
                if let Some(inst) = operand.def_inst(ctx) {
                    if let InstKind::IConst(val) = inst.kind(ctx) {
                        constants.insert(operand, (*val, operand.ty(ctx)));
                    } else {
                        params_to_pass.push(operand);
                    }
                } else {
                    params_to_pass.push(operand);
                }
            }

            for param in params_to_pass.iter() {
                if !param.ty(ctx).is_int32(ctx)
                    && !param.ty(ctx).is_float32(ctx)
                    && !param.ty(ctx).is_ptr(ctx)
                {
                    println!("[ auto_parallelize ] unsupported type in params_to_pass");
                    return false;
                }
            }

            changed = true;
            for block in loop_blocks.iter() {
                self.parallelized_loop.insert(*block);
            }

            // make a function payload
            let func_name = format!("__orzcc_parallel_payload_{}", self.func_counter);
            self.func_counter += 1;
            let ptr_ty = Ty::ptr(ctx);
            let i32_ty = Ty::int(ctx, 32);
            let f32_ty = Ty::float32(ctx);
            let void_ty = Ty::void(ctx);
            let return_ty = if reduction_val.is_some() {
                vec![reduction_val.unwrap().ty(ctx)]
            } else {
                vec![Ty::void(ctx)]
            };
            let payload = Func::new(
                ctx,
                func_name.clone(),
                Signature::new(vec![ptr_ty], return_ty),
            );
            self.generated_payloads.insert(payload);

            let mut deep_clone_map = DeepCloneMap::default();
            let mut header_deep_clone_map = DeepCloneMap::default();

            // make a function header for params passing
            let payload_header = Block::new(ctx);
            let var_list_val = payload_header.new_param(ctx, ptr_ty);

            // reduction
            let payload_reduction_inst = if let Some(reduction_val) = reduction_val {
                if reduction_val.ty(ctx).is_int32(ctx) {
                    let payload_reduction = Inst::iconst(ctx, 0, i32_ty);
                    payload_header.push_back(ctx, payload_reduction);
                    // deep_clone_map.insert_value(reduction_val,
                    // payload_reduction.result(ctx, 0));
                    Some(payload_reduction)
                } else if reduction_val.ty(ctx).is_float32(ctx) {
                    let payload_reduction = Inst::fconst(ctx, 0.0f32, f32_ty);
                    payload_header.push_back(ctx, payload_reduction);
                    // deep_clone_map.insert_value(reduction_val,
                    // payload_reduction.result(ctx, 0));
                    Some(payload_reduction)
                } else {
                    panic!("unsupported type in reduction_val")
                }
            } else {
                None
            };

            // constants
            for (const_value, (constant, ty)) in constants {
                let payload_constant = Inst::iconst(ctx, constant, ty);
                payload_header.push_back(ctx, payload_constant);
                deep_clone_map.insert_value(const_value, payload_constant.result(ctx, 0));
                header_deep_clone_map.insert_value(const_value, payload_constant.result(ctx, 0));
            }

            // params
            for (i, param) in params_to_pass.iter().enumerate() {
                let payload_index = Inst::iconst(ctx, i as i32, i32_ty);
                if param.ty(ctx).is_int32(ctx) {
                    let param_val = Inst::call(
                        ctx,
                        "orzcc_var_list_get_int",
                        vec![var_list_val, payload_index.result(ctx, 0)],
                        vec![i32_ty],
                    );
                    payload_header.push_back(ctx, payload_index);
                    payload_header.push_back(ctx, param_val);
                    deep_clone_map.insert_value(*param, param_val.result(ctx, 0));
                    header_deep_clone_map.insert_value(*param, param_val.result(ctx, 0));
                } else if param.ty(ctx).is_float32(ctx) {
                    let param_val = Inst::call(
                        ctx,
                        "orzcc_var_list_get_float",
                        vec![var_list_val, payload_index.result(ctx, 0)],
                        vec![f32_ty],
                    );
                    payload_header.push_back(ctx, payload_index);
                    payload_header.push_back(ctx, param_val);
                    deep_clone_map.insert_value(*param, param_val.result(ctx, 0));
                    header_deep_clone_map.insert_value(*param, param_val.result(ctx, 0));
                } else if param.ty(ctx).is_ptr(ctx) {
                    let param_val = Inst::call(
                        ctx,
                        "orzcc_var_list_get_ptr",
                        vec![var_list_val, payload_index.result(ctx, 0)],
                        vec![ptr_ty],
                    );
                    payload_header.push_back(ctx, payload_index);
                    payload_header.push_back(ctx, param_val);
                    deep_clone_map.insert_value(*param, param_val.result(ctx, 0));
                    header_deep_clone_map.insert_value(*param, param_val.result(ctx, 0));
                } else {
                    panic!("unsupported type in params_to_pass")
                }
            }

            // start and end
            let payload_start = Inst::call(
                ctx,
                "orzcc_var_list_get_start",
                vec![var_list_val],
                vec![i32_ty],
            );
            payload_header.push_back(ctx, payload_start);
            // deep_clone_map.insert_value(*repr, payload_start.result(ctx, 0));
            header_deep_clone_map.insert_value(*repr, payload_start.result(ctx, 0));

            let payload_end = Inst::call(
                ctx,
                "orzcc_var_list_get_end",
                vec![var_list_val],
                vec![i32_ty],
            );
            payload_header.push_back(ctx, payload_end);
            // deep_clone_map.insert_value(*rhs, payload_end.result(ctx, 0));
            header_deep_clone_map.insert_value(*bound, payload_end.result(ctx, 0));

            payload.push_back(ctx, payload_header);

            // copy the loop body
            for block in loop_blocks.iter() {
                let new_block = Block::new(ctx);
                header_deep_clone_map.insert_block(*block, new_block);
                deep_clone_map.insert_block(*block, new_block);

                #[allow(clippy::unnecessary_to_owned)]
                for param in block.params(ctx).to_vec() {
                    let new_param = new_block.new_param(ctx, param.ty(ctx));
                    header_deep_clone_map.insert_value(param, new_param);
                    deep_clone_map.insert_value(param, new_param);
                }

                payload.push_back(ctx, new_block);
            }

            // jump to the loop
            let payload_jump = if reduction_val.is_some() {
                let args = if reduction_index == 0 {
                    vec![
                        payload_reduction_inst.unwrap().result(ctx, 0),
                        payload_start.result(ctx, 0),
                    ]
                } else {
                    vec![
                        payload_start.result(ctx, 0),
                        payload_reduction_inst.unwrap().result(ctx, 0),
                    ]
                };
                Inst::jump(
                    ctx,
                    deep_clone_map
                        .get_block(header)
                        .unwrap_or_else(|| panic!("mapped header not found")),
                    args,
                )
            } else {
                Inst::jump(
                    ctx,
                    deep_clone_map
                        .get_block(header)
                        .unwrap_or_else(|| panic!("mapped header not found")),
                    vec![payload_start.result(ctx, 0)],
                )
            };
            payload_header.push_back(ctx, payload_jump);

            // payload exit
            let payload_exit = Block::new(ctx);
            header_deep_clone_map.insert_block(exit_block, payload_exit);

            // clone insts
            for block in loop_blocks.iter() {
                let new_block = deep_clone_map.get_block(*block).unwrap();

                if *block == header {
                    let mut cursor = block.cursor();
                    while let Some(inst) = cursor.next(ctx) {
                        let new_inst = inst.deep_clone_sync(
                            ctx,
                            &mut header_deep_clone_map,
                            &mut deep_clone_map,
                        );

                        new_block.push_back(ctx, new_inst);
                    }
                } else {
                    let mut cursor = block.cursor();
                    while let Some(inst) = cursor.next(ctx) {
                        let new_inst = inst.deep_clone(ctx, &mut deep_clone_map);
                        new_block.push_back(ctx, new_inst);
                    }
                }
            }

            // return
            if reduction_val.is_some() {
                if reduction_val.unwrap().ty(ctx).is_int32(ctx) {
                    let payload_return = Inst::call(
                        ctx,
                        "orzcc_var_list_ret_int",
                        vec![
                            var_list_val,
                            deep_clone_map
                                .get_value(reduction_val.unwrap())
                                .unwrap_or_else(|| panic!("mapped reduction_val not found")),
                        ],
                        vec![void_ty],
                    );
                    payload_exit.push_back(ctx, payload_return);
                } else if reduction_val.unwrap().ty(ctx).is_float32(ctx) {
                    let payload_return = Inst::call(
                        ctx,
                        "orzcc_var_list_ret_float",
                        vec![
                            var_list_val,
                            deep_clone_map
                                .get_value(reduction_val.unwrap())
                                .unwrap_or_else(|| panic!("mapped reduction_val not found")),
                        ],
                        vec![void_ty],
                    );
                    payload_exit.push_back(ctx, payload_return);
                } else {
                    panic!("unsupported type in reduction_val")
                }
            }

            let payload_final_return = Inst::ret(ctx, vec![]);

            payload_exit.push_back(ctx, payload_final_return);
            payload.push_back(ctx, payload_exit);

            // now generate the calling code

            // there may be use of values defined in the loop
            let unclosed_values = lp.get_unclosed_values(ctx, &self.scev.loops);

            // // remove all the old insts in the loop
            // let insts_to_remove = loop_blocks
            //     .iter()
            //     .flat_map(|block| block.iter(ctx).collect::<Vec<_>>())
            //     .collect::<Vec<_>>();

            // remove_all_insts(ctx, insts_to_remove, false);

            // generate a new block
            let parallel_body = Block::new(ctx);

            // copy the old block args
            let mut parallel_body_args_map = FxHashMap::default();
            #[allow(clippy::unnecessary_to_owned)]
            for param in header.params(ctx).to_vec() {
                let new_param = parallel_body.new_param(ctx, param.ty(ctx));
                parallel_body_args_map.insert(param, new_param);
            }

            let preheader = lp
                .get_preheader(ctx, &self.scev.loops)
                .unwrap_or_else(|| panic!("preheader not found"));

            preheader.insert_after(ctx, parallel_body);

            let preheader_jump = preheader
                .tail(ctx)
                .unwrap_or_else(|| panic!("tail not found"));

            let preheader_jump_arg_repr = if preheader_jump.is_jump(ctx) {
                preheader_jump
                    .succ(ctx, 0)
                    .get_arg(*repr)
                    .unwrap_or_else(|| panic!("preheader_jump_arg_repr not found"))
            } else {
                panic!("tail inst not valid")
            };

            let preheader_old_args =
                if preheader_jump.is_jump(ctx) && preheader_jump.succ(ctx, 0).block() == header {
                    header
                        .params(ctx)
                        .iter()
                        .map(|param| preheader_jump.succ(ctx, 0).get_arg(*param).unwrap())
                        .collect::<Vec<_>>()
                } else {
                    panic!("tail inst not valid")
                };

            // let exit_block_args =
            // if tail_inst.is_br(ctx) && tail_inst.succ(ctx, 1).block() == exit_block {
            //     exit_block
            //         .params(ctx)
            //         .iter()
            //         .map(|param| tail_inst.succ(ctx, 1).get_arg(*param).unwrap())
            //         .collect::<Vec<_>>()
            // } else {
            //     panic!("tail inst not valid")
            // };

            let trip_count = Inst::ibinary(ctx, IBinaryOp::Sub, *bound, preheader_jump_arg_repr);
            preheader.push_inst_before_terminator(ctx, trip_count);

            let loop_parallelize_threshold = Inst::iconst(ctx, 400, i32_ty);
            preheader.push_inst_before_terminator(ctx, loop_parallelize_threshold);

            let trip_count_less_than_threshold = Inst::ibinary(
                ctx,
                IBinaryOp::Cmp(ICmpCond::Slt),
                trip_count.result(ctx, 0),
                loop_parallelize_threshold.result(ctx, 0),
            );
            preheader.push_inst_before_terminator(ctx, trip_count_less_than_threshold);

            let trip_count_less_than_threshold_jump = Inst::br(
                ctx,
                trip_count_less_than_threshold.result(ctx, 0),
                header,
                preheader_old_args.clone(),
                parallel_body,
                preheader_old_args.clone(),
            );
            preheader.push_inst_before_terminator(ctx, trip_count_less_than_threshold_jump);

            // replace the old jump
            preheader.tail(ctx).unwrap().remove(ctx);

            // init the var list
            let size_const = Inst::iconst(ctx, params_to_pass.len() as i32, i32_ty);
            let arg_list = Inst::call(
                ctx,
                "orzcc_init_var_list",
                vec![size_const.result(ctx, 0)],
                vec![ptr_ty],
            );
            let arg_list_val = arg_list.result(ctx, 0);
            parallel_body.push_back(ctx, size_const);
            parallel_body.push_back(ctx, arg_list);

            // pass arguments
            for param in params_to_pass.iter() {
                if param.ty(ctx).is_int32(ctx) {
                    let arg = Inst::call(
                        ctx,
                        "orzcc_var_list_push_int",
                        vec![arg_list_val, *param],
                        vec![void_ty],
                    );
                    parallel_body.push_back(ctx, arg);
                } else if param.ty(ctx).is_float32(ctx) {
                    let arg = Inst::call(
                        ctx,
                        "orzcc_var_list_push_float",
                        vec![arg_list_val, *param],
                        vec![void_ty],
                    );
                    parallel_body.push_back(ctx, arg);
                } else if param.ty(ctx).is_ptr(ctx) {
                    let arg = Inst::call(
                        ctx,
                        "orzcc_var_list_push_ptr",
                        vec![arg_list_val, *param],
                        vec![void_ty],
                    );
                    parallel_body.push_back(ctx, arg);
                } else {
                    panic!("unsupported type in params_to_pass")
                }
            }

            // call the payload
            let payload_ptr = Inst::get_global(ctx, func_name);
            parallel_body.push_back(ctx, payload_ptr);
            let call_the_payload = if let Some(reduction_val) = reduction_val {
                if reduction_val.ty(ctx).is_int32(ctx) {
                    Inst::call(
                        ctx,
                        "orzcc_parallel_for_reduce_add_int",
                        vec![
                            parallel_body_args_map[block_param],
                            *bound,
                            payload_ptr.result(ctx, 0),
                            arg_list_val,
                            parallel_body_args_map[&reduction_val],
                        ],
                        vec![reduction_val.ty(ctx)],
                    )
                } else if reduction_val.ty(ctx).is_float32(ctx) {
                    Inst::call(
                        ctx,
                        "orzcc_parallel_for_reduce_add_float",
                        vec![
                            parallel_body_args_map[block_param],
                            *bound,
                            payload_ptr.result(ctx, 0),
                            arg_list_val,
                            parallel_body_args_map[&reduction_val],
                        ],
                        vec![reduction_val.ty(ctx)],
                    )
                } else {
                    panic!("unsupported type in reduction_val")
                }
            } else {
                Inst::call(
                    ctx,
                    "orzcc_parallel_for",
                    vec![
                        parallel_body_args_map[block_param],
                        *bound,
                        payload_ptr.result(ctx, 0),
                        arg_list_val,
                    ],
                    vec![void_ty],
                )
            };
            parallel_body.push_back(ctx, call_the_payload);

            // get the original block args
            let tail_inst = header.tail(ctx).unwrap_or_else(|| panic!("tail not found"));
            let exit_block_args =
                if tail_inst.is_br(ctx) && tail_inst.succ(ctx, 1).block() == exit_block {
                    exit_block
                        .params(ctx)
                        .iter()
                        .map(|param| {
                            if tail_inst.succ(ctx, 1).get_arg(*param).unwrap() == *repr {
                                *bound
                            } else if reduction_val.is_some()
                                && tail_inst.succ(ctx, 1).get_arg(*param).unwrap()
                                    == reduction_val.unwrap()
                            {
                                call_the_payload.result(ctx, 0)
                            } else {
                                tail_inst.succ(ctx, 1).get_arg(*param).unwrap()
                            }
                        })
                        .collect::<Vec<_>>()
                } else {
                    panic!("tail inst not valid")
                };

            let jump_exit = Inst::jump(ctx, exit_block, exit_block_args);
            if let Some(reduction_val) = reduction_val {
                jump_exit.replace(ctx, reduction_val, call_the_payload.result(ctx, 0));
            }
            parallel_body.push_back(ctx, jump_exit);

            println!(
                "[ auto_parallelize ] unclosed_values: {:?}",
                unclosed_values
            );

            // they can be either the repr or reduction_val
            // we need to replace them with the corresponding values in the
            // payload for value in unclosed_values {
            //     // for repr, they will be replaced with the rhs
            //     if value == *repr {
            //         for user in value.users(ctx) {
            //             user.replace(ctx, value, *rhs);
            //         }
            //     } else if reduction_val.is_some() && value ==
            // reduction_val.unwrap() {         for user in
            // value.users(ctx) {             user.replace(ctx,
            // value, call_the_payload.result(ctx, 0));         }
            //     }
            // }

            // for block in loop_blocks.iter().filter(|&b| *b != header) {
            //     block.remove(ctx);
            // }
        } else {
            println!("[ auto_parallelize ] repr not found in scevs");
        }

        changed
    }
}

impl LocalPassMut for AutoParallelize {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        self.function_analysis.analyze_all(ctx);

        let scevs = LocalPass::run(&mut self.scev, ctx, func)?;

        let mut loops = Vec::new();

        for lp in self.scev.loops.loops() {
            let depth = lp.depth(&self.scev.loops);
            loops.push(LoopWithDepth { lp, depth });
        }

        loops.sort();

        for LoopWithDepth { lp, .. } in loops {
            if self.process_loop(ctx, lp, &scevs) {
                return Ok(((), true));
            }
        }

        Ok(((), false))
    }
}

impl GlobalPassMut for AutoParallelize {
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

impl TransformPass for AutoParallelize {
    fn register(passman: &mut PassManager)
    where
        Self: Sized,
    {
        let pass = Self::default();

        passman.register_transform(
            AUTO_PARALLELIZE,
            pass,
            vec![
                Box::new(CfgCanonicalize),
                Box::new(LoopSimplify::default()),
                Box::new(Lcssa::default()),
            ],
        );
    }
}
