use std::{hash::Hash, mem::swap};

use rustc_hash::FxHashSet;

use super::live_interval_analysis;
use crate::{
    backend::{
        inst::MInst,
        regs::RegDefUse,
        riscv64::regs::display,
        LowerConfig,
        LowerContext,
        LowerSpec,
        MFunc,
    },
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
};

#[derive(Default)]
pub struct RegisterCoalescing {}

impl RegisterCoalescing {
    pub fn new() -> Self { Self {} }

    pub fn run_on_function<S>(ctx: &mut LowerContext<S>, func: &MFunc<S::I>, config: &LowerConfig)
    where
        S: LowerSpec,
        S::I: Hash,
    {
        let mut live_intervals = live_interval_analysis::analyze_on_function(ctx, *func);

        let mut reg_def_use = RegDefUse::compute_on_function(ctx.mctx(), func, config);
        let mut cursor = func.cursor();
        while let Some(block) = cursor.next(ctx.mctx()) {
            let mut curr_inst = block.head(ctx.mctx());
            while let Some(inst) = curr_inst {
                curr_inst = inst.next(ctx.mctx());

                if let Some((mut to_reg, mut from_reg)) = inst.match_move(ctx.mctx()) {
                    if to_reg == from_reg {
                        // it'll be removed by identical move elimination
                    } else if live_intervals.intervals.contains_key(&to_reg)
                        && live_intervals.intervals.contains_key(&from_reg)
                        && !live_intervals.intervals[&to_reg]
                            .intersects(&live_intervals.intervals[&from_reg], false)
                    {
                        // coalescing only if their live intervals don't intersect
                        if from_reg.is_preg() && to_reg.is_preg() {
                            // don't coalesce physical registers
                            continue;
                        } else if to_reg.is_preg() {
                            // we tend to replace to_reg to from_reg
                            // so if to_reg is a physical register, we swap them
                            swap(&mut to_reg, &mut from_reg);
                        }
                        let to_interval = live_intervals.intervals.remove(&to_reg).unwrap();
                        let from_interval = live_intervals.intervals.remove(&from_reg).unwrap();
                        let new_interval = to_interval.union(&from_interval);
                        live_intervals.intervals.insert(from_reg, new_interval);
                        live_intervals.intervals.remove(&to_reg);

                        // println!(
                        //     "[ reg coalescing ] Coalescing {} to {}",
                        //     display(to_reg),
                        //     display(from_reg),
                        // );

                        // replace all uses of to_reg with from_reg
                        for use_inst in reg_def_use
                            .uses
                            .get(&to_reg)
                            .unwrap_or(&FxHashSet::default())
                            .clone()
                        {
                            use_inst.replace_reg(ctx.mctx_mut(), to_reg, from_reg);
                            reg_def_use.add_use(from_reg, use_inst);
                            reg_def_use.remove_use(to_reg, use_inst);
                        }

                        // replace all defs of to_reg with from_reg
                        for def_inst in reg_def_use
                            .defs
                            .get(&to_reg)
                            .unwrap_or(&FxHashSet::default())
                            .clone()
                        {
                            def_inst.replace_reg(ctx.mctx_mut(), to_reg, from_reg);
                            reg_def_use.add_def(from_reg, def_inst);
                            reg_def_use.remove_def(to_reg, def_inst);
                        }
                    }
                }
            }
        }
    }

    pub fn run<S>(ctx: &mut LowerContext<S>, config: &LowerConfig)
    where
        S: LowerSpec,
        S::I: Hash,
    {
        let funcs = ctx
            .mctx
            .funcs
            .iter_mut()
            .map(|(_, func_data)| func_data.self_ptr())
            .collect::<Vec<_>>();

        for func in funcs {
            if func.is_external(ctx.mctx()) {
                continue;
            }

            Self::run_on_function(ctx, &func, config);
        }
    }
}
