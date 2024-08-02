use std::collections::VecDeque;

use rustc_hash::FxHashSet;

use super::control_flow::CfgCanonicalize;
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        function_analysis::FunctionAnalysis,
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        remove_all_insts,
        Block,
        Context,
        Func,
        Inst,
        InstKind,
        Value,
        ValueKind,
    },
    utils::{cdg::CdgInfo, cfg::CfgRegion},
};

pub const ADCE: &str = "adce";

/// Aggressive Dead Code Elimination.
pub struct Adce {
    func_analysis: FunctionAnalysis,
}

impl Default for Adce {
    fn default() -> Self {
        Self {
            func_analysis: FunctionAnalysis::new(),
        }
    }
}

impl LocalPassMut for Adce {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        // ref: https://yunmingzhang.wordpress.com/wp-content/uploads/2013/12/dcereport-2.pdf
        // ref: https://www.cnblogs.com/lixingyang/p/17728846.html

        use InstKind as Ik;

        let mut changed = false;

        self.func_analysis.analyze_all(ctx);

        let cdg = CdgInfo::<Block>::new(ctx, func);

        let mut live_params: FxHashSet<Value> = FxHashSet::default();
        let mut live_blocks: FxHashSet<Block> = FxHashSet::default();
        let mut live_insts: FxHashSet<Inst> = FxHashSet::default();

        #[derive(Clone, Copy, Hash, PartialEq, Eq)]
        enum LiveEntry {
            Inst(Inst),
            Param(Value),
        }

        let mut worklist = VecDeque::new();

        for block in func.iter(ctx) {
            for inst in block.iter(ctx) {
                match inst.kind(ctx) {
                    Ik::Undef
                    | Ik::IConst(_)
                    | Ik::FConst(_)
                    | Ik::IBinary(_)
                    | Ik::FBinary(_)
                    | Ik::IUnary(_)
                    | Ik::FUnary(_)
                    | Ik::Cast(_)
                    | Ik::GetGlobal(_)
                    | Ik::Load
                    | Ik::Offset
                    | Ik::StackSlot(_)
                    | Ik::Br
                    | Ik::Jump => {}
                    Ik::Call(sym) => {
                        if let Some(callee) = ctx.lookup_func(sym) {
                            if !self.func_analysis.is_pure(callee) {
                                worklist.push_back(LiveEntry::Inst(inst));
                            }
                        } else {
                            // libfunc, not pure
                            worklist.push_back(LiveEntry::Inst(inst));
                        }
                    }
                    Ik::CallIndirect(_) | Ik::Store | Ik::Ret => {
                        worklist.push_back(LiveEntry::Inst(inst));
                    }
                }
            }
        }

        while let Some(entry) = worklist.pop_front() {
            let block = match entry {
                LiveEntry::Inst(inst) => {
                    if !live_insts.insert(inst) {
                        continue;
                    }

                    for opd in inst.operands(ctx) {
                        match opd.kind(ctx) {
                            ValueKind::InstResult { inst, .. } => {
                                worklist.push_back(LiveEntry::Inst(*inst));
                            }
                            ValueKind::BlockParam { .. } => {
                                worklist.push_back(LiveEntry::Param(opd));
                            }
                        }
                    }

                    // args in successor is processed by param, just treated as a PHI node.

                    inst.container(ctx).unwrap()
                }
                LiveEntry::Param(param) => {
                    if !live_params.insert(param) {
                        continue;
                    }

                    if let ValueKind::BlockParam { block, .. } = param.kind(ctx) {
                        for pred in block.preds(ctx) {
                            let tail = pred.tail(ctx).unwrap();
                            assert!(tail.is_terminator(ctx));

                            // the tail is live.
                            worklist.push_back(LiveEntry::Inst(tail));

                            // the incoming arguments are all live.
                            for succ in tail.succs(ctx) {
                                if succ.block() != *block {
                                    continue;
                                }
                                // only mark the arguments passed to this param as live
                                let incoming = succ.get_arg(param).unwrap();
                                match incoming.kind(ctx) {
                                    ValueKind::InstResult { inst, .. } => {
                                        worklist.push_back(LiveEntry::Inst(*inst));
                                    }
                                    ValueKind::BlockParam { .. } => {
                                        worklist.push_back(LiveEntry::Param(incoming));
                                    }
                                }
                            }
                        }

                        *block
                    } else {
                        unreachable!();
                    }
                }
            };

            // the defining block of the instruction/param is live.
            live_blocks.insert(block);

            for cdg_pred in cdg.preds(block) {
                // add the terminator of cdg preds to live
                let tail = cdg_pred.tail(ctx).unwrap();
                worklist.push_back(LiveEntry::Inst(tail));
                live_blocks.insert(cdg_pred);
            }
        }

        // modify jumps
        let mut blocks_to_redirect = Vec::new();

        let mut cursor = func.cursor();
        while let Some(block) = cursor.next(ctx) {
            let tail = block.tail(ctx).unwrap();
            assert!(tail.is_terminator(ctx), "do canonicalization first");
            if !live_insts.contains(&tail) {
                // jump to the next live post idom
                let mut idom = cdg.idom(block).unwrap();
                while !live_blocks.contains(&idom) {
                    idom = cdg.idom(idom).unwrap();
                }

                tail.remove(ctx);
                blocks_to_redirect.push((block, idom));
                changed = true;
            }
        }

        // now modify argument passing
        // 1. remove all args pass to dead params.
        // 2. drop block params and maintain manually.

        let mut cursor = func.cursor();
        while let Some(block) = cursor.next(ctx) {
            let tail = block.tail(ctx);
            if tail.is_none() {
                continue; // removed
            }
            let tail = tail.unwrap();
            if !tail.is_terminator(ctx) {
                continue; // removed
            }
            let dead_params = tail
                .succs(ctx)
                .iter()
                .flat_map(|succ| succ.args().keys())
                .copied()
                .filter(|param| !live_params.contains(param))
                .collect::<Vec<_>>();
            for param in dead_params.iter().copied() {
                tail.remove_args_passing_to_param(ctx, param);
                changed = true;
            }
        }

        // the possible uses are removed, now remove the dead instructions
        let mut insts_to_remove = Vec::new();
        for block in func.iter(ctx) {
            for inst in block.iter(ctx) {
                if !live_insts.contains(&inst) && !inst.is_terminator(ctx) {
                    insts_to_remove.push(inst);
                    changed = true;
                }
            }
        }
        remove_all_insts(ctx, insts_to_remove, false);

        // possible uses of params are removed, now remove the dead params
        let dead_params = func
            .iter(ctx)
            // ignore entry block, whose parameters are function parameters
            .filter(|block| *block != func.entry_node(ctx))
            .flat_map(|block| block.params(ctx).iter().copied())
            .filter(|param| !live_params.contains(param))
            .collect::<Vec<_>>();

        for param in dead_params {
            // idx is modified after removing, so just get it now
            let idx = param.idx(ctx);
            let block = param.def_block(ctx);
            block.drop_param_without_removing_args(ctx, idx);
            changed = true;
        }

        for (block, idom) in blocks_to_redirect {
            let jump = Inst::jump(ctx, idom, vec![]);
            block.push_back(ctx, jump);
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for Adce {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        for func in ctx.funcs() {
            let ((), local_changed) = LocalPassMut::run(self, ctx, func)?;
            changed |= local_changed;
        }

        Ok(((), changed))
    }
}

impl TransformPass for Adce {
    fn register(passman: &mut crate::ir::passman::PassManager)
    where
        Self: Sized,
    {
        let pass = Self::default();
        passman.register_transform(ADCE, pass, vec![Box::new(CfgCanonicalize)]);
    }
}
