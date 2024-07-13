use std::collections::HashSet;

use rustc_hash::FxHashMap;

use super::block_defuse_analysis;
use crate::{
    backend::{
        inst::MInst,
        regs::Reg,
        riscv64::regs::display,
        LowerContext,
        LowerSpec,
        MBlock,
        MFunc,
    },
    collections::linked_list::LinkedListContainerPtr,
    utils::{cfg::CfgInfo, dfs::DfsContext},
};

#[derive(Debug, Clone)]
pub struct BlockInOut<I> {
    pub in_: FxHashMap<MBlock<I>, HashSet<Reg>>,
    pub out: FxHashMap<MBlock<I>, HashSet<Reg>>,
}

impl<I> Default for BlockInOut<I> {
    fn default() -> Self {
        BlockInOut {
            in_: FxHashMap::default(),
            out: FxHashMap::default(),
        }
    }
}

impl<I> BlockInOut<I>
where
    I: MInst,
{
    pub fn new() -> Self { Self::default() }

    pub fn in_(&self, block: &MBlock<I>) -> Option<&HashSet<Reg>> { self.in_.get(block) }

    pub fn out(&self, block: &MBlock<I>) -> Option<&HashSet<Reg>> { self.out.get(block) }

    pub fn display(&self, ctx: &LowerContext<I::S>) -> String {
        let mut s = String::new();

        for (block, in_) in &self.in_ {
            s.push_str(&format!("{} in: ", block.label(ctx.mctx())));
            for reg in in_ {
                s.push_str(&format!("{}, ", display(*reg)));
            }
            s.push('\n');
        }

        for (block, out) in &self.out {
            s.push_str(&format!("{} out: ", block.label(ctx.mctx())));
            for reg in out {
                s.push_str(&format!("{}, ", display(*reg)));
            }
            s.push('\n');
        }

        s
    }
}

pub fn analyze_on_function<S>(ctx: &LowerContext<S>, func: MFunc<S::I>) -> BlockInOut<S::I>
where
    S: LowerSpec,
{
    let def_uses = block_defuse_analysis::analyze_on_function(ctx, func);

    let cfg = CfgInfo::new(ctx.mctx(), func);
    let mut dfs = DfsContext::<MBlock<S::I>>::default();

    let mut in_ = FxHashMap::default();
    let mut out = FxHashMap::default();

    for block in func.iter(ctx.mctx()) {
        in_.insert(block, HashSet::new());
        out.insert(block, HashSet::new());
    }

    let mut changed = true;
    while changed {
        changed = false;

        for block in dfs.post_order_iter(ctx.mctx(), func) {
            let mut in_set = HashSet::new();
            let mut out_set = HashSet::new();

            // out[B] = U in[S] for all S in succ[B]
            for succ in cfg.succs(block).unwrap() {
                out_set.extend(in_[succ].clone());
            }

            // in[B] = use[B] U (out[B] - def[B])
            in_set.extend(def_uses.uses(&block).unwrap().clone());
            in_set.extend(out_set.difference(def_uses.defs(&block).unwrap()).cloned());

            if in_[&block] != in_set {
                in_.insert(block, in_set);
                changed = true;
            }

            if out[&block] != out_set {
                out.insert(block, out_set);
                changed = true;
            }
        }
    }

    BlockInOut { in_, out }
}
