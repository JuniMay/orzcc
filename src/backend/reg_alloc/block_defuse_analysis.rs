use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    backend::{inst::MInst, regs::Reg, LowerContext, LowerSpec, MBlock, MFunc},
    collections::linked_list::LinkedListContainerPtr,
};

#[derive(Debug, Clone)]
pub struct BlockDefUse<I> {
    pub uses: FxHashMap<MBlock<I>, FxHashSet<Reg>>,
    pub defs: FxHashMap<MBlock<I>, FxHashSet<Reg>>,
}

impl<I> Default for BlockDefUse<I> {
    fn default() -> Self {
        BlockDefUse {
            uses: FxHashMap::default(),
            defs: FxHashMap::default(),
        }
    }
}

impl<I> BlockDefUse<I>
where
    I: MInst,
{
    pub fn new() -> Self { Self::default() }

    pub fn uses(&self, block: &MBlock<I>) -> Option<&FxHashSet<Reg>> { self.uses.get(block) }

    pub fn defs(&self, block: &MBlock<I>) -> Option<&FxHashSet<Reg>> { self.defs.get(block) }

    pub fn display(&self, ctx: &LowerContext<I::S>) -> String {
        let mut s = String::new();

        for (block, uses) in &self.uses {
            s.push_str(&format!("{} uses: ", block.label(ctx.mctx())));
            for reg in uses {
                s.push_str(&format!("{}, ", I::S::display_reg(*reg)));
            }
            s.push('\n');
        }

        for (block, defs) in &self.defs {
            s.push_str(&format!("{} defs: ", block.label(ctx.mctx())));
            for reg in defs {
                s.push_str(&format!("{}, ", I::S::display_reg(*reg)));
            }
            s.push('\n');
        }

        s
    }
}

pub fn analyze_on_function<S>(ctx: &LowerContext<S>, func: MFunc<S::I>) -> BlockDefUse<S::I>
where
    S: LowerSpec,
{
    let mut defuse = BlockDefUse::new();

    for block in func.iter(ctx.mctx()) {
        let mut uses = FxHashSet::default();
        let mut defs = FxHashSet::default();

        for inst in block.iter(ctx.mctx()) {
            for reg in inst.uses(ctx.mctx(), &ctx.config) {
                if !defs.contains(&reg) {
                    uses.insert(reg);
                }
            }
            for reg in inst.defs(ctx.mctx(), &ctx.config) {
                defs.insert(reg);
            }
        }

        // remove non-allocatable registers
        for reg in S::non_allocatable_regs() {
            uses.remove(&reg.into());
            defs.remove(&reg.into());
        }

        defuse.uses.insert(block, uses);
        defuse.defs.insert(block, defs);
    }

    defuse
}
