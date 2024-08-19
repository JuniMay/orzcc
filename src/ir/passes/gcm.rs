use rustc_hash::{FxHashMap, FxHashSet};

use super::{control_flow::CfgCanonicalize, gvn::GVNInst, simple_dce::SimpleDce};
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassManager, PassResult, TransformPass},
        Block,
        Context,
        Func,
        Inst,
        InstKind,
        Value,
        ValueKind,
    },
    utils::{
        cfg::{CfgInfo, CfgRegion},
        def_use::{Usable, User},
        dominance::Dominance,
        loop_info::LoopContext,
    },
};

pub const GCM: &str = "gcm";

#[derive(Default)]
pub struct Gcm {
    visited: FxHashSet<Inst>,
    dominance: Dominance<Block>,
    loop_ctx: LoopContext<Block>,
    dce: SimpleDce,
}

impl Gcm {
    fn schedule_early(&mut self, ctx: &mut Context, inst: Inst, func: Func) {
        if !self.visited.insert(inst) || Self::is_pinned(ctx, inst) {
            return;
        }

        let entry = func.entry_node(ctx);
        inst.unlink(ctx);
        entry.push_inst_before_terminator(ctx, inst);

        for opd in inst.operands(ctx) {
            let opd_block = match opd.kind(ctx) {
                ValueKind::InstResult { inst, .. } => {
                    let inst = *inst;
                    self.schedule_early(ctx, inst, func);
                    inst.container(ctx).unwrap()
                }
                ValueKind::BlockParam { block, .. } => *block,
            };

            if self.dominance.level(inst.container(ctx).unwrap()) < self.dominance.level(opd_block)
            {
                inst.unlink(ctx);
                opd_block.push_inst_before_terminator(ctx, inst);
            }
        }
    }

    fn schedule_late(&mut self, ctx: &mut Context, inst: Inst) {
        if !self.visited.insert(inst) || Self::is_pinned(ctx, inst) {
            return;
        }

        let mut lca = None;
        let mut has_uses = false;

        #[allow(clippy::unnecessary_to_owned)]
        for result in inst.results(ctx).to_vec() {
            for user in result.users(ctx) {
                self.schedule_late(ctx, user);
                let user_block = user.container(ctx).unwrap();
                lca = Some(match lca {
                    Some(lca) => self.lca(lca, user_block),
                    None => user_block,
                });
                has_uses = true;
            }
        }

        if has_uses {
            let mut best_block = lca.unwrap();
            let mut curr_block = best_block;

            // after schedule early, the container should be the `early block`
            while curr_block != inst.container(ctx).unwrap() {
                curr_block = self.dominance.idom(curr_block).unwrap_or(curr_block);

                let lp_curr = self.loop_ctx.get_loop(curr_block);
                let lp_best = self.loop_ctx.get_loop(best_block);

                let curr_depth = lp_curr.map_or(0, |lp| lp.depth(&self.loop_ctx));
                let best_depth = lp_best.map_or(0, |lp| lp.depth(&self.loop_ctx));

                if curr_depth < best_depth {
                    best_block = curr_block;
                }
            }

            inst.unlink(ctx);
            best_block.push_inst_before_terminator(ctx, inst);
        }

        let best_block = inst.container(ctx).unwrap();

        // place the instruction before its users
        let results: FxHashSet<Value> = inst.results(ctx).iter().copied().collect();

        for best_block_inst in best_block.iter(ctx) {
            if best_block_inst == inst {
                continue;
            }
            if best_block_inst
                .operands(ctx)
                .iter()
                .any(|opd| results.contains(opd))
            {
                // this inst uses the result of the inst, we need to place the inst before it
                inst.unlink(ctx);
                best_block_inst.insert_before(ctx, inst);
                // this is the first instruction that uses the result, we can break
                break;
            }
        }
    }

    fn is_pinned(ctx: &Context, inst: Inst) -> bool {
        use InstKind as Ik;

        match inst.kind(ctx) {
            Ik::Br | Ik::Ret | Ik::Jump => true,
            // not sure about side effects
            Ik::Call(_) | Ik::CallIndirect(_) => true,
            Ik::Load | Ik::Store | Ik::LoadElem { .. } | Ik::StoreElem { .. } => true,
            // should not move stack slot instruction.
            Ik::StackSlot(_) => true,
            Ik::Undef
            | Ik::IConst(_)
            | Ik::FConst(_)
            | Ik::IBinary(_)
            | Ik::FBinary(_)
            | Ik::IUnary(_)
            | Ik::FUnary(_)
            | Ik::Cast(_)
            | Ik::Offset
            | Ik::GetGlobal(_) => false,
        }
    }

    fn lca(&mut self, a: Block, b: Block) -> Block {
        if a == b {
            return a;
        }

        let mut a = a;
        let mut b = b;

        while self.dominance.level(a) < self.dominance.level(b) {
            b = self.dominance.idom(b).unwrap_or(b);
        }

        while self.dominance.level(b) < self.dominance.level(a) {
            a = self.dominance.idom(a).unwrap_or(a);
        }

        while a != b {
            a = self.dominance.idom(a).unwrap_or(a);
            b = self.dominance.idom(b).unwrap_or(b);
        }

        a
    }

    fn gvn(&mut self, ctx: &mut Context, insts: &[Inst]) -> bool {
        let mut vmap: FxHashMap<GVNInst, Vec<Value>> = FxHashMap::default();

        let mut changed = false;

        for inst in insts.iter() {
            match inst.kind(ctx) {
                InstKind::IConst(_)
                | InstKind::FConst(_)
                | InstKind::IBinary(_)
                | InstKind::FBinary(_)
                | InstKind::IUnary(_)
                | InstKind::FUnary(_)
                | InstKind::Cast(_)
                | InstKind::Offset
                | InstKind::GetGlobal(_) => {
                    let gvn_inst = GVNInst::from_inst(ctx, *inst);

                    if let Some(value) = vmap.get(&gvn_inst) {
                        for (old_value, new_value) in inst
                            .results(ctx)
                            .to_vec()
                            .iter()
                            .copied()
                            .zip(value.iter().copied())
                        {
                            for user in old_value.users(ctx) {
                                user.replace(ctx, old_value, new_value);
                                changed = true;
                            }
                        }
                    } else {
                        vmap.insert(gvn_inst, inst.results(ctx).to_vec());
                    }
                }
                InstKind::Undef
                | InstKind::StackSlot(_)
                | InstKind::Jump
                | InstKind::Br
                | InstKind::Call(_)
                | InstKind::CallIndirect(_)
                | InstKind::Ret
                | InstKind::Load
                | InstKind::Store
                | InstKind::LoadElem { .. }
                | InstKind::StoreElem { .. } => {}
            }
        }

        changed
    }
}

impl LocalPassMut for Gcm {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        self.visited.clear();

        let mut changed = false;

        let cfg = CfgInfo::new(ctx, func);
        self.dominance = Dominance::new(ctx, &cfg);
        self.loop_ctx = LoopContext::new(&cfg, &self.dominance);

        let rpo = self.dominance.rpo().to_vec();

        let mut insts = Vec::new();

        for block in rpo.iter() {
            for inst in block.iter(ctx) {
                insts.push(inst);
            }
        }

        let mut gvn_count = 0;
        while self.gvn(ctx, &insts) {
            changed = true;
            gvn_count += 1;
            if gvn_count > 8 {
                break;
            }
        }

        // println!("[ gcm ] GVN iterations: {}", gvn_count);

        for inst in insts.iter() {
            self.schedule_early(ctx, *inst, func);
        }

        self.visited.clear();
        for inst in insts.iter().rev() {
            self.schedule_late(ctx, *inst);
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for Gcm {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;
        for func in ctx.funcs() {
            let (_, local_changed) = LocalPassMut::run(self, ctx, func)?;
            changed |= local_changed;
        }
        changed |= GlobalPassMut::run(&mut self.dce, ctx)?.1;
        Ok(((), changed))
    }
}

impl TransformPass for Gcm {
    fn register(passman: &mut PassManager) {
        let pass = Gcm::default();
        passman.register_transform(GCM, pass, vec![Box::new(CfgCanonicalize)]);
    }
}
