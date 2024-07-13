use std::collections::VecDeque;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passes::control_flow::CfgCanonicalize,
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        Block,
        Context,
        Func,
        Inst,
        InstKind,
        ValueKind,
    },
    utils::{
        cfg::{CfgInfo, CfgNode},
        def_use::User,
        dominance::Dominance,
        loop_info::{Loop, LoopContext},
    },
};

pub const LOOP_INVARIANT_MOTION: &str = "loop-invariant-motion";

#[derive(Default)]
pub struct LoopInvariantMotion {
    loop_ctx: LoopContext<Block>,
}

impl LoopInvariantMotion {
    pub fn process_loop(
        &mut self,
        ctx: &mut Context,
        lp: Loop<Block>,
        cfg: &CfgInfo<Block, Func>,
    ) -> bool {
        use InstKind as Ik;
        // we need to create a preheader block to hold the moved instructions
        let preheader = Block::new(ctx);

        let header = lp.header(&self.loop_ctx);

        let mut visited = FxHashSet::default();
        let mut queue = VecDeque::new();
        let mut changed = false;

        queue.push_back(header);

        while let Some(block) = queue.pop_front() {
            if !visited.insert(block) {
                continue;
            }

            let mut next_inst = block.head(ctx);
            while let Some(inst) = next_inst {
                next_inst = inst.next(ctx);

                let mut movable = true;

                for opd in inst.operands(ctx) {
                    match opd.kind(ctx) {
                        ValueKind::BlockParam { block, .. } => {
                            // check if the block is in this loop
                            if self.loop_ctx.is_in_loop(*block, lp) {
                                movable = false;
                                break;
                            }
                        }
                        ValueKind::InstResult { inst, .. } => {
                            // check if the instruction is in this loop
                            let def_block = inst.container(ctx).unwrap();
                            if self.loop_ctx.is_in_loop(def_block, lp) {
                                movable = false;
                                break;
                            }
                        }
                    }
                }

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
                    | Ik::Offset => {
                        // these instructions are movable
                    }
                    Ik::Call(_) | Ik::CallIndirect(_) => {
                        // TODO: we need to decide if there are any side effects
                        movable = false;
                    }
                    Ik::Load => {
                        // we don't know if any store in the loop modifies the
                        // address, so we can't move the load
                        movable = false;
                    }
                    Ik::Store | Ik::Br | Ik::Jump | Ik::Ret => {
                        // these instructions are not movable
                        movable = false;
                    }
                    // stack slot should not appear in the loop
                    Ik::StackSlot(_) => unreachable!(),
                }

                // if the instruction is movable, we can unlink and move it to
                // the preheader
                if movable {
                    inst.unlink(ctx);
                    preheader.push_back(ctx, inst);
                    changed = true;
                }
            }

            for succ in block.succs(ctx) {
                if self.loop_ctx.is_in_loop(succ, lp) {
                    queue.push_back(succ);
                }
            }
        }

        if !changed {
            preheader.remove(ctx);
            return false;
        }

        // there can be multiple predecessors of the header, we need to maintain the
        // block params. It can be sure that all the predecessors have identical
        // block params, so we can just copy all the block params from the header to
        // the preheader, and modify the branch/jump instructions in the predecessors
        // to jump to the preheader, and make a jump from the preheader to the header.

        let preds = cfg.preds(header).unwrap();

        let mut param_mapping = FxHashMap::default();
        let mut preheader_jump_args = Vec::new();

        #[allow(clippy::unnecessary_to_owned)] // inacurrate clippy
        for param in header.params(ctx).to_vec() {
            let ty = param.ty(ctx);
            let new_param = preheader.new_param(ctx, ty);
            param_mapping.insert(param, new_param);
            preheader_jump_args.push(new_param);
        }

        let preheader_jump = Inst::jump(ctx, header, preheader_jump_args);
        preheader.push_back(ctx, preheader_jump);

        for pred in preds {
            // we should skip the preds inside the loop
            if self.loop_ctx.is_in_loop(*pred, lp) {
                continue;
            }

            let tail_inst = pred.tail(ctx);

            match tail_inst {
                Some(inst) if inst.is_terminator(ctx) => {
                    inst.replace(ctx, header, preheader);
                }
                Some(_) => {
                    panic!("block tail is not a terminator, do canonicalization first");
                }
                None => {
                    panic!("block has no tail, do canonicalization first");
                }
            }
        }

        // insert the preheader before header
        header.insert_before(ctx, preheader);

        // also, we need to map the preheader to the upper loop, or just not map it
        // if this is a top-level loop
        if let Some(parent) = lp.parent(&self.loop_ctx) {
            self.loop_ctx.add_node_to_loop(preheader, parent);
        }

        changed
    }
}

impl LocalPassMut for LoopInvariantMotion {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        // we should iterate loop from inner to outer, so that we can move as many
        // instructions as possible to the preheader of the outermost loop

        let cfg = CfgInfo::new(ctx, func);
        let dominance = Dominance::new(ctx, &cfg);

        self.loop_ctx = LoopContext::new(&cfg, &dominance);

        struct LoopWithDepth {
            lp: Loop<Block>,
            depth: u32,
        }

        impl PartialEq for LoopWithDepth {
            fn eq(&self, other: &Self) -> bool { self.depth == other.depth }
        }

        impl Eq for LoopWithDepth {}

        impl PartialOrd for LoopWithDepth {
            fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
                Some(self.depth.cmp(&other.depth))
            }
        }

        impl Ord for LoopWithDepth {
            fn cmp(&self, other: &Self) -> std::cmp::Ordering { self.depth.cmp(&other.depth) }
        }

        let mut loops = Vec::new();

        for lp in self.loop_ctx.loops() {
            let depth = lp.depth(&self.loop_ctx);
            loops.push(LoopWithDepth { lp, depth });
        }

        loops.sort();
        loops.reverse();

        let mut changed = false;

        for LoopWithDepth { lp, .. } in loops {
            changed |= self.process_loop(ctx, lp, &cfg);
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for LoopInvariantMotion {
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

impl TransformPass for LoopInvariantMotion {
    fn register(passman: &mut crate::ir::passman::PassManager) {
        let pass = Self::default();
        passman.register_transform(LOOP_INVARIANT_MOTION, pass, vec![Box::new(CfgCanonicalize)]);
    }
}
