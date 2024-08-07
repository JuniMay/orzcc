use super::{Lcssa, LoopSimplify};
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        deep_clone::DeepCloneMap,
        passes::control_flow::CfgCanonicalize,
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        Block,
        Context,
        Func,
    },
    utils::{
        cfg::{CfgInfo, CfgNode},
        def_use::User,
        dominance::Dominance,
        loop_info::{Loop, LoopContext, LoopWithDepth},
    },
};

pub const LOOP_PEEL: &str = "loop-peel";

#[derive(Default)]
pub struct LoopPeel {
    /// Loops
    loop_ctx: LoopContext<Block>,
    /// Deep clone map for cloning instructions.
    deep_clone_map: DeepCloneMap,
}

impl LoopPeel {
    fn process_loop(&mut self, ctx: &mut Context, lp: Loop<Block>) -> bool {
        let preheader = lp.get_preheader(ctx, &self.loop_ctx).unwrap();
        let header = lp.header(&self.loop_ctx);
        let blocks = lp.get_blocks(ctx, &self.loop_ctx);

        self.deep_clone_map.clear();
        let mut insertion_point = preheader;

        for block in blocks.iter() {
            let new_block = Block::new(ctx);

            #[allow(clippy::unnecessary_to_owned)]
            for param in block.params(ctx).to_vec() {
                let new_param = new_block.new_param(ctx, param.ty(ctx));
                self.deep_clone_map.insert_value(param, new_param);
            }

            if *block == header {
                // redirect the initial jump to the new header
                preheader.tail(ctx).unwrap().replace(ctx, header, new_block);
            }

            self.deep_clone_map.insert_block(*block, new_block);

            insertion_point.insert_after(ctx, new_block);
            insertion_point = new_block;
        }

        for block in blocks.iter() {
            let new_block = self.deep_clone_map.get_block(*block).unwrap();
            let mut cursor = block.cursor();

            while let Some(inst) = cursor.next(ctx) {
                let new_inst = inst.deep_clone(ctx, &mut self.deep_clone_map);
                new_block.push_back(ctx, new_inst);
            }

            for succ in block.succs(ctx) {
                if succ == header {
                    // this block has a backedge, bu the new block jumps back to
                    // the new header, we need to replace it to the old header.
                    let tail = new_block.tail(ctx).unwrap();
                    let new_header = self.deep_clone_map.get_block(header).unwrap();
                    tail.replace(ctx, new_header, header);
                }
            }
        }

        true
    }
}

impl LocalPassMut for LoopPeel {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let cfg = CfgInfo::new(ctx, func);
        let dominance = Dominance::new(ctx, &cfg);

        self.loop_ctx = LoopContext::new(&cfg, &dominance);

        let mut loops = Vec::new();

        for lp in self.loop_ctx.loops() {
            let depth = lp.depth(&self.loop_ctx);
            loops.push(LoopWithDepth { lp, depth });
        }

        loops.sort();

        for LoopWithDepth { lp, .. } in loops {
            if self.process_loop(ctx, lp) {
                return Ok(((), true));
            }
        }

        Ok(((), false))
    }
}

impl GlobalPassMut for LoopPeel {
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

impl TransformPass for LoopPeel {
    fn register(passman: &mut crate::ir::passman::PassManager)
    where
        Self: Sized,
    {
        let pass = Self::default();

        passman.register_transform(
            LOOP_PEEL,
            pass,
            vec![
                Box::new(CfgCanonicalize),
                Box::new(LoopSimplify::default()),
                Box::new(Lcssa::default()),
            ],
        );
    }
}
