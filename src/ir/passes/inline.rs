use rustc_hash::FxHashMap;

use super::control_flow::{CfgCanonicalize, CfgSimplify};
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        debug::CommentPos,
        deep_clone::DeepCloneMap,
        passman::{GlobalPassMut, LocalPassMut, ParamStorage, PassResult, TransformPass},
        Block,
        Context,
        Func,
        Inst,
        InstKind,
        SymbolKind,
    },
    utils::{
        cfg::CfgRegion,
        def_use::{Usable, User},
    },
};

pub const INLINE: &str = "inline";

pub struct Inline {
    deep_clone_map: DeepCloneMap,
    max_inline_depth: usize,
    max_inlinable_insn: usize,

    /// Record the depth of inlined instructions.
    ///
    /// This is only for **recursive** call instructions generated by inlining,
    /// because that can cause infinite inlining.
    ///
    /// This is not cleared after inlining, and will be remained across
    /// iterations. Sometimes, because of the insn limit, we may not inline a
    /// function before doing other optimizations, but we do want to inline
    /// it later. So we must maintain this depth counter to prevent infinite
    /// inlining.
    ///
    /// FIXME: loop-unroll does not preserve the depth. A temporary solution is
    /// check how many times a function is called in the caller, and set a
    /// threshold for that.
    depths: FxHashMap<Inst, usize>,
}

#[allow(clippy::derivable_impls)]
impl Default for Inline {
    fn default() -> Self {
        Self {
            deep_clone_map: DeepCloneMap::default(),
            max_inline_depth: 2,
            max_inlinable_insn: 1024,
            depths: FxHashMap::default(),
        }
    }
}

impl Inline {
    fn inline_one_call(
        &mut self,
        ctx: &mut Context,
        func: Func,
        inst: Inst,
        call_count: &FxHashMap<Func, usize>,
    ) -> bool {
        self.deep_clone_map.clear();

        let callee = if let InstKind::Call(sym) = inst.kind(ctx) {
            if let SymbolKind::FuncDef(callee) = ctx.lookup_symbol(sym).unwrap() {
                *callee
            } else {
                return false;
            }
        } else {
            return false;
        };

        if callee == func {
            // not inlining recursive calls, mutual recursion can be inlined (maybe)
            return false;
        }

        if call_count.get(&callee).unwrap_or(&0) > &8 {
            // do not inline too many times
            return false;
        }

        let depth = *self.depths.get(&inst).unwrap_or(&0);

        if depth >= self.max_inline_depth {
            // do not inline too deep
            return false;
        }

        if callee.insn(ctx) > self.max_inlinable_insn {
            // do not inline too large functions
            return false;
        }

        // do not inline recursive calls
        // for callee_block in callee.iter(ctx) {
        //     for calee_inst in callee_block.iter(ctx) {
        //         if let InstKind::Call(calee_sym) = calee_inst.kind(ctx) {
        //             if let SymbolKind::FuncDef(callee_callee) =
        //                 ctx.lookup_symbol(calee_sym).unwrap()
        //             {
        //                 if *callee_callee == callee {
        //                     return false;
        //                 }
        //             }
        //         }
        //     }
        // }

        let results = inst.results(ctx).to_vec();

        // the block holding this call instruction
        let curr_block = inst.container(ctx).unwrap();

        // we do not create jump when splitting, because next block will be a clone of
        // the entry block of the callee

        // a new block, after the call instruction.
        let split_block = curr_block.split(ctx, inst, false);

        // clone the blocks and block params of the callee function.

        // no need to clone the block params of the entry block, we can just map them
        // with the arguments.
        let new_entry = Block::new(ctx);
        for (param, arg) in callee
            .entry_node(ctx)
            .params(ctx)
            .iter()
            .zip(inst.operands(ctx))
        {
            self.deep_clone_map.insert_value(*param, arg);
        }
        curr_block.insert_after(ctx, new_entry);

        let jump = Inst::jump(ctx, new_entry, vec![]);
        curr_block.push_back(ctx, jump);

        // map entry
        self.deep_clone_map
            .insert_block(callee.entry_node(ctx), new_entry);

        // the insert point at the caller
        let mut insert_point = new_entry;

        // the iterating block of the callee, skip the entry block
        let mut callee_cursor = callee.entry_node(ctx).next(ctx);

        // create blocks and block params first, so jumps can be resolved
        while let Some(block) = callee_cursor {
            callee_cursor = block.next(ctx);

            let new_block = Block::new(ctx);

            #[allow(clippy::unnecessary_to_owned)]
            for param in block.params(ctx).to_vec() {
                // create parameters and map them
                let new_param = new_block.new_param(ctx, param.ty(ctx));
                self.deep_clone_map.insert_value(param, new_param);
            }

            // update insert point
            insert_point.insert_after(ctx, new_block);
            insert_point = new_block;

            self.deep_clone_map.insert_block(block, new_block);
        }

        // now clone the instructions, some special cases may need to be handled
        // - stack slots must be placed in the entry block of this caller
        // - ret instruction will be replaced with a jump to the split block
        let mut callee_cursor = callee.head(ctx);
        while let Some(block) = callee_cursor {
            callee_cursor = block.next(ctx);

            let new_block = self.deep_clone_map.get_block(block).unwrap();

            let mut inst_cursor = block.cursor();
            while let Some(inst) = inst_cursor.next(ctx) {
                if inst.is_stack_slot(ctx) {
                    let cloned = inst.deep_clone(ctx, &mut self.deep_clone_map);
                    func.entry_node(ctx).push_front(ctx, cloned);
                } else if inst.is_ret(ctx) {
                    // not clone, just jump
                    let jump = Inst::jump(ctx, split_block, vec![]);
                    new_block.push_back(ctx, jump);

                    // replace all uses
                    for (result, opd) in results.iter().zip(inst.operands(ctx)) {
                        let new_opd = self.deep_clone_map.get_value(opd).unwrap();
                        for user in result.users(ctx) {
                            user.replace(ctx, *result, new_opd);
                        }
                    }
                } else {
                    let cloned = inst.deep_clone(ctx, &mut self.deep_clone_map);
                    new_block.push_back(ctx, cloned);

                    if let InstKind::Call(sym) = cloned.kind(ctx) {
                        // record the depth of the inlined call instruction
                        if sym == callee.name(ctx) {
                            // only record when the callee of the cloned inst is the same as the
                            // callee of the original inst, i.e., a recursive call.
                            //
                            // for those non-recursive calls, we can safely ignore them, because
                            // they will not cause infinite inlining. As long as the insn limit
                            // is not reached, we can expand them.
                            self.depths.insert(cloned, depth + 1);
                            cloned.comment(ctx, CommentPos::Before, format!("depth={}", depth + 1));
                        }
                    }
                }
            }
        }

        // remove the call instruction
        inst.remove(ctx);
        self.depths.remove(&inst);

        true
    }
}

impl LocalPassMut for Inline {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        let mut call_count = FxHashMap::default();

        // TODO: temporary solution, not very good.
        for block in func.iter(ctx) {
            for inst in block.iter(ctx) {
                if let InstKind::Call(sym) = inst.kind(ctx) {
                    if let Some(func) = ctx.lookup_func(sym) {
                        *call_count.entry(func).or_insert(0) += 1;
                    }
                }
            }
        }

        // using cursor, because after inlining, we want to continue from the newly
        // inlined block.
        let mut cursor = func.cursor();
        while let Some(block) = cursor.next(ctx) {
            // manually iterate, because we may remove the call instruction, and want to
            // continue to the next instruction before inlining.
            let mut curr_inst = block.head(ctx);
            while let Some(inst) = curr_inst {
                curr_inst = inst.next(ctx);
                if inst.is_call(ctx) {
                    changed |= self.inline_one_call(ctx, func, inst, &call_count);
                }
            }
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for Inline {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        // TODO: we can inline functions from `main` to avoid large insn count, and
        // switch to other functions when `main` is done. But it is hard to iterate.

        let mut changed = false;
        for func in ctx.funcs() {
            let (_, local_changed) = LocalPassMut::run(self, ctx, func)?;
            changed |= local_changed;
        }
        Ok(((), changed))
    }

    fn fetch_params(&mut self, params: &ParamStorage) {
        self.max_inline_depth = params
            .get("inline-max-depth")
            .unwrap_or(self.max_inline_depth);

        self.max_inlinable_insn = params
            .get("inline-max-insn")
            .unwrap_or(self.max_inlinable_insn);

        println!(
            "[ inline config ] inline: max_depth={}, max_insn={}",
            self.max_inline_depth, self.max_inlinable_insn
        );
    }
}

impl TransformPass for Inline {
    fn register(passman: &mut crate::ir::passman::PassManager) {
        let pass = Self::default();
        // using cfg-canonicalize to remove unreachable blocks, and cfg-simplify to
        // inlining easiermake
        // XXX: unreachable codes might have wrong block arguments,
        // which will cause compiler panic. An example is `fft.sy` in SysY testcases.
        passman.register_transform(
            INLINE,
            pass,
            vec![Box::new(CfgCanonicalize), Box::new(CfgSimplify)],
        );

        passman.add_parameter("inline-max-depth", 2);
        passman.add_parameter("inline-max-insn", 512);
    }
}
