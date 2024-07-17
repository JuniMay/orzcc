use std::{
    collections::{HashSet, VecDeque},
    hash::Hash,
};

use rustc_hash::FxHashMap;

use super::{
    cfg::{CfgInfo, CfgNode},
    def_use::Usable,
    dominance::Dominance,
};
use crate::{
    collections::{
        linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
        storage::{ArenaAlloc, ArenaDeref, ArenaPtr, BaseArena, BaseArenaPtr},
    },
    ir,
};

/// Represents a natural loop in the control flow graph.
pub struct LoopInfo<N> {
    /// The header of the loop.
    header: N,
    /// The parent loop of this loop.
    parent: Option<Loop<N>>,
    /// The children loops of this loop.
    children: Vec<Loop<N>>,
    /// The depth of the loop.
    depth: u32,
}

#[derive(Debug)]
pub struct Loop<N>(BaseArenaPtr<LoopInfo<N>>);

pub struct LoopContext<N> {
    loops: BaseArena<LoopInfo<N>>,
    node_to_loop: FxHashMap<N, Loop<N>>,
}

impl<N> Loop<N>
where
    N: CfgNode + Hash,
{
    fn new(loop_ctx: &mut LoopContext<N>, header: N) -> Self {
        loop_ctx.alloc(LoopInfo {
            header,
            parent: None,
            children: Vec::new(),
            depth: 0,
        })
    }

    pub fn header(self, loop_ctx: &LoopContext<N>) -> N { self.deref(loop_ctx).header }

    pub fn parent(self, loop_ctx: &LoopContext<N>) -> Option<Loop<N>> {
        self.deref(loop_ctx).parent
    }

    fn set_parent(self, loop_ctx: &mut LoopContext<N>, parent: Option<Loop<N>>) {
        if let Some(parent) = parent {
            parent.deref_mut(loop_ctx).children.push(self);
        }
        self.deref_mut(loop_ctx).parent = parent;
    }

    pub fn depth(self, loop_ctx: &LoopContext<N>) -> u32 { self.deref(loop_ctx).depth }

    fn set_depth(self, loop_ctx: &mut LoopContext<N>, depth: u32) {
        self.deref_mut(loop_ctx).depth = depth
    }

    pub fn children(self, loop_ctx: &LoopContext<N>) -> Vec<Loop<N>> {
        self.deref(loop_ctx).children.clone()
    }

    pub fn is_child_of(self, loop_ctx: &LoopContext<N>, parent: Loop<N>) -> bool {
        if self == parent {
            return true;
        }
        let mut lp = self;
        while let Some(p) = lp.parent(loop_ctx) {
            if p == parent {
                return true;
            }
            lp = p;
        }
        false
    }
}

impl Loop<ir::Block> {
    /// Get the preheader of the loop.
    ///
    /// This is specialized for IR, to get rid of the use of [CfgInfo].
    pub fn get_preheader(
        self,
        ctx: &ir::Context,
        loop_ctx: &LoopContext<ir::Block>,
    ) -> Option<ir::Block> {
        let preds = self.header(loop_ctx).preds(ctx);

        // header has only one predecessor from outside the loop
        // the predecessor only has one successor which is the header
        let out_loop_preds = preds
            .into_iter()
            .filter(|p| !loop_ctx.is_in_loop(*p, self))
            .collect::<Vec<_>>();

        if out_loop_preds.len() == 1 {
            // possible preheader
            let pred = out_loop_preds[0];
            if pred.succs(ctx).len() == 1 {
                // yeah, only one successor, which is the header, so it is the preheader
                return Some(pred);
            }
        }
        None
    }

    /// Get all the values that are defined in the loop but used outside the
    /// loop.
    pub fn get_unclosed_values(
        self,
        ctx: &ir::Context,
        loop_ctx: &LoopContext<ir::Block>,
    ) -> HashSet<ir::Value> {
        let header = self.header(loop_ctx);

        let mut queue = VecDeque::new();
        let mut visited = HashSet::new();

        queue.push_back(header);

        let mut values = HashSet::new();

        while let Some(block) = queue.pop_front() {
            if !visited.insert(block) {
                continue;
            }

            for inst in block.iter(ctx) {
                for result in inst.results(ctx) {
                    for user in result.users(ctx) {
                        let user_block = user.container(ctx).unwrap();
                        if !loop_ctx.is_in_loop(user_block, self) {
                            values.insert(*result);
                        }
                    }
                }
            }
        }

        values
    }
}

pub struct LoopWithDepth<N> {
    pub lp: Loop<N>,
    pub depth: u32,
}

impl<N> PartialEq for LoopWithDepth<N> {
    fn eq(&self, other: &Self) -> bool { self.depth == other.depth }
}

impl<N> Eq for LoopWithDepth<N> {}

impl<N> PartialOrd for LoopWithDepth<N> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.depth.cmp(&other.depth))
    }
}

impl<N> Ord for LoopWithDepth<N> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering { self.depth.cmp(&other.depth) }
}

impl<N> Default for LoopContext<N> {
    fn default() -> Self {
        Self {
            loops: BaseArena::default(),
            node_to_loop: FxHashMap::default(),
        }
    }
}

impl<N> LoopContext<N>
where
    N: CfgNode + Hash,
{
    pub fn new(cfg: &CfgInfo<N, N::Region>, dominance: &Dominance<N>) -> Self {
        let mut ctx = LoopContext::default();
        ctx.detect_loops(cfg, dominance);
        ctx
    }

    pub fn get_loop(&self, node: N) -> Option<Loop<N>> { self.node_to_loop.get(&node).copied() }

    pub fn loops(&self) -> Vec<Loop<N>> { self.loops.iter().map(|(p, _)| Loop(p)).collect() }

    pub fn is_in_loop(&self, node: N, lp: Loop<N>) -> bool {
        match self.node_to_loop.get(&node) {
            Some(imm_loop) => imm_loop.is_child_of(self, lp),
            None => false,
        }
    }

    pub fn add_node_to_loop(&mut self, node: N, lp: Loop<N>) { self.node_to_loop.insert(node, lp); }

    fn detect_loops(&mut self, cfg: &CfgInfo<N, N::Region>, dominance: &Dominance<N>) {
        // a sequence of detected loops
        let mut loops = Vec::new();

        // traverse in reverse post order first to get all the back edges
        for node in dominance.rpo().iter().copied() {
            for pred in cfg.preds(node).unwrap().iter().copied() {
                // this node dominates its predecessor, there is a back edge
                if dominance.dominates(node, pred) {
                    let lp = Loop::new(self, node);
                    self.node_to_loop.insert(node, lp);
                    // because we traverse in reverse post order, so the outer loop is
                    // created before the inner loop
                    loops.push(lp);
                    break;
                }
            }
        }

        // ref: cranelift/codegen/loop_analysis.rs, `discover_loop_blocks`
        //
        // Loop analysis in Cranelift traverse loop header in reverse order (in
        // the `PrimaryMap`), which most likely visits the inner loop before the
        // outer loop, and thus create the loop tree/hierarchy while building
        // the loop blocks.

        let mut stack = Vec::new();

        for lp in loops.into_iter().rev() {
            let header = lp.header(self);
            for pred in cfg.preds(header).unwrap().iter().copied() {
                if dominance.dominates(header, pred) {
                    // we push all the back edges into the stack, it seems that
                    // multiple loops with the same header will be merged automatically
                    stack.push(pred);
                }
            }

            while let Some(node) = stack.pop() {
                match self.node_to_loop.get(&node) {
                    None => {
                        // this node is in this loop
                        self.node_to_loop.insert(node, lp);
                        // also need to push all preds of node into the stack
                        for pred in cfg.preds(node).unwrap().iter().copied() {
                            stack.push(pred);
                        }
                    }
                    Some(inner) => {
                        // this node is visited before
                        let mut inner = *inner;
                        while let Some(p) = inner.parent(self) {
                            if p == lp {
                                // this node is visited
                                break;
                            } else {
                                // go up to the parent loop
                                inner = p;
                            }
                        }
                        if inner.parent(self).is_none() {
                            // we reached the top of the loop hierarchy, all the loops are not
                            // included in this loop, so we can set the parent loop of the inner to
                            // this loop
                            if inner == lp {
                                // the inner loop has no parent, and is
                                // identical to the current one, do nothing
                            } else {
                                // set the parent loop of the inner loop to this loop
                                inner.set_parent(self, Some(lp));
                                // continue to traverse from the preds of the header of this loop
                                let header = inner.header(self);
                                for pred in cfg.preds(header).unwrap().iter().copied() {
                                    stack.push(pred);
                                }
                            }
                        } else {
                            // the parent is this loop, do nothing
                        }
                    }
                }
            }
        }

        // then, calculate the depth of each loop.
        let mut stack = Vec::new();
        for lp in self.loops() {
            if lp.depth(self) == 0 {
                stack.push(lp);
                while let Some(lp) = stack.last() {
                    match lp.parent(self) {
                        None => {
                            lp.set_depth(self, 1);
                            stack.pop();
                        }
                        Some(p) => {
                            if p.depth(self) == 0 {
                                stack.push(p);
                            } else {
                                lp.set_depth(self, p.depth(self) + 1);
                                stack.pop();
                            }
                        }
                    }
                }
            }
        }
    }
}

impl<N> Clone for Loop<N> {
    fn clone(&self) -> Self { *self }
}

impl<N> Copy for Loop<N> {}

impl<N> PartialEq for Loop<N> {
    fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}

impl<N> Eq for Loop<N> {}

impl<N> Hash for Loop<N> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.0.hash(state) }
}

impl<N> ArenaPtr for Loop<N> {
    type A = LoopContext<N>;
    type T = LoopInfo<N>;

    fn try_deref(self, arena: &Self::A) -> Option<&Self::T> { ArenaDeref::try_deref(arena, self) }

    fn try_deref_mut(self, arena: &mut Self::A) -> Option<&mut Self::T> {
        ArenaDeref::try_deref_mut(arena, self)
    }
}

impl<N> ArenaDeref<LoopInfo<N>, Loop<N>> for LoopContext<N> {
    fn try_deref(&self, ptr: Loop<N>) -> Option<&LoopInfo<N>> { self.loops.try_deref(ptr.0) }

    fn try_deref_mut(&mut self, ptr: Loop<N>) -> Option<&mut LoopInfo<N>> {
        self.loops.try_deref_mut(ptr.0)
    }
}

impl<N> ArenaAlloc<LoopInfo<N>, Loop<N>> for LoopContext<N> {
    fn alloc_with<F>(&mut self, f: F) -> Loop<N>
    where
        F: FnOnce(Loop<N>) -> LoopInfo<N>,
    {
        Loop(self.loops.alloc_with(|p| f(Loop(p))))
    }
}
