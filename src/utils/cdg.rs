use std::{hash::Hash, marker::PhantomData};

use rustc_hash::FxHashMap;

use super::cfg::{CfgNode, CfgRegion};
use crate::{
    collections::storage::{ArenaAlloc, ArenaDeref, ArenaFree, ArenaPtr, BaseArena, BaseArenaPtr},
    utils::{cfg::CfgInfo, dominance::Dominance},
};

/// Control Dependence Graph (CDG) information.
///
/// Reference: Modern Compiler Implementation in C, Chapter 19.5
#[derive(Debug)]
pub struct CdgInfo<N> {
    succs: FxHashMap<N, Vec<N>>,
    preds: FxHashMap<N, Vec<N>>,
    post_idoms: FxHashMap<N, Option<N>>,
}

struct RevNodeData<N> {
    node: N,
    succs: Vec<RevNode<N>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct RevNode<N> {
    ptr: BaseArenaPtr<RevNodeData<N>>,
    phantom: PhantomData<N>,
}

impl<N> Hash for RevNode<N> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.ptr.hash(state) }
}

struct RevRegionData<N>
where
    N: CfgNode,
{
    entry: RevNode<N>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct RevRegion<N>
where
    N: CfgNode,
{
    ptr: BaseArenaPtr<RevRegionData<N>>,
    phantom: PhantomData<N>,
}

struct RevArena<N>
where
    N: CfgNode,
{
    region: BaseArena<RevRegionData<N>>,
    nodes: BaseArena<RevNodeData<N>>,
}

impl<N> Default for RevArena<N>
where
    N: CfgNode,
{
    fn default() -> Self {
        Self {
            region: BaseArena::default(),
            nodes: BaseArena::default(),
        }
    }
}

impl<N> CdgInfo<N>
where
    N: CfgNode + Hash,
{
    pub fn new(arena: &N::A, region: N::Region) -> Self {
        let cfg = CfgInfo::<N, N::Region>::new(arena, region);

        let mut rev_arena = RevArena::default();

        let mut rev_nodes = FxHashMap::default();

        for node in cfg.reachable_nodes(arena) {
            let rev_node = rev_arena.alloc(RevNodeData {
                node,
                succs: Vec::new(),
            });
            rev_nodes.insert(node, rev_node);
        }

        let mut entry = None;

        for node in cfg.reachable_nodes(arena) {
            let rev_node = rev_nodes[&node];
            // construct the reversed cfg
            for pred in cfg.preds(node).unwrap() {
                rev_node
                    .deref_mut(&mut rev_arena)
                    .succs
                    .push(rev_nodes[pred]);
            }

            if cfg.succs(node).unwrap().is_empty() {
                entry = Some(rev_node);
            }
        }

        let entry = entry.unwrap();

        let rev_region = rev_arena.alloc(RevRegionData { entry });

        let rev_cfg_info = CfgInfo::new(&rev_arena, rev_region);
        let post_dominance = Dominance::new(&rev_arena, &rev_cfg_info);

        let mut succs: FxHashMap<N, Vec<N>> = FxHashMap::default();
        let mut preds: FxHashMap<N, Vec<N>> = FxHashMap::default();
        let mut post_idoms: FxHashMap<N, Option<N>> = FxHashMap::default();

        for node in rev_cfg_info.reachable_nodes(&rev_arena) {
            let y = node.deref(&rev_arena).node;
            let frontier = post_dominance.frontier(node);
            for n in frontier {
                let x = n.deref(&rev_arena).node;
                succs.entry(x).or_default().push(y);
                preds.entry(y).or_default().push(x);
            }

            let idom = post_dominance.idom(node).map(|n| n.deref(&rev_arena).node);
            post_idoms.insert(y, idom);
        }

        Self {
            succs,
            preds,
            post_idoms,
        }
    }

    pub fn succs(&self, node: N) -> Vec<N> { self.succs.get(&node).cloned().unwrap_or_default() }

    pub fn preds(&self, node: N) -> Vec<N> { self.preds.get(&node).cloned().unwrap_or_default() }

    pub fn idom(&self, node: N) -> Option<N> { *self.post_idoms.get(&node).unwrap() }
}

impl<N> CfgNode for RevNode<N>
where
    N: CfgNode,
{
    type Region = RevRegion<N>;

    fn succs(self, arena: &Self::A) -> Vec<Self> { self.deref(arena).succs.clone() }
}

impl<N> CfgRegion for RevRegion<N>
where
    N: CfgNode,
{
    type Node = RevNode<N>;

    fn entry_node(self, arena: &Self::A) -> Self::Node { self.deref(arena).entry }
}

impl<N> ArenaPtr for RevNode<N>
where
    N: CfgNode,
{
    type A = RevArena<N>;
    type T = RevNodeData<N>;

    fn try_deref(self, arena: &Self::A) -> Option<&Self::T> { arena.try_deref(self) }

    fn try_deref_mut(self, arena: &mut Self::A) -> Option<&mut Self::T> {
        arena.try_deref_mut(self)
    }
}

impl<N> ArenaAlloc<RevNodeData<N>, RevNode<N>> for RevArena<N>
where
    N: CfgNode,
{
    fn alloc_with<F>(&mut self, f: F) -> RevNode<N>
    where
        F: FnOnce(RevNode<N>) -> RevNodeData<N>,
    {
        RevNode {
            ptr: self.nodes.alloc_with(|ptr| {
                f(RevNode {
                    ptr,
                    phantom: PhantomData,
                })
            }),
            phantom: PhantomData,
        }
    }
}

impl<N> ArenaDeref<RevNodeData<N>, RevNode<N>> for RevArena<N>
where
    N: CfgNode,
{
    fn try_deref(&self, ptr: RevNode<N>) -> Option<&RevNodeData<N>> {
        self.nodes.try_deref(ptr.ptr)
    }

    fn try_deref_mut(&mut self, ptr: RevNode<N>) -> Option<&mut RevNodeData<N>> {
        self.nodes.try_deref_mut(ptr.ptr)
    }
}

impl<N> ArenaFree<RevNodeData<N>, RevNode<N>> for RevArena<N>
where
    N: CfgNode,
{
    fn free(&mut self, ptr: RevNode<N>) { self.nodes.free(ptr.ptr) }
}

impl<N> ArenaPtr for RevRegion<N>
where
    N: CfgNode,
{
    type A = RevArena<N>;
    type T = RevRegionData<N>;

    fn try_deref(self, arena: &Self::A) -> Option<&Self::T> { arena.try_deref(self) }

    fn try_deref_mut(self, arena: &mut Self::A) -> Option<&mut Self::T> {
        arena.try_deref_mut(self)
    }
}

impl<N> ArenaAlloc<RevRegionData<N>, RevRegion<N>> for RevArena<N>
where
    N: CfgNode,
{
    fn alloc_with<F>(&mut self, f: F) -> RevRegion<N>
    where
        F: FnOnce(RevRegion<N>) -> RevRegionData<N>,
    {
        RevRegion {
            ptr: self.region.alloc_with(|ptr| {
                f(RevRegion {
                    ptr,
                    phantom: PhantomData,
                })
            }),
            phantom: PhantomData,
        }
    }
}

impl<N> ArenaDeref<RevRegionData<N>, RevRegion<N>> for RevArena<N>
where
    N: CfgNode,
{
    fn try_deref(&self, ptr: RevRegion<N>) -> Option<&RevRegionData<N>> {
        self.region.try_deref(ptr.ptr)
    }

    fn try_deref_mut(&mut self, ptr: RevRegion<N>) -> Option<&mut RevRegionData<N>> {
        self.region.try_deref_mut(ptr.ptr)
    }
}

impl<N> ArenaFree<RevRegionData<N>, RevRegion<N>> for RevArena<N>
where
    N: CfgNode,
{
    fn free(&mut self, ptr: RevRegion<N>) { self.region.free(ptr.ptr) }
}
