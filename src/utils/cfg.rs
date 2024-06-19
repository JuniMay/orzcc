use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use crate::collections::storage::ArenaPtr;

/// A node in a control flow graph.
pub trait CfgNode: ArenaPtr {
    /// The region type associated with the node.
    type Region: CfgRegion<A = Self::A, Node = Self>;

    /// Get the successors of the node.
    ///
    /// Successors are easy to get from the branch instructions. But for
    /// predecessors, we need to traverse the whole control flow graph.
    fn succs(&self, arena: &Self::A) -> Vec<Self>;
}

/// A trait defining a region that contains control flow graph information.
///
/// `Region` is a concept borrowed from MLIR. But here we don't have nested
/// operations, but only a flat list of instructions. So a function can be
/// regarded as a [CfgRegion].
pub trait CfgRegion: ArenaPtr {
    /// The node type associated with the region.
    type Node: CfgNode<A = Self::A, Region = Self>;

    /// Get the entry node of the region.
    fn entry_node(&self, arena: &Self::A) -> Self::Node;
}

/// Control flow graph information.
pub struct CfgInfo<N, R>
where
    N: CfgNode + Hash,
    R: CfgRegion<A = N::A, Node = N>,
{
    /// The region associated with the control flow graph.
    region: R,
    /// The predecessors of each node.
    succs: HashMap<N, HashSet<N>>,
    /// The successors of each node.
    preds: HashMap<N, HashSet<N>>,
}

impl<N, R> CfgInfo<N, R>
where
    N: CfgNode + Hash,
    R: CfgRegion<A = N::A, Node = N>,
{
    /// Derive the control flow graph information from the region.
    pub fn new(arena: &N::A, region: R) -> Self {
        let mut succs: HashMap<N, HashSet<N>> = HashMap::new();
        let mut preds: HashMap<N, HashSet<N>> = HashMap::new();

        let entry = region.entry_node(arena);

        let mut worklist: Vec<N> = vec![entry];
        let mut visited = HashSet::new();

        while let Some(node) = worklist.pop() {
            if visited.contains(&node) {
                continue;
            }

            visited.insert(node);

            for succ in node.succs(arena) {
                succs.entry(node).or_default().insert(succ);
                preds.entry(succ).or_default().insert(node);
                worklist.push(succ);
            }
        }

        Self {
            region,
            succs,
            preds,
        }
    }

    /// Get the successors of a node.
    pub fn succs(&self, node: N) -> &HashSet<N> { self.succs.get(&node).unwrap() }

    /// Get the predecessors of a node.
    pub fn preds(&self, node: N) -> &HashSet<N> { self.preds.get(&node).unwrap() }

    /// Get the region associated with the control flow graph.
    pub fn region(&self) -> &R { &self.region }
}
