use std::hash::Hash;

use rustc_hash::{FxHashMap, FxHashSet};

use crate::collections::storage::ArenaPtr;

/// A node in a control flow graph.
pub trait CfgNode: ArenaPtr {
    /// The region type associated with the node.
    type Region: CfgRegion<A = Self::A, Node = Self>;

    /// Get the successors of the node.
    ///
    /// Successors are easy to get from the branch instructions. But for
    /// predecessors, we need to traverse the whole control flow graph.
    fn succs(self, arena: &Self::A) -> Vec<Self>;
}

/// A trait defining a region that contains control flow graph information.
///
/// `Region` is a concept borrowed from MLIR. But here we don't have nested
/// operations, but only a flat list of instructions. So a function can be
/// regarded as a [CfgRegion].
pub trait CfgRegion: ArenaPtr {
    /// The node type associated with the region.
    type Node: CfgNode<A = Self::A, Region = Self> + Hash;

    /// Get the entry node of the region.
    fn entry_node(self, arena: &Self::A) -> Self::Node;

    /// Generate the control flow information for the region.
    fn cfg_info(self, arena: &Self::A) -> CfgInfo<Self::Node, Self> { CfgInfo::new(arena, self) }
}

/// Control flow graph information.
pub struct CfgInfo<N, R>
where
    N: CfgNode,
    R: CfgRegion<A = N::A, Node = N>,
{
    /// The region associated with the control flow graph.
    region: R,
    /// The predecessors of each node.
    succs: FxHashMap<N, Vec<N>>,
    /// The successors of each node.
    preds: FxHashMap<N, Vec<N>>,
}

impl<N, R> CfgInfo<N, R>
where
    N: CfgNode + Hash,
    R: CfgRegion<A = N::A, Node = N>,
{
    /// Derive the control flow graph information from the region.
    pub fn new(arena: &N::A, region: R) -> Self {
        let mut succs: FxHashMap<N, Vec<N>> = FxHashMap::default();
        let mut preds: FxHashMap<N, Vec<N>> = FxHashMap::default();

        let entry = region.entry_node(arena);

        let mut worklist: Vec<N> = vec![entry];
        let mut visited = FxHashSet::default();

        while let Some(node) = worklist.pop() {
            if visited.contains(&node) {
                continue;
            }

            visited.insert(node);

            // for all visited nodes, we need to create entries in the
            // predecessors and successors maps, indicating that they are
            // reachable.
            succs.entry(node).or_default();
            preds.entry(node).or_default();

            for succ in node.succs(arena) {
                // we should not add duplicate nodes to succs/preds

                // TODO: is it valid to `br` to the same block with different arguments?
                // MLIR supports `cond_br` to the same block, but not sure about the control
                // flow semantics.

                // TODO: temporary solution, just iterate with `contains`
                if !succs.entry(node).or_default().contains(&succ) {
                    succs.entry(node).or_default().push(succ);
                }
                if !preds.entry(succ).or_default().contains(&node) {
                    preds.entry(succ).or_default().push(node);
                }

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
    ///
    /// The order of successors is not guaranteed.
    ///
    /// # Returns
    ///
    /// - `Some(succs)`: The successors of the node.
    /// - `None`: The node is not reachable.
    pub fn succs(&self, node: N) -> Option<&[N]> { self.succs.get(&node).map(|v| v.as_slice()) }

    /// Get the predecessors of a node.
    ///
    /// The order of predecessors is not guaranteed.
    ///
    /// # Returns
    ///
    /// - `Some(preds)`: The predecessors of the node.
    /// - `None`: The node is not reachable.
    pub fn preds(&self, node: N) -> Option<&[N]> { self.preds.get(&node).map(|v| v.as_slice()) }

    /// Get the region associated with the control flow graph.
    pub fn region(&self) -> R { self.region }

    /// Get the reachable nodes in the control flow graph.
    pub fn reachable_nodes(&self, arena: &N::A) -> FxHashSet<N> {
        let mut reachables = FxHashSet::default();
        let entry = self.region.entry_node(arena);

        let mut worklist = vec![entry];
        while let Some(node) = worklist.pop() {
            if !reachables.insert(node) {
                continue;
            }

            if let Some(succs) = self.succs(node) {
                worklist.extend_from_slice(succs);
            }
        }

        reachables
    }
}
