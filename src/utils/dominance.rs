//! # Dominance Analysis
//!
//! This module implements the algorithm described in "A Simple, Fast Dominance
//! Algorithm" by Cooper, et al.

use std::{collections::HashMap, hash::Hash};

use super::{
    cfg::{CfgInfo, CfgNode},
    dfs::DfsContext,
};
use crate::utils::cfg::CfgRegion;

pub struct Dominance<N>
where
    N: CfgNode,
{
    /// The immediate dominator of each node.
    idoms: HashMap<N, Option<N>>,
    /// The dominance frontier of each node.
    frontiers: HashMap<N, Vec<N>>,
    /// The dominance tree of each node.
    domtree: HashMap<N, Vec<N>>,
}

impl<N> Default for Dominance<N>
where
    N: CfgNode,
{
    fn default() -> Self {
        Self {
            idoms: HashMap::new(),
            frontiers: HashMap::new(),
            domtree: HashMap::new(),
        }
    }
}

impl<N> Dominance<N>
where
    N: CfgNode + Hash,
{
    /// Returns the immediate dominator of `node`.
    pub fn idom(&self, node: N) -> Option<N> { self.idoms[&node] }

    /// Returns the dominance frontier of `node`.
    pub fn frontier(&self, node: N) -> &[N] { &self.frontiers[&node] }

    /// Returns the dominated nodes of `node`.
    pub fn dominated(&self, node: N) -> &[N] { &self.domtree[&node] }

    fn intersect(n1: N, n2: N, idoms: &HashMap<N, Option<N>>, postorder: &HashMap<N, usize>) -> N {
        let mut finger1 = n1;
        let mut finger2 = n2;
        while finger1 != finger2 {
            while postorder[&finger1] < postorder[&finger2] {
                finger1 = idoms[&finger1].unwrap();
            }
            while postorder[&finger2] < postorder[&finger1] {
                finger2 = idoms[&finger2].unwrap();
            }
        }
        finger1
    }

    /// Constructs a new `Dominance` instance.
    ///
    /// # Parameters
    ///
    /// - `arena`: The arena of the control flow graph nodes and region.
    /// - `region`: The region of the control flow graph.
    /// - `cfg`: The control flow graph information.
    ///
    /// # Returns
    ///
    /// A new [Dominance] instance.
    pub fn new(arena: &N::A, region: N::Region, cfg: &CfgInfo<N, N::Region>) -> Self {
        let mut idoms = HashMap::new();
        let mut frontiers = HashMap::new();
        let mut domtree = HashMap::new();

        let mut dfs: DfsContext<N> = DfsContext::default();

        let mut postorder = HashMap::new();
        let mut rpo = Vec::new();

        for (i, node) in dfs.post_order_iter(arena, region).enumerate() {
            postorder.insert(node, i);
            rpo.push(node);
            // for all nodes, doms <- Undefined, see Figure 3 in the paper.
            idoms.insert(node, None);
        }
        rpo.reverse();

        // idom of entry is entry
        assert!(rpo[0] == region.entry_node(arena));
        // doms[start_node] <- start_node
        idoms.insert(rpo[0], Some(rpo[0]));

        let mut changed = true;
        while changed {
            changed = false;
            // for all nodes, in reverse postorder (except start_node)
            for node in rpo.iter().skip(1) {
                let mut new_idom = None;
                for pred in cfg.preds(*node).unwrap() {
                    if idoms[pred].is_some() {
                        new_idom = Some(*pred);
                        break; // pick the first processed predecessor of node
                    }
                }

                // because we process in rpo, so there must be a processed pred
                assert!(new_idom.is_some());

                for pred in cfg.preds(*node).unwrap() {
                    // for all other predecessors of node
                    if idoms[pred].is_some() {
                        // if doms[pred] is calculated
                        new_idom = Some(Self::intersect(
                            new_idom.unwrap(),
                            *pred,
                            &idoms,
                            &postorder,
                        ));
                    }
                }

                if idoms[node] != new_idom {
                    idoms.insert(*node, new_idom);
                    changed = true;
                }
            }
        }
        idoms.insert(rpo[0], None);

        for node in rpo.iter() {
            domtree.insert(*node, Vec::new());
        }
        for (node, idom) in idoms.iter() {
            if let Some(idom) = idom {
                domtree.get_mut(idom).unwrap().push(*node);
            }
        }

        for node in rpo.iter() {
            frontiers.insert(*node, Vec::new());
        }
        for node in rpo.iter() {
            if cfg.preds(*node).unwrap().len() >= 2 {
                for pred in cfg.preds(*node).unwrap() {
                    let mut runner = *pred;
                    while runner != idoms[node].unwrap() {
                        frontiers.get_mut(&runner).unwrap().push(*node);
                        runner = idoms[&runner].unwrap();
                    }
                }
            }
        }

        Self {
            idoms,
            frontiers,
            domtree,
        }
    }
}
