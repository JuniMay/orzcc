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
    idom: HashMap<N, Option<N>>,
    /// The dominance frontier of each node.
    frontier: HashMap<N, Vec<N>>,
    /// The dominance tree of each node.
    domtree: HashMap<N, Vec<N>>,
}

impl<N> Default for Dominance<N>
where
    N: CfgNode,
{
    fn default() -> Self {
        Self {
            idom: HashMap::new(),
            frontier: HashMap::new(),
            domtree: HashMap::new(),
        }
    }
}

impl<N> Dominance<N>
where
    N: CfgNode + Hash,
{
    pub fn idom(&self, node: N) -> Option<N> { self.idom[&node] }

    pub fn frontier(&self, node: N) -> &[N] { &self.frontier[&node] }

    pub fn domtree(&self, node: N) -> &[N] { &self.domtree[&node] }

    fn intersect(&self, n1: N, n2: N, postorder: &HashMap<N, usize>) -> N {
        let mut finger1 = n1;
        let mut finger2 = n2;
        while finger1 != finger2 {
            while postorder[&finger1] < postorder[&finger2] {
                finger1 = self.idom[&finger1].unwrap();
            }
            while postorder[&finger2] < postorder[&finger1] {
                finger2 = self.idom[&finger2].unwrap();
            }
        }
        finger1
    }

    pub fn compute(&mut self, arena: &N::A, region: N::Region, cfg: &CfgInfo<N, N::Region>) {
        let mut dfs: DfsContext<N> = DfsContext::default();

        let mut postorder = HashMap::new();
        let mut rpo = Vec::new();
        self.idom.clear();

        for (i, node) in dfs.post_order_iter(arena, region).enumerate() {
            postorder.insert(node, i);
            rpo.push(node);
            self.idom.insert(node, None);
        }

        rpo.reverse();

        // idom of entry is entry
        assert!(rpo[0] == region.entry_node(arena));
        self.idom.insert(rpo[0], Some(rpo[0]));

        let mut changed = true;
        while changed {
            changed = false;
            for node in rpo.iter().skip(1) {
                let mut new_idom = None;
                for pred in cfg
                    .preds(*node)
                    .unwrap()
                    .iter()
                    .filter(|pred| *pred != node)
                {
                    if self.idom[pred].is_some() {
                        new_idom = Some(*pred);
                        break;
                    }
                }

                if new_idom.is_none() {
                    continue;
                }

                for pred in cfg.preds(*node).unwrap() {
                    if self.idom[pred].is_some() {
                        new_idom = Some(self.intersect(new_idom.unwrap(), *pred, &postorder));
                    }
                }
                if self.idom[node] != new_idom {
                    self.idom.insert(*node, new_idom);
                    changed = true;
                }
            }
        }

        self.idom.insert(rpo[0], None);

        self.domtree.clear();
        for node in rpo.iter() {
            self.domtree.insert(*node, Vec::new());
        }

        for (node, idom) in self.idom.iter() {
            if let Some(idom) = idom {
                self.domtree.get_mut(idom).unwrap().push(*node);
            }
        }

        self.frontier.clear();
        for node in rpo.iter() {
            self.frontier.insert(*node, Vec::new());
        }

        for node in rpo.iter() {
            if cfg.preds(*node).unwrap().len() >= 2 {
                for pred in cfg.preds(*node).unwrap() {
                    let mut runner = *pred;
                    while runner != self.idom[node].unwrap() {
                        self.frontier.get_mut(&runner).unwrap().push(*node);
                        runner = self.idom[&runner].unwrap();
                    }
                }
            }
        }
    }
}
