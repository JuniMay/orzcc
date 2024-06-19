use std::collections::{HashMap, HashSet};

use super::cfg::CfgNode;

#[derive(Default)]
pub struct Dominance<N>
where
    N: CfgNode,
{
    /// The immediate dominator of each node.
    idom: HashMap<N, N>,
    /// The dominance frontier of each node.
    frontier: HashMap<N, HashSet<N>>,
    /// The dominance tree of each node.
    domtree: HashMap<N, HashSet<N>>,
}
