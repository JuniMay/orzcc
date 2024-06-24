//! # Depth-First Search on Control Flow Graph
//!
//! This idea is from Cranelift, which implements a low-level DFS interface and
//! iterator as reusable components.

use std::{collections::HashSet, hash::Hash};

use super::cfg::{CfgNode, CfgRegion};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Event {
    Enter,
    Leave,
}

pub struct DfsContext<N>
where
    N: CfgNode,
{
    stack: Vec<(Event, N)>,
    visited: HashSet<N>,
}

impl<N> Default for DfsContext<N>
where
    N: CfgNode,
{
    fn default() -> Self {
        Self {
            stack: Vec::new(),
            visited: HashSet::new(),
        }
    }
}

impl<N> DfsContext<N>
where
    N: CfgNode + Hash,
{
    pub fn iter<'a>(&'a mut self, arena: &'a N::A, region: N::Region) -> DfsIterator<'a, N> {
        self.stack.clear();
        self.visited.clear();
        let entry = region.entry_node(arena);
        self.stack.push((Event::Enter, entry));
        DfsIterator { arena, dfs: self }
    }

    pub fn pre_order_iter<'a>(
        &'a mut self,
        arena: &'a N::A,
        region: N::Region,
    ) -> DfsPreOrderIterator<'a, N> {
        DfsPreOrderIterator(self.iter(arena, region))
    }

    pub fn post_order_iter<'a>(
        &'a mut self,
        arena: &'a N::A,
        region: N::Region,
    ) -> DfsPostOrderIterator<'a, N> {
        DfsPostOrderIterator(self.iter(arena, region))
    }
}

pub struct DfsIterator<'a, N>
where
    N: CfgNode,
{
    arena: &'a N::A,
    dfs: &'a mut DfsContext<N>,
}

impl<'a, N> Iterator for DfsIterator<'a, N>
where
    N: CfgNode + Hash,
{
    type Item = (Event, N);

    fn next(&mut self) -> Option<Self::Item> {
        let mut event_node = None;

        while let Some((event, node)) = self.dfs.stack.pop() {
            if event == Event::Enter && self.dfs.visited.contains(&node) {
                continue;
            }
            event_node = Some((event, node));
            break;
        }

        let (event, node) = event_node?;

        if event == Event::Enter && self.dfs.visited.insert(node) {
            self.dfs.stack.push((Event::Leave, node));

            // TODO: this can be optimized if a `CfgInfo` is provided.
            let succs = node.succs(self.arena);

            self.dfs.stack.extend(
                succs
                    .into_iter()
                    // heuristic from cranelift. because this is a stack, so
                    // reversing the list will prioritize the first succ of the
                    // block.
                    //
                    // it is said in cranelift that this tends to prioritize
                    // loop backedges over out-edges, maybe further investigation
                    // is needed.
                    .rev()
                    .filter(|succ| !self.dfs.visited.contains(succ))
                    .map(|succ| (Event::Enter, succ)),
            );
        }

        Some((event, node))
    }
}

pub struct DfsPreOrderIterator<'a, N>(DfsIterator<'a, N>)
where
    N: CfgNode;

impl<'a, N> Iterator for DfsPreOrderIterator<'a, N>
where
    N: CfgNode + Hash,
{
    type Item = N;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.0.next()? {
                (Event::Enter, node) => return Some(node),
                (Event::Leave, _) => continue,
            }
        }
    }
}

pub struct DfsPostOrderIterator<'a, N>(DfsIterator<'a, N>)
where
    N: CfgNode;

impl<'a, N> Iterator for DfsPostOrderIterator<'a, N>
where
    N: CfgNode + Hash,
{
    type Item = N;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.0.next()? {
                (Event::Leave, node) => return Some(node),
                (Event::Enter, _) => continue,
            }
        }
    }
}
