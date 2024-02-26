//! # Unreachable BB Elim
//!
//! This module contains the implementation of the Unreachable BB Elim pass.


use crate::ir::{
    entities::{FunctionData, ValueKind},
    pass::LocalPass,
    values::{Block, Function},
};

use std::collections::HashSet;

pub struct UnreachableBlockElimination {}

impl LocalPass for UnreachableBlockElimination {
    type Ok = ();
    type Err = String;

    fn run(&mut self, _function: Function, data: &mut FunctionData) -> Result<Self::Ok, Self::Err> {
        let mut reachable_blocks: HashSet<Block> = HashSet::new();
        let mut queue: VecDeque<Block> = VecDeque::new();

        if let Some(entry) = data.layout.entry_block() {
            queue.push_back(entry);
            reachable_blocks.insert(entry);
        } else {
            return Err("Function does not have an entry block".to_string());
        }

        while let Some(block) = queue.pop_front() {
            if let Some(node) = data.layout.blocks().node(block) {
                for &succ in node.successors().iter() {
                    if reachable_blocks.insert(succ) {
                        queue.push_back(succ);
                    }
                }
            }
        }

        let blocks_to_remove: Vec<Block> = data.layout.blocks()
            .iter()
            .filter(|&block| !reachable_blocks.contains(block))
            .cloned()
            .collect();

        for block in blocks_to_remove.iter() {
            data.layout.remove_block(*block).map_err(|err| format!("{:?}", err))?;
        }

        Ok(())
    }
}
