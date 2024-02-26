//! # Unreachable BB Elim
//!
//! This module contains the implementation of the Unreachable BB Elim pass.


use crate::ir::{
    entities::FunctionData,
    pass::LocalPassMut,
    values::{Block, Function},
};
use crate::passes::control_flow_analysis::ControlFlowGraph;

use std::collections::HashSet;
use std::collections::VecDeque;

pub struct UnreachableBlockElimination {}

impl LocalPassMut for UnreachableBlockElimination {
    type Ok = ();
    type Err = String;

    fn run(&mut self, _function: Function, data: &mut FunctionData) -> Result<Self::Ok, Self::Err> {

        let mut cfg = ControlFlowGraph::new();
        
        let mut reachable_blocks: HashSet<Block> = HashSet::new();
        let mut queue: VecDeque<Block> = VecDeque::new();


        if let Some(entry) = data.layout().entry_block() {
            queue.push_back(entry);
            reachable_blocks.insert(entry);
        } else {
            return Err("Function does not have an entry block".to_string());
        }
        while let Some(block) = queue.pop_front() {
            if let Some(succs) = cfg.get_succ(&block) {
                for &succ in succs.iter() {
                    if reachable_blocks.insert(succ) {
                        queue.push_back(succ);
                    }
                }
            }
        }


        let mut all_blocks: HashSet<Block> = HashSet::new();
        for (block, _block_node) in data.layout().blocks() {
            all_blocks.insert(block);
        }
        let unreachable_blocks: HashSet<Block> = all_blocks.difference(&reachable_blocks).cloned().collect();

        for block in unreachable_blocks {
            if let Err(e) = data.layout_mut().remove_block(block) {
                return Err(format!("Failed to remove block {:?}: {:?}", block, e));
            }
        }

        Ok(())
    }
}
