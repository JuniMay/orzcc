//! # Dominance Analysis
//!
//! This module contains the implementation of the dominance analysis pass.
//!
//! This implements the algorithm described in "A Simple, Fast Dominance Algorithm" by Cooper et al.
//!

use std::collections::HashMap;

use thiserror::Error;

use crate::{ir::{
    entities::FunctionData,
    pass::LocalPass,
    values::{Block, Function},
}, passes::control_flow_analysis::ControlFlowAnalysis};

use super::control_flow_analysis::{ControlFlowAnalysisError, ControlFlowGraph};

/// The result of the dominance analysis pass.
pub struct Dominance {
    /// The immediate dominator of each block.
    pub idoms: HashMap<Block, Option<Block>>,
    /// The dominance frontier of each block.
    pub frontiers: HashMap<Block, Vec<Block>>,
    /// The dominator tree.
    pub domtree: HashMap<Block, Vec<Block>>,
}

#[derive(Debug, Error)]
pub enum DominanceAnalysisError {
    #[error(transparent)]
    ControlFlowAnalysisError(#[from] ControlFlowAnalysisError),
}

pub struct DominanceAnalysis {
    /// Postorder traversal numbers of the blocks.
    postorder: HashMap<Block, usize>,

    /// Reverse postorder traversal sequence.
    rpo: Vec<Block>,
}

impl DominanceAnalysis {
    pub fn new() -> Self {
        Self {
            postorder: HashMap::new(),
            rpo: Vec::new(),
        }
    }

    // fn dfs(&mut self, block: Block, cfg: &ControlFlowGraph)
}

impl LocalPass for DominanceAnalysis {
    type Ok = Dominance;
    type Err = DominanceAnalysisError;

    fn run(&mut self, function: Function, data: &FunctionData) -> Result<Self::Ok, Self::Err> {
        let mut cfa = ControlFlowAnalysis {};
        let cfg = cfa.run(function, data)?;

        todo!()
    }
}
