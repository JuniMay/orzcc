//! # Dominance Analysis
//!
//! This module contains the implementation of the dominance analysis pass.

use std::collections::HashMap;

use thiserror::Error;

use crate::{ir::{
    entities::FunctionData,
    pass::LocalPass,
    values::{Block, Function},
}, passes::control_flow_analysis::ControlFlowAnalysis};

use super::control_flow_analysis::ControlFlowAnalysisError;

pub struct Dominance {
    pub idoms: HashMap<Block, Option<Block>>,
    pub frontiers: HashMap<Block, Vec<Block>>,
    pub domtree: HashMap<Block, Vec<Block>>,
}

#[derive(Debug, Error)]
pub enum DominanceAnalysisError {
    #[error(transparent)]
    ControlFlowAnalysisError(#[from] ControlFlowAnalysisError),
}

pub struct DominanceAnalysis {}

impl DominanceAnalysis {}

impl LocalPass for DominanceAnalysis {
    type Ok = Dominance;
    type Err = DominanceAnalysisError;

    fn run(&mut self, function: Function, data: &FunctionData) -> Result<Self::Ok, Self::Err> {
        let mut cfa = ControlFlowAnalysis {};
        let cfg = cfa.run(function, data)?;

        todo!()
    }
}
