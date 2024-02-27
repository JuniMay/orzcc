//! # Data-Flow Analysis Pass for OrzIR
//!
//! There is already a data-flow graph (DFG) in OrzIR, however, the def-use chains are not
//! maintained. This pass will build the def-use chains.
//!
//! Note that in OrzIR, the def of a instruction is explicit (for SSA form), but the use of a
//! instruction is not explicit. 

use std::collections::HashSet;

use crate::ir::{entities::FunctionData, pass::LocalPass, values::{Function, Value}};

pub enum DataFlowAnalysisError {
    
}

pub struct DefUseChain {
    pub def: Value,
    pub uses: HashSet<Value>,
}

pub struct DataFlowAnalysis {
    
}

impl LocalPass for DataFlowAnalysis {
    type Ok = DefUseChain;
    type Err = DataFlowAnalysisError;

    fn run(&mut self, function: Function, data: &FunctionData) -> Result<Self::Ok, Self::Err> {
        todo!()
    }
}