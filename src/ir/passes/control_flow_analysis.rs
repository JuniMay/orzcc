//! # Control Flow Analysis
//!
//! This module contains the implementation of the control flow analysis (CFA)
//! pass.

use std::collections::HashMap;

use thiserror::Error;

use super::{PassError, PassResult};
use crate::ir::{
    entities::{FunctionData, ValueKind},
    passes::LocalPass,
    values::{Block, Function},
};

#[derive(Debug, Clone, Default)]
pub struct ControlFlowGraph {
    preds: HashMap<Block, Vec<Block>>,
    succs: HashMap<Block, Vec<Block>>,
}

impl ControlFlowGraph {
    pub fn new() -> Self { Self::default() }

    pub fn succs(&self, block: &Block) -> Option<&Vec<Block>> { self.succs.get(block) }

    pub fn preds(&self, block: &Block) -> Option<&Vec<Block>> { self.preds.get(block) }
}

pub struct ControlFlowAnalysis {}

#[derive(Debug, Error)]
pub enum ControlFlowAnalysisError {
    #[error(
    "unexpected termination at block: {0:?}, please run the control flow normalization pass first."
  )]
    UnexpectedBlockTermination(Block),
}

impl From<ControlFlowAnalysisError> for PassError {
    fn from(err: ControlFlowAnalysisError) -> Self {
        PassError::analysis_error("control-flow-analysis".to_string(), Box::new(err))
    }
}

impl LocalPass for ControlFlowAnalysis {
    type Ok = ControlFlowGraph;

    fn run_on_function(
        &mut self,
        _function: Function,
        data: &FunctionData,
    ) -> PassResult<Self::Ok> {
        let mut cfg = ControlFlowGraph::new();

        let layout = data.layout();
        for (block, _block_node) in layout.blocks() {
            cfg.preds.insert(block, Vec::new());
        }
        for (block, _block_node) in layout.blocks() {
            let mut succ = Vec::new();
            // just get the last instruction of the block
            // because the normalization pass ensures that the last instruction is a
            // terminator
            let inst = layout.exit_inst_of_block(block).unwrap();
            let inst_data = data.dfg().local_value_data(inst.into()).unwrap();
            match inst_data.kind() {
                ValueKind::Jump(jump) => {
                    succ.push(jump.dst());
                    cfg.preds.get_mut(&jump.dst()).unwrap().push(block);
                }
                ValueKind::Branch(br) => {
                    succ.push(br.then_dst());
                    succ.push(br.else_dst());
                    cfg.preds.get_mut(&br.then_dst()).unwrap().push(block);
                    cfg.preds.get_mut(&br.else_dst()).unwrap().push(block);
                }
                ValueKind::Return(_) => {}
                _ => {
                    return Err(ControlFlowAnalysisError::UnexpectedBlockTermination(block).into());
                }
            }
            cfg.succs.insert(block, succ);
        }

        Ok(cfg)
    }
}

#[cfg(test)]
mod test {
    use std::io::Cursor;

    use super::ControlFlowAnalysis;
    use crate::ir::{frontend::parser::Parser, passes::LocalPass};

    #[test]
    fn test_cfa() {
        let ir = r#"
            func @check_positive(i32) -> i32 {
                ^entry(i32 %0):
                    %cmp = icmp.sle i32 %0, i32 0x0
                    %cond = not i1 %cmp
                    br i1 %cond, ^positive, ^negative(i32 0xffffffff)
                
                ^positive:
                    jump ^negative(i32 1)
                
                ^negative(i32 %1):
                    ret i32 %1
            }"#;

        let mut buf = Cursor::new(ir);
        let mut parser = Parser::new(&mut buf);
        let module = parser.parse().unwrap().into_ir("test".into()).unwrap();

        let mut cfa = ControlFlowAnalysis {};

        let function = module.get_value_by_name("@check_positive").unwrap();
        let function_data = module.function_data(function.into()).unwrap();

        let cfg = cfa.run_on_function(function.into(), function_data).unwrap();

        let entry = function_data.dfg().get_block_by_name("^entry").unwrap();
        let positive = function_data.dfg().get_block_by_name("^positive").unwrap();
        let negative = function_data.dfg().get_block_by_name("^negative").unwrap();

        assert_eq!(cfg.succs.get(&entry).unwrap(), &vec![positive, negative]);
        assert_eq!(cfg.succs.get(&positive).unwrap(), &vec![negative]);
        assert_eq!(cfg.succs.get(&negative).unwrap(), &vec![]);
        assert_eq!(cfg.preds.get(&positive).unwrap(), &vec![entry]);
        assert_eq!(cfg.preds.get(&negative).unwrap(), &vec![entry, positive]);
    }
}
