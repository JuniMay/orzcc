//! # Control Flow Analysis
//!
//! This module contains the implementation of the control flow analysis (CFA) pass.
//!

use std::collections::HashMap;

use super::control_flow_normalization::ControlFlowNormalization;
use crate::ir::{
    entities::{FunctionData, ValueKind},
    pass::LocalPassMut,
    values::{Block, Function},
};

pub struct ControlFlowGraph {
    pred: HashMap<Block, Vec<Block>>,
    succ: HashMap<Block, Vec<Block>>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        Self {
            pred: HashMap::new(),
            succ: HashMap::new(),
        }
    }
}

pub struct ControlFlowAnalysis {}

impl LocalPassMut for ControlFlowAnalysis {
    type Ok = ControlFlowGraph;
    type Err = String;

    fn run(&mut self, _function: Function, data: &mut FunctionData) -> Result<Self::Ok, Self::Err> {
        // before control-flow analysis, normalize the control flow
        let mut control_flow_normalization = ControlFlowNormalization {};
        control_flow_normalization.run(_function, data).unwrap();

        let mut cfg = ControlFlowGraph::new();

        let layout = data.layout();
        for (block, _block_node) in layout.blocks() {
            let mut succ = Vec::new();
            // just get the last instruction of the block
            // because the normalization pass ensures that the last instruction is a terminator
            let inst = layout.exit_inst_of_block(block).unwrap();
            let inst_data = data.dfg().local_value_data(inst.into()).unwrap();
            match inst_data.kind() {
                ValueKind::Jump(jump) => {
                    succ.push(jump.dst());
                    if cfg.pred.contains_key(&jump.dst()) {
                        cfg.pred.get_mut(&jump.dst()).unwrap().push(block);
                    } else {
                        cfg.pred.insert(jump.dst(), vec![block]);
                    }
                }
                ValueKind::Branch(br) => {
                    succ.push(br.then_dst());
                    succ.push(br.else_dst());
                    if cfg.pred.contains_key(&br.then_dst()) {
                        cfg.pred.get_mut(&br.then_dst()).unwrap().push(block);
                    } else {
                        cfg.pred.insert(br.then_dst(), vec![block]);
                    }
                    if cfg.pred.contains_key(&br.else_dst()) {
                        cfg.pred.get_mut(&br.else_dst()).unwrap().push(block);
                    } else {
                        cfg.pred.insert(br.else_dst(), vec![block]);
                    }
                }
                _ => {}
            }
            cfg.succ.insert(block, succ);
        }

        Ok(cfg)
    }
}

#[cfg(test)]
mod test {
    use std::io::{self, Cursor};

    use crate::{
        ir::{
            frontend::parser::Parser,
            module::Module,
            pass::{GlobalPass, LocalPassMut},
        },
        passes::printer::Printer,
    };

    use super::ControlFlowAnalysis;

    fn _print(module: &Module) {
        let mut stdout = io::stdout();
        let mut printer = Printer::new(&mut stdout);
        printer.run(module).unwrap();
    }

    #[test]
    fn test_cfa() {
        let ir = r#"
            func @check_positive(i32) -> i32 {
                ^entry(i32 %0):
                    %cmp = icmp.sle i32 %0, i32 0x0
                    %cond = not i1 %cmp
                    br i1 %cond, ^positive, ^negative
                
                ^positive:
                    ret i32 0x1
                
                ^negative:
                    ret i32 0xFFFFFFFF
            }"#;

        let mut buf = Cursor::new(ir);
        let mut parser = Parser::new(&mut buf);
        let mut module = parser.parse().unwrap().into_ir("test".into()).unwrap();

        let mut cfa = ControlFlowAnalysis {};

        let function = module.get_value_by_name("@check_positive").unwrap();
        let function_data = module.function_data_mut(function.into()).unwrap();

        let cfg = cfa.run(function.into(), function_data).unwrap();

        let entry = function_data.dfg().get_block_by_name("^entry").unwrap();
        let positive = function_data.dfg().get_block_by_name("^positive").unwrap();
        let negative = function_data.dfg().get_block_by_name("^negative").unwrap();

        assert_eq!(cfg.succ.get(&entry).unwrap(), &vec![positive, negative]);
        assert_eq!(cfg.pred.get(&positive).unwrap(), &vec![entry]);
        assert_eq!(cfg.pred.get(&negative).unwrap(), &vec![entry]);
    }
}
