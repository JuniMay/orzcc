//! # Control Flow Analysis
//!
//! This module contains the implementation of the control flow analysis (CFA) pass.

use std::collections::HashMap;

use crate::ir::{
    entities::{FunctionData, ValueKind},
    pass::LocalPass,
    values::{Block, Function},
};

pub struct CfaContext {
    pred: HashMap<Block, Vec<Block>>,
    succ: HashMap<Block, Vec<Block>>,
}

impl CfaContext {
    pub fn new() -> Self {
        Self {
            pred: HashMap::new(),
            succ: HashMap::new(),
        }
    }
}

pub struct Cfa {}

impl LocalPass for Cfa {
    type Ok = CfaContext;
    type Err = String;

    fn run(&mut self, function: Function, data: &mut FunctionData) -> Result<Self::Ok, Self::Err> {
        let mut context = CfaContext::new();

        let layout = data.layout();
        for (block, node) in layout.blocks() {
            let mut succ = Vec::new();
            for (inst, node) in node.insts() {
                let inst_data = data.dfg().local_value_data(inst.into()).unwrap();
                match inst_data.kind() {
                    ValueKind::Jump(jump) => {
                        succ.push(jump.dst());
                        if context.pred.contains_key(&jump.dst()) {
                            context.pred.get_mut(&jump.dst()).unwrap().push(block);
                        } else {
                            context.pred.insert(jump.dst(), vec![block]);
                        }
                    }
                    ValueKind::Branch(br) => {
                        succ.push(br.then_dst());
                        succ.push(br.else_dst());
                        if context.pred.contains_key(&br.then_dst()) {
                            context.pred.get_mut(&br.then_dst()).unwrap().push(block);
                        } else {
                            context.pred.insert(br.then_dst(), vec![block]);
                        }
                        if context.pred.contains_key(&br.else_dst()) {
                            context.pred.get_mut(&br.else_dst()).unwrap().push(block);
                        } else {
                            context.pred.insert(br.else_dst(), vec![block]);
                        }
                    }
                    _ => {}
                }
            }
        }

        Ok(context)
    }
}
