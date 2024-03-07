//! # Data-Flow Analysis Pass for OrzIR
//!
//! There is already a data-flow graph (DFG) in OrzIR, however, the def-use chains are not
//! maintained. This pass will build the def-use chains.
//!
//! Note that in OrzIR, the def of a instruction is explicit (for SSA form), but the use of a
//! instruction is not explicit.

use std::collections::HashMap;

use thiserror::Error;

use crate::ir::{
    entities::{FunctionData, ValueKind},
    passes::LocalPass,
    values::{Function, Value},
};

use super::PassResult;

#[derive(Debug, Error)]
pub enum DataFlowAnalysisError {}

pub struct DefUseChain {
    pub uses: HashMap<Value, Vec<Value>>,
}

impl Default for DefUseChain {
    fn default() -> Self {
        Self::new()
    }
}

impl DefUseChain {
    pub fn new() -> Self {
        Self {
            uses: HashMap::new(),
        }
    }
}

pub struct DataFlowAnalysis {}

impl DataFlowAnalysis {
    fn insert_use(&mut self, use_: Value, def: Value, chain: &mut DefUseChain) {
        chain.uses.get_mut(&def).unwrap().push(use_);
    }
}

impl LocalPass for DataFlowAnalysis {
    type Ok = DefUseChain;

    fn run(&mut self, _function: Function, data: &FunctionData) -> PassResult<Self::Ok> {
        let dfg = data.dfg();
        let mut chain = DefUseChain::new();
        for value in dfg.values().keys() {
            chain.uses.insert(*value, Vec::new());
        }

        for (value, data) in dfg.values() {
            match data.kind() {
                ValueKind::Alloc(_alloc) => {}
                ValueKind::Load(load) => self.insert_use(*value, load.ptr(), &mut chain),
                ValueKind::Store(store) => {
                    self.insert_use(*value, store.ptr(), &mut chain);
                    self.insert_use(*value, store.val(), &mut chain);
                }
                ValueKind::Binary(binary) => {
                    self.insert_use(*value, binary.lhs(), &mut chain);
                    self.insert_use(*value, binary.rhs(), &mut chain);
                }
                ValueKind::Unary(unary) => self.insert_use(*value, unary.val(), &mut chain),
                ValueKind::Jump(jump) => {
                    for arg in jump.args() {
                        self.insert_use(*value, *arg, &mut chain);
                    }
                }
                ValueKind::Branch(br) => {
                    self.insert_use(*value, br.cond(), &mut chain);
                    for arg in br.then_args() {
                        self.insert_use(*value, *arg, &mut chain);
                    }
                    for arg in br.else_args() {
                        self.insert_use(*value, *arg, &mut chain);
                    }
                }
                ValueKind::Return(ret) => {
                    if let Some(val) = ret.val() {
                        self.insert_use(*value, val, &mut chain);
                    }
                }
                ValueKind::Call(call) => {
                    for arg in call.args() {
                        self.insert_use(*value, *arg, &mut chain);
                    }
                }
                ValueKind::GetElemPtr(gep) => {
                    self.insert_use(*value, gep.ptr(), &mut chain);
                    for idx in gep.indices() {
                        self.insert_use(*value, *idx, &mut chain);
                    }
                }
                ValueKind::Cast(cast) => {
                    self.insert_use(*value, cast.val(), &mut chain);
                }
                _ => {}
            }
        }

        Ok(chain)
    }
}
