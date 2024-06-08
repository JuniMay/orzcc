//! # Data-Flow Analysis Pass for OrzIR
//!
//! There is already a data-flow graph (DFG) in OrzIR, however, the def-use
//! chains are not maintained. This pass will build the def-use chains.
//!
//! Note that in OrzIR, the def of a instruction is explicit (for SSA form), but
//! the use of a instruction is not explicit.

use std::collections::HashMap;

use thiserror::Error;

use super::PassResult;
use crate::ir::{
    entities::{FunctionData, ValueKind},
    module::DataFlowGraph,
    passes::LocalPass,
    values::{Function, Value},
};

#[derive(Debug, Error)]
pub enum DataFlowAnalysisError {}

pub struct DefUseChain {
    pub uses: HashMap<Value, Vec<Value>>,
}

impl Default for DefUseChain {
    fn default() -> Self { Self::new() }
}

impl DefUseChain {
    pub fn new() -> Self {
        Self {
            uses: HashMap::new(),
        }
    }

    pub fn insert_use(&mut self, use_: Value, def: Value) {
        self.uses.entry(def).or_insert_with(Vec::new).push(use_);
    }
}

pub struct DataFlowAnalysis {}

impl DataFlowAnalysis {
    fn insert_use(
        &mut self,
        use_: Value,
        def: Value,
        chain: &mut DefUseChain,
        dfg: &DataFlowGraph,
    ) {
        if dfg.local_value_data(def).is_none() {
            // the defined value is global
            return;
        }
        chain.uses.get_mut(&def).unwrap().push(use_);
    }
}

impl LocalPass for DataFlowAnalysis {
    type Ok = DefUseChain;

    fn run_on_function(
        &mut self,
        _function: Function,
        data: &FunctionData,
    ) -> PassResult<Self::Ok> {
        let dfg = &data.dfg;
        let mut chain = DefUseChain::new();
        for value in dfg.values().keys() {
            chain.uses.insert(*value, Vec::new());
        }

        for (value, data) in dfg.values() {
            match data.kind() {
                ValueKind::Alloc(_alloc) => {}
                ValueKind::Load(load) => self.insert_use(*value, load.ptr(), &mut chain, dfg),
                ValueKind::Store(store) => {
                    self.insert_use(*value, store.ptr(), &mut chain, dfg);
                    self.insert_use(*value, store.val(), &mut chain, dfg);
                }
                ValueKind::Binary(binary) => {
                    self.insert_use(*value, binary.lhs(), &mut chain, dfg);
                    self.insert_use(*value, binary.rhs(), &mut chain, dfg);
                }
                ValueKind::Unary(unary) => self.insert_use(*value, unary.val(), &mut chain, dfg),
                ValueKind::Jump(jump) => {
                    for arg in jump.args() {
                        self.insert_use(*value, *arg, &mut chain, dfg);
                    }
                }
                ValueKind::Branch(br) => {
                    self.insert_use(*value, br.cond(), &mut chain, dfg);
                    for arg in br.then_args() {
                        self.insert_use(*value, *arg, &mut chain, dfg);
                    }
                    for arg in br.else_args() {
                        self.insert_use(*value, *arg, &mut chain, dfg);
                    }
                }
                ValueKind::Return(ret) => {
                    if let Some(val) = ret.val() {
                        self.insert_use(*value, val, &mut chain, dfg);
                    }
                }
                ValueKind::Call(call) => {
                    for arg in call.args() {
                        self.insert_use(*value, *arg, &mut chain, dfg);
                    }
                }
                ValueKind::GetElemPtr(gep) => {
                    self.insert_use(*value, gep.ptr(), &mut chain, dfg);
                    for idx in gep.indices() {
                        self.insert_use(*value, *idx, &mut chain, dfg);
                    }
                }
                ValueKind::Cast(cast) => {
                    self.insert_use(*value, cast.val(), &mut chain, dfg);
                }
                _ => {}
            }
        }

        Ok(chain)
    }
}
