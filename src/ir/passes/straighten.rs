//! # Straighten(IR)
//!
//! This module contains the implementation of the straighten pass.

use std::collections::HashMap;

use thiserror::Error;

use super::{
    control_flow_analysis::ControlFlowAnalysis,
    control_flow_canonicalization::ControlFlowCanonicalization,
    GlobalPassMut,
    PassManager,
    PassResult,
    TransformationPass,
};
use crate::ir::{
    entities::{FunctionData, FunctionKind},
    module::Module,
    passes::{LocalPass, LocalPassMut},
    values::{Block, Function, Inst},
};

const STRAIGHTEN: &str = "straighten";

pub struct Straighten {}

#[derive(Debug, Error)]
pub enum StraightenError {}

pub struct BlockPair {
    block1: Block,
    block2: Block,
}

impl LocalPassMut for Straighten {
    type Ok = ();

    fn run_on_function(
        &mut self,
        _function: Function,
        data: &mut FunctionData,
    ) -> PassResult<(Self::Ok, bool)> {
        let mut cfa = ControlFlowAnalysis {};
        let cfg = cfa.run_on_function(_function, data).unwrap();
        let mut changed = false;

        let exit_block = data.layout().exit_block().unwrap();

        let mut to_merge_blockpairs: Vec<BlockPair> = Vec::new();

        // bool token use for block merging
        let mut block_visited: HashMap<Block, bool> = HashMap::new();
        for (block, _block_node) in data.layout().blocks() {
            block_visited.insert(block, false);
        }

        // iter all blocks
        for (block, _block_node) in data.layout().blocks() {
            if block == exit_block {
                continue;
            }

            if let Some(visited) = block_visited.get_mut(&block) {
                if *visited {
                    continue;
                }
            }

            let succs = cfg.succs(&block);
            let succs_len = succs.as_ref().map_or(0, |v| v.len());

            // if curr block have only one succ
            if succs_len == 1 {
                let succ_block = cfg.succs(&block).unwrap().first().unwrap();
                let succ_preds = cfg.preds(succ_block);
                let succ_preds_len = succ_preds.as_ref().map_or(0, |v| v.len());
                // if the succ block have only one pred, then merge
                // ...A -> B ...
                if succ_preds_len == 1 {
                    if let Some(visited) = block_visited.get_mut(succ_block) {
                        *visited = true;
                    }
                    let pair = BlockPair {
                        block1: block,
                        block2: *succ_block,
                    };
                    to_merge_blockpairs.push(pair);
                }
            }
        }

        // Check if the vector is empty
        if to_merge_blockpairs.is_empty() {
            return Ok(((), false));
        } else {
            // merge the block couple
            for pair in to_merge_blockpairs {
                // remove branch inst
                if let Some(branch_inst) = data.layout().exit_inst_of_block(pair.block1) {
                    let _ = data.layout_mut().remove_inst(branch_inst);
                } else {
                    panic!("can not find branch inst - straighten");
                }

                // append succ_bb's instructions to curr_bb
                let instructions: Vec<Inst> = data
                    .layout()
                    .blocks()
                    .node(pair.block2)
                    .unwrap()
                    .insts()
                    .into_iter()
                    .map(|(inst, _)| inst)
                    .collect();

                for inst in instructions {
                    let _ = data.layout_mut().remove_inst(inst);
                    let _ = data.layout_mut().append_inst(inst, pair.block1);
                }

                // remove succ_bb
                data.remove_block(pair.block2);

                changed = true;
            }
        }
        Ok(((), changed))
    }
}

impl GlobalPassMut for Straighten {
    type Ok = ();

    fn run_on_module(&mut self, module: &mut Module) -> PassResult<(Self::Ok, bool)> {
        let functions = module.function_layout().to_vec();
        let mut changed = false;
        for function in functions {
            let function_data = module.function_data_mut(function).unwrap();

            if let FunctionKind::Declaration = *function_data.kind() {
                continue;
            }
            if let FunctionKind::Intrinsic = *function_data.kind() {
                continue;
            }
            let (_, local_changed) = self.run_on_function(function, function_data).unwrap();
            changed = changed || local_changed;
        }

        Ok(((), changed))
    }
}

impl Straighten {
    pub fn register() {
        let pass = Box::new(Straighten {});
        let canonic = Box::new(ControlFlowCanonicalization {});
        PassManager::register_transformation(STRAIGHTEN, pass, vec![canonic]);
    }
}

impl TransformationPass for Straighten {
    fn reset(&mut self) {}
}

#[cfg(test)]
mod test {
    use std::io::{BufWriter, Cursor};

    use super::{Straighten, STRAIGHTEN};
    use crate::ir::{
        frontend::parser::Parser,
        module::Module,
        passes::{printer::Printer, GlobalPass, PassManager},
    };
    fn print(module: &Module) {
        let mut buf = BufWriter::new(Vec::new());
        let mut printer = Printer::new(&mut buf);
        printer.run_on_module(module).unwrap();
        let s = String::from_utf8(buf.into_inner().unwrap()).unwrap();
        println!("{}", s);
    }

    #[test]
    fn test_straighten() {
        let ir = r#"
            func @check_positive(i32) -> i32 {
                ^entry(i32 %0):
                    jump ^positive
                
                ^positive:
                    jump ^exit

                ^exit:
                    ret i32 0x1

            }"#;

        let mut buf = Cursor::new(ir);
        let mut parser = Parser::new(&mut buf);
        let mut module = parser.parse().unwrap().into_ir("test".into()).unwrap();

        Straighten::register();
        let iter = PassManager::run_transformation(STRAIGHTEN, &mut module, 4321);
        assert_eq!(iter, 3);
        print(&module);
    }
}
