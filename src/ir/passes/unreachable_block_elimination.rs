//! # Unreachable Block Elim
//!
//! This module contains the implementation of the Unreachable BB Elim pass.

use thiserror::Error;

use crate::ir::{
    entities::FunctionData,
    module::Module,
    passes::{LocalPass, LocalPassMut},
    values::{Block, Function},
};

use std::collections::HashSet;
use std::collections::VecDeque;

use super::{
    control_flow_analysis::ControlFlowAnalysis,
    control_flow_canonicalization::ControlFlowCanonicalization, GlobalPassMut, PassManager,
    PassResult, TransformationPass,
};

const UNREACHABLE_BLOCK_ELIMINATION: &str = "unreachable-block-elimination";

pub struct UnreachableBlockElimination {}

#[derive(Debug, Error)]
pub enum UnreachableBlockEliminationError {}

impl LocalPassMut for UnreachableBlockElimination {
    type Ok = ();

    fn run_on_function(
        &mut self,
        _function: Function,
        data: &mut FunctionData,
    ) -> PassResult<(Self::Ok, bool)> {
        let mut cfa = ControlFlowAnalysis {};
        let cfg = cfa.run_on_function(_function, data).unwrap();

        let mut reachable_blocks: HashSet<Block> = HashSet::new();
        let mut queue: VecDeque<Block> = VecDeque::new();

        let entry_block = data.layout().entry_block().unwrap();

        queue.push_back(entry_block);
        reachable_blocks.insert(entry_block);

        let mut changed = false;

        while let Some(block) = queue.pop_front() {
            if let Some(succs) = cfg.succs(&block) {
                for &succ in succs.iter() {
                    if reachable_blocks.insert(succ) {
                        // the block is not visited yet
                        queue.push_back(succ);
                    }
                }
            }
        }

        let mut unreachable_blocks: Vec<Block> = Vec::new();
        for (block, _block_node) in data.layout().blocks() {
            if !reachable_blocks.contains(&block) {
                unreachable_blocks.push(block);
            }
        }

        for block in unreachable_blocks {
            data.remove_block(block);
            changed = true;
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for UnreachableBlockElimination {
    type Ok = ();

    fn run_on_module(&mut self, module: &mut Module) -> PassResult<(Self::Ok, bool)> {
        let functions = module.function_layout().to_vec();
        let mut changed = false;
        for function in functions {
            let function_data = module.function_data_mut(function).unwrap();
            let (_, local_changed) = self.run_on_function(function, function_data).unwrap();
            changed = changed || local_changed;
        }

        Ok(((), changed))
    }
}

impl UnreachableBlockElimination {
    pub fn register() {
        let pass = Box::new(UnreachableBlockElimination {});
        let canonic = Box::new(ControlFlowCanonicalization {});
        PassManager::register_transformation(UNREACHABLE_BLOCK_ELIMINATION, pass, vec![canonic]);
    }
}

impl TransformationPass for UnreachableBlockElimination {
    fn reset(&mut self) {}
}

#[cfg(test)]
mod test {
    use std::io::{BufWriter, Cursor};

    use crate::ir::{
        frontend::parser::Parser,
        module::Module,
        passes::{printer::Printer, GlobalPass, PassManager},
    };

    use super::{UnreachableBlockElimination, UNREACHABLE_BLOCK_ELIMINATION};

    fn print(module: &Module) {
        let mut buf = BufWriter::new(Vec::new());
        let mut printer = Printer::new(&mut buf);
        printer.run_on_module(module).unwrap();
        let s = String::from_utf8(buf.into_inner().unwrap()).unwrap();
        println!("{}", s);
    }

    #[test]
    fn test_unreachable_block_elimination0() {
        let ir = r#"
            func @check_positive(i32) -> i32 {
                ^entry(i32 %0):
                    %cmp = icmp.sle i32 %0, i32 0x0
                    %cond = not i1 %cmp
                    br i1 %cond, ^positive, ^negative(i32 0xffffffff)
                
                ^unreachable_block:
                    %2 = add i32 1, i32 2
                    # need canonicalization to remove this block

                ^positive:
                    jump ^negative(i32 1)
                
                ^negative(i32 %1):
                    ret i32 %1

            }"#;

        let mut buf = Cursor::new(ir);
        let mut parser = Parser::new(&mut buf);
        let mut module = parser.parse().unwrap().into_ir("test".into()).unwrap();

        UnreachableBlockElimination::register();
        let iter =
            PassManager::run_transformation(UNREACHABLE_BLOCK_ELIMINATION, &mut module, 1234);
        assert_eq!(iter, 2);
        print(&module);
    }

    #[test]
    fn test_unreachable_block_elimination1() {
        let ir = r#"            
            func @test_func() -> void {
                ^1:
                    jump ^2
                ^2:
                    jump ^1
                # the following blocks are unreachable
                ^3:
                    jump ^2
                ^4:
                    jump ^1
                ^5:
                    %0 = add i32 1, i32 2
                    %cond = icmp.sle i32 %0, i32 0x0
                    br %cond, ^4, ^3
            }"#;

        let mut buf = Cursor::new(ir);
        let mut parser = Parser::new(&mut buf);
        let mut module = parser.parse().unwrap().into_ir("test".into()).unwrap();

        UnreachableBlockElimination::register();
        let iter =
            PassManager::run_transformation(UNREACHABLE_BLOCK_ELIMINATION, &mut module, 1234);
        assert_eq!(iter, 2);
        print(&module);
    }
}
