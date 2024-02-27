//! # Unreachable Block Elim
//!
//! This module contains the implementation of the Unreachable BB Elim pass.

use thiserror::Error;

use crate::ir::{
    entities::FunctionData,
    passes::{LocalPass, LocalPassMut},
    values::{Block, Function},
};

use std::collections::HashSet;
use std::collections::VecDeque;

use super::control_flow_analysis::ControlFlowAnalysis;

pub struct UnreachableBlockElimination {}

#[derive(Debug, Error)]
pub enum UnreachableBlockEliminationError {}

impl LocalPassMut for UnreachableBlockElimination {
    type Ok = ();
    type Err = UnreachableBlockEliminationError;

    fn run(&mut self, _function: Function, data: &mut FunctionData) -> Result<Self::Ok, Self::Err> {
        let mut cfa = ControlFlowAnalysis {};
        let cfg = cfa.run(_function, data).unwrap();

        let mut reachable_blocks: HashSet<Block> = HashSet::new();
        let mut queue: VecDeque<Block> = VecDeque::new();

        let entry_block = data.layout().entry_block().unwrap();

        queue.push_back(entry_block);
        reachable_blocks.insert(entry_block);

        while let Some(block) = queue.pop_front() {
            if let Some(succs) = cfg.succ(&block) {
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
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use std::io::{BufWriter, Cursor};

    use crate::ir::{
        frontend::parser::Parser,
        module::Module,
        passes::{control_flow_normalization::ControlFlowNormalization, printer::Printer},
        passes::{GlobalPass, LocalPassMut},
    };

    use super::UnreachableBlockElimination;

    fn print(module: &Module) {
        let mut buf = BufWriter::new(Vec::new());
        let mut printer = Printer::new(&mut buf);
        printer.run(module).unwrap();
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
                    # need normalization to remove this block

                ^positive:
                    jump ^negative(i32 1)
                
                ^negative(i32 %1):
                    ret i32 %1

            }"#;

        let mut buf = Cursor::new(ir);
        let mut parser = Parser::new(&mut buf);
        let mut module = parser.parse().unwrap().into_ir("test".into()).unwrap();
        let mut normalization = ControlFlowNormalization {};
        let mut ube = UnreachableBlockElimination {};

        let function = module.get_value_by_name("@check_positive").unwrap();
        let function_data = module.function_data_mut(function.into()).unwrap();

        normalization.run(function.into(), function_data).unwrap();
        ube.run(function.into(), function_data).unwrap();

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
        let mut normalization = ControlFlowNormalization {};
        let mut ube = UnreachableBlockElimination {};

        let function = module.get_value_by_name("@test_func").unwrap();
        let function_data = module.function_data_mut(function.into()).unwrap();

        normalization.run(function.into(), function_data).unwrap();
        ube.run(function.into(), function_data).unwrap();

        print(&module);
    }
}
