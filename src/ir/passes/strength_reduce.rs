//! # Strength_reduce(IR)
//!
//! This module contains the implementation of the strength_reduce pass.
use std::collections::HashMap;

use thiserror::Error;

use super::{
    GlobalPassMut,
    PassManager,
    PassResult,
    TransformationPass,
};
use crate::ir::{
    entities::{FunctionData, FunctionKind, ValueKind},
    module::Module,
    passes::{LocalPass, LocalPassMut},
    values::{Binary, Block, Function, Inst, BinaryOp},
};

const STRENGTH_REDUCE: &str = "strength-reduce";

pub struct StrengthReduce {}

#[derive(Debug, Error)]
pub enum StrengthReduceError {}

// pub struct TochangeBlockInsts {
//     block: Block,
//     tochange_insts: Vec<Inst>,
// }

pub enum OptimizationSituation {
    // Multiplication by zero
    MulByZero,
    // Multiplication by a power of two, stores the number of shifts
    MulByPowerOfTwo { shift: usize },
    // Division by one
    DivByOne,
    // Division by negative one
    DivByNegativeOne,
    // Division by two
    DivByTwo,
    // Division by a power of two, stores the number of shifts
    DivByPowerOfTwo { shift: usize },
    // Remainder by a power of two, stores the bitmask for the operation
    SRemByPowerOfTwo { mask: usize },
}

pub struct StrengthReduceOptimization {
    inst: Inst,
    situation: OptimizationSituation,
}

impl LocalPassMut for StrengthReduce {
    type Ok = ();

    fn run_on_function(
        &mut self,
        _function: Function,
        data: &mut FunctionData,
    ) -> PassResult<(Self::Ok, bool)> {
        let mut changed = false;
        let dfg = &mut data.dfg;

        // let mut to_change_blockinsts: Vec<TochangeBlockInsts> = Vec::new();

        let mut inst_situation: Vec<StrengthReduceOptimization> = Vec::new();

        // iter all blocks, the part of record
        for (block, block_node) in data.layout.blocks() {

            // iter all inst in block
            for (inst, _) in block_node.insts() {
                let inst_data = dfg.local_value_data(inst.into()).unwrap();
                if let ValueKind::Binary(binary) = inst_data.kind() {
                    let operation = binary.op();
                    match operation {
                        BinaryOp::FMul => {
                            let lhs = binary.lhs();
                            let rhs = binary.rhs();
                            // if lhs/rhs is zero or lhs/rhs is one or lhs/rhs is negative one or lhs/rhs is +-power of two

                        }
                        BinaryOp::Mul => {
                            let lhs = binary.lhs();
                            let rhs = binary.rhs();
                            // if lhs/rhs is zero or lhs/rhs is one or lhs/rhs is negative one or lhs/rhs is +-power of two
                            
                        }
                        BinaryOp::FDiv => {
                            let lhs = binary.lhs();
                            let rhs = binary.rhs();
                            // if rhs is one or rhs is negative one or rhs is power of two
                        
                        }
                        BinaryOp::SDiv => {
                            let lhs = binary.lhs();
                            let rhs = binary.rhs();
                            // if rhs is one or rhs is negative one or rhs is power of two

                        }
                        BinaryOp::UDiv => {
                            let lhs = binary.lhs();
                            let rhs = binary.rhs();
                            // if rhs is one or rhs is power of two

                        }
                        BinaryOp::SRem => {
                            let lhs = binary.lhs();
                            let rhs = binary.rhs();
                            // if rhs is power of two

                        }
                        BinaryOp::URem => {
                            let lhs = binary.lhs();
                            let rhs = binary.rhs();
                            // if rhs is power of two

                        }
                        _ => {
                            continue;
                        }
                    }
                } else {
                    continue;
                }
            }
        }


        // Check if the vector is empty, the part of change
        if inst_situation.is_empty() {
            return Ok(((), false));
        } else {
            todo!()
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for StrengthReduce {
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

impl StrengthReduce {
    pub fn register() {
        let pass = Box::new(StrengthReduce {});
        PassManager::register_transformation(STRENGTH_REDUCE, pass, Vec::new());
    }
}

impl TransformationPass for StrengthReduce {
    fn reset(&mut self) {}
}

// #[cfg(test)]
// mod test {
//     use std::io::{BufWriter, Cursor};

//     use super::{StrengthReduce, STRENGTH_REDUCE};
//     use crate::ir::{
//         frontend::parser::Parser,
//         module::Module,
//         passes::{printer::Printer, GlobalPass, PassManager},
//     };
//     fn print(module: &Module) {
//         let mut buf = BufWriter::new(Vec::new());
//         let mut printer = Printer::new(&mut buf);
//         printer.run_on_module(module).unwrap();
//         let s = String::from_utf8(buf.into_inner().unwrap()).unwrap();
//         println!("{}", s);
//     }

//     #[test]
//     fn test_strength_reduce() {
//         let ir = r#"

//             }"#;

//         let mut buf = Cursor::new(ir);
//         let mut parser = Parser::new(&mut buf);
//         let mut module = parser.parse().unwrap().into_ir("test".into()).unwrap();

//         StrengthReduce::register();
//         let iter = PassManager::run_transformation(STRENGTH_REDUCE, &mut module, 4321);
//         assert_eq!(iter, 3);
//         print(&module);
//     }
// }
