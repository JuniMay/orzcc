//! # Peephole(IR)
//!
//! This module contains the implementation of the peephole pass.
use thiserror::Error;

use super::{
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

const PEEPHOLE: &str = "peephole";

pub struct Peephole {}

#[derive(Debug, Error)]
pub enum PeepholeError {}


impl LocalPassMut for Peephole {
    type Ok = ();

    fn run_on_function(
        &mut self,
        _function: Function,
        data: &mut FunctionData,
    ) -> PassResult<(Self::Ok, bool)> {
        let mut changed = false;

        todo!("Peephole pass");

        Ok(((), changed))
    }
}

impl GlobalPassMut for Peephole {
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

impl Peephole {
    pub fn register() {
        let pass = Box::new(Peephole {});
        PassManager::register_transformation(PEEPHOLE, pass, Vec::new());
    }
}

impl TransformationPass for Peephole {
    fn reset(&mut self) {}
}

// #[cfg(test)]
// mod test {
//     use std::io::{BufWriter, Cursor};

//     use super::{Peephole, PEEPHOLE};
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
//     fn test_peephole() {
//         let ir = r#"

//             }"#;

//         let mut buf = Cursor::new(ir);
//         let mut parser = Parser::new(&mut buf);
//         let mut module = parser.parse().unwrap().into_ir("test".into()).unwrap();

//         Peephole::register();
//         let iter = PassManager::run_transformation(PEEPHOLE, &mut module, 4321);
//         assert_eq!(iter, 3);
//         print(&module);
//     }
// }
