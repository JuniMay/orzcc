//! # Control-Flow Normalization Pass of IR
//!
//! This pass will normalize the IR structure, which will transform the
//! following:
//! 1. If a block has no terminator (which is a jump, branch, or a return
//!    instruction), a jump to the next block will be added.
//! 2. If a block has more instructions after the terminator, those instructions
//!    will removed.
//!
//! This pass will not optimize the code, but make the IR structure more
//! consistent and easier to work with.

use thiserror::Error;

use super::{GlobalPassMut, PassError, PassManager, PassResult, TransformationPass};
use crate::ir::{
    builders::BuildLocalValue,
    entities::FunctionData,
    passes::LocalPassMut,
    values::{Block, Function},
};

const CONTROL_FLOW_CANONICALIZATION: &str = "control-flow-canonicalization";

pub struct ControlFlowCanonicalization;

#[derive(Error, Debug)]
pub enum ControlFlowCanonicalizationError {
    #[error("block parameters exist in the next block, a jump cannot be added. block: {0:?}")]
    InvalidBlockParameters(Block),

    #[error("next block not found for block: {0:?}, a jump cannot be added.")]
    NextBlockNotFound(Block),
}

impl From<ControlFlowCanonicalizationError> for PassError {
    fn from(err: ControlFlowCanonicalizationError) -> Self {
        PassError::preparation_error(CONTROL_FLOW_CANONICALIZATION.to_string(), Box::new(err))
    }
}

impl LocalPassMut for ControlFlowCanonicalization {
    type Ok = ();

    fn run_on_function(
        &mut self,
        _function: Function,
        data: &mut FunctionData,
    ) -> PassResult<(Self::Ok, bool)> {
        let mut insts_to_remove = Vec::new();
        let mut blocks_to_add_jump = Vec::new();

        let dfg = &data.dfg;
        let layout = &data.layout;

        let mut changed = false;

        for (block, block_node) in layout.blocks() {
            let mut has_terminator = false;
            for (inst, _inst_node) in block_node.insts() {
                if has_terminator {
                    // there is already a terminator, remove the rest of the instructions
                    insts_to_remove.push(inst);
                    continue;
                }
                let inst_data = dfg.local_value_data(inst.into()).unwrap();
                if inst_data.kind().is_terminator() {
                    has_terminator = true;
                }
            }
            if !has_terminator {
                let next_block = layout
                    .next_block(block)
                    .ok_or(ControlFlowCanonicalizationError::NextBlockNotFound(block))?;
                let next_block_data = dfg.block_data(next_block).unwrap();
                if !next_block_data.params().is_empty() {
                    // there are params in the next block, a jump cannot be added
                    return Err(
                        ControlFlowCanonicalizationError::InvalidBlockParameters(block).into(),
                    );
                }
                blocks_to_add_jump.push((block, next_block));
            }
        }

        for inst in insts_to_remove {
            data.remove_inst(inst);
            changed = true;
        }

        for (block, next_block) in blocks_to_add_jump {
            let jump = data.dfg.builder().jump(next_block, vec![]).unwrap();
            data.layout.append_inst(jump.into(), block).unwrap();
            changed = true;
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for ControlFlowCanonicalization {
    type Ok = ();

    fn run_on_module(
        &mut self,
        module: &mut crate::ir::module::Module,
    ) -> PassResult<(Self::Ok, bool)> {
        let functions = module.function_layout().to_vec();
        let mut changed = false;
        for function in functions {
            let function_data = module.function_data_mut(function).unwrap();
            let (_, local_changed) = self.run_on_function(function, function_data)?;
            changed = changed || local_changed;
        }

        Ok(((), changed))
    }
}

impl ControlFlowCanonicalization {
    pub fn register() {
        let pass = Box::new(ControlFlowCanonicalization {});
        PassManager::register_transformation(CONTROL_FLOW_CANONICALIZATION, pass, Vec::new());
    }
}

impl TransformationPass for ControlFlowCanonicalization {
    fn reset(&mut self) {}
}

#[cfg(test)]
mod test {
    use std::io::{BufWriter, Cursor};

    use super::ControlFlowCanonicalization;
    use crate::ir::{
        frontend::parser::Parser,
        module::Module,
        passes::{
            control_flow_canonicalization::CONTROL_FLOW_CANONICALIZATION,
            printer::Printer,
            GlobalPass,
            PassManager,
        },
    };

    fn verify(module: &Module, function_name: &str) -> bool {
        let function = module.get_value_by_name(function_name).unwrap();
        let function_data = module.function_data(function.into()).unwrap();

        let valid = true;

        let dfg = &function_data.dfg;
        let layout = &function_data.layout;

        for (_block, block_node) in layout.blocks() {
            let mut has_terminator = false;
            for (inst, _inst_node) in block_node.insts() {
                if has_terminator {
                    return false;
                }
                let inst_data = dfg.local_value_data(inst.into()).unwrap();
                if inst_data.kind().is_terminator() {
                    has_terminator = true;
                }
            }
            if !has_terminator {
                return false;
            }
        }

        valid
    }

    fn print(module: &Module) {
        let mut buf = BufWriter::new(Vec::new());
        let mut printer = Printer::new(&mut buf);
        printer.run_on_module(module).unwrap();
        let s = String::from_utf8(buf.into_inner().unwrap()).unwrap();
        println!("{}", s);
    }

    #[test]
    fn test_canonicalization() {
        let ir = r#"
            func @test_func() -> void {
            ^entry:
                %0 = add i32 1, i32 2
                jump ^block1
                %1 = sub i32 3, i32 4
                %2 = add i32 %0, %1
            ^block1:
            ^block2:
            ^block3:
                %3 = add i32 5, i32 6
                %4 = sub i32 7, i32 8
                %5 = add i32 %3, %4
            ^block4:
            ^block5:
                ret
            }"#;
        let mut buf = Cursor::new(ir);
        let mut parser = Parser::new(&mut buf);
        let mut module = parser.parse().unwrap().into_ir("test".into()).unwrap();

        ControlFlowCanonicalization::register();
        let iter =
            PassManager::run_transformation(CONTROL_FLOW_CANONICALIZATION, &mut module, 1234);

        assert_eq!(iter, 2);
        assert!(verify(&module, "@test_func"));

        print(&module);
    }
}
