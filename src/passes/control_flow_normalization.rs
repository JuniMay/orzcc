//! # Control-Flow Normalization Pass of IR
//!
//! This pass will normalize the IR structure, which will transform the following:
//! 1. If a block has no terminator (which is a jump, branch, or a return instruction), a jump
//!    to the next block will be added.
//! 2. If a block has more instructions after the terminator, those instructions will removed.
//!
//! This pass will not optimize the code, but make the IR structure more consistent and easier to
//! work with.

use thiserror::Error;

use crate::ir::{
    builder::LocalValueBuilder,
    entities::FunctionData,
    pass::LocalPassMut,
    values::{Block, Function},
};

pub struct ControlFlowNormalization {}

#[derive(Error, Debug)]
pub enum ControlFlowNormalizationError {
    #[error("block parameters exist in the next block, a jump cannot be added. block: {0:?}")]
    InvalidBlockParameters(Block),
}

impl LocalPassMut for ControlFlowNormalization {
    type Ok = ();
    type Err = ControlFlowNormalizationError;

    fn run(&mut self, _function: Function, data: &mut FunctionData) -> Result<Self::Ok, Self::Err> {
        let mut insts_to_remove = Vec::new();
        let mut blocks_to_add_jump = Vec::new();

        let dfg = data.dfg();
        let layout = data.layout();

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
                let next_block = layout.next_block(block).unwrap();
                let next_block_data = dfg.block_data(next_block).unwrap();
                if next_block_data.params().len() > 0 {
                    // there are params in the next block, a jump cannot be added
                    return Err(ControlFlowNormalizationError::InvalidBlockParameters(block));
                }
                blocks_to_add_jump.push((block, next_block));
            }
        }

        for inst in insts_to_remove {
            data.layout_mut().remove_inst(inst).unwrap();
        }

        for (block, next_block) in blocks_to_add_jump {
            let jump = data.dfg_mut().builder().jump(next_block, vec![]).unwrap();
            data.layout_mut().append_inst(jump.into(), block).unwrap();
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::ControlFlowNormalization;
    use crate::{
        ir::{
            frontend::parser::Parser,
            module::Module,
            pass::{GlobalPass, LocalPassMut},
        },
        passes::printer::Printer,
    };
    use std::io::{self, Cursor};

    fn verify(module: &Module, function_name: &str) -> bool {
        let function = module.get_value_by_name(function_name).unwrap();
        let function_data = module.function_data(function.into()).unwrap();

        let valid = true;

        let dfg = function_data.dfg();
        let layout = function_data.layout();

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

    fn _print(module: &Module) {
        let mut stdout = io::stdout();
        let mut printer = Printer::new(&mut stdout);
        printer.run(module).unwrap();
    }

    #[test]
    fn test_normalization() {
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

        let mut pass = ControlFlowNormalization {};

        let function = module.get_value_by_name("@test_func").unwrap();
        let function_data = module.function_data_mut(function.into()).unwrap();

        pass.run(function.into(), function_data).unwrap();

        assert!(verify(&module, "@test_func"));

        // _print(&module);
    }
}
