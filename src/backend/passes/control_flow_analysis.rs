//! # Control Flow Analysis
//!
//! This module contains the implementation of the control flow analysis (CFA)
//! pass.

use std::collections::HashMap;

use thiserror::Error;

use crate::backend::{MachineBlock, MachineContext, MachineFunctionData, MachineInstData};

use super::{LocalPass, PassError, PassResult};

#[derive(Debug, Clone, Default)]
pub struct ControlFlowGraph {
    preds: HashMap<MachineBlock, Vec<MachineBlock>>,
    succs: HashMap<MachineBlock, Vec<MachineBlock>>,
}

impl ControlFlowGraph {
    pub fn new() -> Self { Self::default() }

    pub fn succs(&self, block: &MachineBlock) -> Option<&Vec<MachineBlock>> { self.succs.get(block) }

    pub fn preds(&self, block: &MachineBlock) -> Option<&Vec<MachineBlock>> { self.preds.get(block) }

    pub fn to_mermaid(&self) -> String {
        let mut result = String::from("graph TD;\n");
        
        for (block, succs) in &self.succs {
            for succ in succs {
                result.push_str(&format!("    {} --> {};\n", block.0, succ.0));
            }
        }
        
        result
    }
}

pub struct ControlFlowAnalysis {}

#[derive(Debug, Error)]
pub enum ControlFlowAnalysisError {
    #[error(
    "unexpected termination at block: {0:?}, please run the control flow normalization pass first."
  )]
    UnexpectedBlockTermination(MachineBlock),
}

impl From<ControlFlowAnalysisError> for PassError {
    fn from(err: ControlFlowAnalysisError) -> Self {
        PassError::analysis_error("control-flow-analysis".to_string(), Box::new(err))
    }
}

impl LocalPass for ControlFlowAnalysis {
    type Ok = ControlFlowGraph;

    fn run_on_function(
        &mut self,
        ctx: &MachineContext,
        data: &MachineFunctionData,
    ) -> PassResult<Self::Ok> {
        let mut cfg = ControlFlowGraph::new();

        let layout = data.layout();
        for (block, _block_node) in layout.blocks() {
            cfg.preds.insert(block, Vec::new());
        }
        for (block, _block_node) in layout.blocks() {
            let mut succ = Vec::new();
            // in asm, we need to check every instruction in the block
            for (inst, _) in layout.insts_of_block(block).unwrap() {
                let inst_data = ctx.inst_data(inst).unwrap();
                match inst_data {
                    MachineInstData::Branch { block: target, .. } => {
                        succ.push(*target);
                        cfg.preds.get_mut(target).unwrap().push(block);
                    }
                    MachineInstData::J { block: target, .. } => {
                        succ.push(*target);
                        cfg.preds.get_mut(target).unwrap().push(block);
                        break;
                    }
                    _ => {}
                }
            }
            if succ.is_empty() {
                if let Some(next_block) = layout.next_block(block) {
                    succ.push(next_block);
                    cfg.preds.get_mut(&next_block).unwrap().push(block);
                } else {
                    // this is the end of the function
                }
            }
            cfg.succs.insert(block, succ);
        }

        Ok(cfg)
    }
}

#[cfg(test)]
mod test {
    use std::io::Cursor;

    use super::ControlFlowAnalysis;
    use crate::{backend::{passes::LocalPass, MachineSymbol}, codegen::{self, CodegenContext}, ir::{frontend::parser::Parser, passes::{control_flow_canonicalization::ControlFlowCanonicalization, PassManager}}};

    #[test]
    fn test_cfa() {
        let ir = r#"
            func @check_positive(i32) -> i32 {
                ^entry(i32 %0):
                    %cmp = icmp.sle i32 %0, i32 0x0
                    %cond = not i1 %cmp
                    br i1 %cond, ^positive, ^negative(i32 0xffffffff)
                
                ^positive:
                    %2 = add %0, i32 1

                ^pass:
                    jump ^negative(i32 1)
                
                ^negative(i32 %1):
                    ret i32 %1
            }"#;

        let mut buf = Cursor::new(ir);
        let mut parser = Parser::new(&mut buf);
        let mut module = parser.parse().unwrap().into_ir("test".into()).unwrap();
        
        let mut codegen_ctx = CodegenContext::new();

        // ControlFlowCanonicalization::register();
        // let iter = PassManager::run_transformation("control-flow-canonicalization", &mut module, 1234);

        codegen_ctx.codegen(&module);

        println!("{:}", codegen_ctx.machine_ctx);

        let mut cfa = ControlFlowAnalysis {};

        let func = codegen_ctx.machine_ctx.functions.get(&MachineSymbol("check_positive".to_string())).unwrap();
        let cfg = cfa.run_on_function(&codegen_ctx.machine_ctx, func).unwrap();

        println!("{:}", cfg.to_mermaid());

    }

    #[test]
    fn test_cfa1() {
        let ir = r#"
        func @nestedBlocks() -> void {
            ^aa:
            ^ab:
            ^ce:
            ^cf:
            ^entry:
                jump ^outer
            ^outer:
                jump ^inner
                jump ^end
            ^inner:
            ^abaaa:
            ^abace:
            ^abacf:
                jump ^end
            ^end:
                ret void
            }
            "#;

        let mut buf = Cursor::new(ir);
        let mut parser = Parser::new(&mut buf);
        let mut module = parser.parse().unwrap().into_ir("test".into()).unwrap();
        
        let mut codegen_ctx = CodegenContext::new();

        // ControlFlowCanonicalization::register();
        // let iter = PassManager::run_transformation("control-flow-canonicalization", &mut module, 1234);

        codegen_ctx.codegen(&module);

        println!("{:}", codegen_ctx.machine_ctx);

        let mut cfa = ControlFlowAnalysis {};

        let func = codegen_ctx.machine_ctx.functions.get(&MachineSymbol("nestedBlocks".to_string())).unwrap();
        let cfg = cfa.run_on_function(&codegen_ctx.machine_ctx, func).unwrap();

        println!("{:}", cfg.to_mermaid());

    }
}
