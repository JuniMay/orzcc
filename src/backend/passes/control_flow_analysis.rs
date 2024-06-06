//! # Control Flow Analysis
//!
//! This module contains the implementation of the control flow analysis (CFA)
//! pass.

use std::collections::{HashMap, HashSet};

use thiserror::Error;

use super::{LocalPass, PassError, PassResult};
use crate::backend::{MachineBlock, MachineContext, MachineFunctionData, MachineInstData};

#[derive(Debug, Clone, Default)]
pub struct ControlFlowGraph {
    preds: HashMap<MachineBlock, Vec<MachineBlock>>,
    succs: HashMap<MachineBlock, Vec<MachineBlock>>,
}

impl ControlFlowGraph {
    pub fn new() -> Self { Self::default() }

    pub fn succs(&self, block: &MachineBlock) -> Option<&Vec<MachineBlock>> {
        self.succs.get(block)
    }

    pub fn preds(&self, block: &MachineBlock) -> Option<&Vec<MachineBlock>> {
        self.preds.get(block)
    }

    pub fn to_mermaid(&self) -> String {
        let mut result = String::from("graph TD;\n");

        for (block, succs) in &self.succs {
            for succ in succs {
                result.push_str(&format!("    {} --> {};\n", block.0, succ.0));
            }
        }

        result
    }

    pub fn post_order(&self, entry: MachineBlock) -> Vec<MachineBlock> {
        // get post order traversal of the control flow graph, using iteration instead of recursion
        let mut post_order = Vec::new();
        let mut visited = HashSet::new();
        let mut stack1 = vec![entry];
        let mut stack2 = Vec::new();

        while let Some(block) = stack1.pop() {
            if visited.contains(&block) {
                continue;
            }

            visited.insert(block);
            stack2.push(block);

            if let Some(succs) = self.succs.get(&block) {
                for succ in succs {
                    stack1.push(*succ);
                }
            }
        }

        while let Some(block) = stack2.pop() {
            post_order.push(block);
        }

        post_order
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
    use crate::{
        backend::{passes::LocalPass, MachineSymbol},
        codegen::{self, CodegenContext},
        ir::{
            frontend::parser::Parser,
            passes::{control_flow_canonicalization::ControlFlowCanonicalization, PassManager},
        },
    };

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
        // let iter = PassManager::run_transformation("control-flow-canonicalization",
        // &mut module, 1234);

        codegen_ctx.codegen(&module);

        println!("{:}", codegen_ctx.machine_ctx);

        let mut cfa = ControlFlowAnalysis {};

        let func = codegen_ctx
            .machine_ctx
            .functions
            .get(&MachineSymbol("check_positive".to_string()))
            .unwrap();
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
        // let iter = PassManager::run_transformation("control-flow-canonicalization",
        // &mut module, 1234);

        codegen_ctx.codegen(&module);

        println!("{:}", codegen_ctx.machine_ctx);

        let mut cfa = ControlFlowAnalysis {};

        let func = codegen_ctx
            .machine_ctx
            .functions
            .get(&MachineSymbol("nestedBlocks".to_string()))
            .unwrap();
        let cfg = cfa.run_on_function(&codegen_ctx.machine_ctx, func).unwrap();

        println!("{:}", cfg.to_mermaid());
    }

    #[test]
    fn test_post_order() {
        let ir = r#"
        func @param32_rec(i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32) -> i32 {
            ^0(i32 %__ARG_a1, i32 %__ARG_a2, i32 %__ARG_a3, i32 %__ARG_a4, i32 %__ARG_a5, i32 %__ARG_a6, i32 %__ARG_a7, i32 %__ARG_a8, i32 %__ARG_a9, i32 %__ARG_a10, i32 %__ARG_a11, i32 %__ARG_a12, i32 %__ARG_a13, i32 %__ARG_a14, i32 %__ARG_a15, i32 %__ARG_a16, i32 %__ARG_a17, i32 %__ARG_a18, i32 %__ARG_a19, i32 %__ARG_a20, i32 %__ARG_a21, i32 %__ARG_a22, i32 %__ARG_a23, i32 %__ARG_a24, i32 %__ARG_a25, i32 %__ARG_a26, i32 %__ARG_a27, i32 %__ARG_a28, i32 %__ARG_a29, i32 %__ARG_a30, i32 %__ARG_a31, i32 %__ARG_a32):
                %__SLOT_ARG_a32 = alloc i32
                %__SLOT_ARG_a31 = alloc i32
                %__SLOT_ARG_a30 = alloc i32
                %__SLOT_ARG_a29 = alloc i32
                %__SLOT_ARG_a28 = alloc i32
                %__SLOT_ARG_a27 = alloc i32
                %__SLOT_ARG_a26 = alloc i32
                %__SLOT_ARG_a25 = alloc i32
                %__SLOT_ARG_a24 = alloc i32
                %__SLOT_ARG_a23 = alloc i32
                %__SLOT_ARG_a22 = alloc i32
                %__SLOT_ARG_a21 = alloc i32
                %__SLOT_ARG_a20 = alloc i32
                %__SLOT_ARG_a19 = alloc i32
                %__SLOT_ARG_a18 = alloc i32
                %__SLOT_ARG_a17 = alloc i32
                %__SLOT_ARG_a16 = alloc i32
                %__SLOT_ARG_a15 = alloc i32
                %__SLOT_ARG_a14 = alloc i32
                %__SLOT_ARG_a13 = alloc i32
                %__SLOT_ARG_a12 = alloc i32
                %__SLOT_ARG_a11 = alloc i32
                %__SLOT_ARG_a10 = alloc i32
                %__SLOT_ARG_a9 = alloc i32
                %__SLOT_ARG_a8 = alloc i32
                %__SLOT_ARG_a7 = alloc i32
                %__SLOT_ARG_a6 = alloc i32
                %__SLOT_ARG_a5 = alloc i32
                %__SLOT_ARG_a4 = alloc i32
                %__SLOT_ARG_a3 = alloc i32
                %__SLOT_ARG_a2 = alloc i32
                %__SLOT_ARG_a1 = alloc i32
                store i32 %__ARG_a1, ptr %__SLOT_ARG_a1
                store i32 %__ARG_a2, ptr %__SLOT_ARG_a2
                store i32 %__ARG_a3, ptr %__SLOT_ARG_a3
                store i32 %__ARG_a4, ptr %__SLOT_ARG_a4
                store i32 %__ARG_a5, ptr %__SLOT_ARG_a5
                store i32 %__ARG_a6, ptr %__SLOT_ARG_a6
                store i32 %__ARG_a7, ptr %__SLOT_ARG_a7
                store i32 %__ARG_a8, ptr %__SLOT_ARG_a8
                store i32 %__ARG_a9, ptr %__SLOT_ARG_a9
                store i32 %__ARG_a10, ptr %__SLOT_ARG_a10
                store i32 %__ARG_a11, ptr %__SLOT_ARG_a11
                store i32 %__ARG_a12, ptr %__SLOT_ARG_a12
                store i32 %__ARG_a13, ptr %__SLOT_ARG_a13
                store i32 %__ARG_a14, ptr %__SLOT_ARG_a14
                store i32 %__ARG_a15, ptr %__SLOT_ARG_a15
                store i32 %__ARG_a16, ptr %__SLOT_ARG_a16
                store i32 %__ARG_a17, ptr %__SLOT_ARG_a17
                store i32 %__ARG_a18, ptr %__SLOT_ARG_a18
                store i32 %__ARG_a19, ptr %__SLOT_ARG_a19
                store i32 %__ARG_a20, ptr %__SLOT_ARG_a20
                store i32 %__ARG_a21, ptr %__SLOT_ARG_a21
                store i32 %__ARG_a22, ptr %__SLOT_ARG_a22
                store i32 %__ARG_a23, ptr %__SLOT_ARG_a23
                store i32 %__ARG_a24, ptr %__SLOT_ARG_a24
                store i32 %__ARG_a25, ptr %__SLOT_ARG_a25
                store i32 %__ARG_a26, ptr %__SLOT_ARG_a26
                store i32 %__ARG_a27, ptr %__SLOT_ARG_a27
                store i32 %__ARG_a28, ptr %__SLOT_ARG_a28
                store i32 %__ARG_a29, ptr %__SLOT_ARG_a29
                store i32 %__ARG_a30, ptr %__SLOT_ARG_a30
                store i32 %__ARG_a31, ptr %__SLOT_ARG_a31
                store i32 %__ARG_a32, ptr %__SLOT_ARG_a32
                %__RET_SLOT = alloc i32
            
            ^if:
                %0 = load i32, ptr %__SLOT_ARG_a1
                %1 = icmp.eq i32 %0, i32 0x00000000
                br i1 %1, ^then, ^else
            
            ^then:
                %2 = load i32, ptr %__SLOT_ARG_a2
                store i32 %2, ptr %__RET_SLOT
                jump ^1()
            
            ^else:
                %3 = load i32, ptr %__SLOT_ARG_a1
                %4 = sub i32 %3, i32 0x00000001
                %5 = load i32, ptr %__SLOT_ARG_a2
                %6 = load i32, ptr %__SLOT_ARG_a3
                %7 = add i32 %5, i32 %6
                %8 = srem i32 %7, i32 0x3b800001
                %9 = load i32, ptr %__SLOT_ARG_a4
                %10 = load i32, ptr %__SLOT_ARG_a5
                %11 = load i32, ptr %__SLOT_ARG_a6
                %12 = load i32, ptr %__SLOT_ARG_a7
                %13 = load i32, ptr %__SLOT_ARG_a8
                %14 = load i32, ptr %__SLOT_ARG_a9
                %15 = load i32, ptr %__SLOT_ARG_a10
                %16 = load i32, ptr %__SLOT_ARG_a11
                %17 = load i32, ptr %__SLOT_ARG_a12
                %18 = load i32, ptr %__SLOT_ARG_a13
                %19 = load i32, ptr %__SLOT_ARG_a14
                %20 = load i32, ptr %__SLOT_ARG_a15
                %21 = load i32, ptr %__SLOT_ARG_a16
                %22 = load i32, ptr %__SLOT_ARG_a17
                %23 = load i32, ptr %__SLOT_ARG_a18
                %24 = load i32, ptr %__SLOT_ARG_a19
                %25 = load i32, ptr %__SLOT_ARG_a20
                %26 = load i32, ptr %__SLOT_ARG_a21
                %27 = load i32, ptr %__SLOT_ARG_a22
                %28 = load i32, ptr %__SLOT_ARG_a23
                %29 = load i32, ptr %__SLOT_ARG_a24
                %30 = load i32, ptr %__SLOT_ARG_a25
                %31 = load i32, ptr %__SLOT_ARG_a26
                %32 = load i32, ptr %__SLOT_ARG_a27
                %33 = load i32, ptr %__SLOT_ARG_a28
                %34 = load i32, ptr %__SLOT_ARG_a29
                %35 = load i32, ptr %__SLOT_ARG_a30
                %36 = load i32, ptr %__SLOT_ARG_a31
                %37 = load i32, ptr %__SLOT_ARG_a32
                %38 = call i32 @param32_rec(i32 %4, i32 %8, i32 %9, i32 %10, i32 %11, i32 %12, i32 %13, i32 %14, i32 %15, i32 %16, i32 %17, i32 %18, i32 %19, i32 %20, i32 %21, i32 %22, i32 %23, i32 %24, i32 %25, i32 %26, i32 %27, i32 %28, i32 %29, i32 %30, i32 %31, i32 %32, i32 %33, i32 %34, i32 %35, i32 %36, i32 %37, i32 0x00000000)
                store i32 %38, ptr %__RET_SLOT
                jump ^1()
            
            ^exit:
            
            ^1:
                %39 = load i32, ptr %__RET_SLOT
                ret i32 %39
            
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

        let func = codegen_ctx.machine_ctx.functions.get(&MachineSymbol("param32_rec".to_string())).unwrap();
        let cfg = cfa.run_on_function(&codegen_ctx.machine_ctx, func).unwrap();

        println!("{:}", cfg.to_mermaid());

        let post_order = cfg.post_order(func.layout().entry_block().unwrap());

        for block in post_order {
            println!("{:?}", block);
        }

    }
}
