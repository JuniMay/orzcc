use std::collections::{HashMap, HashSet};

use thiserror::Error;

use super::{
    block_defsue_analysis::DefUseAnalysis,
    control_flow_analysis::ControlFlowAnalysis,
    LocalPass,
    PassError,
    PassResult,
};
use crate::backend::{MachineBlock, MachineContext, MachineFunctionData, MachineSymbol, Register};

#[derive(Debug, Clone, Default)]
pub struct BlockInOut {
    pub in_: HashMap<MachineBlock, HashSet<Register>>,
    pub out: HashMap<MachineBlock, HashSet<Register>>,
}

impl BlockInOut {
    pub fn new() -> Self { Self::default() }

    pub fn in_(&self, block: &MachineBlock) -> Option<&HashSet<Register>> { self.in_.get(block) }

    pub fn out(&self, block: &MachineBlock) -> Option<&HashSet<Register>> { self.out.get(block) }
}

pub struct InOutAnalysis {}

impl LocalPass for InOutAnalysis {
    type Ok = BlockInOut;

    fn run_on_function(
        &mut self,
        ctx: &MachineContext,
        data: &MachineFunctionData,
    ) -> PassResult<Self::Ok> {
        let mut cfa = ControlFlowAnalysis {};
        let cfg = cfa.run_on_function(ctx, data)?;

        let mut dua = DefUseAnalysis {};
        let defs_uses = dua.run_on_function(ctx, data)?;

        let mut in_ = HashMap::new();
        let mut out = HashMap::new();

        for (block, _) in data.layout().blocks() {
            in_.insert(block, HashSet::<Register>::new());
            out.insert(block, HashSet::<Register>::new());
        }

        let mut changed = true;

        while changed {
            changed = false;

            // iterate over all blocks reverse order
            for (block, _) in data.layout().blocks().into_iter().rev() {
                let mut in_set: HashSet<Register> = HashSet::new();
                let mut out_set: HashSet<Register> = HashSet::new();

                // out[B] = U in[S] for all S in succ[B]
                for succ in cfg.succs(&block).unwrap() {
                    out_set.extend(in_.get(succ).unwrap().iter().cloned());
                }

                // in[B] = use[B] U (out[B] - def[B])
                in_set.extend(defs_uses.uses(&block).unwrap().iter().cloned());
                in_set.extend(out_set.difference(defs_uses.defs(&block).unwrap()).cloned());

                if in_.get(&block).unwrap() != &in_set {
                    in_.insert(block, in_set);
                    changed = true;
                }

                if out.get(&block).unwrap() != &out_set {
                    out.insert(block, out_set);
                    changed = true;
                }
            }
        }

        Ok(BlockInOut { in_, out })
    }
}

#[cfg(test)]
mod test {
    use std::io::Cursor;

    use super::InOutAnalysis;
    use crate::{
        backend::{
            passes::{block_defsue_analysis::DefUseAnalysis, LocalPass},
            MachineSymbol,
        },
        codegen::CodegenContext,
        ir::frontend::parser::Parser,
    };

    #[test]
    fn test_inout() {
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
        codegen_ctx.codegen(&module);

        println!("{:}", codegen_ctx.machine_ctx);

        let mut in_out = InOutAnalysis {};

        let func = codegen_ctx
            .machine_ctx
            .functions
            .get(&MachineSymbol("check_positive".to_string()))
            .unwrap();

        let block_inout = in_out
            .run_on_function(&codegen_ctx.machine_ctx, func)
            .unwrap();

        println!("{:#?}", block_inout);
    }
}
