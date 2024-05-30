use std::collections::{HashMap, HashSet};

use thiserror::Error;

use super::{LocalPass, PassError, PassResult};
use crate::backend::{
    MachineBlock,
    MachineContext,
    MachineFunctionData,
    MachineInstData,
    MachineSymbol,
    Register,
    RiscvFpReg,
    RiscvGpReg,
};

const ARGUMENT_REGISTERS: [Register; 16] = [
    Register::General(RiscvGpReg::A0),
    Register::General(RiscvGpReg::A1),
    Register::General(RiscvGpReg::A2),
    Register::General(RiscvGpReg::A3),
    Register::General(RiscvGpReg::A4),
    Register::General(RiscvGpReg::A5),
    Register::General(RiscvGpReg::A6),
    Register::General(RiscvGpReg::A7),
    Register::FloatingPoint(RiscvFpReg::Fa0),
    Register::FloatingPoint(RiscvFpReg::Fa1),
    Register::FloatingPoint(RiscvFpReg::Fa2),
    Register::FloatingPoint(RiscvFpReg::Fa3),
    Register::FloatingPoint(RiscvFpReg::Fa4),
    Register::FloatingPoint(RiscvFpReg::Fa5),
    Register::FloatingPoint(RiscvFpReg::Fa6),
    Register::FloatingPoint(RiscvFpReg::Fa7),
];

const RETURN_REGISTERS: [Register; 5] = [
    Register::General(RiscvGpReg::Ra),
    Register::General(RiscvGpReg::A0),
    Register::General(RiscvGpReg::A1),
    Register::FloatingPoint(RiscvFpReg::Fa0),
    Register::FloatingPoint(RiscvFpReg::Fa1),
];

#[derive(Debug, Clone, Default)]
pub struct BlockDefUse {
    pub uses: HashMap<MachineBlock, HashSet<Register>>,
    pub defs: HashMap<MachineBlock, HashSet<Register>>,
}

impl BlockDefUse {
    pub fn new() -> Self { Self::default() }

    pub fn uses(&self, block: &MachineBlock) -> Option<&HashSet<Register>> { self.uses.get(block) }

    pub fn defs(&self, block: &MachineBlock) -> Option<&HashSet<Register>> { self.defs.get(block) }
}

pub struct DefUseAnalysis {}

impl LocalPass for DefUseAnalysis {
    type Ok = BlockDefUse;

    fn run_on_function(
        &mut self,
        ctx: &MachineContext,
        data: &MachineFunctionData,
    ) -> PassResult<Self::Ok> {
        let mut block_defuse = BlockDefUse::new();

        for (block, _block_node) in data.layout.blocks() {
            let mut uses = HashSet::new();
            let mut defs = HashSet::new();

            for (inst, _) in data.layout().insts_of_block(block).unwrap() {
                let inst_data = ctx.inst_data(inst).unwrap();
                match inst_data {
                    MachineInstData::Load { dest, base, .. } => {
                        defs.insert(*dest);
                        uses.insert(*base);
                    }
                    MachineInstData::FloatLoad { dest, base, .. } => {
                        defs.insert(*dest);
                        uses.insert(*base);
                    }
                    MachineInstData::PseudoLoad { dest, .. } => {
                        defs.insert(*dest);
                    }
                    MachineInstData::PseudoStore { value, rt, .. } => {
                        uses.insert(*value);
                        defs.insert(*rt);
                        uses.insert(*rt);
                    }
                    MachineInstData::FloatPseudoLoad { dest, rt, .. } => {
                        defs.insert(*dest);
                        defs.insert(*rt);
                        uses.insert(*rt);
                    }
                    MachineInstData::FloatPseudoStore { value, rt, .. } => {
                        uses.insert(*value);
                        defs.insert(*rt);
                        uses.insert(*rt);
                    }
                    MachineInstData::Store { value, base, .. } => {
                        uses.insert(*value);
                        uses.insert(*base);
                    }
                    MachineInstData::FloatStore { value, base, .. } => {
                        uses.insert(*value);
                        uses.insert(*base);
                    }
                    MachineInstData::FMv { rd, rs, .. } => {
                        defs.insert(*rd);
                        uses.insert(*rs);
                    }
                    MachineInstData::FCvt { rd, rs, .. } => {
                        defs.insert(*rd);
                        uses.insert(*rs);
                    }
                    MachineInstData::Binary { rd, rs1, rs2, .. } => {
                        defs.insert(*rd);
                        uses.insert(*rs1);
                        uses.insert(*rs2);
                    }
                    MachineInstData::BinaryImm { rd, rs1, .. } => {
                        defs.insert(*rd);
                        uses.insert(*rs1);
                    }
                    MachineInstData::FloatBinary { rd, rs1, rs2, .. } => {
                        defs.insert(*rd);
                        uses.insert(*rs1);
                        uses.insert(*rs2);
                    }
                    MachineInstData::FloatMulAdd {
                        rd, rs1, rs2, rs3, ..
                    } => {
                        defs.insert(*rd);
                        uses.insert(*rs1);
                        uses.insert(*rs2);
                        uses.insert(*rs3);
                    }
                    MachineInstData::FloatUnary { rd, rs, .. } => {
                        defs.insert(*rd);
                        uses.insert(*rs);
                    }
                    MachineInstData::Li { rd, .. } => {
                        defs.insert(*rd);
                    }
                    MachineInstData::Ret => {}
                    MachineInstData::Call { .. } => {
                        for reg in ARGUMENT_REGISTERS.iter() {
                            uses.insert(*reg);
                        }
                        for reg in RETURN_REGISTERS.iter() {
                            defs.insert(*reg);
                        }
                    }
                    MachineInstData::Branch { rs1, rs2, .. } => {
                        uses.insert(*rs1);
                        uses.insert(*rs2);
                    }
                    MachineInstData::J { .. } => {}
                }
            }

            block_defuse.uses.insert(block, uses);
            block_defuse.defs.insert(block, defs);
        }

        Ok(block_defuse)
    }
}

#[cfg(test)]
mod test {
    use std::io::Cursor;

    use crate::{
        backend::{
            passes::{block_defsue_analysis::DefUseAnalysis, LocalPass},
            MachineSymbol,
        },
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
        codegen_ctx.codegen(&module);

        println!("{:}", codegen_ctx.machine_ctx);

        let mut dua = DefUseAnalysis {};

        let func = codegen_ctx
            .machine_ctx
            .functions
            .get(&MachineSymbol("check_positive".to_string()))
            .unwrap();
        let block_defuse = dua.run_on_function(&codegen_ctx.machine_ctx, func).unwrap();

        println!("{:#?}", block_defuse);

        assert_eq!(block_defuse.uses.len(), 4);
        assert_eq!(block_defuse.defs.len(), 4);
    }
}
