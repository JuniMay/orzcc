use std::collections::{HashMap, HashSet};

use super::{LocalPass, PassResult};
use crate::backend::{
    MachineBlock,
    MachineContext,
    MachineFunctionData,
    MachineInstData,
    Register,
    RiscvGpReg,
    RETURN_REGISTERS,
};

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
                        if !defs.contains(base) {
                            uses.insert(*base);
                        }
                        defs.insert(*dest);
                    }
                    MachineInstData::FloatLoad { dest, base, .. } => {
                        if !defs.contains(base) {
                            uses.insert(*base);
                        }
                        defs.insert(*dest);
                    }
                    MachineInstData::PseudoLoad { dest, .. } => {
                        defs.insert(*dest);
                    }
                    MachineInstData::PseudoStore { value, rt, .. } => {
                        if !defs.contains(value) {
                            uses.insert(*value);
                        }
                        defs.insert(*rt);
                    }
                    MachineInstData::FloatPseudoLoad { dest, rt, .. } => {
                        defs.insert(*dest);
                        defs.insert(*rt);
                    }
                    MachineInstData::FloatPseudoStore { value, rt, .. } => {
                        if !defs.contains(value) {
                            uses.insert(*value);
                        }
                        defs.insert(*rt);
                    }
                    MachineInstData::Store { value, base, .. } => {
                        if !defs.contains(base) {
                            uses.insert(*base);
                        }
                        if !defs.contains(value) {
                            uses.insert(*value);
                        }
                    }
                    MachineInstData::FloatStore { value, base, .. } => {
                        if !defs.contains(base) {
                            uses.insert(*base);
                        }
                        if !defs.contains(value) {
                            uses.insert(*value);
                        }
                    }
                    MachineInstData::FMv { rd, rs, .. } => {
                        if !defs.contains(rs) {
                            uses.insert(*rs);
                        }
                        defs.insert(*rd);
                    }
                    MachineInstData::FCvt { rd, rs, .. } => {
                        if !defs.contains(rs) {
                            uses.insert(*rs);
                        }
                        defs.insert(*rd);
                    }
                    MachineInstData::Binary { rd, rs1, rs2, .. } => {
                        if !defs.contains(rs1) {
                            uses.insert(*rs1);
                        }
                        if !defs.contains(rs2) {
                            uses.insert(*rs2);
                        }
                        defs.insert(*rd);
                    }
                    MachineInstData::BinaryImm { rd, rs1, .. } => {
                        if !defs.contains(rs1) {
                            uses.insert(*rs1);
                        }
                        defs.insert(*rd);
                    }
                    MachineInstData::FloatBinary { rd, rs1, rs2, .. } => {
                        if !defs.contains(rs1) {
                            uses.insert(*rs1);
                        }
                        if !defs.contains(rs2) {
                            uses.insert(*rs2);
                        }
                        defs.insert(*rd);
                    }
                    MachineInstData::FloatMulAdd {
                        rd, rs1, rs2, rs3, ..
                    } => {
                        if !defs.contains(rs1) {
                            uses.insert(*rs1);
                        }
                        if !defs.contains(rs2) {
                            uses.insert(*rs2);
                        }
                        if !defs.contains(rs3) {
                            uses.insert(*rs3);
                        }
                        defs.insert(*rd);
                    }
                    MachineInstData::FloatUnary { rd, rs, .. } => {
                        if !defs.contains(rs) {
                            uses.insert(*rs);
                        }
                        defs.insert(*rd);
                    }
                    MachineInstData::Li { rd, .. } => {
                        defs.insert(*rd);
                    }
                    MachineInstData::Ret => {}
                    MachineInstData::Call { .. } => {
                        // TODO: this might be wrong
                        // for reg in CALLER_SAVED_REGISTERS.iter() {
                        //     uses.insert(*reg);
                        // }
                        for reg in RETURN_REGISTERS.iter() {
                            defs.insert(*reg);
                        }
                    }
                    MachineInstData::Branch { rs1, rs2, .. } => {
                        if !defs.contains(rs1) {
                            uses.insert(*rs1);
                        }
                        if !defs.contains(rs2) {
                            uses.insert(*rs2);
                        }
                    }
                    MachineInstData::J { .. } => {}
                }
            }

            // remove gp.zero from uses
            uses.remove(&Register::General(RiscvGpReg::Zero));

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
        codegen::CodegenContext,
        ir::frontend::parser::Parser,
    };

    #[test]
    fn test_cfa() {
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
        let module = parser
            .parse()
            .unwrap()
            .into_ir("param32_rec".into())
            .unwrap();

        let mut codegen_ctx = CodegenContext::new();
        codegen_ctx.codegen(&module);

        println!("{:}", codegen_ctx.machine_ctx);

        let mut dua = DefUseAnalysis {};

        let func = codegen_ctx
            .machine_ctx
            .functions
            .get(&MachineSymbol("param32_rec".to_string()))
            .unwrap();
        let block_defuse = dua.run_on_function(&codegen_ctx.machine_ctx, func).unwrap();

        println!("{:#?}", block_defuse);

        // assert_eq!(block_defuse.uses.len(), 4);
        // assert_eq!(block_defuse.defs.len(), 4);
    }
}
