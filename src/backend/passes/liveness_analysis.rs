use std::collections::{HashMap, HashSet};

use super::{
    block_defsue_analysis::DefUseAnalysis,
    control_flow_analysis::ControlFlowAnalysis,
    LocalPass,
    PassResult,
};
use crate::backend::{MachineBlock, MachineContext, MachineFunctionData, Register};

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

        // get post order traversal of the control flow graph
        let post_order = cfg.post_order(data.layout().entry_block().unwrap());

        let mut changed = true;

        while changed {
            changed = false;

            for block in post_order.iter() {
                let mut in_set: HashSet<Register> = HashSet::new();
                let mut out_set: HashSet<Register> = HashSet::new();

                // out[B] = U in[S] for all S in succ[B]
                for succ in cfg.succs(block).unwrap() {
                    out_set.extend(in_.get(succ).unwrap().iter().cloned());
                }

                // in[B] = use[B] U (out[B] - def[B])
                in_set.extend(defs_uses.uses(block).unwrap().iter().cloned());
                in_set.extend(out_set.difference(defs_uses.defs(block).unwrap()).cloned());

                if in_.get(block).unwrap() != &in_set {
                    in_.insert(*block, in_set);
                    changed = true;
                }

                if out.get(block).unwrap() != &out_set {
                    out.insert(*block, out_set);
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
        backend::{passes::LocalPass, MachineSymbol},
        codegen::CodegenContext,
        ir::frontend::parser::Parser,
    };

    #[test]
    fn test_inout() {
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
        let module = parser.parse().unwrap().into_ir("test".into()).unwrap();

        let mut codegen_ctx = CodegenContext::new();
        codegen_ctx.codegen(&module);

        println!("{:}", codegen_ctx.machine_ctx);

        let mut in_out = InOutAnalysis {};

        let func = codegen_ctx
            .machine_ctx
            .functions
            .get(&MachineSymbol("param32_rec".to_string()))
            .unwrap();

        let block_inout = in_out
            .run_on_function(&codegen_ctx.machine_ctx, func)
            .unwrap();

        for (block, _) in func.layout().blocks() {
            println!("Block: {:?}", block);
            println!("    IN: {:?}", block_inout.in_(&block));
            println!("    OUT: {:?}", block_inout.out(&block));
        }
    }
}
