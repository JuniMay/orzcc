use orzcc::{
    backend::{
        riscv64::{
            imm::Imm12,
            inst::{AluOpRRI, RvInst},
            regs::sp,
        },
        MBlock,
        MContext,
        MFunc,
        RawData,
    },
    collections::linked_list::LinkedListContainerPtr,
};

#[test]
fn test_backend_rv_basic() {
    let mut mctx = MContext::default();

    let func = MFunc::new(&mut mctx, "test");
    let block = MBlock::new(&mut mctx, ".entry");

    func.push_back(&mut mctx, block);

    let (addi, _rd) = RvInst::alu_rri(
        &mut mctx,
        AluOpRRI::Addi,
        sp().into(),
        Imm12::try_from_i64(-32).unwrap(),
    );

    block.push_back(&mut mctx, addi);

    mctx.add_raw_data("zeros", RawData::Bss(16));

    println!("{}", mctx.display());
}
