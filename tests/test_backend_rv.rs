use orzcc::{
    backend::{
        riscv64::{
            imm::Imm12,
            inst::{AluOpRRI, RvInst},
            lower::RvLowerSpec,
            regs::sp,
        },
        LowerConfig,
        LowerContext,
        MBlock,
        MContext,
        MFunc,
        RawData,
    },
    collections::{diagnostic::RenderOptions, linked_list::LinkedListContainerPtr},
    frontend::ir::{into_ir, Parser},
    ir::{Context, Signature, Ty},
};

#[test]
fn test_backend_rv_basic() {
    let mut ctx = Context::default();
    let mut mctx = MContext::default();

    let void = Ty::void(&mut ctx);
    let sig = Signature::new(vec![], vec![void]);

    let func = MFunc::new(&mut mctx, "test", sig);
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

#[test]
fn test_backend_rv_fib() {
    let src = include_str!("ir/fibonacci.orzir");
    let parser = Parser::new(src);
    let (ast, mut ctx, mut diag) = parser.parse();

    if into_ir(ast, &mut ctx, &mut diag).is_some() {
        println!("{}", ctx.display(true));
    } else {
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
        panic!("test failed");
    }
    let mut lower_ctx: LowerContext<RvLowerSpec> = LowerContext::new(
        &ctx,
        LowerConfig {
            omit_frame_pointer: true,
        },
    );
    lower_ctx.lower();
    // TODO: regalloc
    lower_ctx.after_regalloc();
    let mctx = lower_ctx.finish();
    println!("{}", mctx.display());
}
