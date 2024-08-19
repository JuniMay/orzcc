use orzcc::{
    self,
    backend::{riscv64::lower::RvLowerSpec, LowerConfig, LowerContext},
    collections::diagnostic::RenderOptions,
    frontend::ir::{into_ir, Parser},
};

#[test]
fn test_multithread() {
    let src = include_str!("ir/multithread.orzir");
    let parser = Parser::new(src);
    let (ast, mut ctx, mut diag) = parser.parse();

    if into_ir(ast, &mut ctx, &mut diag).is_some() {
        println!("{}", ctx.display(true));
    } else {
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
        panic!("test failed");
    }
    let mut lower_ctx: LowerContext<RvLowerSpec> = LowerContext::new(&ctx, LowerConfig::default());
    lower_ctx.mctx_mut().set_arch("rv64imafdc_zba_zbb");
    lower_ctx.lower();
    lower_ctx.reg_alloc();
    lower_ctx.after_regalloc();
    let mctx = lower_ctx.finish();
    println!("{}", mctx.display());
}
