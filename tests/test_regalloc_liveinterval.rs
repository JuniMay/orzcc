use orzcc::{
    backend::{
        reg_alloc::live_interval_analysis,
        riscv64::lower::RvLowerSpec,
        LowerConfig,
        LowerContext,
    },
    collections::diagnostic::RenderOptions,
    frontend::ir::{into_ir, Parser},
};

#[test]
fn test_regalloc_liveinterval() {
    let src = include_str!("ir/basic.orzir");
    let parser = Parser::new(src);
    let (ast, mut ctx, mut diag) = parser.parse();

    if into_ir(ast, &mut ctx, &mut diag).is_some() {
        println!("{}", ctx.display(true));
    } else {
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
        panic!("test failed");
    }

    let lower_config = LowerConfig::default();

    let mut lower_ctx: LowerContext<RvLowerSpec> = LowerContext::new(&ctx, lower_config);

    lower_ctx.lower();

    println!("{}", lower_ctx.mctx().display());

    for (func_name, m_func) in lower_ctx.funcs.iter() {
        if !m_func.is_external(lower_ctx.mctx()) {
            println!("Function: {}", func_name);
            let live_intervals: live_interval_analysis::LiveInterval =
                live_interval_analysis::analyze_on_function(&lower_ctx, *m_func);
            println!(
                "{}",
                live_intervals.pretty_display::<RvLowerSpec>(&lower_ctx, *m_func)
            );
        }
    }
}
