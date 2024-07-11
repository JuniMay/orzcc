use orzcc::{
    backend::{
        reg_alloc::graph_coloring_allocation::GraphColoringAllocation,
        riscv64::lower::RvLowerSpec,
        LowerConfig,
        LowerContext,
        MFunc,
    },
    collections::diagnostic::RenderOptions,
    frontend::ir::{into_ir, Parser},
};

#[test]
fn test_regalloc_graphcoloring() {
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

    let funcs: Vec<MFunc<_>> = lower_ctx
        .funcs
        .values()
        .filter(|f| !f.is_external(lower_ctx.mctx()))
        .copied()
        .collect();

    for func in funcs {
        println!("Function: {}", func.label(lower_ctx.mctx()));
        let mut allocation = GraphColoringAllocation::new();
        allocation.run_on_function(&mut lower_ctx, func);
    }

    lower_ctx.after_regalloc();

    let mctx = lower_ctx.finish();

    println!("{}", mctx.display());
}
