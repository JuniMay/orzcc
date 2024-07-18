use orzcc::{
    collections::diagnostic::RenderOptions,
    frontend::ir::{into_ir, Parser},
    ir::function_analysis::FunctionAnalysis,
};

#[test]
fn test_alias_analysis() {
    let src = include_str!("ir/sysy_03_sort1_snapshot.orzir");
    let parser = Parser::new(src);
    let (ast, mut ctx, mut diag) = parser.parse();

    if into_ir(ast, &mut ctx, &mut diag).is_some() {
        println!("{}", ctx.display(true));
    } else {
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
        panic!("test failed");
    }

    let mut fa = FunctionAnalysis::new();
    fa.analyze_all(&ctx);

    for func in ctx.funcs() {
        let is_pure = fa.is_pure(func);
        println!("Function: {:?}, is_pure: {:?}", func, is_pure);
    }
}
