use orzcc::{
    collections::diagnostic::RenderOptions,
    frontend::ir::{into_ir, Parser},
    ir::{
        passes::control_flow::{CfgSimplify, CFG_SIMPLIFY},
        passman::{PassManager, TransformPass},
    },
};

#[test]
fn test_ir_cfg_simplify_unreachable() {
    let src = include_str!("ir/cfg_simplify_unreachable.orzir");
    let parser = Parser::new(src);
    let (ast, mut ctx, mut diag) = parser.parse();

    if !diag.is_empty() {
        println!("{:#?}", ast);
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
        panic!("ast failed");
    }

    if into_ir(ast, &mut ctx, &mut diag).is_some() {
        println!("{}", ctx.display(true));
    } else {
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
        panic!("conversion failed");
    }

    let mut passman = PassManager::default();

    CfgSimplify::register(&mut passman);

    assert_eq!(passman.run_transform(CFG_SIMPLIFY, &mut ctx, 32), 4);

    println!("{}", ctx.display(true));
}
