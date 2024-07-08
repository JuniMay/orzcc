use orzcc::{
    collections::diagnostic::RenderOptions,
    frontend::ir::{into_ir, Parser},
    ir::{passes::control_flow::CfgSimplify, passman::GlobalPassMut},
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

    let mut cfg_simplify = CfgSimplify;

    match GlobalPassMut::run(&mut cfg_simplify, &mut ctx) {
        Ok((_, changed)) => {
            assert!(changed);
            ctx.alloc_all_names();
            println!("{}", ctx.display(true));
        }
        Err(e) => {
            panic!("test failed: {:?}", e);
        }
    }
}
