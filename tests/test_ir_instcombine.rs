use orzcc::{
    collections::diagnostic::RenderOptions,
    frontend::ir::{into_ir, Parser},
    ir::{
        passes::instcombine::{InstCombine, INSTCOMBINE},
        passman::{PassManager, TransformPass},
    },
};

#[test]
fn test_ir_instcombine_mul_to_shl() {
    let src = include_str!("ir/instcombine_mul_to_shl.orzir");
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

    InstCombine::register(&mut passman);

    assert_eq!(passman.run_transform(INSTCOMBINE, &mut ctx, 1), 1);

    ctx.alloc_all_names();
    println!("{}", ctx.display(true));
}

#[test]
fn test_ir_instcombine_mv_const_rhs() {
    let src = include_str!("ir/instcombine_mv_const_rhs.orzir");
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

    InstCombine::register(&mut passman);

    assert_eq!(passman.run_transform(INSTCOMBINE, &mut ctx, 1), 1);

    ctx.alloc_all_names();
    println!("{}", ctx.display(true));
}

#[test]
fn test_ir_instcombine_add_zero_elim() {
    let src = include_str!("ir/instcombine_add_zero_elim.orzir");
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

    InstCombine::register(&mut passman);

    assert_eq!(passman.run_transform(INSTCOMBINE, &mut ctx, 3), 2);

    ctx.alloc_all_names();
    println!("{}", ctx.display(true));
}
