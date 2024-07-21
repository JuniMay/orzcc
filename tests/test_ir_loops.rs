use orzcc::{
    collections::diagnostic::RenderOptions,
    frontend::ir::{into_ir, Parser},
    ir::{
        passes::loops::{
            Lcssa,
            LoopInvariantMotion,
            LoopSimplify,
            ScevAnalysis,
            LCSSA,
            LOOP_INVARIANT_MOTION,
            LOOP_SIMPLIFY,
        },
        passman::{GlobalPass, PassManager, TransformPass},
    },
};

#[test]
fn test_ir_loop_invariant_motion() {
    let src = include_str!("ir/loop_invariant_basic.orzir");
    let parser = Parser::new(src);
    let (ast, mut ctx, mut diag) = parser.parse();

    if !diag.is_empty() {
        println!("{:#?}", ast);
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
        panic!("ast failed");
    }

    if into_ir(ast, &mut ctx, &mut diag).is_none() {
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
        panic!("conversion failed");
    }

    let mut passman = PassManager::default();
    LoopInvariantMotion::register(&mut passman);

    assert_eq!(passman.run_transform(LOOP_INVARIANT_MOTION, &mut ctx, 1), 1);

    ctx.alloc_all_names();
    println!("{}", ctx.display(true));
}

#[test]
fn test_ir_loop_simplify() {
    let src = include_str!("ir/loop_simplify.orzir");
    let parser = Parser::new(src);
    let (ast, mut ctx, mut diag) = parser.parse();

    if !diag.is_empty() {
        println!("{:#?}", ast);
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
        panic!("ast failed");
    }

    if into_ir(ast, &mut ctx, &mut diag).is_none() {
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
        panic!("conversion failed");
    }

    let mut passman = PassManager::default();
    LoopSimplify::register(&mut passman);

    assert_eq!(passman.run_transform(LOOP_SIMPLIFY, &mut ctx, 1), 1);

    ctx.alloc_all_names();
    println!("{}", ctx.display(true));
}

#[test]
fn test_ir_lcssa() {
    let src = include_str!("ir/lcssa.orzir");
    let parser = Parser::new(src);
    let (ast, mut ctx, mut diag) = parser.parse();

    if !diag.is_empty() {
        println!("{:#?}", ast);
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
        panic!("ast failed");
    }

    if into_ir(ast, &mut ctx, &mut diag).is_none() {
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
        panic!("conversion failed");
    }

    println!("{}", ctx.display(true));

    let mut passman = PassManager::default();
    Lcssa::register(&mut passman);

    assert_eq!(passman.run_transform(LCSSA, &mut ctx, 1), 1);

    ctx.alloc_all_names();
    println!("{}", ctx.display(true));
}

#[test]
fn test_ir_loop_scev() {
    let src = include_str!("ir/loop_scev.orzir");
    let parser = Parser::new(src);
    let (ast, mut ctx, mut diag) = parser.parse();

    if !diag.is_empty() {
        println!("{:#?}", ast);
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
        panic!("ast failed");
    }

    if into_ir(ast, &mut ctx, &mut diag).is_none() {
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
        panic!("conversion failed");
    }

    println!("{}", ctx.display(true));

    let mut scev = ScevAnalysis::default();

    let indvars = GlobalPass::run(&mut scev, &ctx).unwrap();

    for (func, indvars) in indvars {
        println!("function: {}", func.name(&ctx));
        for indvar in indvars {
            println!("indvar: {}", indvar.display(&ctx));
        }
    }
}
