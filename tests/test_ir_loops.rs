use orzcc::{
    collections::diagnostic::RenderOptions,
    frontend::ir::{into_ir, Parser},
    ir::{passes::loops::LoopInvariantMotion, passman::GlobalPassMut},
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

    let mut lim = LoopInvariantMotion::default();

    match GlobalPassMut::run(&mut lim, &mut ctx) {
        Ok((_, changed)) => {
            assert!(changed);
            ctx.alloc_all_names();
            println!("{}", ctx.display(true));
        }
        Err(e) => {
            panic!("test failed: {:?}", e);
        }
    }

    match GlobalPassMut::run(&mut lim, &mut ctx) {
        Ok((_, changed)) => assert!(!changed),
        Err(e) => panic!("test failed: {:?}", e),
    }
}
