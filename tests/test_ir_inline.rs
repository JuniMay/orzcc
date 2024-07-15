use orzcc::{
    collections::diagnostic::RenderOptions,
    frontend::ir::{into_ir, Parser},
    ir::{
        passes::inline::{Inline, INLINE},
        passman::{PassManager, TransformPass},
    },
};

#[test]
fn test_ir_inline() {
    let src = include_str!("ir/sysy_51_short_circuit3_snapshot.orzir");
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
    Inline::register(&mut passman);

    assert_eq!(passman.run_transform(INLINE, &mut ctx, 1), 1);

    ctx.alloc_all_names();
    println!("{}", ctx.display(true));
}
