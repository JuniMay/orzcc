use orzcc::{
    collections::diagnostic::RenderOptions,
    frontend::ir::{into_ir, Parser},
};

#[test]
fn test_ir_parse_0() {
    let src = include_str!("ir/basic.orzir");
    let parser = Parser::new(src);
    let (ast, mut diag, mut ctx) = parser.parse();

    println!("{:#?}", ast);

    into_ir(ast, &mut ctx, &mut diag);

    println!("{}", diag.render(src, &RenderOptions::unicode_round()));
    println!("{}", ctx.display(true));
}
