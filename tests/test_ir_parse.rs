use orzcc::{collections::diagnostic::RenderOptions, frontend::ir::Parser};

#[test]
fn test_ir_parse_0() {
    let src = include_str!("ir/basic.orzir");
    let parser = Parser::new(src);
    let (ast, mut diag, _ctx) = parser.parse();

    println!("{:#?}", ast);
    println!("{}", diag.render(src, &RenderOptions::unicode_round()));
}
