use orzcc::{
    backend::riscv64::regs::display, collections::diagnostic::RenderOptions, frontend::ir::{into_ir, Parser}, ir::{passes::global2local::*, passman::{GlobalPassMut, PassManager, TransformPass}}
};

#[test]
fn test_ir_global2local() {
    let src = include_str!("ir/global2local_basic.orzir");
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
    Global2Local::register(&mut passman);

    // -------------------- //

    let debug : bool = true;
    println!("{}", ctx.display(debug));

    let mut g2l = Global2Local::default();
    GlobalPassMut::run(&mut g2l, &mut ctx);

    println!("{}", ctx.display(debug));
}