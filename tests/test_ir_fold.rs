use orzcc::{
    collections::diagnostic::RenderOptions,
    frontend::ir::{into_ir, Parser},
    ir::{passes::fold::ConstantFolding, passman::GlobalPassMut},
};

#[test]
fn test_ir_mem2reg_basic() {
    let src = include_str!("ir/sysy_00_bitset_snapshot.orzir");
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

    let mut fold = ConstantFolding::default();

    match GlobalPassMut::run(&mut fold, &mut ctx) {
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
