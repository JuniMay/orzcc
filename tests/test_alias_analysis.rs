use orzcc::{
    collections::{diagnostic::RenderOptions, linked_list::LinkedListContainerPtr},
    frontend::ir::{into_ir, Parser},
    ir::alias_analysis::AliasAnalysis,
};

#[test]
fn test_alias_analysis() {
    let src = include_str!("ir/alias_analysis.orzir");
    let parser = Parser::new(src);
    let (ast, mut ctx, mut diag) = parser.parse();

    if into_ir(ast, &mut ctx, &mut diag).is_some() {
        println!("{}", ctx.display(true));
    } else {
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
        panic!("test failed");
    }

    for func in ctx.funcs() {
        let mut pointers = Vec::new();
        for block in func.iter(&ctx) {
            for inst in block.iter(&ctx) {
                if inst.result(&ctx, 0).ty(&ctx).is_ptr(&ctx) {
                    pointers.push(inst.result(&ctx, 0));
                }
            }
        }

        // test all pointer pairs
        for a in &pointers {
            for b in &pointers {
                let result = AliasAnalysis::analyze(&ctx, *a, *b, true);
                println!("{:?} {:?} {:}", a, b, result);
            }
        }
    }
}
