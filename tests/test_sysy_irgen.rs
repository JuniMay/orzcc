#![cfg(feature = "frontend-sysy")]

use orzcc::frontend::sysy::{self, SysYParser};

#[test]
fn test_sysy_irgen_80_chaos_token() {
    let src = include_str!("sysy/functional/80_chaos_token.sy");
    let src = sysy::preprocess(src);
    let mut ast = SysYParser::new().parse(&src).unwrap();
    ast.type_check();
    let mut ir = sysy::irgen(&ast);
    ir.alloc_all_names();
    println!("{}", ir.display(true));
}

#[test]
fn test_sysy_irgen_85_long_code() {
    let src = include_str!("sysy/functional/85_long_code.sy");
    let src = sysy::preprocess(src);
    let mut ast = SysYParser::new().parse(&src).unwrap();
    ast.type_check();
    let mut ir = sysy::irgen(&ast);
    ir.alloc_all_names();
    println!("{}", ir.display(true));
}

#[test]
fn test_sysy_irgen_95_float() {
    let src = include_str!("sysy/functional/95_float.sy");
    let src = sysy::preprocess(src);
    let mut ast = SysYParser::new().parse(&src).unwrap();
    ast.type_check();
    let mut ir = sysy::irgen(&ast);
    ir.alloc_all_names();
    println!("{}", ir.display(true));
}

#[test]
fn test_sysy_irgen_05_arr_defn4() {
    let src = include_str!("sysy/functional/05_arr_defn4.sy");
    let src = sysy::preprocess(src);
    let mut ast = SysYParser::new().parse(&src).unwrap();
    ast.type_check();
    let mut ir = sysy::irgen(&ast);
    ir.alloc_all_names();
    println!("{}", ir.display(true));
}
