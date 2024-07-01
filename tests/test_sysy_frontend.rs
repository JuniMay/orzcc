#![cfg(feature = "frontend-sysy")]

use orzcc::frontend::sysy::{self, SysYParser};

#[test]
fn test_sysy_frontend_0() {
    // long_code2 will overflow when testing, see:
    // https://stackoverflow.com/questions/42955243/cargo-test-release-causes-a-stack-overflow-why-doesnt-cargo-bench
    // it works fine in release mode with independent main executable.
    //
    // Maybe we can do some common subexpression elimination on the AST to reduce
    // the depth.
    let src = include_str!("sysy/functional/85_long_code.sy");
    let src = sysy::preprocess(src);
    let mut ast = SysYParser::new().parse(&src).unwrap();
    ast.type_check();
    println!("{:#?}", ast);
}

#[test]
fn test_sysy_frontend_1() {
    let src = include_str!("sysy/functional/04_arr_defn3.sy");
    let src = sysy::preprocess(src);
    let mut ast = SysYParser::new().parse(&src).unwrap();
    ast.type_check();
    println!("{:#?}", ast);
}

#[test]
fn test_sysy_frontend_80_chaos_token() {
    let src = include_str!("sysy/functional/80_chaos_token.sy");
    let src = sysy::preprocess(src);
    let mut ast = SysYParser::new().parse(&src).unwrap();
    ast.type_check();
    println!("{:#?}", ast);
}
