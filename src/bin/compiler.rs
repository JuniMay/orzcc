//! The compiler executable.

use orzcc::frontend::sysy::{self, SysYParser};

fn main() {
    // stack overflow on debug mode, but release mode is fine
    let src = include_str!("../../tests/sysy/functional/86_long_code2.sy");
    let src = sysy::preprocess(src);
    let mut ast = SysYParser::new().parse(&src).unwrap();

    println!("parse done");

    ast.type_check();

    println!("type check done");

    let mut ir = sysy::irgen(&ast);

    println!("irgen done");

    ir.alloc_all_names();
    println!("{}", ir.display(true));
}
