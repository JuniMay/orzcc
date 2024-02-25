use std::io::Cursor;

use orzcc::ir::{exec::interpreter::Interpreter, frontend::parser::Parser};


#[test]
fn test_00() {
    let ir = include_str!("ir_cases/00_arithmetic.orzir");
    let mut buf = Cursor::new(ir);
    let mut parser = Parser::new(&mut buf);
    let ast = parser.parse().unwrap();
    let module = ast.into_ir("test".into()).unwrap();

    let mut interpreter = Interpreter::new(&module);

    interpreter.run();
}