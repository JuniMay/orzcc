use orzcc::{
    self,
    ir::{frontend::parser::Parser, pass::GlobalPass},
    passes::printer::Printer,
};
use std::io::{self, Cursor};

fn test_ir(ir: &str) {
    let mut buf = Cursor::new(ir);
    let mut parser = Parser::new(&mut buf);
    let ast = parser.parse().unwrap();
    println!("{:#?}", ast);
    let mut module = ast.into_ir("test".into()).unwrap();
    let mut stdout = io::stdout();
    let mut printer = Printer::new(&mut stdout);
    printer.run(&mut module).unwrap();
}

#[test]
fn test_00() {
    let ir = include_str!("ir_cases/00_arithmetic.orzir");
    test_ir(ir);
}

#[test]
fn test_01() {
    let ir = include_str!("ir_cases/01_cond_branch.orzir");
    test_ir(ir);
}

#[test]
fn test_02() {
    let ir = include_str!("ir_cases/02_loop.orzir");
    test_ir(ir);
}

#[test]
fn test_03() {
    let ir = include_str!("ir_cases/03_call.orzir");
    test_ir(ir);
}

#[test]
fn test_04() {
    let ir = include_str!("ir_cases/04_rec.orzir");
    test_ir(ir);
}

#[test]
fn test_05() {
    let ir = include_str!("ir_cases/05_empty_block.orzir");
    test_ir(ir);
}

#[test]
fn test_06() {
    let ir = include_str!("ir_cases/06_cast.orzir");
    test_ir(ir);
}

#[test]
fn test_07() {
    let ir = include_str!("ir_cases/07_types.orzir");
    test_ir(ir);
}

#[test]
fn test_08() {
    let ir = include_str!("ir_cases/08_zero.orzir");
    test_ir(ir);
}

#[test]
fn test_09() {
    let ir = include_str!("ir_cases/09_extratest.orzir");
    test_ir(ir);
}

#[test]
fn test_10() {
    let ir = include_str!("ir_cases/10_complex.orzir");
    test_ir(ir);
}

#[test]
fn test_11() {
    let ir = include_str!("ir_cases/11_many_labels.orzir");
    test_ir(ir);
}

#[test]
fn test_12() {
    let ir = include_str!("ir_cases/12_casts.orzir");
    test_ir(ir);
}

#[test]
fn test_13() {
    let ir = include_str!("ir_cases/13_func_ptr.orzir");
    test_ir(ir);
}
