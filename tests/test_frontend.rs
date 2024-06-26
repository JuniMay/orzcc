use std::io::{BufWriter, Cursor};

use orzcc::{
    self,
    ir::{
        frontend::parser::Parser,
        passes::{printer::Printer, GlobalPass},
    },
};

fn print(module: &orzcc::ir::module::Module) {
    let mut buf = BufWriter::new(Vec::new());
    let mut printer = Printer::new(&mut buf);
    printer.run_on_module(module).unwrap();
    let s = String::from_utf8(buf.into_inner().unwrap()).unwrap();
    println!("{}", s);
}

fn test_ir(ir: &str) {
    let mut buf = Cursor::new(ir);
    let mut parser = Parser::new(&mut buf);
    let ast = parser.parse().unwrap();
    println!("{:#?}", ast);
    let module = ast.into_ir("test".into()).unwrap();
    print(&module);
}

#[test]
fn test_frontend_runnable_00() {
    let ir = include_str!("orzir/00_arithmetic.orzir");
    test_ir(ir);
}

#[test]
fn test_frontend_runnable_01() {
    let ir = include_str!("orzir/01_cond_branch.orzir");
    test_ir(ir);
}

#[test]
fn test_frontend_runnable_02() {
    let ir = include_str!("orzir/02_loop.orzir");
    test_ir(ir);
}

#[test]
fn test_frontend_runnable_03() {
    let ir = include_str!("orzir/03_call.orzir");
    test_ir(ir);
}

#[test]
fn test_frontend_runnable_04() {
    let ir = include_str!("orzir/04_rec.orzir");
    test_ir(ir);
}

#[test]
fn test_frontend_runnable_05() {
    let ir = include_str!("orzir/05_empty_block.orzir");
    test_ir(ir);
}

#[test]
fn test_frontend_runnable_06() {
    let ir = include_str!("orzir/06_cast.orzir");
    test_ir(ir);
}

#[test]
fn test_frontend_runnable_07() {
    let ir = include_str!("orzir/07_types.orzir");
    test_ir(ir);
}

#[test]
fn test_frontend_runnable_08() {
    let ir = include_str!("orzir/08_zero.orzir");
    test_ir(ir);
}

#[test]
fn test_frontend_runnable_09() {
    let ir = include_str!("orzir/09_extratest.orzir");
    test_ir(ir);
}

#[test]
fn test_frontend_runnable_10() {
    let ir = include_str!("orzir/10_complex.orzir");
    test_ir(ir);
}

#[test]
fn test_frontend_runnable_11() {
    let ir = include_str!("orzir/11_many_labels.orzir");
    test_ir(ir);
}

#[test]
fn test_frontend_runnable_12() {
    let ir = include_str!("orzir/12_casts.orzir");
    test_ir(ir);
}

#[test]
fn test_frontend_runnable_13() {
    let ir = include_str!("orzir/13_func_ptr.orzir");
    test_ir(ir);
}
