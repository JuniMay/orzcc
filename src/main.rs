use orzcc::ir::{exec::debugger::Debugger, frontend::parser::Parser};

fn main() {
    let filepath = "tests/ir_cases/00_arithmetic.orzir";
    let mut file = std::fs::File::open(filepath).unwrap();
    let mut parser = Parser::new(&mut file);
    let module = parser.parse().unwrap().into_ir("TEST".into()).unwrap();

    let mut debugger = Debugger::new(&module);

    debugger.repl();
}
