use orzcc::{
    self,
    ir::{frontend::parser::Parser, pass::GlobalPass},
    passes::printer::Printer,
};
use std::io::{self, Cursor};

#[test]
fn test_ast() {
    let mut buf = Cursor::new(
        r#"global @x = i32 0x10101010
const @y = i32 0x20202020
type $z = { i32, float }
global @array = [ i32; 3 ] [ 0x01, 0x02, 0x03 ]
global @arrarr = [ [float ;3]; 4] [ [0x1, 0x2, 0x3],[0x1, 0x2, 0x3],[0x1, 0x2, 0x3],[0x1, 0x2, 0x3]]
type $struct = { i32, [float; 3] }
global @s = [$struct ; 2] [ { 0x01, [0x1, 0x2, 0x3] }, { 0x02, [0x4, 0x5, 0x6] } ]

fn @fib(i32) -> i32 {

^entry(i32 %0):
    %cond = icmp.sle i32 %0, i32 1234
    br i1 %cond, ^ret(i32 0x01, float 0x123), ^else(i32 %0)

^else(i32 %1):
    %2 = sub i32 %1, i32 0x01
    %3 = sub i32 %1, i32 0x02
    %4 = call i32 @fib(i32 %2)
    %5 = call i32 @fib(i32 %3)
    %6 = add i32 %4, i32 %5
    jump ^ret(i32 %6, float 0x1234)

^ret(i32 %result, float %123):
    ret i32 %result
}
"#,
    );
    let mut parser = Parser::new(&mut buf);
    let ast = parser.parse().unwrap();
    let mut module = ast.into_ir("test".into()).unwrap();
    let mut stdout = io::stdout();
    let mut printer = Printer::new(&mut stdout);
    printer.run(&mut module).unwrap();
}


fn test_ir(str: &str) {
    let mut buf = Cursor::new(str);
    let mut parser = Parser::new(&mut buf);
    let ast = parser.parse().unwrap();
    let mut module = ast.into_ir("test".into()).unwrap();
    let mut stdout = io::stdout();
    let mut printer = Printer::new(&mut stdout);
    printer.run(&mut module).unwrap();
}

#[test]
fn test_ir_cases() {
    // get testcase str under `ir_cases` dir of the project
    let path = std::path::Path::new("tests/ir_cases");
    // traverse all files
    for entry in std::fs::read_dir(path).unwrap() {
        let entry = entry.unwrap();
        let path = entry.path();
        println!("path: {:?}", path);
        if path.is_file() {
            let str = std::fs::read_to_string(path).unwrap();
            test_ir(&str);
        }
    }
}