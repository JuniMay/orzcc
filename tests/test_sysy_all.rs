#![cfg(feature = "frontend-sysy")]

use std::path::Path;

#[allow(dead_code)]
fn compile_src(path: &str) {
    use orzcc::{
        backend::{riscv64::lower::RvLowerSpec, LowerConfig, LowerContext},
        frontend::sysy::{self, SysYParser},
    };

    let src = std::fs::read_to_string(path).unwrap();
    let src = sysy::preprocess(&src);

    let mut ast = SysYParser::new().parse(&src).unwrap();

    ast.type_check();
    let mut ir = sysy::irgen(&ast);

    ir.alloc_all_names();
    let mut lower_ctx: LowerContext<RvLowerSpec> = LowerContext::new(&ir, LowerConfig::default());
    lower_ctx.lower();
    lower_ctx.reg_alloc();
    lower_ctx.after_regalloc();
    let mctx = lower_ctx.finish();

    println!("{}", mctx.display());
}

#[allow(dead_code)]
fn visit_dir(dir: &Path) {
    if dir.is_dir() {
        for entry in std::fs::read_dir(dir).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.is_dir() {
                visit_dir(&path);
            } else {
                let path = path.to_str().unwrap();
                if path.ends_with(".sy") {
                    println!("compiling: {}", path);
                    compile_src(path);
                }
            }
        }
    }
}

#[test]
fn test_compile_sysy_all() {
    // let path = "./tests/sysy/";
    // visit_dir(Path::new(path));
}
