use std::io;

use ir::{
    builder::{GlobalValueBuilder, LocalBlockBuilder, LocalValueBuilder},
    module::Module,
    pass::GlobalPass,
    types::Type,
};
use passes::printer::Printer;

pub mod collections;
pub mod ir;
pub mod passes;

fn main() -> io::Result<()> {
    let mut module = Module::new("test".to_string());
    let function = module
        .builder()
        .function_def(
            "test_func".to_string(),
            Type::mk_function(vec![], Type::mk_void()),
        )
        .unwrap();

    let function_data = module.function_data_mut(function).unwrap();

    let block = function_data.dfg_mut().builder().block(vec![]).unwrap();

    let dfg = function_data.dfg_mut();

    let alloc0 = dfg.builder().alloc(Type::mk_int(32)).unwrap();
    let alloc1 = dfg.builder().alloc(Type::mk_float()).unwrap();
    let alloc2 = dfg.builder().alloc(Type::mk_double()).unwrap();

    let layout = function_data.layout_mut();

    layout.append_block(block).ok();
    layout.append_inst(alloc0.into(), block).ok();
    layout.append_inst(alloc1.into(), block).ok();
    layout.append_inst(alloc2.into(), block).ok();

    let mut stdout = io::stdout();
    let mut printer = Printer::new(&mut stdout);

    printer.run(&mut module);

    Ok(())
}
