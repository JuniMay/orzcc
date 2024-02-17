use std::io;

use ir::{
    builder::{GlobalValueBuilder, LocalBlockBuilder, LocalValueBuilder},
    entities::FunctionKind,
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
        .function(
            "test_func".to_string(),
            Type::mk_function(vec![], Type::mk_void()),
            FunctionKind::Definition,
        )
        .unwrap();

    let function_data = module.function_data_mut(function).unwrap();

    let block = function_data.dfg_mut().builder().block(vec![]).unwrap();

    let alloc0 = function_data
        .dfg_mut()
        .builder()
        .alloc(Type::mk_int(32))
        .unwrap();
    let alloc1 = function_data
        .dfg_mut()
        .builder()
        .alloc(Type::mk_float())
        .unwrap();
    let alloc2 = function_data
        .dfg_mut()
        .builder()
        .alloc(Type::mk_double())
        .unwrap();

    let _ = function_data.layout_mut().blocks_mut().append(block);

    let block_node = function_data
        .layout_mut()
        .blocks_mut()
        .node_mut(block)
        .unwrap();

    let _ = block_node.insts_mut().append(alloc0.into());
    let _ = block_node.insts_mut().append(alloc1.into());
    let _ = block_node.insts_mut().append(alloc2.into());

    let mut stdout = io::stdout();

    let mut printer = Printer::new(&mut stdout);

    printer.run(&mut module);

    Ok(())
}
