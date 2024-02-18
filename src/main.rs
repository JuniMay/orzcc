use ir::{
    builder::{ConstantBuilder, GlobalValueBuilder, LocalBlockBuilder, LocalValueBuilder},
    module::Module,
    pass::GlobalPass,
    types::Type,
    values::{BinaryOp, ICmpCond},
};
use passes::printer::Printer;
use std::io;

pub mod collections;
pub mod ir;
pub mod passes;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut module = Module::new("fibonacci".to_string());

    let fn_fib = module.builder().function_def(
        "fib".to_string(),
        Type::mk_function(vec![Type::mk_i32()], Type::mk_i32()),
    )?;

    let dfg = module.function_data_mut(fn_fib).unwrap().dfg_mut();
    let mut dfg_builder = dfg.builder();

    let entry_block_param = dfg_builder.block_param(Type::mk_i32())?;
    let ret_block_param = dfg_builder.block_param(Type::mk_i32())?;
    let else_block_param = dfg_builder.block_param(Type::mk_i32())?;

    let entry_block = dfg_builder.block(vec![entry_block_param])?;
    let ret_block = dfg_builder.block(vec![ret_block_param])?;
    let else_block = dfg_builder.block(vec![else_block_param])?;

    let one0 = dfg_builder.integer(1)?;
    let one1 = dfg_builder.integer(1)?;
    let one2 = dfg_builder.integer(1)?;
    let two0 = dfg_builder.integer(2)?;

    let icmp = dfg_builder.binary(BinaryOp::ICmp(ICmpCond::Sle), entry_block_param, one0)?;
    let br = dfg_builder.branch(
        icmp,
        ret_block,
        else_block,
        vec![one1],
        vec![entry_block_param],
    )?;

    let sub0 = dfg_builder.binary(BinaryOp::Sub, else_block_param, one2)?;
    let sub1 = dfg_builder.binary(BinaryOp::Sub, else_block_param, two0)?;
    let call0 = dfg_builder.call(Type::mk_i32(), fn_fib.into(), vec![sub0])?;
    let call1 = dfg_builder.call(Type::mk_i32(), fn_fib.into(), vec![sub1])?;
    let add = dfg_builder.binary(BinaryOp::Add, call0, call1)?;
    let jump = dfg_builder.jump(ret_block, vec![add])?;

    let ret = dfg_builder.return_(Some(ret_block_param))?;

    dfg.assign_block_name(entry_block, "entry".to_string())?;
    dfg.assign_block_name(ret_block, "ret".to_string())?;
    dfg.assign_block_name(else_block, "else".to_string())?;

    dfg.assign_local_value_name(icmp, "cond".to_string())?;
    dfg.assign_local_value_name(ret_block_param, "result".to_string())?;

    let layout = module.function_data_mut(fn_fib).unwrap().layout_mut();

    layout.append_block(entry_block)?;
    layout.append_block(else_block)?;
    layout.append_block(ret_block)?;

    layout.append_inst(icmp.into(), entry_block)?;
    layout.append_inst(br.into(), entry_block)?;

    layout.append_inst(sub0.into(), else_block)?;
    layout.append_inst(sub1.into(), else_block)?;
    layout.append_inst(call0.into(), else_block)?;
    layout.append_inst(call1.into(), else_block)?;
    layout.append_inst(add.into(), else_block)?;
    layout.append_inst(jump.into(), else_block)?;

    layout.append_inst(ret.into(), ret_block)?;

    let mut stdout = io::stdout();
    let mut printer = Printer::new(&mut stdout);

    printer.run(&mut module)?;

    Ok(())
}
