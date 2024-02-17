use std::io;

use ir::{
    builder::{ConstantBuilder, GlobalValueBuilder, LocalBlockBuilder, LocalValueBuilder},
    module::Module,
    pass::GlobalPass,
    types::Type,
    values::{BinaryOp, ICmpCond},
};
use passes::printer::Printer;

pub mod collections;
pub mod ir;
pub mod passes;

fn main() -> io::Result<()> {
    let mut module = Module::new("fibonacci".to_string());

    let fn_fib = module
        .builder()
        .function_def(
            "fib".to_string(),
            Type::mk_function(vec![Type::mk_int(32)], Type::mk_int(32)),
        )
        .unwrap();

    let dfg = module.function_data_mut(fn_fib).unwrap().dfg_mut();

    let entry_block_param = dfg.builder().block_param(Type::mk_int(32)).unwrap();
    let ret_block_param = dfg.builder().block_param(Type::mk_int(32)).unwrap();
    let else_block_param = dfg.builder().block_param(Type::mk_int(32)).unwrap();

    let entry_block = dfg.builder().block(vec![entry_block_param.into()]).unwrap();
    let ret_block = dfg.builder().block(vec![ret_block_param.into()]).unwrap();
    let else_block = dfg.builder().block(vec![else_block_param]).unwrap();

    let one0 = dfg.builder().bytes(Type::mk_int(32), vec![1]).unwrap();
    let one1 = dfg.builder().bytes(Type::mk_int(32), vec![1]).unwrap();
    let one2 = dfg.builder().bytes(Type::mk_int(32), vec![1]).unwrap();
    let two0 = dfg.builder().bytes(Type::mk_int(32), vec![2]).unwrap();

    let icmp = dfg
        .builder()
        .binary(
            BinaryOp::ICmp(ICmpCond::Sle),
            entry_block_param,
            one0.into(),
        )
        .unwrap();
    let br = dfg
        .builder()
        .branch(
            icmp,
            ret_block,
            else_block,
            vec![one1],
            vec![entry_block_param],
        )
        .unwrap();

    let sub0 = dfg
        .builder()
        .binary(BinaryOp::Sub, else_block_param, one2.into())
        .unwrap();
    let sub1 = dfg
        .builder()
        .binary(BinaryOp::Sub, else_block_param, two0.into())
        .unwrap();
    let call0 = dfg
        .builder()
        .call(Type::mk_int(32), fn_fib.into(), vec![sub0.into()])
        .unwrap();
    let call1 = dfg
        .builder()
        .call(Type::mk_int(32), fn_fib.into(), vec![sub1.into()])
        .unwrap();
    let add = dfg
        .builder()
        .binary(BinaryOp::Add, call0.into(), call1.into())
        .unwrap();
    let jump = dfg.builder().jump(ret_block, vec![add.into()]).unwrap();

    let ret = dfg.builder().return_(Some(ret_block_param)).unwrap();

    dfg.assign_block_name(entry_block, "entry".to_string()).ok();
    dfg.assign_block_name(ret_block, "ret".to_string()).ok();
    dfg.assign_block_name(else_block, "else".to_string()).ok();

    dfg.assign_local_value_name(icmp, "cond".to_string()).ok();
    dfg.assign_local_value_name(ret_block_param, "result".to_string())
        .ok();

    let layout = module.function_data_mut(fn_fib).unwrap().layout_mut();

    layout.append_block(entry_block).ok();
    layout.append_block(else_block).ok();
    layout.append_block(ret_block).ok();

    layout.append_inst(icmp.into(), entry_block).ok();
    layout.append_inst(br.into(), entry_block).ok();

    layout.append_inst(sub0.into(), else_block).ok();
    layout.append_inst(sub1.into(), else_block).ok();
    layout.append_inst(call0.into(), else_block).ok();
    layout.append_inst(call1.into(), else_block).ok();
    layout.append_inst(add.into(), else_block).ok();
    layout.append_inst(jump.into(), else_block).ok();

    layout.append_inst(ret.into(), ret_block).ok();

    let mut stdout = io::stdout();
    let mut printer = Printer::new(&mut stdout);

    printer.run(&mut module);

    Ok(())
}
