use std::vec;

use ir::{builder::Builder, layout::LayoutOpErr, module::Module, types::Type};

use crate::ir::{
    entities::{BinaryOp, ConstantData},
    layout::Layout,
    printer::Printer,
};

pub mod backend;
pub mod collections;
pub mod ir;

fn main() -> Result<(), LayoutOpErr> {
    let mut module = Module::new();
    let mut layout = Layout::new();
    let mut builder = Builder::new(&mut module, &mut layout);

    let putchar = builder.create_fn(
        "putchar".to_string(),
        vec![Type::mk_int(32)],
        Type::mk_void(),
        true,
    );
    let func = builder.create_fn(String::from("test_func"), vec![], Type::mk_int(32), false);

    let entry_bb = builder.create_block(vec![]);

    let param_0 = builder.create_param(Type::mk_int(32));
    let param_1 = builder.create_param(Type::mk_float());

    let then_bb = builder.create_block(vec![param_0, param_1]);

    let param_2 = builder.create_param(Type::mk_int(32));
    let param_3 = builder.create_param(Type::mk_int(64));
    let param_4 = builder.create_param(Type::mk_float());

    let else_bb = builder.create_block(vec![param_2, param_3, param_4]);

    let param_5 = builder.create_param(Type::mk_int(32));

    let exit_bb = builder.create_block(vec![param_5]);

    builder
        .append_function(putchar.into())?
        .append_function(func.into())?
        .set_curr_fn(func)
        .append_block(entry_bb.into())?
        .append_block(then_bb.into())?
        .append_block(else_bb.into())?
        .append_block(exit_bb.into())?;

    let constant_0 = builder.create_constant(ConstantData::mk_bytes(Type::mk_int(8), vec![0]));

    let constant_1 = builder.create_constant(ConstantData::mk_bytes(Type::mk_int(32), vec![1]));
    let constant_2 = builder.create_constant(ConstantData::mk_bytes(
        Type::mk_float(),
        vec![0xff, 0xff, 0xff, 0xff],
    ));
    let constant_3 = builder.create_constant(ConstantData::mk_bytes(
        Type::mk_int(64),
        vec![0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff],
    ));

    let entry_conbr = builder.mk_condbr(
        constant_0,
        then_bb.into(),
        else_bb.into(),
        vec![constant_1.into(), constant_2.into()],
        vec![constant_1.into(), constant_3.into(), constant_2.into()],
    );

    let then_add = builder.mk_binary(BinaryOp::Add, param_0, param_0);
    let then_br = builder.mk_br(exit_bb.into(), vec![then_add.into()]);

    let else_mul = builder.mk_binary(BinaryOp::Mul, param_2, param_2);
    let else_br = builder.mk_br(exit_bb.into(), vec![else_mul.into()]);

    let ret = builder.mk_ret(param_5.into());

    builder
        .set_curr_block(entry_bb.into())
        .append_inst(entry_conbr.into())?
        .set_curr_block(then_bb.into())
        .append_inst(then_add.into())?
        .append_inst(then_br.into())?
        .set_curr_block(else_bb.into())
        .append_inst(else_mul.into())?
        .append_inst(else_br.into())?
        .set_curr_block(exit_bb.into())
        .append_inst(ret.into())?;

    module
        .assign_block_name(entry_bb.into(), "entry".to_string())
        .ok();
    module
        .assign_block_name(else_bb.into(), "else".to_string())
        .ok();

    module.allocate_name(&layout);

    let printer = Printer::new(&module, &layout);

    println!("{}", printer.emit_module());

    Ok(())
}
