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

    let func = builder.create_fn(String::from("test_func"), vec![], Type::mk_int(32));
    let entry_bb = builder.create_block(vec![]);
    let exit_bb = builder.create_block(vec![]);

    let constant0 = builder.create_constant(ConstantData::mk_bytes(
        Type::mk_int(32),
        4114514i32
            .to_be_bytes()
            .to_vec()
            .into_iter()
            .rev()
            .collect(),
    ));

    let constant1 = builder.create_constant(ConstantData::mk_bytes(
        Type::mk_int(32),
        1919810i32
            .to_be_bytes()
            .to_vec()
            .into_iter()
            .rev()
            .collect(),
    ));

    let add = builder.mk_binary(BinaryOp::Add, constant0, constant1);

    let ret_inst = builder.mk_ret(add.into());

    let br_inst = builder.mk_br(exit_bb.into(), vec![]);

    builder
        .append_function(func.into())?
        .set_curr_fn(func)
        .append_block(entry_bb.into())?
        .set_curr_block(entry_bb.into())
        .append_inst(add.into())?
        .append_inst(br_inst.into())?
        .append_block(exit_bb.into())?
        .set_curr_block(exit_bb.into())
        .append_inst(ret_inst.into())?;

    module.allocate_name(&layout);

    let printer = Printer::new(&module, &layout);

    println!("{}", printer.emit_module());

    Ok(())
}
