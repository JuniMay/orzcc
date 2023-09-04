use ir::{builder::Builder, module::Module, types::Type};

pub mod backend;
pub mod collections;
pub mod ir;

fn main() {
    let mut module = Module::new();
    let mut builder = Builder::new(&mut module);

    let main_fn = builder.create_fn(String::from("main"), Vec::new(), Type::mk_int(64usize));
    let float_fn = builder.create_fn(
        String::from("test_float"),
        vec![Type::mk_half(), Type::mk_double()],
        Type::mk_float(),
    );

    builder.layout_mut().append_function(main_fn.into()).ok();
    builder.layout_mut().append_function(float_fn.into()).ok();

    println!("{}", module.to_string());
}
