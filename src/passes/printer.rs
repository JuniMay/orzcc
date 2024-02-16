use crate::ir::{
    entities::{BlockData, FunctionData, ValueData},
    layout::BlockNode,
    module::Module,
    pass::GlobalPass,
    values::{Block, Function, Value},
};

use std::io;

pub struct Printer<'a, T>
where
    T: io::Write,
{
    buf: &'a mut T,
}

impl<'a, T> Printer<'a, T>
where
    T: io::Write,
{
    pub fn new(buf: &'a mut T) -> Self {
        Self { buf }
    }

    fn print_module(&mut self, module: &Module) -> io::Result<()> {
        write!(self.buf, "; orzir module: {} \n", module.name())?;
        todo!()
    }

    fn print_function(&mut self, function: Function, data: &FunctionData) -> io::Result<()> {
        todo!()
    }

    fn print_block(&mut self, block: Block, data: &BlockData, node: &BlockNode) -> io::Result<()> {
        todo!()
    }

    fn print_value(&mut self, value: Value, data: &ValueData) -> io::Result<()> {
        todo!()
    }
}
