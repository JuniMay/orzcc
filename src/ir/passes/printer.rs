use std::io;

use super::{PassError, PassResult};
use crate::ir::{
    entities::FunctionKind,
    module::{DataFlowGraph, Module},
    passes::GlobalPass,
    types::Type,
    values::{Function, Value},
};

pub struct Printer<'a, T>
where
    T: io::Write,
{
    buf: &'a mut T,
}

impl From<io::Error> for PassError {
    fn from(err: io::Error) -> Self { PassError::other("printer".to_string(), Box::new(err)) }
}

impl<'a, T> Printer<'a, T>
where
    T: io::Write,
{
    pub fn new(buf: &'a mut T) -> Self { Self { buf } }

    fn print_module(&mut self, module: &Module) -> io::Result<()> {
        writeln!(self.buf, "# orzir module: {} ", module.name())?;
        for name in module.identified_type_layout() {
            let ty = Type::get_identified(name).unwrap();
            writeln!(self.buf, "type {} = {}", name, ty)?;
        }

        for value in module.global_slot_layout() {
            if module
                .with_value_data(*value, |data| data.kind().is_global_slot())
                .unwrap()
            {
                self.print_global_value(*value, module)?;
                writeln!(self.buf)?;
            }
        }

        for function in module.function_layout() {
            self.print_function(*function, module)?;
        }

        Ok(())
    }

    fn print_function(&mut self, function: Function, module: &Module) -> io::Result<()> {
        let data = module.function_data(function).unwrap();
        let function_name = module.value_name(function.into());

        if let FunctionKind::Intrinsic = data.kind() {
            return Ok(());
        }

        writeln!(self.buf)?;

        if let FunctionKind::Declaration = data.kind() {
            writeln!(self.buf, "decl {}{}", function_name, data.ty())?;
            return Ok(());
        }

        writeln!(self.buf, "func {}{} {{", function_name, data.ty())?;
        let dfg = &data.dfg;
        let layout = &data.layout;

        for (block, node) in layout.blocks().into_iter() {
            let block_data = dfg.block_data(block).unwrap();

            write!(self.buf, "{}", dfg.block_name(block))?;

            if !block_data.params().is_empty() {
                write!(self.buf, "(")?;
                for (i, param) in block_data.params().iter().enumerate() {
                    if i != 0 {
                        write!(self.buf, ", ")?;
                    }
                    write!(
                        self.buf,
                        "{} {}",
                        dfg.local_value_data(*param).unwrap().ty(),
                        dfg.value_name(*param)
                    )?;
                }
                writeln!(self.buf, "):")?;
            } else {
                writeln!(self.buf, ":")?;
            }

            for (inst, _) in node.insts().into_iter() {
                write!(self.buf, "    ")?;
                self.print_local_value(inst.into(), dfg)?;
                writeln!(self.buf)?;
            }

            writeln!(self.buf)?;
        }

        writeln!(self.buf, "}}")
    }

    fn print_global_value(&mut self, value: Value, module: &Module) -> io::Result<()> {
        write!(self.buf, "{}", value.format_as_global_value(module)?)
    }

    fn print_local_value(&mut self, value: Value, dfg: &DataFlowGraph) -> io::Result<()> {
        write!(self.buf, "{}", value.format_as_local_value(dfg)?)
    }
}

impl<T> GlobalPass for Printer<'_, T>
where
    T: io::Write,
{
    type Ok = ();

    fn run_on_module(&mut self, module: &Module) -> PassResult<Self::Ok> {
        self.print_module(module)?;
        Ok(())
    }
}
