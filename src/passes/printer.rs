use crate::ir::{
    entities::{BlockData, FunctionData, ValueData, ValueKind},
    layout::BlockNode,
    module::{DataFlowGraph, Module},
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

    fn print_value(&mut self, value: Value, dfg: &DataFlowGraph) -> io::Result<()> {
        let data = dfg.local_value_data(value).unwrap();

        match data.kind() {
            ValueKind::Zero => write!(self.buf, "{} zero", data.ty()),
            ValueKind::Undef => write!(self.buf, "{} undef", data.ty()),
            ValueKind::Bytes(bytes) => {
                // hexidecimal format with little endian
                write!(self.buf, "{} 0x", data.ty())?;
                for byte in bytes.iter().rev() {
                    write!(self.buf, "{:02x}", byte)?;
                }
                Ok(())
            }
            ValueKind::Array(elems) => {
                write!(self.buf, "{} [", data.ty())?;
                for (i, elem) in elems.iter().enumerate() {
                    if i != 0 {
                        write!(self.buf, ", ")?;
                    }
                    self.print_value(*elem, dfg)?;
                }
                write!(self.buf, "]")
            }
            ValueKind::Struct(fields) => {
                write!(self.buf, "{} {{", data.ty())?;
                for (i, field) in fields.iter().enumerate() {
                    if i != 0 {
                        write!(self.buf, ", ")?;
                    }
                    self.print_value(*field, dfg)?;
                }
                write!(self.buf, "}}")
            }
            ValueKind::GlobalSlot(slot) => {
                let name = dfg.value_name(value);
                if slot.mutable() {
                    write!(self.buf, "global {}", name)?;
                } else {
                    write!(self.buf, "const {}", name)?;
                }
                write!(self.buf, " = ")?;
                self.print_value(slot.init(), dfg)
            }
            ValueKind::Alloc(alloc) => {
                write!(self.buf, "{} = alloc {}", dfg.value_name(value), alloc.ty())
            }
            ValueKind::Load(load) => {
                write!(self.buf, "{} = load {}, ", dfg.value_name(value), data.ty())?;
                self.print_value(load.ptr(), dfg)
            }
            ValueKind::Store(store) => {
                write!(self.buf, "store ")?;
                self.print_value(store.val(), dfg)?;
                write!(self.buf, ", ")?;
                self.print_value(store.ptr(), dfg)
            }
            ValueKind::Binary(binary) => {
                write!(self.buf, "{} = {} ", dfg.value_name(value), binary.op())?;
                self.print_value(binary.lhs(), dfg)?;
                write!(self.buf, ", ")?;
                self.print_value(binary.rhs(), dfg)
            }
            ValueKind::Unary(unary) => {
                write!(self.buf, "{} = {} ", dfg.value_name(value), unary.op())?;
                self.print_value(unary.val(), dfg)
            }
            ValueKind::Jump(jump) => {
                write!(self.buf, "jump {}(", dfg.block_name(jump.dst().into()))?;
                for arg in jump.args() {
                    write!(self.buf, ", ")?;
                    self.print_value(*arg, dfg)?;
                }
                write!(self.buf, ")")
            }
            ValueKind::Branch(branch) => {
                write!(self.buf, "br ")?;
                self.print_value(branch.cond(), dfg)?;
                write!(self.buf, ", {}(", dfg.block_name(branch.then_dst()))?;
                for arg in branch.then_args() {
                    write!(self.buf, ", ")?;
                    self.print_value(*arg, dfg)?;
                }
                write!(self.buf, "), {}(", dfg.block_name(branch.else_dst()))?;
                for arg in branch.else_args() {
                    write!(self.buf, ", ")?;
                    self.print_value(*arg, dfg)?;
                }
                write!(self.buf, ")")
            }
            ValueKind::Return(ret) => {
                write!(self.buf, "ret ")?;
                if let Some(val) = ret.val() {
                    self.print_value(val, dfg)?;
                }
                Ok(())
            }
            ValueKind::Call(call) => {
                write!(
                    self.buf,
                    "{} = call {} {}(",
                    dfg.value_name(value),
                    data.ty(),
                    dfg.value_name(call.callee())
                )?;
                for (i, arg) in call.args().iter().enumerate() {
                    if i != 0 {
                        write!(self.buf, ", ")?;
                    }
                    self.print_value(*arg, dfg)?;
                }
                write!(self.buf, ")")
            }
            ValueKind::GetElemPtr(gep) => {
                write!(self.buf, "{} = getelemptr ", dfg.value_name(value))?;
                self.print_value(gep.ptr(), dfg)?;
                for idx in gep.indices() {
                    write!(self.buf, ", ")?;
                    self.print_value(*idx, dfg)?;
                }
                Ok(())
            }
            ValueKind::BlockParam => {
                write!(self.buf, "{} {}", data.ty(), dfg.value_name(value))
            }
            ValueKind::Function => {
                write!(self.buf, "{}", dfg.value_name(value))
            }
        }
    }
}
