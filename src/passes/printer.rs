use crate::ir::{
    entities::{FunctionData, FunctionKind, ValueKind},
    module::{DataFlowGraph, Module},
    pass::GlobalPass,
    values::Value,
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
        write!(self.buf, "# orzir module: {} \n", module.name())?;

        for name in module.custom_type_layout() {
            let ty = module.custom_type(name).unwrap();
            write!(self.buf, "type {} = {}\n", name, ty)?;
        }

        for value in module.global_slot_layout() {
            if module
                .with_value_data(*value, |data| data.kind().is_global_slot())
                .unwrap()
            {
                self.print_global_value(*value, module)?;
                write!(self.buf, "\n")?;
            }
        }

        for function in module.function_layout() {
            self.print_function(module.function_data(*function).unwrap())?;
        }

        Ok(())
    }

    fn print_function(&mut self, data: &FunctionData) -> io::Result<()> {
        if let FunctionKind::Intrinsic = data.kind() {
            return Ok(());
        }

        write!(self.buf, "\n")?;

        write!(self.buf, "fn {}{} {{\n", data.name(), data.ty())?;

        if let FunctionKind::Declaration = data.kind() {
            return Ok(());
        }

        let dfg = data.dfg();
        let layout = data.layout();

        for (block, node) in layout.blocks().into_iter() {
            let block_data = dfg.block_data(block).unwrap();

            write!(self.buf, "{}", dfg.block_name(block))?;

            if block_data.params().len() > 0 {
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
                write!(self.buf, "):\n")?;
            } else {
                write!(self.buf, ":\n")?;
            }

            for (inst, _) in node.insts().into_iter() {
                write!(self.buf, "    ")?;
                self.print_local_value(inst.into(), dfg)?;
                write!(self.buf, "\n")?;
            }
        }

        write!(self.buf, "}}\n")
    }

    /// Print the value as operand in the instruction
    fn print_operand(&mut self, value: Value, dfg: &DataFlowGraph) -> io::Result<()> {
        let data = dfg.local_value_data(value).unwrap();

        if data.kind().is_const() {
            return self.print_local_value(value, dfg);
        }

        write!(self.buf, "{} {}", data.ty(), dfg.value_name(value))
    }

    fn print_global_value(&mut self, value: Value, module: &Module) -> io::Result<()> {
        module
            .with_value_data(value, |data| {
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
                            self.print_global_value(*elem, module)?;
                        }
                        write!(self.buf, "]")
                    }
                    ValueKind::Struct(fields) => {
                        write!(self.buf, "{} {{", data.ty())?;
                        for (i, field) in fields.iter().enumerate() {
                            if i != 0 {
                                write!(self.buf, ", ")?;
                            }
                            self.print_global_value(*field, module)?;
                        }
                        write!(self.buf, "}}")
                    }
                    ValueKind::GlobalSlot(slot) => {
                        if slot.mutable() {
                            write!(self.buf, "global {} = ", module.value_name(value))?;
                        } else {
                            write!(self.buf, "const {} = ", module.value_name(value))?;
                        }
                        self.print_global_value(slot.init(), module)
                    }
                    _ => panic!("unexpected local value kind when printing global value"),
                }
            })
            .unwrap()
    }

    fn print_local_value(&mut self, value: Value, dfg: &DataFlowGraph) -> io::Result<()> {
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
                    self.print_local_value(*elem, dfg)?;
                }
                write!(self.buf, "]")
            }
            ValueKind::Struct(fields) => {
                write!(self.buf, "{} {{", data.ty())?;
                for (i, field) in fields.iter().enumerate() {
                    if i != 0 {
                        write!(self.buf, ", ")?;
                    }
                    self.print_local_value(*field, dfg)?;
                }
                write!(self.buf, "}}")
            }
            ValueKind::Alloc(alloc) => {
                write!(self.buf, "{} = alloc {}", dfg.value_name(value), alloc.ty())
            }
            ValueKind::Load(load) => {
                write!(self.buf, "{} = load {}, ", dfg.value_name(value), data.ty())?;
                self.print_operand(load.ptr(), dfg)
            }
            ValueKind::Store(store) => {
                write!(self.buf, "store ")?;
                self.print_operand(store.val(), dfg)?;
                write!(self.buf, ", ")?;
                self.print_operand(store.ptr(), dfg)
            }
            ValueKind::Binary(binary) => {
                write!(self.buf, "{} = {} ", dfg.value_name(value), binary.op())?;
                self.print_operand(binary.lhs(), dfg)?;
                write!(self.buf, ", ")?;
                self.print_operand(binary.rhs(), dfg)
            }
            ValueKind::Unary(unary) => {
                write!(self.buf, "{} = {} ", dfg.value_name(value), unary.op())?;
                self.print_operand(unary.val(), dfg)
            }
            ValueKind::Jump(jump) => {
                write!(self.buf, "jump {}(", dfg.block_name(jump.dst().into()))?;
                for (i, arg) in jump.args().iter().enumerate() {
                    if i != 0 {
                        write!(self.buf, ", ")?;
                    }
                    self.print_operand(*arg, dfg)?;
                }
                write!(self.buf, ")")
            }
            ValueKind::Branch(branch) => {
                write!(self.buf, "br ")?;
                self.print_operand(branch.cond(), dfg)?;
                write!(self.buf, ", {}(", dfg.block_name(branch.then_dst()))?;
                for (i, arg) in branch.then_args().iter().enumerate() {
                    if i != 0 {
                        write!(self.buf, ", ")?;
                    }
                    self.print_operand(*arg, dfg)?;
                }
                write!(self.buf, "), {}(", dfg.block_name(branch.else_dst()))?;
                for (i, arg) in branch.else_args().iter().enumerate() {
                    if i != 0 {
                        write!(self.buf, ", ")?;
                    }
                    self.print_operand(*arg, dfg)?;
                }
                write!(self.buf, ")")
            }
            ValueKind::Return(ret) => {
                write!(self.buf, "ret ")?;
                if let Some(val) = ret.val() {
                    self.print_operand(val, dfg)?;
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
                    self.print_operand(*arg, dfg)?;
                }
                write!(self.buf, ")")
            }
            ValueKind::GetElemPtr(gep) => {
                write!(self.buf, "{} = getelemptr ", dfg.value_name(value))?;
                self.print_operand(gep.ptr(), dfg)?;
                for idx in gep.indices() {
                    write!(self.buf, ", ")?;
                    self.print_operand(*idx, dfg)?;
                }
                Ok(())
            }
            ValueKind::Function | ValueKind::GlobalSlot(_) | ValueKind::BlockParam => {
                panic!(
                    "function, global slot, and block param should not be printed as local value"
                );
            }
        }
    }
}

impl<T> GlobalPass for Printer<'_, T>
where
    T: io::Write,
{
    type Ok = ();
    type Err = io::Error;

    fn run(&mut self, module: &mut Module) -> Result<Self::Ok, Self::Err> {
        self.print_module(module)?;
        Ok(())
    }
}
