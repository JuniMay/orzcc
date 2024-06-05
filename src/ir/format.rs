use std::{
    fmt,
    io::{self, BufWriter, Write},
};

use super::{
    entities::ValueKind,
    module::{DataFlowGraph, Module},
    values::{BinaryOp, CastOp, FCmpCond, ICmpCond, UnaryOp, Value},
};

impl fmt::Display for ICmpCond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ICmpCond::Eq => write!(f, "eq"),
            ICmpCond::Ne => write!(f, "ne"),
            ICmpCond::Slt => write!(f, "slt"),
            ICmpCond::Sle => write!(f, "sle"),
        }
    }
}

impl fmt::Display for FCmpCond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FCmpCond::OEq => write!(f, "oeq"),
            FCmpCond::ONe => write!(f, "one"),
            FCmpCond::OLt => write!(f, "olt"),
            FCmpCond::OLe => write!(f, "ole"),
        }
    }
}

impl fmt::Display for CastOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CastOp::Trunc => write!(f, "trunc"),
            CastOp::ZExt => write!(f, "zext"),
            CastOp::SExt => write!(f, "sext"),
            CastOp::FpToUI => write!(f, "fptoui"),
            CastOp::FpToSI => write!(f, "fptosi"),
            CastOp::UIToFp => write!(f, "uitofp"),
            CastOp::SIToFp => write!(f, "sitofp"),
            CastOp::FpTrunc => write!(f, "fptrunc"),
            CastOp::FpExt => write!(f, "fpext"),
            CastOp::Bitcast => write!(f, "bitcast"),
        }
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "add"),
            BinaryOp::FAdd => write!(f, "fadd"),
            BinaryOp::Sub => write!(f, "sub"),
            BinaryOp::FSub => write!(f, "fsub"),
            BinaryOp::Mul => write!(f, "mul"),
            BinaryOp::FMul => write!(f, "fmul"),
            BinaryOp::UDiv => write!(f, "udiv"),
            BinaryOp::SDiv => write!(f, "sdiv"),
            BinaryOp::FDiv => write!(f, "fdiv"),
            BinaryOp::URem => write!(f, "urem"),
            BinaryOp::SRem => write!(f, "srem"),
            BinaryOp::FRem => write!(f, "frem"),
            BinaryOp::And => write!(f, "and"),
            BinaryOp::Or => write!(f, "or"),
            BinaryOp::Xor => write!(f, "xor"),
            BinaryOp::Shl => write!(f, "shl"),
            BinaryOp::LShr => write!(f, "lshr"),
            BinaryOp::AShr => write!(f, "ashr"),
            BinaryOp::ICmp(cond) => write!(f, "icmp.{}", cond),
            BinaryOp::FCmp(cond) => write!(f, "fcmp.{}", cond),
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::FNeg => write!(f, "fneg"),
            UnaryOp::Not => write!(f, "not"),
        }
    }
}

impl Value {
    /// Format the value as a global value.
    pub fn format_as_global_value(&self, module: &Module) -> io::Result<String> {
        let mut buf = BufWriter::new(Vec::new());
        module
            .with_value_data(*self, |data| {
                match data.kind() {
                    ValueKind::Zero => {
                        write!(buf, "zero")
                    }
                    ValueKind::Undef => {
                        write!(buf, "undef")
                    }
                    ValueKind::Bytes(bytes) => {
                        // hexidecimal format with little endian
                        write!(buf, "0x")?;
                        for byte in bytes.iter().rev() {
                            write!(buf, "{:02x}", byte)?;
                        }
                        Ok(())
                    }
                    ValueKind::Array(elems) => {
                        write!(buf, "[")?;
                        for (i, elem) in elems.iter().enumerate() {
                            if i != 0 {
                                write!(buf, ", ")?;
                            }
                            write!(buf, "{}", elem.format_as_global_value(module)?)?;
                        }
                        write!(buf, "]")
                    }
                    ValueKind::Struct(fields) => {
                        write!(buf, "{{")?;
                        for (i, field) in fields.iter().enumerate() {
                            if i != 0 {
                                write!(buf, ", ")?;
                            }
                            write!(buf, "{}", field.format_as_global_value(module)?)?;
                        }
                        write!(buf, "}}")
                    }
                    ValueKind::GlobalSlot(slot) => {
                        let ty = module
                            .with_value_data(slot.init(), |data| data.ty())
                            .unwrap();
                        if slot.mutable() {
                            write!(buf, "slot {} = global {} ", module.value_name(*self), ty)?;
                        } else {
                            write!(buf, "slot {} = const {} ", module.value_name(*self), ty)?;
                        }
                        write!(buf, "{}", slot.init().format_as_global_value(module)?)
                    }
                    _ => panic!("unexpected local value kind when printing global value"),
                }
            })
            .unwrap()?;

        let s = String::from_utf8(buf.into_inner().unwrap()).unwrap();
        Ok(s)
    }

    /// Format the value as a local operand for instructions.
    pub fn format_as_operand(&self, dfg: &DataFlowGraph) -> io::Result<String> {
        dfg.with_value_data(*self, |data| {
            if data.kind().is_const() {
                // self.print_local_value(*self, dfg)
                self.format_as_local_value(dfg)
            } else {
                Ok(format!("{} {}", data.ty(), dfg.value_name(*self)))
            }
        })
        .unwrap()
    }

    /// Format the value as a local inst/constant.
    pub fn format_as_local_value(&self, dfg: &DataFlowGraph) -> io::Result<String> {
        let mut buf = BufWriter::new(Vec::new());
        let data = dfg.local_value_data(*self).unwrap();

        match data.kind() {
            ValueKind::Zero => write!(buf, "{} zero", data.ty())?,
            ValueKind::Undef => write!(buf, "{} undef", data.ty())?,
            ValueKind::Bytes(bytes) => {
                // hexidecimal format with little endian
                write!(buf, "{} 0x", data.ty())?;
                if bytes.is_empty() {
                    write!(buf, "00")?;
                }
                for byte in bytes.iter().rev() {
                    write!(buf, "{:02x}", byte)?;
                }
            }
            ValueKind::Array(elems) => {
                write!(buf, "{} [", data.ty())?;
                for (i, elem) in elems.iter().enumerate() {
                    if i != 0 {
                        write!(buf, ", ")?;
                    }
                    write!(buf, "{}", elem.format_as_local_value(dfg)?)?;
                }
                write!(buf, "]")?;
            }
            ValueKind::Struct(fields) => {
                write!(buf, "{} {{", data.ty())?;
                for (i, field) in fields.iter().enumerate() {
                    if i != 0 {
                        write!(buf, ", ")?;
                    }
                    write!(buf, "{}", field.format_as_local_value(dfg)?)?;
                }
                write!(buf, "}}")?;
            }
            ValueKind::Alloc(alloc) => {
                write!(buf, "{} = alloc {}", dfg.value_name(*self), alloc.ty())?;
            }
            ValueKind::Load(load) => {
                write!(buf, "{} = load {}, ", dfg.value_name(*self), data.ty())?;
                write!(buf, "{}", load.ptr().format_as_operand(dfg)?)?;
            }
            ValueKind::Cast(cast) => {
                write!(
                    buf,
                    "{} = {} {}, ",
                    dfg.value_name(*self),
                    cast.op(),
                    data.ty()
                )?;
                // self.print_operand(cast.val(), dfg)
                write!(buf, "{}", cast.val().format_as_operand(dfg)?)?;
            }
            ValueKind::Store(store) => {
                write!(buf, "store ")?;
                write!(buf, "{}", store.val().format_as_operand(dfg)?)?;
                write!(buf, ", ")?;
                write!(buf, "{}", store.ptr().format_as_operand(dfg)?)?;
            }
            ValueKind::Binary(binary) => {
                write!(buf, "{} = {} ", dfg.value_name(*self), binary.op())?;
                write!(buf, "{}", binary.lhs().format_as_operand(dfg)?)?;
                write!(buf, ", ")?;
                write!(buf, "{}", binary.rhs().format_as_operand(dfg)?)?;
            }
            ValueKind::Unary(unary) => {
                write!(buf, "{} = {} ", dfg.value_name(*self), unary.op())?;
                write!(buf, "{}", unary.val().format_as_operand(dfg)?)?;
            }
            ValueKind::Jump(jump) => {
                write!(buf, "jump {}(", dfg.block_name(jump.dst()))?;
                for (i, arg) in jump.args().iter().enumerate() {
                    if i != 0 {
                        write!(buf, ", ")?;
                    }
                    write!(buf, "{}", arg.format_as_operand(dfg)?)?;
                }
                write!(buf, ")")?;
            }
            ValueKind::Branch(branch) => {
                write!(buf, "br ")?;
                write!(buf, "{}", branch.cond().format_as_operand(dfg)?)?;
                write!(buf, ", {}", dfg.block_name(branch.then_dst()))?;
                if !branch.then_args().is_empty() {
                    write!(buf, "(")?;
                    for (i, arg) in branch.then_args().iter().enumerate() {
                        if i != 0 {
                            write!(buf, ", ")?;
                        }
                        write!(buf, "{}", arg.format_as_operand(dfg)?)?;
                    }
                    write!(buf, ")")?;
                }
                write!(buf, ", {}", dfg.block_name(branch.else_dst()))?;
                if !branch.else_args().is_empty() {
                    write!(buf, "(")?;
                    for (i, arg) in branch.else_args().iter().enumerate() {
                        if i != 0 {
                            write!(buf, ", ")?;
                        }
                        write!(buf, "{}", arg.format_as_operand(dfg)?)?;
                    }
                    write!(buf, ")")?;
                }
            }
            ValueKind::Return(ret) => {
                write!(buf, "ret ")?;
                if let Some(val) = ret.val() {
                    write!(buf, "{}", val.format_as_operand(dfg)?)?;
                }
            }
            ValueKind::Call(call) => {
                if !data.ty().is_void() {
                    write!(buf, "{} = ", dfg.value_name(*self))?;
                }
                write!(buf, "call {} {}(", data.ty(), dfg.value_name(call.callee()))?;
                for (i, arg) in call.args().iter().enumerate() {
                    if i != 0 {
                        write!(buf, ", ")?;
                    }
                    write!(buf, "{}", arg.format_as_operand(dfg)?)?;
                }
                write!(buf, ")")?;
            }
            ValueKind::GetElemPtr(gep) => {
                write!(buf, "{} = getelemptr {}, ", dfg.value_name(*self), gep.ty())?;
                write!(buf, "{}", gep.ptr().format_as_operand(dfg)?)?;
                for idx in gep.indices() {
                    write!(buf, ", ")?;
                    write!(buf, "{}", idx.format_as_operand(dfg)?)?;
                }
            }
            ValueKind::Function | ValueKind::GlobalSlot(_) | ValueKind::BlockParam => {
                panic!("function, global slot, and block param should not be formatted as local value.");
            }
        }

        let s = String::from_utf8(buf.into_inner().unwrap()).unwrap();
        Ok(s)
    }
}
