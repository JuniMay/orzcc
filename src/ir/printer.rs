use crate::ir::{types::TyKind, value::ValueKind, GLOBAL_PREFIX, IDENTIFIER_PREFIX};

use super::{
    entities::{BlockCall, ConstantKind, InstData},
    layout::Layout,
    module::Module,
    value::{Block, Constant, Function, Global, Inst, Value},
    BLOCK_PREFIX, INDENT,
};

/// Printer to emit the IR.
pub struct Printer<'a> {
    /// The module of ir.
    module: &'a Module,
    /// The code layout of ir.
    layout: &'a Layout,
}

impl<'a> Printer<'a> {
    pub fn new(module: &'a Module, layout: &'a Layout) -> Self {
        Self { module, layout }
    }

    pub fn emit_identified_type(&self, name: &String) -> String {
        format!(
            "type {}{} = {}",
            IDENTIFIER_PREFIX,
            name.clone(),
            self.module.identified_types.get(name).unwrap().to_string()
        )
    }

    pub fn emit_constant(&self, constant: Constant) -> String {
        let constant_data = self.module.constants.get(&constant).unwrap();
        match constant_data.kind {
            ConstantKind::Zero => String::from("zero"),
            ConstantKind::Undef => String::from("undef"),
            ConstantKind::Bytes(ref bytes) => bytes
                .iter()
                .rev()
                .map(|b| format!("{:02x}", b))
                .collect::<Vec<_>>()
                .join(""),
            ConstantKind::Array(ref elems) => {
                format!(
                    "[{}]",
                    elems
                        .iter()
                        .map(|e| self.emit_constant(*e))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
            ConstantKind::Struct(ref fields) => {
                format!(
                    "{{ {} }}",
                    fields
                        .iter()
                        .map(|e| self.emit_constant(*e))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
    }

    pub fn emit_operand(&self, value: Value, with_type: bool) -> String {
        let value_data = self.module.values.get(&value).unwrap();

        if let TyKind::Void = value_data.ty.kind() {
            // A void value cannot be converted as an operand.
            // This happens when calling a return-void function.
            return String::from("");
        }

        let ty_str = if with_type {
            format!("{} ", value_data.ty.to_string())
        } else {
            String::from("")
        };
        let operand_str = match value_data.kind {
            ValueKind::Block => format!(
                "{}{}",
                BLOCK_PREFIX,
                self.module
                    .block_name_allocator
                    .get_name(value.into())
                    .unwrap_or(String::from("<unknown>"))
            ),
            ValueKind::Inst | ValueKind::BlockParam => format!(
                "{}{}",
                IDENTIFIER_PREFIX,
                self.module
                    .value_name_allocator
                    .get_name(value)
                    .unwrap_or(String::from("<unknown>")),
            ),
            ValueKind::Function => format!(
                "{}{}",
                GLOBAL_PREFIX,
                self.module
                    .functions
                    .get(&value.into())
                    .unwrap()
                    .name
                    .clone()
            ),
            ValueKind::Global => format!(
                "{}{}",
                GLOBAL_PREFIX,
                self.module.globals.get(&value.into()).unwrap().name.clone()
            ),
            ValueKind::Constant => self.emit_constant(value.into()),
        };

        format!("{}{}", ty_str, operand_str)
    }

    pub fn emit_block(&self, block: Block) -> String {
        let block_data = self.module.blocks.get(&block).unwrap();

        let mut res = format!(
            "\n{}{}",
            BLOCK_PREFIX,
            self.module.block_name_allocator.get_name(block).unwrap()
        );

        if !block_data.params.is_empty() {
            res.push_str(
                format!(
                    "({})",
                    block_data
                        .params
                        .iter()
                        .map(|p| { self.emit_operand(*p, true) })
                        .collect::<Vec<_>>()
                        .join(", ")
                )
                .as_str(),
            );
        }

        res.push_str(":\n");

        for (inst, _inst_node) in self.layout.get_insts(block).unwrap().iter() {
            res.push_str(INDENT);
            res.push_str(self.emit_inst(inst).as_str());
            res.push_str("\n");
        }
        res
    }

    pub fn emit_block_call(&self, block_call: &BlockCall) -> String {
        let mut res = self.emit_operand(block_call.block.into(), true);
        if !block_call.args.is_empty() {
            res.push_str(
                format!(
                    "({})",
                    block_call
                        .args
                        .iter()
                        .map(|p| { self.emit_operand(*p, true) })
                        .collect::<Vec<_>>()
                        .join(", ")
                )
                .as_str(),
            );
        }
        res
    }

    pub fn emit_inst(&self, inst: Inst) -> String {
        let inst_data = self.module.insts.get(&inst).unwrap();
        match inst_data {
            InstData::Alloc { ty } => format!(
                "{} = alloc {}",
                self.emit_operand(inst.into(), false),
                ty.to_string()
            ),
            InstData::Load { ty, addr } => format!(
                "{} = load {}, {}",
                self.emit_operand(inst.into(), false),
                ty.to_string(),
                self.emit_operand(*addr, true)
            ),
            InstData::Store { val, addr } => format!(
                "store {}, {}",
                self.emit_operand(*val, true),
                self.emit_operand(*addr, true),
            ),
            InstData::Binary { op, lhs, rhs } => format!(
                "{} = {} {}, {}",
                self.emit_operand(inst.into(), false),
                op,
                self.emit_operand(*lhs, true),
                self.emit_operand(*rhs, true),
            ),
            InstData::ICmp { cond, lhs, rhs } => format!(
                "{} = icmp.{} {}, {}",
                self.emit_operand(inst.into(), false),
                cond,
                self.emit_operand(*lhs, true),
                self.emit_operand(*rhs, true),
            ),
            InstData::FCmp { cond, lhs, rhs } => format!(
                "{} = fcmp.{} {}, {}",
                self.emit_operand(inst.into(), false),
                cond,
                self.emit_operand(*lhs, true),
                self.emit_operand(*rhs, true),
            ),
            InstData::Unary { op, val } => format!(
                "{} = {} {}",
                self.emit_operand(inst.into(), false),
                op,
                self.emit_operand(*val, true)
            ),
            InstData::Br { dst } => format!("br {}", self.emit_block_call(dst)),
            InstData::CondBr {
                cond,
                dst_then,
                dst_else,
            } => format!(
                "condbr {}, {}, {}",
                self.emit_operand(*cond, true),
                self.emit_block_call(dst_then),
                self.emit_block_call(dst_else)
            ),
            InstData::Ret { val } => {
                if let Some(val) = val {
                    format!("ret {}", self.emit_operand(*val, true))
                } else {
                    String::from("ret void")
                }
            }
            InstData::Call {
                fn_ty,
                fn_val,
                args,
            } => {
                if let TyKind::Fn(_, ret) = fn_ty.kind() {
                    if let TyKind::Void = ret.kind() {
                        format!(
                            "call {} {}({})",
                            ret.to_string(),
                            self.emit_operand(*fn_val, false),
                            args.iter()
                                .map(|v| self.emit_operand(*v, false))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    } else {
                        format!(
                            "{} = call {} {}({})",
                            self.emit_operand(inst.into(), false),
                            ret.to_string(),
                            self.emit_operand(*fn_val, false),
                            args.iter()
                                .map(|v| self.emit_operand(*v, false))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    }
                } else {
                    panic!("calling with non-function type");
                }
            }
        }
    }

    pub fn emit_global(&self, global: Global) -> String {
        let global_data = self.module.globals.get(&global).unwrap();

        format!(
            "{} {}{} = {}",
            if global_data.mutable {
                "global"
            } else {
                "constant"
            },
            GLOBAL_PREFIX,
            global_data.name,
            self.emit_constant(global_data.init)
        )
    }

    pub fn emit_function(&self, function: Function) -> String {
        let function_data = self.module.functions.get(&function).unwrap();
        let mut res = format!(
            "fn {} {} {{",
            function_data.name.clone(),
            function_data.ty.to_string()
        );
        for (block, _block_node) in self.layout.local_layouts.get(&function).unwrap().iter() {
            res.push_str(&self.emit_block(block).as_str());
        }
        res.push_str("}");
        res
    }

    pub fn emit_module(&self) -> String {
        let mut res = String::from("# Generated by ORZCC\n\n");
        for ty_name in &self.layout.identified_types {
            res.push_str(self.emit_identified_type(ty_name).as_str());
        }
        for global in &self.layout.globals {
            res.push_str(&self.emit_global(*global).as_str());
        }
        for function in &self.layout.functions {
            res.push_str(&self.emit_function(*function).as_str());
        }
        res
    }
}
