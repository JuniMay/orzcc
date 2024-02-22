use std::collections::HashMap;

use thiserror::Error;

use crate::ir::{
    builder::{
        BuilderErr, ConstantBuilder, GlobalValueBuilder, LocalBlockBuilder, LocalValueBuilder,
    },
    module::Module,
    types::{Type, TypeKind},
    values::{Block, Function, Value},
};

use super::{
    ast::{Ast, AstNode, AstNodeBox},
    InstKind,
};

#[derive(Debug, Error)]
pub enum AstSyntaxErr {
    #[error("builder error: {0}")]
    BuilderErr(BuilderErr),

    #[error("name duplicated")]
    NameDuplicated,
}

impl From<BuilderErr> for AstSyntaxErr {
    fn from(err: BuilderErr) -> Self {
        Self::BuilderErr(err)
    }
}

struct AstLoweringContext {
    module: Module,
    global_map: HashMap<String, Value>,
    value_map: HashMap<String, Value>,
    block_map: HashMap<String, Block>,
}

impl AstLoweringContext {
    fn new(module_name: String) -> Self {
        Self {
            module: Module::new(module_name),
            global_map: HashMap::new(),
            value_map: HashMap::new(),
            block_map: HashMap::new(),
        }
    }

    fn map_local(&mut self, name: String, value: Value) {
        self.value_map.insert(name, value);
    }

    fn map_global(&mut self, name: String, value: Value) {
        self.global_map.insert(name, value);
    }

    fn map_block(&mut self, name: String, block: Block) {
        self.block_map.insert(name, block);
    }

    fn get_value(&self, name: &str) -> Result<Value, AstSyntaxErr> {
        self.value_map
            .get(name)
            .cloned()
            .or_else(|| self.global_map.get(name).cloned())
            .ok_or(BuilderErr::ValueNotFound.into())
    }

    fn get_block(&self, name: &str) -> Result<Block, AstSyntaxErr> {
        self.block_map
            .get(name)
            .cloned()
            .ok_or(BuilderErr::BlockNotFound.into())
    }
}

macro_rules! dfg_mut {
    ($module:expr, $function:expr) => {
        $module.function_data_mut($function).unwrap().dfg_mut()
    };
}

macro_rules! layout_mut {
    ($module:expr, $function:expr) => {
        $module.function_data_mut($function).unwrap().layout_mut()
    };
}

impl Ast {
    pub fn into_ir(&self, name: String) -> Result<Module, AstSyntaxErr> {
        let mut ctx = AstLoweringContext::new(name);

        for item in self.items.iter() {
            match item.as_ref() {
                AstNode::TypeDef(ref def) => {
                    Type::set_identified(def.name.clone(), def.ty.clone());
                    ctx.module.add_identified_type(def.name.clone());
                }
                AstNode::GlobalDef(ref def) => {
                    let mutable = def.mutable;
                    let ty = def.ty.clone();
                    let name = def.name.clone();

                    let init = self.global_init_to_value(ty.clone(), &def.init, &mut ctx.module)?;
                    let value = ctx.module.builder().global_slot(init, mutable)?;
                    ctx.map_global(name, value);
                }
                AstNode::FunctionDecl(decl) => {
                    let name = decl.name.clone();
                    let ty = decl.ty.clone();

                    let function = ctx.module.builder().function_decl(name.clone(), ty)?;

                    ctx.map_global(name, function.into());
                }
                AstNode::FunctionDef(def) => {
                    let function = ctx
                        .module
                        .builder()
                        .function_def(def.name.clone(), def.ty.clone())?;

                    ctx.map_global(def.name.clone(), function.into());
                }
                _ => unreachable!(),
            }
        }

        for item in self.items.iter() {
            if let AstNode::FunctionDef(def) = item.as_ref() {
                let function = ctx.get_value(&def.name)?.into();

                for block_node in def.blocks.iter() {
                    match block_node.as_ref() {
                        AstNode::Block(ast_block) => {
                            let mut params = Vec::new();
                            for (param_type, param_name) in ast_block.params.iter() {
                                let value = dfg_mut!(ctx.module, function)
                                    .builder()
                                    .block_param(param_type.clone())?;
                                params.push(value);
                                ctx.map_local(param_name.clone(), value);
                                dfg_mut!(ctx.module, function)
                                    .assign_local_value_name(value, param_name.clone())
                                    .map_err(|_| AstSyntaxErr::NameDuplicated)?;
                            }
                            let block: Block =
                                dfg_mut!(ctx.module, function).builder().block(params)?;
                            ctx.map_block(ast_block.name.clone(), block);
                            dfg_mut!(ctx.module, function)
                                .assign_block_name(block, ast_block.name.clone())
                                .map_err(|_| AstSyntaxErr::NameDuplicated)?;
                            layout_mut!(ctx.module, function)
                                .append_block(block)
                                .unwrap();
                        }
                        _ => unreachable!(),
                    }
                }

                for block_node in def.blocks.iter() {
                    match block_node.as_ref() {
                        AstNode::Block(ast_block) => {
                            let block: Block = ctx.get_block(&ast_block.name)?;
                            for inst_node in ast_block.insts.iter() {
                                let inst =
                                    self.inst_to_value(inst_node, &mut ctx, function)?.into();
                                layout_mut!(ctx.module, function)
                                    .append_inst(inst, block)
                                    .unwrap();
                            }
                        }
                        _ => unreachable!(),
                    }
                }

                // remove all local-scoped values
                ctx.value_map.clear();
            }
        }

        Ok(ctx.module)
    }

    fn inst_to_value(
        &self,
        inst: &AstNodeBox,
        ctx: &mut AstLoweringContext,
        function: Function,
    ) -> Result<Value, AstSyntaxErr> {
        match inst.as_ref() {
            AstNode::Inst(ast_inst) => {
                let value = match &ast_inst.kind {
                    InstKind::Binary(op) => {
                        let lhs = self.operand_to_value(&ast_inst.operands[0], ctx, function)?;
                        let rhs = self.operand_to_value(&ast_inst.operands[1], ctx, function)?;

                        dfg_mut!(ctx.module, function)
                            .builder()
                            .binary(op.clone(), lhs, rhs)?
                    }
                    InstKind::Unary(op) => {
                        let operand =
                            self.operand_to_value(&ast_inst.operands[0], ctx, function)?;

                        dfg_mut!(ctx.module, function)
                            .builder()
                            .unary(op.clone(), operand)?
                    }
                    InstKind::Store => {
                        let val = self.operand_to_value(&ast_inst.operands[0], ctx, function)?;
                        let ptr = self.operand_to_value(&ast_inst.operands[1], ctx, function)?;

                        dfg_mut!(ctx.module, function).builder().store(val, ptr)?
                    }
                    InstKind::Load => {
                        let ptr = self.operand_to_value(&ast_inst.operands[0], ctx, function)?;

                        dfg_mut!(ctx.module, function)
                            .builder()
                            .load(ast_inst.ty.clone().unwrap(), ptr)?
                    }
                    InstKind::Cast => {
                        let val = self.operand_to_value(&ast_inst.operands[0], ctx, function)?;

                        dfg_mut!(ctx.module, function)
                            .builder()
                            .cast(ast_inst.ty.clone().unwrap(), val)?
                    }
                    InstKind::Alloc => dfg_mut!(ctx.module, function)
                        .builder()
                        .alloc(ast_inst.ty.clone().unwrap())?,
                    InstKind::Jump => {
                        let (name, args) = match ast_inst.operands[0].as_ref() {
                            AstNode::Callee(callee) => {
                                let mut args = Vec::new();
                                for arg in callee.args.iter() {
                                    let value = self.operand_to_value(arg, ctx, function)?;
                                    args.push(value);
                                }
                                (callee.name.clone(), args)
                            }
                            _ => unreachable!(),
                        };

                        let block = ctx.get_block(&name)?;

                        dfg_mut!(ctx.module, function).builder().jump(block, args)?
                    }
                    InstKind::Branch => {
                        let cond = self.operand_to_value(&ast_inst.operands[0], ctx, function)?;
                        let (then_name, then_args) = match ast_inst.operands[1].as_ref() {
                            AstNode::Callee(callee) => {
                                let mut args = Vec::new();
                                for arg in callee.args.iter() {
                                    let value = self.operand_to_value(arg, ctx, function)?;
                                    args.push(value);
                                }
                                (callee.name.clone(), args)
                            }
                            _ => unreachable!(),
                        };
                        let (else_name, else_args) = match ast_inst.operands[2].as_ref() {
                            AstNode::Callee(callee) => {
                                let mut args = Vec::new();
                                for arg in callee.args.iter() {
                                    let value = self.operand_to_value(arg, ctx, function)?;
                                    args.push(value);
                                }
                                (callee.name.clone(), args)
                            }
                            _ => unreachable!(),
                        };
                        let then_block = ctx.get_block(&then_name)?;
                        let else_block = ctx.get_block(&else_name)?;

                        dfg_mut!(ctx.module, function)
                            .builder()
                            .branch(cond, then_block, else_block, then_args, else_args)?
                    }
                    InstKind::Return => {
                        let value = if ast_inst.operands.is_empty() {
                            None
                        } else {
                            let value =
                                self.operand_to_value(&ast_inst.operands[0], ctx, function)?;
                            Some(value)
                        };

                        dfg_mut!(ctx.module, function).builder().return_(value)?
                    }
                    InstKind::Call => {
                        let (name, args) = match ast_inst.operands[0].as_ref() {
                            AstNode::Callee(callee) => {
                                let mut args = Vec::new();
                                for arg in callee.args.iter() {
                                    let value = self.operand_to_value(arg, ctx, function)?;
                                    args.push(value);
                                }
                                (callee.name.clone(), args)
                            }
                            _ => unreachable!(),
                        };
                        let callee = ctx.get_value(&name)?;

                        dfg_mut!(ctx.module, function).builder().call(
                            ast_inst.ty.clone().unwrap(),
                            callee,
                            args,
                        )?
                    }
                    InstKind::GetElemPtr => {
                        let ptr = self.operand_to_value(&ast_inst.operands[0], ctx, function)?;
                        let mut indices = Vec::new();
                        for index in ast_inst.operands.iter().skip(1) {
                            let value = self.operand_to_value(index, ctx, function)?;
                            indices.push(value);
                        }

                        dfg_mut!(ctx.module, function).builder().getelemptr(
                            ptr,
                            ast_inst.ty.clone().unwrap(),
                            indices,
                        )?
                    }
                };
                if ast_inst.dest.is_some() {
                    ctx.map_local(ast_inst.dest.clone().unwrap(), value);
                    dfg_mut!(ctx.module, function)
                        .assign_local_value_name(value, ast_inst.dest.clone().unwrap())
                        .map_err(|_| AstSyntaxErr::NameDuplicated)?;
                }
                Ok(value)
            }
            _ => unreachable!(),
        }
    }

    fn operand_to_value(
        &self,
        operand: &AstNodeBox,
        ctx: &mut AstLoweringContext,
        function: Function,
    ) -> Result<Value, AstSyntaxErr> {
        match operand.as_ref() {
            AstNode::Operand(operand) => match operand.value.as_ref() {
                AstNode::GlobalIdent(ref name) => ctx.get_value(name),
                AstNode::LocalIdent(ref name) => ctx.get_value(name),
                AstNode::Bytes(ref bytes) => {
                    if let Some(ref ty) = operand.ty {
                        dfg_mut!(ctx.module, function)
                            .builder()
                            .bytes(ty.clone(), bytes.clone())
                            .map_err(|e| e.into())
                    } else {
                        panic!("Type not found for local constant")
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    fn global_init_to_value(
        &self,
        ty: Type,
        init: &AstNodeBox,
        module: &mut Module,
    ) -> Result<Value, AstSyntaxErr> {
        let value = match init.as_ref() {
            AstNode::Array(array) => {
                let (_size, elem_type) =
                    ty.as_array().ok_or(BuilderErr::InvalidType(ty.clone()))?;
                let mut values = Vec::new();
                for elem in array.elems.iter() {
                    let value = self.global_init_to_value(elem_type.clone(), elem, module)?;
                    values.push(value);
                }

                module.builder().array(ty, values)?
            }
            AstNode::Struct(struct_) => {
                let struct_ty = match ty.kind() {
                    TypeKind::Struct(_) => ty.clone(),
                    TypeKind::Identified(name) => {
                        let struct_ty = Type::get_identified(name)
                            .ok_or(BuilderErr::IdentifiedTypeNotFound(name.clone()))?;
                        struct_ty.clone()
                    }
                    _ => return Err(BuilderErr::InvalidType(ty.clone()).into()),
                };
                let struct_ty = struct_ty
                    .as_struct()
                    .ok_or(BuilderErr::InvalidType(struct_ty.clone()))?;
                let mut values = Vec::new();
                for (i, field) in struct_.fields.iter().enumerate() {
                    let value = self.global_init_to_value(struct_ty[i].clone(), field, module)?;
                    values.push(value);
                }
                module.builder().struct_(ty, values)?
            }
            AstNode::Bytes(bytes) => module.builder().bytes(ty, bytes.clone())?,
            _ => unreachable!(),
        };
        Ok(value)
    }
}
