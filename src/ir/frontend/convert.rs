use thiserror::Error;

use crate::ir::{
    builders::{
        BuildAggregateConstant, BuildBlock, BuildError, BuildGlobalValue, BuildLocalValue,
        BuildNonAggregateConstant,
    },
    module::Module,
    types::Type,
    values::{Block, Function, Value},
};

use super::{
    ast::{Ast, AstNodeBox, AstNodeKind},
    tokens::Span,
    InstKind,
};

#[derive(Debug, Error)]
pub enum SemanticError {
    #[error(transparent)]
    BuildError(#[from] BuildError),

    #[error("name duplicated at {0:?}")]
    NameDuplicated(Span),

    #[error("value name not found at {0:?}")]
    ValueNameNotFound(Span),

    #[error("block name not found at {0:?}")]
    BlockNameNotFound(Span),
}

struct AstLoweringContext {
    module: Module,
}

impl AstLoweringContext {
    fn new(module_name: String) -> Self {
        Self {
            module: Module::new(module_name),
        }
    }
}

macro_rules! dfg_mut {
    ($module:expr, $function:expr) => {
        $module.function_data_mut($function).unwrap().dfg_mut()
    };
}

macro_rules! dfg {
    ($module:expr, $function:expr) => {
        $module.function_data($function).unwrap().dfg()
    };
}

macro_rules! layout_mut {
    ($module:expr, $function:expr) => {
        $module.function_data_mut($function).unwrap().layout_mut()
    };
}

impl Ast {
    pub fn into_ir(&self, name: String) -> Result<Module, SemanticError> {
        let mut ctx = AstLoweringContext::new(name);

        for item in self.items.iter() {
            match item.as_ref().kind {
                AstNodeKind::TypeDef(ref def) => {
                    Type::set_identified(def.name.clone(), def.ty.clone());
                    ctx.module.add_identified_type(def.name.clone());
                }
                AstNodeKind::GlobalDef(ref def) => {
                    let mutable = def.mutable;
                    let ty = def.ty.clone();
                    let name = def.name.clone();
                    let init = self.global_init_to_value(ty.clone(), &def.init, &mut ctx.module)?;
                    let value = ctx.module.builder().global_slot(init, mutable)?;
                    ctx.module
                        .assign_name(value, name)
                        .map_err(|_| SemanticError::NameDuplicated(def.init.span))?;
                }
                AstNodeKind::FunctionDecl(ref decl) => {
                    let name = decl.name.clone();
                    let ty = decl.ty.clone();
                    let function = ctx.module.builder().function_decl(ty)?;
                    ctx.module
                        .assign_name(function.into(), name)
                        .map_err(|_| SemanticError::NameDuplicated(item.span))?;
                }
                AstNodeKind::FunctionDef(ref def) => {
                    let function = ctx.module.builder().function_def(def.ty.clone())?;
                    ctx.module
                        .assign_name(function.into(), def.name.clone())
                        .map_err(|_| SemanticError::NameDuplicated(item.span))?;
                }
                _ => unreachable!(),
            }
        }

        for item in self.items.iter() {
            if let AstNodeKind::FunctionDef(ref def) = item.as_ref().kind {
                let function = ctx
                    .module
                    .get_value_by_name(&def.name)
                    .expect("function should be found in the context")
                    .into();

                for block_node in def.blocks.iter() {
                    match block_node.as_ref().kind {
                        AstNodeKind::Block(ref ast_block) => {
                            let mut params = Vec::new();
                            for (param_type, param_name) in ast_block.params.iter() {
                                let value = dfg_mut!(ctx.module, function)
                                    .builder()
                                    .block_param(param_type.clone())?;

                                params.push(value);
                                dfg!(ctx.module, function)
                                    .assign_local_value_name(value, param_name.clone())
                                    .map_err(|_| SemanticError::NameDuplicated(block_node.span))?;
                            }
                            let block: Block =
                                dfg_mut!(ctx.module, function).builder().block(params)?;
                            dfg!(ctx.module, function)
                                .assign_block_name(block, ast_block.name.clone())
                                .map_err(|_| SemanticError::NameDuplicated(block_node.span))?;

                            layout_mut!(ctx.module, function)
                                .append_block(block)
                                .unwrap();
                        }
                        _ => unreachable!(),
                    }
                }

                for block_node in def.blocks.iter() {
                    match block_node.as_ref().kind {
                        AstNodeKind::Block(ref ast_block) => {
                            let block: Block = dfg!(ctx.module, function)
                                .get_block_by_name(&ast_block.name)
                                .ok_or(SemanticError::BlockNameNotFound(block_node.span))?;

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
            }
        }

        Ok(ctx.module)
    }

    fn inst_to_value(
        &self,
        inst: &AstNodeBox,
        ctx: &mut AstLoweringContext,
        function: Function,
    ) -> Result<Value, SemanticError> {
        match inst.as_ref().kind {
            AstNodeKind::Inst(ref ast_inst) => {
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
                    InstKind::Cast(op) => {
                        let val = self.operand_to_value(&ast_inst.operands[0], ctx, function)?;

                        dfg_mut!(ctx.module, function).builder().cast(
                            op.clone(),
                            ast_inst.ty.clone().unwrap(),
                            val,
                        )?
                    }
                    InstKind::Alloc => dfg_mut!(ctx.module, function)
                        .builder()
                        .alloc(ast_inst.ty.clone().unwrap())?,
                    InstKind::Jump => {
                        let (name, args) = match ast_inst.operands[0].as_ref().kind {
                            AstNodeKind::Callee(ref callee) => {
                                let mut args = Vec::new();
                                for arg in callee.args.iter() {
                                    let value = self.operand_to_value(arg, ctx, function)?;
                                    args.push(value);
                                }
                                (callee.name.clone(), args)
                            }
                            _ => unreachable!(),
                        };

                        let block = dfg_mut!(ctx.module, function)
                            .get_block_by_name(&name)
                            .ok_or(SemanticError::BlockNameNotFound(ast_inst.operands[0].span))?;

                        dfg_mut!(ctx.module, function).builder().jump(block, args)?
                    }
                    InstKind::Branch => {
                        let cond = self.operand_to_value(&ast_inst.operands[0], ctx, function)?;
                        let (then_name, then_args) = match ast_inst.operands[1].as_ref().kind {
                            AstNodeKind::Callee(ref callee) => {
                                let mut args = Vec::new();
                                for arg in callee.args.iter() {
                                    let value = self.operand_to_value(arg, ctx, function)?;
                                    args.push(value);
                                }
                                (callee.name.clone(), args)
                            }
                            _ => unreachable!(),
                        };
                        let (else_name, else_args) = match ast_inst.operands[2].as_ref().kind {
                            AstNodeKind::Callee(ref callee) => {
                                let mut args = Vec::new();
                                for arg in callee.args.iter() {
                                    let value = self.operand_to_value(arg, ctx, function)?;
                                    args.push(value);
                                }
                                (callee.name.clone(), args)
                            }
                            _ => unreachable!(),
                        };
                        let then_block = dfg_mut!(ctx.module, function)
                            .get_block_by_name(&then_name)
                            .ok_or(SemanticError::BlockNameNotFound(ast_inst.operands[1].span))?;

                        let else_block = dfg_mut!(ctx.module, function)
                            .get_block_by_name(&else_name)
                            .ok_or(SemanticError::BlockNameNotFound(ast_inst.operands[2].span))?;

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
                        let (name, args) = match ast_inst.operands[0].as_ref().kind {
                            AstNodeKind::Callee(ref callee) => {
                                let mut args = Vec::new();
                                for arg in callee.args.iter() {
                                    let value = self.operand_to_value(arg, ctx, function)?;
                                    args.push(value);
                                }
                                (callee.name.clone(), args)
                            }
                            _ => unreachable!(),
                        };
                        let callee = dfg_mut!(ctx.module, function)
                            .get_value_by_name(&name)
                            .ok_or(SemanticError::ValueNameNotFound(ast_inst.operands[0].span))?;

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
                    dfg_mut!(ctx.module, function)
                        .assign_local_value_name(value, ast_inst.dest.clone().unwrap())
                        .map_err(|_| SemanticError::NameDuplicated(inst.span))?;
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
    ) -> Result<Value, SemanticError> {
        match operand.as_ref().kind {
            AstNodeKind::Operand(ref operand) => match operand.value.as_ref().kind {
                AstNodeKind::GlobalIdent(ref name) => ctx
                    .module
                    .get_value_by_name(name)
                    .ok_or(SemanticError::ValueNameNotFound(operand.value.span)),
                AstNodeKind::LocalIdent(ref name) => dfg!(ctx.module, function)
                    .get_local_value_by_name(name)
                    .ok_or(SemanticError::ValueNameNotFound(operand.value.span)),
                AstNodeKind::Bytes(ref bytes) => {
                    if let Some(ref ty) = operand.ty {
                        dfg_mut!(ctx.module, function)
                            .builder()
                            .bytes(ty.clone(), bytes.clone())
                            .map_err(|e| e.into())
                    } else {
                        panic!("type not found for local constant")
                    }
                }
                AstNodeKind::Zero => {
                    if let Some(ref ty) = operand.ty {
                        dfg_mut!(ctx.module, function)
                            .builder()
                            .zero(ty.clone())
                            .map_err(|e| e.into())
                    } else {
                        panic!("type not found for local zero value")
                    }
                }
                AstNodeKind::Undef => {
                    if let Some(ref ty) = operand.ty {
                        dfg_mut!(ctx.module, function)
                            .builder()
                            .undef(ty.clone())
                            .map_err(|e| e.into())
                    } else {
                        panic!("type not found for local undef value")
                    }
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        }
    }

    #[allow(clippy::only_used_in_recursion)]
    fn global_init_to_value(
        &self,
        ty: Type,
        init: &AstNodeBox,
        module: &mut Module,
    ) -> Result<Value, SemanticError> {
        let value = match init.as_ref().kind {
            AstNodeKind::Array(ref array) => {
                let (_size, elem_type) = ty
                    .as_array()
                    .ok_or_else(|| BuildError::InvalidType(ty.clone()))?;
                let mut values = Vec::new();
                for elem in array.elems.iter() {
                    let value = self.global_init_to_value(elem_type.clone(), elem, module)?;
                    values.push(value);
                }

                module.builder().array(ty, values)?
            }
            AstNodeKind::Struct(ref struct_) => {
                let struct_ty = ty
                    .as_struct()
                    .ok_or_else(|| BuildError::InvalidType(ty.clone()))?;
                let mut values = Vec::new();
                for (i, field) in struct_.fields.iter().enumerate() {
                    let value = self.global_init_to_value(struct_ty[i].clone(), field, module)?;
                    values.push(value);
                }
                module.builder().struct_(ty, values)?
            }
            AstNodeKind::Bytes(ref bytes) => module.builder().bytes(ty, bytes.clone())?,
            AstNodeKind::Zero => module.builder().zero(ty)?,
            AstNodeKind::Undef => module.builder().undef(ty)?,
            _ => unreachable!(),
        };
        Ok(value)
    }
}
