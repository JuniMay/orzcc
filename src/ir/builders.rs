//! # Builders of OrzIR.
//!
//! Builder is a unified interface to construct values (globals, constants, instructions, etc.) in
//! the OrzIR. There are two kinds of builders: `LocalBuilder` and `GlobalBuilder`.
//!
//! ## Local Builder
//!
//! Local builder is used to build values in a local data flow graph (DFG). It is useful when
//! constructing local instructions and blocks.
//!
//! ## Global Builder
//!
//! Global builder is used to build global variables, constants and even functions.
//!
use super::{
    entities::{BlockData, FunctionData, FunctionKind, ValueData, ValueKind},
    module::{DataFlowGraph, Module},
    types::Type,
    values::{
        Alloc, Binary, BinaryOp, Block, Branch, Call, Cast, CastOp, Function, GetElemPtr,
        GlobalSlot, Jump, Load, Return, Store, Unary, UnaryOp, Value,
    },
};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum BuildError {
    /// Value not found.
    ///
    /// The value is not found in the data flow graph.
    #[error("value not found")]
    ValueNotFound,

    /// Block not found.
    ///
    /// The block is not found in the data flow graph.
    #[error("block not found")]
    BlockNotFound,

    /// Type not found.
    #[error("identified type not found: {0}")]
    IdentifiedTypeNotFound(String),

    /// Invalid value type.
    ///
    /// This can be used when the value type is not valid for the operation/construction,
    /// e.g. a function type provided for zero/bytes/...
    #[error("invalid value type: {0}")]
    InvalidType(Type),

    /// Invalid value kind.
    #[error("invalid value kind")]
    InvalidKind,

    /// Trying to build aggregated constant with non-constant value.
    #[error("invalid mutability")]
    InvalidMutability,

    /// Global value in local data flow graph.
    #[error("invalid global value")]
    InvalidGlobalValue,

    /// Local value in global data flow graph.
    #[error("invalid local value")]
    InvalidLocalValue,

    /// Incompatible value type.
    ///
    /// This can be used when two values are not compatible, e.g. in binary operations.
    #[error("incompatible value type of {0} and {1}")]
    IncompatibleType(Type, Type),

    /// Incompatible array size.
    #[error("incompatible array size: expected {0}, got {1}")]
    IncompatibleArraySize(usize, usize),

    /// Incompatible array element type.
    #[error("incompatible array element type: expected {0}, got {1}")]
    IncompatibleArrayElemType(Type, Type),

    /// Incompatible struct field number.
    #[error("incompatible struct field number: expected {0}, got {1}")]
    IncompatibleStructFieldNumber(usize, usize),

    /// Incompatible struct field type.
    #[error("incompatible struct field type: expected {0}, got {1}")]
    IncompatibleStructFieldType(Type, Type),

    /// Incompatible block argument number.
    #[error("incompatible block argument number: expected {0}, got {1}")]
    IncompatibleBlockArgNumber(usize, usize),

    /// Incompatible block argument type.
    #[error("incompatible block argument type: expected {0}, got {1}")]
    IncompatibleBlockArgType(Type, Type),
}

/// Query value data.
///
/// This trait is used to provide the query interface for builder.
pub trait QueryValueData {
    /// Get the type of a value.
    fn value_type(&self, value: Value) -> Result<Type, BuildError>;

    /// Check if a value is a constant.
    fn is_value_const(&self, value: Value) -> Result<bool, BuildError>;

    /// Check if a value is a block parameter.
    fn is_value_block_param(&self, value: Value) -> Result<bool, BuildError>;

    /// Check if a value exists in the data flow graph.
    fn value_exists(&self, value: Value) -> bool {
        self.value_type(value).is_ok()
    }
}

pub trait QueryBlockData {
    fn block_data(&self, block: Block) -> Result<&BlockData, BuildError>;
}

pub trait QueryDfgData: QueryValueData + QueryBlockData {}

/// Add value to the data flow graph or the module.
pub trait AddValue {
    fn add_value(&mut self, data: ValueData) -> Result<Value, BuildError>;
}

/// Add block to the data flow graph.
pub trait AddBlock {
    fn add_block(&mut self, data: BlockData) -> Result<Block, BuildError>;
}

pub trait BuildNonAggregateConstant: QueryValueData + AddValue {
    /// Build a zero constant.
    fn zero(&mut self, ty: Type) -> Result<Value, BuildError> {
        if !ty.is_zero_initializable() {
            return Err(BuildError::InvalidType(ty));
        }
        self.add_value(ValueData::new(ty, ValueKind::Zero))
    }

    /// Build an undef constant.
    fn undef(&mut self, ty: Type) -> Result<Value, BuildError> {
        self.add_value(ValueData::new(ty, ValueKind::Undef))
    }

    /// Build a bytes constant.
    fn bytes(&mut self, ty: Type, bytes: Vec<u8>) -> Result<Value, BuildError> {
        if !ty.is_numeric() && !ty.is_ptr() {
            return Err(BuildError::InvalidType(ty));
        }
        self.add_value(ValueData::new(ty, ValueKind::Bytes(bytes)))
    }

    /// Build an integer constant of i32 type.
    fn integer(&mut self, value: i32) -> Result<Value, BuildError> {
        let bytes = value.to_le_bytes().to_vec();

        // remove leading zeros
        let bytes = bytes
            .into_iter()
            .rev()
            .skip_while(|&b| b == 0)
            .collect::<Vec<u8>>()
            .into_iter()
            .rev()
            .collect::<Vec<u8>>();

        self.bytes(Type::mk_int(32), bytes)
    }

    /// Build a float constant.
    ///
    /// This is a shorthand for `bytes` with the bytes of the float.
    fn float(&mut self, value: f32) -> Result<Value, BuildError> {
        let bytes = value.to_le_bytes().to_vec();
        self.bytes(Type::mk_float(), bytes)
    }
}

/// Constant builder for both global and local purposes.
pub trait BuildAggregateConstant: QueryValueData + AddValue {
    /// Build an array constant.
    fn array(&mut self, ty: Type, values: Vec<Value>) -> Result<Value, BuildError> {
        let (size, elem_type) = ty
            .as_array()
            .ok_or_else(|| BuildError::InvalidType(ty.clone()))?;

        if values.len() != size {
            return Err(BuildError::IncompatibleArraySize(size, values.len()));
        }

        for value in &values {
            if self.value_type(*value)? != elem_type.clone() {
                return Err(BuildError::IncompatibleArrayElemType(
                    elem_type,
                    self.value_type(*value)?,
                ));
            }
            if !self.is_value_const(*value)? {
                return Err(BuildError::InvalidMutability);
            }
        }

        self.add_value(ValueData::new(ty, ValueKind::Array(values)))
    }

    /// Build a struct constant.
    fn struct_(&mut self, ty: Type, values: Vec<Value>) -> Result<Value, BuildError> {
        let fields = ty
            .as_struct()
            .ok_or_else(|| BuildError::InvalidType(ty.clone()))?;

        if fields.len() != values.len() {
            return Err(BuildError::IncompatibleStructFieldNumber(
                fields.len(),
                values.len(),
            ));
        }

        for (field_type, value) in fields.iter().zip(&values) {
            if self.value_type(*value)? != field_type.clone() {
                return Err(BuildError::IncompatibleStructFieldType(
                    field_type.clone(),
                    self.value_type(*value)?,
                ));
            }
            if !self.is_value_const(*value)? {
                return Err(BuildError::InvalidMutability);
            }
        }

        self.add_value(ValueData::new(ty, ValueKind::Struct(values)))
    }
}

/// Local value builder for local data flow graph.
pub trait BuildLocalValue: QueryDfgData + AddValue + BuildNonAggregateConstant {
    /// Build a block parameter.
    fn block_param(&mut self, ty: Type) -> Result<Value, BuildError> {
        self.add_value(ValueData::new(ty, ValueKind::BlockParam))
    }

    /// Build an alloc instruction.
    fn alloc(&mut self, ty: Type) -> Result<Value, BuildError> {
        self.add_value(Alloc::new_value_data(ty))
    }

    /// Build a load instruction.
    fn load(&mut self, ty: Type, ptr: Value) -> Result<Value, BuildError> {
        if !self.value_type(ptr)?.is_ptr() {
            return Err(BuildError::InvalidType(ty));
        }
        self.add_value(Load::new_value_data(ty, ptr))
    }

    /// Build a cast instruction
    fn cast(&mut self, op: CastOp, ty: Type, val: Value) -> Result<Value, BuildError> {
        self.add_value(Cast::new_value_data(op, ty, val))
    }

    /// Build a store instruction.
    fn store(&mut self, val: Value, ptr: Value) -> Result<Value, BuildError> {
        if !self.value_type(ptr)?.is_ptr() {
            return Err(BuildError::InvalidType(self.value_type(ptr)?));
        }

        if !self.value_exists(val) {
            return Err(BuildError::ValueNotFound);
        }

        self.add_value(Store::new_value_data(val, ptr))
    }

    /// Build a binary instruction.
    fn binary(&mut self, op: BinaryOp, lhs: Value, rhs: Value) -> Result<Value, BuildError> {
        let lhs_type = self.value_type(lhs)?;
        let rhs_type = self.value_type(rhs)?;

        if op.require_int() && (!lhs_type.is_int() || !rhs_type.is_int()) {
            return Err(BuildError::InvalidType(lhs_type));
        }

        if op.require_float() && (!lhs_type.is_float() || !rhs_type.is_float()) {
            return Err(BuildError::InvalidType(lhs_type));
        }

        if op.require_same_type() && lhs_type != rhs_type {
            return Err(BuildError::IncompatibleType(lhs_type, rhs_type));
        }

        let res_type = match op {
            BinaryOp::ICmp(_) | BinaryOp::FCmp(_) => Type::mk_int(1),
            _ => lhs_type,
        };

        self.add_value(Binary::new_value_data(res_type, op, lhs, rhs))
    }

    /// Build a unary instruction.
    fn unary(&mut self, op: UnaryOp, val: Value) -> Result<Value, BuildError> {
        let val_type = self.value_type(val)?;

        if op.require_int() && !val_type.is_int() {
            return Err(BuildError::InvalidType(val_type));
        }

        if op.require_float() && !val_type.is_float() {
            return Err(BuildError::InvalidType(val_type));
        }

        self.add_value(Unary::new_value_data(val_type, op, val))
    }

    /// Build a jump instruction.
    fn jump(&mut self, dst: Block, args: Vec<Value>) -> Result<Value, BuildError> {
        let block_data = self.block_data(dst)?;

        if block_data.params().len() != args.len() {
            return Err(BuildError::IncompatibleBlockArgNumber(
                block_data.params().len(),
                args.len(),
            ));
        }

        for (param, arg) in block_data.params().iter().zip(&args) {
            let param_type = self.value_type(*param)?;
            let arg_type = self.value_type(*arg)?;

            if param_type != arg_type {
                return Err(BuildError::IncompatibleBlockArgType(param_type, arg_type));
            }
        }

        self.add_value(Jump::new_value_data(dst, args))
    }

    /// Build a branch instruction.
    fn branch(
        &mut self,
        cond: Value,
        then_dst: Block,
        else_dst: Block,
        then_args: Vec<Value>,
        else_args: Vec<Value>,
    ) -> Result<Value, BuildError> {
        if !self.value_type(cond)?.is_int() {
            return Err(BuildError::InvalidType(self.value_type(cond)?));
        }

        let then_block_data = self.block_data(then_dst)?;
        let else_block_data = self.block_data(else_dst)?;

        if then_block_data.params().len() != then_args.len() {
            return Err(BuildError::IncompatibleBlockArgNumber(
                then_block_data.params().len(),
                then_args.len(),
            ));
        }

        if else_block_data.params().len() != else_args.len() {
            return Err(BuildError::IncompatibleBlockArgNumber(
                else_block_data.params().len(),
                else_args.len(),
            ));
        }

        for (param, arg) in then_block_data.params().iter().zip(&then_args) {
            let param_type = self.value_type(*param)?;
            let arg_type = self.value_type(*arg)?;

            if param_type != arg_type {
                return Err(BuildError::IncompatibleBlockArgType(param_type, arg_type));
            }
        }

        for (param, arg) in else_block_data.params().iter().zip(&else_args) {
            let param_type = self.value_type(*param)?;
            let arg_type = self.value_type(*arg)?;

            if param_type != arg_type {
                return Err(BuildError::IncompatibleBlockArgType(param_type, arg_type));
            }
        }

        self.add_value(Branch::new_value_data(
            cond, then_dst, else_dst, then_args, else_args,
        ))
    }

    /// Build a return instruction.
    fn return_(&mut self, val: Option<Value>) -> Result<Value, BuildError> {
        // type check of return is not performed here
        self.add_value(Return::new_value_data(val))
    }

    /// Build a call instruction.
    fn call(&mut self, ret_ty: Type, callee: Value, args: Vec<Value>) -> Result<Value, BuildError> {
        let callee_type = self.value_type(callee)?;

        if !callee_type.is_function() && !callee_type.is_ptr() {
            return Err(BuildError::InvalidType(callee_type));
        }

        self.add_value(Call::new_value_data(ret_ty, callee, args))
    }

    /// Build a get element pointer instruction.
    fn getelemptr(
        &mut self,
        ptr: Value,
        ty: Type,
        indices: Vec<Value>,
    ) -> Result<Value, BuildError> {
        if !self.value_type(ptr)?.is_ptr() {
            return Err(BuildError::InvalidType(self.value_type(ptr)?));
        }

        self.add_value(GetElemPtr::new_value_data(ptr, ty, indices))
    }
}

/// Block builder for local data flow graph.
pub trait BuildBlock: QueryDfgData + AddBlock {
    /// Build a block.
    fn block(&mut self, params: Vec<Value>) -> Result<Block, BuildError> {
        for param in params.iter() {
            if !self.is_value_block_param(*param)? {
                return Err(BuildError::InvalidKind);
            }
        }

        self.add_block(BlockData::new(params))
    }
}

/// Global value builder for the module.
pub trait BuildGlobalValue:
    QueryValueData + AddValue + BuildNonAggregateConstant + BuildAggregateConstant
{
    /// Add a constructed function to the module.
    fn add_function(
        &mut self,
        value_data: ValueData,
        function_data: FunctionData,
    ) -> Result<Function, BuildError>;

    /// Build a global slot.
    fn global_slot(&mut self, init: Value, mutable: bool) -> Result<Value, BuildError> {
        if !self.is_value_const(init)? {
            return Err(BuildError::InvalidMutability);
        }

        self.add_value(GlobalSlot::new_value_data(Type::mk_ptr(), init, mutable))
    }

    /// Build a global function.
    fn function(&mut self, ty: Type, kind: FunctionKind) -> Result<Function, BuildError> {
        let data = FunctionData::new(ty, kind);
        self.add_function(data.new_value_data(), data)
    }

    /// Build a function definition.
    fn function_def(&mut self, ty: Type) -> Result<Function, BuildError> {
        let data = FunctionData::new(ty, FunctionKind::Definition);
        let function = self.add_function(data.new_value_data(), data)?;
        Ok(function)
    }

    /// Build a function declaration.
    fn function_decl(&mut self, ty: Type) -> Result<Function, BuildError> {
        let data = FunctionData::new(ty, FunctionKind::Declaration);
        let function = self.add_function(data.new_value_data(), data)?;
        Ok(function)
    }

    /// Build a intrinsic function.
    fn function_intrinsic(&mut self, ty: Type) -> Result<Function, BuildError> {
        let data = FunctionData::new(ty, FunctionKind::Intrinsic);
        let function = self.add_function(data.new_value_data(), data)?;
        Ok(function)
    }
}

pub struct LocalBuilder<'a> {
    dfg: &'a mut DataFlowGraph,
}

impl LocalBuilder<'_> {
    pub fn new(dfg: &mut DataFlowGraph) -> LocalBuilder {
        LocalBuilder { dfg }
    }
}

impl QueryValueData for LocalBuilder<'_> {
    fn value_type(&self, value: Value) -> Result<Type, BuildError> {
        self.dfg
            .with_value_data(value, |data| data.ty())
            .ok_or(BuildError::ValueNotFound)
    }
    fn is_value_const(&self, value: Value) -> Result<bool, BuildError> {
        self.dfg
            .with_value_data(value, |data| data.kind().is_const())
            .ok_or(BuildError::ValueNotFound)
    }
    fn is_value_block_param(&self, value: Value) -> Result<bool, BuildError> {
        self.dfg
            .with_value_data(value, |data| data.kind().is_block_param())
            .ok_or(BuildError::ValueNotFound)
    }
}

impl QueryBlockData for LocalBuilder<'_> {
    fn block_data(&self, block: Block) -> Result<&BlockData, BuildError> {
        self.dfg.block_data(block).ok_or(BuildError::BlockNotFound)
    }
}

impl AddValue for LocalBuilder<'_> {
    fn add_value(&mut self, data: ValueData) -> Result<Value, BuildError> {
        if data.kind().is_global() {
            return Err(BuildError::InvalidGlobalValue);
        }
        Ok(self.dfg.add_value(data))
    }
}

impl AddBlock for LocalBuilder<'_> {
    fn add_block(&mut self, data: BlockData) -> Result<Block, BuildError> {
        Ok(self.dfg.add_block(data))
    }
}

impl QueryDfgData for LocalBuilder<'_> {}

impl BuildNonAggregateConstant for LocalBuilder<'_> {}

impl BuildLocalValue for LocalBuilder<'_> {}

impl BuildBlock for LocalBuilder<'_> {}

pub struct GlobalBuilder<'a> {
    module: &'a mut Module,
}

impl GlobalBuilder<'_> {
    pub fn new(module: &mut Module) -> GlobalBuilder {
        GlobalBuilder { module }
    }
}

impl QueryValueData for GlobalBuilder<'_> {
    fn value_type(&self, value: Value) -> Result<Type, BuildError> {
        self.module
            .with_value_data(value, |data| data.ty())
            .ok_or(BuildError::ValueNotFound)
    }

    fn is_value_const(&self, value: Value) -> Result<bool, BuildError> {
        self.module
            .with_value_data(value, |data| data.kind().is_const())
            .ok_or(BuildError::ValueNotFound)
    }

    fn is_value_block_param(&self, value: Value) -> Result<bool, BuildError> {
        self.module
            .with_value_data(value, |data| data.kind().is_block_param())
            .ok_or(BuildError::ValueNotFound)
    }
}

impl AddValue for GlobalBuilder<'_> {
    fn add_value(&mut self, data: ValueData) -> Result<Value, BuildError> {
        Ok(self.module.add_global_slot(data))
    }
}

impl BuildNonAggregateConstant for GlobalBuilder<'_> {}

impl BuildAggregateConstant for GlobalBuilder<'_> {}

impl BuildGlobalValue for GlobalBuilder<'_> {
    fn add_function(
        &mut self,
        value_data: ValueData,
        function_data: FunctionData,
    ) -> Result<Function, BuildError> {
        Ok(self.module.add_function(value_data, function_data))
    }
}
