use std::{error::Error, fmt};

use super::{
    entities::{BlockData, FunctionData, FunctionKind, ValueData, ValueKind},
    module::{DataFlowGraph, Module},
    types::{Type, TypeKind},
    values::{
        Alloc, Binary, BinaryOp, Block, Branch, Call, Cast, Function, GetElemPtr, GlobalSlot, Jump, Load, Return, Store, Unary, UnaryOp, Value
    },
};

#[derive(Debug)]
pub enum BuilderErr {
    /// Value not found.
    ///
    /// The value is not found in the data flow graph.
    ValueNotFound,

    /// Block not found.
    ///
    /// The block is not found in the data flow graph.
    BlockNotFound,

    /// Type not found.
    TypeNotFound,

    /// Invalid value type.
    ///
    /// This can be used when the value type is not valid for the operation/construction,
    /// e.g. a function type provided for zero/bytes/...
    InvalidType(Type),

    InvalidKind,

    /// Trying to build aggregated constant with non-constant value.
    InvalidMutability,

    /// Global value in local data flow graph.
    InvalidGlobalValue,

    /// Local value in global data flow graph.
    InvalidLocalValue,

    /// Incompatible value type.
    ///
    /// This can be used when two values are not compatible, e.g. in binary operations.
    IncompatibleType,

    /// Incompatible array size.
    IncompatibleArraySize,

    /// Incompatible array element type.
    IncompatibleArrayElemType(Type, Type),

    /// Incompatible struct field number.
    IncompatibleStructFieldNumber,

    /// Incompatible struct field type.
    IncompatibleStructFieldType,

    /// Incompatible block argument number.
    IncompatibleBlockArgNumber,

    /// Incompatible block argument type.
    IncompatibleBlockArgType,

    /// Duplicated name.
    NameDuplicated,
}

impl fmt::Display for BuilderErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BuilderErr::*;
        match self {
            ValueNotFound => write!(f, "value not found"),
            BlockNotFound => write!(f, "block not found"),
            TypeNotFound => write!(f, "type not found"),
            InvalidType(ty) => write!(f, "invalid value type {}", ty),
            InvalidKind => write!(f, "invalid value kind"),
            InvalidMutability => write!(f, "invalid mutability"),
            InvalidGlobalValue => write!(f, "invalid global value"),
            InvalidLocalValue => write!(f, "invalid local value"),
            IncompatibleType => write!(f, "incompatible value type"),
            IncompatibleArraySize => write!(f, "incompatible array size"),
            IncompatibleArrayElemType(elem_type, value_type) => write!(
                f,
                "incompatible array element type: {} and {}",
                elem_type, value_type
            ),
            IncompatibleStructFieldNumber => {
                write!(f, "incompatible struct field number")
            }
            IncompatibleStructFieldType => write!(f, "incompatible struct field type"),
            IncompatibleBlockArgNumber => {
                write!(f, "incompatible block argument number")
            }
            IncompatibleBlockArgType => write!(f, "incompatible block argument type"),
            NameDuplicated => write!(f, "duplicated name"),
        }
    }
}

impl Error for BuilderErr {}

pub trait QueryValueData {
    /// Get the type of a value.
    fn value_type(&self, value: Value) -> Result<Type, BuilderErr>;

    /// Check if a value is a constant.
    fn is_value_const(&self, value: Value) -> Result<bool, BuilderErr>;

    /// Check if a value is a block parameter.
    fn is_value_block_param(&self, value: Value) -> Result<bool, BuilderErr>;
}

pub trait QueryBlockData {
    fn block_data(&self, block: Block) -> Result<&BlockData, BuilderErr>;
}

pub trait QueryDfgData: QueryValueData + QueryBlockData {}

pub trait AddValue {
    fn add_value(&mut self, data: ValueData) -> Result<Value, BuilderErr>;
}

pub trait AddBlock {
    fn add_block(&mut self, data: BlockData) -> Result<Block, BuilderErr>;
}

pub trait QueryCustomType {
    fn custom_type(&self, name: String) -> Result<Type, BuilderErr>;
}

pub trait ConstantBuilder: QueryValueData + AddValue + QueryCustomType {
    /// Build a zero constant.
    fn zero(&mut self, ty: Type) -> Result<Value, BuilderErr> {
        if !ty.is_zero_initializable() {
            return Err(BuilderErr::InvalidType(ty.clone()));
        }
        self.add_value(ValueData::new(ty, ValueKind::Zero))
    }

    /// Build an undef constant.
    fn undef(&mut self, ty: Type) -> Result<Value, BuilderErr> {
        self.add_value(ValueData::new(ty, ValueKind::Undef))
    }

    /// Build a bytes constant.
    fn bytes(&mut self, ty: Type, bytes: Vec<u8>) -> Result<Value, BuilderErr> {
        if !ty.is_numeric() && !ty.is_ptr() {
            return Err(BuilderErr::InvalidType(ty.clone()));
        }
        self.add_value(ValueData::new(ty, ValueKind::Bytes(bytes)))
    }

    /// Build an integer constant of i32 type.
    fn integer(&mut self, value: i32) -> Result<Value, BuilderErr> {
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
    fn float(&mut self, value: f32) -> Result<Value, BuilderErr> {
        let bytes = value.to_le_bytes().to_vec();
        self.bytes(Type::mk_float(), bytes)
    }

    /// Build an array constant.
    fn array(&mut self, ty: Type, values: Vec<Value>) -> Result<Value, BuilderErr> {
        let (size, elem_type) = ty.as_array().ok_or(BuilderErr::InvalidType(ty.clone()))?;

        if values.len() != size {
            return Err(BuilderErr::IncompatibleArraySize);
        }

        for value in &values {
            if self.value_type(*value)? != elem_type.clone() {
                return Err(BuilderErr::IncompatibleArrayElemType(
                    elem_type.clone(),
                    self.value_type(*value)?,
                ));
            }
            if !self.is_value_const(*value)? {
                return Err(BuilderErr::InvalidMutability);
            }
        }

        self.add_value(ValueData::new(ty, ValueKind::Array(values)))
    }

    /// Build a struct constant.
    fn struct_(&mut self, ty: Type, values: Vec<Value>) -> Result<Value, BuilderErr> {
        let actual_ty = match ty.kind() {
            TypeKind::Struct(_) => ty.clone(),
            TypeKind::Type(name) => self.custom_type(name.clone())?,
            _ => return Err(BuilderErr::InvalidType(ty.clone())),
        };

        let fields = actual_ty
            .as_struct()
            .ok_or(BuilderErr::InvalidType(ty.clone()))?;

        if fields.len() != values.len() {
            return Err(BuilderErr::IncompatibleStructFieldNumber);
        }

        for (field_type, value) in fields.iter().zip(&values) {
            if self.value_type(*value)? != field_type.clone() {
                return Err(BuilderErr::IncompatibleStructFieldType);
            }
            if !self.is_value_const(*value)? {
                return Err(BuilderErr::InvalidMutability);
            }
        }

        self.add_value(ValueData::new(ty, ValueKind::Struct(values)))
    }
}

pub trait LocalValueBuilder: QueryDfgData + AddValue + ConstantBuilder {
    /// Build a block parameter.
    fn block_param(&mut self, ty: Type) -> Result<Value, BuilderErr> {
        self.add_value(ValueData::new(ty, ValueKind::BlockParam))
    }

    /// Build an alloc instruction.
    fn alloc(&mut self, ty: Type) -> Result<Value, BuilderErr> {
        self.add_value(Alloc::new_value_data(ty))
    }

    /// Build a load instruction.
    fn load(&mut self, ty: Type, ptr: Value) -> Result<Value, BuilderErr> {
        if !self.value_type(ptr)?.is_ptr() {
            return Err(BuilderErr::InvalidType(ty.clone()));
        }
        self.add_value(Load::new_value_data(ty, ptr))
    }

    /// Build a cast instruction
    fn cast(&mut self, ty: Type, val: Value) -> Result<Value, BuilderErr> {
        self.add_value(Cast::new_value_data(ty, val))
    }

    /// Build a store instruction.
    fn store(&mut self, val: Value, ptr: Value) -> Result<Value, BuilderErr> {
        if !self.value_type(ptr)?.is_ptr() {
            return Err(BuilderErr::InvalidType(self.value_type(ptr)?));
        }

        self.add_value(Store::new_value_data(val, ptr))
    }

    /// Build a binary instruction.
    fn binary(&mut self, op: BinaryOp, lhs: Value, rhs: Value) -> Result<Value, BuilderErr> {
        let lhs_type = self.value_type(lhs)?;
        let rhs_type = self.value_type(rhs)?;

        if op.require_int() && (!lhs_type.is_int() || !rhs_type.is_int()) {
            return Err(BuilderErr::InvalidType(lhs_type.clone()));
        }

        if op.require_float() && (!lhs_type.is_float() || !rhs_type.is_float()) {
            return Err(BuilderErr::InvalidType(lhs_type.clone()));
        }

        if op.require_same_type() && lhs_type != rhs_type {
            return Err(BuilderErr::IncompatibleType);
        }

        let res_type = match op {
            BinaryOp::ICmp(_) | BinaryOp::FCmp(_) => Type::mk_int(1),
            _ => lhs_type,
        };

        self.add_value(Binary::new_value_data(res_type, op, lhs, rhs))
    }

    /// Build a unary instruction.
    fn unary(&mut self, op: UnaryOp, val: Value) -> Result<Value, BuilderErr> {
        let val_type = self.value_type(val)?;

        if op.require_int() && !val_type.is_int() {
            return Err(BuilderErr::InvalidType(val_type.clone()));
        }

        if op.require_float() && !val_type.is_float() {
            return Err(BuilderErr::InvalidType(val_type.clone()));
        }

        self.add_value(Unary::new_value_data(val_type, op, val))
    }

    /// Build a jump instruction.
    fn jump(&mut self, dst: Block, args: Vec<Value>) -> Result<Value, BuilderErr> {
        let block_data = self.block_data(dst)?;

        if block_data.params().len() != args.len() {
            return Err(BuilderErr::IncompatibleBlockArgNumber);
        }

        for (param, arg) in block_data.params().iter().zip(&args) {
            let param_type = self.value_type(*param)?;
            let arg_type = self.value_type(*arg)?;

            if param_type != arg_type {
                return Err(BuilderErr::IncompatibleBlockArgType);
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
    ) -> Result<Value, BuilderErr> {
        if !self.value_type(cond)?.is_int() {
            return Err(BuilderErr::InvalidType(self.value_type(cond)?));
        }

        let then_block_data = self.block_data(then_dst)?;
        let else_block_data = self.block_data(else_dst)?;

        if then_block_data.params().len() != then_args.len() {
            return Err(BuilderErr::IncompatibleBlockArgNumber);
        }

        if else_block_data.params().len() != else_args.len() {
            return Err(BuilderErr::IncompatibleBlockArgNumber);
        }

        for (param, arg) in then_block_data.params().iter().zip(&then_args) {
            let param_type = self.value_type(*param)?;
            let arg_type = self.value_type(*arg)?;

            if param_type != arg_type {
                return Err(BuilderErr::IncompatibleBlockArgType);
            }
        }

        for (param, arg) in else_block_data.params().iter().zip(&else_args) {
            let param_type = self.value_type(*param)?;
            let arg_type = self.value_type(*arg)?;

            if param_type != arg_type {
                return Err(BuilderErr::IncompatibleBlockArgType);
            }
        }

        self.add_value(Branch::new_value_data(
            cond, then_dst, else_dst, then_args, else_args,
        ))
    }

    /// Build a return instruction.
    fn return_(&mut self, val: Option<Value>) -> Result<Value, BuilderErr> {
        // type check of return is not performed here
        self.add_value(Return::new_value_data(val))
    }

    /// Build a call instruction.
    fn call(&mut self, ret_ty: Type, callee: Value, args: Vec<Value>) -> Result<Value, BuilderErr> {
        let callee_type = self.value_type(callee)?;

        if !callee_type.is_function() && !callee_type.is_ptr() {
            return Err(BuilderErr::InvalidType(callee_type));
        }

        self.add_value(Call::new_value_data(ret_ty, callee, args))
    }

    /// Build a get element pointer instruction.
    fn getelemptr(
        &mut self,
        ptr: Value,
        ty: Type,
        indices: Vec<Value>,
    ) -> Result<Value, BuilderErr> {
        if !self.value_type(ptr)?.is_ptr() {
            return Err(BuilderErr::InvalidType(self.value_type(ptr)?));
        }

        self.add_value(GetElemPtr::new_value_data(ptr, ty, indices))
    }
}

pub trait LocalBlockBuilder: QueryDfgData + AddBlock {
    /// Build a block.
    fn block(&mut self, params: Vec<Value>) -> Result<Block, BuilderErr> {
        for param in params.iter() {
            if !self.is_value_block_param(*param)? {
                return Err(BuilderErr::InvalidKind);
            }
        }

        self.add_block(BlockData::new(params))
    }
}

pub trait GlobalValueBuilder: QueryValueData + AddValue + ConstantBuilder {
    /// Add a constructed function to the module.
    fn add_function(
        &mut self,
        value_data: ValueData,
        function_data: FunctionData,
    ) -> Result<Function, BuilderErr>;

    /// Build a global slot.
    fn global_slot(&mut self, init: Value, mutable: bool) -> Result<Value, BuilderErr> {
        if !self.is_value_const(init)? {
            return Err(BuilderErr::InvalidMutability);
        }

        self.add_value(GlobalSlot::new_value_data(
            Type::mk_ptr(),
            init,
            mutable,
        ))
    }

    /// Build a global function.
    fn function(
        &mut self,
        name: String,
        ty: Type,
        kind: FunctionKind,
    ) -> Result<Function, BuilderErr> {
        let data = FunctionData::new(name, ty.clone(), kind);
        self.add_function(data.new_value_data(), data)
    }

    /// Build a function definition.
    fn function_def(&mut self, name: String, ty: Type) -> Result<Function, BuilderErr> {
        let data = FunctionData::new(name, ty.clone(), FunctionKind::Definition);
        let function = self.add_function(data.new_value_data(), data)?;
        Ok(function)
    }

    /// Build a function declaration.
    fn function_decl(&mut self, name: String, ty: Type) -> Result<Function, BuilderErr> {
        let data = FunctionData::new(name, ty.clone(), FunctionKind::Declaration);
        let function = self.add_function(data.new_value_data(), data)?;
        Ok(function)
    }

    /// Build a intrinsic function.
    fn function_intrinsic(&mut self, name: String, ty: Type) -> Result<Function, BuilderErr> {
        let data = FunctionData::new(name, ty.clone(), FunctionKind::Intrinsic);
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
    fn value_type(&self, value: Value) -> Result<Type, BuilderErr> {
        self.dfg
            .with_value_data(value, |data| data.ty().clone())
            .ok_or(BuilderErr::ValueNotFound)
    }
    fn is_value_const(&self, value: Value) -> Result<bool, BuilderErr> {
        self.dfg
            .with_value_data(value, |data| data.kind().is_const())
            .ok_or(BuilderErr::ValueNotFound)
    }
    fn is_value_block_param(&self, value: Value) -> Result<bool, BuilderErr> {
        self.dfg
            .with_value_data(value, |data| data.kind().is_block_param())
            .ok_or(BuilderErr::ValueNotFound)
    }
}

impl QueryBlockData for LocalBuilder<'_> {
    fn block_data(&self, block: Block) -> Result<&BlockData, BuilderErr> {
        self.dfg.block_data(block).ok_or(BuilderErr::BlockNotFound)
    }
}

impl AddValue for LocalBuilder<'_> {
    fn add_value(&mut self, data: ValueData) -> Result<Value, BuilderErr> {
        if data.kind().is_global() {
            return Err(BuilderErr::InvalidGlobalValue);
        }
        Ok(self.dfg.add_value(data))
    }
}

impl AddBlock for LocalBuilder<'_> {
    fn add_block(&mut self, data: BlockData) -> Result<Block, BuilderErr> {
        Ok(self.dfg.add_block(data))
    }
}

impl QueryDfgData for LocalBuilder<'_> {}

impl ConstantBuilder for LocalBuilder<'_> {}

impl LocalValueBuilder for LocalBuilder<'_> {}

impl QueryCustomType for LocalBuilder<'_> {
    fn custom_type(&self, name: String) -> Result<Type, BuilderErr> {
        self.dfg.custom_type(&name).ok_or(BuilderErr::TypeNotFound)
    }
}

impl LocalBlockBuilder for LocalBuilder<'_> {}

pub struct GlobalBuilder<'a> {
    module: &'a mut Module,
}

impl GlobalBuilder<'_> {
    pub fn new(module: &mut Module) -> GlobalBuilder {
        GlobalBuilder { module }
    }
}

impl QueryValueData for GlobalBuilder<'_> {
    fn value_type(&self, value: Value) -> Result<Type, BuilderErr> {
        self.module
            .with_value_data(value, |data| data.ty().clone())
            .ok_or(BuilderErr::ValueNotFound)
    }

    fn is_value_const(&self, value: Value) -> Result<bool, BuilderErr> {
        self.module
            .with_value_data(value, |data| data.kind().is_const())
            .ok_or(BuilderErr::ValueNotFound)
    }

    fn is_value_block_param(&self, value: Value) -> Result<bool, BuilderErr> {
        self.module
            .with_value_data(value, |data| data.kind().is_block_param())
            .ok_or(BuilderErr::ValueNotFound)
    }
}

impl AddValue for GlobalBuilder<'_> {
    fn add_value(&mut self, data: ValueData) -> Result<Value, BuilderErr> {
        Ok(self.module.add_global_slot(data))
    }
}

impl QueryCustomType for GlobalBuilder<'_> {
    fn custom_type(&self, name: String) -> Result<Type, BuilderErr> {
        self.module
            .custom_type(&name)
            .ok_or(BuilderErr::TypeNotFound)
    }
}

impl ConstantBuilder for GlobalBuilder<'_> {}

impl GlobalValueBuilder for GlobalBuilder<'_> {
    fn add_function(
        &mut self,
        value_data: ValueData,
        function_data: FunctionData,
    ) -> Result<Function, BuilderErr> {
        Ok(self.module.add_function(value_data, function_data))
    }
}
