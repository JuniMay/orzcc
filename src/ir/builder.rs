use super::{
    entities::{BlockData, ValueData, ValueKind},
    module::{DataFlowGraph, Module},
    types::Type,
    values::{
        Alloc, Binary, BinaryOp, Block, Branch, Call, GetElemPtr, Jump, Load, Return, Store, Unary,
        UnaryOp, Value,
    },
};

/// Builder for local values.
pub struct LocalValueBuilder<'a> {
    /// The data flow graph to add the values.
    dfg: &'a mut DataFlowGraph,
}

/// Error type for local value builder.
pub enum LocalValueBuilderErr {
    /// Adding global value in local builder.
    GlobalValueInLocalValueBuilder,

    /// Invalid value type.
    ///
    /// This can be used when the value type is not valid for the operation/construction,
    /// e.g. a function type provided for zero/bytes/...
    InvalidType,

    /// Invalid value kind.
    ///
    /// For non-constant value in aggregated constant.
    InvalidKind,

    /// Incompatible value type.
    ///
    /// This can be used when two values are not compatible, e.g. in binary operations.
    IncompatibleType,

    /// Incompatible array size.
    IncompatibleArraySize,

    /// Incompatible array element type.
    IncompatibleArrayElemType,

    /// Incompatible struct size.
    IncompatibleStructFieldNumber,

    /// Incompatible struct field type.
    IncompatibleStructFieldType,

    /// Value not found.
    ///
    /// The value is not found in the data flow graph.
    ValueNotFound,

    /// Block not found.
    ///
    /// The block is not found in the data flow graph.
    BlockNotFound,

    /// Incompatible block argument number.
    IncompatibleBlockArgNumber,

    /// Incompatible block argument type.
    IncompatibleBlockArgType,
}

pub struct BlockBuilder<'a> {
    dfg: &'a mut DataFlowGraph,
}

pub struct GlobalValueBuilder<'a> {
    module: &'a mut Module,
}

pub struct CustomTypeBuilder<'a> {
    module: &'a mut Module,
}

impl LocalValueBuilder<'_> {
    pub fn new(dfg: &mut DataFlowGraph) -> LocalValueBuilder {
        LocalValueBuilder { dfg }
    }

    fn add_value(&mut self, data: ValueData) -> Result<Value, LocalValueBuilderErr> {
        if data.kind().is_global() {
            return Err(LocalValueBuilderErr::GlobalValueInLocalValueBuilder);
        } else {
            Ok(self.dfg.add_value(data))
        }
    }

    fn value_data(&self, value: Value) -> Result<&ValueData, LocalValueBuilderErr> {
        self.dfg
            .value_data(value)
            .ok_or(LocalValueBuilderErr::ValueNotFound)
    }

    fn block_data(&self, block: Block) -> Result<&BlockData, LocalValueBuilderErr> {
        self.dfg
            .block_data(block)
            .ok_or(LocalValueBuilderErr::BlockNotFound)
    }

    pub fn block_arg(&mut self, ty: Type) -> Result<Value, LocalValueBuilderErr> {
        self.add_value(ValueData::new(ty, ValueKind::BlockArg))
    }

    pub fn zero(&mut self, ty: Type) -> Result<Value, LocalValueBuilderErr> {
        if !ty.is_zero_initializable() {
            return Err(LocalValueBuilderErr::InvalidType);
        }
        self.add_value(ValueData::new(ty, ValueKind::Zero))
    }

    pub fn undef(&mut self, ty: Type) -> Result<Value, LocalValueBuilderErr> {
        self.add_value(ValueData::new(ty, ValueKind::Undef))
    }

    pub fn bytes(&mut self, ty: Type, bytes: Vec<u8>) -> Result<Value, LocalValueBuilderErr> {
        if !ty.is_numeric() && !ty.is_ptr() {
            return Err(LocalValueBuilderErr::InvalidType);
        }
        self.add_value(ValueData::new(ty, ValueKind::Bytes(bytes)))
    }

    pub fn array(&mut self, ty: Type, values: Vec<Value>) -> Result<Value, LocalValueBuilderErr> {
        let (size, elem_ty) = ty.as_array().ok_or(LocalValueBuilderErr::InvalidType)?;

        if values.len() != size {
            return Err(LocalValueBuilderErr::IncompatibleArraySize);
        }

        for value in &values {
            let data = self.value_data(*value)?;
            if data.ty() != elem_ty {
                return Err(LocalValueBuilderErr::IncompatibleArrayElemType);
            }
            if !data.kind().is_const() {
                return Err(LocalValueBuilderErr::InvalidKind);
            }
        }

        self.add_value(ValueData::new(ty, ValueKind::Array(values)))
    }

    pub fn struct_(&mut self, ty: Type, values: Vec<Value>) -> Result<Value, LocalValueBuilderErr> {
        let fields = ty.as_struct().ok_or(LocalValueBuilderErr::InvalidType)?;

        if fields.len() != values.len() {
            return Err(LocalValueBuilderErr::IncompatibleStructFieldNumber);
        }

        for (field_ty, value) in fields.iter().zip(&values) {
            let data = self.value_data(*value)?;
            if data.ty() != field_ty {
                return Err(LocalValueBuilderErr::IncompatibleStructFieldType);
            }
            if !data.kind().is_const() {
                return Err(LocalValueBuilderErr::InvalidKind);
            }
        }

        self.add_value(ValueData::new(ty, ValueKind::Struct(values)))
    }

    pub fn alloc(&mut self, ty: Type) -> Result<Value, LocalValueBuilderErr> {
        self.add_value(Alloc::new_value_data(ty))
    }

    pub fn load(&mut self, ty: Type, ptr: Value) -> Result<Value, LocalValueBuilderErr> {
        let ptr_data = self.value_data(ptr)?;
        if !ptr_data.ty().is_ptr() {
            return Err(LocalValueBuilderErr::InvalidType);
        }
        self.add_value(Load::new_value_data(ty, ptr))
    }

    pub fn store(&mut self, val: Value, ptr: Value) -> Result<Value, LocalValueBuilderErr> {
        let _val_data = self.value_data(val)?;
        let ptr_data = self.value_data(ptr)?;

        if !ptr_data.ty().is_ptr() {
            return Err(LocalValueBuilderErr::InvalidType);
        }

        self.add_value(Store::new_value_data(val, ptr))
    }

    pub fn binary(
        &mut self,
        op: BinaryOp,
        lhs: Value,
        rhs: Value,
    ) -> Result<Value, LocalValueBuilderErr> {
        let lhs_data = self.value_data(lhs)?;
        let rhs_data = self.value_data(rhs)?;

        if op.require_int() {
            if !lhs_data.ty().is_int() || !rhs_data.ty().is_int() {
                return Err(LocalValueBuilderErr::InvalidType);
            }
        }

        if op.require_float() {
            if !lhs_data.ty().is_float() || !rhs_data.ty().is_float() {
                return Err(LocalValueBuilderErr::InvalidType);
            }
        }

        if op.require_same_type() {
            if lhs_data.ty() != rhs_data.ty() {
                return Err(LocalValueBuilderErr::IncompatibleType);
            }
        }

        self.add_value(Binary::new_value_data(lhs_data.ty().clone(), op, lhs, rhs))
    }

    pub fn unary(&mut self, op: UnaryOp, val: Value) -> Result<Value, LocalValueBuilderErr> {
        let val_data = self.value_data(val)?;

        if op.require_int() {
            if !val_data.ty().is_int() {
                return Err(LocalValueBuilderErr::InvalidType);
            }
        }

        if op.require_float() {
            if !val_data.ty().is_float() {
                return Err(LocalValueBuilderErr::InvalidType);
            }
        }

        self.add_value(Unary::new_value_data(val_data.ty().clone(), op, val))
    }

    pub fn jump(&mut self, dst: Block, args: Vec<Value>) -> Result<Value, LocalValueBuilderErr> {
        let block_data = self.block_data(dst)?;

        if block_data.params().len() != args.len() {
            return Err(LocalValueBuilderErr::IncompatibleBlockArgNumber);
        }

        for (param, arg) in block_data.params().iter().zip(&args) {
            let param_data = self.value_data(*param)?;
            let arg_data = self.value_data(*arg)?;

            if param_data.ty() != arg_data.ty() {
                return Err(LocalValueBuilderErr::IncompatibleBlockArgType);
            }
        }

        self.add_value(Jump::new_value_data(dst, args))
    }

    pub fn branch(
        &mut self,
        cond: Value,
        then_dst: Block,
        else_dst: Block,
        then_args: Vec<Value>,
        else_args: Vec<Value>,
    ) -> Result<Value, LocalValueBuilderErr> {
        let cond_data = self.value_data(cond)?;

        if !cond_data.ty().is_int() {
            return Err(LocalValueBuilderErr::InvalidType);
        }

        let then_block_data = self.block_data(then_dst)?;
        let else_block_data = self.block_data(else_dst)?;

        if then_block_data.params().len() != then_args.len() {
            return Err(LocalValueBuilderErr::IncompatibleBlockArgNumber);
        }

        if else_block_data.params().len() != else_args.len() {
            return Err(LocalValueBuilderErr::IncompatibleBlockArgNumber);
        }

        for (param, arg) in then_block_data.params().iter().zip(&then_args) {
            let param_data = self.value_data(*param)?;
            let arg_data = self.value_data(*arg)?;

            if param_data.ty() != arg_data.ty() {
                return Err(LocalValueBuilderErr::IncompatibleBlockArgType);
            }
        }

        for (param, arg) in else_block_data.params().iter().zip(&else_args) {
            let param_data = self.value_data(*param)?;
            let arg_data = self.value_data(*arg)?;

            if param_data.ty() != arg_data.ty() {
                return Err(LocalValueBuilderErr::IncompatibleBlockArgType);
            }
        }

        self.add_value(Branch::new_value_data(
            cond, then_dst, else_dst, then_args, else_args,
        ))
    }

    pub fn return_(&mut self, val: Option<Value>) -> Result<Value, LocalValueBuilderErr> {
        // type check of return is not performed here
        self.add_value(Return::new_value_data(val))
    }

    pub fn call(
        &mut self,
        ret_ty: Type,
        callee: Value,
        args: Vec<Value>,
    ) -> Result<Value, LocalValueBuilderErr> {
        let callee_data = self.value_data(callee)?;

        if !callee_data.ty().is_fn() && !callee_data.ty().is_ptr() {
            return Err(LocalValueBuilderErr::InvalidType);
        }

        self.add_value(Call::new_value_data(ret_ty, callee, args))
    }

    pub fn get_elem_ptr(
        &mut self,
        ptr: Value,
        ty: Type,
        indices: Vec<Value>,
    ) -> Result<Value, LocalValueBuilderErr> {
        let ptr_data = self.value_data(ptr)?;

        if !ptr_data.ty().is_ptr() {
            return Err(LocalValueBuilderErr::InvalidType);
        }

        self.add_value(GetElemPtr::new_value_data(ptr, ty, indices))
    }
}

impl BlockBuilder<'_> {
    pub fn new(dfg: &mut DataFlowGraph) -> BlockBuilder {
        BlockBuilder { dfg }
    }
}

impl GlobalValueBuilder<'_> {
    pub fn new(module: &mut Module) -> GlobalValueBuilder {
        GlobalValueBuilder { module }
    }
}

impl CustomTypeBuilder<'_> {
    pub fn new(module: &mut Module) -> CustomTypeBuilder {
        CustomTypeBuilder { module }
    }
}
