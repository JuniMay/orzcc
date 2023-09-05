use super::{
    block::{BlockCall, BlockData},
    constant::ConstantData,
    function::FunctionData,
    global::GlobalData,
    instructions::{BinaryOp, InstData, UnaryOp},
    layout::{Layout, LayoutOpErr},
    module::Module,
    types::{TyKind, Type},
    value::{Block, Constant, Function, Global, Inst, Value, ValueData, ValueKind},
};

/// The builder for constructing an IR module.
pub struct Builder<'a> {
    /// The module.
    module: &'a mut Module,

    /// The layout.
    layout: &'a mut Layout,

    /// Current working function.
    curr_fn: Option<Function>,

    /// Current working block.
    curr_block: Option<Block>,
}

impl<'a> Builder<'a> {
    pub fn new(module: &'a mut Module, layout: &'a mut Layout) -> Self {
        Self {
            module,
            layout,
            curr_fn: None,
            curr_block: None,
        }
    }

    /// Add a value by its data into the module and return the value.
    fn add_value(&mut self, ty: Type, kind: ValueKind) -> Value {
        let value = self.module.allocate_id();
        self.module.values.insert(value, ValueData::new(ty, kind));
        value
    }

    /// Add a inst and its data into the module.
    fn add_inst(&mut self, inst: Inst, data: InstData) {
        self.module.insts.insert(inst, data);
    }

    /// Add a function and its data into the module.
    fn add_fn(&mut self, function: Function, data: FunctionData) {
        self.module.functions.insert(function, data);
    }

    /// Add a block and its data into the module.
    fn add_block(&mut self, block: Block, data: BlockData) {
        self.module.blocks.insert(block, data);
    }

    /// Add a global and its data into the module.
    fn add_global(&mut self, global: Global, data: GlobalData) {
        self.module.globals.insert(global, data);
    }

    /// Add a constant and its data into the module.
    fn add_constant(&mut self, constant: Constant, data: ConstantData) {
        self.module.constants.insert(constant, data);
    }

    /// Add an identified type into the module.
    pub fn add_identified_type(&mut self, ty: &Type) {
        if let TyKind::Struct(name, _) = ty.kind() {
            if let Some(name) = name {
                self.module
                    .identified_types
                    .insert(name.clone(), ty.clone());
            } else {
                panic!("type is non-identified");
            }
        } else {
            panic!("type is not a struct");
        }
    }

    /// Get the type of a value.
    pub fn get_value_type(&self, value: &Value) -> Type {
        self.module.values.get(value).unwrap().ty.clone()
    }

    /// Get the return type from a function type
    pub fn get_ret_type(&self, fn_ty: &Type) -> Option<Type> {
        if let TyKind::Fn(_, ret) = fn_ty.kind() {
            Some(ret.clone())
        } else {
            None
        }
    }

    pub fn layout(&self) -> &Layout {
        &self.layout
    }

    pub fn layout_mut(&mut self) -> &mut Layout {
        &mut self.layout
    }

    pub fn append_function(&mut self, function: Function) -> Result<&mut Self, LayoutOpErr> {
        self.layout.append_function(function)?;
        Ok(self)
    }

    pub fn append_block(&mut self, block: Block) -> Result<&mut Self, LayoutOpErr> {
        self.layout.append_block(block, self.curr_fn.unwrap())?;
        Ok(self)
    }

    pub fn append_inst(&mut self, inst: Inst) -> Result<&mut Self, LayoutOpErr> {
        self.layout.append_inst(inst, self.curr_block.unwrap())?;
        Ok(self)
    }

    /// Create a new function from name, param types and return type and return the value.
    ///
    /// This does not change the layout.
    pub fn create_fn(&mut self, name: String, params: Vec<Type>, ret: Type) -> Value {
        let ty = Type::mk_fn(params, ret);
        let value = self.add_value(ty.clone(), ValueKind::Function);
        self.add_fn(
            value.into(),
            FunctionData {
                name: name.clone(),
                ty,
            },
        );
        value
    }

    /// Create a new global value from its information and return the value
    ///
    /// This does not change the layout.
    pub fn create_global(
        &mut self,
        name: String,
        ty: Type,
        init: Constant,
        mutable: bool,
    ) -> Value {
        let value = self.add_value(ty.clone(), ValueKind::Global);
        self.add_global(value.into(), GlobalData::new(name, ty, init, mutable));
        value
    }

    /// Create a new block from params.
    ///
    /// This does not change the layout.
    pub fn create_block(&mut self, params: Vec<Value>) -> Value {
        let value = self.add_value(Type::mk_label(), ValueKind::Block);
        self.add_block(value.into(), BlockData::new(params));
        value
    }

    /// Create a new constant from its data
    pub fn create_constant(&mut self, data: ConstantData) -> Value {
        let value = self.add_value(data.ty.clone(), ValueKind::Constant);
        self.add_constant(value.into(), data);
        value
    }

    /// Create a new block param
    pub fn create_param(&mut self, ty: Type) -> Value {
        let value = self.add_value(ty, ValueKind::BlockParam);
        value
    }

    /// Set current function to `function`.
    pub fn set_curr_fn(&mut self, function: Value) -> &mut Self {
        let value_data = self.module.values.get(&function).unwrap();
        if let ValueKind::Function = value_data.kind {
            self.curr_fn = Some(function.into());
        } else {
            assert!(false, "value is not a function");
        }
        self
    }

    /// Set current block to `block`.
    pub fn set_curr_block(&mut self, block: Value) -> &mut Self {
        let value_data = self.module.values.get(&block).unwrap();
        if let ValueKind::Block = value_data.kind {
            self.curr_block = Some(block.into());
        } else {
            assert!(false, "value is not a block");
        }
        self
    }

    pub fn mk_alloc(&mut self, ty: Type) -> Value {
        let value = self.add_value(Type::mk_ptr(), ValueKind::Inst);
        self.add_inst(value.into(), InstData::Alloc { ty });
        value
    }

    pub fn mk_load(&mut self, ty: Type, addr: Value) -> Value {
        let value = self.add_value(ty.clone(), ValueKind::Inst);
        self.add_inst(value.into(), InstData::Load { ty, addr });
        value
    }

    pub fn mk_store(&mut self, val: Value, addr: Value) -> Value {
        let value = self.add_value(Type::mk_void(), ValueKind::Inst);
        self.add_inst(value.into(), InstData::Store { val, addr });
        value
    }

    pub fn mk_binary(&mut self, op: BinaryOp, lhs: Value, rhs: Value) -> Value {
        let value = self.add_value(self.get_value_type(&lhs), ValueKind::Inst);
        self.add_inst(value.into(), InstData::Binary { op, lhs, rhs });
        value
    }

    pub fn mk_unary(&mut self, op: UnaryOp, val: Value) -> Value {
        let value = self.add_value(self.get_value_type(&val), ValueKind::Inst);
        self.add_inst(value.into(), InstData::Unary { op, val });
        value
    }

    pub fn mk_br(&mut self, block: Block, args: Vec<Value>) -> Value {
        let value = self.add_value(Type::mk_void(), ValueKind::Inst);
        self.add_inst(
            value.into(),
            InstData::Br {
                dst: BlockCall::new(block, args),
            },
        );
        value
    }

    pub fn mk_condbr(
        &mut self,
        cond: Value,
        block_then: Block,
        block_else: Block,
        args_then: Vec<Value>,
        args_else: Vec<Value>,
    ) -> Value {
        let value = self.add_value(Type::mk_void(), ValueKind::Inst);
        self.add_inst(
            value.into(),
            InstData::CondBr {
                cond,
                dst_then: BlockCall::new(block_then, args_then),
                dst_else: BlockCall::new(block_else, args_else),
            },
        );
        value
    }

    pub fn mk_ret(&mut self, val: Option<Value>) -> Value {
        let value = self.add_value(Type::mk_void(), ValueKind::Inst);
        self.add_inst(value.into(), InstData::Ret { val });
        value
    }

    pub fn mk_call(&mut self, fn_ty: Type, fn_val: Value, args: Vec<Value>) -> Value {
        let value = self.add_value(
            self.get_ret_type(&fn_ty)
                .expect("`fn_ty` should be a function type"),
            ValueKind::Inst,
        );
        self.add_inst(
            value.into(),
            InstData::Call {
                fn_ty,
                fn_val,
                args,
            },
        );
        value
    }
}
