use super::{
    block::{Block, BlockCall, BlockData},
    function::{Function, FunctionData},
    inst::{BinaryOp, Inst, InstData, UnaryOp},
    layout::Layout,
    module::Module,
    ty::{TyKind, Type},
    value::{Value, ValueData, ValueKind},
};

pub struct Builder<'a> {
    module: &'a mut Module,

    curr_fn: Option<Function>,
    curr_block: Option<Block>,
}

impl<'a> Builder<'a> {
    pub fn new(module: &'a mut Module) -> Self {
        Self {
            module,
            curr_fn: None,
            curr_block: None,
        }
    }

    fn add_value(&mut self, data: ValueData) -> Value {
        let value = self.module.allocate_id();
        self.module.values.insert(value.clone(), data);
        return value;
    }

    fn add_inst(&mut self, inst: Inst, data: InstData) {
        self.module.insts.insert(inst, data);
    }

    fn add_fn(&mut self, function: Function, data: FunctionData) {
        self.module.functions.insert(function, data);
    }

    fn add_block(&mut self, block: Block, data: BlockData) {
        self.module.blocks.insert(block, data);
    }

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

    pub fn get_value_type(&self, value: &Value) -> Type {
        return self.module.values.get(value).unwrap().ty.clone();
    }

    pub fn get_ret_type(&self, fn_ty: &Type) -> Type {
        if let TyKind::Fn(_, ret) = fn_ty.kind() {
            return ret.clone();
        } else {
            panic!("type is not a function");
        }
    }

    pub fn create_fn(&mut self, name: String, params: Vec<Type>, ret: Type) -> Value {
        let ty = Type::mk_fn(params, ret);

        let value = self.add_value(ValueData::new(ty.clone(), ValueKind::Function));

        self.add_fn(
            value.clone().into(),
            FunctionData {
                name,
                ty,
                layout: Layout::new(),
            },
        );

        return value;
    }

    pub fn create_block(&mut self, params: Vec<Value>) -> Value {
        let value = self.add_value(ValueData::new(Type::mk_void(), ValueKind::Block));
        self.add_block(value.clone().into(), BlockData::new(params));
        return value;
    }

    pub fn set_curr_fn(&mut self, function: &Value) {
        let value_data = self.module.values.get(function).unwrap();
        if let ValueKind::Function = value_data.kind {
            self.curr_fn = Some(function.clone().into());
        } else {
            assert!(false, "value is not a function");
        }
    }

    pub fn set_curr_block(&mut self, block: &Value) {
        let value_data = self.module.values.get(block).unwrap();
        if let ValueKind::Function = value_data.kind {
            self.curr_block = Some(block.clone().into());
        } else {
            assert!(false, "value is not a block");
        }
    }

    pub fn alloca(&mut self, ty: Type) -> Value {
        let value = self.add_value(ValueData::new(Type::mk_ptr(), ValueKind::Inst));
        self.add_inst(value.clone().into(), InstData::Alloca { ty });
        return value;
    }

    pub fn load(&mut self, ty: Type, addr: Value) -> Value {
        let value = self.add_value(ValueData::new(ty.clone(), ValueKind::Inst));
        self.add_inst(value.clone().into(), InstData::Load { ty, addr });
        return value;
    }

    pub fn store(&mut self, val: Value, addr: Value) -> Value {
        let value = self.add_value(ValueData::new(Type::mk_void(), ValueKind::Inst));
        self.add_inst(value.clone().into(), InstData::Store { val, addr });
        return value;
    }

    pub fn binary(&mut self, op: BinaryOp, lhs: Value, rhs: Value) -> Value {
        let value = self.add_value(ValueData::new(self.get_value_type(&lhs), ValueKind::Inst));
        self.add_inst(value.clone().into(), InstData::Binary { op, lhs, rhs });
        return value;
    }

    pub fn unary(&mut self, op: UnaryOp, val: Value) -> Value {
        let value = self.add_value(ValueData::new(self.get_value_type(&val), ValueKind::Inst));
        self.add_inst(value.clone().into(), InstData::Unary { op, val });
        return value;
    }

    pub fn br(&mut self, block: Block, args: Vec<Value>) -> Value {
        let value = self.add_value(ValueData::new(Type::mk_void(), ValueKind::Inst));
        self.add_inst(
            value.clone().into(),
            InstData::Br {
                dst: BlockCall::new(block, args),
            },
        );
        return value;
    }

    pub fn condbr(
        &mut self,
        cond: Value,
        block_then: Block,
        block_else: Block,
        args_then: Vec<Value>,
        args_else: Vec<Value>,
    ) -> Value {
        let value = self.add_value(ValueData::new(Type::mk_void(), ValueKind::Inst));
        self.add_inst(
            value.clone().into(),
            InstData::CondBr {
                cond,
                dst_then: BlockCall::new(block_then, args_then),
                dst_else: BlockCall::new(block_else, args_else),
            },
        );
        return value;
    }

    pub fn ret(&mut self, val: Option<Value>) -> Value {
        let value = self.add_value(ValueData::new(Type::mk_void(), ValueKind::Inst));
        self.add_inst(value.clone().into(), InstData::Ret { val });
        return value;
    }

    pub fn call(&mut self, fn_ty: Type, fn_val: Value, args: Vec<Value>) -> Value {
        let value = self.add_value(ValueData::new(self.get_ret_type(&fn_ty), ValueKind::Inst));
        self.add_inst(
            value.clone().into(),
            InstData::Call {
                fn_ty,
                fn_val,
                args,
            },
        );
        return value;
    }
}
