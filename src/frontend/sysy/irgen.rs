use super::{
    ast::{
        BinaryOp,
        Block,
        BlockItem,
        CompUnit,
        ComptimeVal as Cv,
        ConstDecl,
        ConstDef,
        Decl,
        Expr,
        ExprKind,
        ExprStmt,
        FuncDef,
        FuncFParam,
        Item,
        ReturnStmt,
        Stmt,
        SymbolEntry,
        SymbolTable,
        VarDecl,
        VarDef,
    },
    types::{Type, TypeKind as Tk},
};
use crate::{
    collections::linked_list::LinkedListContainerPtr,
    frontend::sysy::ast::{FuncCall, LVal, UnaryOp},
    ir,
    utils::cfg::CfgRegion,
};

pub fn irgen(ast: &CompUnit, pointer_width: u8) -> ir::Context {
    let mut irgen = IrGenContext::default();
    irgen.ctx.set_pointer_width(pointer_width);

    ast.irgen(&mut irgen);
    irgen.finish()
}

#[derive(Debug, Clone, Copy)]
pub enum IrGenResult {
    Slot(ir::GlobalSlot),
    Value(ir::Value),
}

impl IrGenResult {
    pub fn unwrap_value(self) -> ir::Value {
        match self {
            IrGenResult::Value(val) => val,
            IrGenResult::Slot(_) => unreachable!("expected value"),
        }
    }
}

#[derive(Default)]
pub struct IrGenContext {
    pub ctx: ir::Context,

    pub symtable: SymbolTable,

    pub curr_func: Option<ir::Func>,
    pub curr_func_name: Option<String>,

    pub curr_block: Option<ir::Block>,

    pub loop_entry_stack: Vec<ir::Block>,
    pub loop_exit_stack: Vec<ir::Block>,

    pub curr_ret_slot: Option<ir::Value>,
    pub curr_ret_block: Option<ir::Block>,
}

impl IrGenContext {
    pub fn finish(self) -> ir::Context { self.ctx }

    fn gen_global_comptime(val: &Cv) -> ir::Constant {
        match val {
            Cv::Bool(a) => ir::Constant::from(*a),
            Cv::Int(a) => ir::Constant::from(*a),
            Cv::Float(a) => ir::Constant::from(*a),
            Cv::List(vals) => {
                let mut bytes = Vec::new();
                for v in vals {
                    let c = Self::gen_global_comptime(v);
                    match c.kind() {
                        ir::ConstantKind::Bytes(bs) => bytes.extend(bs),
                        ir::ConstantKind::Zeroinit => {
                            let bytewidth = v.get_type().bytewidth();
                            bytes.resize(bytes.len() + bytewidth, 0);
                        }
                        ir::ConstantKind::Undef => {
                            let bytewidth = v.get_type().bytewidth();
                            bytes.resize(bytes.len() + bytewidth, 0);
                        }
                    }
                }
                ir::Constant::bytes(bytes)
            }
            // the type of zero and undef can be decided by the type of the variable/constants, and
            // will be recorded in [ir::GlobalSlotData]
            Cv::Zeros(_) => ir::Constant::zeroinit(),
            // XXX: global always initialized as zero.
            Cv::Undef(_) => ir::Constant::zeroinit(),
        }
    }

    fn gen_type(&mut self, ty: &Type) -> ir::Ty {
        match ty.kind() {
            Tk::Void => ir::Ty::void(&mut self.ctx),
            Tk::Bool => ir::Ty::int(&mut self.ctx, 1),
            Tk::Int => ir::Ty::int(&mut self.ctx, 32),
            Tk::Float => ir::Ty::float32(&mut self.ctx),
            Tk::Ptr(_) => ir::Ty::ptr(&mut self.ctx),
            Tk::Func(..) => unreachable!("function type should be handled separately"),
            Tk::Array(..) => unreachable!("array type should be handled separately"),
        }
    }

    fn gen_sig(&mut self, ty: &Type) -> ir::Signature {
        if let Tk::Func(params, ret) = ty.kind() {
            let params = params.iter().map(|ty| self.gen_type(ty)).collect();
            let ret = self.gen_type(ret);
            ir::Signature::new(params, vec![ret])
        } else {
            unreachable!("not a function type: {}", ty)
        }
    }

    fn gen_local_comptime(&mut self, val: &Cv) -> ir::Value {
        match val {
            Cv::Bool(a) => {
                let i1 = ir::Ty::int(&mut self.ctx, 1);
                let val = ir::Inst::iconst(&mut self.ctx, *a, i1);
                self.curr_block.unwrap().push_back(&mut self.ctx, val);
                val.result(&self.ctx, 0)
            }
            Cv::Int(a) => {
                let i32 = ir::Ty::int(&mut self.ctx, 32);
                let val = ir::Inst::iconst(&mut self.ctx, *a, i32);
                self.curr_block.unwrap().push_back(&mut self.ctx, val);
                val.result(&self.ctx, 0)
            }
            Cv::Float(a) => {
                let f32 = ir::Ty::float32(&mut self.ctx);
                let val = ir::Inst::fconst(&mut self.ctx, *a, f32);
                self.curr_block.unwrap().push_back(&mut self.ctx, val);
                val.result(&self.ctx, 0)
            }
            Cv::List(_) => unreachable!(),
            Cv::Zeros(ty) => {
                if ty.is_bool() {
                    let i1 = ir::Ty::int(&mut self.ctx, 1);
                    let val = ir::Inst::iconst(&mut self.ctx, 0, i1);
                    self.curr_block.unwrap().push_back(&mut self.ctx, val);
                    val.result(&self.ctx, 0)
                } else if ty.is_int() {
                    let i32 = ir::Ty::int(&mut self.ctx, 32);
                    let val = ir::Inst::iconst(&mut self.ctx, 0, i32);
                    self.curr_block.unwrap().push_back(&mut self.ctx, val);
                    val.result(&self.ctx, 0)
                } else if ty.is_float() {
                    let f32 = ir::Ty::float32(&mut self.ctx);
                    let val = ir::Inst::fconst(&mut self.ctx, 0.0, f32);
                    self.curr_block.unwrap().push_back(&mut self.ctx, val);
                    val.result(&self.ctx, 0)
                } else {
                    unreachable!("invalid zero type: {:?}", ty)
                }
            }
            Cv::Undef(ty) => {
                let ir_ty = self.gen_type(ty);
                let undef = ir::Inst::undef(&mut self.ctx, ir_ty);
                self.curr_block.unwrap().push_back(&mut self.ctx, undef);
                undef.result(&self.ctx, 0)
            }
        }
    }

    fn gen_local_expr(&mut self, expr: &Expr) -> Option<ir::Value> {
        use BinaryOp as Bo;

        let ty = expr.ty();
        let curr_block = self.curr_block.unwrap();

        match &expr.kind {
            ExprKind::Const(v) => Some(self.gen_local_comptime(v)),
            ExprKind::Binary(op, lhs, rhs) => match op {
                Bo::Add
                | Bo::Sub
                | Bo::Mul
                | Bo::Div
                | Bo::Mod
                | Bo::Lt
                | Bo::Gt
                | Bo::Le
                | Bo::Ge
                | Bo::Eq
                | Bo::Ne => {
                    let is_float = lhs.ty().is_float();

                    let lhs = self.gen_local_expr(lhs).unwrap();
                    let rhs = self.gen_local_expr(rhs).unwrap();

                    let inst = match op {
                        Bo::Add => {
                            if is_float {
                                ir::Inst::fbinary(&mut self.ctx, ir::FBinaryOp::Add, lhs, rhs)
                            } else {
                                ir::Inst::ibinary(&mut self.ctx, ir::IBinaryOp::Add, lhs, rhs)
                            }
                        }
                        Bo::Sub => {
                            if is_float {
                                ir::Inst::fbinary(&mut self.ctx, ir::FBinaryOp::Sub, lhs, rhs)
                            } else {
                                ir::Inst::ibinary(&mut self.ctx, ir::IBinaryOp::Sub, lhs, rhs)
                            }
                        }
                        Bo::Mul => {
                            if is_float {
                                ir::Inst::fbinary(&mut self.ctx, ir::FBinaryOp::Mul, lhs, rhs)
                            } else {
                                ir::Inst::ibinary(&mut self.ctx, ir::IBinaryOp::Mul, lhs, rhs)
                            }
                        }
                        Bo::Div => {
                            if is_float {
                                ir::Inst::fbinary(&mut self.ctx, ir::FBinaryOp::Div, lhs, rhs)
                            } else {
                                ir::Inst::ibinary(&mut self.ctx, ir::IBinaryOp::SDiv, lhs, rhs)
                            }
                        }
                        Bo::Mod => {
                            if is_float {
                                ir::Inst::fbinary(&mut self.ctx, ir::FBinaryOp::Rem, lhs, rhs)
                            } else {
                                ir::Inst::ibinary(&mut self.ctx, ir::IBinaryOp::SRem, lhs, rhs)
                            }
                        }
                        Bo::Lt => {
                            if is_float {
                                ir::Inst::fbinary(
                                    &mut self.ctx,
                                    ir::FBinaryOp::Cmp(ir::FCmpCond::ULt),
                                    lhs,
                                    rhs,
                                )
                            } else {
                                ir::Inst::ibinary(
                                    &mut self.ctx,
                                    ir::IBinaryOp::Cmp(ir::ICmpCond::Slt),
                                    lhs,
                                    rhs,
                                )
                            }
                        }
                        Bo::Gt => {
                            if is_float {
                                ir::Inst::fbinary(
                                    &mut self.ctx,
                                    ir::FBinaryOp::Cmp(ir::FCmpCond::ULt),
                                    rhs,
                                    lhs,
                                )
                            } else {
                                ir::Inst::ibinary(
                                    &mut self.ctx,
                                    ir::IBinaryOp::Cmp(ir::ICmpCond::Slt),
                                    rhs,
                                    lhs,
                                )
                            }
                        }
                        Bo::Le => {
                            if is_float {
                                ir::Inst::fbinary(
                                    &mut self.ctx,
                                    ir::FBinaryOp::Cmp(ir::FCmpCond::ULe),
                                    lhs,
                                    rhs,
                                )
                            } else {
                                ir::Inst::ibinary(
                                    &mut self.ctx,
                                    ir::IBinaryOp::Cmp(ir::ICmpCond::Sle),
                                    lhs,
                                    rhs,
                                )
                            }
                        }
                        Bo::Ge => {
                            if is_float {
                                ir::Inst::fbinary(
                                    &mut self.ctx,
                                    ir::FBinaryOp::Cmp(ir::FCmpCond::ULe),
                                    rhs,
                                    lhs,
                                )
                            } else {
                                ir::Inst::ibinary(
                                    &mut self.ctx,
                                    ir::IBinaryOp::Cmp(ir::ICmpCond::Sle),
                                    rhs,
                                    lhs,
                                )
                            }
                        }
                        Bo::Eq => {
                            if is_float {
                                ir::Inst::fbinary(
                                    &mut self.ctx,
                                    ir::FBinaryOp::Cmp(ir::FCmpCond::UEq),
                                    lhs,
                                    rhs,
                                )
                            } else {
                                ir::Inst::ibinary(
                                    &mut self.ctx,
                                    ir::IBinaryOp::Cmp(ir::ICmpCond::Eq),
                                    lhs,
                                    rhs,
                                )
                            }
                        }
                        Bo::Ne => {
                            if is_float {
                                ir::Inst::fbinary(
                                    &mut self.ctx,
                                    ir::FBinaryOp::Cmp(ir::FCmpCond::UNe),
                                    lhs,
                                    rhs,
                                )
                            } else {
                                ir::Inst::ibinary(
                                    &mut self.ctx,
                                    ir::IBinaryOp::Cmp(ir::ICmpCond::Ne),
                                    lhs,
                                    rhs,
                                )
                            }
                        }
                        Bo::LogicalAnd | Bo::LogicalOr => unreachable!(),
                    };

                    curr_block.push_back(&mut self.ctx, inst);
                    Some(inst.result(&self.ctx, 0))
                }
                Bo::LogicalAnd | Bo::LogicalOr => {
                    // logical-and
                    // - compute lhs
                    // - if lhs is false, jump to merge block with arg = false, otherwise rhs block
                    // - compute rhs, jump to merge block with arg = rhs
                    // - merge block, with an argument, which is the result of the logical-and
                    //
                    // logical-or
                    // - compute lhs
                    // - if lhs is true, jump to merge block with arg = true, otherwise rhs block
                    // - compute rhs, jump to merge block with arg = rhs
                    // - merge block, with an argument, which is the result of the logical-or

                    let lhs = self.gen_local_expr(lhs).unwrap();

                    // lhs might have changed the current block.
                    let curr_block = self.curr_block.unwrap();

                    let rhs_block = ir::Block::new(&mut self.ctx);

                    let i1 = ir::Ty::int(&mut self.ctx, 1);
                    let merge_block = ir::Block::new(&mut self.ctx);
                    let block_param = merge_block.new_param(&mut self.ctx, i1);

                    if let Bo::LogicalAnd = op {
                        let false_ = ir::Inst::iconst(&mut self.ctx, false, i1);
                        curr_block.push_back(&mut self.ctx, false_);
                        let false_ = false_.result(&self.ctx, 0);
                        let br = ir::Inst::br(
                            &mut self.ctx,
                            lhs,
                            rhs_block,
                            vec![],
                            merge_block,
                            vec![false_],
                        );
                        curr_block.push_back(&mut self.ctx, br);
                    } else {
                        let true_ = ir::Inst::iconst(&mut self.ctx, true, i1);
                        curr_block.push_back(&mut self.ctx, true_);
                        let true_ = true_.result(&self.ctx, 0);
                        let br = ir::Inst::br(
                            &mut self.ctx,
                            lhs,
                            merge_block,
                            vec![true_],
                            rhs_block,
                            vec![],
                        );
                        curr_block.push_back(&mut self.ctx, br);
                    }

                    self.curr_func.unwrap().push_back(&mut self.ctx, rhs_block);
                    self.curr_block = Some(rhs_block);

                    let rhs = self.gen_local_expr(rhs).unwrap();
                    let jump = ir::Inst::jump(&mut self.ctx, merge_block, vec![rhs]);

                    self.curr_block.unwrap().push_back(&mut self.ctx, jump);

                    self.curr_func
                        .unwrap()
                        .push_back(&mut self.ctx, merge_block);
                    self.curr_block = Some(merge_block);

                    Some(block_param)
                }
            },
            ExprKind::Unary(op, expr) => {
                let is_float = expr.ty().is_float();
                match op {
                    UnaryOp::Neg => {
                        let operand = self.gen_local_expr(expr).unwrap();
                        if is_float {
                            // fneg
                            let fneg = ir::Inst::funary(&mut self.ctx, ir::FUnaryOp::Neg, operand);
                            curr_block.push_back(&mut self.ctx, fneg);
                            Some(fneg.result(&self.ctx, 0))
                        } else {
                            // 0 - operand
                            let i32 = ir::Ty::int(&mut self.ctx, 32);
                            let zero = ir::Inst::iconst(&mut self.ctx, 0, i32);
                            curr_block.push_back(&mut self.ctx, zero);
                            let zero = zero.result(&self.ctx, 0);
                            let sub =
                                ir::Inst::ibinary(&mut self.ctx, ir::IBinaryOp::Sub, zero, operand);
                            curr_block.push_back(&mut self.ctx, sub);
                            Some(sub.result(&self.ctx, 0))
                        }
                    }
                    UnaryOp::Not => {
                        assert!(expr.ty().is_bool());
                        let operand = self.gen_local_expr(expr).unwrap();
                        let not = ir::Inst::iunary(&mut self.ctx, ir::IUnaryOp::Not, operand);
                        curr_block.push_back(&mut self.ctx, not);
                        Some(not.result(&self.ctx, 0))
                    }
                }
            }
            ExprKind::LVal(LVal { ident, indices }) => {
                let entry = self.symtable.lookup(ident).unwrap();
                let ir_value = entry.ir_value.unwrap();
                let bound_ty = entry.ty.clone();

                let mut shape = Vec::new();
                let mut base_ty = &bound_ty;

                while let Some(ty) = base_ty.inner_ty() {
                    if let Tk::Array(_, len) = base_ty.kind() {
                        shape.push(*len);
                    }
                    base_ty = ty; // possibly a pointer, so using `inner_ty`
                }

                let ir_base_ty = self.gen_type(base_ty);

                let slot = if let IrGenResult::Slot(slot) = ir_value {
                    // get global
                    let name = slot.name(&self.ctx).to_string();
                    let get_global = ir::Inst::get_global(&mut self.ctx, name);
                    curr_block.push_back(&mut self.ctx, get_global);
                    get_global.result(&self.ctx, 0)
                } else if let IrGenResult::Value(slot) = ir_value {
                    slot
                } else {
                    unreachable!()
                };

                if shape.is_empty() && indices.is_empty() {
                    if slot.is_block_param(&self.ctx) {
                        Some(slot)
                    } else {
                        // load
                        let load = ir::Inst::load(&mut self.ctx, slot, ir_base_ty);
                        curr_block.push_back(&mut self.ctx, load);
                        Some(load.result(&self.ctx, 0))
                    }
                } else if indices.len() >= shape.len() {
                    // using `>=` for function parameter, which can be a pointer to an array
                    // all indices provided, get the element
                    let mut ir_indices = Vec::new();

                    let shape = if indices.len() == shape.len() {
                        // we need to discard the first dim
                        shape.iter().skip(1).map(|dim| *dim as u32).collect()
                    } else {
                        assert!(indices.len() == shape.len() + 1);
                        shape.iter().map(|dim| *dim as u32).collect()
                    };

                    for idx in indices.iter() {
                        let idx = self.gen_local_expr(idx).unwrap();
                        ir_indices.push(idx);
                    }
                    let load_elem =
                        ir::Inst::load_elem(&mut self.ctx, shape, slot, ir_indices, ir_base_ty);
                    curr_block.push_back(&mut self.ctx, load_elem);
                    Some(load_elem.result(&self.ctx, 0))
                } else {
                    let int = ir::Ty::int(&mut self.ctx, 32);

                    // just get the offset
                    let offset = ir::Inst::iconst(&mut self.ctx, 0, int);
                    curr_block.push_back(&mut self.ctx, offset);
                    let mut offset = offset.result(&self.ctx, 0);

                    let mut bound_ty = &bound_ty;

                    for idx in indices.iter() {
                        bound_ty = bound_ty.inner_ty().unwrap();

                        let idx = self.gen_local_expr(idx).unwrap();

                        let bytewidth = bound_ty.bytewidth();
                        let iconst = ir::Inst::iconst(&mut self.ctx, bytewidth as i32, int);
                        curr_block.push_back(&mut self.ctx, iconst);
                        let bytewidth = iconst.result(&self.ctx, 0);

                        let mul =
                            ir::Inst::ibinary(&mut self.ctx, ir::IBinaryOp::Mul, idx, bytewidth);
                        curr_block.push_back(&mut self.ctx, mul);
                        let mul = mul.result(&self.ctx, 0);

                        let add = ir::Inst::ibinary(&mut self.ctx, ir::IBinaryOp::Add, offset, mul);
                        curr_block.push_back(&mut self.ctx, add);
                        offset = add.result(&self.ctx, 0);
                    }

                    let offset = ir::Inst::offset(&mut self.ctx, slot, offset);
                    curr_block.push_back(&mut self.ctx, offset);
                    Some(offset.result(&self.ctx, 0))
                }
            }
            ExprKind::Coercion(expr) => {
                match (expr.ty().kind(), ty.kind()) {
                    (Tk::Bool, Tk::Int) => {
                        // zeroext
                        let val = self.gen_local_expr(expr).unwrap();
                        let int = ir::Ty::int(&mut self.ctx, 32);
                        let zext = ir::Inst::cast(&mut self.ctx, ir::CastOp::ZExt, val, int);
                        curr_block.push_back(&mut self.ctx, zext);
                        Some(zext.result(&self.ctx, 0))
                    }
                    (Tk::Int, Tk::Bool) => {
                        let val = self.gen_local_expr(expr).unwrap();
                        let int = ir::Ty::int(&mut self.ctx, 32);
                        let zero = ir::Inst::iconst(&mut self.ctx, 0, int);
                        curr_block.push_back(&mut self.ctx, zero);
                        let zero = zero.result(&self.ctx, 0);

                        let icmp = ir::Inst::ibinary(
                            &mut self.ctx,
                            ir::IBinaryOp::Cmp(ir::ICmpCond::Ne),
                            val,
                            zero,
                        );
                        curr_block.push_back(&mut self.ctx, icmp);
                        Some(icmp.result(&self.ctx, 0))
                    }
                    (Tk::Int, Tk::Float) => {
                        // sitofp
                        let val = self.gen_local_expr(expr).unwrap();
                        let float = ir::Ty::float32(&mut self.ctx);
                        let sitofp = ir::Inst::cast(&mut self.ctx, ir::CastOp::SiToFp, val, float);
                        curr_block.push_back(&mut self.ctx, sitofp);
                        Some(sitofp.result(&self.ctx, 0))
                    }
                    (Tk::Float, Tk::Int) => {
                        // fptosi
                        let val = self.gen_local_expr(expr).unwrap();
                        let int = ir::Ty::int(&mut self.ctx, 32);
                        let fptosi = ir::Inst::cast(&mut self.ctx, ir::CastOp::FpToSi, val, int);
                        curr_block.push_back(&mut self.ctx, fptosi);
                        Some(fptosi.result(&self.ctx, 0))
                    }
                    (Tk::Float, Tk::Bool) => {
                        let val = self.gen_local_expr(expr).unwrap();
                        let float = ir::Ty::float32(&mut self.ctx);
                        let zero = ir::Inst::fconst(&mut self.ctx, 0.0, float);
                        curr_block.push_back(&mut self.ctx, zero);
                        let zero = zero.result(&self.ctx, 0);

                        let fcmp = ir::Inst::fbinary(
                            &mut self.ctx,
                            ir::FBinaryOp::Cmp(ir::FCmpCond::UNe),
                            val,
                            zero,
                        );
                        curr_block.push_back(&mut self.ctx, fcmp);
                        Some(fcmp.result(&self.ctx, 0))
                    }
                    (Tk::Array(..), Tk::Ptr(_)) => self.gen_local_expr(expr),

                    _ => unreachable!("invalid coercion: {} -> {}", expr.ty(), ty),
                }
            }
            ExprKind::FuncCall(FuncCall { ident, args }) => {
                let ir_args = args
                    .iter()
                    .map(|arg| self.gen_local_expr(arg).unwrap())
                    .collect();
                let ir_ty = self.gen_type(ty);

                let call = ir::Inst::call(&mut self.ctx, ident, ir_args, vec![ir_ty]);
                curr_block.push_back(&mut self.ctx, call);

                if !ty.is_void() {
                    Some(call.result(&self.ctx, 0))
                } else {
                    None
                }
            }
            ExprKind::InitList(_) => unreachable!(),
        }
    }

    fn gen_sysylib(&mut self) {
        self.symtable.register_sysylib();
        let sysylib_names = [
            "getint",
            "getch",
            "getfloat",
            "getarray",
            "getfarray",
            "putint",
            "putch",
            "putfloat",
            "putarray",
            "putfarray",
            "_sysy_starttime",
            "_sysy_stoptime",
        ];

        for name in sysylib_names.iter() {
            let entry = self.symtable.lookup(name).unwrap();
            let sig = self.gen_sig(&entry.ty.clone());
            self.ctx.add_func_decl(name, sig);
        }

        // memcpy and memset, from libc actually
        let void = ir::Ty::void(&mut self.ctx);
        let ptr = ir::Ty::ptr(&mut self.ctx);
        let int = ir::Ty::int(&mut self.ctx, 32);

        // use void here to ignore return value
        let memcpy_sig = ir::Signature::new(vec![ptr, ptr, int], vec![void]);
        self.ctx.add_func_decl("memcpy", memcpy_sig);

        let memset_sig = ir::Signature::new(vec![ptr, int, int], vec![void]);
        self.ctx.add_func_decl("memset", memset_sig);
    }
}

pub trait IrGen {
    fn irgen(&self, irgen: &mut IrGenContext);
}

impl IrGen for CompUnit {
    fn irgen(&self, irgen: &mut IrGenContext) {
        irgen.symtable.enter_scope();
        irgen.gen_sysylib();
        for item in &self.items {
            item.irgen(irgen);
        }
        irgen.symtable.leave_scope();
    }
}

impl IrGen for Item {
    fn irgen(&self, irgen: &mut IrGenContext) {
        match self {
            Item::Decl(decl) => match decl {
                Decl::ConstDecl(ConstDecl { defs, .. }) => {
                    for ConstDef { ident, init, .. } in defs {
                        let comptime = init
                            .try_fold(&irgen.symtable)
                            .expect("global def expected to have constant initializer");
                        let constant = IrGenContext::gen_global_comptime(&comptime);
                        let size = comptime.get_type().bytewidth();
                        let slot = ir::GlobalSlot::new(
                            &mut irgen.ctx,
                            format!("__GLOBAL_CONST_{}", ident),
                            size,
                            constant,
                        );
                        irgen.symtable.insert(
                            ident.clone(),
                            SymbolEntry {
                                ty: init.ty().clone(),
                                comptime: Some(comptime),
                                ir_value: Some(IrGenResult::Slot(slot)),
                            },
                        );
                    }
                }
                Decl::VarDecl(VarDecl { defs, .. }) => {
                    for VarDef { ident, init, .. } in defs {
                        let comptime = init
                            .as_ref()
                            .unwrap() // None should have been assigned with undef
                            .try_fold(&irgen.symtable)
                            .expect("global def expected to have constant initializer");
                        let constant = IrGenContext::gen_global_comptime(&comptime);
                        let size = comptime.get_type().bytewidth();
                        let slot = ir::GlobalSlot::new(
                            &mut irgen.ctx,
                            format!("__GLOBAL_VAR_{}", ident),
                            size,
                            constant,
                        );
                        irgen.symtable.insert(
                            ident.clone(),
                            SymbolEntry {
                                ty: init.as_ref().unwrap().ty().clone(),
                                comptime: Some(comptime),
                                ir_value: Some(IrGenResult::Slot(slot)),
                            },
                        );
                    }
                }
            },
            Item::FuncDef(func_def) => func_def.irgen(irgen),
        }
    }
}

impl IrGen for FuncDef {
    fn irgen(&self, irgen: &mut IrGenContext) {
        irgen.symtable.enter_scope();

        let mut param_tys = Vec::new();
        for FuncFParam { ty, indices, .. } in self.params.iter() {
            if let Some(indices) = indices {
                let mut ty = ty.clone();
                for dim in indices.iter().rev() {
                    let dim = dim.try_fold(&irgen.symtable).expect("non-constant dim");
                    ty = Type::array(ty, dim.unwrap_int() as usize);
                }
                ty = Type::ptr(ty);
                param_tys.push(ty);
            } else {
                param_tys.push(ty.clone());
            }
        }

        let func_ty = Type::func(param_tys.clone(), self.ret_ty.clone());
        let sig = irgen.gen_sig(&func_ty);

        let func = ir::Func::new(&mut irgen.ctx, &self.ident, sig);

        irgen.symtable.insert_upper(
            self.ident.clone(),
            SymbolEntry {
                ty: func_ty,
                comptime: None,
                ir_value: None,
            },
            1,
        );

        let block = ir::Block::new(&mut irgen.ctx);
        func.push_back(&mut irgen.ctx, block);

        irgen.curr_func = Some(func);
        irgen.curr_func_name = Some(self.ident.clone());
        irgen.curr_block = Some(block);

        // block params
        for (FuncFParam { ident, .. }, ty) in self.params.iter().zip(param_tys.iter()) {
            let ir_ty = irgen.gen_type(ty);
            let param = block.new_param(&mut irgen.ctx, ir_ty);

            param.assign_name(
                &mut irgen.ctx,
                format!(
                    "__PARAM_{}_{}",
                    irgen.curr_func_name.as_ref().unwrap(),
                    ident
                ),
            );

            irgen.symtable.insert(
                ident.clone(),
                SymbolEntry {
                    ty: ty.clone(),
                    comptime: None,
                    ir_value: Some(IrGenResult::Value(param)),
                },
            );
        }

        // create slots for pass-by-value params
        for (FuncFParam { ident, .. }, ty) in self.params.iter().zip(param_tys.iter()) {
            if ty.is_float() || ty.is_int() {
                let slot = ir::Inst::stack_slot(&mut irgen.ctx, ty.bytewidth() as u32);
                slot.result(&irgen.ctx, 0).assign_name(
                    &mut irgen.ctx,
                    format!(
                        "__SLOT_PARAM_{}_{}",
                        irgen.curr_func_name.as_ref().unwrap(),
                        ident
                    ),
                );
                block.push_front(&mut irgen.ctx, slot);
                let slot = slot.result(&irgen.ctx, 0);

                // get old entry
                let param = irgen
                    .symtable
                    .lookup(ident)
                    .unwrap()
                    .ir_value
                    .unwrap()
                    .unwrap_value();

                // store
                let store = ir::Inst::store(&mut irgen.ctx, param, slot);

                block.push_back(&mut irgen.ctx, store);

                // set new entry
                irgen.symtable.insert(
                    ident.clone(),
                    SymbolEntry {
                        ty: ty.clone(),
                        comptime: None,
                        ir_value: Some(IrGenResult::Value(slot)),
                    },
                );
            }
        }

        // create return block and slot
        let ret_block = ir::Block::new(&mut irgen.ctx);
        irgen.curr_ret_block = Some(ret_block);

        if !self.ret_ty.is_void() {
            let ret_slot = ir::Inst::stack_slot(&mut irgen.ctx, self.ret_ty.bytewidth() as u32);
            ret_slot.result(&irgen.ctx, 0).assign_name(
                &mut irgen.ctx,
                format!("__RET_{}", irgen.curr_func_name.as_ref().unwrap()),
            );
            block.push_front(&mut irgen.ctx, ret_slot);
            irgen.curr_ret_slot = Some(ret_slot.result(&irgen.ctx, 0));

            // store 0 into the slot as default value, check `sysy/2024/h_performance/h1`
            if self.ret_ty.is_int() {
                let ty = irgen.gen_type(&self.ret_ty);
                let zero = ir::Inst::iconst(&mut irgen.ctx, 0i32, ty);
                block.push_back(&mut irgen.ctx, zero);

                let zero = zero.result(&irgen.ctx, 0);
                let store = ir::Inst::store(&mut irgen.ctx, zero, irgen.curr_ret_slot.unwrap());
                block.push_back(&mut irgen.ctx, store);
            } else if self.ret_ty.is_float() {
                let ty = irgen.gen_type(&self.ret_ty);
                let zero = ir::Inst::fconst(&mut irgen.ctx, 0.0f32, ty);
                block.push_back(&mut irgen.ctx, zero);

                let zero = zero.result(&irgen.ctx, 0);
                let store = ir::Inst::store(&mut irgen.ctx, zero, irgen.curr_ret_slot.unwrap());
                block.push_back(&mut irgen.ctx, store);
            }
        }

        // generate body
        self.body.irgen(irgen);

        // append return block
        func.push_back(&mut irgen.ctx, ret_block);

        if !self.ret_ty.is_void() {
            // load, ret
            let ret_slot = irgen.curr_ret_slot.unwrap();
            let ty = irgen.gen_type(&self.ret_ty);

            let load = ir::Inst::load(&mut irgen.ctx, ret_slot, ty);
            ret_block.push_back(&mut irgen.ctx, load);
            let val = load.result(&irgen.ctx, 0);

            let ret = ir::Inst::ret(&mut irgen.ctx, vec![val]);
            ret_block.push_back(&mut irgen.ctx, ret);
        } else {
            // just return
            let ret = ir::Inst::ret(&mut irgen.ctx, vec![]);
            ret_block.push_back(&mut irgen.ctx, ret);
        }

        irgen.curr_func = None;
        irgen.curr_func_name = None;
        irgen.curr_block = None;
        irgen.curr_ret_slot = None;
        irgen.curr_ret_block = None;

        irgen.symtable.leave_scope();
    }
}

impl IrGen for Decl {
    fn irgen(&self, irgen: &mut IrGenContext) {
        let entry_block = irgen.curr_func.unwrap().entry_node(&irgen.ctx);
        let curr_block = irgen.curr_block.unwrap();
        match self {
            Decl::ConstDecl(ConstDecl { defs, .. }) => {
                for ConstDef { ident, init, .. } in defs {
                    let comptime = init
                        .try_fold(&irgen.symtable)
                        .expect("global def expected to have constant initializer");
                    let size = init.ty().bytewidth();
                    let stack_slot = ir::Inst::stack_slot(&mut irgen.ctx, size as u32);
                    let stack_slot_name = stack_slot
                        .result(&irgen.ctx, 0)
                        .alloc_name(
                            &mut irgen.ctx,
                            format!(
                                "__SLOT_CONST_{}_{}_",
                                irgen.curr_func_name.as_ref().unwrap(),
                                ident
                            ),
                        )
                        .clone();
                    entry_block.push_front(&mut irgen.ctx, stack_slot);
                    irgen.symtable.insert(
                        ident,
                        SymbolEntry {
                            ty: init.ty().clone(),
                            comptime: Some(comptime),
                            ir_value: Some(IrGenResult::Value(stack_slot.result(&irgen.ctx, 0))),
                        },
                    );
                    if init.ty().is_aggregate() {
                        // create a global data slot, and memcpy the data to the stack slot
                        if let ExprKind::Const(ref val) = init.kind {
                            let int = ir::Ty::int(&mut irgen.ctx, 32);
                            let void = ir::Ty::void(&mut irgen.ctx);

                            match val {
                                Cv::Zeros(_) | Cv::List(_) if val.is_zero() => {
                                    let zero = ir::Inst::iconst(&mut irgen.ctx, 0i32, int);
                                    curr_block.push_back(&mut irgen.ctx, zero);

                                    let bytewidth = init.ty().bytewidth();
                                    let size =
                                        ir::Inst::iconst(&mut irgen.ctx, bytewidth as i32, int);
                                    curr_block.push_back(&mut irgen.ctx, size);

                                    let args = vec![
                                        stack_slot.result(&irgen.ctx, 0),
                                        zero.result(&irgen.ctx, 0),
                                        size.result(&irgen.ctx, 0),
                                    ];
                                    let call =
                                        ir::Inst::call(&mut irgen.ctx, "memset", args, vec![void]);
                                    curr_block.push_back(&mut irgen.ctx, call);
                                }
                                Cv::List(_) => {
                                    // create global and memcpy
                                    let constant = IrGenContext::gen_global_comptime(val);
                                    let size = init.ty().bytewidth();
                                    let name = format!("__DATA{}", stack_slot_name,); // to avoid conflict
                                    ir::GlobalSlot::new(
                                        &mut irgen.ctx,
                                        name.clone(),
                                        size,
                                        constant,
                                    );
                                    let get_global = ir::Inst::get_global(&mut irgen.ctx, name);
                                    curr_block.push_back(&mut irgen.ctx, get_global);

                                    let size = ir::Inst::iconst(&mut irgen.ctx, size as i32, int);
                                    curr_block.push_back(&mut irgen.ctx, size);

                                    let args = vec![
                                        stack_slot.result(&irgen.ctx, 0),
                                        get_global.result(&irgen.ctx, 0),
                                        size.result(&irgen.ctx, 0),
                                    ];
                                    let call =
                                        ir::Inst::call(&mut irgen.ctx, "memcpy", args, vec![void]);
                                    curr_block.push_back(&mut irgen.ctx, call);
                                }
                                Cv::Undef(_)
                                | Cv::Bool(_)
                                | Cv::Int(_)
                                | Cv::Float(_)
                                | Cv::Zeros(_) => {
                                    unreachable!("non-aggregate type should not be here")
                                }
                            }
                        } else {
                            unreachable!("non-constant initializer should not be here")
                        }
                    } else {
                        let init = irgen.gen_local_expr(init).unwrap();
                        let slot = stack_slot.result(&irgen.ctx, 0);
                        let store = ir::Inst::store(&mut irgen.ctx, init, slot);
                        curr_block.push_back(&mut irgen.ctx, store);
                    }
                }
            }
            Decl::VarDecl(VarDecl { defs, .. }) => {
                for VarDef { ident, init, .. } in defs {
                    let init = init.as_ref().unwrap();
                    let size = init.ty().bytewidth();
                    let stack_slot = ir::Inst::stack_slot(&mut irgen.ctx, size as u32);
                    let stack_slot_name = stack_slot
                        .result(&irgen.ctx, 0)
                        .alloc_name(
                            &mut irgen.ctx,
                            format!(
                                "__SLOT_VAR_{}_{}_",
                                irgen.curr_func_name.as_ref().unwrap(),
                                ident
                            ),
                        )
                        .clone();
                    entry_block.push_front(&mut irgen.ctx, stack_slot);
                    irgen.symtable.insert(
                        ident,
                        SymbolEntry {
                            ty: init.ty().clone(),
                            comptime: None,
                            ir_value: Some(IrGenResult::Value(stack_slot.result(&irgen.ctx, 0))),
                        },
                    );

                    if let ExprKind::Const(Cv::Undef(ref ty)) = init.kind {
                        // we don't need to store anything for undefined slot.
                        // but, SysY implicitly requires uninitialized local variables to be zero.
                        if ty.is_int() {
                            let ir_ty = irgen.gen_type(ty);
                            let ctx = &mut irgen.ctx;
                            let zero = ir::Inst::iconst(ctx, 0i32, ir_ty);
                            let store = ir::Inst::store(
                                ctx,
                                zero.result(ctx, 0),
                                stack_slot.result(ctx, 0),
                            );
                            curr_block.push_back(ctx, zero);
                            curr_block.push_back(ctx, store);
                        } else if ty.is_float() {
                            let ir_ty = irgen.gen_type(ty);
                            let ctx = &mut irgen.ctx;
                            let zero = ir::Inst::fconst(ctx, 0.0f32, ir_ty);
                            let store = ir::Inst::store(
                                ctx,
                                zero.result(ctx, 0),
                                stack_slot.result(ctx, 0),
                            );
                            curr_block.push_back(ctx, zero);
                            curr_block.push_back(ctx, store);
                        } else {
                            // memset, just in case.
                            let ctx = &mut irgen.ctx;
                            let int = ir::Ty::int(ctx, 32);
                            let void = ir::Ty::void(ctx);
                            let zero = ir::Inst::iconst(ctx, 0i32, int);
                            curr_block.push_back(ctx, zero);
                            let size = ir::Inst::iconst(ctx, size as i32, int);
                            curr_block.push_back(ctx, size);
                            let args = vec![
                                stack_slot.result(ctx, 0),
                                zero.result(ctx, 0),
                                size.result(ctx, 0),
                            ];
                            let call = ir::Inst::call(ctx, "memset", args, vec![void]);
                            curr_block.push_back(ctx, call);
                        }
                    } else if init.ty().is_aggregate() {
                        // some part are not constant, we need to memcpy and
                        // set one by one
                        let mut global_init = Vec::new();
                        let mut indices = Vec::new();

                        fn handle_init_list(
                            expr: &Expr,
                            global_init: &mut Vec<Cv>,
                            indices: &mut Vec<Vec<usize>>,
                            curr_indices: Vec<usize>,
                        ) {
                            match &expr.kind {
                                ExprKind::InitList(exprs) => {
                                    let mut inits = Vec::new();
                                    for (i, expr) in exprs.iter().enumerate() {
                                        let mut curr_indices = curr_indices.clone();
                                        curr_indices.push(i);
                                        handle_init_list(expr, &mut inits, indices, curr_indices);
                                    }

                                    if inits.iter().all(|val| val.is_zero()) {
                                        global_init.push(Cv::zeros(expr.ty().clone()));
                                    } else {
                                        global_init.push(Cv::list(inits));
                                    }
                                }
                                ExprKind::Const(val) => {
                                    global_init.push(val.clone());
                                }
                                ExprKind::Binary(..)
                                | ExprKind::Unary(..)
                                | ExprKind::FuncCall(_)
                                | ExprKind::LVal(_)
                                | ExprKind::Coercion(_) => {
                                    indices.push(curr_indices);
                                    if expr.ty().is_int() {
                                        // because `zeros` should not be used for non-aggregate
                                        // value, we need to judge the type.
                                        global_init.push(Cv::int(0));
                                    } else if expr.ty().is_float() {
                                        global_init.push(Cv::float(0.0));
                                    } else {
                                        global_init.push(Cv::zeros(expr.ty().clone()));
                                    }
                                }
                            }
                        }
                        handle_init_list(init, &mut global_init, &mut indices, Vec::new());
                        let global_init = global_init.pop().unwrap();
                        let bytewidth = init.ty().bytewidth();

                        let int = ir::Ty::int(&mut irgen.ctx, 32);
                        let void = ir::Ty::void(&mut irgen.ctx);

                        if global_init.is_zero() {
                            let zero = ir::Inst::iconst(&mut irgen.ctx, 0i32, int);
                            curr_block.push_back(&mut irgen.ctx, zero);
                            let size = ir::Inst::iconst(&mut irgen.ctx, bytewidth as i32, int);
                            curr_block.push_back(&mut irgen.ctx, size);
                            let args = vec![
                                stack_slot.result(&irgen.ctx, 0),
                                zero.result(&irgen.ctx, 0),
                                size.result(&irgen.ctx, 0),
                            ];
                            let call = ir::Inst::call(&mut irgen.ctx, "memset", args, vec![void]);
                            curr_block.push_back(&mut irgen.ctx, call);
                        } else {
                            // memcpy
                            let constant = IrGenContext::gen_global_comptime(&global_init);
                            let name = format!("__DATA{}", stack_slot_name,); // to avoid conflict
                            ir::GlobalSlot::new(&mut irgen.ctx, name.clone(), bytewidth, constant);
                            let get_global = ir::Inst::get_global(&mut irgen.ctx, name);
                            curr_block.push_back(&mut irgen.ctx, get_global);

                            let size = ir::Inst::iconst(&mut irgen.ctx, bytewidth as i32, int);
                            curr_block.push_back(&mut irgen.ctx, size);

                            let args = vec![
                                stack_slot.result(&irgen.ctx, 0),
                                get_global.result(&irgen.ctx, 0),
                                size.result(&irgen.ctx, 0),
                            ];
                            let call = ir::Inst::call(&mut irgen.ctx, "memcpy", args, vec![void]);
                            curr_block.push_back(&mut irgen.ctx, call);
                        }

                        // then set the non-zero parts
                        // because it is a declaration, so it cannot be pointer
                        let ty = init.ty();
                        for indices in indices.iter() {
                            let zero = ir::Inst::iconst(&mut irgen.ctx, 0i32, int);
                            curr_block.push_back(&mut irgen.ctx, zero);

                            // calculate the offset, by mul & add
                            let mut offset = zero.result(&irgen.ctx, 0);
                            let mut expr = init;
                            let mut bound_ty = ty;

                            for idx in indices.iter() {
                                if let ExprKind::InitList(ref exprs) = expr.kind {
                                    bound_ty = bound_ty.inner_ty().unwrap();
                                    expr = &exprs[*idx];

                                    let idx = ir::Inst::iconst(&mut irgen.ctx, *idx as i32, int);
                                    curr_block.push_back(&mut irgen.ctx, idx);

                                    let size = ir::Inst::iconst(
                                        &mut irgen.ctx,
                                        bound_ty.bytewidth() as i32,
                                        int,
                                    );
                                    curr_block.push_back(&mut irgen.ctx, size);

                                    let idx = idx.result(&irgen.ctx, 0);
                                    let size = size.result(&irgen.ctx, 0);

                                    let mul = ir::Inst::ibinary(
                                        &mut irgen.ctx,
                                        ir::IBinaryOp::Mul,
                                        idx,
                                        size,
                                    );
                                    curr_block.push_back(&mut irgen.ctx, mul);

                                    let mul = mul.result(&irgen.ctx, 0);

                                    let add = ir::Inst::ibinary(
                                        &mut irgen.ctx,
                                        ir::IBinaryOp::Add,
                                        offset,
                                        mul,
                                    );
                                    curr_block.push_back(&mut irgen.ctx, add);
                                    offset = add.result(&irgen.ctx, 0);
                                } else {
                                    unreachable!("invalid array init: {:?}", expr);
                                }
                            }

                            let base = stack_slot.result(&irgen.ctx, 0);
                            let offset = ir::Inst::offset(&mut irgen.ctx, base, offset);
                            curr_block.push_back(&mut irgen.ctx, offset);
                            let offset = offset.result(&irgen.ctx, 0);

                            let val = irgen.gen_local_expr(expr).unwrap();
                            let store = ir::Inst::store(&mut irgen.ctx, val, offset);
                            curr_block.push_back(&mut irgen.ctx, store);
                        }

                        // TODO: check if we can use memset when most of the
                        // data are zeros
                    } else {
                        let init = irgen.gen_local_expr(init).unwrap();
                        let slot = stack_slot.result(&irgen.ctx, 0);
                        let store = ir::Inst::store(&mut irgen.ctx, init, slot);
                        curr_block.push_back(&mut irgen.ctx, store);
                    }
                }
            }
        }
    }
}

impl IrGen for Stmt {
    fn irgen(&self, irgen: &mut IrGenContext) {
        let curr_block = irgen.curr_block.unwrap();

        match self {
            Stmt::Assign(LVal { ident, indices }, expr) => {
                let entry = irgen.symtable.lookup(ident).unwrap();
                let ir_value = entry.ir_value.unwrap();
                let bound_ty = entry.ty.clone();

                let slot = if let IrGenResult::Slot(slot) = ir_value {
                    // get global
                    let name = slot.name(&irgen.ctx).to_string();
                    let get_global = ir::Inst::get_global(&mut irgen.ctx, name);
                    curr_block.push_back(&mut irgen.ctx, get_global);
                    get_global.result(&irgen.ctx, 0)
                } else if let IrGenResult::Value(slot) = ir_value {
                    slot
                } else {
                    unreachable!()
                };

                if indices.is_empty() {
                    let store_dst = slot;

                    let val = irgen.gen_local_expr(expr).unwrap();
                    let store = ir::Inst::store(&mut irgen.ctx, val, store_dst);
                    curr_block.push_back(&mut irgen.ctx, store);
                } else {
                    // discard the first dimension or pointer
                    let mut bound_ty = bound_ty.inner_ty().unwrap();
                    let mut shape = Vec::new();

                    while let Some(ty) = bound_ty.inner_ty() {
                        let dim = bound_ty.bytewidth() / ty.bytewidth();
                        shape.push(dim as u32);
                        bound_ty = ty;
                    }

                    let mut ir_indices = Vec::new();

                    for idx in indices.iter() {
                        let idx = irgen.gen_local_expr(idx).unwrap();
                        ir_indices.push(idx);
                    }

                    let val = irgen.gen_local_expr(expr).unwrap();
                    let store_elem =
                        ir::Inst::store_elem(&mut irgen.ctx, shape, val, slot, ir_indices);
                    curr_block.push_back(&mut irgen.ctx, store_elem);
                };
            }
            Stmt::Expr(ExprStmt { expr }) => {
                if let Some(ref expr) = expr {
                    irgen.gen_local_expr(expr);
                }
            }
            Stmt::Block(block) => block.irgen(irgen),
            Stmt::If(cond, then_stmt, else_stmt) => {
                let br_entry = ir::Block::new(&mut irgen.ctx);
                br_entry.alloc_name(&mut irgen.ctx, "if.entry_");
                irgen.curr_func.unwrap().push_back(&mut irgen.ctx, br_entry);
                irgen.curr_block = Some(br_entry);

                let then_block = ir::Block::new(&mut irgen.ctx);
                then_block.alloc_name(&mut irgen.ctx, "if.then_");

                let else_block = ir::Block::new(&mut irgen.ctx);
                else_block.alloc_name(&mut irgen.ctx, "if.else_");

                let cond = irgen.gen_local_expr(cond).unwrap();
                let br = ir::Inst::br(
                    &mut irgen.ctx,
                    cond,
                    then_block,
                    Vec::new(),
                    else_block,
                    Vec::new(),
                );
                // short-circuit may change the curr block
                irgen.curr_block.unwrap().push_back(&mut irgen.ctx, br);

                let exit_block = ir::Block::new(&mut irgen.ctx);
                exit_block.alloc_name(&mut irgen.ctx, "if.exit_");

                // then block
                irgen
                    .curr_func
                    .unwrap()
                    .push_back(&mut irgen.ctx, then_block);
                irgen.curr_block = Some(then_block);
                then_stmt.irgen(irgen);

                // check if the then_block ends with a terminator
                let terminator = irgen.curr_block.unwrap().tail(&irgen.ctx);
                if terminator.is_none() || !terminator.unwrap().is_terminator(&irgen.ctx) {
                    // jump to exit block
                    let jump = ir::Inst::jump(&mut irgen.ctx, exit_block, Vec::new());
                    irgen.curr_block.unwrap().push_back(&mut irgen.ctx, jump);
                }

                // else block
                irgen
                    .curr_func
                    .unwrap()
                    .push_back(&mut irgen.ctx, else_block);
                irgen.curr_block = Some(else_block);
                if let Some(else_stmt) = else_stmt {
                    else_stmt.irgen(irgen);
                }

                let terminator = irgen.curr_block.unwrap().tail(&irgen.ctx);
                if terminator.is_none() || !terminator.unwrap().is_terminator(&irgen.ctx) {
                    // jump to exit block
                    let jump = ir::Inst::jump(&mut irgen.ctx, exit_block, Vec::new());
                    irgen.curr_block.unwrap().push_back(&mut irgen.ctx, jump);
                }

                // exit block
                irgen
                    .curr_func
                    .unwrap()
                    .push_back(&mut irgen.ctx, exit_block);
                irgen.curr_block = Some(exit_block);
            }
            Stmt::While(cond, loop_stmt) => {
                let loop_entry = ir::Block::new(&mut irgen.ctx);
                loop_entry.alloc_name(&mut irgen.ctx, "while.entry_");
                irgen
                    .curr_func
                    .unwrap()
                    .push_back(&mut irgen.ctx, loop_entry);

                let loop_exit = ir::Block::new(&mut irgen.ctx);
                loop_exit.alloc_name(&mut irgen.ctx, "while.exit_");

                irgen.loop_entry_stack.push(loop_entry);
                irgen.loop_exit_stack.push(loop_exit);

                irgen.curr_block = Some(loop_entry);
                let cond = irgen.gen_local_expr(cond).unwrap();

                let loop_body = ir::Block::new(&mut irgen.ctx);
                loop_body.alloc_name(&mut irgen.ctx, "while.body_");

                let br = ir::Inst::br(
                    &mut irgen.ctx,
                    cond,
                    loop_body,
                    Vec::new(),
                    loop_exit,
                    Vec::new(),
                );
                irgen.curr_block.unwrap().push_back(&mut irgen.ctx, br);

                irgen
                    .curr_func
                    .unwrap()
                    .push_back(&mut irgen.ctx, loop_body);
                irgen.curr_block = Some(loop_body);

                loop_stmt.irgen(irgen);

                let terminator = irgen.curr_block.unwrap().tail(&irgen.ctx);
                if terminator.is_none() || !terminator.unwrap().is_terminator(&irgen.ctx) {
                    let jump_back = ir::Inst::jump(&mut irgen.ctx, loop_entry, Vec::new());
                    irgen
                        .curr_block
                        .unwrap()
                        .push_back(&mut irgen.ctx, jump_back);
                }

                irgen
                    .curr_func
                    .unwrap()
                    .push_back(&mut irgen.ctx, loop_exit);
                irgen.curr_block = Some(loop_exit);

                irgen.loop_entry_stack.pop();
                irgen.loop_exit_stack.pop();
            }
            Stmt::Break => {
                let dst = irgen.loop_exit_stack.last().unwrap();
                let jump = ir::Inst::jump(&mut irgen.ctx, *dst, Vec::new());
                irgen.curr_block.unwrap().push_back(&mut irgen.ctx, jump);
            }
            Stmt::Continue => {
                let dst = irgen.loop_entry_stack.last().unwrap();
                let jump = ir::Inst::jump(&mut irgen.ctx, *dst, Vec::new());
                irgen.curr_block.unwrap().push_back(&mut irgen.ctx, jump);
            }
            Stmt::Return(ReturnStmt { expr }) => {
                if let Some(expr) = expr {
                    let val = irgen.gen_local_expr(expr).unwrap();
                    let store = ir::Inst::store(&mut irgen.ctx, val, irgen.curr_ret_slot.unwrap());
                    irgen.curr_block.unwrap().push_back(&mut irgen.ctx, store);
                }

                let jump =
                    ir::Inst::jump(&mut irgen.ctx, irgen.curr_ret_block.unwrap(), Vec::new());
                irgen.curr_block.unwrap().push_back(&mut irgen.ctx, jump);
            }
        }
    }
}

impl IrGen for Block {
    fn irgen(&self, irgen: &mut IrGenContext) {
        irgen.symtable.enter_scope();
        for item in self.items.iter() {
            match item {
                BlockItem::Decl(decl) => decl.irgen(irgen),
                BlockItem::Stmt(stmt) => stmt.irgen(irgen),
            }
        }
        irgen.symtable.leave_scope();
    }
}
