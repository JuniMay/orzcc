use super::{
    ast::{
        CompUnit,
        ComptimeVal as Cv,
        ConstDecl,
        Decl,
        Expr,
        FuncDef,
        Item,
        SymbolEntry,
        SymbolTable,
        VarDecl,
    },
    types::{Type, TypeKind as Tk},
};
use crate::ir::{self, GlobalSlot};

#[derive(Debug)]
pub enum IrGenResult {
    Slot(ir::GlobalSlot),
    Value(ir::Value),
    Func(ir::Func),
}

pub struct IrGenContext {
    pub ctx: ir::Context,

    pub symtable: SymbolTable,

    pub curr_func: Option<ir::Func>,
    pub curr_block: Option<ir::Block>,

    pub loop_entry_stack: Vec<ir::Block>,
    pub loop_exit_stack: Vec<ir::Block>,

    pub curr_ret_slot: Option<ir::Value>,
    pub curr_ret_block: Option<ir::Block>,
}

impl IrGenContext {
    fn finish(self) -> ir::Context { self.ctx }

    fn gen_global_comptime(val: &Cv) -> ir::Constant {
        match val {
            Cv::Bool(a) => ir::Constant::from(*a),
            Cv::Int(a) => ir::Constant::from(*a),
            Cv::Float(a) => ir::Constant::from(*a),
            Cv::List(vals) => {
                let mut bytes = Vec::new();
                for v in vals {
                    let c = Self::gen_global_comptime(v);
                    if let ir::ConstantKind::Bytes(bs) = c.kind() {
                        bytes.extend(bs);
                    } else {
                        unreachable!();
                    }
                }
                ir::Constant::bytes(bytes)
            }
            // the type of zero and undef can be decided by the type of the variable/constants, and
            // will be recorded in [ir::GlobalSlotData]
            Cv::Zeros(_) => ir::Constant::zeroinit(),
            Cv::Undef(_) => ir::Constant::undef(),
        }
    }

    fn gen_type(&mut self, ty: &Type) -> ir::Ty {
        match ty.kind() {
            Tk::Void => ir::Ty::void(&mut self.ctx),
            Tk::Bool => ir::Ty::int(&mut self.ctx, 1),
            Tk::Int => ir::Ty::int(&mut self.ctx, 32),
            Tk::Float => ir::Ty::float32(&mut self.ctx),
            Tk::Array(ty, len) => {
                let elem_ty = self.gen_type(ty);
                ir::Ty::array(&mut self.ctx, elem_ty, *len)
            }
            Tk::Ptr(_) => ir::Ty::ptr(&mut self.ctx),
            Tk::Func(..) => unreachable!("function type should be handled separately"),
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
            let func = ir::Func::new(&mut self.ctx, name, sig);
            self.symtable.lookup_mut(name).unwrap().ir_value = Some(IrGenResult::Func(func));
        }

        // memcpy and memset, from libc actually
        let void = ir::Ty::void(&mut self.ctx);
        let ptr = ir::Ty::ptr(&mut self.ctx);
        let int = ir::Ty::int(&mut self.ctx, 32);

        // use void here to ignore return value
        let memcpy_sig = ir::Signature::new(vec![ptr, ptr, int], vec![void]);
        let _memcpy = ir::Func::new(&mut self.ctx, "memcpy", memcpy_sig);

        let memset_sig = ir::Signature::new(vec![ptr, int, int], vec![void]);
        let _memset = ir::Func::new(&mut self.ctx, "memset", memset_sig);
    }
}

pub trait IrGen {
    fn irgen(&self, irgen: &mut IrGenContext);
}

impl IrGen for CompUnit {
    fn irgen(&self, irgen: &mut IrGenContext) {
        irgen.symtable.enter_scope();
        irgen.symtable.register_sysylib();
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
                Decl::ConstDecl(ConstDecl { ty, defs }) => {
                    for def in defs {
                        let comptime = def
                            .init
                            .try_fold(&irgen.symtable)
                            .expect("global def expected to have constant initializer");
                        let init = IrGenContext::gen_global_comptime(&comptime);
                        let ir_ty = irgen.gen_type(ty);
                        let slot = GlobalSlot::new(
                            &mut irgen.ctx,
                            format!("__GLOBAL_CONST_{}", &def.ident),
                            ir_ty,
                            init,
                        );
                        irgen.symtable.insert(
                            def.ident.clone(),
                            SymbolEntry {
                                ty: ty.clone(),
                                comptime: Some(comptime),
                                ir_value: Some(IrGenResult::Slot(slot)),
                            },
                        );
                    }
                }
                Decl::VarDecl(VarDecl { ty, defs }) => {
                    for def in defs {
                        let comptime = def
                            .init
                            .as_ref()
                            .unwrap() // None should have been assigned with undef
                            .try_fold(&irgen.symtable)
                            .expect("global def expected to have constant initializer");
                        let init = IrGenContext::gen_global_comptime(&comptime);
                        let ir_ty = irgen.gen_type(ty);
                        let slot = GlobalSlot::new(
                            &mut irgen.ctx,
                            format!("__GLOBAL_VAR_{}", &def.ident),
                            ir_ty,
                            init,
                        );
                        irgen.symtable.insert(
                            def.ident.clone(),
                            SymbolEntry {
                                ty: ty.clone(),
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
    fn irgen(&self, irgen: &mut IrGenContext) { todo!() }
}

impl IrGen for Expr {
    fn irgen(&self, irgen: &mut IrGenContext) { todo!() }
}
