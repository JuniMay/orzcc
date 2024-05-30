use std::collections::HashMap;

use crate::{
    frontend::{
        Block,
        BlockItem,
        CompUnit,
        CompUnitItem,
        ComptimeVal,
        Decl,
        Expr,
        ExprKind,
        FuncCall,
        FuncDef,
        Stmt,
        SysyType,
        SysyTypeKind,
    },
    ir::{
        builders::{BuildAggregateConstant, BuildGlobalValue, BuildNonAggregateConstant},
        module::Module,
        types::Type,
        values::{Function, Value},
    },
};

pub struct SymbolEntry {
    pub ty: SysyType,
    pub comptime_val: Option<ComptimeVal>,
    pub ir_value: Option<Value>,
}

impl SymbolEntry {
    pub fn from_ty(ty: SysyType) -> Self {
        Self {
            ty,
            comptime_val: None,
            ir_value: None,
        }
    }
}

#[derive(Default)]
pub struct SymbolTable {
    entries: HashMap<String, SymbolEntry>,
}

#[derive(Default)]
pub struct SymbolTableStack {
    stack: Vec<SymbolTable>,
}

impl SymbolTableStack {
    pub fn enter_scope(&mut self) { self.stack.push(SymbolTable::default()) }

    pub fn exit_scope(&mut self) { self.stack.pop(); }

    pub fn insert(&mut self, name: impl Into<String>, entry: SymbolEntry) {
        self.stack
            .last_mut()
            .unwrap()
            .entries
            .insert(name.into(), entry);
    }

    /// Insert the symbol entry into the upper scope.
    pub fn insert_upper(&mut self, name: impl Into<String>, entry: SymbolEntry, upper: usize) {
        self.stack
            .iter_mut()
            .rev()
            .nth(upper)
            .unwrap()
            .entries
            .insert(name.into(), entry);
    }

    pub fn lookup(&self, name: &str) -> Option<&SymbolEntry> {
        for table in self.stack.iter().rev() {
            if let Some(entry) = table.entries.get(name) {
                return Some(entry);
            }
        }
        None
    }

    /// Register the SysY standard library functions.
    pub fn register_sysylib(&mut self) {
        // check if this is the top level scope
        assert_eq!(self.stack.len(), 1);

        let getint = SymbolEntry::from_ty(SysyType::function(vec![], SysyType::int()));
        let getch = SymbolEntry::from_ty(SysyType::function(vec![], SysyType::int()));
        let getfloat = SymbolEntry::from_ty(SysyType::function(vec![], SysyType::float()));
        let getarray = SymbolEntry::from_ty(SysyType::function(
            vec![SysyType::array(None, SysyType::int())],
            SysyType::int(),
        ));
        let getfarray = SymbolEntry::from_ty(SysyType::function(
            vec![SysyType::array(None, SysyType::float())],
            SysyType::int(),
        ));
        let putint =
            SymbolEntry::from_ty(SysyType::function(vec![SysyType::int()], SysyType::void()));
        let putch =
            SymbolEntry::from_ty(SysyType::function(vec![SysyType::int()], SysyType::void()));
        let putfloat = SymbolEntry::from_ty(SysyType::function(
            vec![SysyType::float()],
            SysyType::void(),
        ));
        let putarray = SymbolEntry::from_ty(SysyType::function(
            vec![SysyType::int(), SysyType::array(None, SysyType::int())],
            SysyType::void(),
        ));
        let putfarray = SymbolEntry::from_ty(SysyType::function(
            vec![SysyType::int(), SysyType::array(None, SysyType::float())],
            SysyType::void(),
        ));
        let starttime =
            SymbolEntry::from_ty(SysyType::function(vec![SysyType::int()], SysyType::void()));
        let stoptime =
            SymbolEntry::from_ty(SysyType::function(vec![SysyType::int()], SysyType::void()));

        self.insert("getint", getint);
        self.insert("getch", getch);
        self.insert("getfloat", getfloat);
        self.insert("getarray", getarray);
        self.insert("getfarray", getfarray);
        self.insert("putint", putint);
        self.insert("putch", putch);
        self.insert("putfloat", putfloat);
        self.insert("putarray", putarray);
        self.insert("putfarray", putfarray);
        self.insert("_sysy_starttime", starttime);
        self.insert("_sysy_stoptime", stoptime);
    }
}

pub struct IrGenContext {
    pub module: Module,
    pub symtable: SymbolTableStack,
    pub curr_function: Option<Function>,
    pub curr_block: Option<Block>,
}

pub trait IrGen {
    fn irgen(&self, ctx: &mut IrGenContext);
}

impl CompUnitItem {
    /// Generate the function declaration for the SysY standard library
    /// functions.
    fn irgen_sysylib(&self, ctx: &mut IrGenContext) {
        ctx.symtable.register_sysylib();
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
            let entry = ctx.symtable.lookup(name).unwrap();
            let ty = ctx.irgen_type(&entry.ty);
            let func = ctx
                .module
                .builder()
                .function_decl(ty)
                .expect("failed to create function");
            ctx.module
                .assign_name(func, *name)
                .expect("failed to assign name");
        }
    }
}

impl IrGen for CompUnitItem {
    fn irgen(&self, ctx: &mut IrGenContext) {
        ctx.symtable.enter_scope();
        self.irgen_sysylib(ctx);
        match self {
            CompUnitItem::Decl(decl) => decl.irgen(ctx),
            CompUnitItem::FuncDef(func_def) => func_def.irgen(ctx),
        }
        ctx.symtable.exit_scope();
    }
}

impl IrGen for Decl {
    fn irgen(&self, ctx: &mut IrGenContext) { todo!() }
}

impl IrGen for FuncDef {
    fn irgen(&self, ctx: &mut IrGenContext) {
        // TODO: create an ir function.
        todo!()
    }
}

impl IrGen for Block {
    fn irgen(&self, ctx: &mut IrGenContext) {
        ctx.symtable.enter_scope();
        for item in self.blockitem.iter() {
            match item {
                BlockItem::Decl(decl) => decl.irgen(ctx),
                BlockItem::Stmt(stmt) => stmt.irgen(ctx),
            }
        }
        ctx.symtable.exit_scope();
    }
}

impl IrGen for Stmt {
    fn irgen(&self, ctx: &mut IrGenContext) {
        ctx.symtable.enter_scope();
        match self {
            Stmt::Assign(lval, expr) => {}
            Stmt::ExprStmt(expr_stmt) => {}
            Stmt::Block(block) => {
                block.irgen(ctx);
            }
            Stmt::If(cond_expr, then_stmt, else_stmt) => {}
            Stmt::While(cond_expr, loop_stmt) => {}
            Stmt::Break => {}
            Stmt::Continue => {}
            Stmt::Return(return_stmt) => {}
        }
        ctx.symtable.exit_scope();
    }
}
macro_rules! dfg_mut {
    ($module:expr, $function:expr) => {
        $module.function_data_mut($function).unwrap().dfg_mut()
    };
}

impl IrGenContext {
    pub fn new(module_name: impl Into<String>) -> Self {
        let module = Module::new(module_name.into());
        Self {
            module,
            symtable: SymbolTableStack::default(),
            curr_function: None,
            curr_block: None,
        }
    }

    pub fn irgen(&mut self, compunit: CompUnit) {
        self.symtable.enter_scope();
        for item in compunit.item.iter() {
            item.irgen(self);
        }
        self.symtable.exit_scope();
    }

    /// Generate local comp-time value.
    ///
    /// The global comp-time value should be directly generated.
    pub fn irgen_local_comptime_val(&mut self, val: &ComptimeVal) -> Value {
        match val {
            ComptimeVal::Bool(val) => dfg_mut!(self.module, self.curr_function.unwrap())
                .builder()
                .bytes(Type::i1(), vec![*val as u8])
                .unwrap(),
            ComptimeVal::Int(val) => dfg_mut!(self.module, self.curr_function.unwrap())
                .builder()
                .bytes(Type::i32_(), val.to_le_bytes().to_vec())
                .unwrap(),
            ComptimeVal::Float(val) => dfg_mut!(self.module, self.curr_function.unwrap())
                .builder()
                .bytes(Type::float(), val.to_le_bytes().to_vec())
                .unwrap(),
            ComptimeVal::List(_) => {
                unreachable!()
            }
            ComptimeVal::Zeros(ty) => {
                let ir_ty = self.irgen_type(ty);
                dfg_mut!(self.module, self.curr_function.unwrap())
                    .builder()
                    .zero(ir_ty)
                    .unwrap()
            }
        }
    }

    /// Generate local expression.
    pub fn irgen_local_expr(&mut self, expr: Expr) -> Value {
        let ty = expr.ty;

        match expr.kind {
            ExprKind::Const(comptime_val) => self.irgen_local_comptime_val(&comptime_val),
            ExprKind::Binary(op, lhs, rhs) => {
                todo!()
            }
            ExprKind::Unary(op, expr) => {
                todo!()
            }
            ExprKind::LVal(lval) => {
                todo!()
            }
            ExprKind::Coercion(expr) => {
                todo!()
            }
            ExprKind::FuncCall(FuncCall { ident, args, .. }) => {
                todo!()
            }
            ExprKind::InitList(_) => unreachable!(),
        }
    }

    /// Generate global comp-time value.
    pub fn irgen_global_comptime_val(&mut self, val: &ComptimeVal) -> Value {
        match val {
            ComptimeVal::Bool(val) => self
                .module
                .builder()
                .bytes(Type::i1(), vec![*val as u8])
                .unwrap(),
            ComptimeVal::Int(val) => self
                .module
                .builder()
                .bytes(Type::i32_(), val.to_le_bytes().to_vec())
                .unwrap(),
            ComptimeVal::Float(val) => self
                .module
                .builder()
                .bytes(Type::float(), val.to_le_bytes().to_vec())
                .unwrap(),
            ComptimeVal::List(vals) => {
                let ir_vals = vals
                    .iter()
                    .map(|val| self.irgen_global_comptime_val(val))
                    .collect::<Vec<_>>();
                // get the types of the values
                let elem_ty = self
                    .module
                    .with_value_data(ir_vals[0], |data| data.ty())
                    .unwrap();
                let len = ir_vals.len();
                let ir_ty = Type::array(len, elem_ty);
                self.module.builder().array(ir_ty, ir_vals).unwrap()
            }
            ComptimeVal::Zeros(ty) => {
                let ir_ty = self.irgen_type(ty);
                self.module.builder().zero(ir_ty).unwrap()
            }
        }
    }

    pub fn irgen_global_expr(&mut self, expr: Expr) -> Value {
        match expr.kind {
            ExprKind::Const(comptime_val) => self.irgen_global_comptime_val(&comptime_val),
            _ => unreachable!(),
        }
    }

    pub fn irgen_type(&self, ty: &SysyType) -> Type {
        match &ty.kind() {
            SysyTypeKind::Void => Type::void(),
            SysyTypeKind::Int => Type::int(32),
            SysyTypeKind::Bool => Type::int(1),
            SysyTypeKind::Float => Type::float(),
            SysyTypeKind::Array(Some(len), inner_ty) => {
                Type::array(*len, self.irgen_type(inner_ty))
            }
            SysyTypeKind::Array(None, _inner_ty) => Type::ptr(),
            SysyTypeKind::Function(param_tys, ret_ty) => {
                let mut param_ir_types = Vec::new();
                for param_ty in param_tys {
                    param_ir_types.push(self.irgen_type(param_ty));
                }
                Type::function(param_ir_types, self.irgen_type(ret_ty))
            }
        }
    }

    /// Finish the irgen process and return the module.
    pub fn finish(self) -> Module { self.module }
}
