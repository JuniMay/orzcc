use std::collections::HashMap;

use crate::{
    frontend::{
        Block,
        BlockItem,
        CompUnit,
        CompUnitItem,
        ComptimeVal,
        Decl,
        FuncDef,
        Stmt,
        SysyType,
        SysyTypeKind,
    },
    ir::{module::Module, types::Type, values::Value},
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
}

pub trait IrGen {
    fn irgen(&self, ctx: &mut IrGenContext);
}

impl IrGen for CompUnitItem {
    fn irgen(&self, ctx: &mut IrGenContext) {
        match self {
            CompUnitItem::Decl(decl) => decl.irgen(ctx),
            CompUnitItem::FuncDef(func_def) => func_def.irgen(ctx),
        }
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

impl IrGenContext {
    pub fn new(module_name: impl Into<String>) -> Self {
        let module = Module::new(module_name.into());
        Self {
            module,
            symtable: SymbolTableStack::default(),
        }
    }

    pub fn irgen(&mut self, compunit: CompUnit) {
        self.symtable.enter_scope();
        for item in compunit.item.iter() {
            item.irgen(self);
        }
        self.symtable.exit_scope();
    }

    pub fn irgen_expr(&self) { todo!() }

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
