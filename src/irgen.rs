use std::collections::HashMap;

use crate::{
    frontend::{Block, BlockItem, CompUnit, CompUnitItem, ComptimeVal, Decl, FuncDef, Stmt, SyType, SyTypeKind},
    ir::{module::Module, types::Type, values::Value},
};

pub struct SymbolEntry {
    pub ty: Type,
    pub comptime_val: Option<ComptimeVal>,
    pub ir_value: Option<Value>,
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
            Stmt::Assign(lval, expr) => {

            },
            Stmt::ExprStmt(expr_stmt) => {

            },
            Stmt::Block(block) => {

            },
            Stmt::If(cond_expr, then_stmt, else_stmt) => {

            },
            Stmt::While(cond_expr, loop_stmt) => {

            },
            Stmt::Break => {

            },
            Stmt::Continue => {

            },
            Stmt::Return(return_stmt) => {

            },
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

    /// Finish the irgen process and return the module.
    pub fn finish(self) -> Module { self.module }
}
