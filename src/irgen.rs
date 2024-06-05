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
        self,
        builders::{
            BuildAggregateConstant,
            BuildBlock,
            BuildGlobalValue,
            BuildLocalValue,
            BuildNonAggregateConstant,
        },
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
    pub curr_ret_ty: Option<SysyType>,
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

    pub fn lookup_mut(&mut self, name: &str) -> Option<&mut SymbolEntry> {
        for table in self.stack.iter_mut().rev() {
            if let Some(entry) = table.entries.get_mut(name) {
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
    pub curr_block: Option<ir::values::Block>,
    pub curr_ret_slot: Option<Value>,
    pub curr_ret_block: Option<ir::values::Block>,
    pub loop_entry_stack: Vec<ir::values::Block>,
    pub loop_exit_stack: Vec<ir::values::Block>,
}

macro_rules! dfg_mut {
    ($module:expr, $function:expr) => {
        &mut $module.function_data_mut($function).unwrap().dfg
    };
}

macro_rules! dfg {
    ($module:expr, $function:expr) => {
        &$module.function_data($function).unwrap().dfg
    };
}

macro_rules! curr_dfg {
    ($ctx:expr) => {
        &$ctx
            .module
            .function_data($ctx.curr_function.unwrap())
            .unwrap()
            .dfg
    };
}

macro_rules! layout_mut {
    ($module:expr, $function:expr) => {
        &mut $module.function_data_mut($function).unwrap().layout
    };
}
macro_rules! curr_layout_mut {
    ($ctx:expr) => {
        &mut $ctx
            .module
            .function_data_mut($ctx.curr_function.unwrap())
            .unwrap()
            .layout
    };
}

// macro_rules! layout {
//     ($module:expr, $function:expr) => {
//         &$module.function_data_mut($function).unwrap().layout
//     };
// }

macro_rules! entry_block {
    ($module:expr, $function:expr) => {
        $module
            .function_data_mut($function)
            .unwrap()
            .layout
            .entry_block()
    };
}

pub trait IrGen {
    fn irgen(&self, ctx: &mut IrGenContext);
}

impl IrGen for CompUnitItem {
    fn irgen(&self, ctx: &mut IrGenContext) {
        match self {
            CompUnitItem::Decl(decl) => match decl {
                Decl::ConstDecl(const_decl) => {
                    for def in &const_decl.defs {
                        let init = ctx.irgen_global_expr(&def.init);
                        let slot = ctx.module.builder().global_slot(init, false).unwrap();
                        ctx.module.assign_name(slot, def.ident.clone()).unwrap();
                        let entry = SymbolEntry {
                            ty: def.init.ty(),
                            comptime_val: None,
                            ir_value: Some(slot),
                        };
                        ctx.symtable.insert(def.ident.clone(), entry);
                    }
                }
                Decl::VarDecl(var_decl) => {
                    for def in &var_decl.defs {
                        let init = ctx.irgen_global_expr(def.init.as_ref().unwrap());
                        let slot = ctx.module.builder().global_slot(init, true).unwrap();
                        ctx.module.assign_name(slot, def.ident.clone()).unwrap();
                        let entry = SymbolEntry {
                            ty: def.init.as_ref().unwrap().ty(),
                            comptime_val: None,
                            ir_value: Some(slot),
                        };
                        ctx.symtable.insert(def.ident.clone(), entry);
                    }
                }
            },
            CompUnitItem::FuncDef(func_def) => func_def.irgen(ctx),
        }
    }
}

impl IrGen for Decl {
    /// IR generation for a local declaration.
    ///
    /// The global declaration is handled in [CompUnitItem].
    fn irgen(&self, ctx: &mut IrGenContext) {
        // for local declaration, a memory slot will be allocated.
        // if the initial value is an array, memcpy and memset will be called.
        let curr_function = ctx.curr_function.unwrap();
        let entry_block = entry_block!(ctx.module, curr_function).unwrap();
        let curr_block = ctx.curr_block.unwrap();
        match self {
            Decl::ConstDecl(decl) => {
                for def in decl.defs.iter() {
                    let ty = def.init.ty.as_ref().unwrap();
                    let ir_ty = IrGenContext::irgen_type(ty);
                    let alloc = dfg_mut!(ctx.module, curr_function)
                        .builder()
                        .alloc(ir_ty)
                        .unwrap();
                    // insert the allocation to the front.
                    layout_mut!(ctx.module, curr_function)
                        .prepend_inst(alloc.into(), entry_block)
                        .unwrap();
                    dfg!(ctx.module, curr_function)
                        .assign_local_value_name(alloc, format!("__SLOT_CONST_{}", &def.ident))
                        .unwrap();
                    let entry = SymbolEntry {
                        ty: ty.clone(),
                        comptime_val: None,
                        ir_value: Some(alloc),
                    };
                    ctx.symtable.insert(&def.ident, entry);
                    if ty.is_array() {
                        // handle the init value of the array.
                        // the const init is expected to be constant.

                        if let ExprKind::Const(val) = &def.init.kind {
                            match val {
                                ComptimeVal::Zeros(ty) => {
                                    // just memset
                                    let memset = ctx.module.get_value_by_name("@memset").unwrap();
                                    let zero = dfg_mut!(ctx.module, curr_function)
                                        .builder()
                                        .zero(Type::i32_())
                                        .unwrap();
                                    let size = dfg_mut!(ctx.module, curr_function)
                                        .builder()
                                        .bytes(Type::i32_(), ty.bytewidth().to_le_bytes().into())
                                        .unwrap();
                                    let call = dfg_mut!(ctx.module, curr_function)
                                        .builder()
                                        .call(Type::void(), memset, vec![alloc, zero, size])
                                        .unwrap();
                                    layout_mut!(ctx.module, curr_function)
                                        .append_inst(call.into(), curr_block)
                                        .unwrap();
                                }
                                ComptimeVal::List(_) if val.is_zero() => {
                                    let ty = val.get_type();
                                    // also memset
                                    let memset = ctx.module.get_value_by_name("@memset").unwrap();
                                    let zero = dfg_mut!(ctx.module, curr_function)
                                        .builder()
                                        .zero(Type::i32_())
                                        .unwrap();
                                    let size = dfg_mut!(ctx.module, curr_function)
                                        .builder()
                                        .bytes(Type::i32_(), ty.bytewidth().to_le_bytes().into())
                                        .unwrap();
                                    let call = dfg_mut!(ctx.module, curr_function)
                                        .builder()
                                        .call(Type::void(), memset, vec![alloc, zero, size])
                                        .unwrap();
                                    layout_mut!(ctx.module, curr_function)
                                        .append_inst(call.into(), curr_block)
                                        .unwrap();
                                }
                                ComptimeVal::List(_) => {
                                    // create global & memcpy
                                    let init = ctx.irgen_global_expr(&def.init);
                                    let global_slot =
                                        ctx.module.builder().global_slot(init, false).unwrap();
                                    ctx.module
                                        .assign_name(global_slot, format!("__DATA_{}", &def.ident))
                                        .unwrap();
                                    let memcpy = ctx.module.get_value_by_name("@memcpy").unwrap();
                                    let size = dfg_mut!(ctx.module, curr_function)
                                        .builder()
                                        .bytes(Type::i32_(), ty.bytewidth().to_le_bytes().into())
                                        .unwrap();
                                    let call = dfg_mut!(ctx.module, curr_function)
                                        .builder()
                                        .call(Type::void(), memcpy, vec![alloc, global_slot, size])
                                        .unwrap();
                                    layout_mut!(ctx.module, curr_function)
                                        .append_inst(call.into(), curr_block)
                                        .unwrap();
                                }
                                _ => unreachable!(),
                            }
                        } else {
                            unreachable!();
                        }
                    } else {
                        // store
                        let init = ctx.irgen_local_expr(&def.init);
                        let store = dfg_mut!(ctx.module, curr_function)
                            .builder()
                            .store(init, alloc)
                            .unwrap();
                        layout_mut!(ctx.module, curr_function)
                            .append_inst(store.into(), curr_block)
                            .unwrap();
                    }
                }
            }
            Decl::VarDecl(decl) => {
                for def in decl.defs.iter() {
                    let ty = def.init.as_ref().unwrap().ty();
                    let ir_ty = IrGenContext::irgen_type(&ty);
                    let alloc = dfg_mut!(ctx.module, curr_function)
                        .builder()
                        .alloc(ir_ty)
                        .unwrap();
                    // insert the allocation to the front.
                    layout_mut!(ctx.module, curr_function)
                        .prepend_inst(alloc.into(), entry_block)
                        .unwrap();
                    dfg!(ctx.module, curr_function)
                        .assign_local_value_name(alloc, format!("__SLOT_VAR_{}", &def.ident))
                        .unwrap();
                    let entry = SymbolEntry {
                        ty: ty.clone(),
                        comptime_val: None,
                        ir_value: Some(alloc),
                    };
                    ctx.symtable.insert(&def.ident, entry);
                    if ty.is_array() {
                        // the init value can be variable, and thus needs to be
                        // loaded and then store.
                        let init = def.init.as_ref().unwrap();

                        let mut global_init = Vec::new();
                        // the indices that cannot be memcpy-ed (because of the use of variables)
                        let mut indices = Vec::new();

                        fn handle_init_list(
                            expr: &Expr,
                            global_init: &mut Vec<ComptimeVal>,
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
                                    global_init.push(ComptimeVal::List(inits));
                                }
                                ExprKind::Const(val) => {
                                    global_init.push(val.clone());
                                }
                                _ => {
                                    // this expr is not a constant, additional handling is needed.
                                    indices.push(curr_indices);
                                    // use a undefined value to represent the value in the
                                    // global_init
                                    global_init.push(ComptimeVal::Zeros(
                                        expr.ty.as_ref().unwrap().clone(),
                                    ));
                                }
                            }
                        }
                        handle_init_list(init, &mut global_init, &mut indices, Vec::new());
                        let global_init = global_init.pop().unwrap();
                        let ty = global_init.get_type();
                        if global_init.is_zero() {
                            // memset
                            let memset = ctx.module.get_value_by_name("@memset").unwrap();
                            let zero = dfg_mut!(ctx.module, curr_function)
                                .builder()
                                .zero(Type::i32_())
                                .unwrap();
                            let size = dfg_mut!(ctx.module, curr_function)
                                .builder()
                                .bytes(Type::i32_(), ty.bytewidth().to_le_bytes().into())
                                .unwrap();
                            let call = dfg_mut!(ctx.module, curr_function)
                                .builder()
                                .call(Type::void(), memset, vec![alloc, zero, size])
                                .unwrap();
                            layout_mut!(ctx.module, curr_function)
                                .append_inst(call.into(), curr_block)
                                .unwrap();
                        } else {
                            // memcpy
                            let global_init = ctx.irgen_global_comptime_val(&global_init);
                            let global_slot = ctx
                                .module
                                .builder()
                                .global_slot(global_init, false)
                                .unwrap();
                            ctx.module
                                .assign_name(global_slot, format!("__DATA_{}", &def.ident))
                                .unwrap();
                            let memcpy = ctx.module.get_value_by_name("@memcpy").unwrap();
                            let size = dfg_mut!(ctx.module, curr_function)
                                .builder()
                                .bytes(Type::i32_(), ty.bytewidth().to_le_bytes().into())
                                .unwrap();
                            let call = dfg_mut!(ctx.module, curr_function)
                                .builder()
                                .call(Type::void(), memcpy, vec![alloc, global_slot, size])
                                .unwrap();
                            layout_mut!(ctx.module, curr_function)
                                .append_inst(call.into(), curr_block)
                                .unwrap();
                        }

                        // set the non-constant values by store
                        for indices in indices.iter() {
                            let mut ir_indices = Vec::new();

                            let mut expr = init;

                            for i in indices.iter() {
                                if let ExprKind::InitList(exprs) = &expr.kind {
                                    expr = &exprs[*i];

                                    let index = dfg_mut!(ctx.module, curr_function)
                                        .builder()
                                        .bytes(Type::i32_(), i.to_le_bytes().to_vec())
                                        .unwrap();

                                    ir_indices.push(index);
                                } else {
                                    unreachable!();
                                }
                            }

                            let gep = dfg_mut!(ctx.module, curr_function)
                                .builder()
                                .getelemptr(alloc, IrGenContext::irgen_type(&ty), ir_indices)
                                .unwrap();
                            let init = ctx.irgen_local_expr(expr);
                            let store = dfg_mut!(ctx.module, curr_function)
                                .builder()
                                .store(init, gep)
                                .unwrap();
                            layout_mut!(ctx.module, curr_function)
                                .append_inst(gep.into(), curr_block)
                                .unwrap();
                            layout_mut!(ctx.module, curr_function)
                                .append_inst(store.into(), curr_block)
                                .unwrap();
                        }
                    } else {
                        // store
                        let init = ctx.irgen_local_expr(def.init.as_ref().unwrap());
                        let store = dfg_mut!(ctx.module, curr_function)
                            .builder()
                            .store(init, alloc)
                            .unwrap();
                        layout_mut!(ctx.module, curr_function)
                            .append_inst(store.into(), curr_block)
                            .unwrap();
                    }
                }
            }
        }
    }
}

impl IrGen for FuncDef {
    fn irgen(&self, ctx: &mut IrGenContext) {
        ctx.symtable.enter_scope();

        let mut param_tys = Vec::new();
        for param in self.params.iter() {
            // symbol table for function parameters
            let ty = if let Some(indices) = &param.indices {
                let ty = param.ty.clone();
                let mut ty = ty;
                for dim in indices.iter().rev() {
                    let dim = dim.try_fold(&ctx.symtable).expect("non-constant dim");
                    ty = SysyType::array(Some(dim.as_int() as usize), ty);
                }
                // the first `[]` array is not in the indices
                SysyType::array(None, ty)
            } else {
                param.ty.clone()
            };
            param_tys.push(ty.clone());
        }

        let ty = SysyType::function(param_tys.clone(), self.ret_ty.clone());
        let ir_ty = IrGenContext::irgen_type(&ty);

        let func = ctx.module.builder().function_def(ir_ty).unwrap();
        ctx.module.assign_name(func, self.ident.clone()).unwrap();
        ctx.curr_function = Some(func);
        let entry = SymbolEntry {
            ty,
            comptime_val: None,
            ir_value: Some(func.into()),
        };
        ctx.symtable.insert_upper(self.ident.clone(), entry, 1);

        let block_params = self
            .params
            .iter()
            .zip(param_tys.into_iter())
            .map(|(param, ty)| {
                let ir_ty = IrGenContext::irgen_type(&ty);
                let arg = dfg_mut!(ctx.module, func)
                    .builder()
                    .block_param(ir_ty)
                    .unwrap();
                dfg!(ctx.module, func)
                    .assign_local_value_name(arg, param.ident.clone())
                    .unwrap();
                let entry = SymbolEntry {
                    ty,
                    comptime_val: None,
                    ir_value: Some(arg),
                };
                ctx.symtable.insert(param.ident.clone(), entry);
                arg
            })
            .collect::<Vec<_>>();

        let block = dfg_mut!(ctx.module, func)
            .builder()
            .block(block_params)
            .unwrap();
        layout_mut!(ctx.module, func).append_block(block).unwrap();
        let ret_block = dfg_mut!(ctx.module, func)
            .builder()
            .block(Vec::new())
            .unwrap();
        ctx.curr_block = Some(block);
        ctx.curr_ret_block = Some(ret_block);

        let ret_ty = IrGenContext::irgen_type(&self.ret_ty);
        let ret_slot = dfg_mut!(ctx.module, func)
            .builder()
            .alloc(ret_ty.clone())
            .unwrap();
        layout_mut!(ctx.module, func)
            .append_inst(ret_slot.into(), block)
            .unwrap();

        ctx.curr_ret_slot = Some(ret_slot);
        self.block.irgen(ctx);

        // append the return block
        layout_mut!(ctx.module, func)
            .append_block(ret_block)
            .unwrap();
        let ret_slot = ctx.curr_ret_slot.unwrap();
        let ret_block = ctx.curr_ret_block.unwrap();

        // load, ret
        let ret_val = dfg_mut!(ctx.module, func)
            .builder()
            .load(ret_ty, ret_slot)
            .unwrap();
        let ret = dfg_mut!(ctx.module, func)
            .builder()
            .return_(Some(ret_val))
            .unwrap();

        layout_mut!(ctx.module, func)
            .append_inst(ret_val.into(), ret_block)
            .unwrap();
        layout_mut!(ctx.module, func)
            .append_inst(ret.into(), ret_block)
            .unwrap();

        ctx.curr_function = None;
        ctx.curr_block = None;
        ctx.curr_ret_slot = None;
        ctx.curr_ret_block = None;
        ctx.symtable.exit_scope();
    }
}

impl IrGen for Block {
    /// IR generation for a block.
    ///
    /// The block in the SysY language is a scope, not a basic block.
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
                let slot = ctx.symtable.lookup(&lval.ident).unwrap().ir_value.unwrap();
                let val = ctx.irgen_local_expr(expr);
                let store = dfg_mut!(ctx.module, ctx.curr_function.unwrap())
                    .builder()
                    .store(val, slot)
                    .unwrap();
                curr_layout_mut!(ctx)
                    .append_inst(store.into(), ctx.curr_block.unwrap())
                    .unwrap();
            }
            Stmt::ExprStmt(expr_stmt) => {
                if let Some(ref expr) = expr_stmt.expr {
                    ctx.irgen_local_expr(expr);
                }
            }
            Stmt::Block(block) => {
                block.irgen(ctx);
            }
            Stmt::If(cond_expr, then_stmt, else_stmt) => {}
            Stmt::While(cond_expr, loop_stmt) => {
                let entry_block = dfg_mut!(ctx.module, ctx.curr_function.unwrap())
                    .builder()
                    .block(Vec::new())
                    .unwrap();
                curr_dfg!(ctx)
                    .assign_block_name(entry_block, "loop_entry")
                    .unwrap();
                let exit_block = dfg_mut!(ctx.module, ctx.curr_function.unwrap())
                    .builder()
                    .block(Vec::new())
                    .unwrap();
                curr_layout_mut!(ctx).append_block(entry_block).unwrap();

                ctx.loop_entry_stack.push(entry_block);
                ctx.loop_exit_stack.push(exit_block);

                ctx.curr_block = Some(entry_block);
                let cond = ctx.irgen_local_expr(cond_expr);
                let body_block = dfg_mut!(ctx.module, ctx.curr_function.unwrap())
                    .builder()
                    .block(Vec::new())
                    .unwrap();
                curr_dfg!(ctx)
                    .assign_block_name(body_block, "loop_body")
                    .unwrap();
                let tail_block = dfg_mut!(ctx.module, ctx.curr_function.unwrap())
                    .builder()
                    .block(Vec::new())
                    .unwrap();
                curr_dfg!(ctx)
                    .assign_block_name(tail_block, "loop_tail")
                    .unwrap();

                let br = dfg_mut!(ctx.module, ctx.curr_function.unwrap())
                    .builder()
                    .branch(cond, body_block, exit_block, Vec::new(), Vec::new())
                    .unwrap();
                curr_layout_mut!(ctx)
                    .append_inst(br.into(), entry_block)
                    .unwrap();
                curr_layout_mut!(ctx).append_block(body_block).unwrap();
                ctx.curr_block = Some(body_block);
                loop_stmt.irgen(ctx);

                curr_layout_mut!(ctx).append_block(tail_block).unwrap();
                let jmp_back = dfg_mut!(ctx.module, ctx.curr_function.unwrap())
                    .builder()
                    .jump(entry_block, Vec::new())
                    .unwrap();
                curr_layout_mut!(ctx)
                    .append_inst(jmp_back.into(), tail_block)
                    .unwrap();
                curr_layout_mut!(ctx).append_block(exit_block).unwrap();
                ctx.curr_block = Some(exit_block);

                ctx.loop_entry_stack.pop();
                ctx.loop_exit_stack.pop();
            }
            Stmt::Break => {
                let dst = ctx.loop_exit_stack.last().unwrap();
                let jmp = dfg_mut!(ctx.module, ctx.curr_function.unwrap())
                    .builder()
                    .jump(*dst, Vec::new())
                    .unwrap();
                curr_layout_mut!(ctx)
                    .append_inst(jmp.into(), ctx.curr_block.unwrap())
                    .unwrap();
            }
            Stmt::Continue => {
                let dst = ctx.loop_entry_stack.last().unwrap();
                let jmp = dfg_mut!(ctx.module, ctx.curr_function.unwrap())
                    .builder()
                    .jump(*dst, Vec::new())
                    .unwrap();
                curr_layout_mut!(ctx)
                    .append_inst(jmp.into(), ctx.curr_block.unwrap())
                    .unwrap();
            }
            Stmt::Return(return_stmt) => {
                if let Some(ref expr) = return_stmt.expr {
                    let val = ctx.irgen_local_expr(expr);
                    let store = dfg_mut!(ctx.module, ctx.curr_function.unwrap())
                        .builder()
                        .store(val, ctx.curr_ret_slot.unwrap())
                        .unwrap();
                    curr_layout_mut!(ctx)
                        .append_inst(store.into(), ctx.curr_block.unwrap())
                        .unwrap();
                }
                // jump to return block.
                let jmp = dfg_mut!(ctx.module, ctx.curr_function.unwrap())
                    .builder()
                    .jump(ctx.curr_ret_block.unwrap(), Vec::new())
                    .unwrap();
                curr_layout_mut!(ctx)
                    .append_inst(jmp.into(), ctx.curr_block.unwrap())
                    .unwrap();
            }
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
            curr_function: None,
            curr_block: None,
            curr_ret_slot: None,
            curr_ret_block: None,
            loop_entry_stack: Vec::new(),
            loop_exit_stack: Vec::new(),
        }
    }

    /// Generate the function declaration for the SysY standard library
    /// functions.
    fn irgen_sysylib(&mut self) {
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
            let entry = self.symtable.lookup_mut(name).unwrap();
            let ty = IrGenContext::irgen_type(&entry.ty);
            let func = self
                .module
                .builder()
                .function_decl(ty)
                .expect("failed to create function");
            self.module
                .assign_name(func, *name)
                .expect("failed to assign name");
            entry.ir_value = Some(func.into());
        }

        // memcpy and memset
        let memcpy_ty = Type::function(vec![Type::ptr(), Type::ptr(), Type::int(32)], Type::void());
        let memcpy = self.module.builder().function_decl(memcpy_ty).unwrap();

        let memset_ty = Type::function(
            vec![Type::ptr(), Type::int(32), Type::int(32)],
            Type::void(),
        );
        let memset = self.module.builder().function_decl(memset_ty).unwrap();

        self.module.assign_name(memcpy, "memcpy").unwrap();
        self.module.assign_name(memset, "memset").unwrap();
    }

    pub fn irgen(&mut self, compunit: CompUnit) {
        self.symtable.enter_scope();
        self.irgen_sysylib();
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
                let ir_ty = Self::irgen_type(ty);
                dfg_mut!(self.module, self.curr_function.unwrap())
                    .builder()
                    .zero(ir_ty)
                    .unwrap()
            }
        }
    }

    /// Generate local expression.
    pub fn irgen_local_expr(&mut self, expr: &Expr) -> Value {
        let ty = expr.ty.as_ref().unwrap();

        match &expr.kind {
            ExprKind::Const(comptime_val) => self.irgen_local_comptime_val(comptime_val),
            ExprKind::Binary(op, lhs, rhs) => {
                todo!()
            }
            ExprKind::Unary(op, expr) => {
                todo!()
            }
            ExprKind::LVal(lval) => {
                // load, require ty and slot
                let entry = self.symtable.lookup(&lval.ident).unwrap();
                let slot = entry.ir_value.unwrap();
                let bound_ty = IrGenContext::irgen_type(&entry.ty);

                // three situations:
                // - non-array: load
                // - array but all indices are provided: load
                // - array and some indices are not provided: getelemptr

                // get the shape of the array
                let mut shape = Vec::new();
                let mut innermost_ty = &entry.ty;

                while let SysyTypeKind::Array(len, elem_ty) = innermost_ty.kind() {
                    shape.push(len.unwrap());
                    innermost_ty = elem_ty;
                }

                let innermost_ty = Self::irgen_type(innermost_ty);

                if shape.is_empty() {
                    // non-array
                    let load = dfg_mut!(self.module, self.curr_function.unwrap())
                        .builder()
                        .load(innermost_ty, slot)
                        .unwrap();
                    let curr_block = self.curr_block.unwrap();
                    layout_mut!(self.module, self.curr_function.unwrap())
                        .append_inst(load.into(), curr_block)
                        .unwrap();
                    load
                } else if lval.indices.len() == shape.len() {
                    // all indices are provided
                    let mut ir_indices = Vec::new();
                    for index in lval.indices.iter() {
                        let index = self.irgen_local_expr(index);
                        ir_indices.push(index);
                    }
                    let gep = dfg_mut!(self.module, self.curr_function.unwrap())
                        .builder()
                        .getelemptr(slot, bound_ty, ir_indices)
                        .unwrap();
                    let curr_block = self.curr_block.unwrap();
                    layout_mut!(self.module, self.curr_function.unwrap())
                        .append_inst(gep.into(), curr_block)
                        .unwrap();
                    // load
                    let load = dfg_mut!(self.module, self.curr_function.unwrap())
                        .builder()
                        .load(innermost_ty, gep)
                        .unwrap();
                    let curr_block = self.curr_block.unwrap();
                    layout_mut!(self.module, self.curr_function.unwrap())
                        .append_inst(load.into(), curr_block)
                        .unwrap();
                    load
                } else {
                    // some indices are not provided
                    let mut ir_indices = Vec::new();
                    for index in lval.indices.iter() {
                        let index = self.irgen_local_expr(index);
                        ir_indices.push(index);
                    }
                    let gep = dfg_mut!(self.module, self.curr_function.unwrap())
                        .builder()
                        .getelemptr(slot, bound_ty, ir_indices)
                        .unwrap();
                    let curr_block = self.curr_block.unwrap();
                    layout_mut!(self.module, self.curr_function.unwrap())
                        .append_inst(gep.into(), curr_block)
                        .unwrap();
                    gep
                }
            }
            ExprKind::Coercion(expr) => {
                let val = match (expr.ty.as_ref().unwrap().kind(), ty.kind()) {
                    (SysyTypeKind::Bool, SysyTypeKind::Int) => {
                        // zeroext
                        let val = self.irgen_local_expr(expr);
                        let zext = dfg_mut!(self.module, self.curr_function.unwrap())
                            .builder()
                            .cast(ir::values::CastOp::ZExt, Self::irgen_type(ty), val)
                            .unwrap();
                        zext
                    }
                    (SysyTypeKind::Int, SysyTypeKind::Bool) => {
                        // icmp ne 0
                        let val = self.irgen_local_expr(expr);
                        let zero = dfg_mut!(self.module, self.curr_function.unwrap())
                            .builder()
                            .zero(Self::irgen_type(expr.ty.as_ref().unwrap()))
                            .unwrap();
                        let icmp = dfg_mut!(self.module, self.curr_function.unwrap())
                            .builder()
                            .binary(
                                ir::values::BinaryOp::ICmp(ir::values::ICmpCond::Ne),
                                val,
                                zero,
                            )
                            .unwrap();
                        icmp
                    }
                    (SysyTypeKind::Int, SysyTypeKind::Float) => {
                        // sitofp
                        let val = self.irgen_local_expr(expr);
                        let sitofp = dfg_mut!(self.module, self.curr_function.unwrap())
                            .builder()
                            .cast(ir::values::CastOp::SIToFp, Self::irgen_type(ty), val)
                            .unwrap();
                        sitofp
                    }
                    (SysyTypeKind::Float, SysyTypeKind::Int) => {
                        // fptosi
                        let val = self.irgen_local_expr(expr);
                        let fptosi = dfg_mut!(self.module, self.curr_function.unwrap())
                            .builder()
                            .cast(ir::values::CastOp::FpToSI, Self::irgen_type(ty), val)
                            .unwrap();
                        fptosi
                    }
                    (SysyTypeKind::Float, SysyTypeKind::Bool) => {
                        // fcmp oeq 0.0
                        let val = self.irgen_local_expr(expr);
                        let zero = dfg_mut!(self.module, self.curr_function.unwrap())
                            .builder()
                            .zero(Self::irgen_type(expr.ty.as_ref().unwrap()))
                            .unwrap();
                        let fcmp = dfg_mut!(self.module, self.curr_function.unwrap())
                            .builder()
                            .binary(
                                ir::values::BinaryOp::FCmp(ir::values::FCmpCond::OEq),
                                val,
                                zero,
                            )
                            .unwrap();
                        fcmp
                    }
                    (SysyTypeKind::Array(_, _), SysyTypeKind::Array(_, _)) => {
                        // bitcast
                        let val = self.irgen_local_expr(expr);
                        let bitcast = dfg_mut!(self.module, self.curr_function.unwrap())
                            .builder()
                            .cast(ir::values::CastOp::Bitcast, Self::irgen_type(ty), val)
                            .unwrap();
                        bitcast // TODO: this should be removed
                    }
                    _ => unreachable!(),
                };

                layout_mut!(self.module, self.curr_function.unwrap())
                    .append_inst(val.into(), self.curr_block.unwrap())
                    .unwrap();
                val
            }
            ExprKind::FuncCall(FuncCall { ident, args, .. }) => {
                dbg!(ident);
                let entry = self.symtable.lookup(ident).unwrap();
                let (_, ret_ty) = entry.ty.as_function().unwrap();
                let func = entry.ir_value.unwrap();
                let args = args
                    .iter()
                    .map(|arg| self.irgen_local_expr(arg))
                    .collect::<Vec<_>>();
                let call = dfg_mut!(self.module, self.curr_function.unwrap())
                    .builder()
                    .call(Self::irgen_type(&ret_ty), func, args)
                    .unwrap();
                let curr_block = self.curr_block.unwrap();
                layout_mut!(self.module, self.curr_function.unwrap())
                    .append_inst(call.into(), curr_block)
                    .unwrap();
                call
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
                let ir_ty = Self::irgen_type(ty);
                self.module.builder().zero(ir_ty).unwrap()
            }
        }
    }

    pub fn irgen_global_expr(&mut self, expr: &Expr) -> Value {
        match &expr.kind {
            ExprKind::Const(comptime_val) => self.irgen_global_comptime_val(comptime_val),
            _ => unreachable!(),
        }
    }

    pub fn irgen_type(ty: &SysyType) -> Type {
        match &ty.kind() {
            SysyTypeKind::Void => Type::void(),
            SysyTypeKind::Int => Type::int(32),
            SysyTypeKind::Bool => Type::int(1),
            SysyTypeKind::Float => Type::float(),
            SysyTypeKind::Array(Some(len), inner_ty) => {
                Type::array(*len, Self::irgen_type(inner_ty))
            }
            SysyTypeKind::Array(None, _inner_ty) => Type::ptr(),
            SysyTypeKind::Function(param_tys, ret_ty) => {
                let mut param_ir_types = Vec::new();
                for param_ty in param_tys {
                    param_ir_types.push(Self::irgen_type(param_ty));
                }
                Type::function(param_ir_types, Self::irgen_type(ret_ty))
            }
        }
    }

    /// Finish the irgen process and return the module.
    pub fn finish(self) -> Module { self.module }
}
