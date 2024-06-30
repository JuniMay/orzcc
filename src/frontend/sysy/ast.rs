use std::collections::HashMap;

use super::types::{Type, TypeKind as Tk};
use crate::ir;

#[derive(Debug, Clone)]
pub enum ComptimeVal {
    Bool(bool),
    Int(i32),
    Float(f32),
    List(Vec<ComptimeVal>),
    /// Zero-initialized value.
    ///
    /// Non-aggregate types should not use this value.
    Zeros(Type),
    Undef(Type),
}

impl ComptimeVal {
    pub fn unwrap_int(&self) -> i32 {
        match self {
            Self::Bool(b) => *b as i32,
            Self::Int(i) => *i,
            Self::Float(f) => *f as i32,
            Self::List(_) | Self::Zeros(_) | Self::Undef(_) => panic!("expected int"),
        }
    }

    pub fn bool(b: bool) -> Self { Self::Bool(b) }

    pub fn int(i: i32) -> Self { Self::Int(i) }

    pub fn float(f: f32) -> Self { Self::Float(f) }

    pub fn zeros(t: Type) -> Self {
        assert!(t.is_aggregate());
        Self::Zeros(t)
    }

    pub fn undef(t: Type) -> Self { Self::Undef(t) }

    pub fn list(v: Vec<ComptimeVal>) -> Self { Self::List(v) }

    pub fn get_type(&self) -> Type {
        match self {
            Self::Bool(_) => Type::bool(),
            Self::Int(_) => Type::int(),
            Self::Float(_) => Type::float(),
            Self::List(v) => {
                let elem_type = v.first().unwrap().get_type();
                Type::array(elem_type, v.len())
            }
            Self::Zeros(t) => t.clone(),
            Self::Undef(t) => t.clone(),
        }
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Self::Bool(b) => !*b,
            Self::Int(i) => *i == 0,
            Self::Float(f) => *f == 0.0,
            Self::List(v) => v.iter().all(|x| x.is_zero()),
            Self::Zeros(_) => true,
            Self::Undef(_) => false,
        }
    }

    pub fn logical_or(&self, other: &Self) -> Self {
        let lhs = match self {
            Self::Bool(a) => *a,
            Self::Int(a) => *a != 0,
            Self::Float(a) => *a != 0.0,
            Self::List(_) | Self::Zeros(_) | Self::Undef(_) => unreachable!(),
        };

        let rhs = match other {
            Self::Bool(b) => *b,
            Self::Int(b) => *b != 0,
            Self::Float(b) => *b != 0.0,
            Self::List(_) | Self::Zeros(_) | Self::Undef(_) => unreachable!(),
        };

        Self::Bool(lhs || rhs)
    }

    pub fn logical_and(&self, other: &Self) -> Self {
        let lhs = match self {
            Self::Bool(a) => *a,
            Self::Int(a) => *a != 0,
            Self::Float(a) => *a != 0.0,
            Self::List(_) | Self::Zeros(_) | Self::Undef(_) => unreachable!(),
        };

        let rhs = match other {
            Self::Bool(b) => *b,
            Self::Int(b) => *b != 0,
            Self::Float(b) => *b != 0.0,
            Self::List(_) | Self::Zeros(_) | Self::Undef(_) => unreachable!(),
        };

        Self::Bool(lhs && rhs)
    }
}

impl PartialEq for ComptimeVal {
    fn eq(&self, other: &Self) -> bool {
        use ComptimeVal as Cv;
        match (self, other) {
            (Cv::Bool(a), Cv::Bool(b)) => a == b,
            (Cv::Int(a), Cv::Int(b)) => a == b,
            (Cv::Float(a), Cv::Float(b)) => a == b,

            // coercion situations, int -> float
            (Cv::Int(a), Cv::Float(b)) => (*a as f32) == *b,
            (Cv::Float(a), Cv::Int(b)) => *a == (*b as f32),

            // coercion situations, bool -> int
            (Cv::Bool(a), Cv::Int(b)) => (*a as i32) == *b,
            (Cv::Int(a), Cv::Bool(b)) => *a == (*b as i32),

            // coercion situations, bool -> float
            (Cv::Bool(a), Cv::Float(b)) => (*a as i32) as f32 == *b,
            (Cv::Float(a), Cv::Bool(b)) => *a == (*b as i32) as f32,

            _ => false,
        }
    }
}

impl Eq for ComptimeVal {}

impl PartialOrd for ComptimeVal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use ComptimeVal as Cv;
        match (self, other) {
            (Cv::Bool(a), Cv::Bool(b)) => a.partial_cmp(b),
            (Cv::Int(a), Cv::Int(b)) => a.partial_cmp(b),
            (Cv::Float(a), Cv::Float(b)) => a.partial_cmp(b),

            // coercion situations, int -> float
            (Cv::Int(a), Cv::Float(b)) => (*a as f32).partial_cmp(b),
            (Cv::Float(a), Cv::Int(b)) => a.partial_cmp(&(*b as f32)),

            // coercion situations, bool -> int
            (Cv::Bool(a), Cv::Int(b)) => (*a as i32).partial_cmp(b),
            (Cv::Int(a), Cv::Bool(b)) => a.partial_cmp(&(*b as i32)),

            // coercion situations, bool -> float
            (Cv::Bool(a), Cv::Float(b)) => (*a as i32 as f32).partial_cmp(b),
            (Cv::Float(a), Cv::Bool(b)) => a.partial_cmp(&(*b as i32 as f32)),

            _ => None,
        }
    }
}

impl std::ops::Neg for ComptimeVal {
    type Output = Self;

    fn neg(self) -> Self {
        use ComptimeVal as Cv;
        match self {
            Cv::Bool(a) => Cv::Int(-(a as i32)),
            Cv::Int(a) => Cv::Int(-a),
            Cv::Float(a) => Cv::Float(-a),

            Cv::List(_) | Cv::Zeros(_) | Cv::Undef(_) => panic!("unsupported operation"),
        }
    }
}

impl std::ops::Not for ComptimeVal {
    type Output = Self;

    fn not(self) -> Self {
        use ComptimeVal as Cv;
        match self {
            Cv::Bool(a) => Cv::Bool(!a),
            Cv::Int(a) => Cv::Bool(a != 0),
            Cv::Float(a) => Cv::Bool(a != 0.0),

            Cv::List(_) | Cv::Zeros(_) | Cv::Undef(_) => panic!("unsupported operation"),
        }
    }
}

impl std::ops::Add for ComptimeVal {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        use ComptimeVal as Cv;
        match (self, other) {
            (Cv::Int(a), Cv::Int(b)) => Cv::Int(a + b),
            (Cv::Float(a), Cv::Float(b)) => Cv::Float(a + b),

            // coercion situations, int -> float
            (Cv::Int(a), Cv::Float(b)) => Cv::Float(a as f32 + b),
            (Cv::Float(a), Cv::Int(b)) => Cv::Float(a + b as f32),

            // coercion situations, bool -> int
            (Cv::Bool(a), Cv::Int(b)) => Cv::Int(a as i32 + b),
            (Cv::Int(a), Cv::Bool(b)) => Cv::Int(a + b as i32),
            (Cv::Bool(a), Cv::Bool(b)) => Cv::Int(a as i32 + b as i32),

            // coercion situations, bool -> float
            (Cv::Bool(a), Cv::Float(b)) => Cv::Float(a as i32 as f32 + b),
            (Cv::Float(a), Cv::Bool(b)) => Cv::Float(a + b as i32 as f32),
            _ => panic!("unsupported operation"),
        }
    }
}

impl std::ops::Sub for ComptimeVal {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        use ComptimeVal as Cv;

        match (self, other) {
            (Cv::Int(a), Cv::Int(b)) => Cv::Int(a - b),
            (Cv::Float(a), Cv::Float(b)) => Cv::Float(a - b),

            // coercion situations, int -> float
            (Cv::Int(a), Cv::Float(b)) => Cv::Float(a as f32 - b),
            (Cv::Float(a), Cv::Int(b)) => Cv::Float(a - b as f32),

            // coercion situations, bool -> int
            (Cv::Bool(a), Cv::Int(b)) => Cv::Int(a as i32 - b),
            (Cv::Int(a), Cv::Bool(b)) => Cv::Int(a - b as i32),
            (Cv::Bool(a), Cv::Bool(b)) => Cv::Int(a as i32 - b as i32),

            // coercion situations, bool -> float
            (Cv::Bool(a), Cv::Float(b)) => Cv::Float(a as i32 as f32 - b),
            (Cv::Float(a), Cv::Bool(b)) => Cv::Float(a - b as i32 as f32),
            _ => panic!("unsupported operation"),
        }
    }
}

impl std::ops::Mul for ComptimeVal {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        use ComptimeVal as Cv;
        match (self, other) {
            (Cv::Int(a), Cv::Int(b)) => Cv::Int(a * b),
            (Cv::Float(a), Cv::Float(b)) => Cv::Float(a * b),

            // coercion situations, int -> float
            (Cv::Int(a), Cv::Float(b)) => Cv::Float(a as f32 * b),
            (Cv::Float(a), Cv::Int(b)) => Cv::Float(a * b as f32),

            // coercion situations, bool -> int
            (Cv::Bool(a), Cv::Int(b)) => Cv::Int(a as i32 * b),
            (Cv::Int(a), Cv::Bool(b)) => Cv::Int(a * b as i32),
            (Cv::Bool(a), Cv::Bool(b)) => Cv::Int(a as i32 * b as i32),

            // coercion situations, bool -> float
            (Cv::Bool(a), Cv::Float(b)) => Cv::Float(a as i32 as f32 * b),
            (Cv::Float(a), Cv::Bool(b)) => Cv::Float(a * b as i32 as f32),
            _ => panic!("unsupported operation"),
        }
    }
}

impl std::ops::Div for ComptimeVal {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        use ComptimeVal as Cv;
        match (self, other) {
            (Cv::Int(a), Cv::Int(b)) => Cv::Int(a / b),
            (Cv::Float(a), Cv::Float(b)) => Cv::Float(a / b),

            // coercion situations, int -> float
            (Cv::Int(a), Cv::Float(b)) => Cv::Float(a as f32 / b),
            (Cv::Float(a), Cv::Int(b)) => Cv::Float(a / b as f32),

            // coercion situations, bool -> int
            (Cv::Bool(a), Cv::Int(b)) => Cv::Int(a as i32 / b),
            (Cv::Int(a), Cv::Bool(b)) => Cv::Int(a / b as i32),
            (Cv::Bool(a), Cv::Bool(b)) => Cv::Int(a as i32 / b as i32),

            // coercion situations, bool -> float
            (Cv::Bool(a), Cv::Float(b)) => Cv::Float(a as i32 as f32 / b),
            (Cv::Float(a), Cv::Bool(b)) => Cv::Float(a / b as i32 as f32),
            _ => panic!("unsupported operation"),
        }
    }
}

impl std::ops::Rem for ComptimeVal {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        use ComptimeVal as Cv;
        match (self, other) {
            (Cv::Int(a), Cv::Int(b)) => Cv::Int(a % b),

            // bool -> int
            (Cv::Bool(a), Cv::Bool(b)) => Cv::Int(a as i32 % b as i32),
            (Cv::Bool(a), Cv::Int(b)) => Cv::Int(a as i32 % b),
            (Cv::Int(a), Cv::Bool(b)) => Cv::Int(a % b as i32),

            _ => panic!("unsupported operation"),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Gt,
    Le,
    Ge,
    Eq,
    Ne,
    LogicalAnd,
    LogicalOr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    /// Logical not.
    Not,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncCall {
    pub ident: String,
    pub args: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct LVal {
    pub ident: String,
    pub indices: Vec<Expr>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ExprKind {
    Const(ComptimeVal),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    FuncCall(FuncCall),
    LVal(LVal),
    InitList(Vec<Expr>),
    Coercion(Box<Expr>),
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Option<Type>,
}

impl PartialEq for Expr {
    // TODO: Some patterns can be folded on AST to reduce the depth.
    fn eq(&self, other: &Self) -> bool { self.kind == other.kind }
}

impl Eq for Expr {}

impl Expr {
    pub fn const_(val: ComptimeVal) -> Self {
        let ty = val.get_type();
        Self {
            kind: ExprKind::Const(val),
            ty: Some(ty),
        }
    }

    pub fn binary(op: BinaryOp, lhs: Expr, rhs: Expr) -> Self {
        Self {
            kind: ExprKind::Binary(op, Box::new(lhs), Box::new(rhs)),
            ty: None,
        }
    }

    pub fn unary(op: UnaryOp, expr: Expr) -> Self {
        Self {
            kind: ExprKind::Unary(op, Box::new(expr)),
            ty: None,
        }
    }

    pub fn func_call(ident: String, args: Vec<Expr>) -> Self {
        Self {
            kind: ExprKind::FuncCall(FuncCall { ident, args }),
            ty: None,
        }
    }

    pub fn lval(lval: LVal) -> Self {
        Self {
            kind: ExprKind::LVal(lval),
            ty: None,
        }
    }

    pub fn init_list(exprs: Vec<Expr>) -> Self {
        Self {
            kind: ExprKind::InitList(exprs),
            ty: None,
        }
    }

    pub fn coercion(expr: Expr, to: Type) -> Self {
        if let Some(ref from) = expr.ty {
            if from == &to {
                return expr;
            }
        }

        Self {
            kind: ExprKind::Coercion(Box::new(expr)),
            ty: Some(to),
        }
    }
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expr: Option<Expr>,
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub expr: Option<Expr>,
}

#[derive(Debug)]
pub enum Stmt {
    Assign(LVal, Expr),
    Expr(ExprStmt),
    Block(Block),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Break,
    Continue,
    Return(ReturnStmt),
}

#[derive(Debug)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

#[derive(Debug)]
pub struct Block {
    pub items: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub shape: Vec<Expr>,
    pub init: Expr,
}

#[derive(Debug)]
pub struct VarDef {
    pub ident: String,
    pub shape: Vec<Expr>,
    pub init: Option<Expr>,
}

#[derive(Debug)]
pub struct ConstDecl {
    pub ty: Type,
    pub defs: Vec<ConstDef>,
}

#[derive(Debug)]
pub struct VarDecl {
    pub ty: Type,
    pub defs: Vec<VarDef>,
}

#[derive(Debug)]
pub struct FuncFParam {
    pub ty: Type,
    pub ident: String,
    pub indices: Option<Vec<Expr>>,
}

#[derive(Debug)]
pub struct FuncDef {
    pub ret_ty: Type,
    pub ident: String,
    pub params: Vec<FuncFParam>,
    pub body: Block,
}

#[derive(Debug)]
pub enum Item {
    Decl(Decl),
    FuncDef(FuncDef),
}

#[derive(Debug)]
pub struct CompUnit {
    pub items: Vec<Item>,
}

#[derive(Debug)]
pub struct SymbolEntry {
    pub ty: Type,
    pub comptime: Option<ComptimeVal>,
    pub ir_value: Option<ir::Value>,
}

impl SymbolEntry {
    pub fn from_ty(ty: Type) -> Self {
        Self {
            ty,
            comptime: None,
            ir_value: None,
        }
    }
}

#[derive(Default)]
pub struct SymbolTable {
    stack: Vec<HashMap<String, SymbolEntry>>,
    pub curr_ret_ty: Option<Type>,
}

impl SymbolTable {
    pub fn enter_scope(&mut self) { self.stack.push(HashMap::new()); }

    pub fn exit_scope(&mut self) { self.stack.pop(); }

    pub fn insert(&mut self, name: impl Into<String>, entry: SymbolEntry) {
        self.stack.last_mut().unwrap().insert(name.into(), entry);
    }

    pub fn insert_upper(&mut self, name: impl Into<String>, entry: SymbolEntry, upper: usize) {
        self.stack
            .iter_mut()
            .rev()
            .nth(upper)
            .unwrap()
            .insert(name.into(), entry);
    }

    pub fn lookup(&self, name: &str) -> Option<&SymbolEntry> {
        for scope in self.stack.iter().rev() {
            if let Some(entry) = scope.get(name) {
                return Some(entry);
            }
        }
        None
    }

    pub fn lookup_mut(&mut self, name: &str) -> Option<&mut SymbolEntry> {
        for scope in self.stack.iter_mut().rev() {
            if let Some(entry) = scope.get_mut(name) {
                return Some(entry);
            }
        }
        None
    }

    pub fn register_sysylib(&mut self) {
        assert_eq!(self.stack.len(), 1);

        let getint = SymbolEntry::from_ty(Type::func(vec![], Type::int()));
        let getch = SymbolEntry::from_ty(Type::func(vec![], Type::int()));
        let getfloat = SymbolEntry::from_ty(Type::func(vec![], Type::float()));
        let getarray = SymbolEntry::from_ty(Type::func(vec![Type::ptr(Type::int())], Type::int()));
        let getfarray =
            SymbolEntry::from_ty(Type::func(vec![Type::ptr(Type::float())], Type::int()));
        let putint = SymbolEntry::from_ty(Type::func(vec![Type::int()], Type::void()));
        let putch = SymbolEntry::from_ty(Type::func(vec![Type::int()], Type::void()));
        let putfloat = SymbolEntry::from_ty(Type::func(vec![Type::float()], Type::void()));
        let putarray = SymbolEntry::from_ty(Type::func(
            vec![Type::int(), Type::ptr(Type::int())],
            Type::void(),
        ));
        let putfarray = SymbolEntry::from_ty(Type::func(
            vec![Type::int(), Type::ptr(Type::float())],
            Type::void(),
        ));

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

        let starttime = SymbolEntry::from_ty(Type::func(vec![Type::int()], Type::void()));
        let stoptime = SymbolEntry::from_ty(Type::func(vec![Type::int()], Type::void()));

        self.insert("_sysy_starttime", starttime);
        self.insert("_sysy_stoptime", stoptime);
    }
}

impl CompUnit {
    pub fn type_check(&mut self) {
        let mut symtable = SymbolTable::default();
        symtable.enter_scope();
        symtable.register_sysylib();

        for item in self.items.iter_mut() {
            item.type_check(&mut symtable);
        }

        symtable.exit_scope();
    }
}

impl Item {
    pub fn type_check(&mut self, symtable: &mut SymbolTable) {
        match self {
            Item::Decl(decl) => match decl {
                Decl::ConstDecl(decl) => decl.type_check(symtable),
                Decl::VarDecl(decl) => decl.type_check(symtable),
            },
            Item::FuncDef(FuncDef {
                ret_ty,
                ident,
                params,
                body,
            }) => {
                // symbol table for function parameters
                symtable.enter_scope();

                let mut param_tys = Vec::new();
                for param in params.iter() {
                    let ty = if let Some(ref indices) = param.indices {
                        let mut ty = param.ty.clone();
                        for dim in indices.iter().rev() {
                            let dim = dim.try_fold(symtable).expect("const expr expected");
                            ty = Type::array(ty, dim.unwrap_int() as usize);
                        }
                        ty = Type::ptr(ty);
                        ty
                    } else {
                        param.ty.clone()
                    };
                    param_tys.push(ty.clone());
                    symtable.insert(param.ident.clone(), SymbolEntry::from_ty(ty));
                }

                let func_ty = Type::func(param_tys, ret_ty.clone());
                symtable.insert_upper(ident.clone(), SymbolEntry::from_ty(func_ty), 1);
                symtable.curr_ret_ty = Some(ret_ty.clone());

                body.type_check(symtable);

                symtable.curr_ret_ty = None;
                symtable.exit_scope();
            }
        }
    }
}

impl ConstDecl {
    pub fn type_check(&mut self, symtable: &mut SymbolTable) {
        let mut new_defs = Vec::new();
        for mut def in self.defs.drain(..) {
            // fold the shapes
            let mut shape = def
                .shape
                .drain(..)
                .map(|expr| {
                    expr.try_fold(symtable)
                        .expect("non-constant dim")
                        .unwrap_int()
                })
                .collect::<Vec<_>>();

            let mut ty = self.ty.clone();
            for dim in shape.iter().rev() {
                ty = Type::array(ty, *dim as usize);
            }
            def.init = def.init.type_check(Some(&ty), symtable);
            let folded = def.init.try_fold(symtable).expect("non-constant init");
            def.init = Expr::const_(folded.clone());

            def.shape = shape
                .drain(..)
                .map(ComptimeVal::int)
                .map(Expr::const_)
                .map(|mut e| {
                    e.ty = Some(Type::int());
                    e
                })
                .collect::<Vec<_>>();
            symtable.insert(
                def.ident.clone(),
                SymbolEntry {
                    ty,
                    comptime: Some(folded),
                    ir_value: None,
                },
            );
            new_defs.push(def);
        }
        self.defs = new_defs;
    }
}

impl VarDecl {
    pub fn type_check(&mut self, symtable: &mut SymbolTable) {
        let mut new_defs = Vec::new();
        for mut def in self.defs.drain(..) {
            // fold the shapes
            let mut shape = def
                .shape
                .drain(..)
                .map(|expr| {
                    expr.try_fold(symtable)
                        .expect("non-constant dim")
                        .unwrap_int()
                })
                .collect::<Vec<_>>();

            let mut ty = self.ty.clone();
            for dim in shape.iter().rev() {
                ty = Type::array(ty, *dim as usize);
            }

            let init = def
                .init
                .map(|init| {
                    // fold as much as possible
                    let typed_init = init.type_check(Some(&ty), symtable);
                    match typed_init.try_fold(symtable) {
                        Some(val) => Expr::const_(val),
                        None => typed_init,
                    }
                })
                .unwrap_or_else(|| {
                    let undef = ComptimeVal::undef(ty.clone());
                    Expr::const_(undef)
                });

            def.init = Some(init);
            def.shape = shape
                .drain(..)
                .map(ComptimeVal::int)
                .map(Expr::const_)
                .map(|mut e| {
                    e.ty = Some(Type::int());
                    e
                })
                .collect::<Vec<_>>();

            symtable.insert(def.ident.clone(), SymbolEntry::from_ty(ty));
            new_defs.push(def);
        }
        self.defs = new_defs;
    }
}

impl Block {
    pub fn type_check(&mut self, symtable: &mut SymbolTable) {
        symtable.enter_scope();
        let mut new_items = Vec::new();
        for item in self.items.drain(..) {
            let item = match item {
                BlockItem::Decl(decl) => match decl {
                    Decl::ConstDecl(mut decl) => {
                        decl.type_check(symtable);
                        BlockItem::Decl(Decl::ConstDecl(decl))
                    }
                    Decl::VarDecl(mut decl) => {
                        decl.type_check(symtable);
                        BlockItem::Decl(Decl::VarDecl(decl))
                    }
                },
                BlockItem::Stmt(stmt) => {
                    let stmt = stmt.type_check(symtable);
                    BlockItem::Stmt(stmt)
                }
            };
            new_items.push(item);
        }
        self.items = new_items;
        symtable.exit_scope();
    }
}

impl Stmt {
    pub fn type_check(self, symtable: &mut SymbolTable) -> Self {
        match self {
            Stmt::Assign(LVal { ident, indices }, expr) => {
                let entry = symtable.lookup(&ident).expect("variable not found");

                // type check indices
                let indices: Vec<Expr> = indices
                    .into_iter()
                    .map(|index| index.type_check(Some(&Type::int()), symtable))
                    .collect();

                let mut ty = &entry.ty;

                for _ in 0..indices.len() {
                    ty = ty.inner_ty().unwrap();
                }

                let expr = expr.type_check(Some(ty), symtable);
                Stmt::Assign(LVal { ident, indices }, expr)
            }
            Stmt::Expr(ExprStmt { expr }) => {
                let expr = expr.map(|expr| expr.type_check(None, symtable));
                Stmt::Expr(ExprStmt { expr })
            }
            Stmt::Block(mut block) => {
                block.type_check(symtable);
                Stmt::Block(block)
            }
            Stmt::Break => Stmt::Break,
            Stmt::Continue => Stmt::Continue,
            Stmt::Return(ReturnStmt { expr }) => {
                let expr =
                    expr.map(|expr| expr.type_check(symtable.curr_ret_ty.as_ref(), symtable));

                if expr.is_none() {
                    return Stmt::Return(ReturnStmt { expr });
                }

                let mut expr = expr.unwrap();
                let ret_ty = symtable.curr_ret_ty.as_ref().unwrap();

                if ret_ty.is_float() {
                    expr = Expr::coercion(expr, Type::float());
                } else if ret_ty.is_int() {
                    expr = Expr::coercion(expr, Type::int());
                } else {
                    panic!("unsupported return type");
                }

                Stmt::Return(ReturnStmt { expr: Some(expr) })
            }
            Stmt::If(cond, then_block, else_block) => {
                let cond = cond.type_check(Some(&Type::bool()), symtable);
                let then_block = then_block.type_check(symtable);
                let else_block = else_block.map(|block| block.type_check(symtable));
                Stmt::If(cond, Box::new(then_block), else_block.map(Box::new))
            }
            Stmt::While(cond, block) => {
                let cond = cond.type_check(Some(&Type::bool()), symtable);
                let block = block.type_check(symtable);
                Stmt::While(cond, Box::new(block))
            }
        }
    }
}

impl Expr {
    pub fn ty(&self) -> &Type { self.ty.as_ref().unwrap() }

    pub fn canonicalize_init_list(&mut self, ty: &Type, symtable: &SymbolTable) {
        if let ExprKind::InitList(ref mut vals) = self.kind {
            let (sub_ty, len) = ty.unwrap_array();
            let base_ty = ty.array_base().clone();

            if &base_ty == sub_ty {
                // type check, coerce the elements
                let mut new_vals = Vec::new();
                for val in vals.drain(..) {
                    let val = val.type_check(Some(&base_ty), symtable);
                    if let Some(val) = val.try_fold(symtable) {
                        new_vals.push(Expr::const_(val));
                    } else {
                        new_vals.push(val);
                    }
                }
                if new_vals.len() < len {
                    for _ in new_vals.len()..len {
                        new_vals.push(Expr::const_(ComptimeVal::int(0)));
                    }
                }
                if new_vals.iter().all(|val| {
                    if let ExprKind::Const(ref val) = val.kind {
                        val.is_zero()
                    } else {
                        false
                    }
                }) {
                    // all zeros
                    *self = Expr::const_(ComptimeVal::zeros(ty.clone()));
                } else {
                    *self = Expr::init_list(new_vals);
                    self.ty = Some(ty.clone());
                }

                return;
            }

            let mut elem_init_list = Vec::new();
            let mut new_init_list = Vec::new();

            let elem_total_len = sub_ty.bytewidth() / base_ty.bytewidth();

            for mut val in vals.drain(..) {
                if let ExprKind::InitList(_) = val.kind {
                    if !elem_init_list.is_empty() {
                        elem_init_list.push(val);
                    } else {
                        val.canonicalize_init_list(sub_ty, symtable);
                        new_init_list.push(val);
                    }
                } else {
                    elem_init_list.push(val);
                    if elem_init_list.len() == elem_total_len {
                        let mut init_list = Expr::init_list(elem_init_list);
                        init_list.canonicalize_init_list(sub_ty, symtable);
                        new_init_list.push(init_list);
                        elem_init_list = Vec::new();
                    }
                }
            }

            if !elem_init_list.is_empty() {
                let mut init_list = Expr::init_list(elem_init_list);
                init_list.canonicalize_init_list(sub_ty, symtable);
                new_init_list.push(init_list);
            }

            if new_init_list.len() < len {
                for _ in new_init_list.len()..len {
                    let mut init_list = Expr::init_list(Vec::new());
                    init_list.canonicalize_init_list(sub_ty, symtable);
                    new_init_list.push(init_list);
                }
            }

            *self = Expr::init_list(new_init_list);
            self.ty = Some(ty.clone());
        } else {
            panic!("not an init list");
        }
    }

    pub fn try_fold(&self, symtable: &SymbolTable) -> Option<ComptimeVal> {
        match &self.kind {
            ExprKind::Const(val) => Some(val.clone()),
            ExprKind::Binary(op, lhs, rhs) => {
                let lhs = lhs.try_fold(symtable)?;
                let rhs = rhs.try_fold(symtable)?;

                use BinaryOp as Bo;

                match op {
                    Bo::Add => Some(lhs + rhs),
                    Bo::Sub => Some(lhs - rhs),
                    Bo::Mul => Some(lhs * rhs),
                    Bo::Div => Some(lhs / rhs),
                    Bo::Mod => Some(lhs % rhs),
                    Bo::Lt => Some(ComptimeVal::Bool(lhs < rhs)),
                    Bo::Gt => Some(ComptimeVal::Bool(lhs > rhs)),
                    Bo::Le => Some(ComptimeVal::Bool(lhs <= rhs)),
                    Bo::Ge => Some(ComptimeVal::Bool(lhs >= rhs)),
                    Bo::Eq => Some(ComptimeVal::Bool(lhs == rhs)),
                    Bo::Ne => Some(ComptimeVal::Bool(lhs != rhs)),
                    Bo::LogicalAnd => Some(lhs.logical_and(&rhs)),
                    Bo::LogicalOr => Some(lhs.logical_or(&rhs)),
                }
            }
            ExprKind::Unary(op, expr) => {
                let expr = expr.try_fold(symtable)?;

                match op {
                    UnaryOp::Neg => Some(-expr),
                    UnaryOp::Not => Some(!expr),
                }
            }
            ExprKind::FuncCall(_) => None,
            ExprKind::LVal(LVal { ident, indices }) => {
                let entry = symtable.lookup(ident).unwrap();
                let val = entry.comptime.as_ref()?;
                let mut folded_indices = Vec::new();

                for index in indices {
                    let index = match index.try_fold(symtable)? {
                        ComptimeVal::Int(i) => i,
                        ComptimeVal::Float(f) => f as i32,
                        ComptimeVal::Bool(b) => b as i32,
                        ComptimeVal::List(_) | ComptimeVal::Zeros(_) | ComptimeVal::Undef(_) => {
                            return None
                        }
                    };
                    folded_indices.push(index);
                }

                let val = if let ComptimeVal::List(_) = val {
                    let mut val = val.clone();
                    for index in folded_indices {
                        if let ComptimeVal::List(list) = val {
                            val = list[index as usize].clone();
                        } else {
                            return None;
                        }
                    }
                    val
                } else {
                    val.clone()
                };
                Some(val)
            }
            ExprKind::InitList(vals) => {
                let vals = vals
                    .iter()
                    .map(|val| val.try_fold(symtable))
                    .collect::<Option<Vec<_>>>()?;

                Some(ComptimeVal::list(vals))
            }
            ExprKind::Coercion(expr) => {
                let expr = expr.try_fold(symtable)?;
                match self.ty.as_ref().unwrap().kind() {
                    Tk::Bool => {
                        let expr = if let ComptimeVal::Bool(val) = expr {
                            val as i32
                        } else {
                            panic!("unsupported type coercion")
                        };
                        Some(ComptimeVal::Int(expr))
                    }
                    Tk::Int => {
                        let expr = if let ComptimeVal::Int(val) = expr {
                            val
                        } else if let ComptimeVal::Float(val) = expr {
                            val as i32
                        } else {
                            panic!("unsupported type coercion")
                        };
                        Some(ComptimeVal::Int(expr))
                    }
                    Tk::Float => {
                        let expr = match expr {
                            ComptimeVal::Bool(val) => val as i32 as f32,
                            ComptimeVal::Int(val) => val as f32,
                            ComptimeVal::Float(val) => val,
                            ComptimeVal::List(_)
                            | ComptimeVal::Zeros(_)
                            | ComptimeVal::Undef(_) => panic!("unsupported type coercion"),
                        };
                        Some(ComptimeVal::Float(expr))
                    }
                    Tk::Void | Tk::Ptr(_) | Tk::Array(..) | Tk::Func(..) => {
                        panic!("unsupported type coercion")
                    }
                }
            }
        }
    }

    pub fn type_check(mut self, expect: Option<&Type>, symtable: &SymbolTable) -> Self {
        if self.ty.is_some() && expect.is_none() {
            return self;
        }

        let mut expr = match self.kind {
            ExprKind::Const(_) => self,
            ExprKind::Binary(op, lhs, rhs) => {
                let sub_expect = if matches!(op, BinaryOp::LogicalAnd | BinaryOp::LogicalOr) {
                    Some(Type::bool())
                } else {
                    None
                };

                let mut lhs = lhs.type_check(sub_expect.as_ref(), symtable);
                let mut rhs = rhs.type_check(sub_expect.as_ref(), symtable);

                let lhs_ty = lhs.ty();
                let rhs_ty = rhs.ty();

                match (lhs_ty.kind(), rhs_ty.kind()) {
                    (Tk::Bool, Tk::Int) => {
                        lhs = Expr::coercion(lhs, Type::int());
                    }
                    (Tk::Bool, Tk::Float) => {
                        let tmp = Expr::coercion(lhs, Type::int());
                        lhs = Expr::coercion(tmp, Type::float());
                    }
                    (Tk::Int, Tk::Bool) => {
                        rhs = Expr::coercion(rhs, Type::int());
                    }
                    (Tk::Int, Tk::Float) => {
                        lhs = Expr::coercion(lhs, Type::float());
                    }
                    (Tk::Float, Tk::Bool) => {
                        let tmp = Expr::coercion(rhs, Type::int());
                        rhs = Expr::coercion(tmp, Type::float());
                    }
                    (Tk::Float, Tk::Int) => {
                        rhs = Expr::coercion(rhs, Type::float());
                    }
                    _ => {
                        if lhs_ty != rhs_ty {
                            panic!("unsupported type coercion: {:?} -> {:?}", lhs_ty, rhs_ty);
                        }
                    }
                }

                let lhs_ty = lhs.ty().clone();
                let mut expr = Expr::binary(op, lhs, rhs);

                match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod => {
                        expr.ty = Some(lhs_ty.clone());
                    }
                    BinaryOp::Lt
                    | BinaryOp::Gt
                    | BinaryOp::Le
                    | BinaryOp::Ge
                    | BinaryOp::Eq
                    | BinaryOp::Ne => {
                        expr.ty = Some(Type::bool());
                    }
                    BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                        expr.ty = Some(Type::bool());
                    }
                }
                expr
            }
            ExprKind::Coercion(_) => unreachable!(),
            ExprKind::FuncCall(FuncCall { ident, args }) => {
                let entry = symtable.lookup(&ident).unwrap();

                let (param_tys, ret_ty) = entry.ty.unwrap_func();

                let args = args
                    .into_iter()
                    .zip(param_tys)
                    .map(|(arg, ty)| arg.type_check(Some(ty), symtable))
                    .collect();

                let mut expr = Expr::func_call(ident, args);
                expr.ty = Some(ret_ty.clone());
                expr
            }
            ExprKind::InitList(ref list) => {
                if list.is_empty() {
                    let ty = expect.unwrap();
                    let val = ComptimeVal::zeros(ty.clone());
                    let expr = Expr::const_(val);
                    return expr;
                }

                self.canonicalize_init_list(expect.unwrap(), symtable);
                self
            }
            ExprKind::LVal(LVal { ident, indices }) => {
                let entry = symtable.lookup(&ident).unwrap();

                let indices: Vec<Expr> = indices
                    .into_iter()
                    .map(|index| {
                        if let Some(folded) = index.try_fold(symtable) {
                            Expr::const_(folded)
                        } else {
                            index.type_check(None, symtable)
                        }
                    })
                    .collect();

                let mut ty = entry.ty.clone();
                for _ in 0..indices.len() {
                    ty = ty.inner_ty().unwrap().clone();
                }
                let mut expr = Expr::lval(LVal { ident, indices });
                expr.ty = Some(ty);
                expr
            }
            ExprKind::Unary(op, expr) => {
                let mut expr = expr.type_check(None, symtable);
                let ty = match op {
                    UnaryOp::Neg => {
                        if expr.ty().is_bool() {
                            // if this is bool, convert to int first
                            expr = Expr::coercion(expr, Type::int());
                        }
                        let ty = expr.ty();
                        if ty.is_int() || ty.is_float() {
                            ty.clone()
                        } else {
                            panic!("unsupported type for negation: {:?}", ty);
                        }
                    }
                    UnaryOp::Not => {
                        let ty = expr.ty();
                        if ty.is_bool() {
                            // do nothing
                        } else if ty.is_int() {
                            let zero = Expr::const_(ComptimeVal::Int(0));
                            expr = Expr::binary(BinaryOp::Ne, expr, zero);
                            expr.ty = Some(Type::bool());
                        } else if ty.is_float() {
                            let zero = Expr::const_(ComptimeVal::Float(0.0));
                            expr = Expr::binary(BinaryOp::Ne, expr, zero);
                            expr.ty = Some(Type::bool());
                        } else {
                            panic!("unsupported type for logical not: {:?}", ty);
                        }
                        Type::bool()
                    }
                };

                let mut expr = Expr::unary(op, expr);
                expr.ty = Some(ty);
                expr
            }
        };

        if let Some(ty) = expect {
            if ty.is_float() || ty.is_int() || ty.is_bool() | ty.is_ptr() {
                match ty.kind() {
                    Tk::Bool => expr = Expr::coercion(expr, Type::bool()),
                    Tk::Int => expr = Expr::coercion(expr, Type::int()),
                    Tk::Float => expr = Expr::coercion(expr, Type::float()),
                    // for the parameter, e.g., int[] -> int*
                    Tk::Ptr(_) => expr = Expr::coercion(expr, Type::ptr(ty.clone())),
                    Tk::Array(..) | Tk::Func(..) | Tk::Void => {
                        unreachable!()
                    }
                }
                expr.ty = Some(ty.clone());
            } else if ty != expr.ty() {
                panic!("unsupported type coercion: {:?}", ty);
            }
        }

        expr
    }
}
