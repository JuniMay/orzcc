use std::fmt;

use crate::{
    ir::types::{Type, TypeKind},
    irgen::SymbolTableStack,
};

#[derive(Debug, Clone)]
pub enum ComptimeVal {
    Bool(bool),
    Int(i32),
    Float(f32),
    List(Vec<ComptimeVal>),
    Zeros(Type),
}

impl ComptimeVal {
    pub fn new_bool(val: bool) -> Self { ComptimeVal::Bool(val) }

    pub fn new_int(val: i32) -> Self { ComptimeVal::Int(val) }

    pub fn new_float(val: f32) -> Self { ComptimeVal::Float(val) }

    pub fn new_list(val: Vec<ComptimeVal>) -> Self { ComptimeVal::List(val) }

    pub fn new_zeros(ty: Type) -> Self {
        if ty.is_i1() {
            ComptimeVal::Bool(false)
        } else if ty.is_i32() {
            ComptimeVal::Int(0)
        } else if ty.is_float() {
            ComptimeVal::Float(0.0)
        } else if ty.as_array().is_some() {
            ComptimeVal::Zeros(ty)
        } else {
            panic!("unsupported type")
        }
    }

    pub fn get_type(&self) -> Type {
        match self {
            ComptimeVal::Bool(_) => Type::i1(),
            ComptimeVal::Int(_) => Type::i32_(),
            ComptimeVal::Float(_) => Type::float(),
            ComptimeVal::List(val) => {
                let ty = val[0].get_type();
                Type::array(val.len(), ty)
            }
            ComptimeVal::Zeros(ty) => ty.clone(),
        }
    }

    pub fn logical_and(&self, other: &Self) -> Self {
        match (self, other) {
            (ComptimeVal::Bool(a), ComptimeVal::Bool(b)) => ComptimeVal::Bool(*a && *b),
            _ => panic!("unsupported operation"),
        }
    }

    pub fn logical_or(&self, other: &Self) -> Self {
        match (self, other) {
            (ComptimeVal::Bool(a), ComptimeVal::Bool(b)) => ComptimeVal::Bool(*a || *b),
            _ => panic!("unsupported operation"),
        }
    }
}

impl PartialEq for ComptimeVal {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (ComptimeVal::Bool(a), ComptimeVal::Bool(b)) => a == b,
            (ComptimeVal::Int(a), ComptimeVal::Int(b)) => a == b,
            (ComptimeVal::Float(a), ComptimeVal::Float(b)) => a == b,
            _ => false,
        }
    }
}

impl Eq for ComptimeVal {}

impl PartialOrd for ComptimeVal {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (ComptimeVal::Int(a), ComptimeVal::Int(b)) => a.partial_cmp(b),
            (ComptimeVal::Float(a), ComptimeVal::Float(b)) => a.partial_cmp(b),
            _ => None,
        }
    }
}

impl std::ops::Neg for ComptimeVal {
    type Output = Self;

    fn neg(self) -> Self {
        match self {
            ComptimeVal::Int(a) => ComptimeVal::Int(-a),
            ComptimeVal::Float(a) => ComptimeVal::Float(-a),
            _ => panic!("unsupported operation"),
        }
    }
}

impl std::ops::Not for ComptimeVal {
    type Output = Self;

    fn not(self) -> Self {
        match self {
            ComptimeVal::Bool(a) => ComptimeVal::Bool(!a),
            _ => panic!("unsupported operation"),
        }
    }
}

impl std::ops::Add for ComptimeVal {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match (self, other) {
            (ComptimeVal::Int(a), ComptimeVal::Int(b)) => ComptimeVal::Int(a + b),
            (ComptimeVal::Float(a), ComptimeVal::Float(b)) => ComptimeVal::Float(a + b),
            _ => panic!("unsupported operation"),
        }
    }
}

impl std::ops::Sub for ComptimeVal {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        match (self, other) {
            (ComptimeVal::Int(a), ComptimeVal::Int(b)) => ComptimeVal::Int(a - b),
            (ComptimeVal::Float(a), ComptimeVal::Float(b)) => ComptimeVal::Float(a - b),
            _ => panic!("unsupported operation"),
        }
    }
}

impl std::ops::Mul for ComptimeVal {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        match (self, other) {
            (ComptimeVal::Int(a), ComptimeVal::Int(b)) => ComptimeVal::Int(a * b),
            (ComptimeVal::Float(a), ComptimeVal::Float(b)) => ComptimeVal::Float(a * b),
            _ => panic!("unsupported operation"),
        }
    }
}

impl std::ops::Div for ComptimeVal {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        match (self, other) {
            (ComptimeVal::Int(a), ComptimeVal::Int(b)) => ComptimeVal::Int(a / b),
            (ComptimeVal::Float(a), ComptimeVal::Float(b)) => ComptimeVal::Float(a / b),
            _ => panic!("unsupported operation"),
        }
    }
}

impl std::ops::Rem for ComptimeVal {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        match (self, other) {
            (ComptimeVal::Int(a), ComptimeVal::Int(b)) => ComptimeVal::Int(a % b),
            (ComptimeVal::Float(a), ComptimeVal::Float(b)) => ComptimeVal::Float(a % b),
            _ => panic!("unsupported operation"),
        }
    }
}

pub fn offset2lineno(content: &str, offset: usize) -> usize {
    content[..offset].matches('\n').count() + 1
}

pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.start, self.end)
    }
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.start, self.end)
    }
}

/// CompUnit -> { CompUnitItem }
#[derive(Debug)]
pub struct CompUnit {
    pub item: Vec<CompUnitItem>,
}

/// CompUnitItem -> Decl | FuncDef
#[derive(Debug)]
pub enum CompUnitItem {
    Decl(Decl),
    FuncDef(FuncDef),
}

#[derive(Debug)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

#[derive(Debug)]
pub struct ConstDecl {
    pub ty: Type,
    pub defs: Vec<ConstDef>,
}

#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub shape: Vec<Expr>,
    pub init: Expr,
}

#[derive(Debug)]
pub struct VarDecl {
    pub ty: Type,
    pub defs: Vec<VarDef>,
}

#[derive(Debug)]
pub struct VarDef {
    pub ident: String,
    pub shape: Vec<Expr>,
    pub init: Option<Expr>,
}

#[derive(Debug)]
pub struct FuncDef {
    pub ret_ty: Type,
    pub ident: String,
    pub params: Vec<FuncFParam>,
    pub block: Block,
}

#[derive(Debug)]
pub struct FuncFParam {
    pub ty: Type,
    pub ident: String,
    pub indices: Option<Vec<Expr>>,
}

#[derive(Debug)]
pub struct Block {
    pub blockitem: Vec<BlockItem>,
}

#[derive(Debug)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

#[derive(Debug)]
pub enum Stmt {
    Assign(LVal, Expr),
    ExprStmt(ExprStmt),
    Block(Block),
    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    While(Expr, Box<Stmt>),
    Break,
    Continue,
    Return(Return),
}

#[derive(Debug)]
pub struct Return {
    pub expr: Option<Expr>,
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expr: Option<Expr>,
}

#[derive(Debug)]
pub struct LVal {
    pub ident: String,
    pub indices: Vec<Expr>,
}

/// UnaryOp -> '+' | '−' | '!'
#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

/// FuncRParams -> Expr { ',' Expr }
#[derive(Debug)]
pub struct FuncCall {
    pub ident: String,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy)]
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

#[derive(Debug)]
pub enum ExprKind {
    Const(ComptimeVal),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    FuncCall(FuncCall),
    LVal(LVal),
    InitList(Vec<Expr>),
    /// SysY has type coercion only for int and float
    Coercion(Box<Expr>),
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub ty: Option<Type>,
}

impl Expr {
    pub fn new_const(val: ComptimeVal) -> Self {
        let ty = val.get_type();
        Expr {
            kind: ExprKind::Const(val),
            ty: Some(ty),
        }
    }

    pub fn new_binary(op: BinaryOp, lhs: Box<Expr>, rhs: Box<Expr>) -> Self {
        Expr {
            kind: ExprKind::Binary(op, lhs, rhs),
            ty: None,
        }
    }

    pub fn new_unary(op: UnaryOp, expr: Box<Expr>) -> Self {
        Expr {
            kind: ExprKind::Unary(op, expr),
            ty: None,
        }
    }

    pub fn new_func_call(func_call: FuncCall) -> Self {
        Expr {
            kind: ExprKind::FuncCall(func_call),
            ty: None,
        }
    }

    pub fn new_lval(lval: LVal) -> Self {
        Expr {
            kind: ExprKind::LVal(lval),
            ty: None,
        }
    }

    pub fn new_init_list(init_list: Vec<Expr>) -> Self {
        Expr {
            kind: ExprKind::InitList(init_list),
            ty: None,
        }
    }

    pub fn new_coercion(expr: Box<Expr>, to: Type) -> Self {
        Expr {
            kind: ExprKind::Coercion(expr),
            ty: Some(to),
        }
    }

    pub fn ty(&self) -> Type { self.ty.as_ref().unwrap().clone() }
}

impl Expr {
    pub fn canonialize_init_list(&mut self, ty: Type, shape: &[usize]) { todo!() }

    pub fn type_check(self, symtable: &SymbolTableStack) -> Self {
        if self.ty.is_some() {
            return self;
        }
        match self.kind {
            ExprKind::Const(_) => {
                unreachable!()
            }
            ExprKind::Binary(op, lhs, rhs) => {
                let mut lhs = Box::new(lhs.type_check(symtable));
                let mut rhs = Box::new(rhs.type_check(symtable));

                // check coercion
                // i1 -> i32 -> float
                let lhs_ty = lhs.ty();
                let rhs_ty = rhs.ty();

                match (lhs_ty.kind(), rhs_ty.kind()) {
                    (TypeKind::Int(1), TypeKind::Int(32)) => {
                        lhs = Box::new(Expr::new_coercion(lhs, Type::i32_()));
                    }
                    (TypeKind::Int(1), TypeKind::Float) => {
                        lhs = Box::new(Expr::new_coercion(lhs, Type::float()));
                    }
                    (TypeKind::Int(32), TypeKind::Int(1)) => {
                        rhs = Box::new(Expr::new_coercion(rhs, Type::i32_()));
                    }
                    (TypeKind::Int(32), TypeKind::Float) => {
                        lhs = Box::new(Expr::new_coercion(lhs, Type::float()));
                    }
                    (TypeKind::Float, TypeKind::Int(1)) => {
                        rhs = Box::new(Expr::new_coercion(rhs, Type::i32_()));
                    }
                    (TypeKind::Float, TypeKind::Int(32)) => {
                        rhs = Box::new(Expr::new_coercion(rhs, Type::i32_()));
                    }
                    _ => unimplemented!(),
                }

                let mut expr = Expr::new_binary(op, lhs, rhs);

                match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod => {
                        expr.ty = Some(lhs_ty);
                    }
                    BinaryOp::Lt
                    | BinaryOp::Gt
                    | BinaryOp::Le
                    | BinaryOp::Ge
                    | BinaryOp::Eq
                    | BinaryOp::Ne => {
                        expr.ty = Some(Type::i1());
                    }
                    BinaryOp::LogicalAnd | BinaryOp::LogicalOr => {
                        expr.ty = Some(Type::i1());
                    }
                }

                expr
            }
            ExprKind::Coercion(_) => {
                unreachable!()
            }
            ExprKind::FuncCall(call) => {
                let callee = call.ident;
                let args = call.args.into_iter().map(|arg| arg.type_check(symtable));

                let entry = symtable.lookup(&callee).unwrap();
                let (param_tys, ret_ty) = entry.ty.clone().as_function().unwrap();

                // check coercion of arguments
                let args: Vec<Expr> = args
                    .zip(param_tys.into_iter())
                    .map(|(arg, ty)| {
                        // i1 -> i32 -> float
                        let arg_ty = arg.ty();
                        match (arg_ty.kind(), ty.kind()) {
                            (TypeKind::Int(1), TypeKind::Int(32)) => {
                                Expr::new_coercion(Box::new(arg), Type::i32_())
                            }
                            (TypeKind::Int(1), TypeKind::Float) => {
                                Expr::new_coercion(Box::new(arg), Type::float())
                            }
                            (TypeKind::Int(32), TypeKind::Int(1)) => {
                                Expr::new_coercion(Box::new(arg), Type::i32_())
                            }
                            (TypeKind::Int(32), TypeKind::Float) => {
                                Expr::new_coercion(Box::new(arg), Type::float())
                            }
                            (TypeKind::Float, TypeKind::Int(1)) => {
                                Expr::new_coercion(Box::new(arg), Type::i32_())
                            }
                            (TypeKind::Float, TypeKind::Int(32)) => {
                                Expr::new_coercion(Box::new(arg), Type::i32_())
                            }
                            _ => panic!("unsupported type coercion"),
                        }
                    })
                    .collect();

                let mut expr = Expr::new_func_call(FuncCall {
                    ident: callee,
                    args,
                    span: call.span,
                });

                expr.ty = Some(ret_ty);
                expr
            }
            ExprKind::InitList(_) => {
                // for initialization list, the elements and types should be handled separately
                self
            }
            ExprKind::LVal(lval) => {
                let ident = lval.ident;
                let entry = symtable.lookup(&ident).unwrap();

                // indices coercion to i32
                let indices: Vec<Expr> = lval
                    .indices
                    .into_iter()
                    .map(|index| index.type_check(symtable))
                    .map(|index| {
                        if index.ty().is_i1() {
                            Expr::new_coercion(Box::new(index), Type::i32_())
                        } else {
                            index
                        }
                    })
                    .collect();

                let mut ty = entry.ty.clone();
                if !indices.is_empty() {
                    // peel off the array type
                    for _ in 0..indices.len() {
                        ty = ty.as_array().unwrap().1.clone();
                    }
                }
                let mut expr = Expr::new_lval(LVal { ident, indices });
                expr.ty = Some(ty);
                expr
            }
            ExprKind::Unary(op, expr) => {
                let expr = Box::new(expr.type_check(symtable));
                let ty = match op {
                    UnaryOp::Neg => {
                        let ty = expr.ty();
                        if ty.is_i32() || ty.is_float() {
                            ty
                        } else {
                            panic!("unsupported type for negation");
                        }
                    }
                    UnaryOp::Not => {
                        let ty = expr.ty();
                        if ty.is_i1() {
                            ty
                        } else {
                            panic!("unsupported type for logical not");
                        }
                    }
                };
                let mut expr = Expr::new_unary(op, expr);
                expr.ty = Some(ty);
                expr
            }
        }
    }

    pub fn try_fold(&self, symtable: &SymbolTableStack) -> Option<ComptimeVal> {
        match &self.kind {
            ExprKind::Const(val) => Some(val.clone()),
            ExprKind::Binary(op, lhs, rhs) => {
                let lhs = lhs.try_fold(symtable)?;
                let rhs = rhs.try_fold(symtable)?;

                match op {
                    BinaryOp::Add => Some(lhs + rhs),
                    BinaryOp::Sub => Some(lhs - rhs),
                    BinaryOp::Mul => Some(lhs * rhs),
                    BinaryOp::Div => Some(lhs / rhs),
                    BinaryOp::Mod => Some(lhs % rhs),
                    BinaryOp::Lt => Some(ComptimeVal::Bool(lhs < rhs)),
                    BinaryOp::Gt => Some(ComptimeVal::Bool(lhs > rhs)),
                    BinaryOp::Le => Some(ComptimeVal::Bool(lhs <= rhs)),
                    BinaryOp::Ge => Some(ComptimeVal::Bool(lhs >= rhs)),
                    BinaryOp::Eq => Some(ComptimeVal::Bool(lhs == rhs)),
                    BinaryOp::Ne => Some(ComptimeVal::Bool(lhs != rhs)),
                    BinaryOp::LogicalAnd => Some(lhs.logical_and(&rhs)),
                    BinaryOp::LogicalOr => Some(lhs.logical_or(&rhs)),
                }
            }
            ExprKind::Unary(op, expr) => {
                let expr = expr.try_fold(symtable)?;

                match op {
                    UnaryOp::Neg => Some(-expr),
                    UnaryOp::Not => Some(!expr),
                }
            }
            ExprKind::FuncCall(_) => {
                // function call cannot be folded
                None
            }
            ExprKind::LVal(lval) => {
                let entry = symtable.lookup(&lval.ident).unwrap();
                let val = entry.comptime_val.as_ref()?;
                let mut indices = Vec::new();
                for index in &lval.indices {
                    let index = index.try_fold(symtable)?;
                    let index = match index {
                        ComptimeVal::Int(index) => index,
                        ComptimeVal::Float(index) => index as i32,
                        _ => return None,
                    };
                    indices.push(index);
                }
                let val = match val {
                    ComptimeVal::List(list) => {
                        let mut val = val.clone();
                        for index in indices {
                            val = list[index as usize].clone();
                        }
                        val
                    }
                    _ => val.clone(),
                };
                Some(val)
            }
            ExprKind::InitList(_) => {
                // initialization list cannot be folded
                None
            }
            ExprKind::Coercion(expr) => {
                let expr = expr.try_fold(symtable)?;
                match self.ty.as_ref().unwrap().kind() {
                    TypeKind::Int(1) => {
                        let expr = match expr {
                            ComptimeVal::Bool(val) => val as i32,
                            _ => panic!("unsupported type coercion"),
                        };
                        Some(ComptimeVal::Int(expr))
                    }
                    TypeKind::Int(32) => {
                        let expr = match expr {
                            ComptimeVal::Int(val) => val,
                            ComptimeVal::Float(val) => val as i32,
                            _ => panic!("unsupported type coercion"),
                        };
                        Some(ComptimeVal::Int(expr))
                    }
                    TypeKind::Float => {
                        let expr = match expr {
                            ComptimeVal::Bool(val) => val as i32 as f32,
                            ComptimeVal::Int(val) => val as f32,
                            ComptimeVal::Float(val) => val,
                            _ => panic!("unsupported type coercion"),
                        };
                        Some(ComptimeVal::Float(expr))
                    }
                    _ => unimplemented!(),
                }
            }
        }
    }
}
