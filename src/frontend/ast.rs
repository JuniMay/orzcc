use std::fmt;

use crate::ir::types::Type;

#[derive(Debug)]
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
    pub ty: BasicType,
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
    pub ty: BasicType,
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
    pub ret_ty: BasicType,
    pub ident: String,
    pub params: Vec<FuncFParam>,
    pub block: Block,
}

#[derive(Debug)]
pub enum BasicType {
    Void,
    Int,
    Float,
}

#[derive(Debug)]
pub struct FuncFParam {
    pub ty: BasicType,
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
    pub exp: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug)]
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
pub enum Expr {
    Const(ComptimeVal),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),
    Unary(UnaryOp, Box<Expr>),
    FuncCall(FuncCall),
    LVal(LVal),
    InitList(Vec<Expr>),
}
