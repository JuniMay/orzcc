use std::fmt;

pub fn off2lineno(content: &str, offset: usize) -> usize {
    content[..offset].matches('\n').count() + 1
}

pub struct SourcePos {
    pub start: usize,
    pub end: usize,
}

impl fmt::Display for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.start, self.end)
    }
}

impl fmt::Debug for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.start, self.end)
    }
}

// CompUnit -> { CompUnitItem }
#[derive(Debug)]
pub struct CompUnit {
    pub item: Vec<CompUnitItem>,
}

// CompUnitItem -> Decl | FuncDef
#[derive(Debug)]
pub enum CompUnitItem {
    Decl(Decl),
    FuncDef(FuncDef),
}

// Decl -> ConstDecl | VarDecl
#[derive(Debug)]
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}

// ConstDecl -> 'const' BasicType ConstDef { ',' ConstDef } ';'
#[derive(Debug)]
pub struct ConstDecl {
    pub basictype: BasicType,
    pub constdef: Vec<ConstDef>,
}

// ConstDef -> Ident { '[' ConstExp ']' } '=' ConstInitVal
#[derive(Debug)]
pub struct ConstDef {
    pub ident: String,
    pub constexp: Vec<ConstExp>,
    pub constinitval: ConstInitVal,
}

// ConstInitVal -> ConstExp | '{' [ ConstInitVal { ',' ConstInitVal } ] '}'
#[derive(Debug)]
pub enum ConstInitVal {
    ConstExp(ConstExp),
    ConstInitVal(Vec<ConstInitVal>),
}

// VarDecl -> BasicType VarDef { ',' VarDef } ';'
#[derive(Debug)]
pub struct VarDecl {
    pub basictype: BasicType,
    pub vardef: Vec<VarDef>,
}

// VarDef -> Ident { '[' ConstExp ']' } | Ident { '[' ConstExp ']' } '=' InitVal
#[derive(Debug)]
pub struct VarDef {
    pub ident: String,
    pub constexp: Vec<ConstExp>,
    pub initval: Option<InitVal>,
}

// InitVal -> Exp | '{' [ InitVal { ',' InitVal } ] '}'
#[derive(Debug)]
pub enum InitVal {
    Exp(Exp),
    InitVal(Vec<InitVal>),
}

// FuncDef -> BasicType Ident '(' [FuncFParams] ')' Block
#[derive(Debug)]
pub struct FuncDef {
    pub basictype: BasicType,
    pub ident: String,
    pub funcfparams: FuncFParams,
    pub block: Block,
}

// BasicType -> 'void' | 'int' | 'float'
#[derive(Debug)]
pub enum BasicType {
    Void,
    Int,
    Float,
}

// FuncFParams -> FuncFParam { ',' FuncFParam }
#[derive(Debug)]
pub struct FuncFParams {
    pub funcfparam: Vec<FuncFParam>,
}

// FuncFParam -> BasicType Ident ['[' ']' { '[' Exp ']' }]
#[derive(Debug)]
pub struct FuncFParam {
    pub basictype: BasicType,
    pub ident: String,
    pub exp: Option<Vec<Exp>>,
}

// Block -> '{' { BlockItem } '}'
#[derive(Debug)]
pub struct Block {
    pub blockitem: Vec<BlockItem>,
}

// BlockItem -> Decl | Stmt
#[derive(Debug)]
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}

// Stmt -> LVal '=' Exp ';' | [Exp] ';' | Block
//       | 'if' '( Cond ')' Stmt [ 'else' Stmt ]
//       | 'while' '(' Cond ')' Stmt
//       | 'break' ';' | 'continue' ';'
//       | 'return' [Exp] ';'
#[derive(Debug)]
pub enum Stmt {
    Assign(LVal, Exp),
    ExpStmt(ExpStmt),
    Block(Block),
    If(Cond, Box<Stmt>, Option<Box<Stmt>>),
    While(Cond, Box<Stmt>),
    Break,
    Continue,
    Return(Return),
}

#[derive(Debug)]
pub struct Return {
    pub exp: Option<Exp>,
}

/// ExpStmt -> Exp ';'
#[derive(Debug)]
pub struct ExpStmt {
    pub exp: Option<Exp>,
}

// Exp -> AddExp
#[derive(Debug)]
pub struct Exp {
    pub addexp: AddExp,
}

// Cond -> LOrExp
#[derive(Debug)]
pub struct Cond {
    pub lorexp: LOrExp,
}

// LVal -> Ident {'[' Exp ']'}
#[derive(Debug)]
pub struct LVal {
    pub ident: String,
    pub exp: Vec<Exp>,
}

// PrimaryExp -> '(' Exp ')' | LVal | Number
#[derive(Debug)]
pub enum PrimaryExp {
    Exp(Box<Exp>),
    LVal(LVal),
    Number(Number),
}

// Number -> IntConst | floatConst
#[derive(Debug)]
pub enum Number {
    IntConst(i32),
    FloatConst(f32),
}

// UnaryExp -> PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp
#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    FuncCall(FuncCall),
    UnaryOp(UnaryOp, Box<UnaryExp>),
}

// UnaryOp -> '+' | '−' | '!'
#[derive(Debug)]
pub enum UnaryOp {
    Neg,
    Not,
}

// FuncRParams -> Exp { ',' Exp }
#[derive(Debug)]
pub struct FuncCall {
    pub ident: String,
    pub exp: Vec<Exp>,
    pub pos: SourcePos,
}

// MulExp -> UnaryExp | MulExp ('*' | '/' | '%') UnaryExp
#[derive(Debug)]
pub enum MulExp {
    UnaryExp(UnaryExp),
    MulUExp(Box<MulExp>, MulOp, UnaryExp),
}

// MulOp -> '*' | '/' | '%'
#[derive(Debug)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
}

// AddExp -> MulExp | AddExp ('+' | '−') MulExp
#[derive(Debug)]
pub enum AddExp {
    MulExp(MulExp),
    AddMExp(Box<AddExp>, AddOp, MulExp),
}

// AddOp -> '+' | '−'
#[derive(Debug)]
pub enum AddOp {
    Add,
    Sub,
}

// RelExp -> AddExp | RelExp ('<' | '>' | '<=' | '>=') AddExp
#[derive(Debug)]
pub enum RelExp {
    AddExp(AddExp),
    RelAExp(Box<RelExp>, RelOp, AddExp),
}

// RelOp -> '<' | '>' | '<=' | '>='
#[derive(Debug)]
pub enum RelOp {
    Lt,
    Gt,
    Le,
    Ge,
}

// EqExp -> RelExp | EqExp ('==' | '!=') RelExp
#[derive(Debug)]
pub enum EqExp {
    RelExp(RelExp),
    EqRExp(Box<EqExp>, EqOp, RelExp),
}

// EqOp -> '==' | '!='
#[derive(Debug)]
pub enum EqOp {
    Eq,
    Ne,
}

// LAndExp -> EqExp | LAndExp '&&' EqExp
#[derive(Debug)]
pub enum LAndExp {
    EqExp(EqExp),
    LAndEExp(Box<LAndExp>, EqExp),
}

// LOrExp -> LAndExp | LOrExp '||' LAndExp
#[derive(Debug)]
pub enum LOrExp {
    LAndExp(LAndExp),
    LOrLExp(Box<LOrExp>, LAndExp),
}

// ConstExp -> AddExp
#[derive(Debug)]
pub struct ConstExp {
    pub addexp: AddExp,
}
