use std::vec;

// 编译单元 CompUnit → [ CompUnit ] ( Decl | FuncDef )
pub enum CompUnit {
    Decl(Decl),
    FuncDef(FuncDef),
    
}
// 声明 Decl → ConstDecl | VarDecl
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}
// 常量声明 ConstDecl → 'const' BType ConstDef { ',' ConstDef } ';'
pub struct ConstDecl {
    pub btype: BType,
    pub constdef: Vec<ConstDef>,
}
// 基本类型 BType → 'int' | 'float'
pub enum BType {
    Int,
    Float,
}
// 常数定义 ConstDef → Ident { '[' ConstExp ']' } '=' ConstInitVal
pub struct ConstDef {
    pub ident: String,
    pub constexp: Vec<ConstExp>,
    pub constinitval: ConstInitVal,
}
// 常量初值 ConstInitVal → ConstExp | '{' [ ConstInitVal { ',' ConstInitVal } ] '}'
pub enum ConstInitVal {
    ConstExp(ConstExp),
    ConstInitVal(Vec<ConstInitVal>),
}
// 变量声明 VarDecl → BType VarDef { ',' VarDef } ';'
pub struct VarDecl {
    pub btype: BType,
    pub vardef: Vec<VarDef>,
}
// 变量定义 VarDef → Ident { '[' ConstExp ']' } | Ident { '[' ConstExp ']' } '=' InitVal
pub struct VarDef {
    pub ident: String,
    pub constexp: Option<ConstExp>,
    pub initval: Option<InitVal>,
}
// 变量初值 InitVal → Exp | '{' [ InitVal { ',' InitVal } ] '}'
pub enum InitVal {
    Exp(Exp),
    InitVal(Vec<InitVal>),
}
// 函数定义 FuncDef → FuncType Ident '(' [FuncFParams] ')' Block
pub struct FuncDef {
    pub functype: FuncType,
    pub ident: String,
    pub funcfparams: Option<FuncFParams>,
    pub block: Block,
}
// 函数类型 FuncType → 'void' | 'int' | 'float'
pub enum FuncType {
    Void,
    Int,
    Float,
}
// 函数形参表 FuncFParams → FuncFParam { ',' FuncFParam }
pub struct FuncFParams {
    pub funcfparam: Vec<FuncFParam>,
}
// 函数形参 FuncFParam → BType Ident ['[' ']' { '[' Exp ']' }]
pub struct FuncFParam {
    pub btype: BType,
    pub ident: String,
    pub exp: Option<Exp>,
}
// 语句块 Block → '{' { BlockItem } '}'
pub struct Block {
    pub blockitem: Vec<BlockItem>,
}
// 语句块项 BlockItem → Decl | Stmt
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}
// 语句 Stmt → LVal '=' Exp ';' | [Exp] ';' | Block
//             | 'if' '( Cond ')' Stmt [ 'else' Stmt ]
//             | 'while' '(' Cond ')' Stmt
//             | 'break' ';' | 'continue' ';'
//             | 'return' [Exp] ';'
pub enum Stmt {
    Assign(LVal, Exp),
    Exp(Exp),
    Block(Block),
    If(Cond, Box<Stmt>, Option<Box<Stmt>>),
    While(Cond, Box<Stmt>),
    Break,
    Continue,
    Return(Option<Exp>),
}
// 表达式 Exp → AddExp 注：SysY表达式是 int/float型表达式
pub struct Exp {
    pub addexp: AddExp,
}
// 条件表达式 Cond → LOrExp
pub struct Cond {
    pub lorexp: LOrExp,
}
// 左值表达式 LVal → Ident {'[' Exp ']'}
pub struct LVal {
    pub ident: String,
    pub exp: Option<Exp>,
}
// 基本表达式 PrimaryExp → '(' Exp ')' | LVal | Number
pub enum PrimaryExp {
    Exp(Exp),
    LVal(LVal),
    Number(Number),
}
// 数值 Number → IntConst | floatConst
pub enum Number {
    IntConst(i32),
    FloatConst(f32),
}
// 一元表达式 UnaryExp → PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    FuncRParams(FuncRParams),
    UnaryOp(UnaryOp, Box<UnaryExp>),
}
// 单目运算符 UnaryOp → '+' | '−' | '!' 注：'!'仅出现在仅出现在条件表达式中条件表达式中
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}
// 函数实参表 FuncRParams → Exp { ',' Exp }
pub struct FuncRParams {
    pub exp: Vec<Exp>,
}
// 乘除模表达式 MulExp → UnaryExp | MulExp ('*' | '/' | '%') UnaryExp
pub enum MulExp {
    UnaryExp(UnaryExp),
    MulExp(MulExp, MulOp, UnaryExp),
}
// 加减表达式 AddExp → MulExp | AddExp ('+' | '−') MulExp
pub enum AddExp {
    MulExp(MulExp),
    AddExp(AddExp, AddOp, MulExp),
}
// 关系表达式 RelExp → AddExp | RelExp ('<' | '>' | '<=' | '>=') AddExp
pub enum RelExp {
    AddExp(AddExp),
    RelExp(RelExp, RelOp, AddExp),
}
// 相等性表达式 EqExp → RelExp | EqExp ('==' | '!=') RelExp
pub enum EqExp {
    RelExp(RelExp),
    EqExp(EqExp, EqOp, RelExp),
}
// 逻辑与表达式 LAndExp → EqExp | LAndExp '&&' EqExp
pub enum LAndExp {
    EqExp(EqExp),
    LAndExp(LAndExp, LAndOp, EqExp),
}
// 逻辑或表达式 LOrExp → LAndExp | LOrExp '||' LAndExp
pub enum LOrExp {
    LAndExp(LAndExp),
    LOrExp(LOrExp, LOrOp, LAndExp),
}
// 常量表达式 ConstExp → AddExp
pub struct ConstExp {
    pub addexp: AddExp,
}