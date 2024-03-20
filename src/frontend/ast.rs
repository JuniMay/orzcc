
// 编译单元 CompUnit → { CompUnitItem }
pub struct CompUnit {
    pub item: Vec<CompUnitItem>,
}
// 编译单元项 CompUnitItem → Decl | FuncDef
pub enum CompUnitItem {
    Decl(Decl),
    FuncDef(FuncDef),
}
// 声明 Decl → ConstDecl | VarDecl
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}
// 常量声明 ConstDecl → 'const' BasicType ConstDef { ',' ConstDef } ';'
pub struct ConstDecl {
    pub basictype: BasicType,
    pub constdef: Vec<ConstDef>,
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
// 变量声明 VarDecl → BasicType VarDef { ',' VarDef } ';'
pub struct VarDecl {
    pub basictype: BasicType,
    pub vardef: Vec<VarDef>,
}
// 变量定义 VarDef → Ident { '[' ConstExp ']' } | Ident { '[' ConstExp ']' } '=' InitVal
pub struct VarDef {
    pub ident: String,
    pub constexp: Vec<ConstExp>,
    pub initval: Option<InitVal>,
}
// 变量初值 InitVal → Exp | '{' [ InitVal { ',' InitVal } ] '}'
pub enum InitVal {
    Exp(Exp),
    InitVal(Vec<InitVal>),
}
// 函数定义 FuncDef → BasicType Ident '(' [FuncFParams] ')' Block
pub struct FuncDef {
    pub basictype: BasicType,
    pub ident: String,
    pub funcfparams: FuncFParams,
    pub block: Block,
}
// 函数类型 BasicType → 'void' | 'int' | 'float'
pub enum BasicType {
    Void,
    Int,
    Float,
}
// 函数形参表 FuncFParams → FuncFParam { ',' FuncFParam }
pub struct FuncFParams {
    pub funcfparam: Vec<FuncFParam>,
}
// 函数形参 FuncFParam → BasicType Ident ['[' ']' { '[' Exp ']' }]
pub struct FuncFParam {
    pub basictype: BasicType,
    pub ident: String,
    pub exp: Option<Vec<Exp>>,
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
    ExpSt(ExpSt),
    Block(Block),
    If(Cond, Box<Stmt>, Option<Box<Stmt>>),
    While(Cond, Box<Stmt>),
    Break,
    Continue,
    Return(Return),
}

pub struct Return {
    pub exp: Option<Exp>,
  }  

// Attension：there is a diff between the Exp and the ExpSt 
pub struct ExpSt {
    pub exp: Option<Exp>,
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
    pub exp: Vec<Exp>,
}

// 基本表达式 PrimaryExp → '(' Exp ')' | LVal | Number
pub enum PrimaryExp {
    Exp(Box<Exp>),
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
    FuncCall(FuncCall),
    UnaryOp(UnaryOp, Box<UnaryExp>),
}

// 单目运算符 UnaryOp → '+' | '−' | '!' 注：'!'仅出现在仅出现在条件表达式中条件表达式中，其中 '+' 可以不考虑
pub enum UnaryOp {
    Neg,
    Not,
}

// 函数实参表 FuncRParams → Exp { ',' Exp }
pub struct FuncCall {
    pub ident: String,   
    pub exp: Vec<Exp>,
}

// 乘除模表达式 MulExp → UnaryExp | MulExp ('*' | '/' | '%') UnaryExp
pub enum MulExp {
    UnaryExp(UnaryExp),
    MulUExp(Box<MulExp>, MulOp, UnaryExp),
}

// 乘除模运算符 MulOp → '*' | '/' | '%'
pub enum MulOp {
    Mul,
    Div,
    Mod,
}

// 加减表达式 AddExp → MulExp | AddExp ('+' | '−') MulExp
pub enum AddExp {
    MulExp(MulExp),
    AddMExp(Box<AddExp>, AddOp, MulExp),
}

// 加减运算符 AddOp → '+' | '−'
pub enum AddOp {
    Add,
    Sub,
}

// 关系表达式 RelExp → AddExp | RelExp ('<' | '>' | '<=' | '>=') AddExp
pub enum RelExp {
    AddExp(AddExp),
    RelAExp(Box<RelExp>, RelOp, AddExp),
}

// 关系运算符 RelOp → '<' | '>' | '<=' | '>='
pub enum RelOp {
    Lt,
    Gt,
    Le,
    Ge,
}

// 相等性表达式 EqExp → RelExp | EqExp ('==' | '!=') RelExp
pub enum EqExp {
    RelExp(RelExp),
    EqRExp(Box<EqExp>, EqOp, RelExp),
}

// 相等性运算符 EqOp → '==' | '!='
pub enum EqOp {
    Eq,
    Ne,
}

// 逻辑与表达式 LAndExp → EqExp | LAndExp '&&' EqExp
pub enum LAndExp {
    EqExp(EqExp),
    LAndEExp(Box<LAndExp>, EqExp),
}

// 逻辑或表达式 LOrExp → LAndExp | LOrExp '||' LAndExp
pub enum LOrExp {
    LAndExp(LAndExp),
    LOrLExp(Box<LOrExp>, LAndExp),
}

// 常量表达式 ConstExp → AddExp
pub struct ConstExp {
    pub addexp: AddExp,
}