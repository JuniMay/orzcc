use std::fmt;

pub fn off2lineno(content: &str, offset: usize) -> usize {
    content[..offset].matches('\n').count() + 1
}

pub struct SourcePos {
    pub start: usize,
    pub end: usize,
}
impl fmt::Debug for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {})", self.start, self.end)
    }
}
// 编译单元 CompUnit → { CompUnitItem }
pub struct CompUnit {
    pub item: Vec<CompUnitItem>,
}
impl fmt::Debug for CompUnit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "CompUnit {{")?;
        writeln!(f)?;
        for item in &self.item {
            writeln!(f, "  {:?}", item)?;
        }
        writeln!(f, "}}")
    }
}
// 编译单元项 CompUnitItem → Decl | FuncDef
pub enum CompUnitItem {
    Decl(Decl),
    FuncDef(FuncDef),
}
impl fmt::Debug for CompUnitItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompUnitItem::Decl(decl) => write!(f, "     Decl: {:?}", decl),
            CompUnitItem::FuncDef(funcdef) => write!(f, "     FuncDef: {:?}", funcdef),
        }
    }
}
// 声明 Decl → ConstDecl | VarDecl
pub enum Decl {
    ConstDecl(ConstDecl),
    VarDecl(VarDecl),
}
impl fmt::Debug for Decl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Decl::ConstDecl(constdecl) => write!(f, "ConstDecl: {:?}", constdecl),
            Decl::VarDecl(vardecl) => write!(f, "VarDecl: {:?}", vardecl),
        }
    }
}
// 常量声明 ConstDecl → 'const' BasicType ConstDef { ',' ConstDef } ';'
pub struct ConstDecl {
    pub basictype: BasicType,
    pub constdef: Vec<ConstDef>,
}
impl fmt::Debug for ConstDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Type: {:?}", self.basictype)?;
        for def in &self.constdef {
            writeln!(f, "{:?} ", def)?;
        }
        Ok(())
    }
}
// 常数定义 ConstDef → Ident { '[' ConstExp ']' } '=' ConstInitVal
pub struct ConstDef {
    pub ident: String,
    pub constexp: Vec<ConstExp>,
    pub constinitval: ConstInitVal,
}
impl fmt::Debug for ConstDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "     Id: {:?}", self.ident)?;
        for exp in &self.constexp {
            write!(f, "[{:?}]", exp)?;
        }
        write!(f, " = {:?}", self.constinitval)
    }
}
// 常量初值 ConstInitVal → ConstExp | '{' [ ConstInitVal { ',' ConstInitVal } ]
// '}'
pub enum ConstInitVal {
    ConstExp(ConstExp),
    ConstInitVal(Vec<ConstInitVal>),
}
impl fmt::Debug for ConstInitVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConstInitVal::ConstExp(exp) => write!(f, "{:?}", exp),
            ConstInitVal::ConstInitVal(initval) => {
                write!(f, "{{")?;
                for val in initval {
                    write!(f, "{:?}, ", val)?;
                }
                write!(f, "}}")
            }
        }
    }
}
// 变量声明 VarDecl → BasicType VarDef { ',' VarDef } ';'
pub struct VarDecl {
    pub basictype: BasicType,
    pub vardef: Vec<VarDef>,
}
impl fmt::Debug for VarDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let defs = self
            .vardef
            .iter()
            .map(|def| format!("{:?}", def))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "Type: {:?}, VarDef: {}", self.basictype, defs)
    }
}
// 变量定义 VarDef → Ident { '[' ConstExp ']' } | Ident { '[' ConstExp ']' } '='
// InitVal
pub struct VarDef {
    pub ident: String,
    pub constexp: Vec<ConstExp>,
    pub initval: Option<InitVal>,
}
impl fmt::Debug for VarDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Id: {:?}", self.ident)?;
        for exp in &self.constexp {
            write!(f, "{:?}", exp)?;
        }
        write!(f, ", InitVal: {:?}", self.initval)
    }
}
// 变量初值 InitVal → Exp | '{' [ InitVal { ',' InitVal } ] '}'
pub enum InitVal {
    Exp(Exp),
    InitVal(Vec<InitVal>),
}
impl fmt::Debug for InitVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InitVal::Exp(exp) => write!(f, "Exp({:?})", exp),
            InitVal::InitVal(initval) => {
                write!(f, "{{")?;
                for val in initval {
                    write!(f, "{:?}, ", val)?;
                }
                write!(f, "}}")
            }
        }
    }
}
// 函数定义 FuncDef → BasicType Ident '(' [FuncFParams] ')' Block
pub struct FuncDef {
    pub basictype: BasicType,
    pub ident: String,
    pub funcfparams: FuncFParams,
    pub block: Block,
}
impl fmt::Debug for FuncDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Type: {:?}, FuncName: {:?}\n{:?}\n{:?}",
            self.basictype, self.ident, self.funcfparams, self.block
        )
    }
}
// 函数类型 BasicType → 'void' | 'int' | 'float'
pub enum BasicType {
    Void,
    Int,
    Float,
}
impl fmt::Debug for BasicType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BasicType::Void => write!(f, "void"),
            BasicType::Int => write!(f, "int"),
            BasicType::Float => write!(f, "float"),
        }
    }
}
// 函数形参表 FuncFParams → FuncFParam { ',' FuncFParam }
pub struct FuncFParams {
    pub funcfparam: Vec<FuncFParam>,
}
impl fmt::Debug for FuncFParams {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let params = self
            .funcfparam
            .iter()
            .map(|param| format!("{:?}", param))
            .collect::<Vec<_>>()
            .join(", ");
        write!(f, "     FuncFParams({})", params)
    }
}
// 函数形参 FuncFParam → BasicType Ident ['[' ']' { '[' Exp ']' }]
pub struct FuncFParam {
    pub basictype: BasicType,
    pub ident: String,
    pub exp: Option<Vec<Exp>>,
}
impl fmt::Debug for FuncFParam {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "FuncFParam(basictype: {:?}, ident: {:?}, exp: ",
            self.basictype, self.ident
        )?;
        match &self.exp {
            Some(exp) => {
                for e in exp {
                    write!(f, "{:?}, ", e)?;
                }
                write!(f, ")")
            }
            None => write!(f, "None"),
        }
    }
}
// 语句块 Block → '{' { BlockItem } '}'
pub struct Block {
    pub blockitem: Vec<BlockItem>,
}
impl fmt::Debug for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "      Block {{\n")?;
        for item in &self.blockitem {
            write!(f, "         {:?}\n", item)?;
        }
        write!(f, "      }}")
    }
}
// 语句块项 BlockItem → Decl | Stmt
pub enum BlockItem {
    Decl(Decl),
    Stmt(Stmt),
}
impl fmt::Debug for BlockItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BlockItem::Decl(decl) => write!(f, "Decl({:?})", decl),
            BlockItem::Stmt(stmt) => write!(f, "Stmt({:?})", stmt),
        }
    }
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
impl fmt::Debug for Stmt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stmt::Assign(lval, exp) => write!(f, "Assign({:?}, {:?})", lval, exp),
            Stmt::ExpSt(expst) => write!(f, "ExpSt({:?})", expst),
            Stmt::Block(block) => write!(f, "Block({:?})", block),
            Stmt::If(cond, stmt1, stmt2) => {
                write!(f, "If({:?}) {:?}", cond, stmt1)?;
                match stmt2 {
                    Some(stmt) => write!(f, "else {:?}", stmt),
                    None => write!(f, ""),
                }
            }
            Stmt::While(cond, stmt) => write!(f, "While({:?}){:?}", cond, stmt),
            Stmt::Break => write!(f, "Break"),
            Stmt::Continue => write!(f, "Continue"),
            Stmt::Return(ret) => write!(f, "{:?}", ret),
        }
    }
}
pub struct Return {
    pub exp: Option<Exp>,
}
impl fmt::Debug for Return {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.exp {
            Some(exp) => write!(f, "Return({:?})", exp),
            None => write!(f, "Return"),
        }
    }
}
// Attension：there is a diff between the Exp and the ExpSt
pub struct ExpSt {
    pub exp: Option<Exp>,
}
impl fmt::Debug for ExpSt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.exp {
            Some(exp) => write!(f, "{:?}", exp),
            None => write!(f, "None"),
        }
    }
}

// 表达式 Exp → AddExp 注：SysY表达式是 int/float型表达式
pub struct Exp {
    pub addexp: AddExp,
}
impl fmt::Debug for Exp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{:?}", self.addexp) }
}
// 条件表达式 Cond → LOrExp
pub struct Cond {
    pub lorexp: LOrExp,
}
impl fmt::Debug for Cond {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{:?}", self.lorexp) }
}
// 左值表达式 LVal → Ident {'[' Exp ']'}
pub struct LVal {
    pub ident: String,
    pub exp: Vec<Exp>,
}
impl fmt::Debug for LVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self.ident)?;
        for exp in &self.exp {
            write!(f, "[{:?}]", exp)?;
        }
        write!(f, "")
    }
}
// 基本表达式 PrimaryExp → '(' Exp ')' | LVal | Number
pub enum PrimaryExp {
    Exp(Box<Exp>),
    LVal(LVal),
    Number(Number),
}
impl fmt::Debug for PrimaryExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PrimaryExp::Exp(exp) => write!(f, "Exp({:?})", exp),
            PrimaryExp::LVal(lval) => write!(f, "LVal({:?})", lval),
            PrimaryExp::Number(number) => write!(f, "{:?}", number),
        }
    }
}
// 数值 Number → IntConst | floatConst
pub enum Number {
    IntConst(i32),
    FloatConst(f32),
}
impl fmt::Debug for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Number::IntConst(i) => write!(f, "Int({})", i),
            Number::FloatConst(fl) => write!(f, "Float({})", fl),
        }
    }
}
// 一元表达式 UnaryExp → PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp
// UnaryExp
pub enum UnaryExp {
    PrimaryExp(PrimaryExp),
    FuncCall(FuncCall),
    UnaryOp(UnaryOp, Box<UnaryExp>),
}
impl fmt::Debug for UnaryExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryExp::PrimaryExp(exp) => write!(f, "{:?}", exp),
            UnaryExp::FuncCall(func) => write!(f, "{:?}", func),
            UnaryExp::UnaryOp(op, exp) => write!(f, "{:?}{:?}", op, exp),
        }
    }
}
// 单目运算符 UnaryOp → '+' | '−' | '!'
// 注：'!'仅出现在仅出现在条件表达式中条件表达式中，其中 '+' 可以不考虑
pub enum UnaryOp {
    Neg,
    Not,
}
impl fmt::Debug for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-"),
            UnaryOp::Not => write!(f, "!"),
        }
    }
}
// 函数实参表 FuncRParams → Exp { ',' Exp }
pub struct FuncCall {
    pub ident: String,
    pub exp: Vec<Exp>,
    pub pos: SourcePos,
}
impl fmt::Debug for FuncCall {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let exps = self
            .exp
            .iter()
            .map(|exp| format!("{:?}", exp))
            .collect::<Vec<_>>()
            .join(", ");
        write!(
            f,
            "FuncCall(FuncName: {:?}, Exp: {}, Pos: {:?})",
            self.ident, exps, self.pos
        )
    }
}
// 乘除模表达式 MulExp → UnaryExp | MulExp ('*' | '/' | '%') UnaryExp
pub enum MulExp {
    UnaryExp(UnaryExp),
    MulUExp(Box<MulExp>, MulOp, UnaryExp),
}
impl fmt::Debug for MulExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MulExp::UnaryExp(exp) => write!(f, "{:?}", exp),
            MulExp::MulUExp(exp1, op, exp2) => write!(f, "{:?}{:?}{:?}", exp1, op, exp2),
        }
    }
}
// 乘除模运算符 MulOp → '*' | '/' | '%'
pub enum MulOp {
    Mul,
    Div,
    Mod,
}
impl fmt::Debug for MulOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            MulOp::Mul => write!(f, "*"),
            MulOp::Div => write!(f, "/"),
            MulOp::Mod => write!(f, "%"),
        }
    }
}
// 加减表达式 AddExp → MulExp | AddExp ('+' | '−') MulExp
pub enum AddExp {
    MulExp(MulExp),
    AddMExp(Box<AddExp>, AddOp, MulExp),
}
impl fmt::Debug for AddExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AddExp::MulExp(exp) => write!(f, "{:?}", exp),
            AddExp::AddMExp(exp1, op, exp2) => write!(f, "{:?}{:?}{:?}", exp1, op, exp2),
        }
    }
}
// 加减运算符 AddOp → '+' | '−'
pub enum AddOp {
    Add,
    Sub,
}
impl fmt::Debug for AddOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AddOp::Add => write!(f, "+"),
            AddOp::Sub => write!(f, "-"),
        }
    }
}
// 关系表达式 RelExp → AddExp | RelExp ('<' | '>' | '<=' | '>=') AddExp
pub enum RelExp {
    AddExp(AddExp),
    RelAExp(Box<RelExp>, RelOp, AddExp),
}
impl fmt::Debug for RelExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RelExp::AddExp(exp) => write!(f, "{:?}", exp),
            RelExp::RelAExp(exp1, op, exp2) => write!(f, "{:?}{:?}{:?}", exp1, op, exp2),
        }
    }
}
// 关系运算符 RelOp → '<' | '>' | '<=' | '>='
pub enum RelOp {
    Lt,
    Gt,
    Le,
    Ge,
}
impl fmt::Debug for RelOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RelOp::Lt => write!(f, "<"),
            RelOp::Gt => write!(f, ">"),
            RelOp::Le => write!(f, "<="),
            RelOp::Ge => write!(f, ">="),
        }
    }
}
// 相等性表达式 EqExp → RelExp | EqExp ('==' | '!=') RelExp
pub enum EqExp {
    RelExp(RelExp),
    EqRExp(Box<EqExp>, EqOp, RelExp),
}
impl fmt::Debug for EqExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EqExp::RelExp(exp) => write!(f, "{:?}", exp),
            EqExp::EqRExp(exp1, op, exp2) => write!(f, "{:?}{:?}{:?}", exp1, op, exp2),
        }
    }
}
// 相等性运算符 EqOp → '==' | '!='
pub enum EqOp {
    Eq,
    Ne,
}
impl fmt::Debug for EqOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EqOp::Eq => write!(f, "=="),
            EqOp::Ne => write!(f, "!="),
        }
    }
}
// 逻辑与表达式 LAndExp → EqExp | LAndExp '&&' EqExp
pub enum LAndExp {
    EqExp(EqExp),
    LAndEExp(Box<LAndExp>, EqExp),
}
impl fmt::Debug for LAndExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LAndExp::EqExp(exp) => write!(f, "{:?}", exp),
            LAndExp::LAndEExp(exp1, exp2) => write!(f, "{:?}&&{:?}", exp1, exp2),
        }
    }
}
// 逻辑或表达式 LOrExp → LAndExp | LOrExp '||' LAndExp
pub enum LOrExp {
    LAndExp(LAndExp),
    LOrLExp(Box<LOrExp>, LAndExp),
}
impl fmt::Debug for LOrExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LOrExp::LAndExp(exp) => write!(f, "{:?}", exp),
            LOrExp::LOrLExp(exp1, exp2) => write!(f, "{:?}||{:?}", exp1, exp2),
        }
    }
}
// 常量表达式 ConstExp → AddExp
pub struct ConstExp {
    pub addexp: AddExp,
}
impl fmt::Debug for ConstExp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ConstExp: {:?}", self.addexp)
    }
}
