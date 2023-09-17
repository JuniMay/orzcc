use std::fmt;

use crate::ir::entities::{BinaryOp, FCmpCond, ICmpCond, UnaryOp};

#[derive(Clone, Copy)]
pub struct Pos {
    row: u32,
    col: u32,
}

impl Default for Pos {
    fn default() -> Self {
        Pos { row: 1, col: 0 }
    }
}

impl Pos {
    pub fn new(row: u32, col: u32) -> Self {
        Pos { row, col }
    }

    /// Update pos depending on the character
    pub fn update(&mut self, c: char) {
        match c {
            '\n' => {
                self.row += 1;
                self.col = 0;
            }
            _ => self.col += 1,
        }
    }
}

impl fmt::Display for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.col)
    }
}

impl fmt::Debug for Pos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Debug, Clone)]
pub struct Span {
    pub st: Pos,
    pub ed: Pos,
}

impl Span {
    pub fn new(st: Pos, ed: Pos) -> Self {
        Span { st, ed }
    }
}

/// Keywords
#[derive(Debug, Clone)]
pub enum Keyword {
    /// `global`
    Global,
    /// `constant`
    Constant,
    /// `void`
    Void,
    /// Arbitrary int type `ixx`
    Int(usize),
    /// `half`
    Half,
    /// `float`
    Float,
    /// `double`
    Double,
    /// `ptr`
    Ptr,
    /// `label`
    Label,
    /// `fn`
    Fn,
    /// `alloc`
    Alloc,
    /// `load`
    Load,
    /// `store`
    Store,
    /// Br,
    Br,
    /// J
    J,
    /// Ret
    Ret,
    /// Binary opcodes.
    Binary(BinaryOp),
    /// Integer compare opcodes.
    ICmp(ICmpCond),
    /// Float compare opcodes.
    FCmp(FCmpCond),
    /// Unary compare opcodes.
    Unary(UnaryOp),
    /// Declare
    Declare,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Keyword::Global => write!(f, "global"),
            Keyword::Constant => write!(f, "constant"),
            Keyword::Void => write!(f, "void"),
            Keyword::Int(n) => write!(f, "i{}", n),
            Keyword::Half => write!(f, "half"),
            Keyword::Float => write!(f, "float"),
            Keyword::Double => write!(f, "double"),
            Keyword::Ptr => write!(f, "ptr"),
            Keyword::Label => write!(f, "label"),
            Keyword::Fn => write!(f, "fn"),
            Keyword::Alloc => write!(f, "alloc"),
            Keyword::Load => write!(f, "load"),
            Keyword::Store => write!(f, "store"),
            Keyword::Br => write!(f, "br"),
            Keyword::J => write!(f, "j"),
            Keyword::Ret => write!(f, "ret"),
            Keyword::Binary(op) => write!(f, "{}", op),
            Keyword::ICmp(cond) => write!(f, "{}", cond),
            Keyword::FCmp(cond) => write!(f, "{}", cond),
            Keyword::Unary(op) => write!(f, "{}", op),
            Keyword::Declare => write!(f, "declare"),
        }
    }
}

/// Kind of tokens
#[derive(Debug, Clone)]
pub enum TokenKind {
    /// A label.
    /// An identifier starting with `^`
    Label(String),
    /// A vreg, result of an instruction.
    /// An identifier starting with `%`
    VReg(String),
    /// A global symbol.
    /// An identifier starting with `@`.
    Global(String),
    /// A literal value.
    Literal(Vec<u8>),
    /// A keyword.
    Keyword(Keyword),
    /// Other characters.
    Other(String),
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenKind::Label(s) => write!(f, "label: {}", s),
            TokenKind::VReg(s) => write!(f, "vreg: {}", s),
            TokenKind::Global(s) => write!(f, "global: {}", s),
            TokenKind::Literal(bytes) => write!(
                f,
                "literal: 0x{}",
                bytes
                    .iter()
                    .rev()
                    .map(|b| format!("{:02x}", b))
                    .collect::<Vec<_>>()
                    .join("")
            ),
            TokenKind::Keyword(s) => write!(f, "keyword: {}", s),
            TokenKind::Other(s) => write!(f, "other: {}", s),
        }
    }
}

/// A token.
#[derive(Debug, Clone)]
pub struct Token {
    /// Kind of the token.
    pub kind: TokenKind,
    /// Span of the token.
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Clone)]
pub enum Error {
    InvalidIdentifier(String, Span),
    InvalidCharacter(char, Pos),
    InvalidLiteral(String, Span),
}

pub struct Lexer {
    source: Vec<char>,
    curr_pos: Pos,
    curr_idx: usize,
}

impl Lexer {
    pub fn new(source: String) -> Self {
        Self {
            source: source.chars().collect(),
            curr_pos: Pos::default(),
            curr_idx: 0,
        }
    }

    fn next_char(&mut self, f: impl Fn(char) -> bool) -> Option<(char, Pos)> {
        if self.curr_idx >= self.source.len() {
            return None;
        } else {
            let c = self.source[self.curr_idx];
            if f(c) {
                self.curr_idx += 1;
                self.curr_pos.update(c);
                return Some((c, self.curr_pos));
            } else {
                return None;
            }
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut identifier = String::new();
        while let Some((c, _)) = self.next_char(|c| c.is_alphanumeric() || c == '_' || c == '.') {
            identifier.push(c);
        }
        println!("identifier: {}", identifier);
        identifier
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, Error> {
        let mut tokens = Vec::new();
        while let Some((c, pos)) = self.next_char(|_| true) {
            match c {
                '^' => {
                    let label = format!("^{}", self.read_identifier());
                    tokens.push(Token::new(
                        TokenKind::Label(label),
                        Span::new(pos, self.curr_pos),
                    ));
                }
                '%' => {
                    let label = format!("%{}", self.read_identifier());
                    tokens.push(Token::new(
                        TokenKind::VReg(label),
                        Span::new(pos, self.curr_pos),
                    ));
                }
                '@' => {
                    let label = format!("@{}", self.read_identifier());
                    tokens.push(Token::new(
                        TokenKind::Global(label),
                        Span::new(pos, self.curr_pos),
                    ));
                }
                '#' => {
                    // comment
                    while let Some(_) = self.next_char(|c| c != '\n' && c != '\r') {}
                }
                '-' => {
                    if let Some(_) = self.next_char(|c| c == '>') {
                        tokens.push(Token::new(
                            TokenKind::Other("->".into()),
                            Span::new(pos, self.curr_pos),
                        ));
                    }
                }
                '{' | '}' | '(' | ')' | '[' | ']' | ',' | ';' | '=' | ':' => {
                    tokens.push(Token::new(
                        TokenKind::Other(c.into()),
                        Span::new(pos, self.curr_pos),
                    ));
                }
                '\n' | '\r' => {
                    // skip newline
                }
                ' ' | '\t' => {
                    // skip whitespaces
                }
                _ => {
                    if c.is_alphabetic() {
                        // keyword
                        let mut keyword = String::new();
                        keyword.push(c);
                        let st = pos;
                        while let Some((c, _)) =
                            self.next_char(|c| c.is_alphanumeric() || c == '.')
                        {
                            keyword.push(c);
                        }
                        let kind = TokenKind::Keyword(match keyword.as_str() {
                            "global" => Keyword::Global,
                            "constant" => Keyword::Constant,
                            "void" => Keyword::Void,
                            "i8" => Keyword::Int(8),
                            "i16" => Keyword::Int(16),
                            "i32" => Keyword::Int(32),
                            "i64" => Keyword::Int(64),
                            "i128" => Keyword::Int(128),
                            "half" => Keyword::Half,
                            "float" => Keyword::Float,
                            "double" => Keyword::Double,
                            "ptr" => Keyword::Ptr,
                            "label" => Keyword::Label,
                            "fn" => Keyword::Fn,
                            "alloc" => Keyword::Alloc,
                            "load" => Keyword::Load,
                            "store" => Keyword::Store,
                            "add" => Keyword::Binary(BinaryOp::Add),
                            "fadd" => Keyword::Binary(BinaryOp::FAdd),
                            "sub" => Keyword::Binary(BinaryOp::Sub),
                            "fsub" => Keyword::Binary(BinaryOp::FSub),
                            "mul" => Keyword::Binary(BinaryOp::Mul),
                            "fmul" => Keyword::Binary(BinaryOp::FMul),
                            "udiv" => Keyword::Binary(BinaryOp::UDiv),
                            "sdiv" => Keyword::Binary(BinaryOp::SDiv),
                            "fdiv" => Keyword::Binary(BinaryOp::FDiv),
                            "urem" => Keyword::Binary(BinaryOp::URem),
                            "srem" => Keyword::Binary(BinaryOp::SRem),
                            "frem" => Keyword::Binary(BinaryOp::FRem),
                            "and" => Keyword::Binary(BinaryOp::And),
                            "or" => Keyword::Binary(BinaryOp::Or),
                            "xor" => Keyword::Binary(BinaryOp::Xor),
                            "shl" => Keyword::Binary(BinaryOp::Shl),
                            "lshr" => Keyword::Binary(BinaryOp::LShr),
                            "ashr" => Keyword::Binary(BinaryOp::AShr),
                            "fneg" => Keyword::Unary(UnaryOp::FNeg),
                            "icmp.eq" => Keyword::ICmp(ICmpCond::Eq),
                            "icmp.ne" => Keyword::ICmp(ICmpCond::Ne),
                            "icmp.slt" => Keyword::ICmp(ICmpCond::Slt),
                            "icmp.sle" => Keyword::ICmp(ICmpCond::Sle),
                            "fcmp.oeq" => Keyword::FCmp(FCmpCond::OEq),
                            "fcmp.one" => Keyword::FCmp(FCmpCond::ONe),
                            "fcmp.olt" => Keyword::FCmp(FCmpCond::OLt),
                            "fcmp.ole" => Keyword::FCmp(FCmpCond::OLe),
                            "declare" => Keyword::Declare,
                            "br" => Keyword::Br,
                            "j" => Keyword::J,
                            "ret" => Keyword::Ret,
                            _ => {
                                return Err(Error::InvalidIdentifier(
                                    keyword,
                                    Span::new(st, self.curr_pos),
                                ))
                            }
                        });
                        tokens.push(Token::new(kind, Span::new(st, self.curr_pos)))
                    } else if c.is_ascii_digit() {
                        // A literal
                        let mut literal = String::new();
                        let st = pos;

                        if c == '0' {
                            if let Some(_) = self.next_char(|c| c == 'x') {
                                literal.push('0');
                                literal.push('x');
                                while let Some((c, _)) = self.next_char(|c| c.is_ascii_hexdigit()) {
                                    literal.push(c);
                                }
                            } else {
                                return Err(Error::InvalidCharacter(c, pos));
                            }
                        } else {
                            literal.push(c);
                            while let Some((c, _)) = self.next_char(|c| c.is_ascii_digit()) {
                                literal.push(c);
                            }
                        }

                        let val = if literal.starts_with("0x") {
                            u128::from_str_radix(&literal.trim_start_matches("0x"), 16)
                        } else {
                            u128::from_str_radix(&literal, 10)
                        };

                        if let Ok(val) = val {
                            let bytes = val.to_le_bytes().to_vec();
                            tokens.push(Token::new(
                                TokenKind::Literal(bytes),
                                Span::new(st, self.curr_pos),
                            ));
                        } else {
                            return Err(Error::InvalidLiteral(
                                literal,
                                Span::new(st, self.curr_pos),
                            ));
                        }
                    } else {
                        return Err(Error::InvalidCharacter(c, pos));
                    }
                }
            }
        }

        Ok(tokens)
    }
}

mod tests {

    #[test]
    fn test_lexer() {
        use crate::frontend::ir::Lexer;
        let source = "# Generated by ORZCC

global @x = i32 123

fn @putchar (i32) -> void declare

fn @test_func () -> i32 {
^entry:
    br i8 0x00, ^0(i32 0x01, float 0xffffffff), ^else(i32 0x01, i64 0xffffffffffffffff, float 0xffffffff)

^0(i32 %0, float %1):
    %2 = add i32 %0, i32 %0
    j ^1(i32 %2)

^else(i32 %3, i64 %4, float %5):
    %6 = mul i32 %3, i32 %3
    j ^1(i32 %6)

^1(i32 %7):
    ret i32 %7
}".to_string();

        let mut lexer = Lexer::new(source);

        let tokens = lexer.tokenize().unwrap();

        println!("{:#?}", tokens);
    }
}
