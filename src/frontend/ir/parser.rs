use std::{
    fmt::{self, Debug},
    iter::Peekable,
    str::Chars,
};

use super::ast::*;
use crate::{
    collections::{
        apint::ApInt,
        diagnostic::{Diagnostic, DiagnosticList},
    },
    ir::{
        self,
        CastOp,
        Constant,
        Context,
        FBinaryOp,
        FCmpCond,
        FUnaryOp,
        FloatConstant,
        IBinaryOp,
        ICmpCond,
        IUnaryOp,
        InstKind,
        Signature,
        Symbol,
        Ty,
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    /// A delimiter lexeme.
    ///
    /// In IR, the valid delimiters includes: `:`, `;`, `,`, `=`, `(`, `)`, `[`,
    /// `]`, `{`, `}`, `<`, `>`, `<{`, `}>`, `.` and `->`.
    ///
    /// Note that `.` only appears in `icmp.xxx` or `fcmp.xxx` instructions.
    Delimiter(String),
    /// A block label, starting with `^`, followed by an identifier.
    Label(String),
    /// A value name, starting with `%`, followed by an identifier.
    Value(String),
    /// A symbol name, starting with `@`, followed by an identifier.
    Symbol(String),
    /// All other tokenized string, can be opcode or some literals.
    Tokenized(String),
    /// End of file.
    Eof,

    /// An invalid token.
    Invalid(String),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

impl fmt::Debug for Span {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}:{}", self.start, self.end)
    }
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self { Self { start, end } }
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl From<Span> for std::ops::Range<usize> {
    fn from(span: Span) -> Self { span.start..span.end }
}

impl From<Span> for ir::Span {
    fn from(span: Span) -> Self { ir::Span::from((span.start, span.end)) }
}

pub struct TokenStream<'a> {
    src: Peekable<Chars<'a>>,
    peeked: Option<Token>,
    offset: usize,
}

impl<'a> TokenStream<'a> {
    pub fn new(s: &'a str) -> Self {
        Self {
            src: s.chars().peekable(),
            peeked: None,
            offset: 0,
        }
    }

    fn peek_char(&mut self) -> Option<char> { self.src.peek().copied() }

    fn next_char(&mut self) -> Option<char> {
        let c = self.src.next();
        if let Some(c) = c {
            self.offset += c.len_utf8();
        }
        c
    }

    /// Skip the line comment begin with `//`
    fn skip_line_comment(&mut self) {
        while let Some(c) = self.next_char() {
            if c == '\n' {
                break;
            }
        }
    }

    fn skip_block_comment(&mut self, diag: &mut DiagnosticList) {
        let mut depth = 1;
        let start = self.offset;
        while depth > 0 {
            match self.next_char() {
                Some('/') if self.peek_char() == Some('*') => {
                    self.next_char();
                    depth += 1;
                }
                Some('*') if self.peek_char() == Some('/') => {
                    self.next_char();
                    depth -= 1;
                }
                Some(_) => {}
                None => {
                    // report error and return
                    let snippet = Diagnostic::error("unclosed block comment")
                        .annotate(start..self.offset, "unclosed block comment")
                        .annotate(self.offset - 1..self.offset, "expected `*/` after this");
                    diag.push(snippet);
                    return;
                }
            }
        }
    }

    fn skip_whitespace(&mut self, diag: &mut DiagnosticList) {
        while let Some(c) = self.peek_char() {
            match c {
                ' ' | '\t' | '\r' | '\n' => {
                    self.next_char();
                }
                '/' => {
                    let start = self.offset;
                    let _ = self.next_char();
                    match self.peek_char() {
                        Some('/') => {
                            self.next_char();
                            self.skip_line_comment();
                        }
                        Some('*') => {
                            self.next_char();
                            self.skip_block_comment(diag);
                        }
                        _ => {
                            // since `/` is invalid, report error and skip it
                            let snippet = Diagnostic::error("unexpected character")
                                .annotate(start..start, "invalid character `/`")
                                .note("note", "only `//` or `/*` is allowed here");
                            diag.push(snippet);
                        }
                    }
                }
                _ => break,
            }
        }
    }

    fn read_identifier(&mut self) -> String {
        let mut s = String::new();
        while let Some(c) = self.peek_char() {
            if c.is_alphanumeric() || c == '_' || c == '.' {
                s.push(c);
                self.next_char();
            } else {
                break;
            }
        }
        s
    }

    fn read_label(&mut self) -> Token {
        let start = self.offset;
        let _ = self.next_char();
        let s = self.read_identifier();
        Token {
            kind: TokenKind::Label(s),
            span: Span::new(start, self.offset),
        }
    }

    fn read_value(&mut self) -> Token {
        let start = self.offset;
        let _ = self.next_char();
        let s = self.read_identifier();
        Token {
            kind: TokenKind::Value(s),
            span: Span::new(start, self.offset),
        }
    }

    fn read_symbol(&mut self) -> Token {
        let start = self.offset;
        let _ = self.next_char();
        let s = self.read_identifier();
        Token {
            kind: TokenKind::Symbol(s),
            span: Span::new(start, self.offset),
        }
    }

    fn read_tokenized(&mut self) -> Token {
        let start = self.offset;
        let s = self.read_identifier();
        Token {
            kind: TokenKind::Tokenized(s),
            span: Span::new(start, self.offset),
        }
    }

    pub fn peek(&mut self, diag: &mut DiagnosticList) -> &Token {
        if let Some(ref token) = self.peeked {
            return token;
        }

        self.skip_whitespace(diag);
        let start = self.offset;

        let token = match self.peek_char() {
            Some('^') => self.read_label(),
            Some('%') => self.read_value(),
            Some('@') => self.read_symbol(),
            Some('<') => {
                // '<' or '<{'
                let _ = self.next_char();
                if self.peek_char() == Some('{') {
                    self.next_char();
                    Token {
                        kind: TokenKind::Delimiter("<{".to_string()),
                        span: Span::new(start, self.offset),
                    }
                } else {
                    Token {
                        kind: TokenKind::Delimiter("<".to_string()),
                        span: Span::new(start, self.offset),
                    }
                }
            }
            Some('}') => {
                // '}' or '}>'
                let _ = self.next_char();
                if self.peek_char() == Some('>') {
                    self.next_char();
                    Token {
                        kind: TokenKind::Delimiter("}>".to_string()),
                        span: Span::new(start, self.offset),
                    }
                } else {
                    Token {
                        kind: TokenKind::Delimiter("}".to_string()),
                        span: Span::new(start, self.offset),
                    }
                }
            }
            Some('-') => {
                // must be `->`, otherwise report error and recover
                let _ = self.next_char();
                if self.peek_char() == Some('>') {
                    self.next_char();
                    Token {
                        kind: TokenKind::Delimiter("->".to_string()),
                        span: Span::new(start, self.offset),
                    }
                } else {
                    let snippet = Diagnostic::error("invalid character")
                        .annotate(start..start + 1, "unexpected character `-`")
                        .annotate(self.offset..self.offset + 1, "expected `>` here to be `->`");
                    diag.push(snippet);
                    Token {
                        kind: TokenKind::Invalid("-".to_string()),
                        span: Span::new(start, self.offset),
                    }
                }
            }
            Some(c) if c.is_alphanumeric() => self.read_tokenized(),
            Some(c) if matches!(c, '(' | ')' | ',' | ';' | ':' | '=' | '[' | ']' | '{' | '>') => {
                let _ = self.next_char();
                Token {
                    kind: TokenKind::Delimiter(c.to_string()),
                    span: Span::new(start, self.offset),
                }
            }
            Some(c) => {
                self.next_char();
                let snippet = Diagnostic::error("invalid character")
                    .annotate(start..start + 1, "invalid character");
                diag.push(snippet);
                Token {
                    kind: TokenKind::Invalid(c.to_string()),
                    span: Span::new(start, self.offset),
                }
            }
            None => Token {
                kind: TokenKind::Eof,
                span: Span::new(start, start),
            },
        };

        self.peeked = Some(token);

        // because this is a peek, so the offset should not be changed.
        self.offset = start;

        self.peeked.as_ref().unwrap()
    }

    pub fn next(&mut self, diag: &mut DiagnosticList) -> Token {
        self.peek(diag);
        let token = self.peeked.take().unwrap();
        self.offset = token.span.end; // update offset
        token
    }
}

pub struct Parser<'a> {
    lexer: TokenStream<'a>,
    diag: DiagnosticList,
    ctx: Context,
}

impl<'a> Parser<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            lexer: TokenStream::new(src),
            diag: DiagnosticList::new(),
            ctx: Context::default(),
        }
    }

    pub fn parse(mut self) -> (Vec<Item>, DiagnosticList, Context) {
        let mut items = Vec::new();
        while let Some(item) = self.parse_item() {
            items.push(item);
        }
        (items, self.diag, self.ctx)
    }

    fn expect_delimiter(&mut self, delimiter: impl Into<String>) -> Option<Token> {
        let token = self.lexer.next(&mut self.diag);
        let d = delimiter.into();
        if let TokenKind::Delimiter(s) = &token.kind {
            if s == &d {
                return Some(token);
            }
        }
        let snippet = Diagnostic::error("unexpected token")
            .annotate(token.span.into(), format!("expected `{}`", d));
        self.diag.push(snippet);
        None
    }

    fn maybe_delimiter(&mut self, delimiter: impl Into<String>) -> bool {
        let token = self.lexer.peek(&mut self.diag);
        let d = delimiter.into();
        if let TokenKind::Delimiter(s) = &token.kind {
            if s == &d {
                let _ = self.lexer.next(&mut self.diag);
                return true;
            }
        }
        false
    }

    fn parse_item(&mut self) -> Option<Item> {
        use TokenKind as Tk;

        let token = self.lexer.next(&mut self.diag);
        match token.kind {
            Tk::Tokenized(s) => match s.as_ref() {
                "slot" => {
                    let slot = self.parse_slot()?;
                    Some(Item::Slot(slot))
                }
                "decl" => {
                    let decl = self.parse_decl()?;
                    Some(Item::Decl(decl))
                }
                "func" => {
                    let func = self.parse_func()?;
                    Some(Item::Func(func))
                }
                _ => {
                    let snippet = Diagnostic::error("unexpected token")
                        .annotate(token.span.into(), "expected `slot`, `decl`, or `func`");
                    self.diag.push(snippet);
                    None
                }
            },
            Tk::Delimiter(_) | Tk::Label(_) | Tk::Value(_) | Tk::Invalid(_) | Tk::Symbol(_) => {
                let snippet = Diagnostic::error("unexpected token")
                    .annotate(token.span.into(), "expected `slot`, `decl`, or `func`");
                self.diag.push(snippet);
                None
            }
            Tk::Eof => None,
        }
    }

    fn parse_func(&mut self) -> Option<ParsingFunc> {
        // func <name><sig> { <body> }
        let start = self.lexer.offset;

        let symbol = self.parse_symbol()?;
        let sig = self.parse_sig()?;

        let _ = self.expect_delimiter("{")?;

        let mut blocks = Vec::new();
        loop {
            let token = self.lexer.peek(&mut self.diag);
            if let TokenKind::Delimiter(ref s) = token.kind {
                if s == "}" {
                    break;
                }
            }
            let block = self.parse_block();
            if block.is_none() {
                break; // TODO: maybe we can recover here.
            }
            blocks.push(block.unwrap());
        }

        let _ = self.expect_delimiter("}")?;

        let end = self.lexer.offset;

        Some(ParsingFunc {
            name: symbol,
            sig,
            blocks,
            span: ir::Span::from((start, end)),
        })
    }

    fn parse_value(&mut self) -> Option<ValueRef> {
        let token = self.lexer.next(&mut self.diag);
        if let TokenKind::Value(s) = token.kind {
            Some(ValueRef {
                name: s,
                span: token.span.into(),
            })
        } else {
            let snippet =
                Diagnostic::error("unexpected token").annotate(token.span.into(), "expected value");
            self.diag.push(snippet);
            None
        }
    }

    fn parse_block(&mut self) -> Option<ParsingBlock> {
        use TokenKind as Tk;

        // parse the label and block arguments
        // ^block (<value> : <ty>, ...) :
        let start = self.lexer.offset;
        let token = self.lexer.next(&mut self.diag);
        let block = if let Tk::Label(s) = token.kind {
            BlockRef {
                name: s,
                span: token.span.into(),
            }
        } else {
            let snippet = Diagnostic::error("unexpected token")
                .annotate(token.span.into(), "expected block label");
            self.diag.push(snippet);
            return None;
        };

        let mut params = Vec::new();
        let token = self.lexer.peek(&mut self.diag);

        if let Tk::Delimiter(ref s) = token.kind {
            if s == "(" {
                let _ = self.lexer.next(&mut self.diag);
                loop {
                    let value = self.parse_value()?;
                    let _ = self.expect_delimiter(":")?;
                    let ty = self.parse_ty()?;
                    params.push((value, ty));

                    let token = self.lexer.next(&mut self.diag);
                    if let Tk::Delimiter(s) = token.kind {
                        if s == "," {
                            continue;
                        } else if s == ")" {
                            break;
                        }
                    } else {
                        let snippet = Diagnostic::error("unexpected token")
                            .annotate(token.span.into(), "expected `,` or `)`");
                        self.diag.push(snippet);
                        return None;
                    }
                }
                let _ = self.expect_delimiter(":")?;
            } else if s == ":" {
                let _ = self.lexer.next(&mut self.diag);
            } else {
                let snippet = Diagnostic::error("unexpected token")
                    .annotate(token.span.into(), "expected `:` or `(`");
                self.diag.push(snippet);
                return None;
            }
        }

        let mut insts = Vec::new();
        loop {
            let token = self.lexer.peek(&mut self.diag);
            match token.kind {
                Tk::Delimiter(ref s) if s == "}" => {
                    break;
                }
                Tk::Label(_) => {
                    break;
                }
                Tk::Value(_) | Tk::Tokenized(_) => {
                    let inst = self.parse_inst()?;
                    insts.push(inst);
                }
                Tk::Delimiter(_) | Tk::Eof | Tk::Invalid(_) | Tk::Symbol(_) => {
                    let snippet = Diagnostic::error("unexpected token")
                        .annotate(token.span.into(), "expected instruction");
                    self.diag.push(snippet);
                    return None;
                }
            }
        }

        let end = self.lexer.offset;
        Some(ParsingBlock {
            block,
            params,
            insts,
            span: ir::Span::from((start, end)),
        })
    }

    fn parse_succ(&mut self) -> Option<SuccRef> {
        use TokenKind as Tk;

        // ^block ( %arg0, %arg1, ... ) or just ^block
        let start = self.lexer.offset;
        let token = self.lexer.next(&mut self.diag);
        let block = if let Tk::Label(s) = token.kind {
            BlockRef {
                name: s,
                span: token.span.into(),
            }
        } else {
            let snippet = Diagnostic::error("unexpected token")
                .annotate(token.span.into(), "expected block label");
            self.diag.push(snippet);
            return None;
        };

        let token = self.lexer.peek(&mut self.diag);
        if let TokenKind::Delimiter(ref s) = token.kind {
            if s == "(" {
                let _ = self.lexer.next(&mut self.diag);
                let mut args = Vec::new();
                loop {
                    if self.maybe_delimiter(")") {
                        break;
                    }

                    let value = self.parse_value()?;
                    args.push(value);

                    if self.maybe_delimiter(",") {
                        continue;
                    }
                }
                return Some(SuccRef {
                    block,
                    args,
                    span: ir::Span::from((start, self.lexer.offset)),
                });
            }
        }

        Some(SuccRef {
            block,
            args: Vec::new(),
            span: ir::Span::from((start, self.lexer.offset)),
        })
    }

    fn parse_inst(&mut self) -> Option<ParsingInst> {
        use InstKind as Ik;
        use TokenKind as Tk;

        // two formats:
        // 1. %value, %value, ... = <op> %value, %value, ... : ( <ty>, ... ) | <ty>
        // 2. <op> ...

        let start = self.lexer.offset;

        let token = self.lexer.peek(&mut self.diag);
        let mut results = Vec::new();
        let mut operands = Vec::new();
        let mut result_tys = Vec::new();
        let mut successors = Vec::new();

        let kind = match token.kind {
            Tk::Value(_) => {
                // parse the result names
                loop {
                    let value = self.parse_value()?;
                    results.push(value);

                    let token = self.lexer.next(&mut self.diag);
                    if let Tk::Delimiter(ref s) = token.kind {
                        if s == "," {
                            continue;
                        } else if s == "=" {
                            break;
                        }
                    } else {
                        let snippet = Diagnostic::error("unexpected token")
                            .annotate(token.span.into(), "expected `,` or `=`");
                        self.diag.push(snippet);
                        return None;
                    }
                }

                // opcode
                let token = self.lexer.next(&mut self.diag);
                if let Tk::Tokenized(s) = token.kind {
                    match s.as_ref() {
                        // TODO: maybe support undef value.
                        "iconst" => {
                            // iconst <apint>
                            let token = self.lexer.next(&mut self.diag);
                            if let Tk::Tokenized(s) = token.kind {
                                let apint = ApInt::try_from(s);
                                if let Ok(apint) = apint {
                                    Ik::IConst(apint)
                                } else {
                                    let snippet = Diagnostic::error("invalid apint")
                                        .annotate(token.span.into(), "invalid apint literal");
                                    self.diag.push(snippet);
                                    return None;
                                }
                            } else {
                                let snippet = Diagnostic::error("unexpected token")
                                    .annotate(token.span.into(), "expected integer constant");
                                self.diag.push(snippet);
                                return None;
                            }
                        }
                        "fconst" => {
                            // fconst <apfloat>
                            let token = self.lexer.next(&mut self.diag);
                            if let Tk::Tokenized(s) = token.kind {
                                // trim 0x
                                let s_lower = s.to_lowercase();
                                let s = if let Some(s) = s_lower.strip_prefix("0x") {
                                    s
                                } else {
                                    s_lower.as_str()
                                };
                                let bits = u32::from_str_radix(s, 16);
                                if let Ok(bits) = bits {
                                    Ik::FConst(FloatConstant::Float32(bits))
                                } else {
                                    // try parse as 64 bits
                                    let bits = u64::from_str_radix(s, 16);
                                    if let Ok(bits) = bits {
                                        Ik::FConst(FloatConstant::Float64(bits))
                                    } else {
                                        let snippet = Diagnostic::error("invalid float constant")
                                            .annotate(token.span.into(), "invalid float constant")
                                            .note("note", "expect hexadecimal representation of float constant");
                                        self.diag.push(snippet);
                                        return None;
                                    }
                                }
                            } else {
                                let snippet = Diagnostic::error("unexpected token")
                                    .annotate(token.span.into(), "expected float constant");
                                self.diag.push(snippet);
                                return None;
                            }
                        }
                        "stack_slot" => {
                            // stack_slot <size>
                            let token = self.lexer.next(&mut self.diag);
                            if let Tk::Tokenized(s) = token.kind {
                                // let size = s.parse::<u32>();
                                let radix = if s.starts_with("0x") {
                                    16
                                } else if s.starts_with("0b") {
                                    2
                                } else if s.starts_with("0o") {
                                    8
                                } else {
                                    10
                                };
                                let s = s.to_lowercase();
                                let s = if s.starts_with("0x")
                                    || s.starts_with("0b")
                                    || s.starts_with("0o")
                                {
                                    &s[2..]
                                } else {
                                    &s
                                };
                                let size = u32::from_str_radix(s, radix);
                                if let Ok(size) = size {
                                    Ik::StackSlot(size)
                                } else {
                                    let snippet = Diagnostic::error("invalid apint")
                                        .annotate(token.span.into(), "invalid apint literal");
                                    self.diag.push(snippet);
                                    return None;
                                }
                            } else {
                                let snippet = Diagnostic::error("unexpected token")
                                    .annotate(token.span.into(), "expected integer constant");
                                self.diag.push(snippet);
                                return None;
                            }
                        }
                        "add" => Ik::IBinary(IBinaryOp::Add),
                        "sub" => Ik::IBinary(IBinaryOp::Sub),
                        "mul" => Ik::IBinary(IBinaryOp::Mul),
                        "udiv" => Ik::IBinary(IBinaryOp::UDiv),
                        "sdiv" => Ik::IBinary(IBinaryOp::SDiv),
                        "urem" => Ik::IBinary(IBinaryOp::URem),
                        "srem" => Ik::IBinary(IBinaryOp::SRem),
                        "and" => Ik::IBinary(IBinaryOp::And),
                        "or" => Ik::IBinary(IBinaryOp::Or),
                        "xor" => Ik::IBinary(IBinaryOp::Xor),
                        "shl" => Ik::IBinary(IBinaryOp::Shl),
                        "lshr" => Ik::IBinary(IBinaryOp::LShr),
                        "ashr" => Ik::IBinary(IBinaryOp::AShr),
                        "icmp.eq" => Ik::IBinary(IBinaryOp::Cmp(ICmpCond::Eq)),
                        "icmp.ne" => Ik::IBinary(IBinaryOp::Cmp(ICmpCond::Ne)),
                        "icmp.slt" => Ik::IBinary(IBinaryOp::Cmp(ICmpCond::Slt)),
                        "icmp.sle" => Ik::IBinary(IBinaryOp::Cmp(ICmpCond::Sle)),
                        "icmp.ult" => Ik::IBinary(IBinaryOp::Cmp(ICmpCond::Ult)),
                        "icmp.ule" => Ik::IBinary(IBinaryOp::Cmp(ICmpCond::Ule)),
                        "fadd" => Ik::FBinary(FBinaryOp::Add),
                        "fsub" => Ik::FBinary(FBinaryOp::Sub),
                        "fmul" => Ik::FBinary(FBinaryOp::Mul),
                        "fdiv" => Ik::FBinary(FBinaryOp::Div),
                        "frem" => Ik::FBinary(FBinaryOp::Rem),
                        "fcmp.oeq" => Ik::FBinary(FBinaryOp::Cmp(FCmpCond::OEq)),
                        "fcmp.one" => Ik::FBinary(FBinaryOp::Cmp(FCmpCond::ONe)),
                        "fcmp.olt" => Ik::FBinary(FBinaryOp::Cmp(FCmpCond::OLt)),
                        "fcmp.ole" => Ik::FBinary(FBinaryOp::Cmp(FCmpCond::OLe)),
                        "fcmp.ueq" => Ik::FBinary(FBinaryOp::Cmp(FCmpCond::UEq)),
                        "fcmp.une" => Ik::FBinary(FBinaryOp::Cmp(FCmpCond::UNe)),
                        "fcmp.ult" => Ik::FBinary(FBinaryOp::Cmp(FCmpCond::ULt)),
                        "fcmp.ule" => Ik::FBinary(FBinaryOp::Cmp(FCmpCond::ULe)),
                        "not" => Ik::IUnary(IUnaryOp::Not),
                        "fneg" => Ik::FUnary(FUnaryOp::Neg),
                        "trunc" => Ik::Cast(CastOp::Trunc),
                        "fptrunc" => Ik::Cast(CastOp::FpTrunc),
                        "zext" => Ik::Cast(CastOp::ZExt),
                        "sext" => Ik::Cast(CastOp::SExt),
                        "fptoui" => Ik::Cast(CastOp::FpToUi),
                        "fptosi" => Ik::Cast(CastOp::FpToSi),
                        "uitofp" => Ik::Cast(CastOp::UiToFp),
                        "sitofp" => Ik::Cast(CastOp::SiToFp),
                        "bitcast" => Ik::Cast(CastOp::Bitcast),
                        "fpext" => Ik::Cast(CastOp::FpExt),
                        "ptrtoint" => Ik::Cast(CastOp::PtrToInt),
                        "inttoptr" => Ik::Cast(CastOp::IntToPtr),
                        "load" => Ik::Load,
                        "offset" => Ik::Offset,
                        "get_global" => {
                            let token = self.lexer.next(&mut self.diag);
                            let symbol = if let Tk::Symbol(s) = token.kind {
                                Symbol::new(s).with_source_span(token.span.into())
                            } else {
                                let snippet = Diagnostic::error("unexpected token")
                                    .annotate(token.span.into(), "expected symbol");
                                self.diag.push(snippet);
                                return None;
                            };
                            Ik::GetGlobal(symbol)
                        }
                        "call" => {
                            // call <symbol> ( <args>* )
                            let token = self.lexer.next(&mut self.diag);
                            let symbol = if let Tk::Symbol(s) = token.kind {
                                Symbol::new(s).with_source_span(token.span.into())
                            } else {
                                let snippet = Diagnostic::error("unexpected token")
                                    .annotate(token.span.into(), "expected symbol");
                                self.diag.push(snippet);
                                return None;
                            };
                            Ik::Call(symbol)
                        }
                        "call_indirect" => {
                            // call_indirect <sig>, <callee> ( <args>* )
                            let sig = self.parse_sig()?;
                            self.expect_delimiter(",")?;

                            Ik::CallIndirect(sig)
                        }
                        _ => {
                            let snippet = Diagnostic::error("invalid opcode")
                                .annotate(token.span.into(), "invalid opcode");
                            self.diag.push(snippet);
                            return None;
                        }
                    }
                } else {
                    let snippet = Diagnostic::error("unexpected token")
                        .annotate(token.span.into(), "expected instruction");
                    self.diag.push(snippet);
                    return None;
                }
            }
            Tk::Tokenized(ref s) => {
                match s.as_ref() {
                    "jump" => {
                        let _ = self.lexer.next(&mut self.diag);
                        let succ = self.parse_succ()?;

                        successors.push(succ);

                        Ik::Jump
                    }
                    "br" => {
                        // br %cond, succ0, succ1
                        let _ = self.lexer.next(&mut self.diag);
                        let cond = self.parse_value()?;
                        self.expect_delimiter(",")?;
                        let succ0 = self.parse_succ()?;
                        self.expect_delimiter(",")?;
                        let succ1 = self.parse_succ()?;

                        operands.push(cond);
                        successors.push(succ0);
                        successors.push(succ1);

                        Ik::Br
                    }
                    "call" => {
                        let _ = self.lexer.next(&mut self.diag);
                        let token = self.lexer.next(&mut self.diag);
                        let symbol = if let Tk::Symbol(s) = token.kind {
                            Symbol::new(s).with_source_span(ir::Span::from((
                                token.span.start,
                                token.span.end,
                            )))
                        } else {
                            let snippet = Diagnostic::error("unexpected token")
                                .annotate(token.span.into(), "expected symbol");
                            self.diag.push(snippet);
                            return None;
                        };

                        Ik::Call(symbol)
                    }
                    "call_indirect" => {
                        let _ = self.lexer.next(&mut self.diag);
                        let sig = self.parse_sig()?;
                        self.expect_delimiter(",")?;

                        Ik::CallIndirect(sig)
                    }
                    "ret" => {
                        let _ = self.lexer.next(&mut self.diag);
                        // ret %val // handle later
                        // ret
                        // ret void // we need to consume this one
                        let token = self.lexer.peek(&mut self.diag);
                        if let Tk::Tokenized(ref s) = token.kind {
                            if s == "void" {
                                let _ = self.lexer.next(&mut self.diag);
                            }
                        }

                        Ik::Ret
                    }
                    "store" => {
                        let _ = self.lexer.next(&mut self.diag);
                        Ik::Store
                    }
                    _ => {
                        let snippet = Diagnostic::error("invalid opcode")
                            .annotate(token.span.into(), "invalid opcode");
                        self.diag.push(snippet);
                        return None;
                    }
                }
            }
            Tk::Delimiter(_) | Tk::Eof | Tk::Invalid(_) | Tk::Label(_) | Tk::Symbol(_) => {
                let snippet = Diagnostic::error("unexpected token")
                    .annotate(token.span.into(), "expected instruction");
                self.diag.push(snippet);
                return None;
            }
        };

        // operands and result types.
        //
        // for normal instructions, the format is
        // %opd1, %opd2, %opd3, ...: %res_ty or ( %res_ty1, %res_ty2, ... )
        //
        // for call instructions, the format is
        // ( %arg1, %arg2, ... ) : ( %res_ty1, %res_ty2, ... ) or %res_ty
        //
        // for call_indirect instructions, the format is
        //
        // %callee ( %arg1, %arg2, ... ) : ( %res_ty1, %res_ty2, ... ) or %res_ty

        // maybe just allow any format, skip paren and comma.
        // TODO: more strict parsing
        let mut has_result_tys = false;
        loop {
            let token = self.lexer.peek(&mut self.diag);
            match token.kind {
                Tk::Value(_) => {
                    let value = self.parse_value()?;
                    operands.push(value);
                }
                Tk::Delimiter(ref s) => match s.as_ref() {
                    "," | "(" | ")" => {
                        self.lexer.next(&mut self.diag);
                    }
                    ":" => {
                        self.lexer.next(&mut self.diag);
                        has_result_tys = true;
                        break;
                    }
                    _ => {
                        break;
                    }
                },
                Tk::Eof | Tk::Invalid(_) | Tk::Symbol(_) | Tk::Tokenized(_) | Tk::Label(_) => {
                    break;
                }
            }
        }

        // parse result types
        if has_result_tys {
            let token = self.lexer.peek(&mut self.diag);
            match token.kind {
                Tk::Delimiter(ref s) => match s.as_ref() {
                    "(" => {
                        self.lexer.next(&mut self.diag);
                        loop {
                            let token = self.lexer.peek(&mut self.diag);
                            match token.kind {
                                Tk::Tokenized(_) => {
                                    let ty = self.parse_ty()?;
                                    result_tys.push(ty);
                                }
                                Tk::Delimiter(ref s) => match s.as_ref() {
                                    "," => {
                                        self.lexer.next(&mut self.diag);
                                    }
                                    ")" => {
                                        self.lexer.next(&mut self.diag);
                                        break;
                                    }
                                    _ => {
                                        let snippet = Diagnostic::error("unexpected token")
                                            .annotate(token.span.into(), "expected `,` or `)`");
                                        self.diag.push(snippet);
                                        return None;
                                    }
                                },
                                Tk::Eof
                                | Tk::Invalid(_)
                                | Tk::Symbol(_)
                                | Tk::Value(_)
                                | Tk::Label(_) => {
                                    let snippet = Diagnostic::error("unexpected token")
                                        .annotate(token.span.into(), "expected value or delimiter");
                                    self.diag.push(snippet);
                                    return None;
                                }
                            }
                        }
                    }
                    _ => {
                        let ty = self.parse_ty()?;
                        result_tys.push(ty);
                    }
                },
                Tk::Tokenized(_) => {
                    let ty = self.parse_ty()?;
                    result_tys.push(ty);
                }
                Tk::Eof | Tk::Invalid(_) | Tk::Symbol(_) | Tk::Label(_) | Tk::Value(_) => {
                    let snippet = Diagnostic::error("unexpected token")
                        .annotate(token.span.into(), "expected value or delimiter");
                    self.diag.push(snippet);
                    return None;
                }
            }
        }

        Some(ParsingInst {
            results,
            kind,
            operands,
            successors,
            result_tys,
            span: ir::Span::from((start, self.lexer.offset)),
        })
    }

    fn parse_decl(&mut self) -> Option<ParsingDecl> {
        // decl <name><sig>
        let start = self.lexer.offset;
        let symbol = self.parse_symbol()?;
        let sig = self.parse_sig()?;
        let end = self.lexer.offset;

        Some(ParsingDecl {
            name: symbol,
            sig,
            span: ir::Span::from((start, end)),
        })
    }

    fn parse_slot(&mut self) -> Option<ParsingSlot> {
        // slot <name> : <ty> = <init>
        let start = self.lexer.offset;

        let symbol = self.parse_symbol()?;
        let _ = self.expect_delimiter(":")?;
        let ty = self.parse_ty()?;
        let _ = self.expect_delimiter("=")?;
        let init = self.parse_slot_init()?;

        let end = self.lexer.offset;

        Some(ParsingSlot {
            name: symbol,
            ty,
            init,
            span: ir::Span::from((start, end)),
        })
    }

    fn parse_symbol(&mut self) -> Option<Symbol> {
        let token = self.lexer.next(&mut self.diag);
        if let TokenKind::Symbol(s) = token.kind {
            let symbol = Symbol::new(s).with_source_span(token.span.into());
            Some(symbol)
        } else {
            let snippet = Diagnostic::error("unexpected token")
                .annotate(token.span.into(), "expected symbol starting with `@`");
            self.diag.push(snippet);
            None
        }
    }

    fn parse_slot_init(&mut self) -> Option<Constant> {
        use TokenKind as Tk;
        let start = self.lexer.offset;
        let token = self.lexer.next(&mut self.diag);
        match token.kind {
            Tk::Tokenized(s) => {
                // undef or zeroinit
                match s.as_ref() {
                    "undef" => Some(Constant::undef().with_source_span(token.span.into())),
                    "zeroinit" => Some(Constant::zeroinit().with_source_span(token.span.into())),
                    _ => {
                        let snippet = Diagnostic::error("unexpected token").annotate(
                            token.span.into(),
                            "expected `undef`, `zeroinit`, or byte list",
                        );
                        self.diag.push(snippet);
                        None
                    }
                }
            }
            Tk::Delimiter(s) => {
                // [ <bytes,> ]
                if s == "[" {
                    let mut bytes = Vec::new();
                    loop {
                        let token = self.lexer.next(&mut self.diag);
                        if let Tk::Tokenized(s) = token.kind {
                            // expect to be a 0x00..FF
                            if let Some(byte) = s.strip_prefix("0x") {
                                if let Ok(byte) = u8::from_str_radix(byte, 16) {
                                    bytes.push(byte);
                                } else {
                                    let snippet = Diagnostic::error("invalid byte")
                                        .annotate(token.span.into(), "invalid byte");
                                    self.diag.push(snippet);
                                    return None;
                                }
                            } else {
                                let snippet = Diagnostic::error("invalid byte")
                                    .annotate(token.span.into(), "invalid byte")
                                    .annotate(
                                        token.span.start..token.span.start,
                                        "expect `0x` prefix",
                                    );
                                self.diag.push(snippet);
                                return None;
                            }
                        } else if let Tk::Delimiter(s) = token.kind {
                            if s == "]" {
                                break;
                            } else if s == "," {
                                continue;
                            }
                        } else {
                            let snippet = Diagnostic::error("unexpected token")
                                .annotate(token.span.into(), "expected `,`, `]`, or a byte");
                            self.diag.push(snippet);
                            return None;
                        }
                    }

                    Some(
                        Constant::bytes(bytes)
                            .with_source_span(ir::Span::from((start, self.lexer.offset))),
                    )
                } else {
                    let snippet = Diagnostic::error("unexpected token")
                        .annotate(token.span.into(), format!("expected `[` but got `{}`", s));
                    self.diag.push(snippet);
                    None
                }
            }
            Tk::Label(_) | Tk::Value(_) | Tk::Eof | Tk::Invalid(_) | Tk::Symbol(_) => {
                let snippet = Diagnostic::error("unexpected token")
                    .annotate(token.span.into(), "unexpected token");
                self.diag.push(snippet);
                None
            }
        }
    }

    fn parse_ty(&mut self) -> Option<(Ty, ir::Span)> {
        use TokenKind as Tk;

        let start = self.lexer.offset;
        let token = self.lexer.next(&mut self.diag);
        // iX, f32, f64, void, ptr, < elem; len >, [ elem; len ], { elem0,
        // elem1, ... }, <{ elem0, elem1, .. }>

        let ty = match token.kind {
            Tk::Tokenized(s) => {
                if let Some(s) = s.strip_prefix('i') {
                    // parse the bitwidth
                    if let Ok(bits) = s.parse::<u16>() {
                        Ty::int(&mut self.ctx, bits)
                    } else {
                        let snippet = Diagnostic::error("invalid integer")
                            .annotate(token.span.into(), "invalid integer");
                        self.diag.push(snippet);
                        return None;
                    }
                } else {
                    match s.as_ref() {
                        "f32" => Ty::float32(&mut self.ctx),
                        "f64" => Ty::float64(&mut self.ctx),
                        "void" => Ty::void(&mut self.ctx),
                        "ptr" => Ty::ptr(&mut self.ctx),
                        _ => {
                            let snippet = Diagnostic::error("unexpected token")
                                .annotate(token.span.into(), "unexpected token");
                            self.diag.push(snippet);
                            return None;
                        }
                    }
                }
            }
            Tk::Delimiter(s) => {
                match s.as_ref() {
                    "<" => {
                        // < elem; len >
                        let (elem_ty, _) = self.parse_ty()?;
                        let _ = self.expect_delimiter(";")?;
                        let token = self.lexer.next(&mut self.diag);
                        let len = if let Tk::Tokenized(s) = token.kind {
                            if let Ok(len) = s.parse::<u64>() {
                                len
                            } else {
                                let snippet = Diagnostic::error("invalid integer")
                                    .annotate(token.span.into(), "invalid integer");
                                self.diag.push(snippet);
                                return None;
                            }
                        } else {
                            let snippet = Diagnostic::error("unexpected token")
                                .annotate(token.span.into(), "expected length of the SIMD type");
                            self.diag.push(snippet);
                            return None;
                        };
                        let _ = self.expect_delimiter(">")?;

                        if !len.is_power_of_two() {
                            let snippet = Diagnostic::error("invalid length")
                                .annotate(token.span.into(), "length must be a power of two");
                            self.diag.push(snippet);
                            return None;
                        }

                        Ty::simd(&mut self.ctx, elem_ty, len.trailing_zeros() as u16)
                    }
                    "[" => {
                        // [ elem; len ]
                        let (elem_ty, _) = self.parse_ty()?;
                        let _ = self.expect_delimiter(";")?;
                        let token = self.lexer.next(&mut self.diag);
                        let len = if let Tk::Tokenized(s) = token.kind {
                            if let Ok(len) = s.parse::<usize>() {
                                len
                            } else {
                                let snippet = Diagnostic::error("invalid integer")
                                    .annotate(token.span.into(), "invalid integer");
                                self.diag.push(snippet);
                                return None;
                            }
                        } else {
                            let snippet = Diagnostic::error("unexpected token")
                                .annotate(token.span.into(), "expected length of the array type");
                            self.diag.push(snippet);
                            return None;
                        };
                        let _ = self.expect_delimiter("]")?;

                        Ty::array(&mut self.ctx, elem_ty, len)
                    }
                    "{" => {
                        // { elem0, elem1, ... }
                        let mut elems = Vec::new();
                        loop {
                            let (elem_ty, _) = self.parse_ty()?;
                            elems.push(elem_ty);
                            let token = self.lexer.next(&mut self.diag);
                            if let Tk::Delimiter(s) = token.kind {
                                match s.as_ref() {
                                    "," => continue,
                                    "}" => break,
                                    _ => {}
                                }
                            }
                            let snippet = Diagnostic::error("unexpected token")
                                .annotate(token.span.into(), "expected `,` or `}`");
                            self.diag.push(snippet);
                        }
                        Ty::struct_(&mut self.ctx, elems, false)
                    }
                    "<{" => {
                        // <{ elem0, elem1, .. }>
                        let mut elems = Vec::new();
                        loop {
                            let (elem_ty, _) = self.parse_ty()?;
                            elems.push(elem_ty);
                            let token = self.lexer.next(&mut self.diag);
                            if let Tk::Delimiter(s) = token.kind {
                                match s.as_ref() {
                                    "," => continue,
                                    "}>" => break,
                                    _ => {}
                                }
                            }
                            let snippet = Diagnostic::error("unexpected token")
                                .annotate(token.span.into(), "expected `,` or `}>`");
                            self.diag.push(snippet);
                        }
                        Ty::struct_(&mut self.ctx, elems, true)
                    }
                    _ => {
                        let snippet = Diagnostic::error("unexpected token")
                            .annotate(token.span.into(), "expected `<`, `[`, `{`, or `<{`");
                        self.diag.push(snippet);
                        return None;
                    }
                }
            }
            Tk::Label(_) | Tk::Value(_) | Tk::Eof | Tk::Invalid(_) | Tk::Symbol(_) => {
                let snippet = Diagnostic::error("unexpected token")
                    .annotate(token.span.into(), "expected type");
                self.diag.push(snippet);
                return None;
            }
        };

        let end = self.lexer.offset;
        Some((ty, ir::Span::from((start, end))))
    }

    fn parse_sig(&mut self) -> Option<Signature> {
        use TokenKind as Tk;

        // ( <types,> ) -> <type> | ( <types,> ) -> ( <types,> )
        let start = self.lexer.offset;
        let _ = self.expect_delimiter("(")?;
        let mut params = Vec::new();
        loop {
            let token = self.lexer.peek(&mut self.diag);
            if let Tk::Delimiter(ref s) = token.kind {
                if s == ")" {
                    let _ = self.expect_delimiter(")").unwrap();
                    break;
                }
            }
            let (ty, _) = self.parse_ty()?;
            params.push(ty);
            let token = self.lexer.next(&mut self.diag);
            if let Tk::Delimiter(s) = token.kind {
                match s.as_ref() {
                    "," => continue,
                    ")" => break,
                    _ => {}
                }
            }
            let snippet = Diagnostic::error("unexpected token")
                .annotate(token.span.into(), "expected `,` or `)`");
            self.diag.push(snippet);
            return None;
        }
        let _ = self.expect_delimiter("->")?;
        // because the return type can be struct or simd, the delimiter should be
        // handled separately, so peek here.
        let token = self.lexer.peek(&mut self.diag);

        let ret = match token.kind {
            Tk::Delimiter(ref s) if s == "(" => {
                let _ = self.expect_delimiter("(").unwrap();
                let mut rets = Vec::new();
                loop {
                    let (ty, _) = self.parse_ty()?;
                    rets.push(ty);
                    let token = self.lexer.next(&mut self.diag);
                    if let Tk::Delimiter(s) = token.kind {
                        match s.as_ref() {
                            "," => continue,
                            ")" => break,
                            _ => {}
                        }
                    }
                    let snippet = Diagnostic::error("unexpected token")
                        .annotate(token.span.into(), "expected `,` or `)`");
                    self.diag.push(snippet);
                    return None;
                }
                rets
            }
            Tk::Delimiter(_) | Tk::Tokenized(_) => {
                let (ty, _) = self.parse_ty()?;
                vec![ty]
            }
            Tk::Label(_) | Tk::Value(_) | Tk::Eof | Tk::Invalid(_) | Tk::Symbol(_) => {
                let snippet = Diagnostic::error("unexpected token")
                    .annotate(token.span.into(), "expected type or `(` for types");
                self.diag.push(snippet);
                return None;
            }
        };

        let end = self.lexer.offset;

        let sig = Signature::new(params, ret).with_source_span(ir::Span::from((start, end)));
        Some(sig)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::collections::diagnostic::RenderOptions;

    #[test]
    fn test_skip_line_comment() {
        let mut diag = DiagnosticList::new();
        let mut ts = TokenStream::new("// this is a line comment\na");
        ts.skip_whitespace(&mut diag);
        assert_eq!(ts.peek_char(), Some('a'));
    }

    #[test]
    fn test_skip_block_comment() {
        let mut diag = DiagnosticList::new();
        let mut ts = TokenStream::new("/* this is a /* nested */ block comment */a");
        ts.skip_whitespace(&mut diag);
        assert_eq!(ts.peek_char(), Some('a'));
    }

    #[test]
    fn test_invalid_line_comment() {
        let mut diag = DiagnosticList::new();
        let mut ts = TokenStream::new("/a");
        ts.skip_whitespace(&mut diag);
        assert_eq!(ts.peek_char(), Some('a'));
        assert_eq!(diag.len(), 1);

        println!("{}", diag.render("/a", &RenderOptions::unicode_round()));
    }

    #[test]
    fn test_unclosed_block_comment() {
        let mut diag = DiagnosticList::new();
        let mut ts = TokenStream::new("/* unclosed block\n comment");
        ts.skip_whitespace(&mut diag);
        assert_eq!(ts.peek_char(), None);
        assert_eq!(diag.len(), 1);

        println!(
            "{}",
            diag.render(
                "/* unclosed block\n comment",
                &RenderOptions::unicode_round()
            )
        );
    }

    #[test]
    fn test_read_label() {
        let mut diag = DiagnosticList::new();
        let mut ts = TokenStream::new("^label");
        let token = ts.next(&mut diag);
        assert_eq!(token.kind, TokenKind::Label("label".to_string()));
    }

    #[test]
    fn test_read_value() {
        let mut diag = DiagnosticList::new();
        let mut ts = TokenStream::new("%value");
        let token = ts.next(&mut diag);
        assert_eq!(token.kind, TokenKind::Value("value".to_string()));
    }

    #[test]
    fn test_read_symbol() {
        let mut diag = DiagnosticList::new();
        let mut ts = TokenStream::new("@symbol");
        let token = ts.next(&mut diag);
        assert_eq!(token.kind, TokenKind::Symbol("symbol".to_string()));
    }

    #[test]
    fn test_invalid_chracter_0() {
        let mut diag = DiagnosticList::new();
        let mut ts = TokenStream::new("- - invalid!");
        let token = ts.next(&mut diag);
        assert_eq!(token.kind, TokenKind::Invalid("-".to_string()));
        assert_eq!(diag.len(), 1);

        println!(
            "{}",
            diag.render("- - invalid!", &RenderOptions::unicode_round())
        );
    }

    #[test]
    fn test_invalid_chracter_1() {
        let mut diag = DiagnosticList::new();
        let mut ts = TokenStream::new("! invalid!");
        let token = ts.next(&mut diag);
        assert_eq!(token.kind, TokenKind::Invalid("!".to_string()));
        assert_eq!(diag.len(), 1);

        println!(
            "{}",
            diag.render("! invalid!", &RenderOptions::unicode_round())
        );
    }

    #[test]
    fn test_parse_slot_init() {
        let mut parser = Parser::new("undef");
        let constant = parser.parse_slot_init().unwrap();

        assert_eq!(constant, Constant::undef());

        let mut parser = Parser::new("zeroinit");
        let constant = parser.parse_slot_init().unwrap();

        assert_eq!(constant, Constant::zeroinit());

        let mut parser = Parser::new("[ 0x00 ]");
        let bytes = parser.parse_slot_init().unwrap();

        assert_eq!(bytes, Constant::bytes(vec![0x00]));

        let mut parser = Parser::new("[]");
        let bytes = parser.parse_slot_init().unwrap();

        assert_eq!(bytes, Constant::bytes(vec![]));

        let mut parser = Parser::new("[ 0x00, 0x01, 0x02 ]");
        let bytes = parser.parse_slot_init().unwrap();

        assert_eq!(bytes, Constant::bytes(vec![0x00, 0x01, 0x02]));
    }

    #[test]
    fn test_parse_simd_ty() {
        let mut parser = Parser::new("i11451");
        let (ty, _) = parser.parse_ty().unwrap();
        let ctx = &mut parser.ctx;

        assert_eq!(ty, Ty::int(ctx, 11451));

        let mut parser = Parser::new("< f32; 128 >");
        let (ty, _) = parser.parse_ty().unwrap();
        let ctx = &mut parser.ctx;

        let float32 = Ty::float32(ctx);
        assert_eq!(ty, Ty::simd(ctx, float32, 7));

        let mut parser = Parser::new("[ ptr  ; 128 ]");
        let (ty, _) = parser.parse_ty().unwrap();
        let ctx = &mut parser.ctx;

        let ptr = Ty::ptr(ctx);
        assert_eq!(ty, Ty::array(ctx, ptr, 128));

        let mut parser = Parser::new("<{ i32, f32, [f64; 8], ptr, { i1, i8 } }> ");
        let (ty, _) = parser.parse_ty().unwrap();
        let ctx = &parser.ctx;

        assert_eq!(
            format!("{}", ty.display(ctx)),
            "<{i32, f32, [f64; 8], ptr, {i1, i8}}>"
        )
    }

    #[test]
    fn test_parse_signature() {
        let mut parser = Parser::new("() -> void");
        let sig = parser.parse_sig().unwrap();
        let ctx = &mut parser.ctx;

        assert_eq!(format!("{}", sig.display(ctx)), "() -> void");

        let mut parser = Parser::new("(i32, f32) -> (f64, ptr)");
        let sig = parser.parse_sig().unwrap();
        let ctx = &mut parser.ctx;

        assert_eq!(format!("{}", sig.display(ctx)), "(i32, f32) -> (f64, ptr)");

        let mut parser = Parser::new(
            "(i32, f32, [f64; 8], ptr, { i1, i8 }) -> <{ i32, f32, [f64; 8], ptr, { i1, i8 } }>",
        );
        let sig = parser.parse_sig().unwrap();
        let ctx = &mut parser.ctx;

        assert_eq!(
            format!("{}", sig.display(ctx)),
            // just to test parsing, array should not be passed by value
            "(i32, f32, [f64; 8], ptr, {i1, i8}) -> <{i32, f32, [f64; 8], ptr, {i1, i8}}>"
        );
    }

    #[test]
    fn test_parse_decl() {
        let src = "decl @foo() -> void";

        let mut parser = Parser::new(src);
        let decl = parser.parse_item().unwrap();

        if let Item::Decl(decl) = decl {
            assert_eq!(decl.name, Symbol::new("foo"));
            assert_eq!(format!("{}", decl.sig.display(&parser.ctx)), "() -> void");
        } else {
            panic!("expected decl");
        }
    }
}
