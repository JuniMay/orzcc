use std::io;

use crate::ir::{
    frontend::tokens::{Span, TokenKind},
    values::{BinaryOp, CastOp, FCmpCond, ICmpCond, UnaryOp},
    GLOBAL_PREFIX_CHAR, LABEL_PREFIX_CHAR, LOCAL_PREFIX_CHAR, TYPE_PREFIX_CHAR,
};

use super::{
    tokens::{Pos, Token},
    InstKind, KeywordKind,
};

pub struct Lexer<'a, T>
where
    T: io::Read,
{
    buf: &'a mut T,
    curr_pos: Pos,
    curr_char: Option<char>,
}

impl<'a, T> Lexer<'a, T>
where
    T: io::Read,
{
    pub fn new(buf: &'a mut T) -> Self {
        Self {
            buf,
            curr_pos: Pos::new(),
            curr_char: Some(' '),
        }
    }

    pub(super) fn curr_pos(&self) -> Pos {
        self.curr_pos
    }

    fn next_char(&mut self) -> Option<char> {
        let mut buf = [0; 1];
        match self.buf.read(&mut buf) {
            Ok(0) => {
                self.curr_char = None;
                None
            }
            Ok(_) => {
                self.curr_pos.update(buf[0] as char);
                self.curr_char = Some(buf[0] as char);
                self.curr_char
            }
            Err(_) => {
                self.curr_char = None;
                None
            }
        }
    }

    pub(super) fn next_token(&mut self) -> Option<Token> {
        // skip whitespace
        while let Some(c) = self.curr_char {
            if c.is_whitespace() {
                self.next_char();
            } else if c == '#' {
                self.skip_line_comment();
            } else {
                break;
            }
        }

        let start = self.curr_pos;

        if let Some(c) = self.curr_char {
            match c {
                LABEL_PREFIX_CHAR | GLOBAL_PREFIX_CHAR | LOCAL_PREFIX_CHAR | TYPE_PREFIX_CHAR => {
                    self.handle_ident()
                }
                '0'..='9' => self.handle_number(),
                _ => {
                    if c.is_alphabetic() {
                        self.handle_keyword_inst()
                    } else {
                        self.handle_symbol()
                    }
                }
            }
        } else {
            Some(Token::new(Span::new(start), TokenKind::Eof))
        }
    }

    fn handle_ident(&mut self) -> Option<Token> {
        let mut span = Span::new(self.curr_pos);
        let mut ident = String::new();

        while let Some(c) = self.curr_char {
            if c.is_alphanumeric()
                || matches!(
                    c,
                    '_' | LABEL_PREFIX_CHAR
                        | GLOBAL_PREFIX_CHAR
                        | LOCAL_PREFIX_CHAR
                        | TYPE_PREFIX_CHAR
                )
            {
                ident.push(c);
                span.update(self.curr_pos);
            } else {
                break;
            }
            self.next_char();
        }

        match ident.chars().next().unwrap() {
            LABEL_PREFIX_CHAR => Some(Token::new(span, TokenKind::LabelIdent(ident))),
            GLOBAL_PREFIX_CHAR => Some(Token::new(span, TokenKind::GlobalIdent(ident))),
            LOCAL_PREFIX_CHAR => Some(Token::new(span, TokenKind::LocalIdent(ident))),
            TYPE_PREFIX_CHAR => Some(Token::new(span, TokenKind::TypeIdent(ident))),
            _ => unreachable!(),
        }
    }

    fn handle_symbol(&mut self) -> Option<Token> {
        let mut span = Span::new(self.curr_pos);

        let token = Some(match self.curr_char.unwrap() {
            '(' => Token::new(span, TokenKind::LeftParen),
            ')' => Token::new(span, TokenKind::RightParen),
            '{' => Token::new(span, TokenKind::LeftBrace),
            '}' => Token::new(span, TokenKind::RightBrace),
            ',' => Token::new(span, TokenKind::Comma),
            ':' => Token::new(span, TokenKind::Colon),
            '=' => Token::new(span, TokenKind::Equal),
            '[' => Token::new(span, TokenKind::LeftBracket),
            ']' => Token::new(span, TokenKind::RightBracket),
            ';' => Token::new(span, TokenKind::Semicolon),
            '-' => {
                // expect `->`
                self.next_char();
                if self.curr_char == Some('>') {
                    span.update(self.curr_pos);
                    Token::new(span, TokenKind::Arrow)
                } else {
                    return None;
                }
            }
            _ => return None,
        });

        self.next_char();
        token
    }

    fn handle_number(&mut self) -> Option<Token> {
        // only hexadecimal and decimal numbers are supported
        let mut span = Span::new(self.curr_pos);
        let mut string = String::new();
        let mut radix = 10;

        // get the string of the number
        while let Some(c) = self.curr_char {
            if c.is_digit(radix) {
                string.push(c);
                span.update(self.curr_pos);
            } else if c == 'x' {
                radix = 16;
                span.update(self.curr_pos);
            } else {
                break;
            }
            self.next_char();
        }

        // convert the string to number
        let number = u64::from_str_radix(&string, radix);

        if let Ok(number) = number {
            let bytes = number.to_le_bytes().to_vec();
            // remove leading zeros
            let bytes = bytes
                .into_iter()
                .rev()
                .skip_while(|&x| x == 0)
                .collect::<Vec<_>>()
                .into_iter()
                .rev()
                .collect::<Vec<_>>();
            Some(Token::new(span, TokenKind::Bytes(bytes)))
        } else {
            None
        }
    }

    fn handle_keyword_inst(&mut self) -> Option<Token> {
        let mut span = Span::new(self.curr_pos);

        let mut word = String::new();

        while let Some(c) = self.curr_char {
            if c.is_alphanumeric() || c == '_' || c == '.' {
                word.push(c);
                span.update(self.curr_pos);
            } else {
                break;
            }
            self.next_char();
        }

        let token = Some(Token::new(
            span,
            match word.as_str() {
                "func" => TokenKind::Keyword(KeywordKind::Func),
                "decl" => TokenKind::Keyword(KeywordKind::Decl),
                "half" => TokenKind::Keyword(KeywordKind::Half),
                "float" => TokenKind::Keyword(KeywordKind::Float),
                "double" => TokenKind::Keyword(KeywordKind::Double),
                "ptr" => TokenKind::Keyword(KeywordKind::Ptr),
                "void" => TokenKind::Keyword(KeywordKind::Void),
                "undef" => TokenKind::Keyword(KeywordKind::Undef),
                "zero" => TokenKind::Keyword(KeywordKind::Zero),
                "global" => TokenKind::Keyword(KeywordKind::Global),
                "const" => TokenKind::Keyword(KeywordKind::Const),
                "type" => TokenKind::Keyword(KeywordKind::Type),
                "slot" => TokenKind::Keyword(KeywordKind::Slot),

                "add" => TokenKind::Inst(InstKind::Binary(BinaryOp::Add)),
                "fadd" => TokenKind::Inst(InstKind::Binary(BinaryOp::FAdd)),
                "sub" => TokenKind::Inst(InstKind::Binary(BinaryOp::Sub)),
                "fsub" => TokenKind::Inst(InstKind::Binary(BinaryOp::FSub)),
                "mul" => TokenKind::Inst(InstKind::Binary(BinaryOp::Mul)),
                "fmul" => TokenKind::Inst(InstKind::Binary(BinaryOp::FMul)),
                "udiv" => TokenKind::Inst(InstKind::Binary(BinaryOp::UDiv)),
                "sdiv" => TokenKind::Inst(InstKind::Binary(BinaryOp::SDiv)),
                "fdiv" => TokenKind::Inst(InstKind::Binary(BinaryOp::FDiv)),
                "urem" => TokenKind::Inst(InstKind::Binary(BinaryOp::URem)),
                "srem" => TokenKind::Inst(InstKind::Binary(BinaryOp::SRem)),
                "frem" => TokenKind::Inst(InstKind::Binary(BinaryOp::FRem)),
                "and" => TokenKind::Inst(InstKind::Binary(BinaryOp::And)),
                "or" => TokenKind::Inst(InstKind::Binary(BinaryOp::Or)),
                "xor" => TokenKind::Inst(InstKind::Binary(BinaryOp::Xor)),
                "shl" => TokenKind::Inst(InstKind::Binary(BinaryOp::Shl)),
                "lshr" => TokenKind::Inst(InstKind::Binary(BinaryOp::LShr)),
                "ashr" => TokenKind::Inst(InstKind::Binary(BinaryOp::AShr)),

                "icmp.eq" => TokenKind::Inst(InstKind::Binary(BinaryOp::ICmp(ICmpCond::Eq))),
                "icmp.ne" => TokenKind::Inst(InstKind::Binary(BinaryOp::ICmp(ICmpCond::Ne))),
                "icmp.slt" => TokenKind::Inst(InstKind::Binary(BinaryOp::ICmp(ICmpCond::Slt))),
                "icmp.sle" => TokenKind::Inst(InstKind::Binary(BinaryOp::ICmp(ICmpCond::Sle))),

                "fcmp.oeq" => TokenKind::Inst(InstKind::Binary(BinaryOp::FCmp(FCmpCond::OEq))),
                "fcmp.one" => TokenKind::Inst(InstKind::Binary(BinaryOp::FCmp(FCmpCond::ONe))),
                "fcmp.olt" => TokenKind::Inst(InstKind::Binary(BinaryOp::FCmp(FCmpCond::OLt))),
                "fcmp.ole" => TokenKind::Inst(InstKind::Binary(BinaryOp::FCmp(FCmpCond::OLe))),

                "fneg" => TokenKind::Inst(InstKind::Unary(UnaryOp::FNeg)),
                "not" => TokenKind::Inst(InstKind::Unary(UnaryOp::Not)),

                "store" => TokenKind::Inst(InstKind::Store),
                "load" => TokenKind::Inst(InstKind::Load),
                "alloc" => TokenKind::Inst(InstKind::Alloc),
                "jump" => TokenKind::Inst(InstKind::Jump),
                "br" => TokenKind::Inst(InstKind::Branch),
                "ret" => TokenKind::Inst(InstKind::Return),
                "call" => TokenKind::Inst(InstKind::Call),
                "getelemptr" => TokenKind::Inst(InstKind::GetElemPtr),

                "trunc" => TokenKind::Inst(InstKind::Cast(CastOp::Trunc)),
                "zext" => TokenKind::Inst(InstKind::Cast(CastOp::ZExt)),
                "sext" => TokenKind::Inst(InstKind::Cast(CastOp::SExt)),
                "fptrunc" => TokenKind::Inst(InstKind::Cast(CastOp::FpTrunc)),
                "fpext" => TokenKind::Inst(InstKind::Cast(CastOp::FpExt)),
                "fptoui" => TokenKind::Inst(InstKind::Cast(CastOp::FpToUI)),
                "fptosi" => TokenKind::Inst(InstKind::Cast(CastOp::FpToSI)),
                "uitofp" => TokenKind::Inst(InstKind::Cast(CastOp::UIToFp)),
                "sitofp" => TokenKind::Inst(InstKind::Cast(CastOp::SIToFp)),
                "bitcast" => TokenKind::Inst(InstKind::Cast(CastOp::Bitcast)),

                _ => {
                    if word.starts_with('i') {
                        // parse the number
                        let number = word.chars().skip(1).collect::<String>();
                        let number = number.parse::<u64>();

                        if let Ok(number) = number {
                            TokenKind::Keyword(KeywordKind::Int(number as usize))
                        } else {
                            return None;
                        }
                    } else {
                        return None;
                    }
                }
            },
        ));

        token
    }

    fn skip_line_comment(&mut self) {
        while let Some(c) = self.next_char() {
            if c == '\n' {
                break;
            }
        }
        self.next_char();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Cursor;

    #[test]
    fn test_lexer0() {
        let mut buf = Cursor::new("#123\n func @fib (i32) -> i32 { ret %n }     #123");
        let mut lexer = Lexer::new(&mut buf);

        let tokens = vec![
            TokenKind::Keyword(KeywordKind::Func),
            TokenKind::GlobalIdent("@fib".to_string()),
            TokenKind::LeftParen,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::RightParen,
            TokenKind::Arrow,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::LeftBrace,
            TokenKind::Inst(InstKind::Return),
            TokenKind::LocalIdent("%n".to_string()),
            TokenKind::RightBrace,
            TokenKind::Eof,
        ];

        for token in tokens {
            let next_token = lexer.next_token();

            if next_token.is_none() {
                panic!(
                    "expected token {:?}, but got None, curr_char: {:?}",
                    token, lexer.curr_char
                );
            }
            assert_eq!(
                next_token.unwrap().kind,
                token,
                "curr_char: {:?}",
                lexer.curr_char
            );
        }
    }

}
