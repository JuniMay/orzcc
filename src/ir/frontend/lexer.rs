use std::io;

use crate::ir::{
    frontend::tokens::{Span, TokenKind},
    values::{BinaryOp, FCmpCond, ICmpCond, UnaryOp},
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
                '^' | '@' | '%' | '$' => self.handle_ident(),
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
            if c.is_alphanumeric() || matches!(c, '_' | '^' | '@' | '%' | '$') {
                ident.push(c);
                span.update(self.curr_pos);
            } else {
                break;
            }
            self.next_char();
        }

        match ident.chars().next().unwrap() {
            '^' => Some(Token::new(span, TokenKind::LabelIdent(ident))),
            '@' => Some(Token::new(span, TokenKind::GlobalIdent(ident))),
            '%' => Some(Token::new(span, TokenKind::LocalIdent(ident))),
            '$' => Some(Token::new(span, TokenKind::TypeIdent(ident))),
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
        let number = match radix {
            10 => u64::from_str_radix(&string, 10),
            16 => u64::from_str_radix(&string, 16),
            _ => unreachable!(),
        };

        if number.is_ok() {
            let bytes = number.unwrap().to_le_bytes().to_vec();
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
                "fn" => TokenKind::Keyword(KeywordKind::Fn),
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
                "store" => TokenKind::Inst(InstKind::Store),
                "load" => TokenKind::Inst(InstKind::Load),
                "alloc" => TokenKind::Inst(InstKind::Alloc),
                "jump" => TokenKind::Inst(InstKind::Jump),
                "br" => TokenKind::Inst(InstKind::Branch),
                "ret" => TokenKind::Inst(InstKind::Return),
                "call" => TokenKind::Inst(InstKind::Call),
                "getelemptr" => TokenKind::Inst(InstKind::GetElemPtr),

                _ => {
                    if word.starts_with("i") {
                        // parse the number
                        let number = word.chars().skip(1).collect::<String>();
                        let number = number.parse::<u64>();

                        if number.is_ok() {
                            TokenKind::Keyword(KeywordKind::Int(number.unwrap() as usize))
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
        let mut buf = Cursor::new("#123\n fn @fib (i32) -> i32 { ret %n }     #123");
        let mut lexer = Lexer::new(&mut buf);

        let tokens = vec![
            TokenKind::Keyword(KeywordKind::Fn),
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
                assert!(
                    false,
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

    #[test]
    fn test_lexer1() {
        let mut buf = Cursor::new(
            r#"
          #
# orzir module: fibonacci (just for testing)
  #     

    global @x = i32 0x10101010
    const @y = i32 0x20202020
    type $z = { i32, float }
    global @array = [ i32; 3 ] [ 0x01, 0x02, 0x03 ]

fn @fib(i32) -> i32 {

^entry(i32 %0):
    %cond = icmp.sle i32 %0, i32 1234
    br i1 %cond, ^ret(i32 0x01), ^else(i32 %0)

^else(i32 %1):
    %2 = sub i32 %1, i32 0x01
    %3 = sub i32 %1, i32 0x02
    %4 = call i32 @fib(i32 %2)
    %5 = call i32 @fib(i32 %3)
    %6 = add i32 %4, i32 %5
    jump ^ret(i32 %6)

^ret(i32 %result):
    ret i32 %result
}

########
# cccccoooooommmmmeeeeeennnnnnttttt
########

      # 
"#,
        );

        let mut lexer = Lexer::new(&mut buf);

        let expected_tokens = vec![
            TokenKind::Keyword(KeywordKind::Global),
            TokenKind::GlobalIdent("@x".to_string()),
            TokenKind::Equal,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::Bytes(vec![16, 16, 16, 16]),
            TokenKind::Keyword(KeywordKind::Const),
            TokenKind::GlobalIdent("@y".to_string()),
            TokenKind::Equal,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::Bytes(vec![32, 32, 32, 32]),
            TokenKind::Keyword(KeywordKind::Type),
            TokenKind::TypeIdent("$z".to_string()),
            TokenKind::Equal,
            TokenKind::LeftBrace,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::Comma,
            TokenKind::Keyword(KeywordKind::Float),
            TokenKind::RightBrace,
            TokenKind::Keyword(KeywordKind::Global),
            TokenKind::GlobalIdent("@array".to_string()),
            TokenKind::Equal,
            TokenKind::LeftBracket,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::Semicolon,
            TokenKind::Bytes(vec![3]),
            TokenKind::RightBracket,
            TokenKind::LeftBracket,
            TokenKind::Bytes(vec![1]),
            TokenKind::Comma,
            TokenKind::Bytes(vec![2]),
            TokenKind::Comma,
            TokenKind::Bytes(vec![3]),
            TokenKind::RightBracket,
            TokenKind::Keyword(KeywordKind::Fn),
            TokenKind::GlobalIdent("@fib".to_string()),
            TokenKind::LeftParen,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::RightParen,
            TokenKind::Arrow,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::LeftBrace,
            TokenKind::LabelIdent("^entry".to_string()),
            TokenKind::LeftParen,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::LocalIdent("%0".to_string()),
            TokenKind::RightParen,
            TokenKind::Colon,
            TokenKind::LocalIdent("%cond".to_string()),
            TokenKind::Equal,
            TokenKind::Inst(InstKind::Binary(BinaryOp::ICmp(ICmpCond::Sle))),
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::LocalIdent("%0".to_string()),
            TokenKind::Comma,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::Bytes(vec![210, 4]),
            TokenKind::Inst(InstKind::Branch),
            TokenKind::Keyword(KeywordKind::Int(1)),
            TokenKind::LocalIdent("%cond".to_string()),
            TokenKind::Comma,
            TokenKind::LabelIdent("^ret".to_string()),
            TokenKind::LeftParen,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::Bytes(vec![1]),
            TokenKind::RightParen,
            TokenKind::Comma,
            TokenKind::LabelIdent("^else".to_string()),
            TokenKind::LeftParen,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::LocalIdent("%0".to_string()),
            TokenKind::RightParen,
            TokenKind::LabelIdent("^else".to_string()),
            TokenKind::LeftParen,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::LocalIdent("%1".to_string()),
            TokenKind::RightParen,
            TokenKind::Colon,
            TokenKind::LocalIdent("%2".to_string()),
            TokenKind::Equal,
            TokenKind::Inst(InstKind::Binary(BinaryOp::Sub)),
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::LocalIdent("%1".to_string()),
            TokenKind::Comma,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::Bytes(vec![1]),
            TokenKind::LocalIdent("%3".to_string()),
            TokenKind::Equal,
            TokenKind::Inst(InstKind::Binary(BinaryOp::Sub)),
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::LocalIdent("%1".to_string()),
            TokenKind::Comma,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::Bytes(vec![2]),
            TokenKind::LocalIdent("%4".to_string()),
            TokenKind::Equal,
            TokenKind::Inst(InstKind::Call),
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::GlobalIdent("@fib".to_string()),
            TokenKind::LeftParen,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::LocalIdent("%2".to_string()),
            TokenKind::RightParen,
            TokenKind::LocalIdent("%5".to_string()),
            TokenKind::Equal,
            TokenKind::Inst(InstKind::Call),
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::GlobalIdent("@fib".to_string()),
            TokenKind::LeftParen,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::LocalIdent("%3".to_string()),
            TokenKind::RightParen,
            TokenKind::LocalIdent("%6".to_string()),
            TokenKind::Equal,
            TokenKind::Inst(InstKind::Binary(BinaryOp::Add)),
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::LocalIdent("%4".to_string()),
            TokenKind::Comma,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::LocalIdent("%5".to_string()),
            TokenKind::Inst(InstKind::Jump),
            TokenKind::LabelIdent("^ret".to_string()),
            TokenKind::LeftParen,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::LocalIdent("%6".to_string()),
            TokenKind::RightParen,
            TokenKind::LabelIdent("^ret".to_string()),
            TokenKind::LeftParen,
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::LocalIdent("%result".to_string()),
            TokenKind::RightParen,
            TokenKind::Colon,
            TokenKind::Inst(InstKind::Return),
            TokenKind::Keyword(KeywordKind::Int(32)),
            TokenKind::LocalIdent("%result".to_string()),
            TokenKind::RightBrace,
            TokenKind::Eof,
        ];

        for token in expected_tokens {
            let next_token = lexer.next_token();

            if next_token.is_none() {
                assert!(
                    false,
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
