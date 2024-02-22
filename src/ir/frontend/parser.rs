use std::io::{self};

use crate::ir::types::Type;

use super::{
    ast::{
        self, Array, Ast, AstNode, AstNodeBox, Block, Callee, FunctionDecl, FunctionDef, GlobalDef,
        Inst, Operand, Struct, TypeDef,
    },
    lexer::Lexer,
    tokens::{Pos, Span, Token, TokenKind},
    InstKind, KeywordKind,
};

/// A parser.  
pub struct Parser<'a, T>
where
    T: io::Read,
{
    /// The lexer to tokenize the input.
    lexer: Lexer<'a, T>,

    /// Current token.
    ///
    /// If peeked is true, this is a peeked token. Otherwise, this is a consumed/eaten token.
    curr_token: Token,

    /// If the current token is a peeked token.
    ///
    /// At most one token can be peeked, so this is a boolean.
    peeked: bool,
}

#[derive(Debug)]
pub enum ParseError {
    /// Lexer error
    LexerError,

    /// Unexpected token
    UnexpectedToken(Span),
}

impl<'a, T> Parser<'a, T>
where
    T: io::Read,
{
    pub fn new(input: &'a mut T) -> Self {
        Self {
            lexer: Lexer::new(input),
            curr_token: Token::new(Span::new(Pos::default()), TokenKind::Eof),
            peeked: false,
        }
    }

    /// Get the next token and comsume it.
    fn next_token(&mut self) -> Result<&Token, ParseError> {
        if self.peeked {
            self.peeked = false;
            return Ok(&self.curr_token);
        }
        let token = self.lexer.next_token().ok_or(ParseError::LexerError)?;
        self.curr_token = token;
        Ok(&self.curr_token)
    }

    /// Peek the next token.
    ///
    /// One token can be peeked multiple times.
    fn peek_token(&mut self) -> Result<&Token, ParseError> {
        if self.peeked {
            return Ok(&self.curr_token);
        }
        let token = self.lexer.next_token().ok_or(ParseError::LexerError)?;
        self.curr_token = token;
        self.peeked = true;
        Ok(&self.curr_token)
    }

    /// Consume the peeked token.
    ///
    /// If the current token is not a peeked token, do nothing.
    fn consume(&mut self) {
        self.peeked = false;
    }

    /// Backtrack to the peeked token.
    ///
    /// This will make the current token a peeked token.
    fn backtrack(&mut self) {
        self.peeked = true;
    }

    fn unexpected_token(&self) -> ParseError {
        ParseError::UnexpectedToken(self.curr_token.span.clone())
    }

    /// Consume the token and verify.
    ///
    /// If the current token is not the expected token, return an error.
    fn expect(&mut self, kind: TokenKind) -> Result<(), ParseError> {
        self.next_token()?;
        if self.curr_token.kind == kind {
            Ok(())
        } else {
            Err(self.unexpected_token())
        }
    }

    /// Parse the input.
    pub fn parse(&mut self) -> Result<Ast, ParseError> {
        let mut ast = Ast::new();

        loop {
            let item = self.parse_item()?;
            ast.push(item);
            self.peek_token()?;
            if self.curr_token.kind == TokenKind::Eof {
                break;
            }
        }

        Ok(ast)
    }

    fn parse_item(&mut self) -> Result<AstNodeBox, ParseError> {
        let token = self.peek_token()?;
        match token.kind {
            TokenKind::Keyword(ref kw) => match kw {
                KeywordKind::Type => self.parse_type_def(),
                KeywordKind::Global => self.parse_global(true),
                KeywordKind::Const => self.parse_global(false),
                KeywordKind::Fn => self.parse_function_def(),
                KeywordKind::Decl => self.parse_function_decl(),
                _ => Err(self.unexpected_token()),
            },
            _ => Err(self.unexpected_token()),
        }
    }

    fn parse_type_def(&mut self) -> Result<AstNodeBox, ParseError> {
        self.expect(TokenKind::Keyword(KeywordKind::Type))?;
        let token = self.next_token()?;
        let name = match token.kind {
            TokenKind::TypeIdent(ref name) => name.clone(),
            _ => return Err(self.unexpected_token()),
        };
        self.expect(TokenKind::Equal)?;
        let ty = self.parse_type()?;
        Ok(TypeDef::new_boxed(name, ty))
    }

    fn parse_global(&mut self, mutable: bool) -> Result<AstNodeBox, ParseError> {
        if mutable {
            self.expect(TokenKind::Keyword(KeywordKind::Global))?;
        } else {
            self.expect(TokenKind::Keyword(KeywordKind::Const))?;
        }
        let token = self.next_token()?;
        let name = match token.kind {
            TokenKind::GlobalIdent(ref name) => name.clone(),
            _ => return Err(self.unexpected_token()),
        };
        self.expect(TokenKind::Equal)?;
        let ty = self.parse_type()?;
        let init = self.parse_global_init()?;
        Ok(GlobalDef::new_boxed(mutable, name, ty, init))
    }

    fn parse_global_init(&mut self) -> Result<AstNodeBox, ParseError> {
        let token = self.peek_token()?;
        match token.kind {
            TokenKind::LeftBracket => self.parse_array(),
            TokenKind::LeftBrace => self.parse_struct(),
            TokenKind::Bytes(_) => self.parse_bytes(),
            _ => Err(self.unexpected_token()),
        }
    }

    fn parse_bytes(&mut self) -> Result<AstNodeBox, ParseError> {
        let token = self.next_token()?;
        if let TokenKind::Bytes(ref bytes) = token.kind {
            Ok(AstNode::new_boxed_bytes(bytes.clone()))
        } else {
            Err(self.unexpected_token())
        }
    }

    fn parse_array(&mut self) -> Result<AstNodeBox, ParseError> {
        self.expect(TokenKind::LeftBracket)?;
        let mut elems = Vec::new();
        loop {
            let parse_result = self.parse_global_init();
            match parse_result {
                Ok(node) => elems.push(node),
                Err(_) => match self.next_token()?.kind {
                    TokenKind::RightBracket => break,
                    TokenKind::Comma => continue,
                    _ => return Err(self.unexpected_token()),
                },
            }
        }
        let node = Array::new_boxed(elems);
        Ok(node)
    }

    fn parse_struct(&mut self) -> Result<AstNodeBox, ParseError> {
        self.expect(TokenKind::LeftBrace)?;

        let mut fields = Vec::new();
        loop {
            let parse_result = self.parse_global_init();
            match parse_result {
                Ok(node) => fields.push(node),
                Err(_) => match self.next_token()?.kind {
                    TokenKind::RightBrace => break,
                    TokenKind::Comma => continue,
                    _ => return Err(self.unexpected_token()),
                },
            }
        }
        let node = Struct::new_boxed(fields);
        Ok(node)
    }

    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let token = self.peek_token()?;
        match token.kind {
            TokenKind::LeftParen => self.parse_function_type(),
            TokenKind::LeftBrace => self.parse_struct_type(),
            TokenKind::LeftBracket => self.parse_array_type(),
            TokenKind::TypeIdent(_) => self.parse_type_ident(),
            TokenKind::Keyword(_) => self.parse_primitive_type(),
            _ => Err(self.unexpected_token()),
        }
    }

    fn parse_primitive_type(&mut self) -> Result<Type, ParseError> {
        // consume the token
        let token = self.next_token()?;
        match token.kind {
            TokenKind::Keyword(ref kw) => match kw {
                KeywordKind::Int(n) => Ok(Type::mk_int(*n)),
                KeywordKind::Half => Ok(Type::mk_half()),
                KeywordKind::Float => Ok(Type::mk_float()),
                KeywordKind::Double => Ok(Type::mk_double()),
                KeywordKind::Ptr => Ok(Type::mk_ptr()),
                KeywordKind::Void => Ok(Type::mk_void()),
                _ => Err(self.unexpected_token()),
            },
            _ => Err(self.unexpected_token()),
        }
    }

    fn parse_type_ident(&mut self) -> Result<Type, ParseError> {
        // consume the token
        match self.next_token()?.kind {
            TokenKind::TypeIdent(ref name) => Ok(Type::mk_type(name.clone())),
            _ => Err(self.unexpected_token()),
        }
    }

    fn parse_struct_type(&mut self) -> Result<Type, ParseError> {
        self.expect(TokenKind::LeftBrace)?;
        let mut fields = Vec::new();
        loop {
            let parse_result = self.parse_type();
            match parse_result {
                Ok(ty) => fields.push(ty),
                Err(_) => match self.next_token()?.kind {
                    TokenKind::RightBrace => break,
                    TokenKind::Comma => continue,
                    _ => return Err(self.unexpected_token()),
                },
            }
        }
        Ok(Type::mk_struct(fields))
    }

    fn parse_array_type(&mut self) -> Result<Type, ParseError> {
        self.expect(TokenKind::LeftBracket)?;
        let ty = self.parse_type()?;
        self.expect(TokenKind::Semicolon)?;
        let token = self.next_token()?;
        let size = if let TokenKind::Bytes(ref bytes) = token.kind {
            // little endian byte sequence to usize
            let mut size = 0;
            for (i, byte) in bytes.iter().enumerate() {
                size |= (*byte as usize) << (i * 8);
            }
            size
        } else {
            return Err(self.unexpected_token());
        };
        self.expect(TokenKind::RightBracket)?;
        Ok(Type::mk_array(size, ty))
    }

    fn parse_function_type(&mut self) -> Result<Type, ParseError> {
        self.expect(TokenKind::LeftParen)?;
        let mut params = Vec::new();
        loop {
            let parse_result = self.parse_type();
            match parse_result {
                Ok(ty) => params.push(ty),
                Err(_) => match self.next_token()?.kind {
                    TokenKind::RightParen => break,
                    TokenKind::Comma => continue,
                    _ => return Err(self.unexpected_token()),
                },
            }
        }
        self.expect(TokenKind::Arrow)?;
        let ret = self.parse_type()?;
        Ok(Type::mk_function(params, ret))
    }

    fn parse_block(&mut self) -> Result<AstNodeBox, ParseError> {
        let token = self.peek_token()?;
        let name = if let TokenKind::LabelIdent(ref name) = token.kind {
            name.clone()
        } else {
            return Err(self.unexpected_token());
        };

        self.consume();

        let token = self.next_token()?;
        match token.kind {
            TokenKind::LeftParen => {
                let mut params = Vec::new();
                loop {
                    let ty = self.parse_type()?;
                    let token = self.next_token()?;
                    let ident = match token.kind {
                        TokenKind::LocalIdent(ref name) => name.clone(),
                        _ => return Err(self.unexpected_token()),
                    };

                    params.push((ty, ident));
                    match self.next_token()?.kind {
                        TokenKind::RightParen => break,
                        TokenKind::Comma => continue,
                        _ => return Err(self.unexpected_token()),
                    }
                }

                self.expect(TokenKind::Colon)?;
                let insts = self.parse_block_body()?;
                let node = Block::new_boxed(name, params, insts);
                Ok(node)
            }
            TokenKind::Colon => {
                let insts = self.parse_block_body()?;
                let node = Block::new_boxed(name, Vec::new(), insts);
                Ok(node)
            }
            _ => Err(self.unexpected_token()),
        }
    }

    fn parse_block_body(&mut self) -> Result<Vec<AstNodeBox>, ParseError> {
        let mut insts = Vec::new();
        loop {
            let parse_result = self.parse_inst();
            if let Ok(node) = parse_result {
                insts.push(node);
            } else {
                // because `parse_inst` will consume the token, we need to backtrack
                self.backtrack();
                break;
            }
        }
        Ok(insts)
    }

    fn parse_inst(&mut self) -> Result<AstNodeBox, ParseError> {
        // this will consume the token
        let token = self.next_token()?;
        match token.kind {
            TokenKind::LocalIdent(ref name) => {
                let dest = name.clone();
                self.expect(TokenKind::Equal)?;

                let token = self.next_token()?;
                let node = match token.kind {
                    TokenKind::Inst(ref inst) => match inst {
                        InstKind::Binary(op) => {
                            let kind = InstKind::Binary(op.clone());
                            let lhs = self.parse_operand()?;
                            self.expect(TokenKind::Comma)?;
                            let rhs: Box<AstNode> = self.parse_operand()?;
                            Inst::new_boxed(kind, Some(dest), vec![lhs, rhs])
                        }
                        InstKind::Unary(op) => {
                            let kind = InstKind::Unary(op.clone());
                            let operand = self.parse_operand()?;
                            Inst::new_boxed(kind, Some(dest), vec![operand])
                        }
                        InstKind::Load => {
                            let ty = self.parse_type()?;
                            self.expect(TokenKind::Comma)?;
                            let ptr = self.parse_operand()?;
                            Inst::new_boxed_load(dest, ty, ptr)
                        }
                        InstKind::Alloc => {
                            let ty = self.parse_type()?;
                            Inst::new_boxed_alloc(dest, ty)
                        }
                        InstKind::Call => {
                            let ty = self.parse_type()?;
                            let callee = self.parse_callee()?;
                            Inst::new_boxed_call(Some(dest), ty, callee)
                        }
                        InstKind::GetElemPtr => {
                            let ty = self.parse_type()?;
                            self.expect(TokenKind::Comma)?;
                            let ptr = self.parse_operand()?;
                            let mut operands = vec![ptr];

                            // parse indices
                            loop {
                                let parse_result = self.parse_operand();
                                match parse_result {
                                    Ok(node) => operands.push(node),
                                    Err(_) => match self.curr_token.kind {
                                        TokenKind::Comma => {
                                            self.consume();
                                            continue;
                                        }
                                        _ => break,
                                    },
                                }
                            }
                            Inst::new_boxed_getelemptr(dest, ty, operands)
                        }
                        _ => return Err(self.unexpected_token()),
                    },
                    _ => return Err(self.unexpected_token()),
                };
                Ok(node)
            }
            TokenKind::Inst(ref inst) => match inst {
                InstKind::Store => {
                    let val = self.parse_operand()?;
                    self.expect(TokenKind::Comma)?;
                    let ptr = self.parse_operand()?;
                    Ok(Inst::new_boxed(InstKind::Store, None, vec![val, ptr]))
                }
                InstKind::Jump => {
                    let dst = self.parse_callee()?;
                    Ok(Inst::new_boxed(InstKind::Jump, None, vec![dst]))
                }
                InstKind::Branch => {
                    let cond = self.parse_operand()?;
                    self.expect(TokenKind::Comma)?;
                    let then = self.parse_callee()?;
                    self.expect(TokenKind::Comma)?;
                    let else_ = self.parse_callee()?;
                    Ok(Inst::new_boxed(
                        InstKind::Branch,
                        None,
                        vec![cond, then, else_],
                    ))
                }
                InstKind::Call => {
                    let ty = self.parse_type()?;
                    let callee = self.parse_callee()?;
                    Ok(Inst::new_boxed_call(None, ty, callee))
                }
                InstKind::Return => {
                    let val = self.parse_operand();
                    let operands = if val.is_ok() { vec![val?] } else { Vec::new() };
                    Ok(ast::Inst::new_boxed(InstKind::Return, None, operands))
                }
                _ => Err(self.unexpected_token()),
            },
            _ => Err(self.unexpected_token()),
        }
    }

    /// Parse operand w/o parameters
    fn parse_operand(&mut self) -> Result<AstNodeBox, ParseError> {
        let ty = self.parse_type()?;
        let token = self.peek_token()?;
        let ident: Box<AstNode> = match token.kind {
            TokenKind::GlobalIdent(ref name) => AstNode::new_boxed_global_ident(name.clone()),
            TokenKind::LocalIdent(ref name) => AstNode::new_boxed_local_ident(name.clone()),
            TokenKind::Bytes(ref bytes) => AstNode::new_boxed_bytes(bytes.clone()),
            _ => return Err(self.unexpected_token()),
        };
        self.consume();
        Ok(Operand::new_boxed(ty, ident))
    }

    fn parse_function_decl(&mut self) -> Result<AstNodeBox, ParseError> {
        self.expect(TokenKind::Keyword(KeywordKind::Decl))?;
        let token = self.next_token()?;
        let name = match token.kind {
            TokenKind::GlobalIdent(ref name) => name.clone(),
            _ => return Err(self.unexpected_token()),
        };
        let ty = self.parse_type()?;
        Ok(FunctionDecl::new_boxed(name, ty))
    }

    fn parse_function_def(&mut self) -> Result<AstNodeBox, ParseError> {
        self.expect(TokenKind::Keyword(KeywordKind::Fn))?;
        let token = self.next_token()?;
        let name = match token.kind {
            TokenKind::GlobalIdent(ref name) => name.clone(),
            _ => return Err(self.unexpected_token()),
        };
        let ty = self.parse_type()?;
        self.expect(TokenKind::LeftBrace)?;
        let mut blocks = Vec::new();

        loop {
            let parse_result = self.parse_block();
            match parse_result {
                Ok(node) => blocks.push(node),
                Err(_) => match self.next_token()?.kind {
                    TokenKind::RightBrace => break,
                    _ => return Err(self.unexpected_token()),
                },
            }
        }

        Ok(FunctionDef::new_boxed(name, ty, blocks))
    }

    fn parse_callee(&mut self) -> Result<AstNodeBox, ParseError> {
        let token = self.peek_token()?;
        let name = match token.kind {
            TokenKind::GlobalIdent(ref name) => name.clone(),
            TokenKind::LabelIdent(ref name) => name.clone(),
            _ => return Err(self.unexpected_token()),
        };
        self.consume();

        let token = self.peek_token()?;
        match token.kind {
            TokenKind::LeftParen => {
                let mut params = Vec::new();
                self.consume();
                loop {
                    let parse_result = self.parse_operand();
                    match parse_result {
                        Ok(node) => params.push(node),
                        Err(_) => match self.next_token()?.kind {
                            TokenKind::RightParen => break,
                            TokenKind::Comma => continue,
                            _ => return Err(self.unexpected_token()),
                        },
                    }
                }
                let callee = Callee::new_boxed(name, params);
                Ok(callee)
            }
            _ => Ok(Callee::new_boxed(name, Vec::new())),
        }
    }
}

#[cfg(test)]
mod test {
    use std::io::Cursor;

    use super::Parser;

    #[test]
    fn test_parser0() {
        let mut buf = Cursor::new("#123\n fn @fib (i32) -> i32 { ^bb: ret i32 %n }     #123");
        let mut parser = Parser::new(&mut buf);
        let ast = parser.parse();
        println!("{:?}", ast)
    }

    #[test]
    fn test_parser1() {
        let mut buf = Cursor::new(
            r#"global @x = i32 0x10101010
const @y = i32 0x20202020
type $z = { i32, float }
global @array = [ i32; 3 ] [ 0x01, 0x02, 0x03 ]
global @arrarr = [ [float ;3]; 4] [ [0x1, 0x2, 0x3],[0x1, 0x2, 0x3],[0x1, 0x2, 0x3]]

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

^ret(i32 %result, float %123):
    ret i32 %result
}
"#,
        );
        let mut parser = Parser::new(&mut buf);
        let ast = parser.parse();
        println!("{:#?}", ast)
    }
}
