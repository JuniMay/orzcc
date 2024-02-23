use std::io::{self};

use thiserror::Error;

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

#[derive(Debug, Error)]
pub enum ParseError {
    /// Lexer error
    #[error("lexer error at {0}")]
    LexerError(Pos),

    /// Unexpected token
    #[error("unexpected token at {0}")]
    UnexpectedToken(Span),
}

impl<'a, T> Parser<'a, T>
where
    T: io::Read,
{
    pub fn new(input: &'a mut T) -> Self {
        Self {
            lexer: Lexer::new(input),
            curr_token: Token::new(Span::default(), TokenKind::Eof),
            peeked: false,
        }
    }

    /// Get the next token and comsume it.
    fn next_token(&mut self) -> Result<&Token, ParseError> {
        if self.peeked {
            self.peeked = false;
            return Ok(&self.curr_token);
        }
        let token = self
            .lexer
            .next_token()
            .ok_or(ParseError::LexerError(self.lexer.curr_pos()))?;
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
        let token = self
            .lexer
            .next_token()
            .ok_or(ParseError::LexerError(self.lexer.curr_pos()))?;
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
        ParseError::UnexpectedToken(self.curr_token.span)
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
                KeywordKind::Func => self.parse_function_def(),
                KeywordKind::Decl => self.parse_function_decl(),
                _ => Err(self.unexpected_token()),
            },
            _ => Err(self.unexpected_token()),
        }
    }

    fn parse_type_def(&mut self) -> Result<AstNodeBox, ParseError> {
        self.expect(TokenKind::Keyword(KeywordKind::Type))?;
        let mut span = self.curr_token.span;
        let token = self.next_token()?;
        let name = match token.kind {
            TokenKind::TypeIdent(ref name) => name.clone(),
            _ => return Err(self.unexpected_token()),
        };
        self.expect(TokenKind::Equal)?;
        let ty = self.parse_type()?;
        span.extend(&self.curr_token.span);
        Ok(TypeDef::new_boxed(name, ty, span))
    }

    fn parse_global(&mut self, mutable: bool) -> Result<AstNodeBox, ParseError> {
        if mutable {
            self.expect(TokenKind::Keyword(KeywordKind::Global))?;
        } else {
            self.expect(TokenKind::Keyword(KeywordKind::Const))?;
        }
        let mut span = self.curr_token.span;
        let token = self.next_token()?;
        let name = match token.kind {
            TokenKind::GlobalIdent(ref name) => name.clone(),
            _ => return Err(self.unexpected_token()),
        };
        self.expect(TokenKind::Equal)?;
        let ty = self.parse_type()?;
        let init = self.parse_global_init()?;
        span.extend(&self.curr_token.span);
        Ok(GlobalDef::new_boxed(mutable, name, ty, init, span))
    }

    fn parse_global_init(&mut self) -> Result<AstNodeBox, ParseError> {
        let token = self.peek_token()?;
        match token.kind {
            TokenKind::LeftBracket => self.parse_array(),
            TokenKind::LeftBrace => self.parse_struct(),
            TokenKind::Bytes(_) => self.parse_bytes(),
            TokenKind::Keyword(KeywordKind::Zero) => self.parse_zero(),
            TokenKind::Keyword(KeywordKind::Undef) => self.parse_undef(),
            _ => Err(self.unexpected_token()),
        }
    }

    fn parse_zero(&mut self) -> Result<AstNodeBox, ParseError> {
        self.expect(TokenKind::Keyword(KeywordKind::Zero))?;
        Ok(AstNode::new_boxed_zero(self.curr_token.span))
    }

    fn parse_undef(&mut self) -> Result<AstNodeBox, ParseError> {
        self.expect(TokenKind::Keyword(KeywordKind::Undef))?;
        Ok(AstNode::new_boxed_undef(self.curr_token.span))
    }

    fn parse_bytes(&mut self) -> Result<AstNodeBox, ParseError> {
        let token = self.next_token()?;
        if let TokenKind::Bytes(ref bytes) = token.kind {
            Ok(AstNode::new_boxed_bytes(bytes.clone(), token.span))
        } else {
            Err(self.unexpected_token())
        }
    }

    fn parse_array(&mut self) -> Result<AstNodeBox, ParseError> {
        self.expect(TokenKind::LeftBracket)?;
        let mut span = self.curr_token.span;
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
        span.extend(&self.curr_token.span);
        let node = Array::new_boxed(elems, span);
        Ok(node)
    }

    fn parse_struct(&mut self) -> Result<AstNodeBox, ParseError> {
        self.expect(TokenKind::LeftBrace)?;
        let mut span = self.curr_token.span;
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
        span.extend(&self.curr_token.span);
        let node = Struct::new_boxed(fields, span);
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
            TokenKind::TypeIdent(ref name) => Ok(Type::mk_identified(name.clone())),
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
        let mut span = token.span;
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
                span.extend(&self.curr_token.span);
                let insts = self.parse_block_body()?;
                let node = Block::new_boxed(name, params, insts, span);
                Ok(node)
            }
            TokenKind::Colon => {
                let insts = self.parse_block_body()?;
                let node = Block::new_boxed(name, Vec::new(), insts, span);
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
                let mut span = token.span;
                self.expect(TokenKind::Equal)?;

                let token = self.next_token()?;
                let node = match token.kind {
                    TokenKind::Inst(ref inst) => match inst {
                        InstKind::Binary(op) => {
                            let kind = InstKind::Binary(op.clone());
                            let lhs = self.parse_operand()?;
                            self.expect(TokenKind::Comma)?;
                            let rhs: Box<AstNode> = self.parse_operand()?;
                            span.extend(&self.curr_token.span);
                            Inst::new_boxed(kind, Some(dest), vec![lhs, rhs], span)
                        }
                        InstKind::Unary(op) => {
                            let kind = InstKind::Unary(op.clone());
                            let operand = self.parse_operand()?;
                            span.extend(&self.curr_token.span);
                            Inst::new_boxed(kind, Some(dest), vec![operand], span)
                        }
                        InstKind::Load => {
                            let ty = self.parse_type()?;
                            self.expect(TokenKind::Comma)?;
                            let ptr = self.parse_operand()?;
                            span.extend(&self.curr_token.span);
                            Inst::new_boxed_load(dest, ty, ptr, span)
                        }
                        InstKind::Cast(op) => {
                            let op = op.clone();
                            let ty = self.parse_type()?;
                            self.expect(TokenKind::Comma)?;
                            let val = self.parse_operand()?;
                            span.extend(&self.curr_token.span);
                            Inst::new_boxed_cast(dest, op, ty, val, span)
                        }
                        InstKind::Alloc => {
                            let ty = self.parse_type()?;
                            span.extend(&self.curr_token.span);
                            Inst::new_boxed_alloc(dest, ty, span)
                        }
                        InstKind::Call => {
                            let ty = self.parse_type()?;
                            let callee = self.parse_callee()?;
                            span.extend(&self.curr_token.span);
                            Inst::new_boxed_call(Some(dest), ty, callee, span)
                        }
                        InstKind::GetElemPtr => {
                            let ty = self.parse_type()?;
                            self.expect(TokenKind::Comma)?;
                            let ptr = self.parse_operand()?;
                            self.expect(TokenKind::Comma)?;
                            let mut operands = vec![ptr];

                            // parse indices
                            loop {
                                let node = self.parse_operand()?;
                                operands.push(node);
                                let token = self.peek_token()?;
                                if let TokenKind::Comma = token.kind {
                                    self.consume();
                                } else {
                                    break;
                                }
                            }
                            span.extend(&self.curr_token.span);
                            Inst::new_boxed_getelemptr(dest, ty, operands, span)
                        }
                        _ => return Err(self.unexpected_token()),
                    },
                    _ => return Err(self.unexpected_token()),
                };
                Ok(node)
            }
            TokenKind::Inst(ref inst) => match inst {
                InstKind::Store => {
                    let mut span = token.span;
                    let val = self.parse_operand()?;
                    self.expect(TokenKind::Comma)?;
                    let ptr = self.parse_operand()?;
                    span.extend(&self.curr_token.span);
                    Ok(Inst::new_boxed(InstKind::Store, None, vec![val, ptr], span))
                }
                InstKind::Jump => {
                    let mut span = token.span;
                    let dst = self.parse_callee()?;
                    span.extend(&self.curr_token.span);
                    Ok(Inst::new_boxed(InstKind::Jump, None, vec![dst], span))
                }
                InstKind::Branch => {
                    let mut span = token.span;
                    let cond = self.parse_operand()?;
                    self.expect(TokenKind::Comma)?;
                    let then = self.parse_callee()?;
                    self.expect(TokenKind::Comma)?;
                    let else_ = self.parse_callee()?;
                    span.extend(&self.curr_token.span);
                    Ok(Inst::new_boxed(
                        InstKind::Branch,
                        None,
                        vec![cond, then, else_],
                        span,
                    ))
                }
                InstKind::Call => {
                    let mut span = token.span;
                    let ty = self.parse_type()?;
                    let callee = self.parse_callee()?;
                    span.extend(&self.curr_token.span);
                    Ok(Inst::new_boxed_call(None, ty, callee, span))
                }
                InstKind::Return => {
                    let mut span = token.span;
                    let val = self.parse_operand();
                    if val.is_ok() {
                        span.extend(&self.curr_token.span);
                    }
                    let operands = if val.is_ok() { vec![val?] } else { Vec::new() };
                    Ok(ast::Inst::new_boxed(InstKind::Return, None, operands, span))
                }
                _ => Err(self.unexpected_token()),
            },
            _ => Err(self.unexpected_token()),
        }
    }

    /// Parse operand w/o parameters
    fn parse_operand(&mut self) -> Result<AstNodeBox, ParseError> {
        let token = self.peek_token()?;
        let mut span = token.span;
        let ty = self.parse_type().ok();
        let token = self.peek_token()?;
        let ident: Box<AstNode> = match token.kind {
            TokenKind::GlobalIdent(ref name) => {
                AstNode::new_boxed_global_ident(name.clone(), token.span)
            }
            TokenKind::LocalIdent(ref name) => {
                AstNode::new_boxed_local_ident(name.clone(), token.span)
            }
            TokenKind::Bytes(ref bytes) => AstNode::new_boxed_bytes(bytes.clone(), token.span),
            TokenKind::Keyword(KeywordKind::Zero) => AstNode::new_boxed_zero(token.span),
            TokenKind::Keyword(KeywordKind::Undef) => AstNode::new_boxed_undef(token.span),
            _ => return Err(self.unexpected_token()),
        };
        span.extend(&self.curr_token.span);
        self.consume();
        Ok(Operand::new_boxed(ty, ident, span))
    }

    fn parse_function_decl(&mut self) -> Result<AstNodeBox, ParseError> {
        self.expect(TokenKind::Keyword(KeywordKind::Decl))?;
        let mut span = self.curr_token.span;
        let token = self.next_token()?;
        let name = match token.kind {
            TokenKind::GlobalIdent(ref name) => name.clone(),
            _ => return Err(self.unexpected_token()),
        };
        let ty = self.parse_type()?;
        span.extend(&self.curr_token.span);
        Ok(FunctionDecl::new_boxed(name, ty, span))
    }

    fn parse_function_def(&mut self) -> Result<AstNodeBox, ParseError> {
        self.expect(TokenKind::Keyword(KeywordKind::Func))?;
        let mut span = self.curr_token.span;
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
        span.extend(&self.curr_token.span);
        Ok(FunctionDef::new_boxed(name, ty, blocks, span))
    }

    fn parse_callee(&mut self) -> Result<AstNodeBox, ParseError> {
        let token = self.peek_token()?;
        let name = match token.kind {
            TokenKind::GlobalIdent(ref name) => name.clone(),
            TokenKind::LabelIdent(ref name) => name.clone(),
            _ => return Err(self.unexpected_token()),
        };
        let mut span = token.span;
        self.consume();

        let token = self.peek_token()?;
        match token.kind {
            TokenKind::LeftParen => {
                let mut params = Vec::new();
                span.extend(&token.span);
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
                span.extend(&self.curr_token.span);
                let callee = Callee::new_boxed(name, params, span);
                Ok(callee)
            }
            _ => Ok(Callee::new_boxed(name, Vec::new(), span)),
        }
    }
}
