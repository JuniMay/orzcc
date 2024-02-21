//! # Parser
//!
//! ```ebnf
//! INT -> r"i[0-9]+"
//! GLOBAL_IDENT -> r"@[a-zA-Z_][a-zA-Z0-9_]*"
//! LOCAL_IDENT -> r"%[a-zA-Z_][a-zA-Z0-9_]*"
//! TYPE_IDENT -> r"\$[a-zA-Z_][a-zA-Z0-9_]*"
//! LABEL_IDENT -> r"\^[a-zA-Z_][a-zA-Z0-9_]*"
//! DECIMAL -> r"[0-9]+"
//! HEX -> r"0x[0-9a-fA-F]+"
//! NUMBER -> DECIMAL | HEX
//!
//! TYPE_LIST -> TYPE , TYPE_LIST | TYPE
//! FN_TYPE -> ( TYPE_LIST ) "->" TYPE
//! STRUCT_TYPE -> { TYPE_LIST }
//! ARRAY -> [ TYPE ; NUMBER ]
//! TYPE -> ARRAY | STRUCT | INT | half | float | double | ptr | void | TYPE_IDENT
//!
//! VALUE_ELEM -> NUMBER | ARRAY_INIT | STRUCT_INIT
//! VALUE_ELEMS -> VALUE_ELEM , VALUE_ELEMS | VALUE_ELEM
//! ARRAY_INIT -> [ VALUE_ELEMS ]
//! STRUCT_INIT -> { VALUE_ELEMS }
//! GLOBAL_INIT -> ARRAY_INIT | STRUCT_INIT | NUMBER
//! GLOBAL -> (global | const) GLOBAL_IDENT = TYPE GLOBAL_INIT
//!
//! OPERAND -> TYPE (GLOBAL_IDENT | LOCAL_IDENT | NUMBER)
//!
//! BINRAY_INST -> LOCAL_IDENT = BINARY_OP OPERAND , OPERAND
//! UNARY_INST -> LOCAL_IDENT = UNARY_OP OPERAND
//! LOAD_INST -> LOCAL_IDENT = load TYPE , OPERAND
//! STORE_INST -> store OPERAND , OPERAND
//! PARAM_LIST -> TYPE LOCAL_IDENT , PARAM_LIST | TYPE LOCAL_IDENT
//! LABEL -> LABEL_IDENT ( PARAM_LIST ) :
//! ```

use std::{
    borrow::BorrowMut,
    io::{self},
};

use crate::ir::types::Type;

use super::{
    ast::{self, Array, Ast, AstNode, AstNodeBox, Global, InstKind, Operand, Struct, TypeDef},
    lexer::Lexer,
    tokens::{Inst, Keyword, Pos, Span, Token, TokenKind},
};

pub struct Parser<'a, T>
where
    T: io::Read,
{
    lexer: Lexer<'a, T>,
    /// Current token
    curr_token: Token,
    /// Peeked
    peeked: bool,
}

pub enum ParseError {
    LexerError,
    UnexpectedToken,
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

    fn next_token(&mut self) -> Result<&Token, ParseError> {
        if self.peeked {
            self.peeked = false;
            return Ok(&self.curr_token);
        }
        let token = self.lexer.next_token().ok_or(ParseError::LexerError)?;
        self.curr_token = token;
        Ok(&self.curr_token)
    }

    fn peek_token(&mut self) -> Result<&Token, ParseError> {
        if self.peeked {
            return Ok(&self.curr_token);
        }
        let token = self.lexer.next_token().ok_or(ParseError::LexerError)?;
        self.curr_token = token;
        self.peeked = true;
        Ok(&self.curr_token)
    }

    fn eat(&mut self) {
        self.peeked = false;
    }

    pub fn parse(&mut self) -> Result<Ast, ParseError> {
        let _ = self.next_token()?;

        let mut ast = Ast::new();

        loop {
            let item = self.parse_item()?;
            ast.push(item);
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
                Keyword::Type => self.parse_type_def(),
                Keyword::Global => self.parse_global(true),
                Keyword::Const => self.parse_global(false),
                Keyword::Fn => self.parse_function_def(),
                Keyword::Decl => self.parse_function_decl(),
                _ => Err(ParseError::UnexpectedToken),
            },
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<(), ParseError> {
        if self.curr_token.kind == kind {
            let _ = self.next_token()?;
            Ok(())
        } else {
            Err(ParseError::UnexpectedToken)
        }
    }

    fn parse_type_def(&mut self) -> Result<AstNodeBox, ParseError> {
        self.expect(TokenKind::Keyword(Keyword::Type))?;
        let token = self.next_token()?;
        let name = match token.kind {
            TokenKind::TypeIdent(ref name) => name.clone(),
            _ => return Err(ParseError::UnexpectedToken),
        };
        self.expect(TokenKind::Equal)?;
        let ty = self.parse_type()?;
        Ok(TypeDef::new_boxed(name, ty))
    }

    fn parse_global(&mut self, mutable: bool) -> Result<AstNodeBox, ParseError> {
        self.expect(TokenKind::Keyword(Keyword::Global))?;
        let token = self.next_token()?;
        let name = match token.kind {
            TokenKind::GlobalIdent(ref name) => name.clone(),
            _ => return Err(ParseError::UnexpectedToken),
        };
        self.expect(TokenKind::Equal)?;
        let ty = self.parse_type()?;
        let init = self.parse_global_init()?;
        Ok(Global::new_boxed(mutable, name, ty, init))
    }

    fn parse_global_init(&mut self) -> Result<AstNodeBox, ParseError> {
        let token = self.peek_token()?;
        match token.kind {
            TokenKind::LeftBracket => self.parse_array(),
            TokenKind::LeftBrace => self.parse_struct(),
            TokenKind::Bytes(_) => self.parse_bytes(),
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    fn parse_bytes(&mut self) -> Result<AstNodeBox, ParseError> {
        if let TokenKind::Bytes(ref bytes) = self.next_token()?.kind {
            Ok(AstNode::new_boxed_bytes(bytes.clone()))
        } else {
            Err(ParseError::UnexpectedToken)
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
                    _ => return Err(ParseError::UnexpectedToken),
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
                    _ => return Err(ParseError::UnexpectedToken),
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
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    fn parse_primitive_type(&mut self) -> Result<Type, ParseError> {
        let token = self.next_token()?;
        match token.kind {
            TokenKind::Keyword(ref kw) => match kw {
                Keyword::Int(n) => Ok(Type::mk_int(*n)),
                Keyword::Half => Ok(Type::mk_half()),
                Keyword::Float => Ok(Type::mk_float()),
                Keyword::Double => Ok(Type::mk_double()),
                Keyword::Ptr => Ok(Type::mk_ptr()),
                Keyword::Void => Ok(Type::mk_void()),
                _ => Err(ParseError::UnexpectedToken),
            },
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    fn parse_type_ident(&mut self) -> Result<Type, ParseError> {
        match self.next_token()?.kind {
            TokenKind::TypeIdent(ref name) => Ok(Type::mk_type(name.clone())),
            _ => Err(ParseError::UnexpectedToken),
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
                    _ => return Err(ParseError::UnexpectedToken),
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
            return Err(ParseError::UnexpectedToken);
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
                    _ => return Err(ParseError::UnexpectedToken),
                },
            }
        }
        self.expect(TokenKind::Arrow)?;
        let ret = self.parse_type()?;
        Ok(Type::mk_function(params, ret))
    }

    fn parse_block(&mut self) -> Result<AstNodeBox, ParseError> {
        let token = self.next_token()?;
        let name = if let TokenKind::LabelIdent(ref name) = token.kind {
            name.clone()
        } else {
            return Err(ParseError::UnexpectedToken);
        };

        let token = self.next_token()?;
        match token.kind {
            TokenKind::LeftParen => {
                todo!("parse block params");
            }
            TokenKind::Colon => {
                todo!("parse block body");
            }
            _ => return Err(ParseError::UnexpectedToken),
        }
    }

    fn parse_inst(&mut self) -> Result<AstNodeBox, ParseError> {
        let token = self.next_token()?;
        match token.kind {
            TokenKind::LocalIdent(ref name) => {
                let dest = AstNode::new_boxed_local_ident(name.clone());
                self.expect(TokenKind::Equal)?;

                let token = self.next_token()?;
                let node = match token.kind {
                    TokenKind::Inst(ref inst) => match inst {
                        Inst::Binary(op) => {
                            let kind = InstKind::Binary(op.clone());
                            let lhs = self.parse_operand()?;
                            self.expect(TokenKind::Comma)?;
                            let rhs = self.parse_operand()?;
                            let node = ast::Inst::new_boxed(kind, Some(dest), vec![lhs, rhs]);
                            node
                        }
                        Inst::Unary(op) => {
                            let kind = InstKind::Unary(op.clone());
                            let operand = self.parse_operand()?;
                            let node = ast::Inst::new_boxed(kind, Some(dest), vec![operand]);
                            node
                        }
                        Inst::Load => {
                            let ty = self.parse_type()?;
                            self.expect(TokenKind::Comma)?;
                            let ptr = self.parse_operand()?;
                            let node = ast::Inst::new_boxed_load(dest, ty, ptr);
                            node
                        }
                        Inst::Alloc => {
                            let ty = self.parse_type()?;
                            let node = ast::Inst::new_boxed_alloc(dest, ty);
                            node
                        }
                        Inst::Call => {
                            let ty = self.parse_type()?;
                            let callee = self.parse_operand()?;
                            let node = ast::Inst::new_boxed_call(Some(dest), ty, callee);
                            node
                        }
                        Inst::GetElemPtr => {
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
                                            self.eat();
                                            continue;
                                        }
                                        _ => break,
                                    },
                                }
                            }
                            let node = ast::Inst::new_boxed_gep(dest, ty, operands);
                            node
                        }
                        _ => return Err(ParseError::UnexpectedToken),
                    },
                    _ => return Err(ParseError::UnexpectedToken),
                };

                todo!()
            }
            TokenKind::Inst(ref inst) => match inst {
                Inst::Store => {
                    let val = self.parse_operand()?;
                    self.expect(TokenKind::Comma)?;
                    let ptr = self.parse_operand()?;
                    Ok(ast::Inst::new_boxed(InstKind::Store, None, vec![val, ptr]))
                }
                Inst::Jump => {
                    let dst = self.parse_operand()?;
                    Ok(ast::Inst::new_boxed(InstKind::Jump, None, vec![dst]))
                }
                Inst::Branch => {
                    let then = self.parse_operand()?;
                    self.expect(TokenKind::Comma)?;
                    let else_ = self.parse_operand()?;
                    Ok(ast::Inst::new_boxed(
                        InstKind::Branch,
                        None,
                        vec![then, else_],
                    ))
                }
                Inst::Call => {
                    let ty = self.parse_type()?;
                    let callee = self.parse_operand()?;
                    Ok(ast::Inst::new_boxed_call(None, ty, callee))
                }
                Inst::Return => {
                    let val = self.parse_operand()?;
                    Ok(ast::Inst::new_boxed(InstKind::Return, None, vec![val]))
                }
                _ => Err(ParseError::UnexpectedToken),
            },
            _ => Err(ParseError::UnexpectedToken),
        }
    }

    /// Parse operand w/o parameters
    fn parse_raw_operand(&mut self) -> Result<AstNodeBox, ParseError> {
        let ty = self.parse_type()?;
        let token = self.peek_token()?;
        let ident: Box<AstNode> = match token.kind {
            TokenKind::GlobalIdent(ref name) => AstNode::new_boxed_global_ident(name.clone()),
            TokenKind::LocalIdent(ref name) => AstNode::new_boxed_local_ident(name.clone()),
            TokenKind::Bytes(ref bytes) => AstNode::new_boxed_bytes(bytes.clone()),
            _ => return Err(ParseError::UnexpectedToken),
        };
        self.eat();
        Ok(Operand::new_boxed_raw(ty, ident))
    }

    fn parse_function_decl(&mut self) -> Result<AstNodeBox, ParseError> {
        todo!()
    }

    fn parse_function_def(&mut self) -> Result<AstNodeBox, ParseError> {
        todo!()
    }

    fn parse_operand(&mut self) -> Result<AstNodeBox, ParseError> {
        let mut operand = self.parse_raw_operand()?;

        let token = self.peek_token()?;
        match token.kind {
            TokenKind::LeftParen => {
                let mut params = Vec::new();
                loop {
                    let parse_result = self.parse_raw_operand();
                    match parse_result {
                        Ok(node) => params.push(node),
                        Err(_) => match self.curr_token.kind {
                            TokenKind::RightParen => break,
                            TokenKind::Comma => continue,
                            _ => return Err(ParseError::UnexpectedToken),
                        },
                    }
                }
                self.expect(TokenKind::RightParen)?;
                let operand: &mut AstNode = operand.borrow_mut();
                if let AstNode::Operand(ref mut operand) = operand {
                    operand.params_mut().extend(params);
                } else {
                    unreachable!();
                }
            }
            _ => {}
        }

        Ok(operand)
    }
}
