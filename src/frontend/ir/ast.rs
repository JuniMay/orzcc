use crate::ir::{Constant, InstKind, Signature, Span, Symbol, Ty};

#[derive(Debug)]
pub struct ValueRef {
    pub name: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct BlockRef {
    pub name: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct SuccRef {
    pub block: BlockRef,
    pub args: Vec<ValueRef>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsingInst {
    pub results: Vec<ValueRef>,
    pub kind: InstKind,
    pub operands: Vec<ValueRef>,
    pub successors: Vec<SuccRef>,
    pub result_tys: Vec<(Ty, Span)>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsingBlock {
    pub block: BlockRef,
    pub params: Vec<(ValueRef, (Ty, Span))>,
    pub insts: Vec<ParsingInst>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsingFunc {
    pub name: Symbol,
    pub sig: Signature,
    pub blocks: Vec<ParsingBlock>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsingDecl {
    pub name: Symbol,
    pub sig: Signature,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsingSlot {
    pub name: Symbol,
    pub ty: (Ty, Span),
    pub init: Constant,
    pub span: Span,
}

#[derive(Debug)]
pub enum Item {
    Func(ParsingFunc),
    Decl(ParsingDecl),
    Slot(ParsingSlot),
}
