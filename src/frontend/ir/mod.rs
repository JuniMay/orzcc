use crate::ir::{source_loc::Span, Constant, InstKind, Signature, Symbol, Ty};

pub struct ValueRef {
    pub name: String,
    pub span: Span,
}

pub struct BlockRef {
    pub name: String,
    pub span: Span,
}

pub struct SuccRef {
    pub block: BlockRef,
    pub args: Vec<ValueRef>,
    pub span: Span,
}

pub struct ParsingInst {
    pub results: Vec<ValueRef>,
    pub kind: InstKind,
    pub operands: Vec<ValueRef>,
    pub successors: Vec<SuccRef>,
    pub span: Span,
}

pub struct ParsingBlock {
    pub block: BlockRef,
    pub params: Vec<(ValueRef, (Ty, Span))>,
    pub insts: Vec<ParsingInst>,
    pub span: Span,
}

pub struct ParsingFunc {
    pub name: Symbol,
    pub sig: Signature,
    pub blocks: Vec<ParsingBlock>,
    pub span: Span,
}

pub struct ParsingGlobalSlot {
    pub name: Symbol,
    pub ty: (Ty, Span),
    pub constant: Constant,
}
