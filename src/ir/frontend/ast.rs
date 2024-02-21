use crate::ir::{
    types::Type,
    values::{BinaryOp, UnaryOp},
};

use super::InstKind;

/// An abstract syntax tree.
#[derive(Debug)]
pub struct Ast {
    items: Vec<AstNodeBox>,
}

impl Ast {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    /// Push an item to the abstract syntax tree.
    pub fn push(&mut self, item: AstNodeBox) {
        self.items.push(item);
    }
}

pub type AstNodeBox = Box<AstNode>;

/// An abstract syntax tree node.
#[derive(Debug)]
pub enum AstNode {
    /// Type definition
    TypeDef(TypeDef),

    /// Global/Cosntant definition
    GlobalDef(GlobalDef),

    /// Function declaration
    FunctionDecl(FunctionDecl),

    /// Function definition
    FunctionDef(FunctionDef),

    /// Block
    Block(Block),

    /// Instruction
    Inst(Inst),

    /// Array value
    Array(Array),

    /// Struct value
    Struct(Struct),

    /// Bytes (a literal number)
    Bytes(Vec<u8>),

    /// Global identifier
    GlobalIdent(String),

    /// Label identifier
    LabelIdent(String),

    /// Type identifier
    TypeIdent(String),

    /// Local identifier
    LocalIdent(String),

    /// A typed operand
    Operand(Operand),

    /// A callee (block or function)
    Callee(Callee),
}

/// The callee with args of a block or function.
#[derive(Debug)]
pub struct Callee {
    /// Name of the callee.
    name: String,

    /// Arguments of the callee.
    args: Vec<AstNodeBox>,
}

impl Callee {
    pub fn new_boxed(name: String, args: Vec<AstNodeBox>) -> AstNodeBox {
        Box::new(AstNode::Callee(Callee { name, args }))
    }
}

/// The operand of an instruction, with type.
#[derive(Debug)]
pub struct Operand {
    /// The type of the operand.
    ty: Type,

    /// The value of the operand.
    value: AstNodeBox,
}

impl Operand {
    pub fn new_boxed(ty: Type, value: AstNodeBox) -> AstNodeBox {
        Box::new(AstNode::Operand(Operand { ty, value }))
    }
}

impl AstNode {
    pub fn new_boxed_global_ident(name: String) -> AstNodeBox {
        Box::new(AstNode::GlobalIdent(name))
    }

    pub fn new_boxed_local_ident(name: String) -> AstNodeBox {
        Box::new(AstNode::LocalIdent(name))
    }

    pub fn new_boxed_bytes(bytes: Vec<u8>) -> AstNodeBox {
        Box::new(AstNode::Bytes(bytes))
    }
}

#[derive(Debug)]
pub struct TypeDef {
    /// The name of the custom type.
    name: String,

    /// The actual type.
    ty: Type,
}

impl TypeDef {
    pub fn new_boxed(name: String, ty: Type) -> AstNodeBox {
        Box::new(AstNode::TypeDef(TypeDef { name, ty }))
    }
}

#[derive(Debug)]
pub struct GlobalDef {
    /// Whether this is a constant or not.
    mutable: bool,

    /// Name of the global variable.
    name: String,

    /// The type of the global variable.
    ty: Type,

    /// The initial value of the global variable.
    init: AstNodeBox,
}

impl GlobalDef {
    pub fn new_boxed(mutable: bool, name: String, ty: Type, init: AstNodeBox) -> AstNodeBox {
        Box::new(AstNode::GlobalDef(GlobalDef {
            mutable,
            name,
            ty,
            init,
        }))
    }
}

#[derive(Debug)]
pub struct FunctionDecl {
    /// Name of the function.
    name: String,

    /// The type of the function.
    ty: Type,
}

impl FunctionDecl {
    pub fn new_boxed(name: String, ty: Type) -> AstNodeBox {
        Box::new(AstNode::FunctionDecl(FunctionDecl { name, ty }))
    }
}

#[derive(Debug)]
pub struct FunctionDef {
    /// Name of the function.
    name: String,

    /// The type of the function.
    ty: Type,

    /// The blocks of the function.
    blocks: Vec<AstNodeBox>,
}

impl FunctionDef {
    pub fn new_boxed(name: String, ty: Type, blocks: Vec<AstNodeBox>) -> AstNodeBox {
        Box::new(AstNode::FunctionDef(FunctionDef { name, ty, blocks }))
    }
}

#[derive(Debug)]
pub struct Block {
    /// Name of the block.
    name: String,

    /// Parameters of the block.
    params: Vec<AstNodeBox>,

    /// Instructions of the block.
    insts: Vec<AstNodeBox>,
}

impl Block {
    pub fn new_boxed(name: String, params: Vec<AstNodeBox>, insts: Vec<AstNodeBox>) -> AstNodeBox {
        Box::new(AstNode::Block(Block {
            name,
            params,
            insts,
        }))
    }
}

#[derive(Debug)]
pub struct Inst {
    /// Kind of the instruction.
    kind: InstKind,

    /// destination of the instruction
    dest: Option<AstNodeBox>,

    /// The operands of the instruction
    operands: Vec<AstNodeBox>,

    /// The type for alloc/load/call/gep instructions
    ty: Option<Type>,
}

impl Inst {
    pub(super) fn new_boxed(
        kind: InstKind,
        dest: Option<AstNodeBox>,
        operands: Vec<AstNodeBox>,
    ) -> AstNodeBox {
        Box::new(AstNode::Inst(Inst {
            kind,
            dest,
            operands,
            ty: None,
        }))
    }

    pub(super) fn new_boxed_alloc(dest: AstNodeBox, ty: Type) -> AstNodeBox {
        Box::new(AstNode::Inst(Inst {
            kind: InstKind::Alloc,
            dest: Some(dest),
            operands: Vec::new(),
            ty: Some(ty),
        }))
    }

    pub(super) fn new_boxed_load(dest: AstNodeBox, ty: Type, ptr: AstNodeBox) -> AstNodeBox {
        Box::new(AstNode::Inst(Inst {
            kind: InstKind::Load,
            dest: None,
            operands: vec![ptr],
            ty: Some(ty),
        }))
    }

    pub(super) fn new_boxed_call(
        dest: Option<AstNodeBox>,
        ty: Type,
        callee: AstNodeBox,
    ) -> AstNodeBox {
        Box::new(AstNode::Inst(Inst {
            kind: InstKind::Call,
            dest,
            operands: vec![callee],
            ty: Some(ty),
        }))
    }

    pub(super) fn new_boxed_getelemptr(
        dest: AstNodeBox,
        ty: Type,
        operands: Vec<AstNodeBox>,
    ) -> AstNodeBox {
        Box::new(AstNode::Inst(Inst {
            kind: InstKind::GetElemPtr,
            dest: Some(dest),
            operands,
            ty: Some(ty),
        }))
    }
}

#[derive(Debug)]
pub struct Array {
    elems: Vec<AstNodeBox>,
}

impl Array {
    pub fn new_boxed(elems: Vec<AstNodeBox>) -> AstNodeBox {
        Box::new(AstNode::Array(Array { elems }))
    }
}

#[derive(Debug)]
pub struct Struct {
    fields: Vec<AstNodeBox>,
}

impl Struct {
    pub fn new_boxed(fields: Vec<AstNodeBox>) -> AstNodeBox {
        Box::new(AstNode::Struct(Struct { fields }))
    }
}
