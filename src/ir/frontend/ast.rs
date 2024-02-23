use crate::ir::{types::Type, values::CastOp};

use super::{tokens::Span, InstKind};

/// An abstract syntax tree.
#[derive(Debug)]
pub struct Ast {
    pub(super) items: Vec<AstNodeBox>,
}

impl Default for Ast {
    fn default() -> Self {
        Self::new()
    }
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
pub struct AstNode {
    pub(super) span: Option<Span>,
    pub(super) kind: AstNodeKind,
}

impl AstNode {
    pub fn new_boxed(kind: AstNodeKind) -> AstNodeBox {
        Box::new(AstNode { span: None, kind })
    }
}

impl AstNode {
    pub fn new_boxed_global_ident(name: String, span: Span) -> AstNodeBox {
        let mut node = AstNode::new_boxed(AstNodeKind::GlobalIdent(name));
        node.span = Some(span);
        node
    }

    pub fn new_boxed_local_ident(name: String, span: Span) -> AstNodeBox {
        let mut node = AstNode::new_boxed(AstNodeKind::LocalIdent(name));
        node.span = Some(span);
        node
    }

    pub fn new_boxed_bytes(bytes: Vec<u8>, span: Span) -> AstNodeBox {
        let mut node = AstNode::new_boxed(AstNodeKind::Bytes(bytes));
        node.span = Some(span);
        node
    }

    pub fn new_boxed_zero(span: Span) -> AstNodeBox {
        let mut node = AstNode::new_boxed(AstNodeKind::Zero);
        node.span = Some(span);
        node
    }

    pub fn new_boxed_undef(span: Span) -> AstNodeBox {
        let mut node = AstNode::new_boxed(AstNodeKind::Undef);
        node.span = Some(span);
        node
    }
}

#[derive(Debug)]
pub enum AstNodeKind {
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

    /// Zero
    Zero,

    /// Undef
    Undef,

    /// A typed operand
    Operand(Operand),

    /// A callee (block or function)
    Callee(Callee),
}

/// The callee with args of a block or function.
#[derive(Debug)]
pub struct Callee {
    /// Name of the callee.
    pub name: String,

    /// Arguments of the callee.
    pub args: Vec<AstNodeBox>,
}

impl Callee {
    pub fn new_boxed(name: String, args: Vec<AstNodeBox>, span: Span) -> AstNodeBox {
        let mut node = AstNode::new_boxed(AstNodeKind::Callee(Callee { name, args }));
        node.span = Some(span);
        node
    }
}

/// The operand of an instruction, with type.
#[derive(Debug)]
pub struct Operand {
    /// The type of the operand.
    pub ty: Option<Type>,

    /// The value of the operand.
    pub value: AstNodeBox,
}

impl Operand {
    pub fn new_boxed(ty: Option<Type>, value: AstNodeBox, span: Span) -> AstNodeBox {
        let mut node =  AstNode::new_boxed(AstNodeKind::Operand(Operand { ty, value }));
        node.span = Some(span);
        node
    }
}
#[derive(Debug)]
pub struct TypeDef {
    /// The name of the identified type.
    pub name: String,

    /// The actual type.
    pub ty: Type,
}

impl TypeDef {
    pub fn new_boxed(name: String, ty: Type) -> AstNodeBox {
        AstNode::new_boxed(AstNodeKind::TypeDef(TypeDef { name, ty }))
    }
}

#[derive(Debug)]
pub struct GlobalDef {
    /// Whether this is a constant or not.
    pub mutable: bool,

    /// Name of the global variable.
    pub name: String,

    /// The type of the global variable.
    pub ty: Type,

    /// The initial value of the global variable.
    pub init: AstNodeBox,
}

impl GlobalDef {
    pub fn new_boxed(mutable: bool, name: String, ty: Type, init: AstNodeBox) -> AstNodeBox {
        AstNode::new_boxed(AstNodeKind::GlobalDef(GlobalDef {
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
    pub name: String,

    /// The type of the function.
    pub ty: Type,
}

impl FunctionDecl {
    pub fn new_boxed(name: String, ty: Type, span: Span) -> AstNodeBox {
        let mut node = AstNode::new_boxed(AstNodeKind::FunctionDecl(FunctionDecl { name, ty }));
        node.span = Some(span);
        node
    }
}

#[derive(Debug)]
pub struct FunctionDef {
    /// Name of the function.
    pub name: String,

    /// The type of the function.
    pub ty: Type,

    /// The blocks of the function.
    pub blocks: Vec<AstNodeBox>,
}

impl FunctionDef {
    pub fn new_boxed(name: String, ty: Type, blocks: Vec<AstNodeBox>, span: Span) -> AstNodeBox {
        let mut node =
            AstNode::new_boxed(AstNodeKind::FunctionDef(FunctionDef { name, ty, blocks }));
        node.span = Some(span);
        node
    }
}

#[derive(Debug)]
pub struct Block {
    /// Name of the block.
    pub name: String,

    /// Parameters of the block.
    pub params: Vec<(Type, String)>,

    /// Instructions of the block.
    pub insts: Vec<AstNodeBox>,
}

impl Block {
    pub fn new_boxed(
        name: String,
        params: Vec<(Type, String)>,
        insts: Vec<AstNodeBox>,
    ) -> AstNodeBox {
        AstNode::new_boxed(AstNodeKind::Block(Block {
            name,
            params,
            insts,
        }))
    }
}

#[derive(Debug)]
pub struct Inst {
    /// Kind of the instruction.
    pub kind: InstKind,

    /// destination of the instruction
    pub dest: Option<String>,

    /// The operands of the instruction
    pub operands: Vec<AstNodeBox>,

    /// The type for alloc/load/call/gep instructions
    pub ty: Option<Type>,
}

impl Inst {
    pub(super) fn new_boxed(
        kind: InstKind,
        dest: Option<String>,
        operands: Vec<AstNodeBox>,
        span: Span,
    ) -> AstNodeBox {
        let mut node = AstNode::new_boxed(AstNodeKind::Inst(Inst {
            kind,
            dest,
            operands,
            ty: None,
        }));
        node.span = Some(span);
        node
    }

    pub(super) fn new_boxed_alloc(dest: String, ty: Type, span: Span) -> AstNodeBox {
        let mut node = AstNode::new_boxed(AstNodeKind::Inst(Inst {
            kind: InstKind::Alloc,
            dest: Some(dest),
            operands: Vec::new(),
            ty: Some(ty),
        }));
        node.span = Some(span);
        node
    }

    pub(super) fn new_boxed_load(dest: String, ty: Type, ptr: AstNodeBox, span: Span) -> AstNodeBox {
        let mut node = AstNode::new_boxed(AstNodeKind::Inst(Inst {
            kind: InstKind::Load,
            dest: Some(dest),
            operands: vec![ptr],
            ty: Some(ty),
        }));
        node.span = Some(span);
        node
    }

    pub(super) fn new_boxed_cast(
        dest: String,
        op: CastOp,
        ty: Type,
        val: AstNodeBox,
        span: Span,
    ) -> AstNodeBox {
        let mut node = AstNode::new_boxed(AstNodeKind::Inst(Inst {
            kind: InstKind::Cast(op),
            dest: Some(dest),
            operands: vec![val],
            ty: Some(ty),
        }));
        node.span = Some(span);
        node
    }

    pub(super) fn new_boxed_call(dest: Option<String>, ty: Type, callee: AstNodeBox, span: Span) -> AstNodeBox {
        let mut node = AstNode::new_boxed(AstNodeKind::Inst(Inst {
            kind: InstKind::Call,
            dest,
            operands: vec![callee],
            ty: Some(ty),
        }));
        node.span = Some(span);
        node
    }

    pub(super) fn new_boxed_getelemptr(
        dest: String,
        ty: Type,
        operands: Vec<AstNodeBox>,
        span: Span,
    ) -> AstNodeBox {
        let mut node = AstNode::new_boxed(AstNodeKind::Inst(Inst {
            kind: InstKind::GetElemPtr,
            dest: Some(dest),
            operands,
            ty: Some(ty),
        }));
        node.span = Some(span);
        node
    }
}

#[derive(Debug)]
pub struct Array {
    pub elems: Vec<AstNodeBox>,
}

impl Array {
    pub fn new_boxed(elems: Vec<AstNodeBox>) -> AstNodeBox {
        AstNode::new_boxed(AstNodeKind::Array(Array { elems }))
    }
}

#[derive(Debug)]
pub struct Struct {
    pub fields: Vec<AstNodeBox>,
}

impl Struct {
    pub fn new_boxed(fields: Vec<AstNodeBox>) -> AstNodeBox {
        AstNode::new_boxed(AstNodeKind::Struct(Struct { fields }))
    }
}
