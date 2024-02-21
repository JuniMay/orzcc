use crate::ir::{
    types::Type,
    values::{BinaryOp, UnaryOp},
};

pub struct Ast {
    items: Vec<AstNodeBox>,
}

impl Ast {
    pub fn new() -> Self {
        Self { items: Vec::new() }
    }

    pub fn push(&mut self, item: AstNodeBox) {
        self.items.push(item);
    }
}

pub type AstNodeBox = Box<AstNode>;

pub enum AstNode {
    TypeDef(TypeDef),
    Global(Global),
    FunctionDecl(FunctionDecl),
    FunctionDef(FunctionDef),
    Block(Block),
    Inst(Inst),
    Array(Array),
    Struct(Struct),
    Bytes(Vec<u8>),
    GlobalIdent(String),
    LabelIdent(String),
    TypeIdent(String),
    LocalIdent(String),
    Operand(Operand),
}

pub struct Operand {
    ty: Type,
    value: AstNodeBox,
    params: Vec<AstNodeBox>,
}

impl Operand {
    pub fn new_boxed(ty: Type, value: AstNodeBox, params: Vec<AstNodeBox>) -> AstNodeBox {
        Box::new(AstNode::Operand(Operand { ty, value, params }))
    }

    pub fn new_boxed_raw(ty: Type, value: AstNodeBox) -> AstNodeBox {
        Box::new(AstNode::Operand(Operand {
            ty,
            value,
            params: Vec::new(),
        }))
    }

    pub fn params_mut(&mut self) -> &mut Vec<AstNodeBox> {
        &mut self.params
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn value(&self) -> &AstNodeBox {
        &self.value
    }
}

impl AstNode {
    pub fn new_boxed_global_ident(name: String) -> AstNodeBox {
        Box::new(AstNode::GlobalIdent(name))
    }

    pub fn new_boxed_label_ident(name: String) -> AstNodeBox {
        Box::new(AstNode::LabelIdent(name))
    }

    pub fn new_boxed_type_ident(name: String) -> AstNodeBox {
        Box::new(AstNode::TypeIdent(name))
    }

    pub fn new_boxed_local_ident(name: String) -> AstNodeBox {
        Box::new(AstNode::LocalIdent(name))
    }

    pub fn new_boxed_bytes(bytes: Vec<u8>) -> AstNodeBox {
        Box::new(AstNode::Bytes(bytes))
    }
}

pub struct TypeDef {
    name: String,
    ty: Type,
}

impl TypeDef {
    pub fn new_boxed(name: String, ty: Type) -> AstNodeBox {
        Box::new(AstNode::TypeDef(TypeDef { name, ty }))
    }
}

pub struct Global {
    mutable: bool,
    name: String,
    ty: Type,
    init: AstNodeBox,
}

impl Global {
    pub fn new_boxed(mutable: bool, name: String, ty: Type, init: AstNodeBox) -> AstNodeBox {
        Box::new(AstNode::Global(Global {
            mutable,
            name,
            ty,
            init,
        }))
    }
}

pub struct FunctionDecl {
    name: String,
    ty: Type,
}

pub struct FunctionDef {
    name: String,
    ty: Type,
    blocks: Vec<AstNodeBox>,
}

pub struct Block {
    name: String,
    params: Vec<(Type, AstNodeBox)>,
    insts: Vec<AstNodeBox>,
}

pub enum InstKind {
    Binary(BinaryOp),
    Unary(UnaryOp),
    Store,
    Load,
    Alloc,
    Jump,
    Branch,
    Return,
    Call,
    GetElemPtr,
}

pub struct Inst {
    /// Kind of the instruction.
    kind: InstKind,

    /// destination of the instruction
    dest: Option<AstNodeBox>,

    /// The operands of the instruction
    operands: Vec<AstNodeBox>,

    /// The alloc/load/call type
    ty: Option<Type>,
}

impl Inst {
    pub fn new_boxed(
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

    pub fn new_boxed_alloc(dest: AstNodeBox, ty: Type) -> AstNodeBox {
        Box::new(AstNode::Inst(Inst {
            kind: InstKind::Alloc,
            dest: Some(dest),
            operands: Vec::new(),
            ty: Some(ty),
        }))
    }

    pub fn new_boxed_load(dest: AstNodeBox, ty: Type, ptr: AstNodeBox) -> AstNodeBox {
        Box::new(AstNode::Inst(Inst {
            kind: InstKind::Load,
            dest: None,
            operands: vec![ptr],
            ty: Some(ty),
        }))
    }

    pub fn new_boxed_call(dest: Option<AstNodeBox>, ty: Type, callee: AstNodeBox) -> AstNodeBox {
        Box::new(AstNode::Inst(Inst {
            kind: InstKind::Call,
            dest,
            operands: vec![callee],
            ty: Some(ty),
        }))
    }

    pub fn new_boxed_gep(dest: AstNodeBox, ty: Type, operands: Vec<AstNodeBox>) -> AstNodeBox {
        Box::new(AstNode::Inst(Inst {
            kind: InstKind::GetElemPtr,
            dest: Some(dest),
            operands,
            ty: Some(ty),
        }))
    }
}

pub struct Array {
    elems: Vec<AstNodeBox>,
}

impl Array {
    pub fn new_boxed(elems: Vec<AstNodeBox>) -> AstNodeBox {
        Box::new(AstNode::Array(Array { elems }))
    }
}

pub struct Struct {
    fields: Vec<AstNodeBox>,
}

impl Struct {
    pub fn new_boxed(fields: Vec<AstNodeBox>) -> AstNodeBox {
        Box::new(AstNode::Struct(Struct { fields }))
    }
}
