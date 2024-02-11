use super::{
    layout::Layout,
    module::DataFlowGraph,
    types::Type,
    values::{
        Alloc, Binary, Block, Branch, Call, GetElemPtr, Global, Jump, Load, Return, Store, Unary,
        Value,
    },
};

/// Data of the block
pub struct BlockData {
    /// Params of the block
    params: Vec<Value>,
}

impl BlockData {
    pub fn new(params: Vec<Value>) -> Self {
        Self { params }
    }

    pub fn params(&self) -> &[Value] {
        &self.params
    }

    pub fn params_mut(&mut self) -> &mut Vec<Value> {
        &mut self.params
    }
}

pub enum FunctionKind {
    /// Function with definition
    Definition,
    /// Declaration
    Declaration,
    /// Intrinsic
    Intrinsic,
}

/// Data of function.
pub struct FunctionData {
    /// Name of the function.
    name: String,
    /// Type of the function.
    ty: Type,
    /// The kind of the function
    kind: FunctionKind,
    /// Data flow graph
    dfg: DataFlowGraph,
    /// Layout in the function
    layout: Layout,
}

impl FunctionData {
    pub fn new(
        name: String,
        ty: Type,
        kind: FunctionKind,
        dfg: DataFlowGraph,
        layout: Layout,
    ) -> Self {
        Self {
            name,
            ty,
            kind,
            dfg,
            layout,
        }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn name_mut(&mut self) -> &mut String {
        &mut self.name
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn ty_mut(&mut self) -> &mut Type {
        &mut self.ty
    }

    pub fn kind(&self) -> &FunctionKind {
        &self.kind
    }

    pub fn dfg(&self) -> &DataFlowGraph {
        &self.dfg
    }

    pub fn dfg_mut(&mut self) -> &mut DataFlowGraph {
        &mut self.dfg
    }

    pub fn layout(&self) -> &Layout {
        &self.layout
    }

    pub fn layout_mut(&mut self) -> &mut Layout {
        &mut self.layout
    }
}

/// Kinds of value.
pub enum ValueKind {
    /// Zero initializer
    Zero,
    /// Undef
    Undef,
    /// Bytes for non-aggregate types
    Bytes(Vec<u8>),
    /// Array constant
    Array(Vec<Value>),
    /// Struct constant
    Struct(Vec<Value>),
    /// A Global
    Global(Global),
    /// Alloc
    Alloc(Alloc),
    /// Load
    Load(Load),
    /// Store
    Store(Store),
    /// Binary instruction
    Binary(Binary),
    /// Unary instruction
    Unary(Unary),
    /// Jump
    Jump(Jump),
    /// Branch
    Branch(Branch),
    /// Return
    Return(Return),
    /// Call
    Call(Call),
    /// Get element pointer
    GetElemPtr(GetElemPtr),
}

/// Data of a value
///
/// ValueData can be indexed by a Value.
pub struct ValueData {
    /// Type of the value
    ty: Type,
    /// Kind of the value
    kind: ValueKind,
}

impl ValueData {
    pub fn new(ty: Type, kind: ValueKind) -> Self {
        Self { ty, kind }
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }

    pub fn ty_mut(&mut self) -> &mut Type {
        &mut self.ty
    }

    pub fn kind(&self) -> &ValueKind {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut ValueKind {
        &mut self.kind
    }
}
