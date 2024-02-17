use super::{
    layout::Layout,
    module::DataFlowGraph,
    types::Type,
    values::{
        Alloc, Binary, Branch, Call, GetElemPtr, GlobalSlot, Jump, Load, Return, Store, Unary,
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
    pub fn new(name: String, ty: Type, kind: FunctionKind) -> Self {
        let dfg = DataFlowGraph::new();
        let layout = Layout::new();

        Self {
            name,
            ty,
            kind,
            dfg,
            layout,
        }
    }

    /// Create a new `ValueData` struct for the function
    pub fn new_value_data(&self) -> ValueData {
        ValueData::new(self.ty.clone(), ValueKind::Function)
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub fn ty(&self) -> &Type {
        &self.ty
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

    /// Bytes for non-aggregate types, little-endian
    Bytes(Vec<u8>),

    /// Array constant
    Array(Vec<Value>),

    /// Struct constant
    Struct(Vec<Value>),

    /// A global memory slot
    ///
    /// A global value is actually a memory location(pointer) to the global variable.
    GlobalSlot(GlobalSlot),

    /// Alloc
    ///
    /// A stack/local allocation.
    Alloc(Alloc),

    /// Load
    Load(Load),

    /// Store
    Store(Store),

    /// Binary instruction
    ///
    /// Binary instructions include arithmetic, logical, bitwise and comparison operations.
    Binary(Binary),

    /// Unary instruction
    ///
    /// Unary instructions only include fneg for now.
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

    /// Block parameter
    BlockParam,

    /// Function
    ///
    /// Functions are also global values. The data and instructions are stored in `FunctionData`
    Function,
}

impl ValueKind {
    pub fn is_const(&self) -> bool {
        match self {
            ValueKind::Zero
            | ValueKind::Undef
            | ValueKind::Bytes(_)
            | ValueKind::Array(_)
            | ValueKind::Struct(_) => true,
            _ => false,
        }
    }

    pub fn is_global(&self) -> bool {
        matches!(self, ValueKind::GlobalSlot(_) | ValueKind::Function)
    }

    pub fn is_global_slot(&self) -> bool {
        matches!(self, ValueKind::GlobalSlot(_))
    }

    pub fn is_terminator(&self) -> bool {
        matches!(
            self,
            ValueKind::Jump(_) | ValueKind::Branch(_) | ValueKind::Return(_)
        )
    }

    pub fn is_block_param(&self) -> bool {
        matches!(self, ValueKind::BlockParam)
    }
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

    pub fn kind(&self) -> &ValueKind {
        &self.kind
    }
}
