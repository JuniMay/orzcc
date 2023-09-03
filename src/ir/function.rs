use super::{layout::Layout, ty::Type};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Function(usize);

impl Function {
    pub fn new(index: usize) -> Self {
        Self(index)
    }

    pub fn index(self) -> usize {
        self.0
    }
}

/// Data of function
pub struct FunctionData {
    /// Name of the function
    pub name: String,
    /// Type of the function
    pub ty: Type,
    /// Layout of blocks and inst-block mapping in the function
    pub layout: Layout,
}
