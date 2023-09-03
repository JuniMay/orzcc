use super::{constant::Constant, ty::Type};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Global(usize);

impl Global {
    pub fn new(id: usize) -> Self {
        Self(id)
    }

    pub fn index(&self) -> usize {
        self.0
    }
}

/// Data of a global value
pub struct GlobalData {
    /// Type of the value
    pub ty: Type,
    /// Initializer of the value
    pub init: Constant,
    /// If the value is a constant
    pub mutable: bool,
}

impl GlobalData {
    pub fn new(ty: Type, init: Constant, mutable: bool) -> GlobalData {
        GlobalData { ty, init, mutable }
    }
}
