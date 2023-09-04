use super::{types::Type, value::Constant};

/// Data of a global value
#[derive(Debug, Clone)]
pub struct GlobalData {
    /// Name of the value
    pub name: String,
    /// Type of the value
    pub ty: Type,
    /// Initializer of the value
    pub init: Constant,
    /// If the value is a constant
    pub mutable: bool,
}

impl GlobalData {
    pub fn new(name: String, ty: Type, init: Constant, mutable: bool) -> GlobalData {
        GlobalData {
            name,
            ty,
            init,
            mutable,
        }
    }
}
