use super::{types::Type, value::Constant};

/// Kind of constants
pub enum ConstantKind {
    /// Zero (initializer)
    Zero,
    /// Undefined
    Undef,
    /// Bytes of non-aggregated
    Bytes(Vec<u8>),
    /// An array
    Array(Vec<Constant>),
    /// A struct
    Struct(Vec<Constant>),
}

/// Data of the constant
pub struct ConstantData {
    /// Type of the constant
    pub ty: Type,
    /// Kind of the constant
    pub kind: ConstantKind,
}
