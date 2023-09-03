use super::ty::Type;

/// Reference to a constant
///
/// The field is corresponding to the value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Constant(usize);

impl Constant {
    pub fn new(index: usize) -> Self {
        Constant(index)
    }

    /// Get the index of the constant
    pub fn index(&self) -> usize {
        self.0
    }
}

/// Kind of constants
pub enum ConstantKind {
    /// Zero (initializer)
    Zero,
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
