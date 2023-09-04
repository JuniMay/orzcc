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

impl ConstantData {
    pub fn mk_zero(ty: Type) -> ConstantData {
        Self {
            ty,
            kind: ConstantKind::Zero,
        }
    }

    pub fn mk_undef(ty: Type) -> ConstantData {
        Self {
            ty,
            kind: ConstantKind::Undef,
        }
    }

    pub fn mk_bytes(ty: Type, bytes: Vec<u8>) -> ConstantData {
        Self {
            ty,
            kind: ConstantKind::Bytes(bytes),
        }
    }

    pub fn mk_array(ty: Type, elems: Vec<Constant>) -> ConstantData {
        Self {
            ty,
            kind: ConstantKind::Array(elems),
        }
    }

    pub fn mk_struct(ty: Type, fields: Vec<Constant>) -> ConstantData {
        Self {
            ty,
            kind: ConstantKind::Struct(fields),
        }
    }
}
