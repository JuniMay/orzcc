use core::fmt;

use crate::collections::apint::ApInt;

/// A float constant.
///
/// The integer in `iconst` can be represented with [ApInt], but for
/// floating-point, the complex semantics for aribtrary precision is not
/// supported yet, so just use a enum to represent the float constant.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum FloatConstant {
    Float32(u32),
    Float64(u64),
}

impl From<f32> for FloatConstant {
    fn from(f: f32) -> Self { Self::Float32(f.to_bits()) }
}

impl From<f64> for FloatConstant {
    fn from(f: f64) -> Self { Self::Float64(f.to_bits()) }
}

impl TryFrom<FloatConstant> for f32 {
    type Error = &'static str;

    fn try_from(value: FloatConstant) -> Result<Self, Self::Error> {
        match value {
            FloatConstant::Float32(val) => Ok(f32::from_bits(val)),
            _ => Err("constant is not a Float32"),
        }
    }
}

impl TryFrom<FloatConstant> for f64 {
    type Error = &'static str;

    fn try_from(value: FloatConstant) -> Result<Self, Self::Error> {
        match value {
            FloatConstant::Float64(val) => Ok(f64::from_bits(val)),
            _ => Err("constant is not a Float64"),
        }
    }
}

impl fmt::Display for FloatConstant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // output bits, in hex format
        match self {
            FloatConstant::Float32(val) => write!(f, "{}", f32::from_bits(*val)),
            FloatConstant::Float64(val) => write!(f, "{}", f64::from_bits(*val)),
        }
    }
}

impl fmt::LowerHex for FloatConstant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // output bits, in hex format
        match self {
            FloatConstant::Float32(val) => write!(f, "{:#x}", val),
            FloatConstant::Float64(val) => write!(f, "{:#x}", val),
        }
    }
}

/// A global constant value in the IR.
///
/// For instructions like `iconst` or `fconst`, [ApInt] and [FloatConstant] are
/// used instead.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constant {
    /// Undefined value.
    Undef,
    /// Zero initialized constant.
    ///
    /// This can only be used as initializers. For constants in instructions,
    /// use [Constant::Integer] instead.
    Zeroinit,
    /// Any other constants, in bytes.
    Bytes(Vec<u8>),
}

impl Constant {
    /// Create an undefined constant.
    pub fn undef() -> Self { Constant::Undef }

    /// Create a zero initialized constant.
    pub fn zeroinit() -> Self { Constant::Zeroinit }

    /// Create a constant from bytes.
    ///
    /// If all bytes are zero, return [Constant::ZeroInit].
    pub fn bytes(bytes: Vec<u8>) -> Self {
        if bytes.iter().all(|&b| b == 0) {
            Constant::Zeroinit
        } else {
            Constant::Bytes(bytes)
        }
    }
}

impl From<u8> for Constant {
    fn from(value: u8) -> Self { Constant::bytes(vec![value]) }
}

impl From<u16> for Constant {
    fn from(value: u16) -> Self { Constant::bytes(value.to_le_bytes().to_vec()) }
}

impl From<u32> for Constant {
    fn from(value: u32) -> Self { Constant::bytes(value.to_le_bytes().to_vec()) }
}

impl From<u64> for Constant {
    fn from(value: u64) -> Self { Constant::bytes(value.to_le_bytes().to_vec()) }
}

impl From<i8> for Constant {
    fn from(value: i8) -> Self { Constant::bytes(vec![value as u8]) }
}

impl From<i16> for Constant {
    fn from(value: i16) -> Self { Constant::bytes(value.to_le_bytes().to_vec()) }
}

impl From<i32> for Constant {
    fn from(value: i32) -> Self { Constant::bytes(value.to_le_bytes().to_vec()) }
}

impl From<i64> for Constant {
    fn from(value: i64) -> Self { Constant::bytes(value.to_le_bytes().to_vec()) }
}

impl From<f32> for Constant {
    fn from(value: f32) -> Self { Constant::bytes(value.to_le_bytes().to_vec()) }
}

impl From<f64> for Constant {
    fn from(value: f64) -> Self { Constant::bytes(value.to_le_bytes().to_vec()) }
}

impl From<&[u8]> for Constant {
    fn from(value: &[u8]) -> Self { Constant::bytes(value.to_vec()) }
}

impl From<&[u16]> for Constant {
    fn from(value: &[u16]) -> Self {
        let mut bytes = Vec::with_capacity(value.len() * 2);
        for &v in value {
            bytes.extend_from_slice(&v.to_le_bytes());
        }
        Constant::bytes(bytes)
    }
}

impl From<&[u32]> for Constant {
    fn from(value: &[u32]) -> Self {
        let mut bytes = Vec::with_capacity(value.len() * 4);
        for &v in value {
            bytes.extend_from_slice(&v.to_le_bytes());
        }
        Constant::bytes(bytes)
    }
}

impl From<&[u64]> for Constant {
    fn from(value: &[u64]) -> Self {
        let mut bytes = Vec::with_capacity(value.len() * 8);
        for &v in value {
            bytes.extend_from_slice(&v.to_le_bytes());
        }
        Constant::bytes(bytes)
    }
}

impl From<ApInt> for Constant {
    fn from(value: ApInt) -> Self {
        let bytes = value.to_le_bytes();
        Constant::bytes(bytes)
    }
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Constant as C;
        match self {
            C::Undef => write!(f, "undef"),
            C::Zeroinit => write!(f, "zeroinit"),
            C::Bytes(bytes) => {
                // format: [ 0x00, 0x01, ... ]
                write!(f, "[")?;
                for (i, byte) in bytes.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "0x{:02x}", byte)?;
                }
                write!(f, "]")
            }
        }
    }
}
