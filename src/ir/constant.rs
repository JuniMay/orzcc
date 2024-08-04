use core::fmt;
use std::hash::Hash;

use super::source_loc::Span;
use crate::collections::apint::ApInt;

/// An integer constant, max 64 bits.
///
/// [ApInt] is too complex to be used as a constant. Simple integer constants
/// are represented with this struct.
#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct IntConstant(u64, u8);

impl IntConstant {
    fn mask(self) -> u64 {
        (1u64.checked_shl(self.1 as u32))
            .unwrap_or(0)
            .overflowing_sub(1)
            .0
    }

    pub fn into_apint(self) -> ApInt {
        let width = self.1;
        let value = self.0 & self.mask();
        let apint = ApInt::from(value);
        match width.cmp(&64) {
            std::cmp::Ordering::Less => apint.into_truncated(width as usize).0,
            std::cmp::Ordering::Equal => apint,
            std::cmp::Ordering::Greater => panic!("width is too large"),
        }
    }

    pub fn into_signext(self) -> Self {
        if self.1 == 1 {
            // do not signext a single bit, which is boolean
            return self;
        }
        let signed = self.0 as i64;
        let shamt = 64 - self.1;
        let value = (signed << shamt) >> shamt;
        Self(value as u64, 64)
    }

    pub fn resize(self, width: u8) -> Self {
        let mask = (1u64.checked_shl(width as u32))
            .unwrap_or(0)
            .overflowing_sub(1)
            .0;
        let value = self.0 & mask;
        Self(value, width)
    }

    pub fn zero(width: u8) -> Self { Self(0, width) }

    pub fn one(width: u8) -> Self { Self(1, width) }

    pub fn bits(&self) -> u64 { self.0 }

    pub fn is_zero(&self) -> bool { (self.0 & self.mask()) == 0 }

    pub fn is_one(&self) -> bool { (self.0 & self.mask()) == 1 }

    pub fn is_power_of_two(&self) -> bool { self.0.is_power_of_two() }

    pub fn trailing_zeros(&self) -> u32 { self.0.trailing_zeros().min(self.1 as u32) }

    pub fn as_signed(&self) -> i64 { self.into_signext().0 as i64 }
}

impl From<bool> for IntConstant {
    fn from(value: bool) -> Self { Self(value as u64, 1) }
}

impl From<u8> for IntConstant {
    fn from(value: u8) -> Self { Self(value as u64, 8) }
}

impl From<u16> for IntConstant {
    fn from(value: u16) -> Self { Self(value as u64, 16) }
}

impl From<u32> for IntConstant {
    fn from(value: u32) -> Self { Self(value as u64, 32) }
}

impl From<u64> for IntConstant {
    fn from(value: u64) -> Self {
        dbg!(value);
        Self(value, 64)
    }
}

impl From<i8> for IntConstant {
    fn from(value: i8) -> Self { Self(value as u64, 8) }
}

impl From<i16> for IntConstant {
    fn from(value: i16) -> Self { Self(value as u64, 16) }
}

impl From<i32> for IntConstant {
    fn from(value: i32) -> Self { Self(value as u64, 32) }
}

impl From<i64> for IntConstant {
    fn from(value: i64) -> Self { Self(value as u64, 64) }
}

impl TryFrom<ApInt> for IntConstant {
    type Error = &'static str;

    fn try_from(value: ApInt) -> Result<Self, Self::Error> {
        let width = value.width();
        if width > 64 {
            Err("width is too large")
        } else {
            let bits = u64::from(value);
            Ok(Self(bits, width as u8))
        }
    }
}

// display int constant requires masking, because there are signext constants

impl fmt::Display for IntConstant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}i{}", self.0 & self.mask(), self.1)
    }
}

impl fmt::LowerHex for IntConstant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#x}i{}", self.0 & self.mask(), self.1)
    }
}

impl fmt::UpperHex for IntConstant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:#X}i{}", self.0 & self.mask(), self.1)
    }
}

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

impl FloatConstant {
    /// Promote to a Float64, if it is a Float32.
    pub fn promote(self) -> Self {
        match self {
            FloatConstant::Float32(val) => {
                FloatConstant::Float64((f32::from_bits(val) as f64).to_bits())
            }
            FloatConstant::Float64(_) => self,
        }
    }

    pub fn truncate(self) -> Self {
        match self {
            FloatConstant::Float64(val) => {
                FloatConstant::Float32((f64::from_bits(val) as f32).to_bits())
            }
            FloatConstant::Float32(_) => self,
        }
    }

    pub fn width(&self) -> usize {
        match self {
            FloatConstant::Float32(_) => 32,
            FloatConstant::Float64(_) => 64,
        }
    }

    pub fn add(&mut self, other: &Self) -> Self {
        match (self, other) {
            (FloatConstant::Float32(a), FloatConstant::Float32(b)) => {
                (f32::from_bits(*a) + f32::from_bits(*b)).into()
            }
            (FloatConstant::Float64(a), FloatConstant::Float64(b)) => {
                (f64::from_bits(*a) + f64::from_bits(*b)).into()
            }
            _ => panic!("invalid float constant to add"),
        }
    }

    pub fn sub(&mut self, other: &Self) -> Self {
        match (self, other) {
            (FloatConstant::Float32(a), FloatConstant::Float32(b)) => {
                (f32::from_bits(*a) - f32::from_bits(*b)).into()
            }
            (FloatConstant::Float64(a), FloatConstant::Float64(b)) => {
                (f64::from_bits(*a) - f64::from_bits(*b)).into()
            }
            _ => panic!("invalid float constant to subtract"),
        }
    }

    pub fn mul(&mut self, other: &Self) -> Self {
        match (self, other) {
            (FloatConstant::Float32(a), FloatConstant::Float32(b)) => {
                (f32::from_bits(*a) * f32::from_bits(*b)).into()
            }
            (FloatConstant::Float64(a), FloatConstant::Float64(b)) => {
                (f64::from_bits(*a) * f64::from_bits(*b)).into()
            }
            _ => panic!("invalid float constant to multiply"),
        }
    }

    pub fn div(&mut self, other: &Self) -> Self {
        match (self, other) {
            (FloatConstant::Float32(a), FloatConstant::Float32(b)) => {
                (f32::from_bits(*a) / f32::from_bits(*b)).into()
            }
            (FloatConstant::Float64(a), FloatConstant::Float64(b)) => {
                (f64::from_bits(*a) / f64::from_bits(*b)).into()
            }
            _ => panic!("invalid float constant to divide"),
        }
    }

    pub fn rem(&mut self, other: &Self) -> Self {
        match (self, other) {
            (FloatConstant::Float32(a), FloatConstant::Float32(b)) => {
                f32::from_bits(*a).rem_euclid(f32::from_bits(*b)).into()
            }
            (FloatConstant::Float64(a), FloatConstant::Float64(b)) => {
                f64::from_bits(*a).rem_euclid(f64::from_bits(*b)).into()
            }
            _ => panic!("invalid float constant to remainder"),
        }
    }

    pub fn neg(&mut self) -> Self {
        match self {
            FloatConstant::Float32(a) => (-f32::from_bits(*a)).into(),
            FloatConstant::Float64(a) => (-f64::from_bits(*a)).into(),
        }
    }

    pub fn is_nan(&self) -> bool {
        match self {
            FloatConstant::Float32(val) => f32::from_bits(*val).is_nan(),
            FloatConstant::Float64(val) => f64::from_bits(*val).is_nan(),
        }
    }

    pub fn lt(&self, other: &Self) -> bool {
        match (self, other) {
            (FloatConstant::Float32(a), FloatConstant::Float32(b)) => {
                f32::from_bits(*a) < f32::from_bits(*b)
            }
            (FloatConstant::Float64(a), FloatConstant::Float64(b)) => {
                f64::from_bits(*a) < f64::from_bits(*b)
            }
            _ => panic!("invalid float constant to compare"),
        }
    }

    pub fn le(&self, other: &Self) -> bool {
        match (self, other) {
            (FloatConstant::Float32(a), FloatConstant::Float32(b)) => {
                f32::from_bits(*a) <= f32::from_bits(*b)
            }
            (FloatConstant::Float64(a), FloatConstant::Float64(b)) => {
                f64::from_bits(*a) <= f64::from_bits(*b)
            }
            _ => panic!("invalid float constant to compare"),
        }
    }
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

/// A global constant value kind in the IR.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantKind {
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

/// A global constant in IR.
///
/// For instructions like `iconst` or `fconst`, [ApInt] and [FloatConstant] are
/// used instead.
pub struct Constant(ConstantKind, Span);

impl PartialEq for Constant {
    fn eq(&self, other: &Self) -> bool { self.0 == other.0 }
}

impl Eq for Constant {}

impl Hash for Constant {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) { self.0.hash(state) }
}

impl fmt::Debug for Constant {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Constant({:?}, {:?})", self.0, self.1)
    }
}

impl Constant {
    /// Create an undefined constant.
    pub fn undef() -> Self { Self(ConstantKind::Undef, Span::default()) }

    /// Create a zero initialized constant.
    pub fn zeroinit() -> Self { Self(ConstantKind::Zeroinit, Span::default()) }

    /// Create a constant from bytes.
    ///
    /// If all bytes are zero, return [Constant::ZeroInit].
    pub fn bytes(bytes: Vec<u8>) -> Self {
        if bytes.iter().all(|&b| b == 0) {
            Self::zeroinit()
        } else {
            Self(ConstantKind::Bytes(bytes), Span::default())
        }
    }

    pub fn with_source_span(mut self, span: Span) -> Self {
        self.1 = span;
        self
    }

    pub fn source_span(&self) -> Span { self.1 }

    pub fn kind(&self) -> &ConstantKind { &self.0 }

    pub fn get_bytes(&self) -> Option<&Vec<u8>> {
        match self.kind() {
            ConstantKind::Bytes(bytes) => Some(bytes),
            ConstantKind::Undef | ConstantKind::Zeroinit => None,
        }
    }
}

impl From<bool> for Constant {
    fn from(value: bool) -> Self { Constant::bytes(vec![value as u8]) }
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
        use ConstantKind as Ck;
        match self.kind() {
            Ck::Undef => write!(f, "undef"),
            Ck::Zeroinit => write!(f, "zeroinit"),
            Ck::Bytes(bytes) => {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_constant_display() {
        let c = Constant::undef();
        assert_eq!(format!("{}", c), "undef");

        let c = Constant::zeroinit();
        assert_eq!(format!("{}", c), "zeroinit");

        let c = Constant::from(0x1234u16);
        assert_eq!(format!("{}", c), "[0x34, 0x12]");

        let c = Constant::from(0x12345678u32);
        assert_eq!(format!("{}", c), "[0x78, 0x56, 0x34, 0x12]");

        let apint =
            ApInt::try_from("0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef")
                .unwrap();
        let c = Constant::from(apint);

        assert_eq!(
            format!("{}", c),
            concat!(
                "[",
                "0xef, 0xcd, 0xab, 0x90, 0x78, 0x56, 0x34, 0x12, ",
                "0xef, 0xcd, 0xab, 0x90, 0x78, 0x56, 0x34, 0x12, ",
                "0xef, 0xcd, 0xab, 0x90, 0x78, 0x56, 0x34, 0x12, ",
                "0xef, 0xcd, 0xab, 0x90, 0x78, 0x56, 0x34, 0x12",
                "]"
            )
        )
    }
}
