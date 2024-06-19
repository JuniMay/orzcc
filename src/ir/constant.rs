use super::{Context, Ty};
use crate::collections::apint::ApInt;

/// A constant value in the IR.
///
/// Constant can be used to fold instructions in the IR.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constant {
    /// Undefined value.
    Undef(Ty),
    /// An arbitrary precision integer.
    Integer(ApInt),
    /// A floating point number.
    ///
    /// The [f32] in rust does not implement equality comparison or hash, so we
    /// use the raw bits to compare them.
    Float32(u32),
    /// A double precision floating point number.
    ///
    /// Same as [Constant::Float32], we use the raw bits to compare them.
    Float64(u64),
    /// An array of constant values.
    Array(Vec<Constant>),
    /// A struct of constant values.
    Struct(Vec<Constant>, bool),
    /// A SIMD constant.
    Simd(Vec<Constant>),
}

impl Constant {
    /// Get the type of the constant.
    pub fn get_ty(&self, ctx: &mut Context) -> Ty {
        match self {
            Constant::Undef(ty) => *ty,
            Constant::Integer(apint) => Ty::int(ctx, apint.width() as u16),
            Constant::Float32(_) => Ty::float32(ctx),
            Constant::Float64(_) => Ty::float64(ctx),
            Constant::Array(values) => {
                let elem_ty = values.first().unwrap().get_ty(ctx);
                Ty::array(ctx, elem_ty, values.len())
            }
            Constant::Struct(values, is_packed) => {
                let field_tys = values.iter().map(|val| val.get_ty(ctx)).collect();
                Ty::struct_(ctx, field_tys, *is_packed)
            }
            Constant::Simd(values) => {
                let elem_ty = values.first().unwrap().get_ty(ctx);
                Ty::simd(ctx, elem_ty, values.len())
            }
        }
    }

    /// Get the constant as an [f32] number.
    ///
    /// # Returns
    ///
    /// The constant as an [f32] number if it is a [Constant::Float32].
    ///
    /// - `Some(f32)` if the constant is a [Constant::Float32].
    /// - `None` if the constant is not a [Constant::Float32].
    pub fn as_f32(&self) -> Option<f32> {
        #[allow(clippy::wildcard_enum_match_arm)]
        match self {
            Constant::Float32(val) => Some(f32::from_bits(*val)),
            _ => None,
        }
    }

    /// Get the constant as an [f64] number.
    ///
    /// # Returns
    ///
    /// The constant as an [f64] number if it is a [Constant::Float64].
    ///
    /// - `Some(f64)` if the constant is a [Constant::Float64].
    /// - `None` if the constant is not a [Constant::Float64].
    pub fn as_f64(&self) -> Option<f64> {
        #[allow(clippy::wildcard_enum_match_arm)]
        match self {
            Constant::Float64(val) => Some(f64::from_bits(*val)),
            _ => None,
        }
    }
}

impl From<ApInt> for Constant {
    fn from(apint: ApInt) -> Self { Constant::Integer(apint) }
}

impl From<f32> for Constant {
    fn from(val: f32) -> Self { Constant::Float32(val.to_bits()) }
}

impl From<f64> for Constant {
    fn from(val: f64) -> Self { Constant::Float64(val.to_bits()) }
}

impl TryFrom<Constant> for ApInt {
    type Error = &'static str;

    fn try_from(constant: Constant) -> Result<Self, Self::Error> {
        #[allow(clippy::wildcard_enum_match_arm)]
        match constant {
            Constant::Integer(apint) => Ok(apint),
            _ => Err("trying to convert a non-integer constant to ApInt"),
        }
    }
}

impl TryFrom<Constant> for f32 {
    type Error = &'static str;

    fn try_from(constant: Constant) -> Result<Self, Self::Error> {
        #[allow(clippy::wildcard_enum_match_arm)]
        match constant {
            Constant::Float32(val) => Ok(f32::from_bits(val)),
            _ => Err("trying to convert a non-float32 constant to f32"),
        }
    }
}

impl TryFrom<Constant> for f64 {
    type Error = &'static str;

    fn try_from(constant: Constant) -> Result<Self, Self::Error> {
        #[allow(clippy::wildcard_enum_match_arm)]
        match constant {
            Constant::Float64(val) => Ok(f64::from_bits(val)),
            _ => Err("trying to convert a non-float64 constant to f64"),
        }
    }
}
