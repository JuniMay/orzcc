use core::fmt;

use crate::collections::apint::ApInt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Imm12(u16);

impl Imm12 {
    pub fn as_i16(&self) -> i16 {
        // signext
        (self.0 << 4) as i16 >> 4
    }

    pub fn try_from_i64(x: i64) -> Option<Self> {
        if (-2048..=2047).contains(&x) {
            Some(Imm12(x as u16 & 0xfff))
        } else {
            None
        }
    }

    pub fn try_from_u64(x: u64) -> Option<Self> { Self::try_from_i64(x as i64) }

    pub fn try_from_apint(x: ApInt) -> Option<Self> {
        // we need to check the absolute value of x
        let (x, _) = x.into_abs();
        let x = x.into_shrunk();
        // the highest bit must be zero, so less equal to 11
        if x.width() <= 11 {
            Some(Imm12(u16::from(x)))
        } else {
            None
        }
    }

    pub fn bits(&self) -> u16 { self.0 }
}

impl PartialOrd for Imm12 {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.as_i16().partial_cmp(&other.as_i16())
    }
}

impl fmt::Display for Imm12 {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result { write!(f, "{}", self.as_i16()) }
}
