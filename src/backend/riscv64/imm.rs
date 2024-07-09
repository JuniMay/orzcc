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

    pub fn try_from_apint(x: &ApInt) -> Option<Self> {
        // we need to consider several cases:
        // 1. x's width is smaller or equal to 12, then we just take the lower 12 bits
        // 2. x's width is larger than 12, we require the apint is msb extended
        //   1. bits 12.. are all zeros, then we just take the lower 12 bits
        //   2. bits 12.. are all ones, then we just take the lower 12 bits
        //   3. otherwise, we return None

        if x.width() <= 12 {
            Some(Imm12(u16::from(x.clone())))
        } else {
            let (abs, sign) = x.clone().into_abs();

            let abs = abs.into_shrunk();

            if abs.width() <= 11 || (abs.width() == 12 && u16::from(abs) == 0x800 && sign) {
                let bits = x.clone().into_truncated(12).0;
                Some(Imm12(u16::from(bits)))
            } else {
                None
            }
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn imm12_try_from_i64() {
        assert_eq!(Imm12::try_from_i64(0), Some(Imm12(0)));
        assert_eq!(Imm12::try_from_i64(1), Some(Imm12(1)));
        assert_eq!(Imm12::try_from_i64(2047), Some(Imm12(2047)));
        assert_eq!(
            Imm12::try_from_i64(-2048),
            Some(Imm12((-2048i16) as u16 & 0xfff))
        );
        assert_eq!(Imm12::try_from_i64(-2049), None);
        assert_eq!(Imm12::try_from_i64(2048), None);
    }

    #[test]
    fn imm12_try_from_u64() {
        assert_eq!(Imm12::try_from_u64(0), Some(Imm12(0)));
        assert_eq!(Imm12::try_from_u64(1), Some(Imm12(1)));
        assert_eq!(Imm12::try_from_u64(2047), Some(Imm12(2047)));
        assert_eq!(Imm12::try_from_u64(2048), None);
    }

    #[test]
    fn imm12_try_from_apint() {
        let x = ApInt::from(0);
        assert_eq!(Imm12::try_from_apint(&x), Some(Imm12(0)));
        assert_eq!(Imm12::try_from_apint(&x).unwrap().as_i16(), 0);

        let x = ApInt::from(1);
        assert_eq!(Imm12::try_from_apint(&x), Some(Imm12(1)));
        assert_eq!(Imm12::try_from_apint(&x).unwrap().as_i16(), 1);

        let x = ApInt::from(2047);
        assert_eq!(Imm12::try_from_apint(&x), Some(Imm12(2047)));
        assert_eq!(Imm12::try_from_apint(&x).unwrap().as_i16(), 2047);

        let x = ApInt::from(-2047);
        assert_eq!(
            Imm12::try_from_apint(&x),
            Some(Imm12((-2047i16) as u16 & 0xfff))
        );
        assert_eq!(Imm12::try_from_apint(&x).unwrap().as_i16(), -2047);

        let x = ApInt::from(-2048);
        dbg!(&x);
        assert_eq!(
            Imm12::try_from_apint(&x),
            Some(Imm12((-2048i16) as u16 & 0xfff))
        );
        assert_eq!(Imm12::try_from_apint(&x).unwrap().as_i16(), -2048);

        let x = ApInt::from(-2049);
        assert_eq!(Imm12::try_from_apint(&x), None);
    }
}
