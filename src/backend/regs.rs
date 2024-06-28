/// The kind of a register.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegKind {
    /// The general purpose register.
    General,
    /// The floating point register.
    Float,
    /// The vector register.
    Vector,
}

/// The physical register.
///
/// Cranelift uses a bit-encoded representation, but here just separate the
/// register number and the kind.
pub struct PReg(u8, RegKind);

impl PReg {
    pub fn new(num: u8, kind: RegKind) -> Self { Self(num, kind) }

    pub fn num(&self) -> u8 { self.0 }

    pub fn kind(&self) -> RegKind { self.1 }
}

/// The virtual register.
///
/// Let's hope the number of virtual registers does not exceed [u32::MAX].
pub struct VReg(u32, RegKind);

impl VReg {
    pub fn new(num: u32, kind: RegKind) -> Self { Self(num, kind) }

    pub fn num(&self) -> u32 { self.0 }

    pub fn kind(&self) -> RegKind { self.1 }
}
