mod regs;

#[cfg(feature = "arm64")]
mod arm64;

#[cfg(feature = "arm32")]
mod arm32;

#[cfg(feature = "riscv64")]
mod riscv64;

pub use regs::{PReg, RegKind, VReg};
