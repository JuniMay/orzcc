mod block;
mod context;
mod func;
mod inst;
mod lower;
mod regs;

#[cfg(feature = "target-arm64")]
mod arm64;

#[cfg(feature = "target-arm32")]
mod arm32;

#[cfg(feature = "target-riscv64")]
mod riscv64;

pub use regs::{PReg, RegKind, VReg};
