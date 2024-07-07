mod block;
mod context;
mod func;
mod inst;
mod lower;
mod regs;

pub mod reg_alloc;

#[cfg(feature = "target-arm64")]
pub mod arm64;

#[cfg(feature = "target-arm32")]
pub mod arm32;

#[cfg(feature = "target-riscv64")]
pub mod riscv64;

pub use block::MBlock;
pub use context::{MContext, RawData};
pub use func::MFunc;
pub use lower::{LowerConfig, LowerContext, LowerSpec};
pub use regs::{PReg, RegKind, VReg};
