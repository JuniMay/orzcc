pub mod imm;
pub mod inst;
pub mod lower;
pub mod regs;

mod peephole;
mod schedule;

pub use peephole::{run_peephole, run_peephole_after_regalloc};
pub use schedule::schedule;
