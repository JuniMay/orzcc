use std::collections::HashMap;

use super::{inst::MInst, regs::Reg, PReg};
use crate::ir;

pub struct Lower {}

pub enum MemLoc {
    /// The memory location is a register + offset.
    RegOffset { base: Reg, offset: i64 },
    /// The memory location is a stack slot.
    ///
    /// Usually, the offset is based on frame pointer, after generating the
    /// prologue, this offset should be editted with an additional offset.
    Slot { offset: i64 },
}

pub trait LowerSpec {
    type I: MInst;

    fn stack_align() -> u32;

    fn frame_pointer_reg() -> PReg;

    fn stack_pointer_reg() -> PReg;
}
