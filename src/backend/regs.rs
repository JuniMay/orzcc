use core::fmt;
use std::hash::Hash;

use rustc_hash::{FxHashMap, FxHashSet};

use super::{inst::MInst, LowerConfig, MContext};
use crate::collections::linked_list::LinkedListContainerPtr;

/// The kind of a register.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum RegKind {
    /// The general purpose register.
    General,
    /// The floating point register.
    Float,
    /// The vector register.
    Vector,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Reg {
    P(PReg),
    V(VReg),
}

impl Reg {
    pub fn kind(&self) -> RegKind {
        match self {
            Reg::P(preg) => preg.kind(),
            Reg::V(vreg) => vreg.kind(),
        }
    }

    pub fn is_preg(&self) -> bool { matches!(self, Reg::P(_)) }

    pub fn is_vreg(&self) -> bool { matches!(self, Reg::V(_)) }
}

/// The physical register.
///
/// Cranelift uses a bit-encoded representation, but here just separate the
/// register number and the kind.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct PReg(u8, RegKind);

impl PReg {
    pub const fn new(num: u8, kind: RegKind) -> Self { Self(num, kind) }

    pub const fn num(&self) -> u8 { self.0 }

    pub const fn kind(&self) -> RegKind { self.1 }
}

/// The virtual register.
///
/// Let's hope the number of virtual registers does not exceed [u32::MAX].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VReg(u32, RegKind);

impl VReg {
    pub fn new(num: u32, kind: RegKind) -> Self { Self(num, kind) }

    pub fn num(&self) -> u32 { self.0 }

    pub fn kind(&self) -> RegKind { self.1 }
}

impl From<VReg> for Reg {
    fn from(vreg: VReg) -> Self { Self::V(vreg) }
}

impl From<PReg> for Reg {
    fn from(preg: PReg) -> Self { Self::P(preg) }
}

impl fmt::Display for VReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}{}",
            match self.1 {
                RegKind::General => "$r",
                RegKind::Float => "$f",
                RegKind::Vector => "$v",
            },
            self.0
        )
    }
}

/// A simple struct to record the definitions and uses of registers.
///
/// Note that this is not reaching definition analysis.
pub struct RegDefUse<I: MInst + Hash> {
    pub defs: FxHashMap<Reg, FxHashSet<I>>,
    pub uses: FxHashMap<Reg, FxHashSet<I>>,
}

impl<I: MInst + Hash> RegDefUse<I> {
    pub fn compute(mctx: &MContext<I>, config: &LowerConfig) -> Self {
        let mut reg_def_use = RegDefUse {
            defs: FxHashMap::default(),
            uses: FxHashMap::default(),
        };

        for (_, func_data) in mctx.funcs.iter() {
            let func = func_data.self_ptr();
            if func.is_external(mctx) {
                continue;
            }

            for block in func.iter(mctx) {
                for inst in block.iter(mctx) {
                    let defs = inst.defs(mctx, config);
                    let uses = inst.uses(mctx, config);

                    for def in defs {
                        reg_def_use.defs.entry(def).or_default().insert(inst);
                    }

                    for use_ in uses {
                        reg_def_use.uses.entry(use_).or_default().insert(inst);
                    }
                }
            }
        }

        reg_def_use
    }

    pub fn remove_use(&mut self, reg: Reg, inst: I) {
        if let Some(uses) = self.uses.get_mut(&reg) {
            uses.remove(&inst);
        }
    }

    pub fn add_use(&mut self, reg: Reg, inst: I) { self.uses.entry(reg).or_default().insert(inst); }

    pub fn remove_def(&mut self, reg: Reg, inst: I) {
        if let Some(defs) = self.defs.get_mut(&reg) {
            defs.remove(&inst);
        }
    }

    pub fn add_def(&mut self, reg: Reg, inst: I) { self.defs.entry(reg).or_default().insert(inst); }

    pub fn num_uses(&self, reg: Reg) -> usize { self.uses.get(&reg).map_or(0, FxHashSet::len) }

    pub fn num_defs(&self, reg: Reg) -> usize { self.defs.get(&reg).map_or(0, FxHashSet::len) }

    pub fn replace_all_uses(&mut self, mctx: &mut MContext<I>, old: Reg, new: Reg) {
        if let Some(uses) = self.uses.remove(&old) {
            for inst in uses {
                inst.replace_reg(mctx, old, new);
                self.uses.entry(new).or_default().insert(inst);
            }
        }
    }
}
