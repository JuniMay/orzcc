use std::collections::HashMap;

use super::{
    block::MBlockData,
    func::{MFuncData, MLabel},
    inst::MInst,
    RegKind,
    VReg,
};
use crate::collections::storage::BaseArena;

pub struct RawData {
    pub label: MLabel,
    pub data: Vec<u8>,
}

pub struct MContext<I>
where
    I: MInst,
{
    pub(super) insts: BaseArena<I::T>,
    pub(super) blocks: BaseArena<MBlockData<I>>,
    pub(super) funcs: BaseArena<MFuncData<I>>,

    raw_data: Vec<RawData>,

    vreg_counter: u32,
}

impl<I> Default for MContext<I>
where
    I: MInst,
{
    fn default() -> Self {
        Self {
            insts: BaseArena::default(),
            blocks: BaseArena::default(),
            funcs: BaseArena::default(),
            raw_data: Vec::new(),
            vreg_counter: 0,
        }
    }
}

impl<I> MContext<I>
where
    I: MInst,
{
    pub fn new() -> Self { Self::default() }

    pub fn new_vreg(&mut self, kind: RegKind) -> VReg {
        let vreg = VReg::new(self.vreg_counter, kind);
        self.vreg_counter += 1;
        vreg
    }

    pub fn add_raw_data(&mut self, label: MLabel, data: Vec<u8>) {
        self.raw_data.push(RawData { label, data });
    }
}
