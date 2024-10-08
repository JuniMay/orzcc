use core::fmt;

use super::{
    block::MBlockData,
    func::{MFuncData, MLabel},
    inst::{DisplayMInst, MInst},
    RegKind,
    VReg,
};
use crate::collections::{linked_list::LinkedListContainerPtr, storage::BaseArena};

pub enum RawData {
    /// Bytes of the data, declared in the data section.
    Bytes(Vec<u8>),
    /// Zero-initialized bytes of the data, declared in the bss section.
    ///
    /// The field is the size of the zero-initialized data.
    Bss(usize),
}

pub struct MContext<I>
where
    I: MInst,
{
    pub(super) insts: BaseArena<I::T>,
    pub(super) blocks: BaseArena<MBlockData<I>>,
    pub(super) funcs: BaseArena<MFuncData<I>>,

    raw_data: Vec<(MLabel, RawData)>,

    vreg_counter: u32,

    arch: String,
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
            arch: String::new(),
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

    pub fn add_raw_data(&mut self, label: impl Into<MLabel>, data: RawData) {
        self.raw_data.push((label.into(), data));
    }

    pub fn set_arch(&mut self, arch: impl Into<String>) { self.arch = arch.into(); }

    pub fn arch(&self) -> &str { &self.arch }

    pub fn display(&self) -> DisplayMContext<I> { DisplayMContext { mctx: self } }
}

pub struct DisplayMContext<'a, I>
where
    I: MInst,
{
    mctx: &'a MContext<I>,
}

impl<'a, I> fmt::Display for DisplayMContext<'a, I>
where
    I: DisplayMInst<'a>,
{
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let lib = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/lib/orzcc-runtime-lib.s"
        ));

        writeln!(f, "\t.attribute arch, \"{}\"", self.mctx.arch())?;
        // writeln!(f, "\t.option pic")?;

        writeln!(f, "{}", lib)?;

        writeln!(f, "\t.text")?;
        for (_, func_data) in self.mctx.funcs.iter() {
            let func = func_data.self_ptr();

            if func.is_external(self.mctx) {
                continue;
            }

            writeln!(f, "\t.global {}", func.label(self.mctx))?;
            writeln!(f, "\t.align 1")?;
            writeln!(f, "\t.type {}, @function", func.label(self.mctx))?;
            writeln!(f, "{}:", func.label(self.mctx))?;

            for block in func.iter(self.mctx) {
                writeln!(f, "{}:", block.label(self.mctx))?;
                for inst in block.iter(self.mctx) {
                    writeln!(f, "\t{}", inst.display(self.mctx))?;
                }
            }

            writeln!(f)?;
        }

        for (label, raw_data) in self.mctx.raw_data.iter() {
            writeln!(f, "\t.type {}, @object", label)?;
            match raw_data {
                RawData::Bytes(bytes) => {
                    writeln!(f, "\t.data")?;
                    writeln!(f, "\t.global {}", label)?;
                    writeln!(f, "\t.align 2")?;
                    writeln!(f, "{}:", label)?;
                    for byte in bytes.iter() {
                        writeln!(f, "\t.byte {}", byte)?;
                    }
                    writeln!(f)?;
                }
                RawData::Bss(size) => {
                    writeln!(f, "\t.bss")?;
                    writeln!(f, "\t.global {}", label)?;
                    writeln!(f, "\t.align 2")?;
                    writeln!(f, "{}:", label)?;
                    writeln!(f, "\t.zero {}", size)?;
                    writeln!(f)?;
                }
            }
            writeln!(f)?;
        }

        Ok(())
    }
}
