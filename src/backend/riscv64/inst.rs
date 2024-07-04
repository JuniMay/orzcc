use std::fmt;

use super::{imm::Imm12, regs};
use crate::{
    backend::{
        block::MBlock,
        context::MContext,
        func::{MFunc, MLabel},
        inst::{DisplayMInst, MInst},
        lower::{LowerConfig, MemLoc},
        regs::Reg,
        PReg,
        RegKind,
    },
    collections::{
        linked_list::LinkedListNodePtr,
        storage::{ArenaAlloc, ArenaDeref, ArenaPtr, BaseArenaPtr},
    },
};

pub struct RvInstData {
    kind: RvInstKind,
    next: Option<RvInst>,
    prev: Option<RvInst>,
    parent: Option<MBlock<RvInst>>,
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct RvInst(BaseArenaPtr<RvInstData>);

impl RvInst {
    pub fn kind(self, mctx: &MContext<Self>) -> &RvInstKind { &self.deref(mctx).kind }

    pub fn build_li(mctx: &mut MContext<Self>, rd: Reg, imm: impl Into<u64>) -> Self {
        let kind = RvInstKind::Li {
            rd,
            imm: imm.into(),
        };
        let data = RvInstData {
            kind,
            next: None,
            prev: None,
            parent: None,
        };
        mctx.alloc(data)
    }

    pub fn build_alu_rr(mctx: &mut MContext<Self>, op: AluOpRR, rd: Reg, rs: Reg) -> Self {
        let kind = RvInstKind::AluRR { op, rd, rs };
        let data = RvInstData {
            kind,
            next: None,
            prev: None,
            parent: None,
        };
        mctx.alloc(data)
    }

    pub fn build_alu_rri(
        mctx: &mut MContext<Self>,
        op: AluOpRRI,
        rd: Reg,
        rs: Reg,
        imm: Imm12,
    ) -> Self {
        let kind = RvInstKind::AluRRI { op, rd, rs, imm };
        let data = RvInstData {
            kind,
            next: None,
            prev: None,
            parent: None,
        };
        mctx.alloc(data)
    }

    pub fn build_alu_rrr(
        mctx: &mut MContext<Self>,
        op: AluOpRRR,
        rd: Reg,
        rs1: Reg,
        rs2: Reg,
    ) -> Self {
        let kind = RvInstKind::AluRRR { op, rd, rs1, rs2 };
        let data = RvInstData {
            kind,
            next: None,
            prev: None,
            parent: None,
        };
        mctx.alloc(data)
    }

    pub fn build_fpu_rr(mctx: &mut MContext<Self>, op: FpuOpRR, rm: Frm, rd: Reg, rs: Reg) -> Self {
        let kind = RvInstKind::FpuRR { op, rm, rd, rs };
        let data = RvInstData {
            kind,
            next: None,
            prev: None,
            parent: None,
        };
        mctx.alloc(data)
    }

    pub fn build_fpu_rrr(
        mctx: &mut MContext<Self>,
        op: FpuOpRRR,
        rm: Frm,
        rd: Reg,
        rs1: Reg,
        rs2: Reg,
    ) -> Self {
        let kind = RvInstKind::FpuRRR {
            op,
            rm,
            rd,
            rs1,
            rs2,
        };
        let data = RvInstData {
            kind,
            next: None,
            prev: None,
            parent: None,
        };
        mctx.alloc(data)
    }

    pub fn build_fpu_rrrr(
        mctx: &mut MContext<Self>,
        op: FpuOpRRRR,
        rm: Frm,
        rd: Reg,
        rs1: Reg,
        rs2: Reg,
        rs3: Reg,
    ) -> Self {
        let kind = RvInstKind::FpuRRRR {
            op,
            rm,
            rd,
            rs1,
            rs2,
            rs3,
        };
        let data = RvInstData {
            kind,
            next: None,
            prev: None,
            parent: None,
        };
        mctx.alloc(data)
    }

    pub fn build_load(mctx: &mut MContext<Self>, op: LoadOp, rd: Reg, loc: MemLoc) -> Self {
        let kind = RvInstKind::Load { op, rd, loc };
        let data = RvInstData {
            kind,
            next: None,
            prev: None,
            parent: None,
        };
        mctx.alloc(data)
    }

    pub fn build_la(mctx: &mut MContext<Self>, rd: Reg, label: MLabel) -> Self {
        let kind = RvInstKind::La { rd, label };
        let data = RvInstData {
            kind,
            next: None,
            prev: None,
            parent: None,
        };
        mctx.alloc(data)
    }

    pub fn build_load_addr(mctx: &mut MContext<Self>, rd: Reg, loc: MemLoc) -> Self {
        let kind = RvInstKind::LoadAddr { rd, loc };
        let data = RvInstData {
            kind,
            next: None,
            prev: None,
            parent: None,
        };
        mctx.alloc(data)
    }

    pub fn store(mctx: &mut MContext<Self>, op: StoreOp, src: Reg, loc: MemLoc) -> Self {
        let kind = RvInstKind::Store { op, src, loc };
        let data = RvInstData {
            kind,
            next: None,
            prev: None,
            parent: None,
        };
        mctx.alloc(data)
    }

    pub fn ret(mctx: &mut MContext<Self>) -> Self {
        let kind = RvInstKind::Ret;
        let data = RvInstData {
            kind,
            next: None,
            prev: None,
            parent: None,
        };
        mctx.alloc(data)
    }

    pub fn call(mctx: &mut MContext<Self>, func: MFunc<Self>, arg_regs: Vec<PReg>) -> Self {
        let kind = RvInstKind::Call { func, arg_regs };
        let data = RvInstData {
            kind,
            next: None,
            prev: None,
            parent: None,
        };
        mctx.alloc(data)
    }

    pub fn j(mctx: &mut MContext<Self>, block: MBlock<Self>) -> Self {
        let kind = RvInstKind::J { block };
        let data = RvInstData {
            kind,
            next: None,
            prev: None,
            parent: None,
        };
        mctx.alloc(data)
    }

    pub fn br(
        mctx: &mut MContext<Self>,
        op: BrOp,
        rs1: Reg,
        rs2: Reg,
        block: MBlock<Self>,
    ) -> Self {
        let kind = RvInstKind::Br {
            op,
            rs1,
            rs2,
            block,
        };
        let data = RvInstData {
            kind,
            next: None,
            prev: None,
            parent: None,
        };
        mctx.alloc(data)
    }

    pub fn li(mctx: &mut MContext<Self>, imm: impl Into<u64>) -> (Self, Reg) {
        let rd: Reg = mctx.new_vreg(RegKind::General).into();
        let inst = Self::build_li(mctx, rd, imm);
        (inst, rd)
    }

    pub fn alu_rr(mctx: &mut MContext<Self>, op: AluOpRR, rs: Reg) -> (Self, Reg) {
        let rd: Reg = mctx.new_vreg(RegKind::General).into();
        let inst = Self::build_alu_rr(mctx, op, rd, rs);
        (inst, rd)
    }

    pub fn alu_rri(mctx: &mut MContext<Self>, op: AluOpRRI, rs: Reg, imm: Imm12) -> (Self, Reg) {
        let rd: Reg = mctx.new_vreg(RegKind::General).into();
        let inst = Self::build_alu_rri(mctx, op, rd, rs, imm);
        (inst, rd)
    }

    pub fn alu_rrr(mctx: &mut MContext<Self>, op: AluOpRRR, rs1: Reg, rs2: Reg) -> (Self, Reg) {
        let rd: Reg = mctx.new_vreg(RegKind::General).into();
        let inst = Self::build_alu_rrr(mctx, op, rd, rs1, rs2);
        (inst, rd)
    }

    pub fn fpu_rr(mctx: &mut MContext<Self>, op: FpuOpRR, rm: Frm, rs: Reg) -> (Self, Reg) {
        let rd = match op {
            FpuOpRR::FclassS
            | FpuOpRR::FclassD
            | FpuOpRR::FcvtWS
            | FpuOpRR::FcvtWD
            | FpuOpRR::FcvtWuS
            | FpuOpRR::FcvtWuD
            | FpuOpRR::FcvtLS
            | FpuOpRR::FcvtLuS
            | FpuOpRR::FcvtLD
            | FpuOpRR::FcvtLuD
            | FpuOpRR::FmvXW
            | FpuOpRR::FmvXD => mctx.new_vreg(RegKind::General).into(),
            FpuOpRR::FsqrtS
            | FpuOpRR::FsqrtD
            | FpuOpRR::FcvtSW
            | FpuOpRR::FcvtSWu
            | FpuOpRR::FcvtSL
            | FpuOpRR::FcvtSLu
            | FpuOpRR::FcvtDW
            | FpuOpRR::FcvtDWu
            | FpuOpRR::FcvtDL
            | FpuOpRR::FcvtDLu
            | FpuOpRR::FcvtSD
            | FpuOpRR::FcvtDS
            | FpuOpRR::FmvWX
            | FpuOpRR::FmvDX => mctx.new_vreg(RegKind::Float).into(),
        };
        let inst = Self::build_fpu_rr(mctx, op, rm, rd, rs);
        (inst, rd)
    }

    pub fn fpu_rrr(
        mctx: &mut MContext<Self>,
        op: FpuOpRRR,
        rm: Frm,
        rs1: Reg,
        rs2: Reg,
    ) -> (Self, Reg) {
        let rd = match op {
            FpuOpRRR::FaddS
            | FpuOpRRR::FaddD
            | FpuOpRRR::FsubS
            | FpuOpRRR::FsubD
            | FpuOpRRR::FmulS
            | FpuOpRRR::FmulD
            | FpuOpRRR::FdivS
            | FpuOpRRR::FdivD
            | FpuOpRRR::FminS
            | FpuOpRRR::FminD
            | FpuOpRRR::FmaxS
            | FpuOpRRR::FmaxD
            | FpuOpRRR::FsgnjS
            | FpuOpRRR::FsgnjD
            | FpuOpRRR::FsgnjnS
            | FpuOpRRR::FsgnjnD
            | FpuOpRRR::FsgnjxS
            | FpuOpRRR::FsgnjxD => mctx.new_vreg(RegKind::Float).into(),
            FpuOpRRR::FeqS
            | FpuOpRRR::FeqD
            | FpuOpRRR::FltS
            | FpuOpRRR::FltD
            | FpuOpRRR::FleS
            | FpuOpRRR::FleD => mctx.new_vreg(RegKind::General).into(),
        };
        let inst = Self::build_fpu_rrr(mctx, op, rm, rd, rs1, rs2);
        (inst, rd)
    }

    pub fn fpu_rrrr(
        mctx: &mut MContext<Self>,
        op: FpuOpRRRR,
        rm: Frm,
        rs1: Reg,
        rs2: Reg,
        rs3: Reg,
    ) -> (Self, Reg) {
        let rd: Reg = mctx.new_vreg(RegKind::Float).into();
        let inst = Self::build_fpu_rrrr(mctx, op, rm, rd, rs1, rs2, rs3);
        (inst, rd)
    }

    pub fn load(mctx: &mut MContext<Self>, op: LoadOp, loc: MemLoc) -> (Self, Reg) {
        let rd = match op {
            LoadOp::Fld | LoadOp::Flw => mctx.new_vreg(RegKind::Float).into(),
            LoadOp::Lb
            | LoadOp::Lh
            | LoadOp::Lw
            | LoadOp::Ld
            | LoadOp::Lbu
            | LoadOp::Lhu
            | LoadOp::Lwu => mctx.new_vreg(RegKind::General).into(),
        };
        let inst = Self::build_load(mctx, op, rd, loc);
        (inst, rd)
    }

    pub fn la(mctx: &mut MContext<Self>, label: MLabel) -> (Self, Reg) {
        let rd: Reg = mctx.new_vreg(RegKind::General).into();
        let inst = Self::build_la(mctx, rd, label);
        (inst, rd)
    }

    pub fn load_addr(mctx: &mut MContext<Self>, loc: MemLoc) -> (Self, Reg) {
        let rd: Reg = mctx.new_vreg(RegKind::General).into();
        let inst = Self::build_load_addr(mctx, rd, loc);
        (inst, rd)
    }
}

pub enum RvInstKind {
    Li {
        rd: Reg,
        imm: u64,
    },
    AluRR {
        op: AluOpRR,
        rd: Reg,
        rs: Reg,
    },
    AluRRI {
        op: AluOpRRI,
        rd: Reg,
        rs: Reg,
        imm: Imm12,
    },
    AluRRR {
        op: AluOpRRR,
        rd: Reg,
        rs1: Reg,
        rs2: Reg,
    },
    FpuRR {
        op: FpuOpRR,
        rm: Frm,
        rd: Reg,
        rs: Reg,
    },
    FpuRRR {
        op: FpuOpRRR,
        rm: Frm,
        rd: Reg,
        rs1: Reg,
        rs2: Reg,
    },
    FpuRRRR {
        op: FpuOpRRRR,
        rm: Frm,
        rd: Reg,
        rs1: Reg,
        rs2: Reg,
        rs3: Reg,
    },
    Load {
        op: LoadOp,
        rd: Reg,
        loc: MemLoc,
    },
    Store {
        op: StoreOp,
        src: Reg,
        loc: MemLoc,
    },
    Ret,
    Call {
        func: MFunc<RvInst>,
        arg_regs: Vec<PReg>,
    },
    J {
        block: MBlock<RvInst>,
    },
    Br {
        op: BrOp,
        rs1: Reg,
        rs2: Reg,
        block: MBlock<RvInst>,
    },
    La {
        rd: Reg,
        label: MLabel,
    },
    /// Load the address of a memory location into a register.
    ///
    /// Because we don't know the place of local slots in the memory, so we must
    /// create such a forward reference to load the address of a local slot.
    /// After generating all code (and register allocation), we should modify
    /// all the [MemLoc::Slot] into [MemLoc::RegOffset] and make this into a
    /// addi or li + add sequence.
    LoadAddr {
        rd: Reg,
        loc: MemLoc,
    },
}

pub struct DisplayRvInst<'a> {
    mctx: &'a MContext<RvInst>,
    inst: RvInst,
}

impl<'a> DisplayMInst<'a> for RvInst {
    type Display = DisplayRvInst<'a>;

    fn display(self, mctx: &'a MContext<Self>) -> Self::Display {
        DisplayRvInst { mctx, inst: self }
    }
}

impl<'a> fmt::Display for DisplayRvInst<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use RvInstKind as Ik;

        match &self.inst.deref(self.mctx).kind {
            Ik::Li { rd, imm } => write!(f, "li {}, {:#018x}", regs::display(*rd), imm),
            Ik::AluRR { op, rd, rs } => {
                write!(f, "{} {}, {}", op, regs::display(*rd), regs::display(*rs))
            }
            Ik::AluRRI { op, rd, rs, imm } => {
                write!(
                    f,
                    "{} {}, {}, {}",
                    op,
                    regs::display(*rd),
                    regs::display(*rs),
                    imm
                )
            }
            Ik::AluRRR { op, rd, rs1, rs2 } => {
                write!(
                    f,
                    "{} {}, {}, {}",
                    op,
                    regs::display(*rd),
                    regs::display(*rs1),
                    regs::display(*rs2)
                )
            }
            Ik::FpuRR { op, rm, rd, rs } => {
                write!(
                    f,
                    "{} {}, {}{}",
                    op,
                    regs::display(*rd),
                    regs::display(*rs),
                    rm
                )
            }
            Ik::FpuRRR {
                op,
                rm,
                rd,
                rs1,
                rs2,
            } => {
                write!(
                    f,
                    "{} {}, {}, {}{}",
                    op,
                    regs::display(*rd),
                    regs::display(*rs1),
                    regs::display(*rs2),
                    rm
                )
            }
            Ik::FpuRRRR {
                op,
                rm,
                rd,
                rs1,
                rs2,
                rs3,
            } => {
                write!(
                    f,
                    "{} {}, {}, {}, {}{}",
                    op,
                    regs::display(*rd),
                    regs::display(*rs1),
                    regs::display(*rs2),
                    regs::display(*rs3),
                    rm
                )
            }
            Ik::Load { op, rd, loc } => {
                let slot = match loc {
                    MemLoc::RegOffset { base, offset } => {
                        format!("{}({})", offset, regs::display(*base))
                    }
                    MemLoc::Slot { .. } | MemLoc::Incoming { .. } => {
                        unreachable!("mem loc should be modified to reg offset")
                    }
                };
                write!(f, "{} {}, {}", op, regs::display(*rd), slot)
            }
            Ik::Store { op, src, loc } => {
                let slot = match loc {
                    MemLoc::RegOffset { base, offset } => {
                        format!("{}({})", offset, regs::display(*base))
                    }
                    MemLoc::Slot { .. } | MemLoc::Incoming { .. } => {
                        unreachable!("mem loc should be modified to reg offset")
                    }
                };
                write!(f, "{} {}, {}", op, regs::display(*src), slot)
            }
            Ik::Ret => write!(f, "ret"),
            Ik::Call { func, .. } => write!(f, "call {}", func.label(self.mctx)),
            Ik::J { block } => write!(f, "j {}", block.label(self.mctx)),
            Ik::Br {
                op,
                rs1,
                rs2,
                block,
            } => {
                write!(
                    f,
                    "{} {}, {}, {}",
                    op,
                    regs::display(*rs1),
                    regs::display(*rs2),
                    block.label(self.mctx)
                )
            }
            Ik::La { rd, label } => write!(f, "la {}, {}", regs::display(*rd), label),
            Ik::LoadAddr { .. } => {
                unreachable!("load addr should be converted to li + add or addi")
            }
        }
    }
}

pub enum BrOp {
    Beq,
    Bne,
    Blt,
    Bge,
    Bltu,
    Bgeu,
}

impl fmt::Display for BrOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            BrOp::Beq => write!(f, "beq"),
            BrOp::Bne => write!(f, "bne"),
            BrOp::Blt => write!(f, "blt"),
            BrOp::Bge => write!(f, "bge"),
            BrOp::Bltu => write!(f, "bltu"),
            BrOp::Bgeu => write!(f, "bgeu"),
        }
    }
}

pub enum AluOpRRR {
    // gc
    Add,
    Addw,
    Sub,
    Subw,
    Sll,
    Sllw,
    Srl,
    Srlw,
    Sra,
    Sraw,
    Xor,
    Or,
    And,
    Slt,
    Sltu,
    Mul,
    Mulw,
    Mulh,
    Mulhsu,
    Mulhu,
    Div,
    Divw,
    Divu,
    Divuw,
    Rem,
    Remw,
    Remu,
    Remuw,
    Rew,
    // zba
    Adduw,
    Sh1add,
    Sh1adduw,
    Sh2add,
    Sh2adduw,
    Sh3add,
    Sh3adduw,
    // zbb
    Andn,
    Orn,
    Xnor,
    Max,
    Maxu,
    Min,
    Minu,
    Rol,
    Rolw,
    Ror,
    Rorw,
}

impl fmt::Display for AluOpRRR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AluOpRRR::Add => write!(f, "add"),
            AluOpRRR::Addw => write!(f, "addw"),
            AluOpRRR::Sub => write!(f, "sub"),
            AluOpRRR::Subw => write!(f, "subw"),
            AluOpRRR::Sll => write!(f, "sll"),
            AluOpRRR::Sllw => write!(f, "sllw"),
            AluOpRRR::Srl => write!(f, "srl"),
            AluOpRRR::Srlw => write!(f, "srlw"),
            AluOpRRR::Sra => write!(f, "sra"),
            AluOpRRR::Sraw => write!(f, "sraw"),
            AluOpRRR::Xor => write!(f, "xor"),
            AluOpRRR::Or => write!(f, "or"),
            AluOpRRR::And => write!(f, "and"),
            AluOpRRR::Slt => write!(f, "slt"),
            AluOpRRR::Sltu => write!(f, "sltu"),
            AluOpRRR::Mul => write!(f, "mul"),
            AluOpRRR::Mulw => write!(f, "mulw"),
            AluOpRRR::Mulh => write!(f, "mulh"),
            AluOpRRR::Mulhsu => write!(f, "mulhsu"),
            AluOpRRR::Mulhu => write!(f, "mulhu"),
            AluOpRRR::Div => write!(f, "div"),
            AluOpRRR::Divw => write!(f, "divw"),
            AluOpRRR::Divu => write!(f, "divu"),
            AluOpRRR::Divuw => write!(f, "divuw"),
            AluOpRRR::Rem => write!(f, "rem"),
            AluOpRRR::Remw => write!(f, "remw"),
            AluOpRRR::Remu => write!(f, "remu"),
            AluOpRRR::Remuw => write!(f, "remuw"),
            AluOpRRR::Rew => write!(f, "rew"),
            AluOpRRR::Adduw => write!(f, "add.uw"),
            AluOpRRR::Sh1add => write!(f, "sh1add"),
            AluOpRRR::Sh1adduw => write!(f, "sh1add.uw"),
            AluOpRRR::Sh2add => write!(f, "sh2add"),
            AluOpRRR::Sh2adduw => write!(f, "sh2add.uw"),
            AluOpRRR::Sh3add => write!(f, "sh3add"),
            AluOpRRR::Sh3adduw => write!(f, "sh3add.uw"),
            AluOpRRR::Andn => write!(f, "andn"),
            AluOpRRR::Orn => write!(f, "orn"),
            AluOpRRR::Xnor => write!(f, "xnor"),
            AluOpRRR::Max => write!(f, "max"),
            AluOpRRR::Maxu => write!(f, "maxu"),
            AluOpRRR::Min => write!(f, "min"),
            AluOpRRR::Minu => write!(f, "minu"),
            AluOpRRR::Rol => write!(f, "rol"),
            AluOpRRR::Rolw => write!(f, "rolw"),
            AluOpRRR::Ror => write!(f, "ror"),
            AluOpRRR::Rorw => write!(f, "rorw"),
        }
    }
}

pub enum AluOpRRI {
    Addi,
    Addiw,
    Slli,
    Slliw,
    Srli,
    Srliw,
    Srai,
    Sraiw,
    Xori,
    Ori,
    Andi,
    Slti,
    Sltiu,
    // zba
    Slliuw,
    // zbb
    Rori,
    Roriw,
}

impl fmt::Display for AluOpRRI {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AluOpRRI::Addi => write!(f, "addi"),
            AluOpRRI::Addiw => write!(f, "addiw"),
            AluOpRRI::Slli => write!(f, "slli"),
            AluOpRRI::Slliw => write!(f, "slliw"),
            AluOpRRI::Srli => write!(f, "srli"),
            AluOpRRI::Srliw => write!(f, "srliw"),
            AluOpRRI::Srai => write!(f, "srai"),
            AluOpRRI::Sraiw => write!(f, "sraiw"),
            AluOpRRI::Xori => write!(f, "xori"),
            AluOpRRI::Ori => write!(f, "ori"),
            AluOpRRI::Andi => write!(f, "andi"),
            AluOpRRI::Slti => write!(f, "slti"),
            AluOpRRI::Sltiu => write!(f, "sltiu"),
            AluOpRRI::Slliuw => write!(f, "slli.uw"),
            AluOpRRI::Rori => write!(f, "rori"),
            AluOpRRI::Roriw => write!(f, "roriw"),
        }
    }
}

pub enum AluOpRR {
    // zbb
    Clz,
    Clzw,
    Ctz,
    Ctzw,
    Cpop,
    Cpopw,
    Sextb,
    Sexth,
    Zexth,
    Orcb,
    Rev8,
}

impl fmt::Display for AluOpRR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AluOpRR::Clz => write!(f, "clz"),
            AluOpRR::Clzw => write!(f, "clzw"),
            AluOpRR::Ctz => write!(f, "ctz"),
            AluOpRR::Ctzw => write!(f, "ctzw"),
            AluOpRR::Cpop => write!(f, "cpop"),
            AluOpRR::Cpopw => write!(f, "cpopw"),
            AluOpRR::Sextb => write!(f, "sext.b"),
            AluOpRR::Sexth => write!(f, "sext.h"),
            AluOpRR::Zexth => write!(f, "zext.h"),
            AluOpRR::Orcb => write!(f, "orc.b"),
            AluOpRR::Rev8 => write!(f, "rev8"),
        }
    }
}

pub enum FpuOpRR {
    FsqrtS,
    FsqrtD,
    FclassS,
    FclassD,

    // fcvt
    FcvtSW,
    FcvtSWu,
    FcvtSL,
    FcvtSLu,

    FcvtWS,
    FcvtWuS,
    FcvtLS,
    FcvtLuS,

    FcvtDW,
    FcvtDWu,
    FcvtDL,
    FcvtDLu,

    FcvtWD,
    FcvtWuD,
    FcvtLD,
    FcvtLuD,

    FcvtSD,
    FcvtDS,

    // fmv
    FmvWX,
    FmvXW,
    FmvDX,
    FmvXD,
}

impl fmt::Display for FpuOpRR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FpuOpRR::FsqrtS => write!(f, "fsqrt.s"),
            FpuOpRR::FsqrtD => write!(f, "fsqrt.d"),
            FpuOpRR::FclassS => write!(f, "fclass.s"),
            FpuOpRR::FclassD => write!(f, "fclass.d"),
            FpuOpRR::FcvtSW => write!(f, "fcvt.s.w"),
            FpuOpRR::FcvtSWu => write!(f, "fcvt.s.wu"),
            FpuOpRR::FcvtSL => write!(f, "fcvt.s.l"),
            FpuOpRR::FcvtSLu => write!(f, "fcvt.s.lu"),
            FpuOpRR::FcvtWS => write!(f, "fcvt.w.s"),
            FpuOpRR::FcvtWuS => write!(f, "fcvt.wu.s"),
            FpuOpRR::FcvtLS => write!(f, "fcvt.l.s"),
            FpuOpRR::FcvtLuS => write!(f, "fcvt.lu.s"),
            FpuOpRR::FcvtDW => write!(f, "fcvt.d.w"),
            FpuOpRR::FcvtDWu => write!(f, "fcvt.d.wu"),
            FpuOpRR::FcvtDL => write!(f, "fcvt.d.l"),
            FpuOpRR::FcvtDLu => write!(f, "fcvt.d.lu"),
            FpuOpRR::FcvtWD => write!(f, "fcvt.w.d"),
            FpuOpRR::FcvtWuD => write!(f, "fcvt.wu.d"),
            FpuOpRR::FcvtLD => write!(f, "fcvt.l.d"),
            FpuOpRR::FcvtLuD => write!(f, "fcvt.lu.d"),
            FpuOpRR::FcvtSD => write!(f, "fcvt.s.d"),
            FpuOpRR::FcvtDS => write!(f, "fcvt.d.s"),
            FpuOpRR::FmvWX => write!(f, "fmv.w.x"),
            FpuOpRR::FmvXW => write!(f, "fmv.x.w"),
            FpuOpRR::FmvDX => write!(f, "fmv.d.x"),
            FpuOpRR::FmvXD => write!(f, "fmv.x.d"),
        }
    }
}

pub enum FpuOpRRR {
    FaddS,
    FaddD,
    FsubS,
    FsubD,
    FmulS,
    FmulD,
    FdivS,
    FdivD,
    FminS,
    FminD,
    FmaxS,
    FmaxD,
    FsgnjS,
    FsgnjD,
    FsgnjnS,
    FsgnjnD,
    FsgnjxS,
    FsgnjxD,
    FeqS,
    FeqD,
    FltS,
    FltD,
    FleS,
    FleD,
}

impl fmt::Display for FpuOpRRR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FpuOpRRR::FaddS => write!(f, "fadd.s"),
            FpuOpRRR::FaddD => write!(f, "fadd.d"),
            FpuOpRRR::FsubS => write!(f, "fsub.s"),
            FpuOpRRR::FsubD => write!(f, "fsub.d"),
            FpuOpRRR::FmulS => write!(f, "fmul.s"),
            FpuOpRRR::FmulD => write!(f, "fmul.d"),
            FpuOpRRR::FdivS => write!(f, "fdiv.s"),
            FpuOpRRR::FdivD => write!(f, "fdiv.d"),
            FpuOpRRR::FminS => write!(f, "fmin.s"),
            FpuOpRRR::FminD => write!(f, "fmin.d"),
            FpuOpRRR::FmaxS => write!(f, "fmax.s"),
            FpuOpRRR::FmaxD => write!(f, "fmax.d"),
            FpuOpRRR::FsgnjS => write!(f, "fsgnj.s"),
            FpuOpRRR::FsgnjD => write!(f, "fsgnj.d"),
            FpuOpRRR::FsgnjnS => write!(f, "fsgnjn.s"),
            FpuOpRRR::FsgnjnD => write!(f, "fsgnjn.d"),
            FpuOpRRR::FsgnjxS => write!(f, "fsgnjx.s"),
            FpuOpRRR::FsgnjxD => write!(f, "fsgnjx.d"),
            FpuOpRRR::FeqS => write!(f, "feq.s"),
            FpuOpRRR::FeqD => write!(f, "feq.d"),
            FpuOpRRR::FltS => write!(f, "flt.s"),
            FpuOpRRR::FltD => write!(f, "flt.d"),
            FpuOpRRR::FleS => write!(f, "fle.s"),
            FpuOpRRR::FleD => write!(f, "fle.d"),
        }
    }
}

pub enum FpuOpRRRR {
    FmaddS,
    FmaddD,
    FmsubS,
    FmsubD,
    FnmaddS,
    FnmaddD,
    FnmsubS,
    FnmsubD,
}

impl fmt::Display for FpuOpRRRR {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FpuOpRRRR::FmaddS => write!(f, "fmadd.s"),
            FpuOpRRRR::FmaddD => write!(f, "fmadd.d"),
            FpuOpRRRR::FmsubS => write!(f, "fmsub.s"),
            FpuOpRRRR::FmsubD => write!(f, "fmsub.d"),
            FpuOpRRRR::FnmaddS => write!(f, "fnmadd.s"),
            FpuOpRRRR::FnmaddD => write!(f, "fnmadd.d"),
            FpuOpRRRR::FnmsubS => write!(f, "fnmsub.s"),
            FpuOpRRRR::FnmsubD => write!(f, "fnmsub.d"),
        }
    }
}

pub enum LoadOp {
    Lb,
    Lh,
    Lw,
    Ld,
    Lbu,
    Lhu,
    Lwu,
    Flw,
    Fld,
}

impl fmt::Display for LoadOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LoadOp::Lb => write!(f, "lb"),
            LoadOp::Lh => write!(f, "lh"),
            LoadOp::Lw => write!(f, "lw"),
            LoadOp::Ld => write!(f, "ld"),
            LoadOp::Lbu => write!(f, "lbu"),
            LoadOp::Lhu => write!(f, "lhu"),
            LoadOp::Lwu => write!(f, "lwu"),
            LoadOp::Flw => write!(f, "flw"),
            LoadOp::Fld => write!(f, "fld"),
        }
    }
}

pub enum StoreOp {
    Sb,
    Sh,
    Sw,
    Sd,
    Fsw,
    Fsd,
}

impl fmt::Display for StoreOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            StoreOp::Sb => write!(f, "sb"),
            StoreOp::Sh => write!(f, "sh"),
            StoreOp::Sw => write!(f, "sw"),
            StoreOp::Sd => write!(f, "sd"),
            StoreOp::Fsw => write!(f, "fsw"),
            StoreOp::Fsd => write!(f, "fsd"),
        }
    }
}

pub enum Frm {
    Rne,
    Rtz,
    Rdn,
    Rup,
    Rmm,
    Dyn,
}

impl fmt::Display for Frm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Frm::Rne => write!(f, ", rne"),
            Frm::Rtz => write!(f, ", rtz"),
            Frm::Rdn => write!(f, ", rdn"),
            Frm::Rup => write!(f, ", rup"),
            Frm::Rmm => write!(f, ", rmm"),
            Frm::Dyn => write!(f, ""), // not sure if this is correct
        }
    }
}

impl MInst for RvInst {
    fn from_ptr(ptr: BaseArenaPtr<Self::T>) -> Self { Self(ptr) }

    fn ptr(self) -> BaseArenaPtr<Self::T> { self.0 }

    fn uses(self, mctx: &MContext<Self>, config: &LowerConfig) -> Vec<Reg> {
        use RvInstKind as Ik;

        match &self.deref(mctx).kind {
            Ik::Li { .. } => vec![],
            Ik::AluRR { rs, .. } => vec![*rs],
            Ik::AluRRI { rs, .. } => vec![*rs],
            Ik::AluRRR { rs1, rs2, .. } => vec![*rs1, *rs2],
            Ik::FpuRR { rs, .. } => vec![*rs],
            Ik::FpuRRR { rs1, rs2, .. } => vec![*rs1, *rs2],
            Ik::FpuRRRR { rs1, rs2, rs3, .. } => vec![*rs1, *rs2, *rs3],
            Ik::Load { loc, .. } => match loc {
                MemLoc::RegOffset { base, .. } => vec![*base],
                MemLoc::Slot { .. } | MemLoc::Incoming { .. } => {
                    if config.omit_frame_pointer {
                        vec![regs::sp().into()]
                    } else {
                        vec![regs::fp().into()]
                    }
                }
            },
            Ik::Store { src, loc, .. } => {
                let mut uses = vec![*src];
                match loc {
                    MemLoc::RegOffset { base, .. } => uses.push(*base),
                    // XXX: Slot will use a frame pointer to index local variables
                    // but if it is omitted, it will fall back to the stack pointer
                    MemLoc::Slot { .. } | MemLoc::Incoming { .. } => {
                        if config.omit_frame_pointer {
                            uses.push(regs::sp().into());
                        } else {
                            uses.push(regs::fp().into());
                        }
                    }
                }
                uses
            }
            Ik::Ret => vec![],
            Ik::Call { arg_regs, .. } => arg_regs.iter().map(|r| (*r).into()).collect(),
            Ik::J { .. } => vec![],
            Ik::Br { rs1, rs2, .. } => vec![*rs1, *rs2],
            Ik::La { .. } => vec![],
            Ik::LoadAddr { loc, .. } => match loc {
                MemLoc::RegOffset { base, .. } => vec![*base],
                MemLoc::Slot { .. } | MemLoc::Incoming { .. } => {
                    if config.omit_frame_pointer {
                        vec![regs::sp().into()]
                    } else {
                        vec![regs::fp().into()]
                    }
                }
            },
        }
    }

    fn defs(self, mctx: &MContext<Self>, _config: &LowerConfig) -> Vec<Reg> {
        use RvInstKind as Ik;

        match &self.deref(mctx).kind {
            Ik::Li { rd, .. } => vec![*rd],
            Ik::AluRR { rd, .. } => vec![*rd],
            Ik::AluRRI { rd, .. } => vec![*rd],
            Ik::AluRRR { rd, .. } => vec![*rd],
            Ik::FpuRR { rd, .. } => vec![*rd],
            Ik::FpuRRR { rd, .. } => vec![*rd],
            Ik::FpuRRRR { rd, .. } => vec![*rd],
            Ik::Load { rd, .. } => vec![*rd],
            Ik::Store { .. } => vec![],
            Ik::Ret => vec![],
            Ik::Call { .. } => regs::CALLER_SAVED_REGS
                .iter()
                .map(|r| (*r).into())
                .collect(),
            Ik::J { .. } => vec![],
            Ik::Br { .. } => vec![],
            Ik::La { rd, .. } => vec![*rd],
            Ik::LoadAddr { rd, .. } => vec![*rd],
        }
    }

    fn is_terminator(self, mctx: &MContext<Self>) -> bool {
        use RvInstKind as Ik;

        matches!(&self.deref(mctx).kind, Ik::J { .. } | Ik::Br { .. })
    }

    fn succs(self, mctx: &MContext<Self>) -> Vec<MBlock<Self>> {
        use RvInstKind as Ik;

        if let Ik::J { block } = &self.deref(mctx).kind {
            vec![*block]
        } else if let Ik::Br { block, .. } = &self.deref(mctx).kind {
            vec![*block]
        } else {
            vec![]
        }
    }

    fn adjust_offset<F>(self, mctx: &mut MContext<Self>, f: F, _config: &LowerConfig)
    where
        F: FnOnce(MemLoc) -> Option<MemLoc>,
    {
        use RvInstKind as Ik;

        let (old_loc, new_loc) = match self.kind(mctx) {
            Ik::Load { loc, .. } => (*loc, f(*loc)),
            Ik::Store { loc, .. } => (*loc, f(*loc)),
            Ik::LoadAddr { loc, .. } => (*loc, f(*loc)),
            Ik::Li { .. }
            | Ik::AluRR { .. }
            | Ik::AluRRI { .. }
            | Ik::AluRRR { .. }
            | Ik::FpuRR { .. }
            | Ik::FpuRRR { .. }
            | Ik::FpuRRRR { .. }
            | Ik::Ret
            | Ik::Call { .. }
            | Ik::J { .. }
            | Ik::Br { .. }
            | Ik::La { .. } => return,
        };

        if new_loc.is_none() {
            // no modification is needed
            return;
        }

        let new_loc = match (old_loc, new_loc.unwrap()) {
            (MemLoc::Slot { .. } | MemLoc::Incoming { .. }, MemLoc::RegOffset { base, offset }) => {
                if Imm12::try_from_i64(offset).is_none() {
                    let t0 = regs::t0();
                    let li = Self::build_li(mctx, t0.into(), offset as u64);
                    self.insert_before(mctx, li);
                    MemLoc::RegOffset {
                        base: t0.into(),
                        offset: 0,
                    }
                } else {
                    MemLoc::RegOffset { base, offset }
                }
            }
            _ => unreachable!(),
        };

        match &mut self.deref_mut(mctx).kind {
            Ik::Load { loc, .. } => *loc = new_loc,
            Ik::Store { loc, .. } => *loc = new_loc,
            Ik::LoadAddr { rd, .. } => {
                // we need to remove this instruction and replace with addi or add
                match new_loc {
                    MemLoc::RegOffset { base, offset } => {
                        let rd = *rd;
                        let addi = Self::build_alu_rri(
                            mctx,
                            AluOpRRI::Addi,
                            rd,
                            base,
                            Imm12::try_from_i64(offset).unwrap(),
                        );
                        self.insert_before(mctx, addi);
                        self.remove(mctx);
                    }
                    MemLoc::Slot { .. } | MemLoc::Incoming { .. } => unreachable!(),
                }
            }
            Ik::Li { .. }
            | Ik::AluRR { .. }
            | Ik::AluRRI { .. }
            | Ik::AluRRR { .. }
            | Ik::FpuRR { .. }
            | Ik::FpuRRR { .. }
            | Ik::FpuRRRR { .. }
            | Ik::Ret
            | Ik::Call { .. }
            | Ik::J { .. }
            | Ik::Br { .. }
            | Ik::La { .. } => unreachable!(),
        }
    }
}

impl ArenaPtr for RvInst {
    type A = MContext<Self>;
    type T = RvInstData;

    fn try_deref(self, arena: &Self::A) -> Option<&Self::T> { ArenaDeref::try_deref(arena, self) }

    fn try_deref_mut(self, arena: &mut Self::A) -> Option<&mut Self::T> {
        ArenaDeref::try_deref_mut(arena, self)
    }
}

impl LinkedListNodePtr for RvInst {
    type ContainerPtr = MBlock<Self>;

    fn next(self, arena: &Self::A) -> Option<Self> { self.deref(arena).next }

    fn prev(self, arena: &Self::A) -> Option<Self> { self.deref(arena).prev }

    fn set_next(self, arena: &mut Self::A, next: Option<Self>) {
        self.deref_mut(arena).next = next;
    }

    fn set_prev(self, arena: &mut Self::A, prev: Option<Self>) {
        self.deref_mut(arena).prev = prev;
    }

    fn container(self, arena: &Self::A) -> Option<Self::ContainerPtr> { self.deref(arena).parent }

    fn set_container(self, arena: &mut Self::A, container: Option<Self::ContainerPtr>) {
        self.deref_mut(arena).parent = container;
    }
}
