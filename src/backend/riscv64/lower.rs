use super::{
    imm::Imm12,
    inst::{AluOpRRI, AluOpRRR, BrOp, FpuOpRRR, Frm, LoadOp, RvInst, StoreOp},
    regs::{self, CALLEE_SAVED_REGS, CALLER_SAVED_REGS},
};
use crate::{
    backend::{
        func::MLabel,
        lower::{LowerContext, LowerSpec, MValue, MValueKind, MemLoc},
        regs::Reg,
        riscv64::inst::FpuOpRR,
        MBlock,
        MFunc,
        PReg,
        RegKind,
    },
    collections::{
        apint::ApInt,
        linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    },
    ir,
};

pub struct RvLowerSpec;

impl LowerSpec for RvLowerSpec {
    type I = RvInst;

    fn stack_align() -> u32 { 16 }

    fn frame_pointer() -> PReg { regs::fp() }

    fn stack_pointer() -> PReg { regs::sp() }

    fn pointer_size() -> usize { 64 }

    fn allocatable_gp_regs() -> Vec<PReg> {
        vec![
            regs::t1(),
            regs::t2(),
            regs::t3(),
            regs::t4(),
            regs::t5(),
            regs::t6(),
            regs::a0(),
            regs::a1(),
            regs::a2(),
            regs::a3(),
            regs::a4(),
            regs::a5(),
            regs::a6(),
            regs::a7(),
            regs::s0(),
            regs::s1(),
            regs::s2(),
            regs::s3(),
            regs::s4(),
            regs::s5(),
            regs::s6(),
            regs::s7(),
            regs::s8(),
            regs::s9(),
            regs::s10(),
            regs::s11(),
            regs::ra(),
        ]
    }

    fn allocatable_fp_regs() -> Vec<PReg> {
        vec![
            regs::fa0(),
            regs::fa1(),
            regs::fa2(),
            regs::fa3(),
            regs::fa4(),
            regs::fa5(),
            regs::fa6(),
            regs::fa7(),
            regs::ft0(),
            regs::ft1(),
            regs::ft2(),
            regs::ft3(),
            regs::ft4(),
            regs::ft5(),
            regs::ft6(),
            regs::ft7(),
            regs::ft8(),
            regs::ft9(),
            regs::ft10(),
            regs::ft11(),
            regs::fs0(),
            regs::fs1(),
            regs::fs2(),
            regs::fs3(),
            regs::fs4(),
            regs::fs5(),
            regs::fs6(),
            regs::fs7(),
            regs::fs8(),
            regs::fs9(),
            regs::fs10(),
            regs::fs11(),
        ]
    }

    fn non_allocatable_regs() -> Vec<PReg> {
        vec![regs::zero(), regs::t0(), regs::sp(), regs::gp(), regs::tp()]
    }

    fn callee_saved_regs() -> Vec<PReg> { CALLEE_SAVED_REGS.to_vec() }

    fn caller_saved_regs() -> Vec<PReg> { CALLER_SAVED_REGS.to_vec() }

    fn return_reg(ctx: &ir::Context, ty: ir::Ty) -> PReg {
        if ty.is_integer(ctx) || ty.is_ptr(ctx) {
            regs::a0()
        } else if ty.is_float(ctx) {
            regs::fa0()
        } else {
            unimplemented!("return_reg: {:?}", ty)
        }
    }

    fn total_stack_size(lower: &mut LowerContext<Self>, mfunc: MFunc<Self::I>) -> u64 {
        let saved_regs = mfunc.saved_regs(&lower.mctx);
        let raw = mfunc.storage_stack_size(&lower.mctx)
            + mfunc.outgoing_stack_size(&lower.mctx)
            + saved_regs.len() as u64 * 8;
        (raw + 15) & !15
    }

    fn gen_move(lower: &mut LowerContext<Self>, dst: Reg, src: MValue) {
        let curr_block = lower.curr_block.unwrap();
        let src_ty = src.ty();
        match src.kind() {
            MValueKind::Reg(reg) => {
                let inst = if src_ty.is_integer(lower.ctx) || src_ty.is_ptr(lower.ctx) {
                    RvInst::build_alu_rri(
                        &mut lower.mctx,
                        AluOpRRI::Addi,
                        dst,
                        reg,
                        Imm12::try_from_i64(0).unwrap(),
                    )
                } else if src_ty.is_float32(lower.ctx) {
                    RvInst::build_fpu_rrr(
                        &mut lower.mctx,
                        FpuOpRRR::FsgnjS,
                        Frm::Dyn,
                        dst,
                        reg,
                        reg,
                    )
                } else if src_ty.is_float64(lower.ctx) {
                    RvInst::build_fpu_rrr(
                        &mut lower.mctx,
                        FpuOpRRR::FsgnjD,
                        Frm::Dyn,
                        dst,
                        reg,
                        reg,
                    )
                } else {
                    unimplemented!("gen_move: {:?}", src_ty)
                };
                curr_block.push_back(&mut lower.mctx, inst);
            }
            MValueKind::Imm(imm) => {
                if src_ty.is_integer(lower.ctx) || src_ty.is_ptr(lower.ctx) {
                    let li = RvInst::build_li(&mut lower.mctx, dst, imm as u64);
                    curr_block.push_back(&mut lower.mctx, li);
                } else {
                    // floating point should be loaded into register, we haven't support fli yet
                    unreachable!("gen_move: got imm of type {:?}", src_ty);
                }
            }
            MValueKind::Mem(loc) => match loc {
                MemLoc::RegOffset { base, offset } => {
                    if let Some(imm) = Imm12::try_from_i64(offset) {
                        let inst =
                            RvInst::build_alu_rri(&mut lower.mctx, AluOpRRI::Addi, dst, base, imm);
                        curr_block.push_back(&mut lower.mctx, inst);
                    } else {
                        let (li, t) = RvInst::li(&mut lower.mctx, offset as u64);
                        curr_block.push_back(&mut lower.mctx, li);
                        let inst =
                            RvInst::build_alu_rrr(&mut lower.mctx, AluOpRRR::Add, dst, base, t);
                        curr_block.push_back(&mut lower.mctx, inst);
                    }
                }
                MemLoc::Slot { .. } | MemLoc::Incoming { .. } => {
                    // we cannot get the actual offset now, so we must rely on future modification
                    // of load addr instruction.
                    let inst = RvInst::build_load_addr(&mut lower.mctx, dst, loc);
                    curr_block.push_back(&mut lower.mctx, inst);
                }
            },
            MValueKind::Undef => {
                // do nothing and propagate undef
            }
        }
    }

    fn gen_iconst(lower: &mut LowerContext<Self>, x: &ApInt, ty: ir::Ty) -> MValue {
        if !ty.is_integer(lower.ctx) {
            panic!("gen_iconst: expected integer type, got {:?}", ty);
        }

        let bitwidth = ty.bitwidth_with_ptr(lower.ctx, Self::pointer_size());

        if bitwidth > 64 {
            unimplemented!("gen_iconst: bitwidth > 64: {}", bitwidth);
        }

        let x = if x.width() > bitwidth {
            // TODO: should be handled before lowering
            let (x, _) = x.clone().into_truncated(bitwidth);
            x
        } else {
            x.clone()
        };

        if x.is_zero() {
            return MValue::new_reg(ty, regs::zero());
        }

        if let Some(imm) = Imm12::try_from_apint(&x) {
            MValue::new_imm(ty, imm.as_i16() as i64)
        } else {
            let bits = u64::from(x.clone());
            let (li, t) = RvInst::li(&mut lower.mctx, bits);
            lower.curr_block.unwrap().push_back(&mut lower.mctx, li);
            MValue::new_reg(ty, t)
        }
    }

    fn gen_fconst(lower: &mut LowerContext<Self>, x: &ir::FloatConstant, ty: ir::Ty) -> MValue {
        if !ty.is_float32(lower.ctx) && !ty.is_float64(lower.ctx) {
            panic!("gen_fconst: expected float type, got {:?}", ty);
        }
        let x = if ty.is_float64(lower.ctx) {
            x.promote()
        } else {
            *x
        };
        let t: Reg = match x {
            ir::FloatConstant::Float32(bits) => {
                if bits == 0 {
                    regs::zero().into()
                } else {
                    let (li, t) = RvInst::li(&mut lower.mctx, bits as u64);
                    lower.curr_block.unwrap().push_back(&mut lower.mctx, li);
                    t
                }
            }
            ir::FloatConstant::Float64(bits) => {
                if bits == 0 {
                    regs::zero().into()
                } else {
                    let (li, t) = RvInst::li(&mut lower.mctx, bits);
                    lower.curr_block.unwrap().push_back(&mut lower.mctx, li);
                    t
                }
            }
        };
        // fmv.w.x or fmv.d.x
        if ty.is_float32(lower.ctx) {
            let (fmv, t) = RvInst::fpu_rr(&mut lower.mctx, FpuOpRR::FmvWX, Frm::Dyn, t);
            lower.curr_block.unwrap().push_back(&mut lower.mctx, fmv);
            MValue::new_reg(ty, t)
        } else if ty.is_float64(lower.ctx) {
            let (fmv, t) = RvInst::fpu_rr(&mut lower.mctx, FpuOpRR::FmvDX, Frm::Dyn, t);
            lower.curr_block.unwrap().push_back(&mut lower.mctx, fmv);
            MValue::new_reg(ty, t)
        } else {
            unreachable!();
        }
    }

    fn gen_ibinary(
        lower: &mut LowerContext<Self>,
        op: ir::IBinaryOp,
        lhs: MValue,
        rhs: MValue,
        dst_ty: ir::Ty,
    ) -> MValue {
        use ir::IBinaryOp as Ibop;

        let curr_block = lower.curr_block.unwrap();

        let ty = lhs.ty();

        if lhs.is_undef() || rhs.is_undef() {
            return MValue::new_undef(dst_ty);
        }

        let bitwidth = ty.bitwidth(lower.ctx).unwrap();

        match op {
            Ibop::Add => {
                // reg reg -> add
                // imm reg / reg imm -> addi
                // imm imm -> li + add / addi
                let addi_op = if bitwidth == 32 {
                    AluOpRRI::Addiw
                } else if bitwidth == 64 {
                    AluOpRRI::Addi
                } else {
                    unimplemented!("gen_ibinary: bitwidth: {}", bitwidth);
                };

                let add_op = if bitwidth == 32 {
                    AluOpRRR::Addw
                } else if bitwidth == 64 {
                    AluOpRRR::Add
                } else {
                    unimplemented!("gen_ibinary: bitwidth: {}", bitwidth);
                };

                match (lhs.kind(), rhs.kind()) {
                    (MValueKind::Reg(lhs), MValueKind::Reg(rhs)) => {
                        let (inst, reg) = RvInst::alu_rrr(&mut lower.mctx, add_op, lhs, rhs);
                        curr_block.push_back(&mut lower.mctx, inst);
                        MValue::new_reg(ty, reg)
                    }
                    (MValueKind::Imm(imm), MValueKind::Reg(reg)) => {
                        // the imm must be valid as imm12
                        let (inst, reg) = RvInst::alu_rri(
                            &mut lower.mctx,
                            addi_op,
                            reg,
                            Imm12::try_from_i64(imm).unwrap(),
                        );
                        curr_block.push_back(&mut lower.mctx, inst);
                        MValue::new_reg(ty, reg)
                    }
                    (MValueKind::Reg(reg), MValueKind::Imm(imm)) => {
                        let (inst, reg) = RvInst::alu_rri(
                            &mut lower.mctx,
                            addi_op,
                            reg,
                            Imm12::try_from_i64(imm).unwrap(),
                        );
                        curr_block.push_back(&mut lower.mctx, inst);
                        MValue::new_reg(ty, reg)
                    }
                    (MValueKind::Imm(lhs), MValueKind::Imm(rhs)) => {
                        let (li, t) = RvInst::li(&mut lower.mctx, lhs as u64);
                        curr_block.push_back(&mut lower.mctx, li);
                        let (inst, reg) = RvInst::alu_rri(
                            &mut lower.mctx,
                            addi_op,
                            t,
                            Imm12::try_from_i64(rhs).unwrap(),
                        );
                        curr_block.push_back(&mut lower.mctx, inst);
                        MValue::new_reg(ty, reg)
                    }
                    _ => unreachable!(),
                }
            }
            Ibop::Sub => {
                // reg reg -> sub
                // imm reg -> li + sub
                // reg imm -> addi
                // imm imm -> li + addi / li + sub

                let addi_op = if bitwidth == 32 {
                    AluOpRRI::Addiw
                } else if bitwidth == 64 {
                    AluOpRRI::Addi
                } else {
                    unimplemented!("gen_ibinary: bitwidth: {}", bitwidth);
                };

                let sub_op = if bitwidth == 32 {
                    AluOpRRR::Subw
                } else if bitwidth == 64 {
                    AluOpRRR::Sub
                } else {
                    unimplemented!("gen_ibinary: bitwidth: {}", bitwidth);
                };

                match (lhs.kind(), rhs.kind()) {
                    (MValueKind::Reg(lhs), MValueKind::Reg(rhs)) => {
                        let (inst, reg) = RvInst::alu_rrr(&mut lower.mctx, sub_op, lhs, rhs);
                        curr_block.push_back(&mut lower.mctx, inst);
                        MValue::new_reg(ty, reg)
                    }
                    (MValueKind::Imm(imm), MValueKind::Reg(reg)) => {
                        // not commutative
                        let (li, t) = RvInst::li(&mut lower.mctx, imm as u64);
                        curr_block.push_back(&mut lower.mctx, li);
                        let (inst, reg) = RvInst::alu_rrr(&mut lower.mctx, sub_op, t, reg);
                        curr_block.push_back(&mut lower.mctx, inst);
                        MValue::new_reg(ty, reg)
                    }
                    (MValueKind::Reg(reg), MValueKind::Imm(imm)) => {
                        // we cannot be sure if the negation of imm is a valid imm12
                        if let Some(imm) = Imm12::try_from_i64(-imm) {
                            let (inst, reg) = RvInst::alu_rri(&mut lower.mctx, addi_op, reg, imm);
                            curr_block.push_back(&mut lower.mctx, inst);
                            MValue::new_reg(ty, reg)
                        } else {
                            let (li, t) = RvInst::li(&mut lower.mctx, imm as u64);
                            curr_block.push_back(&mut lower.mctx, li);
                            let (inst, reg) = RvInst::alu_rrr(&mut lower.mctx, sub_op, reg, t);
                            curr_block.push_back(&mut lower.mctx, inst);
                            MValue::new_reg(ty, reg)
                        }
                    }
                    (MValueKind::Imm(lhs), MValueKind::Imm(rhs)) => {
                        let (li, lhs) = RvInst::li(&mut lower.mctx, lhs as u64);
                        curr_block.push_back(&mut lower.mctx, li);
                        if let Some(imm) = Imm12::try_from_i64(-rhs) {
                            let (inst, reg) = RvInst::alu_rri(&mut lower.mctx, addi_op, lhs, imm);
                            curr_block.push_back(&mut lower.mctx, inst);
                            MValue::new_reg(ty, reg)
                        } else {
                            let (li, t) = RvInst::li(&mut lower.mctx, rhs as u64);
                            curr_block.push_back(&mut lower.mctx, li);
                            let (inst, reg) = RvInst::alu_rrr(&mut lower.mctx, sub_op, lhs, t);
                            curr_block.push_back(&mut lower.mctx, inst);
                            MValue::new_reg(ty, reg)
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Ibop::Mul | Ibop::UDiv | Ibop::URem | Ibop::SDiv | Ibop::SRem => {
                let lhs = match lhs.kind() {
                    MValueKind::Reg(reg) => reg,
                    MValueKind::Imm(imm) => {
                        let (li, reg) = RvInst::li(&mut lower.mctx, imm as u64);
                        curr_block.push_back(&mut lower.mctx, li);
                        reg
                    }
                    MValueKind::Mem(_) | MValueKind::Undef => unreachable!(),
                };
                let rhs = match rhs.kind() {
                    MValueKind::Reg(reg) => reg,
                    MValueKind::Imm(imm) => {
                        let (li, reg) = RvInst::li(&mut lower.mctx, imm as u64);
                        curr_block.push_back(&mut lower.mctx, li);
                        reg
                    }
                    MValueKind::Mem(_) | MValueKind::Undef => unreachable!(),
                };
                let op = if bitwidth == 32 {
                    match op {
                        Ibop::Mul => AluOpRRR::Mulw,
                        Ibop::UDiv => AluOpRRR::Divuw,
                        Ibop::URem => AluOpRRR::Remuw,
                        Ibop::SDiv => AluOpRRR::Divw,
                        Ibop::SRem => AluOpRRR::Remw,
                        Ibop::Add
                        | Ibop::Sub
                        | Ibop::And
                        | Ibop::Or
                        | Ibop::Xor
                        | Ibop::Shl
                        | Ibop::LShr
                        | Ibop::AShr
                        | Ibop::Cmp(_) => unreachable!(),
                    }
                } else if bitwidth == 64 {
                    match op {
                        Ibop::Mul => AluOpRRR::Mul,
                        Ibop::UDiv => AluOpRRR::Divu,
                        Ibop::URem => AluOpRRR::Remu,
                        Ibop::SDiv => AluOpRRR::Div,
                        Ibop::SRem => AluOpRRR::Rem,
                        Ibop::Add
                        | Ibop::Sub
                        | Ibop::And
                        | Ibop::Or
                        | Ibop::Xor
                        | Ibop::Shl
                        | Ibop::LShr
                        | Ibop::AShr
                        | Ibop::Cmp(_) => unreachable!(),
                    }
                } else {
                    unimplemented!("gen_ibinary: bitwidth: {}", bitwidth);
                };
                let (inst, reg) = RvInst::alu_rrr(&mut lower.mctx, op, lhs, rhs);
                curr_block.push_back(&mut lower.mctx, inst);
                MValue::new_reg(ty, reg)
            }
            Ibop::And | Ibop::Or | Ibop::Xor => {
                let imm_op = match op {
                    Ibop::And => AluOpRRI::Andi,
                    Ibop::Or => AluOpRRI::Ori,
                    Ibop::Xor => AluOpRRI::Xori,
                    Ibop::Add
                    | Ibop::Sub
                    | Ibop::Mul
                    | Ibop::UDiv
                    | Ibop::SDiv
                    | Ibop::URem
                    | Ibop::SRem
                    | Ibop::Shl
                    | Ibop::LShr
                    | Ibop::AShr
                    | Ibop::Cmp(_) => unreachable!(),
                };
                let reg_op = match op {
                    Ibop::And => AluOpRRR::And,
                    Ibop::Or => AluOpRRR::Or,
                    Ibop::Xor => AluOpRRR::Xor,
                    Ibop::Add
                    | Ibop::Sub
                    | Ibop::Mul
                    | Ibop::UDiv
                    | Ibop::SDiv
                    | Ibop::URem
                    | Ibop::SRem
                    | Ibop::Shl
                    | Ibop::LShr
                    | Ibop::AShr
                    | Ibop::Cmp(_) => unreachable!(),
                };

                match (lhs.kind(), rhs.kind()) {
                    (MValueKind::Reg(lhs), MValueKind::Reg(rhs)) => {
                        let (inst, reg) = RvInst::alu_rrr(&mut lower.mctx, reg_op, lhs, rhs);
                        curr_block.push_back(&mut lower.mctx, inst);
                        MValue::new_reg(ty, reg)
                    }
                    (MValueKind::Imm(imm), MValueKind::Reg(reg)) => {
                        let (inst, reg) = RvInst::alu_rri(
                            &mut lower.mctx,
                            imm_op,
                            reg,
                            Imm12::try_from_i64(imm).unwrap(),
                        );
                        curr_block.push_back(&mut lower.mctx, inst);
                        MValue::new_reg(ty, reg)
                    }
                    (MValueKind::Reg(reg), MValueKind::Imm(imm)) => {
                        let (inst, reg) = RvInst::alu_rri(
                            &mut lower.mctx,
                            imm_op,
                            reg,
                            Imm12::try_from_i64(imm).unwrap(),
                        );
                        curr_block.push_back(&mut lower.mctx, inst);
                        MValue::new_reg(ty, reg)
                    }
                    (MValueKind::Imm(lhs), MValueKind::Imm(rhs)) => {
                        let (li, lhs) = RvInst::li(&mut lower.mctx, lhs as u64);
                        curr_block.push_back(&mut lower.mctx, li);
                        let (inst, reg) = RvInst::alu_rri(
                            &mut lower.mctx,
                            imm_op,
                            lhs,
                            Imm12::try_from_i64(rhs).unwrap(),
                        );
                        curr_block.push_back(&mut lower.mctx, inst);
                        MValue::new_reg(ty, reg)
                    }
                    _ => unreachable!(),
                }
            }
            Ibop::Shl | Ibop::LShr | Ibop::AShr => {
                let imm_op = match op {
                    Ibop::Shl => AluOpRRI::Slli,
                    Ibop::LShr => AluOpRRI::Srli,
                    Ibop::AShr => AluOpRRI::Srai,
                    Ibop::Add
                    | Ibop::Sub
                    | Ibop::Mul
                    | Ibop::UDiv
                    | Ibop::SDiv
                    | Ibop::URem
                    | Ibop::SRem
                    | Ibop::And
                    | Ibop::Or
                    | Ibop::Xor
                    | Ibop::Cmp(_) => unreachable!(),
                };

                let reg_op = match op {
                    Ibop::Shl => AluOpRRR::Sll,
                    Ibop::LShr => AluOpRRR::Srl,
                    Ibop::AShr => AluOpRRR::Sra,
                    Ibop::Add
                    | Ibop::Sub
                    | Ibop::Mul
                    | Ibop::UDiv
                    | Ibop::SDiv
                    | Ibop::URem
                    | Ibop::SRem
                    | Ibop::And
                    | Ibop::Or
                    | Ibop::Xor
                    | Ibop::Cmp(_) => unreachable!(),
                };

                match (lhs.kind(), rhs.kind()) {
                    (MValueKind::Reg(lhs), MValueKind::Reg(rhs)) => {
                        let (inst, reg) = RvInst::alu_rrr(&mut lower.mctx, reg_op, lhs, rhs);
                        curr_block.push_back(&mut lower.mctx, inst);
                        MValue::new_reg(ty, reg)
                    }
                    (MValueKind::Imm(imm), MValueKind::Reg(reg)) => {
                        let (li, lhs) = RvInst::li(&mut lower.mctx, imm as u64);
                        curr_block.push_back(&mut lower.mctx, li);
                        let (inst, reg) = RvInst::alu_rrr(&mut lower.mctx, reg_op, lhs, reg);
                        curr_block.push_back(&mut lower.mctx, inst);
                        MValue::new_reg(ty, reg)
                    }
                    (MValueKind::Reg(reg), MValueKind::Imm(imm)) => {
                        let (inst, reg) = RvInst::alu_rri(
                            &mut lower.mctx,
                            imm_op,
                            reg,
                            Imm12::try_from_i64(imm).unwrap(),
                        );
                        curr_block.push_back(&mut lower.mctx, inst);
                        MValue::new_reg(ty, reg)
                    }
                    (MValueKind::Imm(lhs), MValueKind::Imm(rhs)) => {
                        let (li, lhs) = RvInst::li(&mut lower.mctx, lhs as u64);
                        curr_block.push_back(&mut lower.mctx, li);
                        let (inst, reg) = RvInst::alu_rri(
                            &mut lower.mctx,
                            imm_op,
                            lhs,
                            Imm12::try_from_i64(rhs).unwrap(),
                        );
                        curr_block.push_back(&mut lower.mctx, inst);
                        MValue::new_reg(ty, reg)
                    }
                    _ => unreachable!(),
                }
            }
            Ibop::Cmp(cond) => {
                match cond {
                    ir::ICmpCond::Eq | ir::ICmpCond::Ne => {
                        let addi_op = if bitwidth == 32 {
                            AluOpRRI::Addiw
                        } else if bitwidth == 64 {
                            AluOpRRI::Addi
                        } else {
                            unimplemented!("{}", ty.display(lower.ctx));
                        };

                        let sub_op = if bitwidth == 32 {
                            AluOpRRR::Subw
                        } else if bitwidth == 64 {
                            AluOpRRR::Sub
                        } else {
                            unimplemented!()
                        };

                        // sub rd, rs1, rs2 or addi if imm
                        let reg = match (lhs.kind(), rhs.kind()) {
                            (MValueKind::Reg(lhs), MValueKind::Reg(rhs)) => {
                                let (inst, reg) =
                                    RvInst::alu_rrr(&mut lower.mctx, sub_op, lhs, rhs);
                                curr_block.push_back(&mut lower.mctx, inst);
                                reg
                            }
                            (MValueKind::Imm(imm), MValueKind::Reg(reg)) => {
                                let (li, lhs) = RvInst::li(&mut lower.mctx, imm as u64);
                                curr_block.push_back(&mut lower.mctx, li);
                                let (inst, reg) =
                                    RvInst::alu_rrr(&mut lower.mctx, sub_op, lhs, reg);
                                curr_block.push_back(&mut lower.mctx, inst);
                                reg
                            }
                            (MValueKind::Reg(reg), MValueKind::Imm(imm)) => {
                                if let Some(imm) = Imm12::try_from_i64(-imm) {
                                    let (inst, reg) =
                                        RvInst::alu_rri(&mut lower.mctx, addi_op, reg, imm);
                                    curr_block.push_back(&mut lower.mctx, inst);
                                    reg
                                } else {
                                    let (li, rhs) = RvInst::li(&mut lower.mctx, imm as u64);
                                    curr_block.push_back(&mut lower.mctx, li);
                                    let (inst, reg) =
                                        RvInst::alu_rrr(&mut lower.mctx, sub_op, reg, rhs);
                                    curr_block.push_back(&mut lower.mctx, inst);
                                    reg
                                }
                            }
                            (MValueKind::Imm(lhs), MValueKind::Imm(rhs)) => {
                                let (li, lhs) = RvInst::li(&mut lower.mctx, lhs as u64);
                                curr_block.push_back(&mut lower.mctx, li);
                                if let Some(imm) = Imm12::try_from_i64(-rhs) {
                                    let (inst, reg) =
                                        RvInst::alu_rri(&mut lower.mctx, addi_op, lhs, imm);
                                    curr_block.push_back(&mut lower.mctx, inst);
                                    reg
                                } else {
                                    let (li, rhs) = RvInst::li(&mut lower.mctx, rhs as u64);
                                    curr_block.push_back(&mut lower.mctx, li);
                                    let (inst, reg) =
                                        RvInst::alu_rrr(&mut lower.mctx, sub_op, lhs, rhs);
                                    curr_block.push_back(&mut lower.mctx, inst);
                                    reg
                                }
                            }
                            _ => unreachable!(),
                        };
                        match cond {
                            ir::ICmpCond::Eq => {
                                // sltiu
                                let (inst, reg) = RvInst::alu_rri(
                                    &mut lower.mctx,
                                    AluOpRRI::Sltiu,
                                    reg,
                                    Imm12::try_from_i64(1).unwrap(),
                                );
                                curr_block.push_back(&mut lower.mctx, inst);
                                MValue::new_reg(dst_ty, reg)
                            }
                            ir::ICmpCond::Ne => {
                                // sltu
                                let (inst, reg) = RvInst::alu_rrr(
                                    &mut lower.mctx,
                                    AluOpRRR::Sltu,
                                    regs::zero().into(),
                                    reg,
                                );
                                curr_block.push_back(&mut lower.mctx, inst);
                                MValue::new_reg(dst_ty, reg)
                            }
                            ir::ICmpCond::Slt
                            | ir::ICmpCond::Sle
                            | ir::ICmpCond::Ult
                            | ir::ICmpCond::Ule => unreachable!(),
                        }
                    }
                    ir::ICmpCond::Slt
                    | ir::ICmpCond::Sle
                    | ir::ICmpCond::Ult
                    | ir::ICmpCond::Ule => {
                        let imm_op = match cond {
                            ir::ICmpCond::Slt | ir::ICmpCond::Sle => AluOpRRI::Slti,
                            ir::ICmpCond::Ult | ir::ICmpCond::Ule => AluOpRRI::Sltiu,
                            ir::ICmpCond::Eq | ir::ICmpCond::Ne => unreachable!(),
                        };
                        let reg_op = match cond {
                            ir::ICmpCond::Slt | ir::ICmpCond::Sle => AluOpRRR::Slt,
                            ir::ICmpCond::Ult | ir::ICmpCond::Ule => AluOpRRR::Sltu,
                            ir::ICmpCond::Eq | ir::ICmpCond::Ne => unreachable!(),
                        };
                        let swap = matches!(cond, ir::ICmpCond::Sle | ir::ICmpCond::Ule);

                        let (lhs, rhs) = if swap { (rhs, lhs) } else { (lhs, rhs) };

                        let reg = match (lhs.kind(), rhs.kind()) {
                            (MValueKind::Reg(lhs), MValueKind::Reg(rhs)) => {
                                let (inst, reg) =
                                    RvInst::alu_rrr(&mut lower.mctx, reg_op, lhs, rhs);
                                curr_block.push_back(&mut lower.mctx, inst);
                                reg
                            }
                            (MValueKind::Imm(imm), MValueKind::Reg(reg)) => {
                                let (li, lhs) = RvInst::li(&mut lower.mctx, imm as u64);
                                curr_block.push_back(&mut lower.mctx, li);
                                let (inst, reg) =
                                    RvInst::alu_rrr(&mut lower.mctx, reg_op, lhs, reg);
                                curr_block.push_back(&mut lower.mctx, inst);
                                reg
                            }
                            (MValueKind::Reg(reg), MValueKind::Imm(imm)) => {
                                let (inst, reg) = RvInst::alu_rri(
                                    &mut lower.mctx,
                                    imm_op,
                                    reg,
                                    Imm12::try_from_i64(imm).unwrap(),
                                );
                                curr_block.push_back(&mut lower.mctx, inst);
                                reg
                            }
                            (MValueKind::Imm(lhs), MValueKind::Imm(rhs)) => {
                                let (li, lhs) = RvInst::li(&mut lower.mctx, lhs as u64);
                                curr_block.push_back(&mut lower.mctx, li);
                                let (inst, reg) = RvInst::alu_rri(
                                    &mut lower.mctx,
                                    imm_op,
                                    lhs,
                                    Imm12::try_from_i64(rhs).unwrap(),
                                );
                                curr_block.push_back(&mut lower.mctx, inst);
                                reg
                            }
                            _ => unreachable!(),
                        };

                        // also xor with 1 if sle or ule
                        let reg = if cond == ir::ICmpCond::Sle || cond == ir::ICmpCond::Ule {
                            let (inst, reg) = RvInst::alu_rri(
                                &mut lower.mctx,
                                AluOpRRI::Xori,
                                reg,
                                Imm12::try_from_i64(1).unwrap(),
                            );
                            curr_block.push_back(&mut lower.mctx, inst);
                            reg
                        } else {
                            reg
                        };

                        MValue::new_reg(dst_ty, reg)
                    }
                }
            }
        }
    }

    fn gen_fbinary(
        lower: &mut LowerContext<Self>,
        op: ir::FBinaryOp,
        lhs: MValue,
        rhs: MValue,
        dst_ty: ir::Ty,
    ) -> MValue {
        use ir::FBinaryOp as Fbop;

        let curr_block = lower.curr_block.unwrap();

        let ty = lhs.ty();

        let bitwidth = ty.bitwidth(lower.ctx).unwrap();

        if lhs.is_undef() || rhs.is_undef() {
            return MValue::new_undef(dst_ty);
        }

        let lhs = if let MValueKind::Reg(reg) = lhs.kind() {
            reg
        } else {
            unreachable!()
        };

        let rhs = if let MValueKind::Reg(reg) = rhs.kind() {
            reg
        } else {
            unreachable!()
        };

        match op {
            Fbop::Add | Fbop::Sub | Fbop::Mul | Fbop::Div | Fbop::Rem => {
                let fpu_op = if bitwidth == 32 {
                    match op {
                        Fbop::Add => FpuOpRRR::FaddS,
                        Fbop::Sub => FpuOpRRR::FsubS,
                        Fbop::Mul => FpuOpRRR::FmulS,
                        Fbop::Div => FpuOpRRR::FdivS,
                        Fbop::Rem => unimplemented!(),
                        _ => unreachable!(),
                    }
                } else {
                    match op {
                        Fbop::Add => FpuOpRRR::FaddD,
                        Fbop::Sub => FpuOpRRR::FsubD,
                        Fbop::Mul => FpuOpRRR::FmulD,
                        Fbop::Div => FpuOpRRR::FdivD,
                        Fbop::Rem => unimplemented!(),
                        _ => unreachable!(),
                    }
                };

                let (inst, rd) = RvInst::fpu_rrr(&mut lower.mctx, fpu_op, Frm::Dyn, lhs, rhs);

                curr_block.push_back(&mut lower.mctx, inst);

                MValue::new_reg(dst_ty, rd)
            }
            Fbop::Cmp(cond) => {
                let op = if bitwidth == 32 {
                    match cond {
                        ir::FCmpCond::UEq => FpuOpRRR::FeqS,
                        ir::FCmpCond::UNe => FpuOpRRR::FeqS,
                        ir::FCmpCond::ULt => FpuOpRRR::FltS,
                        ir::FCmpCond::ULe => FpuOpRRR::FleS,
                        ir::FCmpCond::OEq
                        | ir::FCmpCond::ONe
                        | ir::FCmpCond::OLt
                        | ir::FCmpCond::OLe => {
                            unimplemented!()
                        }
                    }
                } else {
                    match cond {
                        ir::FCmpCond::UEq => FpuOpRRR::FeqD,
                        ir::FCmpCond::UNe => FpuOpRRR::FeqD,
                        ir::FCmpCond::ULt => FpuOpRRR::FltD,
                        ir::FCmpCond::ULe => FpuOpRRR::FleD,
                        ir::FCmpCond::OEq
                        | ir::FCmpCond::ONe
                        | ir::FCmpCond::OLt
                        | ir::FCmpCond::OLe => {
                            unimplemented!()
                        }
                    }
                };

                let not = matches!(cond, ir::FCmpCond::UNe);

                let (inst, rd) = RvInst::fpu_rrr(&mut lower.mctx, op, Frm::Dyn, lhs, rhs);
                curr_block.push_back(&mut lower.mctx, inst);

                let rd = if not {
                    // the type is i1, so we need to xor with 1
                    let (inst, rd) = RvInst::alu_rri(
                        &mut lower.mctx,
                        AluOpRRI::Xori,
                        rd,
                        Imm12::try_from_i64(1).unwrap(),
                    );
                    curr_block.push_back(&mut lower.mctx, inst);
                    rd
                } else {
                    rd
                };

                MValue::new_reg(dst_ty, rd)
            }
        }
    }

    fn gen_iunary(
        lower: &mut LowerContext<Self>,
        op: ir::IUnaryOp,
        operand: MValue,
        dst_ty: ir::Ty,
    ) -> MValue {
        let curr_block = lower.curr_block.unwrap();

        let ty = operand.ty();
        let operand = match operand.kind() {
            MValueKind::Reg(reg) => reg,
            MValueKind::Imm(imm) => {
                let (inst, reg) = RvInst::li(&mut lower.mctx, imm as u64);
                curr_block.push_back(&mut lower.mctx, inst);
                reg
            }
            MValueKind::Mem(_) => unreachable!(),
            MValueKind::Undef => return MValue::new_undef(dst_ty),
        };

        let bitwidth = ty.bitwidth(lower.ctx).unwrap();

        match op {
            ir::IUnaryOp::Not => {
                // xor with -1, but we need to notice the bitwidth
                let mask = 1u64.wrapping_shl(bitwidth as u32).wrapping_sub(1);
                if let Some(imm) = Imm12::try_from_i64(mask as i64) {
                    let (inst, reg) =
                        RvInst::alu_rri(&mut lower.mctx, AluOpRRI::Xori, operand, imm);
                    curr_block.push_back(&mut lower.mctx, inst);
                    MValue::new_reg(ty, reg)
                } else {
                    let (inst, reg) = RvInst::li(&mut lower.mctx, mask);
                    curr_block.push_back(&mut lower.mctx, inst);
                    let (inst, reg) = RvInst::alu_rrr(&mut lower.mctx, AluOpRRR::Xor, operand, reg);
                    curr_block.push_back(&mut lower.mctx, inst);
                    MValue::new_reg(ty, reg)
                }
            }
        }
    }

    fn gen_funary(
        lower: &mut LowerContext<Self>,
        op: ir::FUnaryOp,
        operand: MValue,
        dst_ty: ir::Ty,
    ) -> MValue {
        let curr_block = lower.curr_block.unwrap();
        let ty = operand.ty();

        let operand = match operand.kind() {
            MValueKind::Reg(reg) => reg,
            MValueKind::Mem(_) | MValueKind::Imm(_) => unreachable!(),
            MValueKind::Undef => return MValue::new_undef(dst_ty),
        };

        let bitwidth = ty.bitwidth(lower.ctx).unwrap();

        match op {
            ir::FUnaryOp::Neg => {
                let fpu_op = if bitwidth == 32 {
                    FpuOpRRR::FsgnjnS
                } else {
                    FpuOpRRR::FsgnjnD
                };

                let (inst, rd) =
                    RvInst::fpu_rrr(&mut lower.mctx, fpu_op, Frm::Dyn, operand, operand);
                curr_block.push_back(&mut lower.mctx, inst);
                MValue::new_reg(dst_ty, rd)
            }
        }
    }

    fn gen_cast(
        lower: &mut LowerContext<Self>,
        op: ir::CastOp,
        val: MValue,
        dst_ty: ir::Ty,
    ) -> MValue {
        let curr_block = lower.curr_block.unwrap();

        let src_ty = val.ty();

        let src = match val.kind() {
            MValueKind::Reg(reg) => reg,
            MValueKind::Imm(imm) => {
                let (inst, reg) = RvInst::li(&mut lower.mctx, imm as u64);
                curr_block.push_back(&mut lower.mctx, inst);
                reg
            }
            MValueKind::Mem(_) => unreachable!(),
            MValueKind::Undef => return MValue::new_undef(dst_ty),
        };

        let src_bitwidth = src_ty.bitwidth(lower.ctx).unwrap();
        let dst_bitwidth = dst_ty.bitwidth(lower.ctx).unwrap();

        match op {
            ir::CastOp::Bitcast => MValue::new_reg(dst_ty, src),
            ir::CastOp::ZExt => MValue::new_reg(dst_ty, src),
            ir::CastOp::SExt => {
                // shl, asr
                let shift = 64 - src_bitwidth as i64;
                let (inst, rd) = RvInst::alu_rri(
                    &mut lower.mctx,
                    AluOpRRI::Slli,
                    src,
                    Imm12::try_from_i64(shift).unwrap(),
                );
                curr_block.push_back(&mut lower.mctx, inst);
                let (inst, rd) = RvInst::alu_rri(
                    &mut lower.mctx,
                    AluOpRRI::Srai,
                    rd,
                    Imm12::try_from_i64(shift).unwrap(),
                );
                curr_block.push_back(&mut lower.mctx, inst);
                MValue::new_reg(dst_ty, rd)
            }
            ir::CastOp::FpExt => unimplemented!(),
            ir::CastOp::SiToFp => {
                let op = match (src_bitwidth, dst_bitwidth) {
                    (0..=32, 32) => FpuOpRR::FcvtSW,
                    (0..=32, 64) => FpuOpRR::FcvtDW,
                    (33..=64, 32) => FpuOpRR::FcvtSL,
                    (33..=64, 64) => FpuOpRR::FcvtDL,
                    _ => unreachable!(),
                };
                let (inst, rd) = RvInst::fpu_rr(&mut lower.mctx, op, Frm::Dyn, src);
                curr_block.push_back(&mut lower.mctx, inst);
                MValue::new_reg(dst_ty, rd)
            }
            ir::CastOp::FpToSi => {
                let op = match (src_bitwidth, dst_bitwidth) {
                    (32, 0..=32) => FpuOpRR::FcvtWS,
                    (64, 0..=32) => FpuOpRR::FcvtWD,
                    (32, 33..=64) => FpuOpRR::FcvtLS,
                    (64, 33..=64) => FpuOpRR::FcvtLD,
                    _ => unreachable!(),
                };
                let (inst, rd) = RvInst::fpu_rr(&mut lower.mctx, op, Frm::Rtz, src);
                curr_block.push_back(&mut lower.mctx, inst);
                MValue::new_reg(dst_ty, rd)
            }
            ir::CastOp::UiToFp => {
                let op = match (src_bitwidth, dst_bitwidth) {
                    (0..=32, 32) => FpuOpRR::FcvtSWu,
                    (0..=32, 64) => FpuOpRR::FcvtDWu,
                    (33..=64, 32) => FpuOpRR::FcvtSLu,
                    (33..=64, 64) => FpuOpRR::FcvtDLu,
                    _ => unreachable!(),
                };
                let (inst, rd) = RvInst::fpu_rr(&mut lower.mctx, op, Frm::Dyn, src);
                curr_block.push_back(&mut lower.mctx, inst);
                MValue::new_reg(dst_ty, rd)
            }
            ir::CastOp::FpToUi => {
                let op = match (src_bitwidth, dst_bitwidth) {
                    (32, 0..=32) => FpuOpRR::FcvtWuS,
                    (64, 0..=32) => FpuOpRR::FcvtWuD,
                    (32, 33..=64) => FpuOpRR::FcvtLuS,
                    (64, 33..=64) => FpuOpRR::FcvtLuD,
                    _ => unreachable!(),
                };
                // TODO: is the rounding mode correct?
                let (inst, rd) = RvInst::fpu_rr(&mut lower.mctx, op, Frm::Rtz, src);
                curr_block.push_back(&mut lower.mctx, inst);
                MValue::new_reg(dst_ty, rd)
            }
            ir::CastOp::Trunc
            | ir::CastOp::FpTrunc
            | ir::CastOp::IntToPtr
            | ir::CastOp::PtrToInt => unimplemented!(),
        }
    }

    fn gen_offset(lower: &mut LowerContext<Self>, base: MValue, offset: MValue) -> MValue {
        let curr_block = lower.curr_block.unwrap();

        let ty = base.ty(); // ptr

        let base = match base.kind() {
            MValueKind::Reg(reg) => reg,
            MValueKind::Imm(_) => unreachable!(),
            MValueKind::Mem(loc) => {
                match loc {
                    MemLoc::RegOffset { base, offset } => {
                        if let Some(imm) = Imm12::try_from_i64(offset) {
                            let (inst, rd) =
                                RvInst::alu_rri(&mut lower.mctx, AluOpRRI::Addi, base, imm);
                            curr_block.push_back(&mut lower.mctx, inst);
                            rd
                        } else {
                            let (li, rd) = RvInst::li(&mut lower.mctx, offset as u64);
                            curr_block.push_back(&mut lower.mctx, li);
                            let (inst, rd) =
                                RvInst::alu_rrr(&mut lower.mctx, AluOpRRR::Add, base, rd);
                            curr_block.push_back(&mut lower.mctx, inst);
                            rd
                        }
                    }
                    MemLoc::Slot { .. } | MemLoc::Incoming { .. } => {
                        // load addr because we cannot determine the offset
                        let (inst, rd) = RvInst::load_addr(&mut lower.mctx, loc);
                        curr_block.push_back(&mut lower.mctx, inst);
                        rd
                    }
                }
            }
            MValueKind::Undef => return MValue::new_undef(ty),
        };

        match offset.kind() {
            MValueKind::Reg(reg) => {
                // TODO: not sure if signext is required
                let (inst, rd) = RvInst::alu_rrr(&mut lower.mctx, AluOpRRR::Add, base, reg);
                curr_block.push_back(&mut lower.mctx, inst);
                MValue::new_reg(ty, rd)
            }
            MValueKind::Imm(imm) => {
                let (inst, rd) = RvInst::alu_rri(
                    &mut lower.mctx,
                    AluOpRRI::Addi,
                    base,
                    Imm12::try_from_i64(imm).unwrap(),
                );
                curr_block.push_back(&mut lower.mctx, inst);
                MValue::new_reg(ty, rd)
            }
            MValueKind::Mem(_) => unreachable!(),
            MValueKind::Undef => MValue::new_undef(ty),
        }
    }

    fn gen_jump(lower: &mut LowerContext<Self>, dst: MBlock<Self::I>) {
        let curr_block = lower.curr_block.unwrap();
        let inst = RvInst::j(&mut lower.mctx, dst);
        curr_block.push_back(&mut lower.mctx, inst);
    }

    fn gen_br(lower: &mut LowerContext<Self>, cond: MValue, dst: MBlock<Self::I>) {
        let curr_block = lower.curr_block.unwrap();
        match cond.kind() {
            MValueKind::Reg(reg) => {
                let inst = RvInst::br(&mut lower.mctx, BrOp::Bne, reg, regs::zero().into(), dst);
                curr_block.push_back(&mut lower.mctx, inst);
            }
            MValueKind::Imm(imm) => {
                if imm as u64 & 1 == 0 {
                    // do nothing, never jump
                } else {
                    let inst = RvInst::j(&mut lower.mctx, dst);
                    curr_block.push_back(&mut lower.mctx, inst);
                }
            }
            MValueKind::Mem(_) => unreachable!(),
            MValueKind::Undef => {
                // do nothing, because it's undef
            }
        }
    }

    fn gen_call(lower: &mut LowerContext<Self>, func: MFunc<Self::I>, arg_regs: Vec<PReg>) {
        let curr_block = lower.curr_block.unwrap();
        let inst = RvInst::call(&mut lower.mctx, func, arg_regs);
        curr_block.push_back(&mut lower.mctx, inst);
    }

    fn gen_call_indirect(
        _lower: &mut LowerContext<Self>,
        _sig: &ir::Signature,
        _func_ptr: MValue,
        _arg_regs: Vec<PReg>,
    ) {
        unimplemented!("indirect call")
    }

    fn gen_outgoing(lower: &mut LowerContext<Self>, args: Vec<MValue>) -> Vec<PReg> {
        // TODO: maybe support more calling conventions
        let mut used_int_regs = 0;
        let mut used_fp_regs = 0;

        let mut required_stack_size = 0u64;
        let mut pass_by_stack = Vec::new();

        let mut arg_regs = Vec::new();

        for arg in args {
            let ty = arg.ty();
            let bytewidth = ty.bytewidth_with_ptr(lower.ctx, Self::pointer_size()) as u64;

            if ty.is_integer(lower.ctx) || ty.is_ptr(lower.ctx) {
                if used_int_regs < regs::INT_ARG_REGS.len() {
                    let reg = regs::INT_ARG_REGS[used_int_regs];
                    Self::gen_move(lower, reg.into(), arg);
                    arg_regs.push(reg);
                    used_int_regs += 1;
                } else {
                    required_stack_size += bytewidth;
                    pass_by_stack.push(arg);
                }
            } else if ty.is_float(lower.ctx) {
                if used_fp_regs < regs::FP_ARG_REGS.len() {
                    let reg = regs::FP_ARG_REGS[used_fp_regs];
                    Self::gen_move(lower, reg.into(), arg);
                    arg_regs.push(reg);
                    used_fp_regs += 1;
                } else {
                    required_stack_size += bytewidth;
                    pass_by_stack.push(arg);
                }
            } else {
                unimplemented!()
            }
        }

        lower
            .curr_func
            .unwrap()
            .update_outgoing_stack_size(&mut lower.mctx, required_stack_size);

        // the offset is calculated from the end of the stack frame
        // the first arg is at the end of the stack frame, so the offset is 0
        let mut offset = 0i64;
        for arg in pass_by_stack {
            let loc = if Imm12::try_from_i64(offset).is_some() {
                MemLoc::RegOffset {
                    base: regs::sp().into(),
                    offset,
                }
            } else {
                // li
                let (li, rd) = RvInst::li(&mut lower.mctx, offset as u64);
                lower.curr_block.unwrap().push_back(&mut lower.mctx, li);
                // add
                let (inst, rd) =
                    RvInst::alu_rrr(&mut lower.mctx, AluOpRRR::Add, regs::sp().into(), rd);
                lower.curr_block.unwrap().push_back(&mut lower.mctx, inst);
                MemLoc::RegOffset {
                    base: rd,
                    offset: 0,
                }
            };
            Self::gen_store(lower, arg, loc);
            let bytewidth = arg.ty().bytewidth_with_ptr(lower.ctx, Self::pointer_size()) as i64;
            offset += bytewidth;
        }

        arg_regs
    }

    fn gen_incoming(lower: &mut LowerContext<Self>, _sig: &ir::Signature, dsts: Vec<ir::Value>) {
        let mut used_int_regs = 0;
        let mut used_fp_regs = 0;

        let mut pass_by_stack = Vec::new();

        for dst in dsts {
            let ty = dst.ty(lower.ctx);

            if ty.is_integer(lower.ctx) || ty.is_ptr(lower.ctx) {
                if used_int_regs < regs::INT_ARG_REGS.len() {
                    let reg = regs::INT_ARG_REGS[used_int_regs];
                    let vreg = lower.mctx.new_vreg(RegKind::General);
                    Self::gen_move(lower, vreg.into(), MValue::new_reg(ty, reg));
                    lower.lowered.insert(dst, MValue::new_reg(ty, vreg));
                    used_int_regs += 1;
                } else {
                    pass_by_stack.push(dst);
                }
            } else if ty.is_float(lower.ctx) {
                if used_fp_regs < regs::FP_ARG_REGS.len() {
                    let reg = regs::FP_ARG_REGS[used_fp_regs];
                    let vreg = lower.mctx.new_vreg(RegKind::Float);
                    Self::gen_move(lower, vreg.into(), MValue::new_reg(ty, reg));
                    lower.lowered.insert(dst, MValue::new_reg(ty, vreg));
                    used_fp_regs += 1;
                } else {
                    pass_by_stack.push(dst);
                }
            } else {
                unimplemented!()
            }
        }

        let mut offset = 0i64;
        for dst in pass_by_stack {
            let loc = MemLoc::Incoming { offset };
            let mval = Self::gen_load(lower, dst.ty(lower.ctx), loc);
            lower.lowered.insert(dst, mval);
            let bytewidth =
                dst.ty(lower.ctx)
                    .bytewidth_with_ptr(lower.ctx, Self::pointer_size()) as i64;
            offset += bytewidth;
        }
    }

    fn gen_load(lower: &mut LowerContext<Self>, ty: ir::Ty, mem_loc: MemLoc) -> MValue {
        let curr_block = lower.curr_block.unwrap();

        let bitwidth = ty.bitwidth_with_ptr(lower.ctx, Self::pointer_size());
        if ty.is_integer(lower.ctx) || ty.is_ptr(lower.ctx) {
            let op = match bitwidth {
                0..=8 => LoadOp::Lb,
                9..=16 => LoadOp::Lh,
                17..=32 => LoadOp::Lw,
                33..=64 => LoadOp::Ld,
                _ => unimplemented!(),
            };
            let (inst, rd) = RvInst::load(&mut lower.mctx, op, mem_loc);
            curr_block.push_back(&mut lower.mctx, inst);
            MValue::new_reg(ty, rd)
        } else if ty.is_float32(lower.ctx) {
            let (inst, rd) = RvInst::load(&mut lower.mctx, LoadOp::Flw, mem_loc);
            curr_block.push_back(&mut lower.mctx, inst);
            MValue::new_reg(ty, rd)
        } else if ty.is_float64(lower.ctx) {
            let (inst, rd) = RvInst::load(&mut lower.mctx, LoadOp::Fld, mem_loc);
            curr_block.push_back(&mut lower.mctx, inst);
            MValue::new_reg(ty, rd)
        } else {
            unimplemented!()
        }
    }

    fn gen_store(lower: &mut LowerContext<Self>, val: MValue, mem_loc: MemLoc) {
        let curr_block = lower.curr_block.unwrap();

        let src = match val.kind() {
            MValueKind::Reg(reg) => reg,
            MValueKind::Imm(imm) => {
                let (inst, rd) = RvInst::li(&mut lower.mctx, imm as u64);
                curr_block.push_back(&mut lower.mctx, inst);
                rd
            }
            MValueKind::Mem(loc) => {
                match loc {
                    MemLoc::RegOffset { base, offset } => {
                        if let Some(imm) = Imm12::try_from_i64(offset) {
                            let (inst, rd) =
                                RvInst::alu_rri(&mut lower.mctx, AluOpRRI::Addi, base, imm);
                            curr_block.push_back(&mut lower.mctx, inst);
                            rd
                        } else {
                            let (li, rd) = RvInst::li(&mut lower.mctx, offset as u64);
                            curr_block.push_back(&mut lower.mctx, li);
                            let (inst, rd) =
                                RvInst::alu_rrr(&mut lower.mctx, AluOpRRR::Add, base, rd);
                            curr_block.push_back(&mut lower.mctx, inst);
                            rd
                        }
                    }
                    MemLoc::Slot { .. } | MemLoc::Incoming { .. } => {
                        // load addr because we cannot determine the offset
                        let (inst, rd) = RvInst::load_addr(&mut lower.mctx, loc);
                        curr_block.push_back(&mut lower.mctx, inst);
                        rd
                    }
                }
            }
            MValueKind::Undef => return,
        };

        let bitwidth = val.ty().bitwidth_with_ptr(lower.ctx, Self::pointer_size());
        if val.ty().is_integer(lower.ctx) || val.ty().is_ptr(lower.ctx) {
            let op = match bitwidth {
                0..=8 => StoreOp::Sb,
                9..=16 => StoreOp::Sh,
                17..=32 => StoreOp::Sw,
                33..=64 => StoreOp::Sd,
                _ => unimplemented!(),
            };
            let inst = RvInst::store(&mut lower.mctx, op, src, mem_loc);
            curr_block.push_back(&mut lower.mctx, inst);
        } else if val.ty().is_float32(lower.ctx) {
            let inst = RvInst::store(&mut lower.mctx, StoreOp::Fsw, src, mem_loc);
            curr_block.push_back(&mut lower.mctx, inst);
        } else if val.ty().is_float64(lower.ctx) {
            let inst = RvInst::store(&mut lower.mctx, StoreOp::Fsd, src, mem_loc);
            curr_block.push_back(&mut lower.mctx, inst);
        } else {
            unimplemented!()
        }
    }

    fn gen_ret(lower: &mut LowerContext<Self>) {
        let curr_block = lower.curr_block.unwrap();
        let inst = RvInst::ret(&mut lower.mctx);
        curr_block.push_back(&mut lower.mctx, inst);
    }

    fn gen_get_global(lower: &mut LowerContext<Self>, label: MLabel, dst_ty: ir::Ty) -> MValue {
        let curr_block = lower.curr_block.unwrap();
        let (inst, rd) = RvInst::la(&mut lower.mctx, label);
        curr_block.push_back(&mut lower.mctx, inst);
        MValue::new_reg(dst_ty, rd)
    }

    fn gen_func_prologue(lower: &mut LowerContext<Self>, func: MFunc<Self::I>) {
        func.add_saved_reg(&mut lower.mctx, regs::ra());

        // addi sp, sp, -frame_size or sub
        //
        // store reg#0, total_stack_size - 8(sp)
        // store reg#1, total_stack_size - 16(sp)
        // ...
        //
        // store reg#n, total_stack_size - 8n(sp)
        //
        // addi fp, sp, total_stack_size # if we omit fp, this is not needed

        // this is the first block, we need to insert in reverse order
        let curr_block = lower.curr_block.unwrap();

        if !lower.config.omit_frame_pointer {
            func.add_saved_reg(&mut lower.mctx, regs::fp());
        }

        let saved_regs = func.saved_regs(&lower.mctx);

        let total_stack_size = Self::total_stack_size(lower, func) as i64;

        let mut inst_buf = Vec::new();

        let sp = regs::sp();
        let fp = regs::fp();

        if let Some(imm) = Imm12::try_from_i64(-total_stack_size) {
            let addi =
                RvInst::build_alu_rri(&mut lower.mctx, AluOpRRI::Addi, sp.into(), sp.into(), imm);
            inst_buf.push(addi);
        } else {
            let t0 = regs::t0();
            let li = RvInst::build_li(&mut lower.mctx, t0.into(), (-total_stack_size) as u64);
            let add = RvInst::build_alu_rrr(
                &mut lower.mctx,
                AluOpRRR::Add,
                sp.into(),
                sp.into(),
                t0.into(),
            );
            inst_buf.push(li);
            inst_buf.push(add);
        }

        let mut curr_offset = total_stack_size - 8;
        for reg in saved_regs {
            // TODO: make this modular
            if let RegKind::General = reg.kind() {
                if let Some(imm) = Imm12::try_from_i64(curr_offset) {
                    let store = RvInst::store(
                        &mut lower.mctx,
                        StoreOp::Sd,
                        reg.into(),
                        MemLoc::RegOffset {
                            base: sp.into(),
                            offset: imm.as_i16() as i64,
                        },
                    );
                    inst_buf.push(store);
                } else {
                    let t0 = regs::t0();
                    let li = RvInst::build_li(&mut lower.mctx, t0.into(), curr_offset as u64);
                    let add = RvInst::build_alu_rrr(
                        &mut lower.mctx,
                        AluOpRRR::Add,
                        t0.into(),
                        sp.into(),
                        t0.into(),
                    );
                    let store = RvInst::store(
                        &mut lower.mctx,
                        StoreOp::Sd,
                        reg.into(),
                        MemLoc::RegOffset {
                            base: t0.into(),
                            offset: 0,
                        },
                    );
                    inst_buf.push(li);
                    inst_buf.push(add);
                    inst_buf.push(store);
                }
            } else if let RegKind::Float = reg.kind() {
                if let Some(imm) = Imm12::try_from_i64(curr_offset) {
                    let store = RvInst::store(
                        &mut lower.mctx,
                        StoreOp::Fsd,
                        reg.into(),
                        MemLoc::RegOffset {
                            base: sp.into(),
                            offset: imm.as_i16() as i64,
                        },
                    );
                    inst_buf.push(store);
                } else {
                    let t0 = regs::t0();
                    let li = RvInst::build_li(&mut lower.mctx, t0.into(), curr_offset as u64);
                    let add = RvInst::build_alu_rrr(
                        &mut lower.mctx,
                        AluOpRRR::Add,
                        t0.into(),
                        sp.into(),
                        t0.into(),
                    );
                    let store = RvInst::store(
                        &mut lower.mctx,
                        StoreOp::Fsd,
                        reg.into(),
                        MemLoc::RegOffset {
                            base: t0.into(),
                            offset: 0,
                        },
                    );
                    inst_buf.push(li);
                    inst_buf.push(add);
                    inst_buf.push(store);
                }
            } else {
                unimplemented!()
            }
            curr_offset -= 8;
        }

        if !lower.config.omit_frame_pointer {
            if let Some(imm) = Imm12::try_from_i64(total_stack_size) {
                let addi = RvInst::build_alu_rri(
                    &mut lower.mctx,
                    AluOpRRI::Addi,
                    fp.into(),
                    sp.into(),
                    imm,
                );
                inst_buf.push(addi);
            } else {
                let t0 = regs::t0();
                let li = RvInst::build_li(&mut lower.mctx, t0.into(), total_stack_size as u64);
                let add = RvInst::build_alu_rrr(
                    &mut lower.mctx,
                    AluOpRRR::Add,
                    fp.into(),
                    sp.into(),
                    t0.into(),
                );
                inst_buf.push(li);
                inst_buf.push(add);
            }
        }

        for inst in inst_buf.into_iter().rev() {
            curr_block.push_front(&mut lower.mctx, inst);
        }
    }

    fn gen_func_epilogue(lower: &mut LowerContext<Self>, func: MFunc<Self::I>) {
        let curr_block = lower.curr_block.unwrap();
        let saved_regs = func.saved_regs(&lower.mctx);

        let total_stack_size = Self::total_stack_size(lower, func) as i64;

        // load reg#0, total_stack_size - 8(sp)
        // load reg#1, total_stack_size - 16(sp)
        // ...
        // load reg#n, total_stack_size - 8n(sp)
        //
        // addi sp, sp, total_stack_size
        // ret

        let sp = regs::sp();

        let mut curr_offset = total_stack_size - 8;

        for reg in saved_regs {
            if let RegKind::General = reg.kind() {
                if let Some(imm) = Imm12::try_from_i64(curr_offset) {
                    let load = RvInst::build_load(
                        &mut lower.mctx,
                        LoadOp::Ld,
                        reg.into(),
                        MemLoc::RegOffset {
                            base: sp.into(),
                            offset: imm.as_i16() as i64,
                        },
                    );
                    curr_block.push_back(&mut lower.mctx, load);
                } else {
                    let t0 = regs::t0();
                    let li = RvInst::build_li(&mut lower.mctx, t0.into(), curr_offset as u64);
                    let add = RvInst::build_alu_rrr(
                        &mut lower.mctx,
                        AluOpRRR::Add,
                        t0.into(),
                        sp.into(),
                        t0.into(),
                    );
                    let load = RvInst::build_load(
                        &mut lower.mctx,
                        LoadOp::Ld,
                        reg.into(),
                        MemLoc::RegOffset {
                            base: t0.into(),
                            offset: 0,
                        },
                    );
                    curr_block.push_back(&mut lower.mctx, li);
                    curr_block.push_back(&mut lower.mctx, add);
                    curr_block.push_back(&mut lower.mctx, load);
                }
            } else if let RegKind::Float = reg.kind() {
                if let Some(imm) = Imm12::try_from_i64(curr_offset) {
                    let load = RvInst::build_load(
                        &mut lower.mctx,
                        LoadOp::Fld,
                        reg.into(),
                        MemLoc::RegOffset {
                            base: sp.into(),
                            offset: imm.as_i16() as i64,
                        },
                    );
                    curr_block.push_back(&mut lower.mctx, load);
                } else {
                    let t0 = regs::t0();
                    let li = RvInst::build_li(&mut lower.mctx, t0.into(), curr_offset as u64);
                    let add = RvInst::build_alu_rrr(
                        &mut lower.mctx,
                        AluOpRRR::Add,
                        t0.into(),
                        sp.into(),
                        t0.into(),
                    );
                    let load = RvInst::build_load(
                        &mut lower.mctx,
                        LoadOp::Fld,
                        reg.into(),
                        MemLoc::RegOffset {
                            base: t0.into(),
                            offset: 0,
                        },
                    );
                    curr_block.push_back(&mut lower.mctx, li);
                    curr_block.push_back(&mut lower.mctx, add);
                    curr_block.push_back(&mut lower.mctx, load);
                }
            } else {
                unimplemented!()
            }
            curr_offset -= 8;
        }

        if let Some(imm) = Imm12::try_from_i64(total_stack_size) {
            let addi =
                RvInst::build_alu_rri(&mut lower.mctx, AluOpRRI::Addi, sp.into(), sp.into(), imm);
            curr_block.push_back(&mut lower.mctx, addi);
        } else {
            let t0 = regs::t0();
            let li = RvInst::build_li(&mut lower.mctx, t0.into(), total_stack_size as u64);
            let add = RvInst::build_alu_rrr(
                &mut lower.mctx,
                AluOpRRR::Add,
                sp.into(),
                sp.into(),
                t0.into(),
            );
            curr_block.push_back(&mut lower.mctx, li);
            curr_block.push_back(&mut lower.mctx, add);
        }

        Self::gen_ret(lower);
    }

    fn gen_spill_load(lower: &mut LowerContext<Self>, reg: Reg, slot: MemLoc, inst: Self::I) {
        let load = match reg.kind() {
            RegKind::General => RvInst::build_load(&mut lower.mctx, LoadOp::Ld, reg, slot),
            RegKind::Float => RvInst::build_load(&mut lower.mctx, LoadOp::Fld, reg, slot),
            RegKind::Vector => unimplemented!(),
        };
        inst.insert_before(&mut lower.mctx, load);
    }

    fn gen_spill_store(lower: &mut LowerContext<Self>, reg: Reg, slot: MemLoc, inst: Self::I) {
        let store = match reg.kind() {
            RegKind::General => RvInst::store(&mut lower.mctx, StoreOp::Sd, reg, slot),
            RegKind::Float => RvInst::store(&mut lower.mctx, StoreOp::Fsd, reg, slot),
            RegKind::Vector => unimplemented!(),
        };
        inst.insert_after(&mut lower.mctx, store);
    }

    fn display_reg(reg: Reg) -> String { regs::display(reg) }
}
