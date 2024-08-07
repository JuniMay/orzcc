use rustc_hash::{FxHashMap, FxHashSet};

use super::inst::{RvInst, RvInstKind};
use crate::{
    backend::{
        inst::MInst,
        peephole::{Peephole1, Peephole2, Peephole3, PeepholeRule, PeepholeRunner},
        riscv64::{
            inst::{AluOpRRI, AluOpRRR, BrOp, FpuOpRRR, FpuOpRRRR, LoadOp, StoreOp},
            regs,
        },
        LowerConfig,
        MContext,
    },
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
};

const fn fuse_cmp_br() -> Peephole2<RvInst> {
    PeepholeRule {
        rewriter: |mctx, def_use, (a, b)| {
            use RvInstKind as Ik;

            match (a.kind(mctx), b.kind(mctx)) {
                (
                    Ik::AluRRR {
                        op: cmp_op @ (AluOpRRR::Slt | AluOpRRR::Sltu),
                        rd,
                        rs1: cmp_lhs,
                        rs2: cmp_rhs,
                    },
                    Ik::Br {
                        op: br_op @ (BrOp::Beq | BrOp::Bne),
                        rs1: br_lhs,
                        rs2: br_rhs,
                        block,
                    },
                ) => {
                    let rd = *rd;
                    let cmp_lhs = *cmp_lhs;
                    let cmp_rhs = *cmp_rhs;
                    let br_lhs = *br_lhs;
                    let br_rhs = *br_rhs;

                    let block = *block;

                    #[allow(clippy::wildcard_enum_match_arm)]
                    let br_op = match (br_op, cmp_op) {
                        (BrOp::Bne, AluOpRRR::Slt) => BrOp::Blt,
                        (BrOp::Bne, AluOpRRR::Sltu) => BrOp::Bltu,
                        (BrOp::Beq, AluOpRRR::Slt) => BrOp::Bge,
                        (BrOp::Beq, AluOpRRR::Sltu) => BrOp::Bgeu,
                        _ => unreachable!(),
                    };

                    if def_use.num_uses(rd) != 1 {
                        // we can't remove cmp if it has multiple uses
                        return false;
                    }

                    let new_br = if (rd == br_lhs && br_rhs == regs::zero().into())
                        || (rd == br_rhs && br_lhs == regs::zero().into())
                    {
                        // slt(u) + bne => blt(u)
                        // slt(u) + beq => bge(u)
                        RvInst::br(mctx, br_op, cmp_lhs, cmp_rhs, block)
                    } else {
                        return false;
                    };

                    b.insert_after(mctx, new_br);

                    // maintain def-use
                    def_use.remove_def(rd, a);
                    def_use.remove_use(cmp_lhs, a);
                    def_use.remove_use(cmp_rhs, a);

                    def_use.remove_use(br_lhs, b);
                    def_use.remove_use(br_rhs, b);

                    a.remove(mctx);
                    b.remove(mctx);

                    def_use.add_use(cmp_lhs, new_br);
                    def_use.add_use(cmp_rhs, new_br);

                    true
                }
                (
                    Ik::AluRRI {
                        op: AluOpRRI::Sltiu,
                        rd,
                        rs: cmp_lhs,
                        imm,
                    },
                    Ik::Br {
                        op: BrOp::Bne,
                        rs1: br_lhs,
                        rs2: br_rhs,
                        block,
                    },
                ) if imm.as_i16() == 1 => {
                    let rd = *rd;

                    let cmp_lhs = *cmp_lhs;

                    let br_lhs = *br_lhs;
                    let br_rhs = *br_rhs;

                    let block = *block;

                    if def_use.num_uses(rd) != 1 {
                        return false;
                    }

                    let new_br = if (rd == br_lhs && br_rhs == regs::zero().into())
                        || (rd == br_rhs && br_lhs == regs::zero().into())
                    {
                        // sltiu 1 + bnez => beqz
                        RvInst::br(mctx, BrOp::Beq, cmp_lhs, regs::zero().into(), block)
                    } else {
                        return false;
                    };

                    b.insert_after(mctx, new_br);

                    // maintain def-use
                    def_use.remove_def(rd, a);
                    def_use.remove_use(cmp_lhs, a);

                    def_use.remove_use(br_lhs, b);
                    def_use.remove_use(br_rhs, b);

                    a.remove(mctx);
                    b.remove(mctx);

                    def_use.add_use(cmp_lhs, new_br);
                    def_use.add_use(regs::zero().into(), new_br);

                    true
                }
                _ => false,
            }
        },
    }
}

const fn fuse_xori_cmp_br() -> Peephole3<RvInst> {
    PeepholeRule {
        rewriter: |mctx, def_use, (a, b, c)| {
            use RvInstKind as Ik;

            match (a.kind(mctx), b.kind(mctx), c.kind(mctx)) {
                (
                    Ik::AluRRR {
                        op: cmp_op @ (AluOpRRR::Slt | AluOpRRR::Sltu),
                        rd: cmp_rd,
                        rs1: cmp_lhs,
                        rs2: cmp_rhs,
                    },
                    Ik::AluRRI {
                        op: AluOpRRI::Xori,
                        rd: xori_rd,
                        rs: xori_rs,
                        imm: xori_imm,
                    },
                    Ik::Br {
                        op: br_op @ (BrOp::Beq | BrOp::Bne),
                        rs1: br_lhs,
                        rs2: br_rhs,
                        block,
                    },
                ) if def_use.num_uses(*xori_rd) == 1
                    && def_use.num_uses(*cmp_rd) == 1
                    && cmp_rd == xori_rs
                    // && xori_rd == br_rs, this will be checked later because we don't know which is br_rs
                    && xori_imm.as_i16() == 1 =>
                {
                    let cmp_rd = *cmp_rd;
                    let cmp_lhs = *cmp_lhs;
                    let cmp_rhs = *cmp_rhs;

                    let xori_rd = *xori_rd;
                    let xori_rs = *xori_rs;
                    // `let xori_imm = *xori_imm;`,
                    // this is useless because we've checked it's 1.

                    let br_op = *br_op;
                    let br_lhs = *br_lhs;
                    let br_rhs = *br_rhs;

                    let block = *block;

                    let br_op = match (br_op, cmp_op) {
                        (BrOp::Bne, AluOpRRR::Slt) => BrOp::Bge,
                        (BrOp::Bne, AluOpRRR::Sltu) => BrOp::Bgeu,
                        (BrOp::Beq, AluOpRRR::Slt) => BrOp::Blt,
                        (BrOp::Beq, AluOpRRR::Sltu) => BrOp::Bltu,
                        _ => unreachable!(),
                    };

                    let new_br = if (xori_rd == br_lhs && br_rhs == regs::zero().into())
                        || (xori_rd == br_rhs && br_lhs == regs::zero().into())
                    {
                        // slt(u) + xori + bne => bge(u)
                        // slt(u) + xori + beq => blt(u)
                        RvInst::br(mctx, br_op, cmp_lhs, cmp_rhs, block)
                    } else {
                        return false;
                    };

                    c.insert_after(mctx, new_br);

                    // maintain def-use
                    def_use.remove_def(cmp_rd, a);
                    def_use.remove_use(cmp_lhs, a);
                    def_use.remove_use(cmp_rhs, a);

                    def_use.remove_def(xori_rd, b);
                    def_use.remove_use(xori_rs, b);

                    def_use.remove_use(br_lhs, b);
                    def_use.remove_use(br_rhs, b);

                    a.remove(mctx);
                    b.remove(mctx);
                    c.remove(mctx);

                    def_use.add_use(cmp_lhs, new_br);
                    def_use.add_use(cmp_rhs, new_br);

                    true
                }
                _ => false,
            }
        },
    }
}

const fn fuse_shl_add() -> Peephole2<RvInst> {
    PeepholeRule {
        rewriter: |mctx, def_use, (a, b)| {
            use RvInstKind as Ik;

            match (a.kind(mctx), b.kind(mctx)) {
                (
                    Ik::AluRRI {
                        op: AluOpRRI::Slli | AluOpRRI::Slliw,
                        rd: slli_rd,
                        rs: slli_rs,
                        imm,
                    },
                    Ik::AluRRR {
                        op: AluOpRRR::Add | AluOpRRR::Addw,
                        rd: add_rd,
                        rs1: add_rs1,
                        rs2: add_rs2,
                    },
                ) => {
                    // slli $v0, $v1, 1/2/3
                    // add $v2, $v0, $v3
                    // =>
                    // sh1/2/3add $v2, $v1, $v3

                    if def_use.num_uses(*slli_rd) != 1 {
                        // we can't remove slli if it has multiple uses
                        return false;
                    }

                    let shamt = imm.as_i16();
                    let op = match shamt {
                        1 => AluOpRRR::Sh1add,
                        2 => AluOpRRR::Sh2add,
                        3 => AluOpRRR::Sh3add,
                        _ => return false,
                    };

                    let slli_rd = *slli_rd;
                    let slli_rs = *slli_rs;

                    let add_rd = *add_rd;
                    let add_rs1 = *add_rs1;
                    let add_rs2 = *add_rs2;

                    let (shadd, lhs, rhs) = if slli_rd == add_rs1 {
                        // reuse rd so we can remove add without replacing uses
                        (
                            RvInst::build_alu_rrr(mctx, op, add_rd, slli_rs, add_rs2),
                            slli_rs,
                            add_rs2,
                        )
                    } else if slli_rd == add_rs2 {
                        // reuse rd so we can remove add without replacing uses
                        (
                            RvInst::build_alu_rrr(mctx, op, add_rd, slli_rs, add_rs1),
                            add_rs1,
                            slli_rs,
                        )
                    } else {
                        return false;
                    };

                    // now, we overwrite the add with shadd, and the only use of
                    // slli is in add, so we can safely remove slli and add

                    // note that we must maintain the def-use when removing
                    // instructions

                    b.insert_after(mctx, shadd);

                    def_use.remove_def(slli_rd, a);
                    def_use.remove_use(slli_rs, a);

                    def_use.remove_def(add_rd, b);
                    def_use.remove_use(add_rs1, b);
                    def_use.remove_use(add_rs2, b);

                    a.remove(mctx);
                    b.remove(mctx);

                    def_use.add_def(add_rd, shadd);
                    def_use.add_use(lhs, shadd);
                    def_use.add_use(rhs, shadd);

                    true
                }
                _ => false,
            }
        },
    }
}

/// fmul $f0, $f1, $f2
/// fadd $f3, $f0, $f4
/// =>
/// fmadd $f3, $f1, $f2, $f4
///
/// fmul $f0, $f1, $f2
/// fadd $f3, $f4, $f0
/// =>
/// fmadd $f3, $f1, $f2, $f4
///
/// fmul $f0, $f1, $f2
/// fsub $f3, $f0, $f4
/// =>
/// fmsub $f3, $f1, $f2, $f4
///
/// fmul $f0, $f1, $f2
/// fsub $f3, $f4, $f0
/// =>
/// fnmsub $f3, $f1, $f2, $f4
///
/// AGGRESSIVE: cause precision issue on /hidden_functional/37_dct and
/// 38_light2d
const fn fuse_fmul_faddfsub() -> Peephole2<RvInst> {
    PeepholeRule {
        rewriter: |mctx, def_use, (a, b)| {
            use RvInstKind as Ik;

            match (a.kind(mctx), b.kind(mctx)) {
                (
                    Ik::FpuRRR {
                        op: FpuOpRRR::FmulS,
                        rd: fmul_rd,
                        rs1: fmul_rs1,
                        rs2: fmul_rs2,
                        rm: fmul_rm,
                    },
                    Ik::FpuRRR {
                        op: faddsub_op @ (FpuOpRRR::FaddS | FpuOpRRR::FsubS),
                        rd: faddsub_rd,
                        rs1: faddsub_rs1,
                        rs2: faddsub_rs2,
                        rm: faddsub_rm,
                    },
                ) if def_use.num_uses(*fmul_rd) == 1 => {
                    let fmul_rd = *fmul_rd;
                    let fmul_rs1 = *fmul_rs1;
                    let fmul_rs2 = *fmul_rs2;

                    let faddsub_rd = *faddsub_rd;
                    let faddsub_rs1 = *faddsub_rs1;
                    let faddsub_rs2 = *faddsub_rs2;

                    let frm = if *fmul_rm == *faddsub_rm {
                        *fmul_rm
                    } else {
                        return false;
                    };

                    let (op, fmul_rs3) = if *faddsub_op == FpuOpRRR::FaddS {
                        if fmul_rd == faddsub_rs1 {
                            // fmul $f0, $f1, $f2
                            // fadd $f3, $f0, $f4
                            // =>
                            // fmadd $f3, $f1, $f2, $f4
                            (FpuOpRRRR::FmaddS, faddsub_rs2)
                        } else if fmul_rd == faddsub_rs2 {
                            // fmul $f0, $f1, $f2
                            // fadd $f3, $f4, $f0
                            // =>
                            // fmadd $f3, $f1, $f2, $f4
                            (FpuOpRRRR::FmaddS, faddsub_rs1)
                        } else {
                            return false;
                        }
                    } else if *faddsub_op == FpuOpRRR::FsubS {
                        if fmul_rd == faddsub_rs1 {
                            // fmul $f0, $f1, $f2
                            // fsub $f3, $f0, $f4
                            // =>
                            // fmsub $f3, $f1, $f2, $f4 (rd = rs1 * rs2 - rs3)
                            (FpuOpRRRR::FmsubS, faddsub_rs2)
                        } else if fmul_rd == faddsub_rs2 {
                            // fmul $f0, $f1, $f2
                            // fsub $f3, $f4, $f0
                            // =>
                            // fnmsub $f3, $f1, $f2, $f4 (rd = -rs1 * rs2 + rs3)
                            (FpuOpRRRR::FnmsubS, faddsub_rs1)
                        } else {
                            return false;
                        }
                    } else {
                        return false;
                    };

                    let fmaddsub_inst = RvInst::build_fpu_rrrr(
                        mctx, op, frm, faddsub_rd, fmul_rs1, fmul_rs2, fmul_rs3,
                    );

                    b.insert_after(mctx, fmaddsub_inst);

                    // 维护 def-use 链
                    def_use.remove_def(fmul_rd, a);
                    def_use.remove_use(fmul_rs1, a);
                    def_use.remove_use(fmul_rs2, a);

                    def_use.remove_def(faddsub_rd, b);
                    def_use.remove_use(faddsub_rs1, b);
                    def_use.remove_use(faddsub_rs2, b);

                    a.remove(mctx);
                    b.remove(mctx);

                    def_use.add_def(faddsub_rd, fmaddsub_inst);
                    def_use.add_use(fmul_rs1, fmaddsub_inst);
                    def_use.add_use(fmul_rs2, fmaddsub_inst);
                    def_use.add_use(fmul_rs3, fmaddsub_inst);

                    true
                }
                _ => false,
            }
        },
    }
}

// TODO: floating-point move (fsgnj) is not handled yet

const fn remove_redundant_move() -> Peephole1<RvInst> {
    PeepholeRule {
        rewriter: |mctx, def_use, a| {
            use RvInstKind as Ik;

            #[allow(clippy::wildcard_enum_match_arm)]
            match a.kind(mctx) {
                Ik::AluRRI {
                    op: AluOpRRI::Addi,
                    rd,
                    rs,
                    imm,
                } => {
                    if imm.as_i16() != 0 {
                        return false;
                    }

                    let rd = *rd;
                    let rs = *rs;

                    if def_use.num_defs(rd) == 1
                        && def_use.num_defs(rs) == 1
                        && rd.is_vreg()
                        && rs.is_vreg()
                    {
                        // TODO: can we replace rs that are callee-saved regs?

                        // replace all uses of rd with rs, and remove this instruction
                        def_use.replace_all_uses(mctx, rd, rs);

                        def_use.remove_def(rd, a);
                        def_use.remove_use(rs, a);

                        a.remove(mctx);

                        true
                    } else {
                        false
                    }
                }
                _ => false,
            }
        },
    }
}

const fn li_dce() -> Peephole1<RvInst> {
    PeepholeRule {
        rewriter: |mctx, def_use, a| {
            use RvInstKind as Ik;

            #[allow(clippy::wildcard_enum_match_arm)]
            match a.kind(mctx) {
                Ik::Li { rd, .. } => {
                    let rd = *rd;

                    if def_use.num_defs(rd) == 1 && def_use.num_uses(rd) == 0 && rd.is_vreg() {
                        def_use.remove_def(rd, a);
                        a.remove(mctx);

                        true
                    } else {
                        false
                    }
                }
                _ => false,
            }
        },
    }
}

const fn remove_identity_move() -> Peephole1<RvInst> {
    PeepholeRule {
        rewriter: |mctx, def_use, a| {
            use RvInstKind as Ik;

            #[allow(clippy::wildcard_enum_match_arm)]
            match a.kind(mctx) {
                Ik::AluRRI {
                    op: AluOpRRI::Addi,
                    rd,
                    rs,
                    imm,
                } => {
                    if imm.as_i16() != 0 {
                        return false;
                    }

                    let rd = *rd;
                    let rs = *rs;

                    if rd == rs {
                        // just remove this instruction
                        def_use.remove_def(rd, a);
                        def_use.remove_use(rs, a);

                        a.remove(mctx);

                        true
                    } else {
                        false
                    }
                }
                Ik::FpuRRR {
                    op: FpuOpRRR::FsgnjS | FpuOpRRR::FsgnjD,
                    rm: _,
                    rd,
                    rs1,
                    rs2,
                } => {
                    if rs1 == rs2 && rd == rs1 {
                        // just remove this instruction
                        def_use.remove_def(*rd, a);
                        def_use.remove_use(*rs1, a);
                        def_use.remove_use(*rs2, a);

                        a.remove(mctx);

                        true
                    } else {
                        false
                    }
                }
                _ => false,
            }
        },
    }
}

/// s{b|h|w|d} r1, offset(r2)
/// l{b|h|w|d} r1, offset(r2)
/// =>
/// l{b|h|w|d} r1, offset(r2)
const fn remove_load_after_store() -> Peephole2<RvInst> {
    PeepholeRule {
        rewriter: |mctx, def_use, (a, b)| {
            use RvInstKind as Ik;

            match (a.kind(mctx), b.kind(mctx)) {
                (
                    Ik::Store {
                        op: StoreOp::Sb,
                        src: store_src,
                        loc: store_loc,
                    },
                    Ik::Load {
                        op: LoadOp::Lb,
                        rd: load_rd,
                        loc: load_loc,
                    },
                ) => {
                    if store_src == load_rd && store_loc == load_loc {
                        // remove load
                        b.remove(mctx);
                        def_use.remove_inst(b);
                        true
                    } else {
                        false
                    }
                }
                (
                    Ik::Store {
                        op: StoreOp::Sh,
                        src: store_src,
                        loc: store_loc,
                    },
                    Ik::Load {
                        op: LoadOp::Lh,
                        rd: load_rd,
                        loc: load_loc,
                    },
                ) => {
                    if store_src == load_rd && store_loc == load_loc {
                        // remove load
                        b.remove(mctx);
                        def_use.remove_inst(b);
                        true
                    } else {
                        false
                    }
                }
                (
                    Ik::Store {
                        op: StoreOp::Sw,
                        src: store_src,
                        loc: store_loc,
                    },
                    Ik::Load {
                        op: LoadOp::Lw,
                        rd: load_rd,
                        loc: load_loc,
                    },
                ) => {
                    if store_src == load_rd && store_loc == load_loc {
                        // remove load
                        b.remove(mctx);
                        def_use.remove_inst(b);
                        true
                    } else {
                        false
                    }
                }
                (
                    Ik::Store {
                        op: StoreOp::Sd,
                        src: store_src,
                        loc: store_loc,
                    },
                    Ik::Load {
                        op: LoadOp::Ld,
                        rd: load_rd,
                        loc: load_loc,
                    },
                ) => {
                    if store_src == load_rd && store_loc == load_loc {
                        // remove load
                        b.remove(mctx);
                        def_use.remove_inst(b);
                        true
                    } else {
                        false
                    }
                }
                _ => false,
            }
        },
    }
}

fn remove_redundant_jump(mctx: &mut MContext<RvInst>) -> bool {
    let mut changed = false;

    let funcs = mctx
        .funcs
        .iter_mut()
        .map(|(_, func_data)| func_data.self_ptr())
        .collect::<Vec<_>>();

    for func in funcs {
        if func.is_external(mctx) {
            continue;
        }

        let mut cursor = func.cursor();
        while let Some(block) = cursor.next(mctx) {
            if let Some(tail) = block.tail(mctx) {
                if let RvInstKind::J { block: succ } = tail.kind(mctx) {
                    if block.next(mctx) == Some(*succ) {
                        // remove redundant jump
                        tail.remove(mctx);
                        changed = true;
                    } else {
                        let mut can_remove = true;
                        let mut next = block.next(mctx);
                        while let Some(block) = next {
                            if block.size(mctx) == 0 {
                                // the block is empty, continue to search.
                                next = block.next(mctx);
                            } else if block == *succ {
                                // we found the target block, remove the jump
                                break;
                            } else {
                                can_remove = false;
                                break;
                            }
                        }

                        if can_remove {
                            tail.remove(mctx);
                            changed = true;
                        }
                    }
                }
            }
        }
    }

    changed
}

/// Need-ssa: False
/// Need-before-regalloc: False
/// Need-canonicalized-form: False
/// Need-unique-terminator: False
/// This pass can work on any location.
pub fn remove_redundant_labels(mctx: &mut MContext<RvInst>) -> bool {
    let mut changed = false;

    loop {
        let mut local_changed = false;

        let funcs = mctx
            .funcs
            .iter_mut()
            .map(|(_, func_data)| func_data.self_ptr())
            .collect::<Vec<_>>();

        for func in funcs {
            if func.is_external(mctx) {
                continue;
            }

            let mut label_usage = FxHashMap::default();

            for block in func.iter(mctx) {
                for inst in block.iter(mctx) {
                    match inst.kind(mctx) {
                        RvInstKind::J { block } => {
                            label_usage
                                .entry(block.label(mctx).clone())
                                .or_insert_with(FxHashSet::default)
                                .insert(inst);
                        }
                        RvInstKind::Br { block, .. } => {
                            label_usage
                                .entry(block.label(mctx).clone())
                                .or_insert_with(FxHashSet::default)
                                .insert(inst);
                        }
                        RvInstKind::Li { .. }
                        | RvInstKind::AluRR { .. }
                        | RvInstKind::AluRRI { .. }
                        | RvInstKind::AluRRR { .. }
                        | RvInstKind::FpuRR { .. }
                        | RvInstKind::FpuRRR { .. }
                        | RvInstKind::FpuRRRR { .. }
                        | RvInstKind::Load { .. }
                        | RvInstKind::Store { .. }
                        | RvInstKind::Ret
                        | RvInstKind::Call { .. }
                        | RvInstKind::La { .. }
                        | RvInstKind::LoadAddr { .. } => {}
                    }
                }
            }

            let mut cursor = func.cursor();
            while let Some(block) = cursor.next(mctx) {
                // rule 1: if the block's label is not used, merge it with the previous block
                if !label_usage.contains_key(block.label(mctx)) {
                    if let Some(prev) = block.prev(mctx) {
                        // merge block with prev
                        let mut curr_inst = block.head(mctx);
                        while let Some(inst) = curr_inst {
                            curr_inst = inst.next(mctx);
                            inst.unlink(mctx);
                            prev.push_back(mctx, inst);
                        }
                        cursor.next(mctx);
                        block.remove(mctx);
                        local_changed = true;
                    }
                }
                // rule 2: if the block is empty, remove it, then retarget all jumps to it to the
                // next block
                else if block.size(mctx) == 0 && block.next(mctx).is_some() {
                    for inst in label_usage
                        .remove(block.label(mctx))
                        .unwrap_or(FxHashSet::default())
                    {
                        match inst.kind(mctx) {
                            RvInstKind::J { .. } => {
                                inst.redirect_branch(mctx, block.next(mctx).unwrap());
                            }
                            RvInstKind::Br { .. } => {
                                inst.redirect_branch(mctx, block.next(mctx).unwrap());
                            }
                            RvInstKind::Li { .. }
                            | RvInstKind::AluRR { .. }
                            | RvInstKind::AluRRI { .. }
                            | RvInstKind::AluRRR { .. }
                            | RvInstKind::FpuRR { .. }
                            | RvInstKind::FpuRRR { .. }
                            | RvInstKind::FpuRRRR { .. }
                            | RvInstKind::Load { .. }
                            | RvInstKind::Store { .. }
                            | RvInstKind::Ret
                            | RvInstKind::Call { .. }
                            | RvInstKind::La { .. }
                            | RvInstKind::LoadAddr { .. } => unreachable!(),
                        }
                    }
                    cursor.next(mctx);
                    block.remove(mctx);
                    local_changed = true;
                }
            }
        }

        if !local_changed {
            break;
        } else {
            changed = true;
        }
    }

    changed
}

pub fn run_peephole(mctx: &mut MContext<RvInst>, config: &LowerConfig) -> bool {
    let mut runner1 = PeepholeRunner::new();
    let mut runner2 = PeepholeRunner::new();
    let mut runner3 = PeepholeRunner::new();

    runner1.add_rule(li_dce());
    runner1.add_rule(remove_identity_move());
    runner1.add_rule(remove_redundant_move());

    runner2.add_rule(fuse_cmp_br());
    runner2.add_rule(fuse_fmul_faddfsub()); // aggressive

    runner3.add_rule(fuse_xori_cmp_br());

    if mctx.arch().contains("zba") {
        runner2.add_rule(fuse_shl_add());
    }

    let mut changed = false;

    changed |= runner1.run(mctx, config);
    changed |= runner2.run(mctx, config);
    changed |= runner3.run(mctx, config);

    changed
}

pub fn run_peephole_after_regalloc(mctx: &mut MContext<RvInst>, config: &LowerConfig) -> bool {
    let mut runner1 = PeepholeRunner::new();
    let mut runner2 = PeepholeRunner::new();

    runner1.add_rule(remove_identity_move());

    runner2.add_rule(remove_load_after_store());

    let mut changed = false;

    changed |= runner1.run(mctx, config);
    changed |= runner2.run(mctx, config);

    // NOTE: remove redundant jump need to be run after tail duplication
    changed |= remove_redundant_jump(mctx);
    changed |= remove_redundant_labels(mctx);
    changed |= remove_redundant_jump(mctx);

    changed
}
