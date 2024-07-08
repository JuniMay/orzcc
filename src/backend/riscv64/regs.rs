use std::fmt;

use crate::backend::{regs::Reg, PReg, RegKind};

pub const fn zero() -> PReg { PReg::new(0, RegKind::General) }

pub const fn ra() -> PReg { PReg::new(1, RegKind::General) }

pub const fn sp() -> PReg { PReg::new(2, RegKind::General) }

pub const fn gp() -> PReg { PReg::new(3, RegKind::General) }

pub const fn tp() -> PReg { PReg::new(4, RegKind::General) }

pub const fn t0() -> PReg { PReg::new(5, RegKind::General) }

pub const fn t1() -> PReg { PReg::new(6, RegKind::General) }

pub const fn t2() -> PReg { PReg::new(7, RegKind::General) }

pub const fn s0() -> PReg { PReg::new(8, RegKind::General) }

pub const fn fp() -> PReg { s0() }

pub const fn s1() -> PReg { PReg::new(9, RegKind::General) }

pub const fn a0() -> PReg { PReg::new(10, RegKind::General) }

pub const fn a1() -> PReg { PReg::new(11, RegKind::General) }

pub const fn a2() -> PReg { PReg::new(12, RegKind::General) }

pub const fn a3() -> PReg { PReg::new(13, RegKind::General) }

pub const fn a4() -> PReg { PReg::new(14, RegKind::General) }

pub const fn a5() -> PReg { PReg::new(15, RegKind::General) }

pub const fn a6() -> PReg { PReg::new(16, RegKind::General) }

pub const fn a7() -> PReg { PReg::new(17, RegKind::General) }

pub const fn s2() -> PReg { PReg::new(18, RegKind::General) }

pub const fn s3() -> PReg { PReg::new(19, RegKind::General) }

pub const fn s4() -> PReg { PReg::new(20, RegKind::General) }

pub const fn s5() -> PReg { PReg::new(21, RegKind::General) }

pub const fn s6() -> PReg { PReg::new(22, RegKind::General) }

pub const fn s7() -> PReg { PReg::new(23, RegKind::General) }

pub const fn s8() -> PReg { PReg::new(24, RegKind::General) }

pub const fn s9() -> PReg { PReg::new(25, RegKind::General) }

pub const fn s10() -> PReg { PReg::new(26, RegKind::General) }

pub const fn s11() -> PReg { PReg::new(27, RegKind::General) }

pub const fn t3() -> PReg { PReg::new(28, RegKind::General) }

pub const fn t4() -> PReg { PReg::new(29, RegKind::General) }

pub const fn t5() -> PReg { PReg::new(30, RegKind::General) }

pub const fn t6() -> PReg { PReg::new(31, RegKind::General) }

pub const fn ft0() -> PReg { PReg::new(0, RegKind::Float) }

pub const fn ft1() -> PReg { PReg::new(1, RegKind::Float) }

pub const fn ft2() -> PReg { PReg::new(2, RegKind::Float) }

pub const fn ft3() -> PReg { PReg::new(3, RegKind::Float) }

pub const fn ft4() -> PReg { PReg::new(4, RegKind::Float) }

pub const fn ft5() -> PReg { PReg::new(5, RegKind::Float) }

pub const fn ft6() -> PReg { PReg::new(6, RegKind::Float) }

pub const fn ft7() -> PReg { PReg::new(7, RegKind::Float) }

pub const fn fs0() -> PReg { PReg::new(8, RegKind::Float) }

pub const fn fs1() -> PReg { PReg::new(9, RegKind::Float) }

pub const fn fa0() -> PReg { PReg::new(10, RegKind::Float) }

pub const fn fa1() -> PReg { PReg::new(11, RegKind::Float) }

pub const fn fa2() -> PReg { PReg::new(12, RegKind::Float) }

pub const fn fa3() -> PReg { PReg::new(13, RegKind::Float) }

pub const fn fa4() -> PReg { PReg::new(14, RegKind::Float) }

pub const fn fa5() -> PReg { PReg::new(15, RegKind::Float) }

pub const fn fa6() -> PReg { PReg::new(16, RegKind::Float) }

pub const fn fa7() -> PReg { PReg::new(17, RegKind::Float) }

pub const fn fs2() -> PReg { PReg::new(18, RegKind::Float) }

pub const fn fs3() -> PReg { PReg::new(19, RegKind::Float) }

pub const fn fs4() -> PReg { PReg::new(20, RegKind::Float) }

pub const fn fs5() -> PReg { PReg::new(21, RegKind::Float) }

pub const fn fs6() -> PReg { PReg::new(22, RegKind::Float) }

pub const fn fs7() -> PReg { PReg::new(23, RegKind::Float) }

pub const fn fs8() -> PReg { PReg::new(24, RegKind::Float) }

pub const fn fs9() -> PReg { PReg::new(25, RegKind::Float) }

pub const fn fs10() -> PReg { PReg::new(26, RegKind::Float) }

pub const fn fs11() -> PReg { PReg::new(27, RegKind::Float) }

pub const fn ft8() -> PReg { PReg::new(28, RegKind::Float) }

pub const fn ft9() -> PReg { PReg::new(29, RegKind::Float) }

pub const fn ft10() -> PReg { PReg::new(30, RegKind::Float) }

pub const fn ft11() -> PReg { PReg::new(31, RegKind::Float) }

pub const fn display_preg(reg: PReg) -> &'static str {
    match reg.kind() {
        RegKind::General => match reg.num() {
            0 => "zero",
            1 => "ra",
            2 => "sp",
            3 => "gp",
            4 => "tp",
            5 => "t0",
            6 => "t1",
            7 => "t2",
            8 => "s0",
            9 => "s1",
            10 => "a0",
            11 => "a1",
            12 => "a2",
            13 => "a3",
            14 => "a4",
            15 => "a5",
            16 => "a6",
            17 => "a7",
            18 => "s2",
            19 => "s3",
            20 => "s4",
            21 => "s5",
            22 => "s6",
            23 => "s7",
            24 => "s8",
            25 => "s9",
            26 => "s10",
            27 => "s11",
            28 => "t3",
            29 => "t4",
            30 => "t5",
            31 => "t6",
            _ => "<invalid>",
        },
        RegKind::Float => match reg.num() {
            0 => "ft0",
            1 => "ft1",
            2 => "ft2",
            3 => "ft3",
            4 => "ft4",
            5 => "ft5",
            6 => "ft6",
            7 => "ft7",
            8 => "fs0",
            9 => "fs1",
            10 => "fa0",
            11 => "fa1",
            12 => "fa2",
            13 => "fa3",
            14 => "fa4",
            15 => "fa5",
            16 => "fa6",
            17 => "fa7",
            18 => "fs2",
            19 => "fs3",
            20 => "fs4",
            21 => "fs5",
            22 => "fs6",
            23 => "fs7",
            24 => "fs8",
            25 => "fs9",
            26 => "fs10",
            27 => "fs11",
            28 => "ft8",
            29 => "ft9",
            30 => "ft10",
            31 => "ft11",
            _ => "<invalid>",
        },
        RegKind::Vector => "<invalid>",
    }
}

impl fmt::Display for PReg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.num() < 32 {
            write!(f, "{}", display_preg(*self))
        } else {
            unreachable!("invalid register number")
        }
    }
}

pub fn display(reg: Reg) -> String {
    match reg {
        Reg::P(reg) => display_preg(reg).into(),
        Reg::V(reg) => format!("{}", reg),
    }
}

pub const INT_ARG_REGS: [PReg; 8] = [a0(), a1(), a2(), a3(), a4(), a5(), a6(), a7()];

pub const FP_ARG_REGS: [PReg; 8] = [fa0(), fa1(), fa2(), fa3(), fa4(), fa5(), fa6(), fa7()];

pub const CALLER_SAVED_REGS: [PReg; 36] = [
    ra(),
    t0(),
    t1(),
    t2(),
    t3(),
    t4(),
    t5(),
    t6(),
    a0(),
    a1(),
    a2(),
    a3(),
    a4(),
    a5(),
    a6(),
    a7(),
    ft0(),
    ft1(),
    ft2(),
    ft3(),
    ft4(),
    ft5(),
    ft6(),
    ft7(),
    ft8(),
    ft9(),
    ft10(),
    ft11(),
    fa0(),
    fa1(),
    fa2(),
    fa3(),
    fa4(),
    fa5(),
    fa6(),
    fa7(),
];

pub const CALLEE_SAVED_REGS: [PReg; 25] = [
    sp(),
    s0(),
    s1(),
    s2(),
    s3(),
    s4(),
    s5(),
    s6(),
    s7(),
    s8(),
    s9(),
    s10(),
    s11(),
    fs0(),
    fs1(),
    fs2(),
    fs3(),
    fs4(),
    fs5(),
    fs6(),
    fs7(),
    fs8(),
    fs9(),
    fs10(),
    fs11(),
];

pub const RETURN_REGS: [PReg; 2] = [
    a0(),
    // since we only support 64-bit return value, a1 is not used
    // a1(),
    fa0(),
    // since we only support 64-bit return value, fa1 is not used
    // fa1(),
];
