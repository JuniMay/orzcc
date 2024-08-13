use std::{
    collections::{HashMap, HashSet},
    ops::Add,
};

use super::inst::{AluOpRRR, FpuOpRR, FpuOpRRR, FpuOpRRRR, RvInst, RvInstKind};
use crate::{
    backend::{inst::MInst, LowerConfig, MBlock, MContext, MFunc},
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
};

fn can_dual_issue(
    mctx: &MContext<RvInst>,
    inst1: RvInst,
    inst2: RvInst,
    config: &LowerConfig,
) -> bool {
    if dependence(mctx, inst1, inst2, config).contains(&Dependence::Raw)
        || dependence(mctx, inst1, inst2, config).contains(&Dependence::Waw)
    {
        return false;
    }

    // * At most one instruction accesses data memory.
    // * At most one instruction is a branch or jump.
    // * At most one instruction is a floating-point arithmetic operation.
    // * At most one instruction is an integer multiplication or division operation.
    // * Neither instruction explicitly accesses a CSR.
    let mut count_mem = 0;
    let mut count_branch = 0;
    let mut count_fp = 0;
    let mut count_muldiv = 0;

    match inst1.kind(mctx) {
        RvInstKind::AluRRR {
            op:
                AluOpRRR::Mul
                | AluOpRRR::Mulh
                | AluOpRRR::Mulhsu
                | AluOpRRR::Mulhu
                | AluOpRRR::Div
                | AluOpRRR::Divu
                | AluOpRRR::Divuw
                | AluOpRRR::Divw
                | AluOpRRR::Rem
                | AluOpRRR::Remu
                | AluOpRRR::Remuw
                | AluOpRRR::Remw,
            ..
        } => {
            count_muldiv += 1;
        }
        RvInstKind::Load { .. } | RvInstKind::Store { .. } => {
            count_mem += 1;
        }
        RvInstKind::FpuRR { .. } | RvInstKind::FpuRRR { .. } | RvInstKind::FpuRRRR { .. } => {
            count_fp += 1;
        }
        RvInstKind::Br { .. }
        | RvInstKind::J { .. }
        | RvInstKind::Call { .. }
        | RvInstKind::Ret => {
            count_branch += 1;
        }
        RvInstKind::Li { .. }
        | RvInstKind::AluRR { .. }
        | RvInstKind::AluRRI { .. }
        | RvInstKind::AluRRR { .. }
        | RvInstKind::La { .. }
        | RvInstKind::LoadAddr { .. } => {}
    }

    match inst2.kind(mctx) {
        RvInstKind::AluRRR {
            op:
                AluOpRRR::Mul
                | AluOpRRR::Mulh
                | AluOpRRR::Mulhsu
                | AluOpRRR::Mulhu
                | AluOpRRR::Div
                | AluOpRRR::Divu
                | AluOpRRR::Divuw
                | AluOpRRR::Divw
                | AluOpRRR::Rem
                | AluOpRRR::Remu
                | AluOpRRR::Remuw
                | AluOpRRR::Remw,
            ..
        } => {
            count_muldiv += 1;
        }
        RvInstKind::Load { .. } | RvInstKind::Store { .. } => {
            count_mem += 1;
        }
        RvInstKind::FpuRR { .. } | RvInstKind::FpuRRR { .. } | RvInstKind::FpuRRRR { .. } => {
            count_fp += 1;
        }
        RvInstKind::Br { .. }
        | RvInstKind::J { .. }
        | RvInstKind::Call { .. }
        | RvInstKind::Ret => {
            count_branch += 1;
        }
        RvInstKind::Li { .. }
        | RvInstKind::AluRR { .. }
        | RvInstKind::AluRRI { .. }
        | RvInstKind::AluRRR { .. }
        | RvInstKind::La { .. }
        | RvInstKind::LoadAddr { .. } => {}
    }

    if count_mem > 1 || count_branch > 1 || count_fp > 1 || count_muldiv > 1 {
        return false;
    }

    true
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Dependence {
    Raw,
    War,
    Waw,
    Mem,
    Control,
}

#[allow(clippy::wildcard_enum_match_arm)]
fn dependence(
    mctx: &MContext<RvInst>,
    inst1: RvInst,
    inst2: RvInst,
    config: &LowerConfig,
) -> HashSet<Dependence> {
    let clobbers1 = inst1.clobbers(mctx, config);
    let uses1 = inst1.uses(mctx, config);

    let clobbers2 = inst2.clobbers(mctx, config);
    let uses2 = inst2.uses(mctx, config);

    let mut deps = HashSet::new();

    // RAW & WAW
    for clobber1 in clobbers1 {
        for &use2 in uses2.iter() {
            if clobber1 == use2 {
                deps.insert(Dependence::Raw);
                break;
            }
        }

        for &clobber2 in clobbers2.iter() {
            if clobber1 == clobber2 {
                deps.insert(Dependence::Waw);
                break;
            }
        }
    }

    // WAR
    for use1 in uses1 {
        for &clobber2 in clobbers2.iter() {
            if use1 == clobber2 {
                deps.insert(Dependence::War);
                break;
            }
        }
    }

    // MEM & CONTROL
    match inst1.kind(mctx) {
        RvInstKind::Load { .. } | RvInstKind::Store { .. } => match inst2.kind(mctx) {
            RvInstKind::Load { .. } | RvInstKind::Store { .. } => {
                deps.insert(Dependence::Mem);
            }
            _ => {}
        },
        RvInstKind::Br { .. }
        | RvInstKind::J { .. }
        | RvInstKind::Call { .. }
        | RvInstKind::Ret => {
            deps.insert(Dependence::Control);
        }
        _ => {}
    }

    match inst2.kind(mctx) {
        RvInstKind::Br { .. }
        | RvInstKind::J { .. }
        | RvInstKind::Call { .. }
        | RvInstKind::Ret => {
            deps.insert(Dependence::Control);
        }
        _ => {}
    }

    deps
}

fn compute_stall(
    mctx: &mut MContext<RvInst>,
    inst1: RvInst,
    inst2: RvInst,
    config: &LowerConfig,
) -> usize {
    if can_dual_issue(mctx, inst1, inst2, config) {
        return 0;
    }

    match inst1.kind(mctx) {
        #[allow(clippy::wildcard_enum_match_arm)]
        RvInstKind::AluRRR { op, .. } => match op {
            AluOpRRR::Mul | AluOpRRR::Mulh | AluOpRRR::Mulhsu | AluOpRRR::Mulhu => 1,
            AluOpRRR::Div
            | AluOpRRR::Divu
            | AluOpRRR::Divuw
            | AluOpRRR::Divw
            | AluOpRRR::Rem
            | AluOpRRR::Remu
            | AluOpRRR::Remuw
            | AluOpRRR::Remw => 10,
            _ => 1,
        },
        RvInstKind::FpuRR {
            op: FpuOpRR::FsqrtS,
            ..
        }
        | RvInstKind::FpuRRR {
            op: FpuOpRRR::FdivS,
            ..
        } => 19,
        RvInstKind::FpuRR {
            op: FpuOpRR::FsqrtD,
            ..
        }
        | RvInstKind::FpuRRR {
            op: FpuOpRRR::FdivD,
            ..
        } => 29,
        RvInstKind::FpuRR { .. } | RvInstKind::FpuRRR { .. } | RvInstKind::FpuRRRR { .. } => 4,
        RvInstKind::Load { .. } | RvInstKind::Store { .. } => 3,
        RvInstKind::Li { .. }
        | RvInstKind::AluRR { .. }
        | RvInstKind::AluRRI { .. }
        | RvInstKind::Ret
        | RvInstKind::Call { .. }
        | RvInstKind::J { .. }
        | RvInstKind::Br { .. }
        | RvInstKind::La { .. }
        | RvInstKind::LoadAddr { .. } => 1,
    }
}

#[allow(clippy::wildcard_enum_match_arm)]
fn compute_rt(mctx: &mut MContext<RvInst>, inst: RvInst) -> Vec<ReservationTable> {
    match inst.kind(mctx) {
        RvInstKind::AluRRR { op, .. } => match op {
            AluOpRRR::Mul | AluOpRRR::Mulh | AluOpRRR::Mulhsu | AluOpRRR::Mulhu => {
                vec![
                    ReservationTable {
                        mul_ag: 1,
                        ..ReservationTable::zero()
                    },
                    ReservationTable {
                        mul_m1: 1,
                        ..ReservationTable::zero()
                    },
                    ReservationTable {
                        mul_m2: 1,
                        ..ReservationTable::zero()
                    },
                ]
            }
            AluOpRRR::Div | AluOpRRR::Divu | AluOpRRR::Divuw | AluOpRRR::Divw => {
                vec![
                    ReservationTable {
                        div: 1,
                        ..ReservationTable::zero()
                    };
                    // Latency = 2 cycles + log2(dividend) - log2(divisor) + 1 cycle
                    20 // FIXME: not accurate
                ]
            }
            _ => vec![ReservationTable {
                alu: 1,
                ..ReservationTable::zero()
            }],
        },
        RvInstKind::FpuRR { op, .. } => {
            match op {
                FpuOpRR::FsqrtS => {
                    // fsqrt is iterative, assume all units are occupied.
                    vec![
                        ReservationTable {
                            fpu_0: 1,
                            fpu_1: 1,
                            fpu_2: 1,
                            fpu_3: 1,
                            fpu_4: 1,
                            fpu_5: 1,
                            fpu_6: 1,
                            ..ReservationTable::zero()
                        };
                        20
                    ]
                }
                FpuOpRR::FsqrtD => {
                    // fsqrt is iterative, assume all units are occupied.
                    vec![
                        ReservationTable {
                            fpu_0: 1,
                            fpu_1: 1,
                            fpu_2: 1,
                            fpu_3: 1,
                            fpu_4: 1,
                            fpu_5: 1,
                            fpu_6: 1,
                            ..ReservationTable::zero()
                        };
                        30
                    ]
                }
                FpuOpRR::FcvtWS
                | FpuOpRR::FcvtLS
                | FpuOpRR::FcvtSL
                | FpuOpRR::FcvtWuS
                | FpuOpRR::FcvtLuS
                | FpuOpRR::FcvtSLu
                | FpuOpRR::FcvtWD
                | FpuOpRR::FcvtLD
                | FpuOpRR::FcvtWuD
                | FpuOpRR::FcvtLuD
                | FpuOpRR::FclassS
                | FpuOpRR::FclassD => {
                    vec![
                        ReservationTable {
                            fpu_0: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_1: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_2: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_3: 1,
                            ..ReservationTable::zero()
                        },
                    ]
                }
                FpuOpRR::FcvtSW
                | FpuOpRR::FcvtSWu
                | FpuOpRR::FcvtDW
                | FpuOpRR::FcvtDWu
                | FpuOpRR::FcvtSD
                | FpuOpRR::FcvtDS
                | FpuOpRR::FmvWX => {
                    vec![
                        ReservationTable {
                            fpu_0: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_1: 1,
                            ..ReservationTable::zero()
                        },
                    ]
                }
                FpuOpRR::FcvtDL | FpuOpRR::FcvtDLu | FpuOpRR::FmvDX => {
                    // 6 cycles latency
                    vec![
                        ReservationTable {
                            fpu_0: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_1: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_2: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_3: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_4: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_5: 1,
                            ..ReservationTable::zero()
                        },
                    ]
                }
                FpuOpRR::FmvXD | FpuOpRR::FmvXW => {
                    // 1 cycle latency
                    vec![ReservationTable {
                        fpu_0: 1,
                        ..ReservationTable::zero()
                    }]
                }
            }
        }

        RvInstKind::FpuRRR { op, .. } => {
            match op {
                FpuOpRRR::FaddS | FpuOpRRR::FsubS | FpuOpRRR::FmulS => {
                    // 5 cycles
                    vec![
                        ReservationTable {
                            fpu_0: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_1: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_2: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_3: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_4: 1,
                            ..ReservationTable::zero()
                        },
                    ]
                }
                FpuOpRRR::FaddD | FpuOpRRR::FsubD | FpuOpRRR::FmulD => {
                    // 7 cycles
                    vec![
                        ReservationTable {
                            fpu_0: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_1: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_2: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_3: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_4: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_5: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_6: 1,
                            ..ReservationTable::zero()
                        },
                    ]
                }
                FpuOpRRR::FeqS
                | FpuOpRRR::FeqD
                | FpuOpRRR::FleS
                | FpuOpRRR::FltS
                | FpuOpRRR::FleD
                | FpuOpRRR::FltD => {
                    // 4 cycles
                    vec![
                        ReservationTable {
                            fpu_0: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_1: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_2: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_3: 1,
                            ..ReservationTable::zero()
                        },
                    ]
                }
                FpuOpRRR::FmaxS
                | FpuOpRRR::FmaxD
                | FpuOpRRR::FminS
                | FpuOpRRR::FminD
                | FpuOpRRR::FsgnjS
                | FpuOpRRR::FsgnjnS
                | FpuOpRRR::FsgnjxS
                | FpuOpRRR::FsgnjD
                | FpuOpRRR::FsgnjnD
                | FpuOpRRR::FsgnjxD => {
                    // 2 cycles
                    vec![
                        ReservationTable {
                            fpu_0: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_1: 1,
                            ..ReservationTable::zero()
                        },
                    ]
                }
                FpuOpRRR::FdivS => {
                    vec![
                        ReservationTable {
                            fpu_0: 1,
                            fpu_1: 1,
                            fpu_2: 1,
                            fpu_3: 1,
                            fpu_4: 1,
                            fpu_5: 1,
                            fpu_6: 1,
                            ..ReservationTable::zero()
                        };
                        20
                    ]
                }
                FpuOpRRR::FdivD => {
                    vec![
                        ReservationTable {
                            fpu_0: 1,
                            fpu_1: 1,
                            fpu_2: 1,
                            fpu_3: 1,
                            fpu_4: 1,
                            fpu_5: 1,
                            fpu_6: 1,
                            ..ReservationTable::zero()
                        };
                        30
                    ]
                }
            }
        }
        RvInstKind::FpuRRRR { op, .. } => {
            match op {
                FpuOpRRRR::FmaddS | FpuOpRRRR::FnmaddS | FpuOpRRRR::FmsubS | FpuOpRRRR::FnmsubS => {
                    // 5 cycles
                    vec![
                        ReservationTable {
                            fpu_0: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_1: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_2: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_3: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_4: 1,
                            ..ReservationTable::zero()
                        },
                    ]
                }
                FpuOpRRRR::FmaddD | FpuOpRRRR::FnmaddD | FpuOpRRRR::FmsubD | FpuOpRRRR::FnmsubD => {
                    // 7 cycles
                    vec![
                        ReservationTable {
                            fpu_0: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_1: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_2: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_3: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_4: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_5: 1,
                            ..ReservationTable::zero()
                        },
                        ReservationTable {
                            fpu_6: 1,
                            ..ReservationTable::zero()
                        },
                    ]
                }
            }
        }
        RvInstKind::Load { .. } | RvInstKind::Store { .. } => vec![
            ReservationTable {
                mem: 1,
                ..ReservationTable::zero()
            };
            3
        ],
        RvInstKind::AluRR { .. }
        | RvInstKind::AluRRI { .. }
        | RvInstKind::Li { .. }
        | RvInstKind::La { .. } => vec![ReservationTable {
            alu: 1,
            ..ReservationTable::zero()
        }],
        RvInstKind::Ret
        | RvInstKind::Call { .. }
        | RvInstKind::J { .. }
        | RvInstKind::Br { .. }
        | RvInstKind::LoadAddr { .. } => vec![ReservationTable::zero()],
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ReservationTable {
    alu: u8,
    mem: u8,
    div: u8,
    mul_ag: u8,
    mul_m1: u8,
    mul_m2: u8,
    // at most 7, just represents the pipeline of fma unit
    fpu_0: u8,
    fpu_1: u8,
    fpu_2: u8,
    fpu_3: u8,
    fpu_4: u8,
    fpu_5: u8,
    fpu_6: u8,
}

impl ReservationTable {
    fn new() -> Self {
        Self {
            alu: 2,
            mem: 1,
            div: 1,
            mul_ag: 1,
            mul_m1: 1,
            mul_m2: 1,
            fpu_0: 1,
            fpu_1: 1,
            fpu_2: 1,
            fpu_3: 1,
            fpu_4: 1,
            fpu_5: 1,
            fpu_6: 1,
        }
    }

    fn zero() -> Self {
        Self {
            alu: 0,
            mem: 0,
            div: 0,
            mul_ag: 0,
            mul_m1: 0,
            mul_m2: 0,
            fpu_0: 0,
            fpu_1: 0,
            fpu_2: 0,
            fpu_3: 0,
            fpu_4: 0,
            fpu_5: 0,
            fpu_6: 0,
        }
    }

    /// Check if the reservation table exceeds the other reservation table.
    fn exceeds(&self, other: ReservationTable) -> bool {
        self.alu > other.alu
            || self.mem > other.mem
            || self.div > other.div
            || self.mul_ag > other.mul_ag
            || self.mul_m1 > other.mul_m1
            || self.mul_m2 > other.mul_m2
            || self.fpu_0 > other.fpu_0
            || self.fpu_1 > other.fpu_1
            || self.fpu_2 > other.fpu_2
            || self.fpu_3 > other.fpu_3
            || self.fpu_4 > other.fpu_4
            || self.fpu_5 > other.fpu_5
            || self.fpu_6 > other.fpu_6
    }
}

impl Add for ReservationTable {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            alu: self.alu + other.alu,
            mem: self.mem + other.mem,
            div: self.div + other.div,
            mul_ag: self.mul_ag + other.mul_ag,
            mul_m1: self.mul_m1 + other.mul_m1,
            mul_m2: self.mul_m2 + other.mul_m2,
            fpu_0: self.fpu_0 + other.fpu_0,
            fpu_1: self.fpu_1 + other.fpu_1,
            fpu_2: self.fpu_2 + other.fpu_2,
            fpu_3: self.fpu_3 + other.fpu_3,
            fpu_4: self.fpu_4 + other.fpu_4,
            fpu_5: self.fpu_5 + other.fpu_5,
            fpu_6: self.fpu_6 + other.fpu_6,
        }
    }
}

#[derive(Default)]
struct DepGraph {
    succs: HashMap<RvInst, Vec<RvInst>>,
    preds: HashMap<RvInst, Vec<RvInst>>,
}

impl DepGraph {
    fn add_dep(&mut self, inst1: RvInst, inst2: RvInst) {
        self.succs.entry(inst1).or_default().push(inst2);
        self.preds.entry(inst2).or_default().push(inst1);
    }

    fn preds(&self, inst: RvInst) -> &[RvInst] {
        self.preds.get(&inst).map_or(&[], |preds| preds.as_slice())
    }

    fn succs(&self, inst: RvInst) -> &[RvInst] {
        self.succs.get(&inst).map_or(&[], |succs| succs.as_slice())
    }
}

fn schedule_mblock(mctx: &mut MContext<RvInst>, mblock: MBlock<RvInst>, config: &LowerConfig) {
    let mut dep_graph = DepGraph::default();
    let mut degrees: HashMap<RvInst, usize> = HashMap::new();

    // compute the data dependence graph
    for inst in mblock.iter(mctx) {
        degrees.entry(inst).or_insert(0);

        let mut next = inst.next(mctx);
        while let Some(next_inst) = next {
            let deps = dependence(mctx, inst, next_inst, config);
            if !deps.is_empty() {
                dep_graph.add_dep(inst, next_inst);
                let degree = degrees.entry(next_inst).or_insert(0);
                *degree += 1;
            }
            next = next_inst.next(mctx);
        }
    }

    // compute topological order
    let mut topsort = Vec::new();
    let mut queue = Vec::new();

    for (inst, degree) in degrees.iter() {
        if *degree == 0 {
            queue.push(*inst);
        }
    }

    while let Some(inst) = queue.pop() {
        topsort.push(inst);

        for succ in dep_graph.succs(inst).iter() {
            let degree = degrees.get_mut(succ).unwrap();
            *degree -= 1;
            if *degree == 0 {
                queue.push(*succ);
            }
        }
    }

    dbg!(topsort.len());

    let mut scheduled: HashMap<RvInst, usize> = HashMap::new();
    let mut all_rts: Vec<ReservationTable> = Vec::new();

    let r = ReservationTable::new();

    for n in topsort {
        let mut s = 0;
        for &pred in dep_graph.preds(n).iter() {
            s = s.max(scheduled[&pred] + compute_stall(mctx, pred, n, config));
        }

        let rts = compute_rt(mctx, n);

        loop {
            all_rts.resize(s + rts.len(), ReservationTable::zero());

            let mut exceed = false;
            for (i, &rt) in rts.iter().enumerate() {
                if (all_rts[s + i] + rt).exceeds(r) {
                    exceed = true;
                    break;
                }
            }

            if !exceed {
                break;
            }

            s += 1;
        }

        scheduled.insert(n, s);

        for (i, &rt) in rts.iter().enumerate() {
            all_rts[s + i] = all_rts[s + i] + rt;
        }
    }

    // now, sort the instructions based on the scheduled order
    let mut insts = mblock.iter(mctx).collect::<Vec<_>>();
    insts.sort_by_key(|inst| scheduled[inst]);

    // unlink all, and push back
    for inst in insts {
        inst.unlink(mctx);
        mblock.push_back(mctx, inst);
    }
}

fn schedule_mfunc(mctx: &mut MContext<RvInst>, mfunc: MFunc<RvInst>, config: &LowerConfig) {
    let mut cursor = mfunc.cursor();
    while let Some(block) = cursor.next(mctx) {
        schedule_mblock(mctx, block, config);
    }
}

pub fn schedule(mctx: &mut MContext<RvInst>, config: &LowerConfig) {
    let funcs = mctx
        .funcs
        .iter_mut()
        .map(|(_, func_data)| func_data.self_ptr())
        .collect::<Vec<_>>();

    for func in funcs {
        if func.is_external(mctx) {
            continue;
        }
        schedule_mfunc(mctx, func, config);
    }
}
