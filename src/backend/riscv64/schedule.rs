use std::{
    collections::{HashMap, HashSet},
    ops::Add,
};

use super::inst::{AluOpRRR, RvInst, RvInstKind};
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
                | AluOpRRR::Divw,
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
        _ => {}
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
                | AluOpRRR::Divw,
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
        _ => {}
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

fn dependence(
    mctx: &MContext<RvInst>,
    inst1: RvInst,
    inst2: RvInst,
    config: &LowerConfig,
) -> HashSet<Dependence> {
    let defs1 = inst1.defs(mctx, config);
    let uses1 = inst1.uses(mctx, config);

    let defs2 = inst2.defs(mctx, config);
    let uses2 = inst2.uses(mctx, config);

    let mut deps = HashSet::new();

    // RAW & WAW
    for def1 in defs1 {
        for &use2 in uses2.iter() {
            if def1 == use2 {
                deps.insert(Dependence::Raw);
                break;
            }
        }

        for &def2 in defs2.iter() {
            if def1 == def2 {
                deps.insert(Dependence::Waw);
                break;
            }
        }
    }

    // WAR
    for use1 in uses1 {
        for &def2 in defs2.iter() {
            if use1 == def2 {
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
        RvInstKind::AluRRR { op, .. } => match op {
            AluOpRRR::Mul | AluOpRRR::Mulh | AluOpRRR::Mulhsu | AluOpRRR::Mulhu => {
                match inst2.kind(mctx) {
                    RvInstKind::AluRRR {
                        op: AluOpRRR::Mul | AluOpRRR::Mulh | AluOpRRR::Mulhsu | AluOpRRR::Mulhu,
                        ..
                    } => 1,
                    _ => 3,
                }
            }
            AluOpRRR::Div | AluOpRRR::Divu | AluOpRRR::Divuw | AluOpRRR::Divw => 10,
            _ => 1,
        },
        RvInstKind::FpuRR { .. } | RvInstKind::FpuRRR { .. } | RvInstKind::FpuRRRR { .. } => 4,
        RvInstKind::Load { .. } | RvInstKind::Store { .. } => 3,
        _ => 1,
    }
}

fn compute_rt(
    mctx: &mut MContext<RvInst>,
    inst: RvInst,
    config: &LowerConfig,
) -> Vec<ReservationTable> {
    match inst.kind(mctx) {
        RvInstKind::AluRRR { op, .. } => match op {
            AluOpRRR::Mul | AluOpRRR::Mulh | AluOpRRR::Mulhsu | AluOpRRR::Mulhu => {
                vec![
                    ReservationTable {
                        mul_stage0: 1,
                        mul_stage1: 0,
                        mul_stage2: 0,
                        ..ReservationTable::zero()
                    },
                    ReservationTable {
                        mul_stage0: 0,
                        mul_stage1: 1,
                        mul_stage2: 0,
                        ..ReservationTable::zero()
                    },
                    ReservationTable {
                        mul_stage0: 0,
                        mul_stage1: 0,
                        mul_stage2: 1,
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
                    10
                ]
            }
            _ => vec![ReservationTable {
                alu: 1,
                ..ReservationTable::zero()
            }],
        },
        RvInstKind::FpuRR { .. } | RvInstKind::FpuRRR { .. } | RvInstKind::FpuRRRR { .. } => {
            vec![
                ReservationTable {
                    fpu: 1,
                    ..ReservationTable::zero()
                };
                4
            ]
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
        _ => vec![ReservationTable::zero()],
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct ReservationTable {
    alu: u8,
    mem: u8,
    fpu: u8,
    div: u8,
    mul_stage0: u8,
    mul_stage1: u8,
    mul_stage2: u8,
}

impl ReservationTable {
    fn new() -> Self {
        Self {
            alu: 2,
            mem: 1,
            fpu: 1,
            div: 1,
            mul_stage0: 1,
            mul_stage1: 1,
            mul_stage2: 1,
        }
    }

    fn zero() -> Self {
        Self {
            alu: 0,
            mem: 0,
            fpu: 0,
            div: 0,
            mul_stage0: 0,
            mul_stage1: 0,
            mul_stage2: 0,
        }
    }

    /// Check if the reservation table exceeds the other reservation table.
    fn exceeds(&self, other: ReservationTable) -> bool {
        self.alu > other.alu
            || self.mem > other.mem
            || self.fpu > other.fpu
            || self.div > other.div
            || self.mul_stage0 > other.mul_stage0
            || self.mul_stage1 > other.mul_stage1
            || self.mul_stage2 > other.mul_stage2
    }
}

impl Add for ReservationTable {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        Self {
            alu: self.alu + other.alu,
            mem: self.mem + other.mem,
            fpu: self.fpu + other.fpu,
            div: self.div + other.div,
            mul_stage0: self.mul_stage0 + other.mul_stage0,
            mul_stage1: self.mul_stage1 + other.mul_stage1,
            mul_stage2: self.mul_stage2 + other.mul_stage2,
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

        let rts = compute_rt(mctx, n, config);

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
