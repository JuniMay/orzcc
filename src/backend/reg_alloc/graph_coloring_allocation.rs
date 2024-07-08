use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
};

use super::live_interval_analysis::{self, LiveInterval};
use crate::{
    backend::{inst::MInst, lower::MemLoc, regs::Reg, LowerContext, LowerSpec, MFunc, RegKind},
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
};

#[derive(Debug, Clone)]
pub struct InterferenceGraph {
    pub graph: HashMap<Reg, HashSet<Reg>>,
    pub kind: RegKind,
}

impl InterferenceGraph {
    pub fn new(kind: RegKind) -> Self {
        Self {
            graph: HashMap::new(),
            kind,
        }
    }

    pub fn add_edge(&mut self, u: Reg, v: Reg) {
        self.graph.entry(u).or_default().insert(v);
        self.graph.entry(v).or_default().insert(u);
    }

    pub fn remove_edge(&mut self, u: Reg, v: Reg) {
        self.graph.entry(u).and_modify(|e| {
            e.remove(&v);
        });
        self.graph.entry(v).and_modify(|e| {
            e.remove(&u);
        });
    }

    pub fn adjacent(&self, u: Reg) -> Option<&HashSet<Reg>> { self.graph.get(&u) }

    pub fn degree(&self, u: Reg) -> usize { self.graph.get(&u).map(|e| e.len()).unwrap_or(0) }

    pub fn remove_node(&mut self, u: Reg) {
        if let Some(neighbors) = self.graph.remove(&u) {
            for v in neighbors {
                self.graph.entry(v).and_modify(|e| {
                    e.remove(&u);
                });
            }
        }
    }

    pub fn construct_from_live_interval(&mut self, live_ranges: &LiveInterval) {
        match self.kind {
            RegKind::General => {
                for (reg, live_interval) in live_ranges.intervals.iter() {
                    if reg.kind() == RegKind::General {
                        self.graph.entry(*reg).or_default();
                        for (other_reg, other_live_interval) in live_ranges.intervals.iter() {
                            if other_reg.kind() == RegKind::General
                                && reg != other_reg
                                && live_interval.intersects(other_live_interval, true)
                            {
                                self.add_edge(*reg, *other_reg);
                            }
                        }
                    }
                }
            }
            RegKind::Float => {
                for (reg, live_interval) in live_ranges.intervals.iter() {
                    if reg.kind() == RegKind::Float {
                        self.graph.entry(*reg).or_default();
                        for (other_reg, other_live_interval) in live_ranges.intervals.iter() {
                            if other_reg.kind() == RegKind::Float
                                && reg != other_reg
                                && live_interval.intersects(other_live_interval, true)
                            {
                                self.add_edge(*reg, *other_reg);
                            }
                        }
                    }
                }
            }
            RegKind::Vector => {
                unimplemented!(
                    "Vector register interference graph construction is not implemented yet"
                );
            }
        }
    }

    pub fn to_mermaid(&self) -> String {
        let mut s = String::new();
        s.push_str("graph LR;\n");
        for (u, neighbors) in self.graph.iter() {
            for v in neighbors {
                s.push_str(&format!("{:#?} --- {:#?};\n", u, v));
            }
        }
        s
    }

    /// Returns true if only precolored nodes (physical registers) are left in
    /// the graph
    pub fn all_colored(&self) -> bool {
        self.graph.iter().all(|(reg, _)| match reg {
            Reg::P(_) => true,
            Reg::V(_) => false,
        })
    }

    /// Retrun all the precolored nodes in the graph
    pub fn precolored(&self) -> HashSet<Reg> {
        match self.kind {
            RegKind::General => self
                .graph
                .iter()
                .filter(|(reg, _)| reg.kind() == RegKind::General)
                .map(|(reg, _)| *reg)
                .collect(),
            RegKind::Float => self
                .graph
                .iter()
                .filter(|(reg, _)| reg.kind() == RegKind::Float)
                .map(|(reg, _)| *reg)
                .collect(),
            RegKind::Vector => {
                unimplemented!("Vector register interference graph is not implemented")
            }
        }
    }
}

// /// Result of register allocation
// /// Contains the mapping of virtual registers to physical registers
// /// and the kind of registers that were allocated
// #[derive(Debug, Clone)]
// pub struct AllocationResult {
//     pub result: HashMap<Reg, Reg>,
//     pub kind: RegKind,
// }

#[derive(Default)]
pub struct GraphColoringAllocation {
    pub total_spills: usize,
    pub total_loads_added: usize,
    pub total_stores_added: usize,
}

impl GraphColoringAllocation {
    pub fn new() -> Self { Self::default() }

    pub fn run_on_function<S>(&mut self, ctx: &mut LowerContext<S>, func: MFunc<S::I>)
    where
        S: LowerSpec,
        S::I: Hash,
    {
        let mut allocation_results = HashMap::new();
        let mut spilled_registers = HashSet::new();

        // allocate general purpose and floating point registers separately
        for kind in &[RegKind::General, RegKind::Float] {
            let mut working_allocation_results;
            loop {
                working_allocation_results = HashMap::new(); // allocation results for this round

                // analyze live intervals
                let live_intervals = live_interval_analysis::analyze_on_function(ctx, func);

                // construct interference graph
                let mut interference_graph = InterferenceGraph::new(*kind);
                interference_graph.construct_from_live_interval(&live_intervals);

                // simplify the graph
                let mut stack = Vec::new();
                let mut working_graph = interference_graph.clone();

                // iterat until only precolored nodes are left
                while !working_graph.all_colored() {
                    // find a node with degree less than the number of allocatable registers
                    if let Some(reg) = working_graph
                        .graph
                        .iter()
                        .find(|(reg, neighbors)| {
                            reg.kind() == *kind
                                && reg.is_vreg()
                                && neighbors.len() < S::allocatable_regs().len()
                        })
                        .map(|(reg, _)| *reg)
                    {
                        // push the node to the stack
                        stack.push(reg);
                        working_graph.remove_node(reg);
                    } else {
                        // if no such node is found, spill a node
                        let reg = Self::choose_spill_candidate(&working_graph, &spilled_registers);
                        stack.push(reg);
                        working_graph.remove_node(reg);
                    }
                }

                let mut spills = HashSet::new(); // spilled registers in this round

                // select
                while let Some(reg) = stack.pop() {
                    let mut available_colors = match kind {
                        RegKind::General => S::allocatable_gp_regs().clone(),
                        RegKind::Float => S::allocatable_fp_regs().clone(),
                        RegKind::Vector => {
                            unimplemented!("Vector register allocation is not implemented")
                        }
                    };
                    // remove colors of neighbors
                    for neighbor in interference_graph.adjacent(reg).unwrap_or(&HashSet::new()) {
                        match neighbor {
                            Reg::P(preg) => {
                                // for precolored nodes, remove the color from available colors
                                available_colors.retain(|&c| c != *preg)
                            }
                            Reg::V(_) => {
                                // for virtual nodes, remove the color of the allocated register
                                if let Some(allocated_reg) =
                                    working_allocation_results.get(neighbor)
                                {
                                    available_colors.retain(|&c| c != *allocated_reg)
                                }
                            }
                        }
                    }
                    if let Some(color) = available_colors.first() {
                        // color the node
                        working_allocation_results.insert(reg, *color);
                    } else {
                        // we have to spill the node
                        spills.insert(reg);
                    }
                }

                // if no spills are found, we are done
                if spills.is_empty() {
                    break;
                }

                // do the actual spilling
                let mut spill_slots = HashMap::new(); // mapping of spilled registers to spill slots
                for reg in spills {
                    // allocate a spill slot on the stack
                    func.add_storage_stack_size(ctx.mctx_mut(), 8);
                    let stack_offset = func.storage_stack_size(ctx.mctx());
                    spill_slots.insert(reg, stack_offset);
                    spilled_registers.insert(reg);
                    self.total_spills += 1;
                }

                // add loads and stores
                let config = &ctx.config.clone();
                let mut curr_block = func.head(ctx.mctx_mut());
                while let Some(block) = curr_block {
                    let mut curr_inst = block.head(ctx.mctx_mut());
                    while let Some(inst) = curr_inst {
                        let next_inst = inst.next(ctx.mctx_mut());
                        for reg in inst.uses(ctx.mctx_mut(), config) {
                            if let Some(spill_slot) = spill_slots.get(&reg) {
                                // add load before use
                                S::gen_spill_load(
                                    ctx,
                                    reg,
                                    MemLoc::Slot {
                                        offset: -(*spill_slot as i64),
                                    },
                                    inst,
                                );
                                self.total_loads_added += 1;
                            }
                        }
                        for reg in inst.defs(ctx.mctx_mut(), config) {
                            if let Some(spill_slot) = spill_slots.get(&reg) {
                                // add store after def
                                S::gen_spill_store(
                                    ctx,
                                    reg,
                                    MemLoc::Slot {
                                        offset: -(*spill_slot as i64),
                                    },
                                    inst,
                                );
                                self.total_stores_added += 1;
                            }
                        }
                        curr_inst = next_inst;
                    }
                    curr_block = block.next(ctx.mctx_mut());
                }
            }

            // merge the allocation results
            allocation_results.extend(working_allocation_results);
        }

        // mark used callee-saved registers
        for used_reg in allocation_results.values() {
            if S::callee_saved_regs().contains(used_reg) {
                func.add_saved_reg(ctx.mctx_mut(), *used_reg)
            }
        }

        // replace virtual registers with physical registers
        let config = &ctx.config.clone();
        let mut curr_block = func.head(ctx.mctx_mut());
        while let Some(block) = curr_block {
            let mut curr_inst = block.head(ctx.mctx_mut());
            while let Some(inst) = curr_inst {
                let next_inst = inst.next(ctx.mctx_mut());
                for reg in inst.all_regs(ctx.mctx_mut(), config) {
                    if let Some(allocated_reg) = allocation_results.get(&reg) {
                        inst.replace_reg(ctx.mctx_mut(), reg, (*allocated_reg).into());
                    }
                }
                curr_inst = next_inst;
            }
            curr_block = block.next(ctx.mctx_mut());
        }
    }

    fn choose_spill_candidate(graph: &InterferenceGraph, spilled_registers: &HashSet<Reg>) -> Reg {
        let mut max_degree = 0;
        let mut spill_candidate = None;

        // find the node with the highest degree
        for (reg, neighbors) in graph.graph.iter() {
            if reg.is_vreg() && !spilled_registers.contains(reg) {
                let degree = neighbors.len();
                if degree > max_degree {
                    max_degree = degree;
                    spill_candidate = Some(*reg);
                }
            }
        }

        spill_candidate.unwrap_or_else(|| panic!("No spill candidate found"))
    }
}
