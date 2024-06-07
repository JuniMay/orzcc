use std::collections::{HashMap, HashSet};

use super::{
    live_interval_analysis::{LiveRange, LiveRangeAnalysis},
    LocalPass,
    LocalPassMut,
    PassResult,
};
use crate::backend::{
    Immediate,
    LoadKind,
    MachineContext,
    MachineInstData,
    MachineSymbol,
    Register,
    RiscvGpReg,
    StoreKind,
    ALLOCATABLE_REGISTERS_GP,
};

#[derive(Debug, Clone)]
pub enum RegisterType {
    General,
    FloatingPoint,
}

#[derive(Debug, Clone)]
pub struct InterferenceGraph {
    pub graph: HashMap<Register, HashSet<Register>>,
    pub reg_type: RegisterType,
}

impl InterferenceGraph {
    pub fn new(reg_type: RegisterType) -> Self {
        Self {
            graph: HashMap::new(),
            reg_type,
        }
    }

    pub fn add_edge(&mut self, u: Register, v: Register) {
        self.graph.entry(u).or_default().insert(v);
        self.graph.entry(v).or_default().insert(u);
    }

    pub fn remove_edge(&mut self, u: Register, v: Register) {
        self.graph.entry(u).and_modify(|e| {
            e.remove(&v);
        });
        self.graph.entry(v).and_modify(|e| {
            e.remove(&u);
        });
    }

    pub fn adjacent(&self, u: Register) -> Option<&HashSet<Register>> { self.graph.get(&u) }

    pub fn degree(&self, u: Register) -> usize { self.graph.get(&u).map(|e| e.len()).unwrap_or(0) }

    pub fn remove_node(&mut self, u: Register) {
        if let Some(neighbors) = self.graph.remove(&u) {
            for v in neighbors {
                self.graph.entry(v).and_modify(|e| {
                    e.remove(&u);
                });
            }
        }
    }

    pub fn construct_from_live_ranges(&mut self, live_ranges: &LiveRange) {
        match self.reg_type {
            RegisterType::General => {
                for (reg, live_interval) in live_ranges.intervals.iter() {
                    if reg.is_fp() || reg.is_fp_virtual() {
                        continue;
                    }
                    self.graph.entry(*reg).or_default();
                    for (other_reg, other_live_interval) in live_ranges.intervals.iter() {
                        if other_reg.is_fp() || other_reg.is_fp_virtual() {
                            continue;
                        }
                        if reg != other_reg && live_interval.intersects(other_live_interval, true) {
                            self.add_edge(*reg, *other_reg);
                        }
                    }
                }
            }
            RegisterType::FloatingPoint => {
                for (reg, live_interval) in live_ranges.intervals.iter() {
                    if reg.is_gp() || reg.is_gp_virtual() {
                        continue;
                    }
                    self.graph.entry(*reg).or_default();
                    for (other_reg, other_live_interval) in live_ranges.intervals.iter() {
                        if other_reg.is_gp() || other_reg.is_gp_virtual() {
                            continue;
                        }
                        if reg != other_reg && live_interval.intersects(other_live_interval, true) {
                            self.add_edge(*reg, *other_reg);
                        }
                    }
                }
            }
        }
    }

    pub fn to_mermaid(&self) -> String {
        let mut s = String::new();
        s.push_str("graph LR;\n");
        for (u, neighbors) in self.graph.iter() {
            for v in neighbors {
                s.push_str(&format!("{} --- {};\n", u, v));
            }
        }
        s
    }

    /// Returns true if only precolored nodes are left in the graph
    pub fn all_colored(&self) -> bool {
        match self.reg_type {
            RegisterType::General => self.graph.iter().all(|(reg, _)| reg.is_gp()),
            RegisterType::FloatingPoint => self.graph.iter().all(|(reg, _)| reg.is_fp()),
        }
    }

    /// Retrun all the precolored nodes in the graph
    pub fn precolored(&self) -> HashSet<Register> {
        match self.reg_type {
            RegisterType::General => self
                .graph
                .iter()
                .filter(|(reg, _)| reg.is_gp())
                .map(|(reg, _)| *reg)
                .collect(),
            RegisterType::FloatingPoint => self
                .graph
                .iter()
                .filter(|(reg, _)| reg.is_fp())
                .map(|(reg, _)| *reg)
                .collect(),
        }
    }
}

#[derive(Default)]
pub struct GraphColoringAllocation {
    pub total_spills: usize,
    pub total_loads_added: usize,
    pub total_stores_added: usize,
}

impl LocalPassMut for GraphColoringAllocation {
    type Ok = HashMap<Register, Register>;

    fn run_on_function(
        &mut self,
        ctx: &mut MachineContext,
        func_name: &MachineSymbol,
    ) -> PassResult<(Self::Ok, bool)> {
        // Gp registers
        let mut colors;
        let mut changed = false;

        loop {
            colors = HashMap::new();

            let mut lra = LiveRangeAnalysis::default();
            let live_ranges = lra.run_on_function(ctx, ctx.function_data(func_name).unwrap())?;

            // print live ranges
            // for (reg, live_range) in live_ranges.intervals.iter() {
            //     println!("{:?}:", reg);
            //     for range in live_range.ranges.iter() {
            //         println!("\t{}", range);
            //     }
            // }

            let mut interference_graph = InterferenceGraph::new(RegisterType::General);
            interference_graph.construct_from_live_ranges(&live_ranges);

            for (reg, neighbors) in interference_graph.graph.iter() {
                println!(
                    "{} - [ {} ]",
                    reg,
                    neighbors
                        .iter()
                        .map(|r| r.to_string())
                        .collect::<Vec<_>>()
                        .join(", ")
                );
            }

            // println!("{:}", interference_graph.to_mermaid());

            let mut stack = Vec::new();

            let mut working_interference_graph = interference_graph.clone();

            // simplify
            while !working_interference_graph.all_colored() {
                if let Some(reg) = working_interference_graph
                    .graph
                    .iter()
                    .find(|(reg, neighbors)| {
                        reg.is_gp_virtual() && neighbors.len() < ALLOCATABLE_REGISTERS_GP.len()
                    })
                    .map(|(reg, _)| *reg)
                {
                    stack.push(reg);
                    working_interference_graph.remove_node(reg);
                } else {
                    let spill = self.choose_to_spill(&working_interference_graph, &live_ranges);
                    stack.push(spill);
                    working_interference_graph.remove_node(spill);
                }
            }

            // println!("{:?}", stack);

            let mut spills = HashSet::new();

            // select
            while let Some(reg) = stack.pop() {
                let mut ok_colors: Vec<_> = ALLOCATABLE_REGISTERS_GP.iter().rev().collect();
                for neighbor in interference_graph.adjacent(reg).unwrap_or(&HashSet::new()) {
                    if neighbor.is_gp() {
                        ok_colors.retain(|&c| c != neighbor);
                    } else if let Some(color) = colors.get(neighbor) {
                        ok_colors.retain(|&c| c != color);
                    }
                }
                if let Some(color) = ok_colors.pop() {
                    colors.insert(reg, *color);
                } else {
                    spills.insert(reg);
                }
            }

            // spill
            if spills.is_empty() {
                break;
            }

            let mut spill_slots = HashMap::new();
            for spill in spills.iter() {
                changed = true;
                // allocate on stack
                let stack_offset = ctx.function_data_mut(func_name).unwrap().stack_size;
                ctx.function_data_mut(func_name).unwrap().add_stack_size(8);
                let stack_slot = (
                    Register::General(RiscvGpReg::Sp),
                    Immediate(stack_offset as i128),
                );
                spill_slots.insert(spill, stack_slot);
                self.total_spills += 1;
            }

            println!("Spilling");
            for (reg, stack_slot) in spill_slots.iter() {
                println!("{} -> {}(sp)", reg, stack_slot.1);
            }

            let mut insert_later_loads = HashMap::new();
            let mut insert_later_stores = HashMap::new();
            for (block, _) in ctx.function_data(func_name).unwrap().layout.blocks() {
                let insts = ctx
                    .function_data(func_name)
                    .unwrap()
                    .layout
                    .insts_of_block(block)
                    .unwrap();
                for (inst, _) in insts.iter() {
                    let inst_data = ctx.inst_data(inst).unwrap();
                    let defs = inst_data.get_def_operands();
                    let uses = inst_data.get_use_operands();
                    for def in defs {
                        if spills.contains(&def) {
                            let stack_slot = spill_slots.get(&def).unwrap();
                            let load = (def, stack_slot.0, stack_slot.1);
                            insert_later_stores.insert(inst, load);
                        }
                    }
                    for use_ in uses {
                        if spills.contains(&use_) {
                            let stack_slot = spill_slots.get(&use_).unwrap();
                            let store = (use_, stack_slot.0, stack_slot.1);
                            insert_later_loads.insert(inst, store);
                        }
                    }
                }
            }

            // println!("loads added to {:?}", insert_later_loads);
            // println!("stores added to {:?}", insert_later_stores);

            for (inst, (def, base, offset)) in insert_later_loads.iter() {
                let load =
                    MachineInstData::build_load(ctx, LoadKind::DoubleWord, *def, *base, *offset);
                ctx.function_data_mut(func_name)
                    .unwrap()
                    .layout
                    .insert_inst_before(load, *inst)
                    .unwrap();
                self.total_loads_added += 1;
            }

            for (inst, (use_, base, offset)) in insert_later_stores.iter() {
                let store =
                    MachineInstData::build_store(ctx, StoreKind::DoubleWord, *use_, *base, *offset);
                ctx.function_data_mut(func_name)
                    .unwrap()
                    .layout
                    .insert_inst_after(store, *inst)
                    .unwrap();
                self.total_stores_added += 1;
            }

            println!("{}", ctx);
        }

        // rewrite
        let function = ctx.function_data(func_name).unwrap();
        let mut curr_block = function.layout.entry_block();
        while let Some(block) = curr_block {
            // for (inst, _) in insts.iter() {
            //     let inst_data = ctx.inst_data_mut(inst).unwrap();
            //     let defs = inst_data.get_def_operands();
            //     let uses = inst_data.get_use_operands();
            //     for def in defs {
            //         if let Some(color) = colors.get(&def) {
            //             inst_data.replace_operand(def, *color);
            //         }
            //     }
            //     for use_ in uses {
            //         if let Some(color) = colors.get(&use_) {
            //             inst_data.replace_operand(use_, *color);
            //         }
            //     }
            // }
            let mut inst_it = ctx
                .function_data(func_name)
                .unwrap()
                .layout
                .insts_of_block(block)
                .unwrap()
                .front();
            while let Some(inst) = inst_it {
                let inst_data = ctx.inst_data_mut(inst).unwrap();
                let defs = inst_data.get_def_operands();
                let uses = inst_data.get_use_operands();
                for def in defs {
                    if let Some(color) = colors.get(&def) {
                        inst_data.replace_operand(def, *color);
                    }
                }
                for use_ in uses {
                    if let Some(color) = colors.get(&use_) {
                        inst_data.replace_operand(use_, *color);
                    }
                }
                inst_it = ctx
                    .function_data(func_name)
                    .unwrap()
                    .layout
                    .insts_of_block(block)
                    .unwrap()
                    .node(inst)
                    .unwrap()
                    .next;
            }

            curr_block = ctx
                .function_data_mut(func_name)
                .unwrap()
                .layout
                .next_block(block);
        }

        print!("{}", ctx);

        Ok((colors, changed))
    }
}

impl GraphColoringAllocation {
    pub fn new() -> Self { Self::default() }

    pub fn choose_to_spill(
        &self,
        interference_graph: &InterferenceGraph,
        live_range: &LiveRange,
    ) -> Register {
        let mut spill = None;
        let mut max_degree = 0;
        for (reg, _) in interference_graph.graph.iter() {
            if !reg.is_gp_virtual() {
                continue;
            }
            if live_range.intervals.get(reg).unwrap().range_count() > 1 {
                continue;
            }
            if interference_graph.degree(*reg) > max_degree {
                max_degree = interference_graph.degree(*reg);
                spill = Some(*reg);
            }
        }
        spill.unwrap()
    }
}

#[cfg(test)]
mod test {
    use std::io::Cursor;

    use crate::{
        backend::{
            passes::{
                graph_coloring_allocation::{
                    GraphColoringAllocation,
                    InterferenceGraph,
                    RegisterType,
                },
                live_interval_analysis::LiveRangeAnalysis,
                LocalPass,
                LocalPassMut,
            },
            MachineSymbol,
        },
        codegen::CodegenContext,
        ir::frontend::parser::Parser,
    };
    #[test]
    fn test_interference_graph() {
        let ir = r#"
            func @example() -> i32 {
                ^entry:
                %a = add i32 0, i32 2
                %cmp = icmp.sle i32 %a, i32 0
                br i1 %cmp, ^block3, ^block2
            
                ^block2:
                %b = add i32 0, i32 3
                %use_b = add i32 %b, i32 0
                jump ^block4
            
                ^block3:
                %use_a_block3 = add i32 %a, i32 0
                jump ^block4
            
                ^block4:
                %use_a_block4 = add i32 %a, i32 0
                ret i32 %use_a_block4
            }
            "#;

        let mut buf = Cursor::new(ir);
        let mut parser = Parser::new(&mut buf);
        let module = parser.parse().unwrap().into_ir("test".into()).unwrap();

        let mut codegen_ctx = CodegenContext::new();
        codegen_ctx.codegen(&module);

        println!("{:}", codegen_ctx.machine_ctx);

        let func = codegen_ctx
            .machine_ctx
            .functions
            .get(&MachineSymbol("example".to_string()))
            .unwrap();

        let mut lra = LiveRangeAnalysis::default();
        let live_ranges = lra.run_on_function(&codegen_ctx.machine_ctx, func).unwrap();

        lra.dump(&codegen_ctx.machine_ctx, func, &live_ranges);

        let mut interference_graph = InterferenceGraph::new(RegisterType::General);
        interference_graph.construct_from_live_ranges(&live_ranges);

        println!("{:}", interference_graph.to_mermaid());
    }

    #[test]
    fn test_graph_coloring_allocation() {
        let ir = r#"
            func @example() -> i32 {
                ^entry:
                %a = add i32 0, i32 2
                %cmp = icmp.sle i32 %a, i32 0
                br i1 %cmp, ^block3, ^block2
            
                ^block2:
                %b = add i32 0, i32 3
                %use_b = add i32 %b, i32 0
                jump ^block4
            
                ^block3:
                %use_a_block3 = add i32 %a, i32 0
                jump ^block4
            
                ^block4:
                %use_a_block4 = add i32 %a, i32 0
                ret i32 %use_a_block4
            }
            "#;

        let mut buf = Cursor::new(ir);
        let mut parser = Parser::new(&mut buf);
        let module = parser.parse().unwrap().into_ir("test".into()).unwrap();

        let mut codegen_ctx = CodegenContext::new();
        codegen_ctx.codegen(&module);

        println!("{:}", codegen_ctx.machine_ctx);

        let func = codegen_ctx
            .machine_ctx
            .functions
            .get(&MachineSymbol("example".to_string()))
            .unwrap();

        let mut lra = LiveRangeAnalysis::default();
        let live_ranges = lra.run_on_function(&codegen_ctx.machine_ctx, func).unwrap();

        lra.dump(&codegen_ctx.machine_ctx, func, &live_ranges);

        let mut graph_coloring_allocation = GraphColoringAllocation::new();

        let colors = graph_coloring_allocation
            .run_on_function(
                &mut codegen_ctx.machine_ctx,
                &MachineSymbol("example".to_string()),
            )
            .unwrap();

        for (reg, color) in colors.0.iter() {
            println!("{} -> {}", reg, color);
        }
    }

    #[test]
    fn test_graph_coloring_allocation_1() {
        let ir = r#"
        func @param32_rec(i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32, i32) -> i32 {
            ^0(i32 %__ARG_a1, i32 %__ARG_a2, i32 %__ARG_a3, i32 %__ARG_a4, i32 %__ARG_a5, i32 %__ARG_a6, i32 %__ARG_a7, i32 %__ARG_a8, i32 %__ARG_a9, i32 %__ARG_a10, i32 %__ARG_a11, i32 %__ARG_a12, i32 %__ARG_a13, i32 %__ARG_a14, i32 %__ARG_a15, i32 %__ARG_a16, i32 %__ARG_a17, i32 %__ARG_a18, i32 %__ARG_a19, i32 %__ARG_a20, i32 %__ARG_a21, i32 %__ARG_a22, i32 %__ARG_a23, i32 %__ARG_a24, i32 %__ARG_a25, i32 %__ARG_a26, i32 %__ARG_a27, i32 %__ARG_a28, i32 %__ARG_a29, i32 %__ARG_a30, i32 %__ARG_a31, i32 %__ARG_a32):
                %__SLOT_ARG_a32 = alloc i32
                %__SLOT_ARG_a31 = alloc i32
                %__SLOT_ARG_a30 = alloc i32
                %__SLOT_ARG_a29 = alloc i32
                %__SLOT_ARG_a28 = alloc i32
                %__SLOT_ARG_a27 = alloc i32
                %__SLOT_ARG_a26 = alloc i32
                %__SLOT_ARG_a25 = alloc i32
                %__SLOT_ARG_a24 = alloc i32
                %__SLOT_ARG_a23 = alloc i32
                %__SLOT_ARG_a22 = alloc i32
                %__SLOT_ARG_a21 = alloc i32
                %__SLOT_ARG_a20 = alloc i32
                %__SLOT_ARG_a19 = alloc i32
                %__SLOT_ARG_a18 = alloc i32
                %__SLOT_ARG_a17 = alloc i32
                %__SLOT_ARG_a16 = alloc i32
                %__SLOT_ARG_a15 = alloc i32
                %__SLOT_ARG_a14 = alloc i32
                %__SLOT_ARG_a13 = alloc i32
                %__SLOT_ARG_a12 = alloc i32
                %__SLOT_ARG_a11 = alloc i32
                %__SLOT_ARG_a10 = alloc i32
                %__SLOT_ARG_a9 = alloc i32
                %__SLOT_ARG_a8 = alloc i32
                %__SLOT_ARG_a7 = alloc i32
                %__SLOT_ARG_a6 = alloc i32
                %__SLOT_ARG_a5 = alloc i32
                %__SLOT_ARG_a4 = alloc i32
                %__SLOT_ARG_a3 = alloc i32
                %__SLOT_ARG_a2 = alloc i32
                %__SLOT_ARG_a1 = alloc i32
                store i32 %__ARG_a1, ptr %__SLOT_ARG_a1
                store i32 %__ARG_a2, ptr %__SLOT_ARG_a2
                store i32 %__ARG_a3, ptr %__SLOT_ARG_a3
                store i32 %__ARG_a4, ptr %__SLOT_ARG_a4
                store i32 %__ARG_a5, ptr %__SLOT_ARG_a5
                store i32 %__ARG_a6, ptr %__SLOT_ARG_a6
                store i32 %__ARG_a7, ptr %__SLOT_ARG_a7
                store i32 %__ARG_a8, ptr %__SLOT_ARG_a8
                store i32 %__ARG_a9, ptr %__SLOT_ARG_a9
                store i32 %__ARG_a10, ptr %__SLOT_ARG_a10
                store i32 %__ARG_a11, ptr %__SLOT_ARG_a11
                store i32 %__ARG_a12, ptr %__SLOT_ARG_a12
                store i32 %__ARG_a13, ptr %__SLOT_ARG_a13
                store i32 %__ARG_a14, ptr %__SLOT_ARG_a14
                store i32 %__ARG_a15, ptr %__SLOT_ARG_a15
                store i32 %__ARG_a16, ptr %__SLOT_ARG_a16
                store i32 %__ARG_a17, ptr %__SLOT_ARG_a17
                store i32 %__ARG_a18, ptr %__SLOT_ARG_a18
                store i32 %__ARG_a19, ptr %__SLOT_ARG_a19
                store i32 %__ARG_a20, ptr %__SLOT_ARG_a20
                store i32 %__ARG_a21, ptr %__SLOT_ARG_a21
                store i32 %__ARG_a22, ptr %__SLOT_ARG_a22
                store i32 %__ARG_a23, ptr %__SLOT_ARG_a23
                store i32 %__ARG_a24, ptr %__SLOT_ARG_a24
                store i32 %__ARG_a25, ptr %__SLOT_ARG_a25
                store i32 %__ARG_a26, ptr %__SLOT_ARG_a26
                store i32 %__ARG_a27, ptr %__SLOT_ARG_a27
                store i32 %__ARG_a28, ptr %__SLOT_ARG_a28
                store i32 %__ARG_a29, ptr %__SLOT_ARG_a29
                store i32 %__ARG_a30, ptr %__SLOT_ARG_a30
                store i32 %__ARG_a31, ptr %__SLOT_ARG_a31
                store i32 %__ARG_a32, ptr %__SLOT_ARG_a32
                %__RET_SLOT = alloc i32
            
            ^if:
                %0 = load i32, ptr %__SLOT_ARG_a1
                %1 = icmp.eq i32 %0, i32 0x00000000
                br i1 %1, ^then, ^else
            
            ^then:
                %2 = load i32, ptr %__SLOT_ARG_a2
                store i32 %2, ptr %__RET_SLOT
                jump ^1()
            
            ^else:
                %3 = load i32, ptr %__SLOT_ARG_a1
                %4 = sub i32 %3, i32 0x00000001
                %5 = load i32, ptr %__SLOT_ARG_a2
                %6 = load i32, ptr %__SLOT_ARG_a3
                %7 = add i32 %5, i32 %6
                %8 = srem i32 %7, i32 0x3b800001
                %9 = load i32, ptr %__SLOT_ARG_a4
                %10 = load i32, ptr %__SLOT_ARG_a5
                %11 = load i32, ptr %__SLOT_ARG_a6
                %12 = load i32, ptr %__SLOT_ARG_a7
                %13 = load i32, ptr %__SLOT_ARG_a8
                %14 = load i32, ptr %__SLOT_ARG_a9
                %15 = load i32, ptr %__SLOT_ARG_a10
                %16 = load i32, ptr %__SLOT_ARG_a11
                %17 = load i32, ptr %__SLOT_ARG_a12
                %18 = load i32, ptr %__SLOT_ARG_a13
                %19 = load i32, ptr %__SLOT_ARG_a14
                %20 = load i32, ptr %__SLOT_ARG_a15
                %21 = load i32, ptr %__SLOT_ARG_a16
                %22 = load i32, ptr %__SLOT_ARG_a17
                %23 = load i32, ptr %__SLOT_ARG_a18
                %24 = load i32, ptr %__SLOT_ARG_a19
                %25 = load i32, ptr %__SLOT_ARG_a20
                %26 = load i32, ptr %__SLOT_ARG_a21
                %27 = load i32, ptr %__SLOT_ARG_a22
                %28 = load i32, ptr %__SLOT_ARG_a23
                %29 = load i32, ptr %__SLOT_ARG_a24
                %30 = load i32, ptr %__SLOT_ARG_a25
                %31 = load i32, ptr %__SLOT_ARG_a26
                %32 = load i32, ptr %__SLOT_ARG_a27
                %33 = load i32, ptr %__SLOT_ARG_a28
                %34 = load i32, ptr %__SLOT_ARG_a29
                %35 = load i32, ptr %__SLOT_ARG_a30
                %36 = load i32, ptr %__SLOT_ARG_a31
                %37 = load i32, ptr %__SLOT_ARG_a32
                %38 = call i32 @param32_rec(i32 %4, i32 %8, i32 %9, i32 %10, i32 %11, i32 %12, i32 %13, i32 %14, i32 %15, i32 %16, i32 %17, i32 %18, i32 %19, i32 %20, i32 %21, i32 %22, i32 %23, i32 %24, i32 %25, i32 %26, i32 %27, i32 %28, i32 %29, i32 %30, i32 %31, i32 %32, i32 %33, i32 %34, i32 %35, i32 %36, i32 %37, i32 0x00000000)
                store i32 %38, ptr %__RET_SLOT
                jump ^1()
            
            ^exit:
            
            ^1:
                %39 = load i32, ptr %__RET_SLOT
                ret i32 %39
            
            }
            "#;

        let mut buf = Cursor::new(ir);
        let mut parser = Parser::new(&mut buf);
        let _module = parser.parse().unwrap().into_ir("whileIf".into()).unwrap();

        let mut buf = Cursor::new(ir);
        let mut parser = Parser::new(&mut buf);
        let module = parser.parse().unwrap().into_ir("test".into()).unwrap();

        let mut codegen_ctx = CodegenContext::new();
        codegen_ctx.codegen(&module);

        println!("{:}", codegen_ctx.machine_ctx);

        let func = codegen_ctx
            .machine_ctx
            .functions
            .get(&MachineSymbol("param32_rec".to_string()))
            .unwrap();

        let mut lra = LiveRangeAnalysis::default();

        let _live_ranges = lra.run_on_function(&codegen_ctx.machine_ctx, func).unwrap();
        // lra.dump(&codegen_ctx.machine_ctx, func, &live_ranges);

        let mut graph_coloring_allocation = GraphColoringAllocation::new();

        let colors = graph_coloring_allocation
            .run_on_function(
                &mut codegen_ctx.machine_ctx,
                &MachineSymbol("param32_rec".to_string()),
            )
            .unwrap();

        for (reg, color) in colors.0.iter() {
            println!("{:?} -> {:?}", reg, color);
        }

        println!("{:}", codegen_ctx.machine_ctx);
        println!("Total spills: {}", graph_coloring_allocation.total_spills);
        println!(
            "Total loads added: {}",
            graph_coloring_allocation.total_loads_added
        );
        println!(
            "Total stores added: {}",
            graph_coloring_allocation.total_stores_added
        );
    }
}
