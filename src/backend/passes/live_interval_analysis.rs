use std::{collections::{HashMap, HashSet}, fmt::Display};

use thiserror::Error;

use super::{
    block_defsue_analysis::DefUseAnalysis,
    control_flow_analysis::ControlFlowAnalysis,
    liveness_analysis::InOutAnalysis,
    LocalPass,
    PassError,
    PassResult,
};
use crate::backend::{
    MachineBlock,
    MachineContext,
    MachineFunctionData,
    MachineInst,
    MachineInstData,
    MachineSymbol,
    Register, RiscvGpReg,
};

/// Range [start, end)
#[derive(Debug, Clone, Default, Copy)]
pub struct Range {
    pub start: usize,
    pub end: usize,
}

impl Range {
    pub fn new(start: usize, end: usize) -> Self { Self { start, end } }
}

impl Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{:}, {:})", self.start, self.end)
    }
}

#[derive(Debug, Clone, Default)]
pub struct Interval {
    pub ranges: Vec<Range>,
}

impl Interval {
    pub fn new() -> Self { Self::default() }

    pub fn add_range(&mut self, range: Range) { self.ranges.push(range); }

    pub fn optimize(&mut self) {
        self.ranges.sort_by(|a, b| a.start.cmp(&b.start));
        let mut i = 0;
        while i < self.ranges.len() - 1 {
            if self.ranges[i].end >= self.ranges[i + 1].start {
                self.ranges[i].end = self.ranges[i + 1].end;
                self.ranges.remove(i + 1);
            } else {
                i += 1;
            }
        }
    }

    /// Check if two intervals intersect. if ordered is true, 
    /// we assume that the ranges are ordered by start. And an optimized
    /// version of the function is used.
    pub fn intersects(&self, other: &Interval, ordered: bool) -> bool {
        if ordered {
            let mut i = 0;
            let mut j = 0;
            while i < self.ranges.len() && j < other.ranges.len() {
                if self.ranges[i].end <= other.ranges[j].start {
                    i += 1;
                } else if other.ranges[j].end <= self.ranges[i].start {
                    j += 1;
                } else {
                    return true;
                }
            }
            false
        } else {
            for range in self.ranges.iter() {
                for other_range in other.ranges.iter() {
                    if range.start < other_range.end && range.end > other_range.start {
                        return true;
                    }
                }
            }
            false
        }
    }

    pub fn range_count(&self) -> usize { self.ranges.len() }
}

#[derive(Debug, Clone, Default)]
pub struct LiveRange {
    pub intervals: HashMap<Register, Interval>,
}

#[derive(Debug, Clone, Default)]
pub struct LiveRangeAnalysis {
    pub instruction_number: HashMap<MachineInst, usize>
}

impl LocalPass for LiveRangeAnalysis {
    type Ok = LiveRange;

    fn run_on_function(
        &mut self,
        ctx: &MachineContext,
        data: &MachineFunctionData,
    ) -> PassResult<Self::Ok> {
        let mut ioa = InOutAnalysis {};
        let in_out = ioa.run_on_function(ctx, data)?;

        let mut live_interval = LiveRange::default();
        
        self.instruction_number = HashMap::new();

        // number all the instructions
        let mut inst_number = 0;
        for (block, _) in data.layout().blocks() {
            for (inst, _) in data.layout().insts_of_block(block).unwrap() {
                self.instruction_number.insert(inst, inst_number);
                inst_number += 1;
            }
            inst_number += 4;
        }

        // calculate live intervals
        for (block, _) in data.layout().blocks() {
            let mut current_range: HashMap<Register, Range> = HashMap::new();
            let mut last_use: HashMap<Register, MachineInst> = HashMap::new();
            let live_in = in_out.in_(&block).unwrap().clone();
            let live_out = in_out.out(&block).unwrap().clone();
            let block_first_instruction = data.layout().entry_inst_of_block(block);
            // skip empty block
            if block_first_instruction.is_none() {
                continue;
            }
            let block_first_instruction = block_first_instruction.unwrap();
            let block_last_instruction = data.layout().exit_inst_of_block(block).unwrap();

            for (inst, _) in data.layout().insts_of_block(block).unwrap() {
                let inst_data = ctx.inst_data(inst).unwrap();
                for reg in inst_data.get_use_operands() {
                    // except the zero register
                    if reg == Register::General(RiscvGpReg::Zero) {
                        continue;
                    }
                    let range = current_range.entry(reg).or_insert_with(|| {
                        Range::new(
                            *self.instruction_number.get(&block_first_instruction).unwrap(),
                            self.instruction_number.get(&inst).unwrap() + 1,
                        )
                    });
                    range.end = self.instruction_number.get(&inst).unwrap() + 1;

                    last_use.insert(reg, inst);
                }

                for reg in inst_data.get_def_operands() {
                    if current_range.contains_key(&reg) {
                        if current_range.get(&reg).unwrap().end
                            == self.instruction_number.get(&inst).unwrap() + 1
                        {
                            current_range.get_mut(&reg).unwrap().end =
                            self.instruction_number.get(&inst).unwrap() + 2;
                        } else {
                            live_interval
                                .intervals
                                .entry(reg)
                                .or_insert_with(Interval::new)
                                .add_range(*current_range.get(&reg).unwrap());
                            current_range.insert(
                                reg,
                                Range::new(
                                    self.instruction_number.get(&inst).unwrap() + 1,
                                    self.instruction_number.get(&inst).unwrap() + 2,
                                ),
                            );
                            last_use.remove(&reg);
                        }
                    } else {
                        let range = Range::new(
                            self.instruction_number.get(&inst).unwrap() + 1,
                            self.instruction_number.get(&inst).unwrap() + 2,
                        );
                        current_range.insert(reg, range);
                    }
                }
            }
            for (reg, range) in current_range.iter_mut() {
                if live_out.contains(reg) {
                    range.end = self.instruction_number.get(&block_last_instruction).unwrap() + 2;
                }
                live_interval
                    .intervals
                    .entry(*reg)
                    .or_insert_with(Interval::new)
                    .add_range(*range);
            }

            for reg in live_out.iter() {
                if live_in.contains(reg) && !current_range.contains_key(reg) {
                    let range = Range::new(
                        *self.instruction_number.get(&block_first_instruction).unwrap(),
                        self.instruction_number.get(&block_last_instruction).unwrap() + 2,
                    );
                    current_range.insert(*reg, range);
                    
                }
            }
        }

        live_interval.intervals.iter_mut().for_each(|(_, interval)| {
            interval.optimize();
        });

        Ok(live_interval)
    }
}

impl LiveRangeAnalysis {
    pub fn new() -> Self { Self { instruction_number: HashMap::new() } }
    
    pub fn dump(&self, ctx: &MachineContext, data: &MachineFunctionData, live_interval: &LiveRange) {
        //  asm    | instruction number | r1 r2 r3 ... |
        //  xxxxxx | 0                  | |  |         |
        //  xxxxxx | 1                  | |  |  |  ... |
        //  xxxxxx | 2                  |    |  |  ... |
        // Vector to store the formatted lines of the disassembled instructions.
        let mut lines = Vec::new();
        
        // Collect all lines with their instruction number.
        for (block, _) in data.layout().blocks() {
            lines.push(format!("bb_{:}:", block.0));
            for (inst, _) in data.layout().insts_of_block(block).unwrap() {
                let inst_data = ctx.inst_data(inst).unwrap();
                let line = format!("    {:}", inst_data);
                lines.push(line);
            }
        }

        // Calculate the padding to align columns.
        let asm_max_len = lines.iter().map(|line| line.len()).max().unwrap() + 4;
        for line in lines.iter_mut() {
            let padding = asm_max_len - line.len();
            line.push_str(&" ".repeat(padding));
        }

        // instruction numbers
        let mut inst_numbers = Vec::new();
        for (block, _) in data.layout().blocks() {
            inst_numbers.push(String::new());
            for (inst, _) in data.layout().insts_of_block(block).unwrap() {
                inst_numbers.push(format!("{:}", self.instruction_number.get(&inst).unwrap()));
            }
        }

        // Calculate the padding to align columns.
        let max_len = inst_numbers.iter().map(|line| line.len()).max().unwrap() + 4;
        for line in inst_numbers.iter_mut() {
            let padding = max_len - line.len();
            line.push_str(&" ".repeat(padding));
        }

        // register live ranges
        let mut header_line = "".to_string();
        let mut x_pos = Vec::new();
        for (reg, _) in live_interval.intervals.iter() {
            x_pos.push(header_line.chars().count());
            header_line.push_str(&format!("{:} ", reg));
        }

        let mut live_ranges = Vec::new();
        for (block, _) in data.layout().blocks() {
            live_ranges.push(" ".repeat(header_line.len()));
            for (inst, _) in data.layout().insts_of_block(block).unwrap() {
                let mut line = " ".repeat(header_line.len());
                for (idx, (reg, _)) in live_interval.intervals.iter().enumerate() {
                    let range = live_interval.intervals.get(reg).unwrap();
                    let inst_number = self.instruction_number.get(&inst).unwrap();
                    for interval in range.ranges.iter() {
                        if interval.start <= *inst_number && *inst_number < interval.end {
                            line.replace_range(x_pos[idx]..x_pos[idx] + 1, "|");
                        }
                    }
                }
                live_ranges.push(line.clone());
            }
        }

        // Print the formatted lines.
        print!("{:}", " ".repeat(max_len + asm_max_len + 1));
        println!("{:}", header_line);
        for ((asm, inst_number), live_range) in lines.iter().zip(inst_numbers.iter()).zip(live_ranges.iter()) {
            println!("{:} {:} {:}", inst_number, asm, live_range);
        }

    }
}

mod test {
    use std::io::Cursor;

    use super::InOutAnalysis;
    use crate::{
        backend::{
            passes::{block_defsue_analysis::DefUseAnalysis, live_interval_analysis::LiveRangeAnalysis, LocalPass},
            MachineSymbol,
        },
        codegen::CodegenContext,
        ir::frontend::parser::Parser,
    };

    #[test]
    fn test_inout() {
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
        let mut module = parser.parse().unwrap().into_ir("test".into()).unwrap();

        let mut codegen_ctx = CodegenContext::new();
        codegen_ctx.codegen(&module);

        println!("{:}", codegen_ctx.machine_ctx);

        let mut lra = LiveRangeAnalysis::default();

        let func = codegen_ctx
            .machine_ctx
            .functions
            .get(&MachineSymbol("param32_rec".to_string()))
            .unwrap();

        let live_ranges = lra
            .run_on_function(&codegen_ctx.machine_ctx, func)
            .unwrap();

        // lra.dump(&codegen_ctx.machine_ctx, func, &live_ranges);

        for (reg, interval) in live_ranges.intervals.iter() {
            println!("{:}:", reg);
            for range in interval.ranges.iter() {
                println!("    [{:}, {:})", range.start, range.end);
            }
        }

    }
}

