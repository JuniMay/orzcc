use std::{fmt::Display, hash::Hash};

use rustc_hash::FxHashMap;

use super::liveness_analysis;
use crate::{
    backend::{inst::MInst, regs::Reg, LowerContext, LowerSpec, MFunc},
    collections::linked_list::LinkedListContainerPtr,
};

/// Left-closed right-open range
/// [start, end)
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

/// A set of ranges
#[derive(Debug, Clone, Default)]
pub struct Interval {
    pub ranges: Vec<Range>,
}

impl Interval {
    pub fn new() -> Self { Self::default() }

    /// Add a range to the interval.
    pub fn add_range(&mut self, range: Range) { self.ranges.push(range); }

    /// Optimize the ranges by merging overlapping ranges.
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

    /// Check if two intervals intersect. If ordered is true,
    /// we assume that the ranges are sorted by start point, so an optimized
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

    /// Get the number of ranges in the interval.
    pub fn range_count(&self) -> usize { self.ranges.len() }
}

impl Display for Interval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;
        for range in self.ranges.iter() {
            write!(f, "{}, ", range)?;
        }
        write!(f, "]")
    }
}

#[derive(Debug, Clone, Default)]
pub struct LiveInterval {
    pub intervals: FxHashMap<Reg, Interval>,
}

impl LiveInterval {
    pub fn display<S>(&self) -> String
    where
        S: LowerSpec,
    {
        let mut s = String::new();

        for (reg, interval) in &self.intervals {
            s.push_str(&format!("{}: {}\n", S::display_reg(*reg), interval));
        }

        s
    }

    pub fn pretty_display<S>(&self, ctx: &LowerContext<S>, func: MFunc<S::I>) -> String
    where
        S: LowerSpec,
    {
        // instruction number | r1 r2 r3 ... |
        // 0                  | |  |         |
        // 1                  | |  |  |  ... |
        // 2                  |    |  |  ... |

        let mut s = String::new();

        // Vector to store the formatted lines of the disassembled instructions.
        let mut lines = Vec::new();

        // instruction numbers
        let mut inst_numbers = Vec::new();
        let mut inst_number = 0;

        for block in func.iter(ctx.mctx()) {
            inst_numbers.push(String::new());
            for _inst in block.iter(ctx.mctx()) {
                inst_numbers.push(format!("{:}", inst_number));
                inst_number += 1;
            }
            inst_number += 4;
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
        for reg in self.intervals.keys() {
            x_pos.push(header_line.chars().count());
            header_line.push_str(&format!("{:} ", S::display_reg(*reg)));
        }

        inst_number = 0;
        let mut live_ranges = Vec::new();
        for block in func.iter(ctx.mctx()) {
            live_ranges.push(" ".repeat(header_line.len()));
            for _inst in block.iter(ctx.mctx()) {
                let mut line = " ".repeat(header_line.len());
                for (idx, (reg, _)) in self.intervals.iter().enumerate() {
                    let range = self.intervals.get(reg).unwrap();
                    for interval in range.ranges.iter() {
                        if interval.start <= inst_number && inst_number < interval.end {
                            line.replace_range(x_pos[idx]..x_pos[idx] + 1, "|");
                        }
                    }
                }
                live_ranges.push(line.clone());
                inst_number += 1;
            }
            inst_number += 4;
        }

        // Combine the instruction numbers and the register live ranges.
        for (inst_number_line, live_range_line) in inst_numbers.iter().zip(live_ranges.iter()) {
            lines.push(format!("{:}{}", inst_number_line, live_range_line));
        }

        // Combine the header and the lines.
        s.push_str(&format!("{:}{}\n", " ".repeat(max_len), header_line));
        for line in lines.iter() {
            s.push_str(&format!("{}\n", line));
        }

        s
    }
}

fn is_allocatable<S>(reg: Reg) -> bool
where
    S: LowerSpec,
{
    match reg {
        Reg::P(reg) => S::allocatable_regs().contains(&reg),
        Reg::V(_) => true,
    }
}

pub fn analyze_on_function<S>(ctx: &LowerContext<S>, func: MFunc<S::I>) -> LiveInterval
where
    S: LowerSpec,
    S::I: Hash,
{
    let mut instruction_number = FxHashMap::default();
    let in_out = liveness_analysis::analyze_on_function(ctx, func);
    let mut live_interval = LiveInterval::default();

    // number all the instructions
    let mut inst_number = 0;
    for block in func.iter(ctx.mctx()) {
        for inst in block.iter(ctx.mctx()) {
            instruction_number.insert(inst, inst_number);
            inst_number += 1;
        }
        inst_number += 4; // leave some gaps between blocks
    }

    // calculate live intervals
    for block in func.iter(ctx.mctx()) {
        let mut current_range = FxHashMap::default(); // current extending range
        let mut last_use = FxHashMap::default(); // last use of the register
        let live_in = in_out.in_(&block).unwrap();
        let live_out = in_out.out(&block).unwrap();
        let block_first_inst = block.head(ctx.mctx());
        // skip empty blocks
        if block_first_inst.is_none() {
            continue;
        }
        let block_first_inst = block_first_inst.unwrap();
        let block_last_inst = block.tail(ctx.mctx()).unwrap();

        // iterate over all instructions in the block
        for inst in block.iter(ctx.mctx()) {
            // address use registers first
            for reg in inst.uses(ctx.mctx(), &ctx.config) {
                // only calculate live intervals for allocatable registers and virtual registers
                if is_allocatable::<S>(reg) {
                    // if the register is not met before, create a new range from the beginning of
                    // the block
                    let range = current_range.entry(reg).or_insert_with(|| {
                        Range::new(
                            instruction_number[&block_first_inst],
                            instruction_number[&inst] + 1,
                        )
                    });
                    // extend the range to the current instruction
                    range.end = instruction_number[&inst] + 1;
                }

                // update the last use of the register
                last_use.insert(reg, inst);
            }

            // address clobber registers
            // for call instructions, all caller-saved registers are clobbered (may be
            // edited) so we should make all caller-saved registers conflict
            // with all live registers we can do this by mark all caller-saved
            // registers live at [call + 1, call + 2) so here we consider all
            // caller-saved registers 'defined' at the call instruction
            // since they are not used later, they will only live at [call + 1, call + 2)
            for reg in inst.clobbers(ctx.mctx(), &ctx.config) {
                // only calculate live intervals for allocatable registers and virtual registers
                if is_allocatable::<S>(reg) {
                    // if the register is not met before, create a new range from this instruction +
                    // 1 (since we can only use the register after this
                    // instruction)
                    if !current_range.contains_key(&reg) {
                        current_range.insert(
                            reg,
                            Range::new(
                                instruction_number[&inst] + 1,
                                instruction_number[&inst] + 2,
                            ),
                        );
                    } else {
                        // if the register is met before
                        if current_range[&reg].end == instruction_number[&inst] + 1 {
                            // if the range is continuous, extend the range
                            current_range.get_mut(&reg).unwrap().end =
                                instruction_number[&inst] + 2;
                        } else {
                            // if the range is not continuous
                            // first insert the last range into the live interval
                            live_interval
                                .intervals
                                .entry(reg)
                                .or_insert_with(Interval::new)
                                .add_range(current_range[&reg]);
                            // then create a new range from this instruction + 1
                            current_range.insert(
                                reg,
                                Range::new(
                                    instruction_number[&inst] + 1,
                                    instruction_number[&inst] + 2,
                                ),
                            );
                            // remove the last use of the register
                            last_use.remove(&reg);
                        }
                    }
                }
            }
        }

        // push all the remaining ranges into the live interval
        for (reg, range) in current_range.iter_mut() {
            // if the register is in live_out set, extend the range to the end of the block
            if live_out.contains(reg) {
                range.end = instruction_number[&block_last_inst] + 2; // + 2 marks the end of the block
            }
            live_interval
                .intervals
                .entry(*reg)
                .or_insert_with(Interval::new)
                .add_range(*range);
        }

        // for those registers that are in both live_in and live_out, but not met in the
        // block create a range from the beginning of the block to the end of
        // the block
        for reg in live_out
            .iter()
            .filter(|reg| live_in.contains(reg) && !current_range.contains_key(reg))
        {
            // only calculate live intervals for allocatable registers and virtual registers
            if is_allocatable::<S>(*reg) {
                live_interval
                    .intervals
                    .entry(*reg)
                    .or_insert_with(Interval::new)
                    .add_range(Range::new(
                        instruction_number[&block_first_inst],
                        instruction_number[&block_last_inst] + 2,
                    ));
            }
        }
    }

    // optimize the live intervals
    live_interval
        .intervals
        .values_mut()
        .for_each(|interval| interval.optimize());

    live_interval
}
