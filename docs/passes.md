# Passes

## CFG Canonicalization

CFG Canonicalization 的 Pass 位于 `src/ir/passes` 下。主要的目的是在没有跳转指令的基本块中插入跳转指令，以便于后续的 SSA 化。

## mem2reg

mem2reg 的 Pass 位于 `src/ir/passes` 下。

## Todo List

### IR

- [x] CFG Canonicalization
- [x] mem2reg
- [ ] Alias Analysis (basicaa)
- [ ] Dead Code Elimination (maybe aggressive DCE)
- [ ] Constant Folding & Propagation
- [ ] CFG Simplification
- [ ] Global2local
- [ ] Function Inlining
- [ ] Global Value Numbering
- [ ] Strength Reduction
  - [ ] Math Optimization
  - [ ] Multiplication Optimization
  - [ ] Division Optimization
- [ ] Loop Invariant Code Motion
- [ ] Loop Unrolling
- [ ] Scalar Evolution Analysis
- [ ] Tail Call Optimization

### Backend

- [ ] Register Allocation
  - [ ] Better Block Argument Passing (at most one vreg should be enough)
  - [ ] Tune the spill weight
  - [ ] RISC-V `ra` register for allocation.
- [ ] Scheduling (for dual-issue)
- [ ] Peephole Optimization
