# Passes

## CFG Canonicalization

CFG Canonicalization 的 Pass 位于 `src/ir/passes` 下。主要的目的是在没有跳转指令的基本块中插入跳转指令，以便于后续的 SSA 化。

## mem2reg

mem2reg 的 Pass 位于 `src/ir/passes` 下。

## Todo List

### IR

- [x] CFG Canonicalization
- [x] mem2reg
  - [ ] Block Argument Reduction `sysy/hidden_functional/29_long_line`
    - [x] Dead Argument Elimination
    - [ ] Other Reduction
- [ ] Dead Code Elimination (maybe aggressive DCE)
  - [x] Simple DCE
  - [ ] Aggressive DCE
- [ ] Constant Folding & Propagation
  - [x] Integer
  - [ ] Floating Point
- [ ] CFG Simplification
- [ ] Function Inlining
- [ ] Global Value Numbering
- [ ] Global Code Motion
  - [x] Loop Invariant Code Motion
  - [ ] Sink
- [ ] Global2local
- [ ] Strength Reduction
  - [x] Math Optimization (instcombine)
  - [ ] Multiplication Optimization
  - [ ] Division Optimization
- [ ] Loop Unrolling
- [ ] Scalar Evolution Analysis
- [ ] Tail Call Optimization
- [ ] Alias Analysis (basicaa)

可能还需要一个更有效的测试方式。

### Backend

- [ ] Register Allocation
  - [x] Better Block Argument Passing
  - [ ] Tune the spill weight
  - [ ] RISC-V `ra` register for allocation.
  - [ ] Constraint & Hint
  - [ ] Coalescing & Splitting
- [ ] Scheduling
  - [ ] For dual-issue
  - [ ] For register pressure
- [ ] Peephole Optimization
  - [ ] `shNadd`
  - [ ] `Rol` 
  - [ ] `Mulh`
  - [ ] remove redundant direct jump