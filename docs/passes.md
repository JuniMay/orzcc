# Passes

## CFG Canonicalization

CFG Canonicalization 的 Pass 位于 `src/ir/passes` 下。主要的目的是在没有跳转指令的基本块中插入跳转指令，以便于后续的 SSA 化。

## mem2reg

mem2reg 的 Pass 位于 `src/ir/passes` 下。

## Todo List

### IR

- [x] CFG Canonicalization
- [x] mem2reg
  - [x] Block Argument Reduction `sysy/hidden_functional/29_long_line`
    - [x] Dead Argument Elimination
    - [x] Other Reduction
- [x] Dead Code Elimination (maybe aggressive DCE)
  - [x] Simple DCE
  - [x] Aggressive DCE
- [ ] Constant Folding & Propagation
  - [x] Integer
  - [ ] Floating Point
- [ ] CFG Simplification
  - [ ] Branch Condition Expr Simplification
  - [ ] Branch Merge
- [x] Function Inlining
  - [x] Simple Inlining
  - [x] Recursive Inlining
  - [x] ~~Mutually Recursive Inlining~~
- [x] Global Value Numbering
  - [x] Simple GVN
  - [x] Load Elimination
    - [x] Simple Load Elimination
    - [ ] Load Elimination with Function Call
  - [ ] Store Elimination
  - [x] GVN with PRE (Done by GCM)
  - [ ] Load PRE Elimination
  - [x] Pure Func Call Elimination
- [x] Global Code Motion
  - [x] Loop Invariant Code Motion
  - [ ] Sink
- [x] Global2local
- [ ] Strength Reduction
  - [x] Math Optimization (instcombine)
  - [x] Multiplication Optimization
  - [x] Division Optimization
  - [ ] DivMod To MulShift
- [ ] Loop Unrolling
  - [x] Constant Unrolling
  - [ ] Dynamic Unrolling
- [x] Scalar Evolution Analysis
- [ ] Tail Call Optimization
- [x] Alias Analysis (basicaa)
- [ ] Function Analysis
  - [x] Pure Function
  - [ ] Function Clobber
  - [ ] Function Use
- [ ] Reassociation `gameoflife`
- [ ] Tree Height Reduction `gameoflife`
- [ ] Manual Memset
- [ ] Memset Elimination
- [ ] Memset Combination
- [ ] Instruction Scheduler
- [ ] Load/Store combine (i32 + i32 -> i64)
- [ ] Store + Loop -> Memset
- [ ] Hand-crafted Operators
- [ ] Integer Range Analysis

可能还需要一个更有效的测试方式。

### Backend

- [ ] Register Allocation
  - [x] Better Block Argument Passing
  - [x] Tune the spill weight
  - [ ] RISC-V `ra` register for allocation.
  - [ ] Constraint & Hint
  - [x] Coalescing
    - [ ] Further Coalescing
  - [ ] Splitting
- [ ] Scheduling
  - [ ] For dual-issue
  - [ ] For register pressure
- [ ] Peephole Optimization
  - [x] `shNadd`
  - [ ] `Rol`
  - [ ] `Mulh`
  - [x] remove redundant direct jump
