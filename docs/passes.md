# Passes

## CFG Canonicalization

CFG Canonicalization 的 Pass 位于 `src/ir/passes` 下。主要的目的是在没有跳转指令的基本块中插入跳转指令，以便于后续的 SSA 化。

## mem2reg

mem2reg 的 Pass 位于 `src/ir/passes` 下。

## Todo List

- [x] CFG Canonicalization
- [x] mem2reg
- [ ] CFG Simplification
- [ ] Alias Analysis
- [ ] Constant Folding & Propagation
- [ ] Function Inlining
- [ ] Global Value Numbering
- [ ] Dead Code Elimination (maybe aggressive DCE)
- [ ] Loop Invariant Code Motion
- [ ] Loop Unrolling
- [ ] Scalar Evolution Analysis
- [ ] Strength Reduction
  - [ ] Math Optimization
  - [ ] Multiplication Optimization
  - [ ] Division Optimization
- [ ] Tail Call Optimization
