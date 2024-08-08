use std::hash::Hash;

use rustc_hash::FxHashMap;

use super::{
    func::MLabel,
    inst::MInst,
    regs::Reg,
    MBlock,
    MContext,
    MFunc,
    PReg,
    RawData,
    RegKind,
};
use crate::{
    backend::reg_alloc::graph_coloring_allocation::GraphColoringAllocation,
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{self, CastOp, Successor},
    utils::cfg::CfgRegion,
};

/// A memory location
///
/// # Stack Layout
///
/// The stack layout of a function is as follows:
///
/// ```text
///              param by stack #n
///                     ...          -----> maybe depends on the calling convention
///              param by stack #0
///       high +-------------------+ <-- frame pointer
///            |  Saved Registers  |
///        |   +- - - - - - - - - -+
///        |   | (maybe alignment) |
///        |   +- - - - - - - - - -+ <-- start of local slots
///        |   |                   |
///  grow  |   |  Local Variables  |
///        |   |                   |
///        V   +- - - - - - - - - -+
///            |  for arg passing  |
///       low  +-------------------+ <-- stack pointer
///                     ...
/// ```
///
/// We can index local variables/slots with frame pointer or stack pointer. The
/// benefit of using stack pointer is that we can allocate fp as a general
/// purpose register. But either way, we need to modify the offset of the slots
/// after register allocation.
///
/// Actually, the space for arg passing can be dynamically allocated, but it
/// will generate more add/sub instructions. So we can reserve a fixed space
/// (with maximum required space) for arg passing at the prologue.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemLoc {
    /// The memory location is a register + offset.
    RegOffset { base: Reg, offset: i64 },
    /// The memory location is a stack slot.
    ///
    /// Usually, the offset is based on frame pointer, after generating all
    /// instruction and finishing register allocation, this kind of location
    /// should be converted to [MemLoc::RegOffset], with an explicit base
    /// register, e.g., frame pointer.
    ///
    /// The offset represents the relative offset to the start of local slots,
    /// and all the slots should be modified after all the instructions are
    /// generated, and the registers are allocated.
    ///
    /// A scratch register should be reserved if the target needs a place to
    /// hold offset. e.g., for RISC-V, if the immediate cannot fit in the
    /// offset field of load/store, a scratch register will be needed to load
    /// the immediate first.
    ///
    /// TODO: if we index local slots with stack pointer, and place the offset
    /// after the saved registers we can determine the offset from sp before
    /// register allocation, and the scratch register is no longer needed.
    Slot { offset: i64 },
    /// An incoming parameter.
    ///
    /// The offset is relative to the frame pointer, i.e., the start of the
    /// saved registers.
    Incoming { offset: i64 },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MValueKind {
    Mem(MemLoc),
    /// Keep a register for all immediate values. Used for better codegen
    /// quality.
    ///
    /// Another solution is codegen in postorder (like Cranelift), but due to
    /// historical reasons, we use this solution.
    Imm(Reg, i64),
    Reg(Reg),
    Undef,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MValue {
    ty: ir::Ty,
    kind: MValueKind,
}

impl MValue {
    pub fn ty(&self) -> ir::Ty { self.ty }

    pub fn kind(&self) -> MValueKind { self.kind }

    pub fn is_undef(&self) -> bool { matches!(self.kind, MValueKind::Undef) }

    pub fn new_reg(ty: ir::Ty, reg: impl Into<Reg>) -> Self {
        Self {
            ty,
            kind: MValueKind::Reg(reg.into()),
        }
    }

    pub fn new_mem(ty: ir::Ty, mem: MemLoc) -> Self {
        Self {
            ty,
            kind: MValueKind::Mem(mem),
        }
    }

    pub fn new_imm(ty: ir::Ty, reg: impl Into<Reg>, imm: i64) -> Self {
        Self {
            ty,
            kind: MValueKind::Imm(reg.into(), imm),
        }
    }

    pub fn new_undef(ty: ir::Ty) -> Self {
        Self {
            ty,
            kind: MValueKind::Undef,
        }
    }
}

#[derive(Clone)]
pub struct LowerConfig {
    pub omit_frame_pointer: bool,
    pub combine_stack_adjustments: bool,
}

impl Default for LowerConfig {
    fn default() -> Self {
        Self {
            omit_frame_pointer: true,
            combine_stack_adjustments: true,
        }
    }
}

pub struct LowerContext<'a, S>
where
    S: LowerSpec,
{
    /// The machine context.
    pub(super) mctx: MContext<S::I>,

    /// The IR context.
    pub(super) ctx: &'a ir::Context,

    /// The mapping from IR value to lowered machine value.
    pub(super) lowered: FxHashMap<ir::Value, MValue>,
    /// Functions in the machine code.
    ///
    /// Because we want to get the machine function by the symbol when
    /// generating call instruction, so we need to map the IR symbol to mfunc.
    pub funcs: FxHashMap<ir::Symbol, MFunc<S::I>>,
    /// Mapping IR block to machine block.
    pub blocks: FxHashMap<ir::Block, MBlock<S::I>>,
    /// Other global labels, for IR global slots
    ///
    /// This is usually not necessary, because we directly use the IR symbol as
    /// the machine label. However, this two are different types, so it's
    /// better to map them.
    pub labels: FxHashMap<ir::Symbol, MLabel>,

    /// The current function and block.
    pub(super) curr_func: Option<MFunc<S::I>>,
    /// The current block.
    pub(super) curr_block: Option<MBlock<S::I>>,

    /// In case the name of blocks are not allocated in the IR.
    label_counter: u32,

    pub(super) config: LowerConfig,
}

pub trait LowerSpec: Sized {
    type I: MInst<S = Self>;

    /// Get the stack alignment of the target.
    ///
    /// # Returns
    ///
    /// The stack alignment in bytes.
    fn stack_align() -> u32;

    /// Get the frame pointer register.
    fn frame_pointer() -> PReg;

    /// Get the stack pointer register.
    fn stack_pointer() -> PReg;

    /// Get the size of a pointer.
    ///
    /// # Returns
    ///
    /// The size of a pointer in bytes.
    fn pointer_size() -> usize;

    /// Get the allocatable general purpose registers.
    fn allocatable_gp_regs() -> Vec<PReg>;

    /// Get the allocatable floating point registers.
    fn allocatable_fp_regs() -> Vec<PReg>;

    /// Get the allocatable registers.
    fn allocatable_regs() -> Vec<PReg> {
        let mut regs = Self::allocatable_gp_regs();
        regs.extend(Self::allocatable_fp_regs());
        regs
    }

    /// Get the non-allocatable registers.
    fn non_allocatable_regs() -> Vec<PReg>;

    /// Get callee-saved registers.
    fn callee_saved_regs() -> Vec<PReg>;

    /// Get caller-saved registers.
    fn caller_saved_regs() -> Vec<PReg>;

    /// Get the aligned size of the stack frame.
    fn total_stack_size(lower: &mut LowerContext<Self>, mfunc: MFunc<Self::I>) -> u64;

    /// Get the return register by the type.
    fn return_reg(ctx: &ir::Context, ty: ir::Ty) -> PReg;

    /// Generate a move instruction.
    ///
    /// The src can be all kinds of values, including register, memory,
    /// immediate, etc. The types of src and dst must be the same when calling
    /// this function.
    fn gen_move(lower: &mut LowerContext<Self>, dst: Reg, src: MValue);

    fn gen_sp_adjust(lower: &mut LowerContext<Self>, offset: i64);

    fn gen_iconst(lower: &mut LowerContext<Self>, x: ir::IntConstant, dst_ty: ir::Ty) -> MValue;

    fn gen_fconst(lower: &mut LowerContext<Self>, x: ir::FloatConstant, dst_ty: ir::Ty) -> MValue;

    fn gen_ibinary(
        lower: &mut LowerContext<Self>,
        op: ir::IBinaryOp,
        lhs: MValue,
        rhs: MValue,
        dst_ty: ir::Ty,
    ) -> MValue;

    fn gen_fbinary(
        lower: &mut LowerContext<Self>,
        op: ir::FBinaryOp,
        lhs: MValue,
        rhs: MValue,
        dst_ty: ir::Ty,
    ) -> MValue;

    fn gen_iunary(
        lower: &mut LowerContext<Self>,
        op: ir::IUnaryOp,
        operand: MValue,
        dst_ty: ir::Ty,
    ) -> MValue;

    fn gen_funary(
        lower: &mut LowerContext<Self>,
        op: ir::FUnaryOp,
        operand: MValue,
        dst_ty: ir::Ty,
    ) -> MValue;

    fn gen_cast(lower: &mut LowerContext<Self>, op: CastOp, val: MValue, dst_ty: ir::Ty) -> MValue;

    fn gen_offset(lower: &mut LowerContext<Self>, base: MValue, offset: MValue) -> MValue;

    fn gen_jump(lower: &mut LowerContext<Self>, dst: MBlock<Self::I>);

    fn gen_br(lower: &mut LowerContext<Self>, cond: MValue, dst: MBlock<Self::I>);

    fn gen_call(lower: &mut LowerContext<Self>, func: MFunc<Self::I>, arg_regs: Vec<PReg>);

    fn gen_call_indirect(
        lower: &mut LowerContext<Self>,
        sig: &ir::Signature,
        func_ptr: MValue,
        arg_regs: Vec<PReg>,
    );

    fn gen_load(lower: &mut LowerContext<Self>, ty: ir::Ty, mem_loc: MemLoc) -> MValue;

    fn gen_store(lower: &mut LowerContext<Self>, val: MValue, mem_loc: MemLoc);

    fn gen_ret(lower: &mut LowerContext<Self>);

    fn gen_get_global(lower: &mut LowerContext<Self>, label: MLabel, dst_ty: ir::Ty) -> MValue;

    fn gen_outgoing(lower: &mut LowerContext<Self>, args: Vec<MValue>) -> Vec<PReg>;

    fn gen_incoming(lower: &mut LowerContext<Self>, sig: &ir::Signature, dsts: Vec<ir::Value>);

    fn gen_func_prologue(lower: &mut LowerContext<Self>, func: MFunc<Self::I>);

    fn gen_func_epilogue(lower: &mut LowerContext<Self>, func: MFunc<Self::I>);

    fn gen_spill_load(lower: &mut LowerContext<Self>, reg: Reg, slot: MemLoc, inst: Self::I);

    fn gen_spill_store(lower: &mut LowerContext<Self>, reg: Reg, slot: MemLoc, inst: Self::I);

    fn display_reg(reg: Reg) -> String;
}

impl<'a, S> LowerContext<'a, S>
where
    S: LowerSpec,
    S::I: Hash,
{
    pub fn reg_alloc(&mut self) {
        let funcs: Vec<MFunc<_>> = self
            .funcs
            .values()
            .filter(|f| !f.is_external(self.mctx()))
            .copied()
            .collect();

        for func in funcs {
            println!(
                "[ reg_alloc ] Allocating function: {}",
                func.label(self.mctx())
            );
            let mut allocation = GraphColoringAllocation::new();
            allocation.run_on_function(self, func);
            println!(
                "[ reg_alloc ] Spilled {} regs, with {} loads and {} stores inserted",
                allocation.total_spills,
                allocation.total_loads_added,
                allocation.total_stores_added
            )
        }
    }
}

impl<'a, S> LowerContext<'a, S>
where
    S: LowerSpec,
{
    pub fn new(ctx: &'a ir::Context, config: LowerConfig) -> Self {
        Self {
            mctx: MContext::new(),
            ctx,
            lowered: FxHashMap::default(),
            funcs: FxHashMap::default(),
            blocks: FxHashMap::default(),
            labels: FxHashMap::default(),
            curr_func: None,
            curr_block: None,
            label_counter: 0,
            config,
        }
    }

    pub fn finish(self) -> MContext<S::I> { self.mctx }

    pub fn mctx(&self) -> &MContext<S::I> { &self.mctx }

    pub fn mctx_mut(&mut self) -> &mut MContext<S::I> { &mut self.mctx }

    pub fn lower(&mut self) {
        // firstly, create all functions, globals and blocks
        for func in self.ctx.funcs() {
            let symbol = func.name(self.ctx);
            let label = MLabel::from(symbol.to_string());

            let mfunc = MFunc::new(&mut self.mctx, label, func.sig(self.ctx).clone());
            self.funcs.insert(symbol.clone(), mfunc);

            for block in func.iter(self.ctx) {
                let mblock = MBlock::new(
                    &mut self.mctx,
                    format!(
                        ".{}",
                        block.name(self.ctx).cloned().unwrap_or_else(|| {
                            let label = format!("__machbb_{}", self.label_counter);
                            self.label_counter += 1;
                            label
                        })
                    ),
                );
                mfunc.push_back(&mut self.mctx, mblock);
                self.blocks.insert(block, mblock);

                if block != func.entry_node(self.ctx) {
                    // create registers for parameters, entry node will be created in `gen_incoming`
                    for param in block.params(self.ctx) {
                        let ty = param.ty(self.ctx);
                        let reg = if ty.is_integer(self.ctx) || ty.is_ptr(self.ctx) {
                            self.mctx.new_vreg(RegKind::General)
                        } else if ty.is_float(self.ctx) {
                            self.mctx.new_vreg(RegKind::Float)
                        } else {
                            // TODO: to support struct-like types, we need to create multiple
                            // registers
                            unimplemented!("unsupported parameter type: {}", ty.display(self.ctx))
                        };
                        let mvalue = MValue::new_reg(ty, reg);
                        self.lowered.insert(*param, mvalue);
                    }
                }
            }
        }

        for (symbol, sig) in self.ctx.decls() {
            let label = MLabel::from(symbol.to_string());
            let mfunc = MFunc::new_external(&mut self.mctx, label, sig);
            self.funcs.insert(symbol, mfunc);
        }

        for global in self.ctx.global_slots() {
            let symbol = global.name(self.ctx);
            let label = MLabel::from(symbol.to_string());
            let size = global.size(self.ctx);

            match global.init(self.ctx).kind() {
                ir::ConstantKind::Zeroinit | ir::ConstantKind::Undef => {
                    let data = RawData::Bss(size);
                    self.mctx.add_raw_data(label.clone(), data);
                }
                ir::ConstantKind::Bytes(bytes) => {
                    let data = RawData::Bytes(bytes.clone());
                    self.mctx.add_raw_data(label.clone(), data);
                }
            }

            self.labels.insert(symbol.clone(), label);
        }

        // then, lower all instructions
        for func in self.ctx.funcs() {
            self.curr_func = Some(self.funcs[func.name(self.ctx)]);

            let params = func.entry_node(self.ctx).params(self.ctx).to_vec();
            self.curr_block = Some(self.blocks[&func.entry_node(self.ctx)]);
            S::gen_incoming(self, func.sig(self.ctx), params);

            for block in func.iter(self.ctx) {
                self.curr_block = Some(self.blocks[&block]);
                for inst in block.iter(self.ctx) {
                    self.lower_inst(inst);
                }
            }
        }

        // TODO: we can actually allocate the local slots from the end of the
        // storage region, make the spilled slots under them (higher address)
        // so we don't need to rely on the spilled size to determine the offset.
        // In that way, we can allocate t0 after passing the arguments.
    }

    pub fn after_regalloc(&mut self) {
        for func in self.ctx.funcs() {
            let mfunc = self.funcs[func.name(self.ctx)];

            self.curr_func = Some(mfunc);

            self.curr_block = Some(mfunc.head(&self.mctx).unwrap());
            S::gen_func_prologue(self, mfunc);

            self.curr_block = Some(mfunc.tail(&self.mctx).unwrap());
            S::gen_func_epilogue(self, mfunc);
        }

        // prologue/epilogue might changed the saved regs or other things, so we
        // postpone the offset adjustment to here
        self.adjust_offset();
    }

    fn adjust_offset(&mut self) {
        for func in self.ctx.funcs() {
            let mfunc = self.funcs[func.name(self.ctx)];

            let storage_size = mfunc.storage_stack_size(&self.mctx) as i64;
            let outgoing_size = mfunc.outgoing_stack_size(&self.mctx) as i64;

            if !self.config.combine_stack_adjustments {
                assert_eq!(outgoing_size, 0);
            }

            let total_stack_size = S::total_stack_size(self, mfunc) as i64;

            let mut cursor = mfunc.cursor();

            while let Some(mblock) = cursor.next(self.mctx()) {
                let mut curr_inst = mblock.head(&self.mctx);

                while let Some(inst) = curr_inst {
                    // because the target might modify the instruction sequence
                    let next_inst = inst.next(&self.mctx);

                    if self.config.omit_frame_pointer {
                        inst.adjust_offset(
                            &mut self.mctx,
                            |mem_loc| match mem_loc {
                                MemLoc::Slot { offset } => Some(MemLoc::RegOffset {
                                    base: S::stack_pointer().into(),
                                    offset: storage_size + outgoing_size + offset,
                                }),
                                MemLoc::Incoming { offset } => Some(MemLoc::RegOffset {
                                    base: S::stack_pointer().into(),
                                    offset: total_stack_size + offset,
                                }),
                                MemLoc::RegOffset { .. } => None,
                            },
                            &self.config,
                        );
                    } else {
                        inst.adjust_offset(
                            &mut self.mctx,
                            |mem_loc| match mem_loc {
                                MemLoc::Slot { offset } => Some(MemLoc::RegOffset {
                                    base: S::frame_pointer().into(),
                                    offset: -(total_stack_size - storage_size - outgoing_size)
                                        + offset, // offset is negative, so add
                                }),
                                MemLoc::Incoming { offset } => Some(MemLoc::RegOffset {
                                    base: S::frame_pointer().into(),
                                    offset,
                                }),
                                MemLoc::RegOffset { .. } => None,
                            },
                            &self.config,
                        );
                    }
                    curr_inst = next_inst;
                }
            }
        }
    }

    fn lower_inst(&mut self, inst: ir::Inst) {
        use ir::InstKind as Ik;

        let mfunc = self.curr_func.unwrap();

        match inst.kind(self.ctx) {
            Ik::Undef => {
                let ty = inst.result(self.ctx, 0).ty(self.ctx);
                let mvalue = MValue::new_undef(ty);
                self.lowered.insert(inst.result(self.ctx, 0), mvalue);
            }
            Ik::IConst(int) => {
                let mval = S::gen_iconst(self, *int, inst.result(self.ctx, 0).ty(self.ctx));
                self.lowered.insert(inst.result(self.ctx, 0), mval);
            }
            Ik::FConst(f) => {
                let mval = S::gen_fconst(self, *f, inst.result(self.ctx, 0).ty(self.ctx));
                self.lowered.insert(inst.result(self.ctx, 0), mval);
            }
            Ik::StackSlot(size) => {
                let ty = inst.result(self.ctx, 0).ty(self.ctx);
                mfunc.add_storage_stack_size(&mut self.mctx, *size as u64);
                let mval = MValue::new_mem(
                    ty,
                    MemLoc::Slot {
                        // because the stack grows downward, we need to use negative offset
                        offset: -(mfunc.storage_stack_size(&self.mctx) as i64),
                    },
                );
                self.lowered.insert(inst.result(self.ctx, 0), mval);
            }
            Ik::IBinary(op) => {
                let lhs = inst.operand(self.ctx, 0);
                let rhs = inst.operand(self.ctx, 1);
                let mval = S::gen_ibinary(
                    self,
                    *op,
                    self.lowered[&lhs],
                    self.lowered[&rhs],
                    inst.result(self.ctx, 0).ty(self.ctx),
                );
                self.lowered.insert(inst.result(self.ctx, 0), mval);
            }
            Ik::FBinary(op) => {
                let lhs = inst.operand(self.ctx, 0);
                let rhs = inst.operand(self.ctx, 1);
                let mval = S::gen_fbinary(
                    self,
                    *op,
                    self.lowered[&lhs],
                    self.lowered[&rhs],
                    inst.result(self.ctx, 0).ty(self.ctx),
                );
                self.lowered.insert(inst.result(self.ctx, 0), mval);
            }
            Ik::IUnary(op) => {
                let operand = inst.operand(self.ctx, 0);
                let mval = S::gen_iunary(
                    self,
                    *op,
                    self.lowered[&operand],
                    inst.result(self.ctx, 0).ty(self.ctx),
                );
                self.lowered.insert(inst.result(self.ctx, 0), mval);
            }
            Ik::FUnary(op) => {
                let operand = inst.operand(self.ctx, 0);
                let mval = S::gen_funary(
                    self,
                    *op,
                    self.lowered[&operand],
                    inst.result(self.ctx, 0).ty(self.ctx),
                );
                self.lowered.insert(inst.result(self.ctx, 0), mval);
            }
            Ik::Cast(op) => {
                let operand = inst.operand(self.ctx, 0);
                let mval = S::gen_cast(
                    self,
                    *op,
                    self.lowered[&operand],
                    inst.result(self.ctx, 0).ty(self.ctx),
                );
                self.lowered.insert(inst.result(self.ctx, 0), mval);
            }
            Ik::Offset => {
                let base = inst.operand(self.ctx, 0);
                let offset = inst.operand(self.ctx, 1);
                let mval = S::gen_offset(self, self.lowered[&base], self.lowered[&offset]);
                self.lowered.insert(inst.result(self.ctx, 0), mval);
            }
            Ik::Load => {
                let ty = inst.result(self.ctx, 0).ty(self.ctx);
                let ptr = inst.operand(self.ctx, 0);
                let mem_loc = match self.lowered[&ptr].kind() {
                    MValueKind::Reg(reg) => MemLoc::RegOffset {
                        base: reg,
                        offset: 0,
                    },
                    MValueKind::Mem(loc) => loc,
                    MValueKind::Imm(..) => unreachable!(),
                    MValueKind::Undef => {
                        self.lowered
                            .insert(inst.result(self.ctx, 0), MValue::new_undef(ty));
                        return;
                    }
                };

                let mval = S::gen_load(self, ty, mem_loc);
                self.lowered.insert(inst.result(self.ctx, 0), mval);
            }
            Ik::Store => {
                let ptr = inst.operand(self.ctx, 1);
                let mem_loc = match self.lowered[&ptr].kind() {
                    MValueKind::Reg(reg) => MemLoc::RegOffset {
                        base: reg,
                        offset: 0,
                    },
                    MValueKind::Mem(loc) => loc,
                    MValueKind::Imm(..) => unreachable!(),
                    MValueKind::Undef => return,
                };

                let val = inst.operand(self.ctx, 0);
                S::gen_store(self, self.lowered[&val], mem_loc);
            }
            Ik::Jump => {
                let succ = inst.succ(self.ctx, 0);
                self.lower_succ(succ);
                let mblock = self.blocks[&succ.block()];
                S::gen_jump(self, mblock);
            }
            Ik::Br => {
                let cond = inst.operand(self.ctx, 0);
                let mcond = self.lowered[&cond];

                let then_succ = inst.succ(self.ctx, 0);
                let else_succ = inst.succ(self.ctx, 1);

                let mblock_then = self.blocks[&then_succ.block()];
                let mblock_else = self.blocks[&else_succ.block()];

                // TODO: we can do better if there are no lost-copy problem between the two
                // successors.
                if then_succ.args().is_empty() {
                    // if there are no arugments to pass, we can just br, without creating a new
                    // block because no lost-copy problem will happen
                    S::gen_br(self, mcond, mblock_then);

                    if else_succ.args().is_empty() {
                        // if there are no arguments to pass, we can just br, without creating a new
                        // block because no lost-copy problem will happen
                        S::gen_jump(self, mblock_else);
                    } else {
                        // create a new block to jump
                        let mblock_else_arg_passing = MBlock::new(
                            &mut self.mctx,
                            format!(".__machbb_{}", self.label_counter),
                        );
                        self.label_counter += 1;

                        S::gen_jump(self, mblock_else_arg_passing);

                        mblock_else.insert_before(self.mctx_mut(), mblock_else_arg_passing);

                        self.curr_block = Some(mblock_else_arg_passing);

                        self.lower_succ(else_succ);
                        S::gen_jump(self, mblock_else);
                    }
                } else {
                    let mblock_then_arg_passing =
                        MBlock::new(&mut self.mctx, format!(".__machbb_{}", self.label_counter));
                    self.label_counter += 1;

                    let this_mblock = self.curr_block.unwrap();
                    this_mblock.insert_after(self.mctx_mut(), mblock_then_arg_passing);

                    S::gen_br(self, mcond, mblock_then_arg_passing);

                    if else_succ.args().is_empty() {
                        // if there are no arguments to pass, we can just br, without creating a new
                        // block because no lost-copy problem will happen
                        S::gen_jump(self, mblock_else);
                    } else {
                        let mblock_else_arg_passing = MBlock::new(
                            &mut self.mctx,
                            format!(".__machbb_{}", self.label_counter),
                        );
                        self.label_counter += 1;

                        mblock_else.insert_before(self.mctx_mut(), mblock_else_arg_passing);

                        S::gen_jump(self, mblock_else_arg_passing);

                        self.curr_block = Some(mblock_else_arg_passing);
                        self.lower_succ(else_succ);
                        S::gen_jump(self, mblock_else);
                    }

                    self.curr_block = Some(mblock_then_arg_passing);
                    self.lower_succ(then_succ);

                    S::gen_jump(self, mblock_then);
                }
            }
            Ik::Call(symbol) => {
                let mfunc = self.funcs[symbol];

                let args = inst
                    .operands(self.ctx)
                    .into_iter()
                    .map(|v| self.lowered[&v])
                    .collect();

                let arg_regs = S::gen_outgoing(self, args);
                S::gen_call(self, mfunc, arg_regs);

                if !self.config.combine_stack_adjustments {
                    // restore sp
                    let outgoing_size = self
                        .curr_func
                        .unwrap()
                        .take_outgoing_stack_size(self.mctx_mut());

                    if outgoing_size != 0 {
                        // add the offset back
                        S::gen_sp_adjust(self, outgoing_size as i64);
                    }
                }

                if !inst.results(self.ctx).is_empty() {
                    if inst.results(self.ctx).len() == 1 {
                        let ty = inst.result(self.ctx, 0).ty(self.ctx);
                        let reg = S::return_reg(self.ctx, ty);
                        // we need to create a vreg, because we cannot know if the return register
                        // will be overwritten by the next instruction
                        let vreg = self.mctx.new_vreg(reg.kind());
                        S::gen_move(self, vreg.into(), MValue::new_reg(ty, reg));
                        self.lowered
                            .insert(inst.result(self.ctx, 0), MValue::new_reg(ty, vreg));
                    } else {
                        unimplemented!()
                    }
                }
            }
            Ik::CallIndirect(_sig) => {
                unimplemented!()
            }
            Ik::GetGlobal(symbol) => {
                let mlabel = match self.ctx.lookup_symbol(symbol).unwrap() {
                    ir::SymbolKind::FuncDef(_) | ir::SymbolKind::FuncDecl(_) => {
                        let mval = self.funcs[symbol];
                        mval.label(&self.mctx)
                    }
                    ir::SymbolKind::GlobalSlot(_) => self.labels.get(symbol).unwrap(),
                };
                let mval =
                    S::gen_get_global(self, mlabel.clone(), inst.result(self.ctx, 0).ty(self.ctx));
                self.lowered.insert(inst.result(self.ctx, 0), mval);
            }
            Ik::Ret => {
                if !inst.operands(self.ctx).is_empty() {
                    // TODO: multiple return values are not supported yet
                    let val = inst.operand(self.ctx, 0);
                    let ty = val.ty(self.ctx);
                    let mval = self.lowered[&val];
                    let ret_reg = S::return_reg(self.ctx, ty);
                    S::gen_move(self, ret_reg.into(), mval);
                }
                // `ret` should be generated in the epilogue
            }
            Ik::StoreElem { .. } | Ik::LoadElem { .. } => {
                unreachable!("do legalize first");
            }
        }
    }

    fn lower_succ(&mut self, succ: &Successor) {
        let args = succ.args();

        // 1. we can directly pass those immediate arguments
        // 2. for mem loc, we can directly pass those are not reg offset, because they
        //    either use sp or fp
        // 3. for reg offset and reg, we need to check if they will be overwritten in
        //    the passing.

        let mut buffer = FxHashMap::default();
        let mut tys = FxHashMap::default();
        let mut reg_outgoing = FxHashMap::default();

        for (param, arg) in args.iter() {
            let mval = self.lowered[&arg.inner()];

            if let MValueKind::Reg(reg) = self.lowered[param].kind() {
                match mval.kind() {
                    MValueKind::Imm(..) => {
                        S::gen_move(self, reg, mval);
                    }
                    MValueKind::Mem(loc) => match loc {
                        MemLoc::Incoming { .. } | MemLoc::Slot { .. } => {
                            S::gen_move(self, reg, mval);
                        }
                        MemLoc::RegOffset { base, .. } => {
                            // we can be sure that for each dst, only one src will be passed
                            buffer.insert(reg, mval);
                            tys.insert(reg, mval.ty());
                            // record the out-degree of the register
                            reg_outgoing
                                .entry(base)
                                .and_modify(|v| *v += 1)
                                .or_insert(1);
                            // also record the out-degree of the dst register
                            reg_outgoing.entry(reg).or_insert(0);
                        }
                    },
                    MValueKind::Reg(src) => {
                        if src != reg {
                            buffer.insert(reg, mval);
                            tys.insert(reg, mval.ty());
                            reg_outgoing.entry(src).and_modify(|v| *v += 1).or_insert(1);
                            reg_outgoing.entry(reg).or_insert(0);
                        } else {
                            // we don't need to move the value if the source and
                            // destination are the same
                        }
                    }
                    MValueKind::Undef => {
                        // we don't need to move the value if it's undefined
                    }
                }
            } else {
                unreachable!()
            }
        }

        // we can iteratively look for dst registers whose outgoing degree is 0,
        // and move the value to the dst register. This is a topological sort
        // actually. and if no such register is found, we can create a virtual
        // register to break the cycle.

        // record the vregs that are created to break the cycle, and the corresponding
        // dst registers
        let mut vreg_buffer = FxHashMap::default();

        while !buffer.is_empty() {
            let mut queue = Vec::new();

            for (reg, out) in reg_outgoing.iter() {
                if *out == 0 && buffer.contains_key(reg) {
                    // push those outgoing degree is 0 and are dst registers
                    queue.push(*reg);
                }
            }

            while let Some(reg) = queue.pop() {
                let mval = buffer.remove(&reg).unwrap();
                S::gen_move(self, reg, mval);

                let src_reg = match mval.kind() {
                    MValueKind::Reg(reg) => reg,
                    MValueKind::Mem(MemLoc::RegOffset { base, .. }) => base,
                    MValueKind::Mem(_) | MValueKind::Imm(..) | MValueKind::Undef => unreachable!(),
                };

                if let Some(out) = reg_outgoing.get_mut(&src_reg) {
                    *out -= 1;
                    if *out == 0 && buffer.contains_key(&src_reg) {
                        queue.push(src_reg);
                    }
                }
            }

            // if there are still registers in the buffer, we need to create a
            // virtual register to break the cycle
            if !buffer.is_empty() {
                // choose one
                let (reg, mval) = buffer.iter().next().unwrap();
                let (reg, mval) = (*reg, *mval);

                let vreg = self.mctx.new_vreg(reg.kind());
                // record the virtual register and the corresponding dst register
                vreg_buffer.insert(reg, vreg);
                // remove the dst register from the buffer, and re-map with vreg, also update
                // the out-degree
                buffer.insert(vreg.into(), mval);
                buffer.remove(&reg);
                reg_outgoing.insert(vreg.into(), 0);
            }
        }

        // now we need to move the value from the virtual register to the dst register
        for (dst, vreg) in vreg_buffer {
            S::gen_move(self, dst, MValue::new_reg(tys[&dst], vreg));
        }
    }
}
