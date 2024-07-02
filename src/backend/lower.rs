use std::collections::HashMap;

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
    collections::{apint::ApInt, linked_list::LinkedListContainerPtr},
    ir::{self},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MemLoc {
    /// The memory location is a register + offset.
    RegOffset { base: Reg, offset: i64 },
    /// The memory location is a stack slot.
    ///
    /// Usually, the offset is based on frame pointer, after generating the
    /// prologue, this offset should be editted with an additional offset.
    Slot { offset: i64 },
}

/// A lowered machine value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum MValue {
    /// The IR value is lowered to a register.
    Reg {
        /// The type of the ir value.
        ty: ir::Ty,
        /// The register.
        reg: Reg,
    },
    /// The IR value is lowered to a memory location.
    ///
    /// Memory location must be pointer type.
    Mem {
        /// The memory location.
        loc: MemLoc,
    },
    /// The IR value is lowered into a immediate
    Imm {
        /// The type of the immediate.
        ty: ir::Ty,
        /// The immediate value.
        imm: i64,
    },
    /// The IR value is undefined.
    Undef,
}

impl MValue {
    pub fn new_reg(ty: ir::Ty, reg: impl Into<Reg>) -> Self {
        Self::Reg {
            ty,
            reg: reg.into(),
        }
    }

    pub fn new_mem(loc: MemLoc) -> Self { Self::Mem { loc } }

    pub fn new_imm(ty: ir::Ty, imm: i64) -> Self { Self::Imm { ty, imm } }
}

pub struct LowerContext<'a, S>
where
    S: LowerSpec,
{
    mctx: MContext<S::I>,

    ctx: &'a ir::Context,

    /// The mapping from IR value to lowered machine value.
    lowered: HashMap<ir::Value, MValue>,
    /// Functions in the machine code.
    ///
    /// Because we want to get the machine function by the symbol when
    /// generating call instruction, so we need to map the IR symbol to mfunc.
    funcs: HashMap<ir::Symbol, MFunc<S::I>>,
    /// Mapping IR block to machine block.
    blocks: HashMap<ir::Block, MBlock<S::I>>,
    /// Other global labels, for IR global slots
    ///
    /// This is usually not necessary, because we directly use the IR symbol as
    /// the machine label. However, this two are different types, so it's
    /// better to map them.
    labels: HashMap<ir::Symbol, MLabel>,

    /// The current function and block.
    curr_func: Option<MFunc<S::I>>,
    /// The current block.
    curr_block: Option<MBlock<S::I>>,

    label_counter: u32,
}

pub trait LowerSpec: Sized {
    type I: MInst;

    fn stack_align() -> u32;

    fn frame_pointer_reg() -> PReg;

    fn stack_pointer_reg() -> PReg;

    fn pointer_size() -> usize;

    fn sig_def_use(sig: &ir::Signature) -> (Vec<PReg>, Vec<PReg>);

    fn gen_move(lower: &mut LowerContext<Self>, dst: Reg, src: Reg) -> Self::I;

    fn gen_iconst(lower: &mut LowerContext<Self>, x: &ApInt) -> MValue;

    fn gen_fconst(lower: &mut LowerContext<Self>, x: &ir::FloatConstant) -> MValue;

    fn gen_stack_slot(lower: &mut LowerContext<Self>, size: u32) -> MemLoc;

    fn gen_ibinary(
        lower: &mut LowerContext<Self>,
        op: ir::IBinaryOp,
        lhs: MValue,
        rhs: MValue,
    ) -> MValue;

    fn gen_fbinary(
        lower: &mut LowerContext<Self>,
        op: ir::FBinaryOp,
        lhs: MValue,
        rhs: MValue,
    ) -> MValue;

    fn gen_iunary(lower: &mut LowerContext<Self>, op: ir::IUnaryOp, operand: MValue) -> MValue;

    fn gen_funary(lower: &mut LowerContext<Self>, op: ir::FUnaryOp, operand: MValue) -> MValue;

    fn gen_cast(lower: &mut LowerContext<Self>, ty: ir::Ty, val: MValue) -> MValue;

    fn gen_offset(lower: &mut LowerContext<Self>, base: MValue, offset: MValue) -> MValue;

    fn gen_jump(lower: &mut LowerContext<Self>, dst: MBlock<Self::I>);

    fn gen_br(
        lower: &mut LowerContext<Self>,
        cond: MValue,
        then_block: MBlock<Self::I>,
        else_block: MBlock<Self::I>,
    );

    fn gen_call(lower: &mut LowerContext<Self>, func: MFunc<Self::I>, args: Vec<MValue>);

    fn gen_call_indirect(
        lower: &mut LowerContext<Self>,
        sig: ir::Signature,
        func_ptr: MValue,
        args: Vec<MValue>,
    );

    fn gen_load(lower: &mut LowerContext<Self>, ty: ir::Ty, ptr: MemLoc) -> MValue;

    fn gen_store(lower: &mut LowerContext<Self>, val: MValue, ptr: MemLoc);

    fn gen_ret(lower: &mut LowerContext<Self>);

    fn gen_get_global(lower: &mut LowerContext<Self>, label: MLabel) -> MValue;
}

impl<'a, S> LowerContext<'a, S>
where
    S: LowerSpec,
{
    pub fn new(ctx: &'a ir::Context) -> Self {
        Self {
            mctx: MContext::new(),
            ctx,
            lowered: HashMap::new(),
            funcs: HashMap::new(),
            blocks: HashMap::new(),
            labels: HashMap::new(),
            curr_func: None,
            curr_block: None,
            label_counter: 0,
        }
    }

    pub fn finish(self) -> MContext<S::I> { self.mctx }

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
                    block.name(self.ctx).cloned().unwrap_or_else(|| {
                        let label = format!("__machbb_{}", self.label_counter);
                        self.label_counter += 1;
                        label
                    }),
                );
                mfunc.push_back(&mut self.mctx, mblock);
                self.blocks.insert(block, mblock);
                // create registers for parameters
                for param in block.params(self.ctx) {
                    let ty = param.ty(self.ctx);
                    let reg = if ty.is_integer(self.ctx) || ty.is_ptr(self.ctx) {
                        self.mctx.new_vreg(RegKind::General)
                    } else if ty.is_float(self.ctx) {
                        self.mctx.new_vreg(RegKind::Float)
                    } else {
                        // TODO: to support struct-like types, we need to create multiple registers
                        unimplemented!("unsupported parameter type: {}", ty.display(self.ctx))
                    };
                    let mvalue = MValue::new_reg(ty, reg);
                    self.lowered.insert(*param, mvalue);
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
            let ty = global.ty(self.ctx);

            match global.init(self.ctx).kind() {
                ir::ConstantKind::Zeroinit | ir::ConstantKind::Undef => {
                    let data = RawData::Bss(ty.bytewidth_with_ptr(self.ctx, S::pointer_size()));
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
            todo!()
        }
    }

    fn lower_inst(&mut self, inst: ir::Inst) {}
}
