//! Legalize the IR for Lowering.
//!
//! This is a pass specialized for higher level instructions, like `store_elem`
//! and `load_elem`.

use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        Context,
        Func,
        IBinaryOp,
        Inst,
        InstKind,
    },
    utils::def_use::{Usable, User},
};

pub const LEGALIZE: &str = "legalize";

pub struct Legalize;

impl LocalPassMut for Legalize {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut cursor = func.cursor();
        while let Some(block) = cursor.next(ctx) {
            let mut curr_inst = block.head(ctx);

            while let Some(inst) = curr_inst {
                curr_inst = inst.next(ctx);

                if let InstKind::StoreElem { shape } = inst.kind(ctx) {
                    let val = inst.operand(ctx, 0);
                    let addr = inst.operand(ctx, 1);

                    let indices_vec = inst.operands(ctx);
                    let shape = shape.clone();

                    let indices = &indices_vec.as_slice()[2..];

                    let idx_ty = indices[0].ty(ctx);
                    let bytewidth = val.ty(ctx).bytewidth(ctx);

                    let mut total_size =
                        shape.iter().map(|dim| *dim as usize).product::<usize>() * bytewidth;

                    let mut new_insts = Vec::new();

                    let zero = Inst::iconst(ctx, 0, idx_ty);
                    new_insts.push(zero);

                    let mut offset = zero.result(ctx, 0);

                    let first_idx = indices[0];

                    let size = Inst::iconst(ctx, total_size, idx_ty);
                    let mul = Inst::ibinary(ctx, IBinaryOp::Mul, first_idx, size.result(ctx, 0));
                    let add = Inst::ibinary(ctx, IBinaryOp::Add, offset, mul.result(ctx, 0));

                    offset = add.result(ctx, 0);

                    new_insts.push(size);
                    new_insts.push(mul);
                    new_insts.push(add);

                    for (dim, idx) in shape.iter().zip(indices.iter().skip(1)) {
                        total_size /= *dim as usize;
                        let size = Inst::iconst(ctx, total_size, idx_ty);
                        let mul = Inst::ibinary(ctx, IBinaryOp::Mul, *idx, size.result(ctx, 0));
                        let add = Inst::ibinary(ctx, IBinaryOp::Add, offset, mul.result(ctx, 0));

                        offset = add.result(ctx, 0);

                        new_insts.push(size);
                        new_insts.push(mul);
                        new_insts.push(add);
                    }

                    let addr = Inst::offset(ctx, addr, offset);
                    let store = Inst::store(ctx, val, addr.result(ctx, 0));

                    new_insts.push(addr);
                    new_insts.push(store);

                    inst.extend_after(ctx, new_insts);

                    inst.remove(ctx);
                } else if let InstKind::LoadElem { shape } = inst.kind(ctx) {
                    let val = inst.result(ctx, 0);
                    let addr = inst.operand(ctx, 0);

                    let indices_vec = inst.operands(ctx);
                    let shape = shape.clone();

                    let indices = &indices_vec.as_slice()[1..];

                    let idx_ty = indices[0].ty(ctx);
                    let bytewidth = val.ty(ctx).bytewidth(ctx);

                    let mut total_size =
                        shape.iter().map(|dim| *dim as usize).product::<usize>() * bytewidth;

                    let mut new_insts = Vec::new();

                    let zero = Inst::iconst(ctx, 0, idx_ty);
                    new_insts.push(zero);

                    let mut offset = zero.result(ctx, 0);

                    let first_idx = indices[0];

                    let size = Inst::iconst(ctx, total_size, idx_ty);
                    let mul = Inst::ibinary(ctx, IBinaryOp::Mul, first_idx, size.result(ctx, 0));
                    let add = Inst::ibinary(ctx, IBinaryOp::Add, offset, mul.result(ctx, 0));

                    offset = add.result(ctx, 0);

                    new_insts.push(size);
                    new_insts.push(mul);
                    new_insts.push(add);

                    for (dim, idx) in shape.iter().zip(indices.iter().skip(1)) {
                        total_size /= *dim as usize;
                        let size = Inst::iconst(ctx, total_size, idx_ty);
                        let mul = Inst::ibinary(ctx, IBinaryOp::Mul, *idx, size.result(ctx, 0));
                        let add = Inst::ibinary(ctx, IBinaryOp::Add, offset, mul.result(ctx, 0));

                        offset = add.result(ctx, 0);

                        new_insts.push(size);
                        new_insts.push(mul);
                        new_insts.push(add);
                    }

                    let addr = Inst::offset(ctx, addr, offset);
                    let load = Inst::load(ctx, addr.result(ctx, 0), val.ty(ctx));

                    new_insts.push(addr);
                    new_insts.push(load);

                    inst.extend_after(ctx, new_insts);

                    let result = load.result(ctx, 0);

                    for user in val.users(ctx) {
                        user.replace(ctx, val, result);
                    }

                    inst.remove(ctx);
                } else {
                    continue;
                }
            }
        }

        Ok(((), false))
    }
}

impl GlobalPassMut for Legalize {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;
        for func in ctx.funcs() {
            let (_, local_changed) = LocalPassMut::run(self, ctx, func)?;
            changed |= local_changed;
        }
        Ok(((), changed))
    }
}

impl TransformPass for Legalize {
    fn register(passman: &mut crate::ir::passman::PassManager)
    where
        Self: Sized,
    {
        passman.register_transform(LEGALIZE, Legalize, Vec::new());
    }
}
