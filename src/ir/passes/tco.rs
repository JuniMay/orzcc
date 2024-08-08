use super::control_flow::CfgSimplify;
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassManager, PassResult, TransformPass},
        Block,
        Context,
        Func,
        Inst,
        InstKind,
    },
    utils::cfg::CfgRegion,
};

pub const TCO: &str = "tco";

pub struct Tco;

impl Tco {
    pub fn is_tail_call(ctx: &Context, func: Func, inst: Inst) -> bool {
        match inst.kind(ctx) {
            InstKind::Call(sym) => {
                Self::is_tail_call(ctx, func, inst.next(ctx).unwrap())
                    && sym == func.name(ctx)
                    && func.entry_node(ctx).params(ctx).len() < 16
            }
            InstKind::Jump => {
                let target = inst.succ(ctx, 0).block().head(ctx).unwrap();
                Self::is_tail_call(ctx, func, target)
            }
            InstKind::Br => {
                let inst_then = inst.succ(ctx, 0).block().head(ctx).unwrap();
                let inst_else = inst.succ(ctx, 1).block().head(ctx).unwrap();

                Self::is_tail_call(ctx, func, inst_then) && Self::is_tail_call(ctx, func, inst_else)
            }
            InstKind::Ret => true,
            InstKind::Undef
            | InstKind::IConst(_)
            | InstKind::FConst(_)
            | InstKind::StackSlot(_)
            | InstKind::IBinary(_)
            | InstKind::FBinary(_)
            | InstKind::IUnary(_)
            | InstKind::FUnary(_)
            | InstKind::Cast(_)
            | InstKind::Offset
            | InstKind::CallIndirect(_)
            | InstKind::GetGlobal(_)
            | InstKind::Load
            | InstKind::Store
            | InstKind::StoreElem { .. }
            | InstKind::LoadElem { .. } => false,
        }
    }
}

impl LocalPassMut for Tco {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut tail_calls = Vec::new();
        for block in func.iter(ctx) {
            for inst in block.iter(ctx) {
                if let InstKind::Call(sym) = inst.kind(ctx) {
                    if sym == func.name(ctx) && Self::is_tail_call(ctx, func, inst) {
                        tail_calls.push(inst);
                    }
                }
            }
        }

        if tail_calls.is_empty() {
            return Ok(((), false));
        }

        // create a new entry block.
        let new_entry = Block::new(ctx);
        let old_entry = func.entry_node(ctx);
        let mut new_params = Vec::new();

        #[allow(clippy::unnecessary_to_owned)]
        for param in old_entry.params(ctx).to_vec() {
            let ty = param.ty(ctx);
            let new_param = new_entry.new_param(ctx, ty);
            new_params.push(new_param);
        }

        old_entry.insert_before(ctx, new_entry);
        let jump = Inst::jump(ctx, old_entry, new_params);
        new_entry.push_back(ctx, jump);

        // replace the tail calls with jumps to the old entry.
        for inst in tail_calls {
            let args = inst.operands(ctx);
            let jump = Inst::jump(ctx, old_entry, args);
            inst.insert_before(ctx, jump);
        }

        Ok(((), true))
    }
}

impl GlobalPassMut for Tco {
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

impl TransformPass for Tco {
    fn register(passman: &mut PassManager) {
        passman.register_transform(TCO, Tco, vec![Box::new(CfgSimplify)]);
    }
}
