use rustc_hash::FxHashSet;

use crate::{
    collections::linked_list::LinkedListContainerPtr,
    ir::{
        passman::{GlobalPassMut, PassResult, TransformPass},
        Context,
        InstKind,
        Symbol,
        SymbolKind,
    },
};

pub const GLOBAL_DCE: &str = "global-dce";

pub struct GlobalDce;

impl GlobalPassMut for GlobalDce {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        use InstKind as Ik;

        let mut func_uses = FxHashSet::default();
        let mut slot_uses = FxHashSet::default();

        for func in ctx.funcs() {
            for block in func.iter(ctx) {
                for inst in block.iter(ctx) {
                    if let Ik::Call(sym) = inst.kind(ctx) {
                        if let Some(SymbolKind::FuncDef(callee)) = ctx.lookup_symbol(sym) {
                            if func != *callee {
                                // ignore recursive calls
                                func_uses.insert(*callee);
                            }
                        }
                    } else if let Ik::GetGlobal(sym) = inst.kind(ctx) {
                        match ctx.lookup_symbol(sym) {
                            Some(SymbolKind::FuncDef(callee)) => {
                                if func != *callee {
                                    func_uses.insert(*callee);
                                }
                            }
                            Some(SymbolKind::GlobalSlot(slot)) => {
                                slot_uses.insert(*slot);
                            }
                            _ => {}
                        }
                    }
                }
            }
        }

        let main_func = ctx.lookup_func(&Symbol::from("main")).unwrap();
        func_uses.insert(main_func);

        let mut funcs_to_remove = Vec::new();
        let mut slots_to_remove = Vec::new();

        for func in ctx.funcs() {
            if !func_uses.contains(&func) {
                funcs_to_remove.push(func);
            }
        }

        for slot in ctx.global_slots() {
            if !slot_uses.contains(&slot) {
                slots_to_remove.push(slot);
            }
        }

        for func in funcs_to_remove {
            func.remove(ctx);
        }

        for slot in slots_to_remove {
            slot.remove(ctx);
        }

        PassResult::Ok(((), false)) // only run once.
    }
}

impl TransformPass for GlobalDce {
    fn register(passman: &mut crate::ir::passman::PassManager)
    where
        Self: Sized,
    {
        passman.register_transform(GLOBAL_DCE, GlobalDce, Vec::new());
    }
}
