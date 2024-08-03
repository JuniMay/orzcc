use std::{collections::HashMap, vec};

use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        global::Symbol,
        passman::{GlobalPassMut, PassManager, PassResult, TransformPass},
        ConstantKind,
        Context,
        Func,
        GlobalSlot,
        Inst,
        InstKind,
        Ty,
    },
    utils::{
        cfg::CfgRegion,
        def_use::{Usable, User},
    },
};

pub const GLOBAL2LOCAL: &str = "global2local";

#[derive(Default, Debug)]
pub struct Global2Local {
    /// Slot symbols to its users.
    ///
    /// The user instruction should be `get_global`.
    slot_users: HashMap<Symbol, Vec<Inst>>,
    slot_tys: HashMap<Symbol, Ty>,
}

impl Global2Local {
    fn analyze_usage(&mut self, ctx: &Context) {
        self.slot_users.clear();

        // only collect the global slots used in main function
        let func_main = ctx.lookup_func(&Symbol::from("main")).unwrap();
        for block in func_main.iter(ctx) {
            for inst in block.iter(ctx) {
                if !inst.is_used(ctx) {
                    // if the instruction is not used, just ignore.
                    continue;
                }

                if let InstKind::GetGlobal(symbol) = inst.kind(ctx) {
                    // decide the type of the global slot, if the type cannot be decided, just
                    // ignore it

                    self.slot_users
                        .entry(symbol.clone())
                        .or_default()
                        // there should be no duplicate instructions
                        .push(inst);

                    if self.slot_tys.contains_key(symbol) {
                        // already analyzed the type, no need to analyze again
                        continue;
                    }

                    let ptr = inst.result(ctx, 0);
                    let mut ty = None;
                    for user in ptr.users(ctx) {
                        if let InstKind::Offset = user.kind(ctx) {
                            // it is used as a pointer, ignore it
                            ty = None;
                            break;
                        }

                        if let InstKind::Load = user.kind(ctx) {
                            // look at the first load to decide the type of the global slot
                            ty = Some(user.result(ctx, 0).ty(ctx));
                            break;
                        }
                    }

                    let ty = match ty {
                        Some(ty) => ty,
                        None => {
                            // cannot decide the type of the global slot, just ignore it
                            continue;
                        }
                    };

                    self.slot_tys.insert(symbol.clone(), ty);
                }
            }
        }

        for func in ctx
            .funcs()
            .iter()
            .filter(|f| f.name(ctx) != &Symbol::from("main"))
        {
            for block in func.iter(ctx) {
                for inst in block.iter(ctx) {
                    if let InstKind::GetGlobal(sym) = inst.kind(ctx) {
                        // exclude the global slots used in other functions
                        self.slot_users.remove(sym);
                    }
                }
            }
        }
    }

    pub fn internalize(&mut self, ctx: &mut Context, func: Func, globals: Vec<GlobalSlot>) -> bool {
        let mut changed = false;

        let entry = func.entry_node(ctx);

        let mut insert_point = entry.head(ctx).unwrap();

        while let InstKind::StackSlot(_) = insert_point.kind(ctx) {
            insert_point = insert_point.next(ctx).unwrap();
        }

        for slot in globals {
            let slot_users = self.slot_users.remove(slot.name(ctx));

            if slot_users.is_none() {
                // not used in main function or used in other functions, ignore
                continue;
            }

            let ty = if let Some(ty) = self.slot_tys.get(slot.name(ctx)) {
                *ty
            } else {
                // cannot decide the type of the global slot, just ignore it
                continue;
            };

            let slot_users = slot_users.unwrap();
            let slot_size = slot.size(ctx);

            if slot_size > 8 {
                // only internalize small slots
                continue;
            }

            if !ty.is_float(ctx) && !ty.is_integer(ctx) {
                // only internalize integer and float slots
                continue;
            }

            changed = true; // we are going to internalize it

            // insert `stack_slot` of the same size at the beginning of the entry block
            let slot_name = slot.name(ctx).to_string();

            let stack_slot = Inst::stack_slot(ctx, slot_size as u32);
            // internalized slots are placed at the beginning of the entry block
            entry.push_front(ctx, stack_slot);

            let stack_slot = stack_slot.result(ctx, 0);
            stack_slot.alloc_name(ctx, format!("INTERNALIZED_{}_", slot_name));

            // initialize the slot with global slot's value
            // insert before the insert_point, so the slot will initialized before any usage
            match slot.init(ctx).kind() {
                ConstantKind::Undef => {}
                ConstantKind::Zeroinit => {
                    if ty.is_float(ctx) {
                        let zero = Inst::fconst(ctx, 0.0, ty);
                        let store = Inst::store(ctx, zero.result(ctx, 0), stack_slot);
                        insert_point.insert_before(ctx, zero);
                        insert_point.insert_before(ctx, store);
                    } else if ty.is_integer(ctx) {
                        let zero = Inst::iconst(ctx, 0, ty);
                        let store = Inst::store(ctx, zero.result(ctx, 0), stack_slot);
                        insert_point.insert_before(ctx, zero);
                        insert_point.insert_before(ctx, store);
                    } else {
                        unreachable!()
                    }
                }
                ConstantKind::Bytes(bytes) => {
                    let slice = bytes.as_slice();

                    if ty.is_float32(ctx) {
                        let value = f32::from_le_bytes(slice.try_into().unwrap());
                        let constant = Inst::fconst(ctx, value, ty);
                        let store = Inst::store(ctx, constant.result(ctx, 0), stack_slot);
                        insert_point.insert_before(ctx, constant);
                        insert_point.insert_before(ctx, store);
                    } else if ty.is_float64(ctx) {
                        let value = f64::from_le_bytes(slice.try_into().unwrap());
                        let constant = Inst::fconst(ctx, value, ty);
                        let store = Inst::store(ctx, constant.result(ctx, 0), stack_slot);
                        insert_point.insert_before(ctx, constant);
                        insert_point.insert_before(ctx, store);
                    } else if ty.is_integer(ctx) {
                        let value = u32::from_le_bytes(slice.try_into().unwrap());
                        let constant = Inst::iconst(ctx, value, ty);
                        let store = Inst::store(ctx, constant.result(ctx, 0), stack_slot);
                        insert_point.insert_before(ctx, constant);
                        insert_point.insert_before(ctx, store);
                    } else {
                        unreachable!()
                    }
                }
            }

            for user in slot_users {
                assert!(user.is_get_global(ctx));
                let result = user.result(ctx, 0);
                for user in result.users(ctx) {
                    user.replace(ctx, result, stack_slot);
                }
            }
        }

        changed
    }
}

impl GlobalPassMut for Global2Local {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        self.analyze_usage(ctx);

        let func_main = ctx.lookup_func(&Symbol::from("main")).unwrap();
        let globals: Vec<GlobalSlot> = ctx
            .global_slots()
            .into_iter()
            .filter(|global_slot| self.slot_users.contains_key(global_slot.name(ctx)))
            .collect();

        changed |= self.internalize(ctx, func_main, globals);

        Ok(((), changed))
    }
}

impl TransformPass for Global2Local {
    fn register(passman: &mut PassManager) {
        let pass = Global2Local::default();
        passman.register_transform(GLOBAL2LOCAL, pass, vec![]);
    }
}
