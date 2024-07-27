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
    slots_users: HashMap<Symbol, Vec<Inst>>,
}

impl Global2Local {
    fn analyze_usage(&mut self, ctx: &Context) {
        // only collect the global slots used in main function
        let func_main = ctx.lookup_func(&Symbol::from("main")).unwrap();
        for block in func_main.iter(ctx) {
            for inst in block.iter(ctx) {
                if let InstKind::GetGlobal(symbol) = inst.kind(ctx) {
                    self.slots_users
                        .entry(symbol.clone())
                        .or_default()
                        // there should be no duplicate instructions
                        .push(inst);
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
                        self.slots_users.remove(sym);
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
            let slot_users = self.slots_users.remove(slot.name(ctx));

            if slot_users.is_none() {
                // not used in main function or used in other functions, ignore
                continue;
            }

            let slot_users = slot_users.unwrap();

            let ty = slot.ty(ctx);
            // TODO: eliminate the use of opaque pointer type, and use `R32` or `R64`
            // instead
            let bytewidth = ty.bytewidth(ctx).unwrap();

            if !ty.is_float(ctx) && !ty.is_integer(ctx) {
                // only internalize scalar types
                // TODO: is array really not profitable to internalize? how about memory
                // locality?
                continue;
            }

            // insert `stack_slot` of the same size at the beginning of the entry block
            let slot_name = slot.name(ctx).to_string();

            let stack_slot = Inst::stack_slot(ctx, bytewidth as u32);
            // internalized slots are placed at the beginning of the entry block
            entry.push_front(ctx, stack_slot);

            changed = true; // TODO: actually changed, not sure if this is correct

            let stack_slot = stack_slot.result(ctx, 0);
            stack_slot.alloc_name(ctx, format!("INTERNALIZED_{}_", slot_name));

            // initialize the slot with global slot's value
            // insert before the insert_point, so the slot will initialized before any usage
            match slot.init(ctx).kind() {
                ConstantKind::Undef => {}
                ConstantKind::Zeroinit => {
                    // TODO: the type should be get by the usages of the global slot

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
                        // TODO: actually reachable, but just ignore it for now
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

        self.slots_users = HashMap::new();
        self.analyze_usage(ctx);

        let func_main = ctx.lookup_func(&Symbol::from("main")).unwrap();
        let globals: Vec<GlobalSlot> = ctx
            .global_slots()
            .into_iter()
            .filter(|global_slot| self.slots_users.contains_key(global_slot.name(ctx)))
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
