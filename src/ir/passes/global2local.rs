use core::panic;
use std::{collections::{HashMap, HashSet}, vec};

use crate::{
    collections::{linked_list::{LinkedListContainerPtr, LinkedListNodePtr}, storage::ArenaPtr}, ir::{
        global::Symbol, passman::{GlobalPassMut, PassManager, PassResult, TransformPass}, ty::TyData, ConstantKind, Context, Func, GlobalSlot, Inst, InstKind, Ty, Value
    }, utils::{cfg::CfgRegion, def_use::User}
};

pub const GLOBAL2LOCAL: &str = "global2local";

#[derive(Default, Debug, Clone)]
pub struct SlotUsers{
    user_funcs: HashSet<Func>,
    user_insts: HashSet<Inst>,
}

impl SlotUsers {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn single_func(&self) -> Option<Func> {
        if self.user_funcs.len() == 1 {
            self.user_funcs.iter().next().cloned()
        } else {
            None
        }
    }
}

enum GlobalSlotInitKind {
    Undef,
    Zeroinit,
    BytesSingle,
    BytesMultiple,
}

#[derive(Default, Debug)]
pub struct Global2Local {
    // 全局槽到它的使用者的映射
    slots_users: HashMap<Symbol, SlotUsers>,

    // 之所以采用先存储后增添的方法是因为不同指令的创建位置不同：
    // 1. 栈槽的创建位置：在入口块的最前面
    // 2. 栈槽的初始化位置：在栈槽的创建之后（对于被移入局部的槽），或者在GetGlobal指令之后（对于无法移入局部的槽）
    // 至于需要修改、删除的指令也采用先存储后修改、删除的方式，
    // 这是因为必须先创建，才能修改，进而才能删除。

    // 需要在入口块最前面创建的指令（包含StackSlot类型的指令）
    insts_to_create_at_front: Vec<Inst>,
    // 需要在上面的指令之后创建的指令（包含初始化上面指令创建的栈槽的指令）
    insts_to_create_after_front: Vec<Inst>,
    // 需要在每个全局槽的第一个GetGlobal指令之后创建的指令（包含复制及其相关的指令）
    insts_to_create_after_inst: HashMap<Inst, Vec<Inst>>,
    // 需要修改的指令（包含使用全局槽指针（ptrs_to_global）的指令）
    insts_to_change: HashSet<(Inst, Value, Value)>,
    // 需要被删除的全局槽（除了无法移入局部的槽）
    slots_to_remove: HashSet<GlobalSlot>,
    // 需要被删除的指令（包含多余的GetGlobal指令）
    insts_to_remove: HashSet<Inst>,
}

impl Global2Local {
    // 向slots_users中添加一个槽，或者向slots_users中现有的槽的user_funcs/user_insts中添加一个函数/指令。
    pub fn add_global_slot_user(&mut self, symbol: &Symbol, user_inst: &Inst, user_func: &Func) {
        if self.slots_users.get_mut(symbol).is_none() {
            let slotusers = SlotUsers::new();
            self.slots_users.insert(symbol.clone(), slotusers);
        }
        let slotusers = self.slots_users.get_mut(symbol).unwrap();
        slotusers.user_funcs.insert(*user_func);
        slotusers.user_insts.insert(*user_inst);
    }

    pub fn analyze_usage(&mut self, ctx: &Context) {
        let slot_names: HashMap<Symbol, GlobalSlot> = 
            ctx.global_slots()
            .iter()
            .map(|&slot| (slot.name(ctx).clone(), slot))
            .collect::<HashMap<Symbol, GlobalSlot>>();

        for func in ctx.funcs() {
            for block in func.iter(ctx){
                for inst in block.iter(ctx){
                    if let InstKind::GetGlobal(symbol) = inst.kind(ctx) {
                        if slot_names.contains_key(symbol) {
                            self.add_global_slot_user(symbol, &inst, &func);
                        }
                    }
                }
            }
        }
    }

    pub fn display_usage_map(&self, ctx: &Context) {
        for (symbol, slotusers) in self.slots_users.iter() {
            println!("{}: ", symbol);
            for func in slotusers.user_funcs.iter() {
                println!("  func:{};", func.name(ctx));
            }
        }
    }

    pub fn change_global_into_local(
        &mut self,
        ctx: &mut Context,
        func: &Func,
        global_slot: &GlobalSlot,
        slotusers: &SlotUsers,
    ) -> bool {
        // slot_global                              slot
        // inst_create_slot_local           none -> (inst) -> ptr(value)
        // inst(s)_get_global               slot -> (inst) -> ptr(value)
        // inst_get_global_others           slot -> (inst) -> ptr(value)
        // inst_users_of_ptrs_to_global     ptr  -> (inst) -> others
        // ptr(s)_to_global                         ptr(value)
        // ptr_to_local                             ptr(value)
        if slotusers.user_funcs.len() != 1 {
            panic!("G2L-162: Global slot is used by not one functions.");
        }
        let changed = true;

        let initkind = match global_slot.init(ctx).kind() {
            ConstantKind::Undef => GlobalSlotInitKind::Undef,
            ConstantKind::Zeroinit => GlobalSlotInitKind::Zeroinit,
            ConstantKind::Bytes(_) => {
                if matches!(
                    global_slot.ty(ctx).deref(ctx), 
                    TyData::Integer(_) 
                    | TyData::Float32 
                    | TyData::Float64
                ) {
                    GlobalSlotInitKind::BytesSingle
                } else {
                    GlobalSlotInitKind::BytesMultiple
                }
            }
        };
        let size = global_slot.ty(ctx).bytewidth(ctx).unwrap();
        let mut insts_get_global = slotusers.user_insts.clone(); // 此处insts一定都在当前函数中，且使用的global_slot一定是当前的全局槽。
        let inst_first_get_global = func.iter(ctx)
            .flat_map(|block| block.iter(ctx))
            .find(|inst| { // 找到第一个GetGlobal类型的指令，而不是所有。
                if let InstKind::GetGlobal(symbol) = inst.kind(ctx) {
                    symbol == global_slot.name(ctx)
                } else {
                    false
                }
            }).unwrap();
        let inst_create_slot_local: Inst = Inst::stack_slot(ctx, size as u32);
        let ptrs_to_global = insts_get_global.iter()
            .map(|inst| inst.result(ctx, 0))
            .collect::<HashSet<_>>();
        let ptr_to_local : Value = inst_create_slot_local.result(ctx, 0);
        let insts_users_of_ptrs_to_global = func.iter(ctx)
            .flat_map(|block| block.iter(ctx))
            .filter(|inst| {
                !inst.operands(ctx).iter()
                .collect::<HashSet<_>>()
                .is_disjoint(&ptrs_to_global.iter().collect())
            }).collect::<HashSet<_>>();

        // 0. 特殊判断：
        if matches!(initkind, GlobalSlotInitKind::BytesMultiple)
             && insts_get_global.len() == 1
             && insts_users_of_ptrs_to_global.len() == 1 {
            return false;
        }
        if size > 8 { return false; }

        // 1. 确定inst_create_slot_local
        self.insts_to_create_at_front.push(inst_create_slot_local);

        // 2. 初始化inst_create_slot_local
        match initkind {
            GlobalSlotInitKind::Undef => {
                // do nothing
            }
            GlobalSlotInitKind::Zeroinit => {
                let type_int = Ty::int(ctx, 32);
                let type_void = Ty::void(ctx);
                let inst_zero_init = Inst::iconst(ctx, 0, type_int);
                let inst_size = Inst::iconst(ctx, size as i32, type_int);
                let inst_call_memset = Inst::call(
                    ctx,
                    "memset",
                    vec![
                        inst_create_slot_local.result(ctx, 0),
                        inst_zero_init.result(ctx, 0),
                        inst_size.result(ctx, 0),
                    ],
                    vec![type_void],
                );

                self.insts_to_create_after_front.extend(vec![
                    inst_zero_init,
                    inst_size,
                    inst_call_memset,
                ]);
            }
            GlobalSlotInitKind::BytesSingle => {
                let content_bytes = global_slot.init(ctx).get_bytes().unwrap().as_slice();
                let inst_constant = match global_slot.ty(ctx).deref(ctx){
                    TyData::Integer(_) => { Inst::iconst(
                        ctx, 
                        u32::from_le_bytes(content_bytes.try_into().unwrap()), 
                        global_slot.ty(ctx)
                    ) },
                    TyData::Float32 => { Inst::fconst(ctx, 
                        f32::from_le_bytes(content_bytes.try_into().unwrap()), 
                        global_slot.ty(ctx)
                    ) },
                    TyData::Float64 => { Inst::fconst(ctx, 
                        f64::from_le_bytes(content_bytes.try_into().unwrap()), 
                        global_slot.ty(ctx)
                    ) },
                    TyData::Void 
                    | TyData::Ptr 
                    | TyData::Simd{ .. } 
                    | TyData::Array{ .. } 
                    | TyData::Struct{ .. } => panic!("G2L: Unsupported global slot type"),
                };
                let inst_store = Inst::store(
                    ctx, 
                    inst_constant.result(ctx, 0), 
                    inst_create_slot_local.result(ctx, 0)
                );

                self.insts_to_create_after_front.extend(
                    vec![
                        inst_constant,
                        inst_store,
                    ]
                );
            }
            GlobalSlotInitKind::BytesMultiple => {
                let type_inst = Ty::int(ctx, 32);
                let type_void = Ty::void(ctx);
                let inst_size = Inst::iconst(ctx, size as i32, type_inst);
                let inst_call_memcpy = Inst::call(
                    ctx,
                    "memcpy",
                    vec![
                        inst_create_slot_local.result(ctx, 0),
                        inst_first_get_global.result(ctx, 0),
                        inst_size.result(ctx, 0),
                    ],
                    vec![type_void],
                );

                self.insts_to_create_after_inst.entry(inst_first_get_global).or_insert(vec![
                    inst_size,
                    inst_call_memcpy,
                ]);
            }
        };

        // 3. 分析需要删除的Inst（主要是GetGlobal）
        match initkind {
            GlobalSlotInitKind::Undef
            | GlobalSlotInitKind::Zeroinit
            | GlobalSlotInitKind::BytesSingle => {
                self.insts_to_remove.extend(insts_get_global);
            },
            GlobalSlotInitKind::BytesMultiple => {
                insts_get_global.remove(&inst_first_get_global);
                self.insts_to_remove.extend(insts_get_global);
            },
        }

        // 4. 分析需要更改的Inst
        for inst in insts_users_of_ptrs_to_global.iter() {
            for ptr_to_global in &ptrs_to_global{
                self.insts_to_change.insert((*inst, *ptr_to_global, ptr_to_local));
            }
        }

        // 5. 分析需要删除的Slot
        if !matches!(initkind, GlobalSlotInitKind::BytesMultiple) {
            self.slots_to_remove.insert(*global_slot);
        }
        changed
    }

    pub fn change_globals_into_locals(
        &mut self,
        ctx: &mut Context,
        func: &Func,
        globals: HashSet<GlobalSlot>,
    ) -> bool {
        let mut changed = false;
        let debug = false;
        let slots_users = self.slots_users.clone();
        for global in globals.iter() {
            let slotusers = slots_users.get(global.name(ctx)).unwrap();
            changed |= self.change_global_into_local(ctx, func, global, slotusers);
        }
       if debug { println!("change analyzed."); }

        // 这里一定要注意增添、修改和删除的顺序：先增添、再修改、最后删除。
        // 否则会导致后续的Inst的引用关系出错。

        let inst_head = func.entry_node(ctx).head(ctx).unwrap();
        if let Some(inst_to_create_first) = &self.insts_to_create_at_front.first(){
            for inst_to_create in &self.insts_to_create_at_front {
                if debug { println!("inst creating: {}", inst_to_create.display(ctx, true)); }
                inst_head.insert_before(ctx, *inst_to_create);
            }
            for inst_to_create in &self.insts_to_create_after_front {
                if debug { println!("after front inst creating: {}", inst_to_create.display(ctx, true)); }
                inst_head.insert_before(ctx, *inst_to_create);
            }
            func.entry_node(ctx).set_head(ctx, Some(**inst_to_create_first));
            changed |= true;
        }
        if debug { println!("inst created."); }

        for (inst, insts_to_create) in self.insts_to_create_after_inst.iter() {
            if debug {println!("else inst creating: {}", inst.display(ctx, true)); }
            inst.extend_after(ctx, insts_to_create.clone());
            changed |= true;
        }
        if debug {println!("else insts created."); }

        for (inst, old_ptr, new_ptr) in &self.insts_to_change {
            if debug { println!("inst changing: {}", inst.display(ctx, true)); }
            if debug { println!("  old_ptr: {}, new_ptr: {}", old_ptr.name(ctx).unwrap(), new_ptr.name(ctx).unwrap()); }
            inst.replace(ctx, *old_ptr, *new_ptr);
            changed |= true;
        }
        if debug {println!("insts changed."); }

        for inst_to_remove in self.insts_to_remove.iter() {
            if debug { println!("inst removing: {}", inst_to_remove.display(ctx, true)); }
            inst_to_remove.remove(ctx);
            changed |= true;
        }
        if debug { println!("insts removed."); }

        for slot_to_remove in self.slots_to_remove.iter() {
            if debug { println!("slot removing: {}", slot_to_remove.display(ctx, true)); }
            slot_to_remove.remove(ctx);
        }
        if debug { println!("slots removed."); }

        changed
    }
}

impl GlobalPassMut for Global2Local {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        // self.analyze_usage(ctx);
        // println!("Usage analyzed.");
        let mut changed = false;

        self.analyze_usage(ctx);
        println!("Usage analyzed.");

        // 先把需要G2L的全局槽按照函数分类，然后逐个函数进行G2L。
        let mut funcs_to_pass_with_globals: HashMap<Func, HashSet<GlobalSlot>> = HashMap::new();
        for global_slot in ctx.global_slots() {
            if let Some(slotusers) = self.slots_users.get(global_slot.name(ctx)) {
                if let Some(func) = slotusers.single_func() {
                    // changed = self.change_global_into_local(ctx, &func, &global_slot, slotusers);
                    funcs_to_pass_with_globals
                        .entry(func)
                        .or_default()
                        .insert(global_slot);
                } else {
                    /* do nothing */
                }
            } else {
                global_slot.remove(ctx);
                changed |= true;
            }
        }
        // 只对那些只被调用一次的函数进行G2L。
        let names_of_func_called_once = [String::from("main")];
        // 进行G2L。
        for (func, globals) in funcs_to_pass_with_globals {
            if names_of_func_called_once.contains(&func.name(ctx).to_string()) {
                changed |= self.change_globals_into_locals(ctx, &func, globals);
            }
        }

        ctx.alloc_all_names();

        Ok(((), changed))
    }
}

impl TransformPass for Global2Local {
    fn register(passman: &mut PassManager) {
        let pass = Global2Local::default();
        passman.register_transform(GLOBAL2LOCAL, pass, vec![]);
    }
}