use std::{collections::{HashMap, HashSet}, ops::Deref, vec};

use crate::{
    backend::riscv64::inst, collections::{linked_list::{LinkedListContainerPtr, LinkedListNodePtr}, storage::ArenaPtr}, ir::{
        debug, global::Symbol, passman::{GlobalPassMut, PassManager, PassResult, TransformPass}, ty::TyData, Constant, ConstantKind, Context, Func, GlobalSlot, Inst, InstKind, Ty
    }, utils::{cfg::CfgRegion, def_use::User}
};

pub const GLOBAL2LOCAL: &str = "global2local";

// 用于对全局内容进行使用情况分析。
// 全局分析：
// 0. 参照User-Usable-Operand进行。
// 1. 分析用户Inst及其所在函数。
// 生成：
// 1. 因为要调用Block::new_param(ctx, ty)生成Value，所以要存储ty（在slot中）；
// 2. 因为要调用用户Func的entry_node(ctx)得到Block，所以要存储Func；

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

#[derive(Default, Debug)]
pub struct Global2Local {
    // Same as ValueData::users: FxHashMap<Inst, usize>,
    // Here Symbol is the name of global_slot (GlobalSlot::name(self, ctx))
    // Get global_slots from ctx.global_slots(),
    // Get allocation infomation from Inst::GetGlobal(Symbol), remember to 
    // check whether Symbol is a global slot or a function name.

    slots: HashMap<Symbol, SlotUsers>,
}

impl Global2Local {
    // 向slots中添加一个槽，或者向slots中现有的槽的user_funcs/user_insts中添加一个函数/指令。
    pub fn add_global_slot_user(&mut self, symbol: &Symbol, user_inst: &Inst, user_func: &Func) {
        if self.slots.get_mut(symbol).is_none() {
            let slotusers = SlotUsers::new();
            self.slots.insert(symbol.clone(), slotusers);
        }
        let slotusers = self.slots.get_mut(symbol).unwrap();
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
        for (symbol, slotusers) in self.slots.iter() {
            println!("{}: ", symbol);
            for func in slotusers.user_funcs.iter() {
                println!("  func:{};", func.name(ctx));
            }
        }
    }

    pub fn init_new_local_stack_slot(
        &self,
        ctx: &mut Context,
        local_stack_slot: &Inst,
        global_slot: &GlobalSlot,
    ) -> bool {
        let mut changed = false;
        let slot_size = global_slot.ty(ctx).bytewidth(ctx).unwrap();

        match global_slot.init(ctx).kind() {
            // 对于创建好的局部槽，通过在Block中添加IR指令（Inst）进行初始化。

            // (1) 如果全局槽为undef，则不需进行初始化，并删除全局槽（以及对全局槽的调用，后面一块删；剩下的相同）；
            ConstantKind::Undef => {
                global_slot.remove(ctx);
            }
            // (2)如果为zeroinit，则使用memset初始化，并删除全局槽；
            ConstantKind::Zeroinit => {
                let int_ty = Ty::int(ctx, 32);
                let inst_zero_init = Inst::iconst(ctx, 0, int_ty);
                let inst_size = Inst::iconst(ctx, slot_size as i32, int_ty);
                let result_tys = vec![Ty::void(ctx)];
                let inst_call_memset = Inst::call(
                    ctx,
                    "memset",
                    vec![
                        local_stack_slot.result(ctx, 0),
                        inst_zero_init.result(ctx, 0),
                        inst_size.result(ctx, 0),
                    ],
                    result_tys,
                );
                local_stack_slot.extend_after(
                    ctx,
                    vec![
                        inst_zero_init,
                        inst_size,
                        inst_call_memset,
                    ]
                );

                global_slot.remove(ctx);
            }
            // (3) 如果有bytes则使用memcpy或者常量指令：
            ConstantKind::Bytes(_) => {
                // 接下来的some_inst_constant是用于判断全局槽存储的是单个变量（三种）还是其他（例如数组）的，顺便携带单个变量的种类信息
                let content_bytes = global_slot.init(ctx).get_bytes().unwrap().as_slice();
                let some_inst_constant = match global_slot.ty(ctx).deref(ctx){
                    TyData::Integer(_) => { Some(Inst::iconst(ctx, u32::from_le_bytes(content_bytes.try_into().unwrap()), global_slot.ty(ctx))) },
                    TyData::Float32 => { Some(Inst::fconst(ctx, f32::from_le_bytes(content_bytes.try_into().unwrap()), global_slot.ty(ctx))) },
                    TyData::Float64 => { Some(Inst::fconst(ctx, f64::from_le_bytes(content_bytes.try_into().unwrap()), global_slot.ty(ctx))) },
                    TyData::Void | TyData::Ptr | TyData::Simd{ .. } | TyData::Array{ .. } | TyData::Struct{ .. } => { None }
                };
                if let Some(inst_constant) = some_inst_constant {
                    // 如果全局槽存储单个变量(int,float32,float64)，则在局部槽之后：
                    // 1.创建对应类型的常数
                    // 2.将常数存储到局部槽
                    // 3.删除全局槽
                    let inst_store = Inst::store(ctx, inst_constant.result(ctx, 0), local_stack_slot.result(ctx, 0));
                        local_stack_slot.extend_after(
                            ctx,
                            vec![
                                inst_constant,
                                inst_store,
                            ]
                        );
                        global_slot.remove(ctx);
                } else {
                    // 对于其他情况，则在局部槽之后：
                    // 1.获取全局槽指针
                    // 2.设置槽的字节数
                    // 3.调用memcpy函数（即使用Inst::call）将数组内容拷贝到局部槽
                    // 注意，此处需要调用memcpy将全局槽复制过来，所以原全局槽不能删掉。但是调用全局槽的指令需要删掉。由于该指令的返回值仍被其他指令使用，所以应该先更改其他指令（即进行步骤三）再删除。
                    let int_ty = Ty::int(ctx, 32);
                    let inst_get_global = Inst::get_global(ctx, global_slot.name(ctx).clone());
                    let inst_size = Inst::iconst(ctx, slot_size as i32, int_ty);
                    let result_tys = vec![Ty::void(ctx)];
                    let inst_call = Inst::call(
                        ctx,
                        "memcpy", 
                        vec![
                            local_stack_slot.result(ctx, 0),
                            inst_get_global.result(ctx, 0),
                            inst_size.result(ctx, 0),
                        ],
                        result_tys,
                    );
                    local_stack_slot.extend_after(
                        ctx, 
                        vec![
                            inst_get_global,
                            inst_size,
                            inst_call,
                        ],
                    )
                }
            }
        } // match global_slot.init(ctx).kind()
        changed
    }

    pub fn change_global_into_local(
        &self,
        ctx: &mut Context,
        func: &Func,
        global_slot: &GlobalSlot,
        slotusers: &SlotUsers,
    ) -> bool {
        let mut changed = true;

        // 把全局槽移到这个函数中变为局部变量。步骤：
        // 1.在局部（入口块）添加StackSlot类型的inst，确保这个局部槽具有与全局槽相同的特性。
        // 2.遍历所有GetGlobal类型的inst，找到那些获取目标全局槽的指令，记录这些指令中代表全局槽的指针变量，并将这些变量存储到一个集合（old_ptrs）中，随后删除这些指令。
        // 3.遍历所有Load和Store指令，检查它们的操作数是否在old_ptrs集合中。如果是，将操作数替换为新的局部指针变量（ptr_new）。

        // 1.
        let entry_block = func.entry_node(ctx);
        let slot_size = global_slot.ty(ctx).bytewidth(ctx).unwrap();
        let local_stack_slot = Inst::stack_slot(ctx, slot_size as u32);
        entry_block.push_front(ctx, local_stack_slot);
        entry_block.set_head(ctx, Some(local_stack_slot));

        changed |= self.init_new_local_stack_slot(ctx, &local_stack_slot, global_slot);

        println!("1st step done.");

        // 2.遍历所有GetGlobal类型的inst，找到那些获取目标全局槽的指令，记录这些指令返回的指向全局槽的指针变量，并将这些变量存储到一个集合（ptrs_to_global_slot）中。之后的代码会删除这些指令。
        let mut ptrs_to_global_slot = HashSet::new();
        for inst in slotusers.user_insts.iter() {
            // there are all GetGlobal insts in slotusers.user_insts
            // 并且它们调用的全局槽也都是这个全局槽
            let old_ptr = inst.result(ctx, 0);
            ptrs_to_global_slot.insert(old_ptr);
        }
        println!("2nd step done.");

        // 3.遍历所有Load、Store和Offset类型的指令，检查它们的操作数（旧全局槽的局部指针）是否在old_ptrs集合中。如果是，将操作数替换为新的局部指针变量（ptr_new）。
        // 这个玩意是用来避免犯同时进行可变与不可变引用的错误（有关ctx）
        let mut modifications = Vec::new();

        for block in func.iter(ctx) {
            for inst in block.iter(ctx) {
                match inst.kind(ctx) {
                    InstKind::Load => {
                        let ptr_to_global_slot = inst.operand(ctx, 0);
                        if ptrs_to_global_slot.contains(&ptr_to_global_slot) {
                            // 收集需要替换的信息
                            let new_ptr = local_stack_slot.result(ctx, 0);
                            modifications.push((inst, ptr_to_global_slot, new_ptr));
                        }
                    }
                    InstKind::Store => {
                        let ptr_to_global_slot = inst.operand(ctx, 1);
                        if ptrs_to_global_slot.contains(&ptr_to_global_slot) {
                            // 收集需要替换的信息
                            let new_ptr = local_stack_slot.result(ctx, 0);
                            modifications.push((inst, ptr_to_global_slot, new_ptr));
                        }
                    }
                    InstKind::Offset => {
                        let ptr_to_global_slot = inst.operand(ctx, 0);
                        if ptrs_to_global_slot.contains(&ptr_to_global_slot) {
                            // 收集需要替换的信息
                            let new_ptr = local_stack_slot.result(ctx, 0);
                            modifications.push((inst, ptr_to_global_slot, new_ptr));
                        }
                    }
                    InstKind::Call(_) => {
                        for ptr in inst.operands(ctx) {
                            if ptrs_to_global_slot.contains(&ptr) {
                                // 收集需要替换的信息
                                let new_ptr = local_stack_slot.result(ctx, 0);
                                modifications.push((inst, ptr, new_ptr));
                            }
                        }
                    }
                    _ => {
                        for ptr in inst.operands(ctx) {
                            if ptrs_to_global_slot.contains(&ptr) {
                                panic!("Unexpected pointer to global slot: {}", ptr.name(ctx).unwrap());
                            }
                        }
                    }
                }
            }
        }

        // 在不再持有不可变引用的情况下进行修改
        for (inst, old_ptr, new_ptr) in modifications {
            inst.replace(ctx, old_ptr, new_ptr);
        }
        println!("3rd step done.");

        // ctx.alloc_all_names();
        // println!("{}", ctx.display(debug));
        // let debug = true;
        for old_get_global_inst in slotusers.user_insts.iter() {
            // println!("Removing inst: {}", old_get_global_inst.display(ctx, debug));
            old_get_global_inst.remove(ctx);
        }

        changed
    }
}

impl GlobalPassMut for Global2Local {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        self.analyze_usage(ctx);
        println!("Usage analyzed.");

        // let debug = true;
        // ctx.alloc_all_names();
        // println!("{}", ctx.display(debug));
        let mut changed = false;

        // 如果usage_map中的某个全局槽对应0个函数，删掉这个全局槽
        // 如果usage_map中的某个全局槽对应1个函数，把这个全局槽的初始化移到这个函数中
        // 如果usage_map中的某个全局槽对应多个函数，不管

        for global_slot in ctx.global_slots() {
            if let Some(slotusers) = self.slots.get(global_slot.name(ctx)) {
                if let Some(func) = slotusers.single_func() {
                    if func.name(ctx).to_string() == "main" {
                        changed = self.change_global_into_local(ctx, &func, &global_slot, slotusers);
                    }
                }
            } else {
                changed = true;
                global_slot.remove(ctx);
            }
        }

        // 只需要定义Value即可，名称分配自动进行，无需担心
        ctx.alloc_all_names();

        // println!("{}", ctx.display(debug));

        Ok(((), changed))
    }
}

impl TransformPass for Global2Local {
    fn register(passman: &mut PassManager) {
        let pass = Global2Local::default();
        passman.register_transform(GLOBAL2LOCAL, pass, vec![]);
    }
}