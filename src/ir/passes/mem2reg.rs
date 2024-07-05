use std::collections::{HashMap, HashSet, VecDeque};

use super::control_flow::CfgCanonicalize;
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassManager, PassResult, TransformPass},
        Block,
        Context,
        Func,
        Inst,
        InstKind,
        Ty,
        Value,
        ValueKind,
    },
    utils::{
        cfg::{CfgInfo, CfgNode, CfgRegion},
        def_use::{Usable, User},
        dominance::Dominance,
    },
};

/// The pass name of mem2reg.
pub const MEM2REG: &str = "mem2reg";

/// Mem2reg pass.
///
/// This pass will promote stack slots to registers.
///
/// In LLVM, this promotion will generate phi instructions.
///
/// ```llvm
/// %dst:
///     %x0 = phi [%val0, %pred0], [%val1, %pred1], ...
/// ```
///
/// But since we use block parameters, the generated code will be like:
///
/// ```orzir
/// ^pred0:
///     //...
///     br %cond, ^dst(%val0), ...
/// ^pred1:
///     //...
///     br %cond, ^dst(%val1), ...
/// ^dst(%x0): // type ignored
///     //...
/// ```
///
/// The algorithms are similar, block param form just hoists the moves to the
/// predecessors.
#[derive(Default)]
pub struct Mem2reg {
    /// Variable set.
    vars: HashSet<Value>,

    /// Define blocks
    def_blocks: HashMap<Value, HashSet<Block>>,

    /// Block worklists
    worklists: HashMap<Value, VecDeque<Block>>,

    /// Incoming values
    incomings: HashMap<Value, Value>,

    /// The param to variable map.
    block_params: HashMap<Value, Value>,

    /// Dominance
    dominance: Dominance<Block>,

    /// The types of the variables.
    types: HashMap<Value, Ty>,
}

impl Mem2reg {
    fn reset(&mut self) {
        self.vars.clear();
        self.def_blocks.clear();
        self.worklists.clear();
        self.incomings.clear();
        self.block_params.clear();
        self.types.clear();
    }

    fn prepare(&mut self, ctx: &mut Context, func: Func) {
        let cfg = CfgInfo::new(ctx, func);
        self.dominance = Dominance::new(ctx, &cfg);

        // initialize variables
        // look for all the stack slots in the function, which should be in the entry
        // block.
        let entry = func.entry_node(ctx);

        let mut last_stack_slot = entry.head(ctx).unwrap();

        for inst in entry.iter(ctx) {
            if let InstKind::StackSlot(_) = inst.kind(ctx) {
                let slot = inst.result(ctx, 0);
                if let Some(ty) = self.is_promotable(ctx, slot) {
                    self.vars.insert(slot);
                    self.types.insert(slot, ty);
                    self.def_blocks.insert(slot, HashSet::new());
                    self.worklists.insert(slot, VecDeque::new());
                }
                last_stack_slot = inst;
            } else {
                // actually it is possible to support stack slot at any position, but
                // for now just follow what llvm does.
                break;
            }
        }

        // we also need to insert undef instructions to construct undef value.
        // the instructions should be right after the last stack slot.
        for var in self.vars.iter() {
            let undef = Inst::undef(ctx, self.types[var]);
            last_stack_slot.insert_after(ctx, undef);
            self.incomings.insert(*var, undef.result(ctx, 0));
            last_stack_slot = undef; // not necessary, just to keep order.
        }

        // construct worklist
        for block in func.iter(ctx) {
            for inst in block.iter(ctx) {
                if let InstKind::Store = inst.kind(ctx) {
                    let ptr = inst.operand(ctx, 1);
                    if self.vars.contains(&ptr) && !self.def_blocks[&ptr].contains(&block) {
                        // this ptr is a to-be-promoted stack slot
                        self.def_blocks.get_mut(&ptr).unwrap().insert(block);
                        self.worklists.get_mut(&ptr).unwrap().push_back(block);
                    }
                }
            }
        }
    }

    fn insert_block_params(&mut self, ctx: &mut Context) {
        let mut inserted: HashMap<Value, HashSet<Block>> = HashMap::new();

        for (slot, worklist) in self.worklists.iter_mut() {
            while !worklist.is_empty() {
                let block = worklist.pop_front().unwrap();
                for df in self.dominance.frontier(block) {
                    if inserted.entry(*slot).or_default().contains(df) {
                        continue;
                    }

                    let ty = self.types[slot];
                    let param = df.new_param(ctx, ty);

                    // map param -> value
                    self.block_params.insert(param, *slot);

                    inserted.entry(*slot).or_default().insert(*df);

                    if !self.def_blocks[slot].contains(df) {
                        worklist.push_back(*df);
                    }
                }
            }
        }
    }

    fn is_promotable(&self, ctx: &Context, value: Value) -> Option<Ty> {
        let kind = value.kind(ctx);

        // we can only know the type from all the uses of the value, i.e. load
        // instructions. also, we must make sure that the def is a `stack_slot`

        if let ValueKind::InstResult { inst, idx: 0 } = kind {
            // only stack slot can be promoted
            if let InstKind::StackSlot(_) = inst.kind(ctx) {
                let mut ty = None;
                // TODO: actually, we can support different types for stores and loads into the
                // same slot, by just modifying the renaming process, but for now, only same
                // type is supported.
                for user in value.users(ctx) {
                    if let InstKind::Load = user.kind(ctx) {
                        let result_ty = user.result(ctx, 0).ty(ctx);
                        if ty.is_none() {
                            ty = Some(result_ty);
                        } else if ty != Some(result_ty) {
                            return None;
                        }
                    } else if let InstKind::Store = user.kind(ctx) {
                        // the pointer can be used as the val, or the ptr, we need to check both
                        // cases. if it is used as a val, we cannot promote it.
                        let val = user.operand(ctx, 0);
                        if val == value {
                            // the value is used as a non-ptr, cannot promote
                            return None;
                        }
                        let val_ty = val.ty(ctx);
                        if ty.is_none() {
                            ty = Some(val_ty);
                        } else if ty != Some(val_ty) {
                            return None;
                        }
                    } else {
                        return None; // other uses are also not promotable
                    }
                }
                return ty;
            }
        }
        None
    }

    fn rename(&mut self, ctx: &mut Context, block: Block) -> bool {
        let mut changed = false;

        // TODO: not efficient, maybe stack can be used to optimize.
        let incomings_ckpt = self.incomings.clone();

        for param in block.params(ctx) {
            if self.block_params.contains_key(param) {
                let var = self.block_params[param];
                if self.vars.contains(&var) {
                    self.incomings.insert(var, *param);
                }
            }
        }

        let mut cursor = block.cursor();

        let mut insts_to_remove = Vec::new();

        while let Some(inst) = cursor.next(ctx) {
            if let InstKind::Load = inst.kind(ctx) {
                let ptr = inst.operand(ctx, 0);
                let result = inst.result(ctx, 0);
                if self.vars.contains(&ptr) {
                    let incoming = self.incomings[&ptr];
                    for user in result.users(ctx) {
                        user.replace(ctx, result, incoming);
                    }
                    assert!(result.users(ctx).is_empty());
                    insts_to_remove.push(inst);
                }
            } else if let InstKind::Store = inst.kind(ctx) {
                let val = inst.operand(ctx, 0);
                let ptr = inst.operand(ctx, 1);
                if self.vars.contains(&ptr) {
                    self.incomings.insert(ptr, val);
                    insts_to_remove.push(inst);
                }
            }
        }

        // edit the branch instructions
        for succ in block.succs(ctx) {
            let mut additional_param_map = HashMap::new();

            for param in succ.params(ctx) {
                if let Some(var) = self.block_params.get(param) {
                    let incoming = self.incomings[var];
                    additional_param_map.insert(*param, incoming);
                }
            }

            let inst = block.tail(ctx);

            if let Some(inst) = inst {
                if inst.is_terminator(ctx) {
                    for (param, incoming) in additional_param_map {
                        inst.add_succ_arg(ctx, succ, param, incoming);
                    }
                } else {
                    panic!("block tail is not a terminator, do canonicalization first");
                }
            } else {
                panic!("block has no tail, do canonicalization first");
            }
        }

        // remove the insts
        for inst in insts_to_remove {
            inst.remove(ctx);
        }

        let children = self.dominance.children(block).to_vec();
        for child in children {
            changed |= self.rename(ctx, child);
        }

        self.incomings = incomings_ckpt;
        changed
    }
}

impl LocalPassMut for Mem2reg {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        self.reset();
        self.prepare(ctx, func);
        self.insert_block_params(ctx);

        // now we need to rename
        let entry = func.entry_node(ctx);
        let mut changed = self.rename(ctx, entry);
        for var in self.vars.iter() {
            if let ValueKind::InstResult { inst, .. } = var.kind(ctx) {
                inst.remove(ctx);
                changed = true;
            }
        }
        Ok(((), changed))
    }
}

impl GlobalPassMut for Mem2reg {
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

impl TransformPass for Mem2reg {
    fn register(passman: &mut PassManager) {
        let pass = Mem2reg::default();
        passman.register_transform(MEM2REG, pass, vec![Box::new(CfgCanonicalize)]);
    }
}
