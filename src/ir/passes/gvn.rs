use std::collections::{HashMap, HashSet};

use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        alias_analysis::{AliasAnalysis, AliasAnalysisResult},
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        Block,
        CastOp,
        Context,
        Func,
        Inst,
        InstKind,
        Ty,
        Value,
    },
    utils::{
        cfg::CfgInfo,
        def_use::{Usable, User},
        dominance::Dominance,
    },
};

pub const GVN: &str = "gvn";

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GVNInst {
    /// The kind of the instruction.
    kind: InstKind,
    /// The operands of the instruction.
    operands: Vec<Value>,
    /// Special Type for `Cast` instruction.
    ty: Option<Ty>,
}

impl GVNInst {
    pub fn new(kind: InstKind, operands: Vec<Value>, ty: Option<Ty>) -> Self {
        Self { kind, operands, ty }
    }

    pub fn from_inst(ctx: &Context, inst: Inst) -> Self {
        let kind = inst.kind(ctx).clone();
        // let operands = inst.operands(ctx).to_vec();
        let operands = if inst.is_commutative(ctx) {
            let mut operands = inst.operands(ctx).to_vec();
            operands.sort_by_key(|a| a.id());
            operands
        } else {
            inst.operands(ctx).to_vec()
        };
        let ty = match inst.kind(ctx) {
            InstKind::Cast(_) => Some(inst.result(ctx, 0).ty(ctx)),
            _ => None,
        };
        Self { kind, operands, ty }
    }

    pub fn kind(&self) -> &InstKind { &self.kind }

    pub fn operands(&self) -> &[Value] { &self.operands }

    pub fn ty(&self) -> Option<Ty> { self.ty }
}

#[derive(Default)]
pub struct GlobalValueNumbering {
    /// The value table.
    value_table: HashMap<GVNInst, Vec<Value>>,
    /// dominator tree
    dom: Dominance<Block>,
    /// change flag
    changed: bool,
}

impl GlobalValueNumbering {
    pub fn gvn_block(&mut self, ctx: &mut Context, block: &Block, depth: usize) {
        if depth > 100 {
            return;
        }

        let old_value_table = self.value_table.clone();

        let mut cursor = block.cursor();
        while let Some(inst) = cursor.next(ctx) {
            match inst.kind(ctx) {
                InstKind::IConst(_)
                | InstKind::FConst(_)
                | InstKind::IBinary(_)
                | InstKind::FBinary(_)
                | InstKind::IUnary(_)
                | InstKind::FUnary(_)
                | InstKind::Cast(_)
                | InstKind::Offset
                | InstKind::GetGlobal(_) => {
                    let gvn_inst = GVNInst::from_inst(ctx, inst);
                    // if the instruction is in the value table, replace it with the value in
                    // the table
                    if let Some(value) = self.value_table.get(&gvn_inst) {
                        for (old_value, new_value) in inst
                            .results(ctx)
                            .to_vec()
                            .iter()
                            .zip(value.iter())
                            .map(|(a, b)| (*a, *b))
                        {
                            let users = old_value.users(ctx).clone();
                            for user in users {
                                user.replace(ctx, old_value, new_value);
                                self.changed = true;
                            }
                        }
                    } else {
                        // if the instruction is not in the value table, add it to the table
                        self.value_table.insert(gvn_inst, inst.results(ctx).into());
                    }
                }
                // DONE:
                // Load %a ... Load %a
                // The second Load should be replaced with the first Load
                // Load %a ... Store %a ... Load %a
                // The second Load should not be replaced
                // Load %a ... Store %a
                // No action
                // Store %a ... Load %a
                // The Load should be replaced with the Store
                // TODO:
                // Store %a ... Store %a
                // The first store is redundant
                // Store %a ... Load %a ... Store %a
                // The first store is not redundant
                // Store %a ... %a is not used
                // The store is redundant
                InstKind::Load => {
                    // travel reverse in this block to find any def of the same value
                    let ptr = inst.operand(ctx, 0);
                    let mut curr_inst = inst;
                    let mut maybe_def_val = None;
                    let mut get_clobbered = false;
                    while let Some(last_inst) = curr_inst.prev(ctx) {
                        match last_inst.kind(ctx) {
                            InstKind::Store => {
                                match AliasAnalysis::analyze(ctx, ptr, last_inst.operand(ctx, 1)) {
                                    AliasAnalysisResult::MustAlias => {
                                        // if we get a must alias, we can replace the load with the
                                        // store's val
                                        maybe_def_val = Some(last_inst.operand(ctx, 0));
                                        break;
                                    }
                                    AliasAnalysisResult::MayAlias => {
                                        // if we get a may alias, its clobbered by the store, so we
                                        // quit
                                        get_clobbered = true;
                                        break;
                                    }
                                    AliasAnalysisResult::NoAlias => {
                                        // if we get a no alias, we can continue
                                    }
                                }
                            }
                            InstKind::Load => {
                                match AliasAnalysis::analyze(ctx, ptr, last_inst.operand(ctx, 0)) {
                                    AliasAnalysisResult::MustAlias => {
                                        // if we get a must alias, we can replace the load with the
                                        // load's val
                                        maybe_def_val = Some(last_inst.result(ctx, 0));
                                        break;
                                    }
                                    AliasAnalysisResult::MayAlias
                                    | AliasAnalysisResult::NoAlias => {
                                        // if we get a may alias or no alias, we
                                        // can continue
                                    }
                                }
                            }
                            InstKind::Call(_) | InstKind::CallIndirect(_) => {
                                // if we get a call, we can't continue
                                get_clobbered = true;
                                break;
                            }
                            InstKind::GetGlobal(_)
                            | InstKind::Offset
                            | InstKind::StackSlot(_)
                            | InstKind::Cast(CastOp::IntToPtr) => {
                                // if we get a get_global, offset or stack_slot, we can't continue
                                match AliasAnalysis::analyze(ctx, ptr, last_inst.result(ctx, 0)) {
                                    AliasAnalysisResult::MustAlias
                                    | AliasAnalysisResult::MayAlias => {
                                        // if we get a must alias, we meet the
                                        // true def of this val, we should treat
                                        // as clobber // TODO: REALLY?
                                        get_clobbered = true;
                                        break;
                                    }
                                    AliasAnalysisResult::NoAlias => {
                                        // if we get a no alias, we can continue
                                    }
                                }
                            }
                            _ => {}
                        }
                        curr_inst = last_inst;
                    }
                    // if we found a def, replace the load with the def
                    if let Some(def_val) = maybe_def_val {
                        let users = inst.result(ctx, 0).users(ctx).clone();
                        for user in users {
                            user.replace(ctx, inst.result(ctx, 0), def_val);
                            self.changed = true;
                        }
                        println!(
                            "[ gvn ] eliminate load {:?} with {:?}",
                            inst.result(ctx, 0),
                            def_val
                        );
                    } else if !get_clobbered {
                        // this means we meet the start of the block
                        // we will trace back to the predecessor blocks
                        let mut worklist_blocks = Vec::new();
                        let mut visited_blocks = HashSet::new();
                        for pred in block.preds(ctx) {
                            worklist_blocks.push(pred);
                        }
                        let mut maybe_def_val_list = Vec::new();
                        while let Some(work_block) = worklist_blocks.pop() {
                            if visited_blocks.contains(&work_block) {
                                continue;
                            }
                            let mut maybe_def_val = None;
                            let mut get_clobbered = false;
                            for block_inst in work_block.iter(ctx).rev() {
                                // if we meet currrent block, then we should stop at current inst
                                if block_inst == inst {
                                    break;
                                }
                                match block_inst.kind(ctx) {
                                    InstKind::Store => {
                                        match AliasAnalysis::analyze(
                                            ctx,
                                            ptr,
                                            block_inst.operand(ctx, 1),
                                        ) {
                                            AliasAnalysisResult::MustAlias => {
                                                // if we get a must alias, we can replace the load
                                                // with the store's val
                                                maybe_def_val = Some(block_inst.operand(ctx, 0));
                                                break;
                                            }
                                            AliasAnalysisResult::MayAlias => {
                                                // if we get a may alias, its clobbered by the
                                                // store, so we quit
                                                get_clobbered = true;
                                                break;
                                            }
                                            AliasAnalysisResult::NoAlias => {
                                                // if we get a no alias, we can
                                                // continue
                                            }
                                        }
                                    }
                                    InstKind::Load => {
                                        match AliasAnalysis::analyze(
                                            ctx,
                                            ptr,
                                            block_inst.operand(ctx, 0),
                                        ) {
                                            AliasAnalysisResult::MustAlias => {
                                                // if we get a must alias, we can replace the load
                                                // with the load's val
                                                maybe_def_val = Some(block_inst.result(ctx, 0));
                                                break;
                                            }
                                            AliasAnalysisResult::MayAlias
                                            | AliasAnalysisResult::NoAlias => {
                                                // if we get a may alias or no
                                                // alias, we can continue
                                            }
                                        }
                                    }
                                    InstKind::Call(_) | InstKind::CallIndirect(_) => {
                                        // if we get a call, we can't continue
                                        get_clobbered = true;
                                        break;
                                    }
                                    InstKind::GetGlobal(_)
                                    | InstKind::Offset
                                    | InstKind::StackSlot(_)
                                    | InstKind::Cast(CastOp::IntToPtr) => {
                                        // if we get a get_global, offset or stack_slot, we can't
                                        // continue
                                        match AliasAnalysis::analyze(
                                            ctx,
                                            ptr,
                                            block_inst.result(ctx, 0),
                                        ) {
                                            AliasAnalysisResult::MustAlias
                                            | AliasAnalysisResult::MayAlias => {
                                                // if we get a must alias, we meet the
                                                // true def of this val, we should treat
                                                // as clobber // TODO: REALLY?
                                                get_clobbered = true;
                                                break;
                                            }
                                            AliasAnalysisResult::NoAlias => {
                                                // if we get a no alias, we can
                                                // continue
                                            }
                                        }
                                    }
                                    _ => {}
                                }
                            }
                            if let Some(def_val) = maybe_def_val {
                                maybe_def_val_list.push(Some(def_val));
                            } else if get_clobbered {
                                maybe_def_val_list.push(None);
                            } else {
                                // we meet the start of the block, we will trace back to the
                                // predecessor
                                visited_blocks.insert(work_block);
                                for pred in work_block.preds(ctx) {
                                    if !visited_blocks.contains(&pred) {
                                        worklist_blocks.push(pred);
                                    }
                                }
                            }
                        }
                        // if there is no none in the maybe_def_val_list, and all
                        // the values are the same, we can replace the load with the
                        // value
                        if !maybe_def_val_list.is_empty()
                            && maybe_def_val_list.iter().all(|x| x.is_some())
                            && maybe_def_val_list
                                .iter()
                                .all(|x| x.unwrap() == maybe_def_val_list[0].unwrap())
                        {
                            let def_val = maybe_def_val_list[0].unwrap();
                            let users = inst.result(ctx, 0).users(ctx).clone();
                            for user in users {
                                user.replace(ctx, inst.result(ctx, 0), def_val);
                                self.changed = true;
                            }
                            println!(
                                "[ gvn ] eliminate load {:?} with {:?}",
                                inst.result(ctx, 0),
                                def_val
                            );
                        }
                    }
                }
                InstKind::Store => {}
                InstKind::Call(_) | InstKind::CallIndirect(_) => {}
                InstKind::Undef
                | InstKind::StackSlot(_)
                | InstKind::Jump
                | InstKind::Br
                | InstKind::Ret => {}
            }
        }

        // recursively apply GVN to the children of dominator tree
        let children = self.dom.children(*block).to_vec();
        for child in children {
            self.gvn_block(ctx, &child, depth + 1);
        }

        // restore old value table
        self.value_table = old_value_table;
    }
}

impl LocalPassMut for GlobalValueNumbering {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        self.changed = false;

        let cfg = CfgInfo::new(ctx, func);
        self.dom = Dominance::new(ctx, &cfg);

        self.value_table = HashMap::new();

        // apply GVN to the entry block
        if let Some(entry_block) = func.head(ctx) {
            self.gvn_block(ctx, &entry_block, 0);
        }

        Ok(((), self.changed))
    }
}

impl GlobalPassMut for GlobalValueNumbering {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        for func in ctx.funcs() {
            let ((), local_changed) = LocalPassMut::run(self, ctx, func)?;
            changed |= local_changed;
        }

        Ok(((), changed))
    }
}

impl TransformPass for GlobalValueNumbering {
    fn register(passman: &mut crate::ir::passman::PassManager) {
        let pass = Self::default();
        passman.register_transform(
            GVN,
            pass,
            vec![
            //     Box::<ConstantFolding>::default(),
            //     Box::<InstCombine>::default(),
            ],
        );
    }
}
