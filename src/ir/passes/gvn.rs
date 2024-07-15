use std::collections::HashMap;

use crate::{
    collections::linked_list::LinkedListContainerPtr,
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        Block,
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
                InstKind::Undef
                | InstKind::StackSlot(_)
                | InstKind::Jump
                | InstKind::Br
                | InstKind::Call(_)
                | InstKind::CallIndirect(_)
                | InstKind::Ret
                | InstKind::Load
                | InstKind::Store => {}
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
