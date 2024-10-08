use std::hash::Hash;

use super::{inst::MInst, regs::RegDefUse, LowerConfig, MContext};
use crate::collections::linked_list::LinkedListContainerPtr;

pub struct PeepholeRule<I, IT>
where
    I: MInst + Hash,
{
    pub rewriter: fn(&mut MContext<I>, &mut RegDefUse<I>, IT) -> bool,
}

pub type Peephole1<I> = PeepholeRule<I, I>;

pub type Peephole2<I> = PeepholeRule<I, (I, I)>;

pub type Peephole3<I> = PeepholeRule<I, (I, I, I)>;

pub struct PeepholeRunner<I, IT>
where
    I: MInst + Hash,
{
    rules: Vec<PeepholeRule<I, IT>>,
}

impl<I, IT> PeepholeRunner<I, IT>
where
    I: MInst + Hash,
{
    pub fn new() -> Self { Self { rules: Vec::new() } }

    pub fn add_rule(&mut self, rule: PeepholeRule<I, IT>) { self.rules.push(rule); }
}

impl<I> PeepholeRunner<I, I>
where
    I: MInst + Hash,
{
    pub fn run(&self, mctx: &mut MContext<I>, config: &LowerConfig) -> bool {
        let mut reg_def_use = RegDefUse::compute(mctx, config);

        let mut changed = false;

        let funcs = mctx
            .funcs
            .iter_mut()
            .map(|(_, func_data)| func_data.self_ptr())
            .collect::<Vec<_>>();

        for func in funcs {
            if func.is_external(mctx) {
                continue;
            }

            let mut cursor = func.cursor();
            while let Some(block) = cursor.next(mctx) {
                let mut curr_inst = block.head(mctx);
                while let Some(inst) = curr_inst {
                    curr_inst = inst.next(mctx);

                    for rule in &self.rules {
                        if (rule.rewriter)(mctx, &mut reg_def_use, inst) {
                            changed = true;
                            break;
                        }
                    }
                }
            }
        }

        changed
    }
}

impl<I> PeepholeRunner<I, (I, I)>
where
    I: MInst + Hash,
{
    pub fn run(&self, mctx: &mut MContext<I>, config: &LowerConfig) -> bool {
        let mut reg_def_use = RegDefUse::compute(mctx, config);

        let mut changed = false;

        let funcs = mctx
            .funcs
            .iter_mut()
            .map(|(_, func_data)| func_data.self_ptr())
            .collect::<Vec<_>>();

        for func in funcs {
            if func.is_external(mctx) {
                continue;
            }

            let mut cursor = func.cursor();
            while let Some(block) = cursor.next(mctx) {
                let mut curr_a = block.head(mctx);
                let mut curr_b = curr_a.and_then(|inst| inst.next(mctx));

                while let Some((inst_a, inst_b)) = curr_a.zip(curr_b) {
                    curr_a = curr_b;
                    curr_b = curr_b.and_then(|inst| inst.next(mctx));

                    let mut local_changed = false;

                    for rule in &self.rules {
                        if (rule.rewriter)(mctx, &mut reg_def_use, (inst_a, inst_b)) {
                            local_changed = true;
                            break;
                        }
                    }

                    if local_changed {
                        // inst_a/inst_b might be removed, so we move forward again.
                        curr_a = curr_b;
                        curr_b = curr_b.and_then(|inst| inst.next(mctx));
                    }

                    changed |= local_changed;
                }
            }
        }

        changed
    }
}

impl<I> PeepholeRunner<I, (I, I, I)>
where
    I: MInst + Hash,
{
    pub fn run(&self, mctx: &mut MContext<I>, config: &LowerConfig) -> bool {
        let mut reg_def_use = RegDefUse::compute(mctx, config);

        let mut changed = false;

        let funcs = mctx
            .funcs
            .iter_mut()
            .map(|(_, func_data)| func_data.self_ptr())
            .collect::<Vec<_>>();

        for func in funcs {
            if func.is_external(mctx) {
                continue;
            }

            let mut cursor = func.cursor();
            while let Some(block) = cursor.next(mctx) {
                let mut curr_a = block.head(mctx);
                let mut curr_b = curr_a.and_then(|inst| inst.next(mctx));
                let mut curr_c = curr_b.and_then(|inst| inst.next(mctx));

                while let (Some(inst_a), Some(inst_b), Some(inst_c)) = (curr_a, curr_b, curr_c) {
                    curr_a = curr_b;
                    curr_b = curr_c;
                    curr_c = curr_c.and_then(|inst| inst.next(mctx));

                    let mut local_changed = false;

                    for rule in &self.rules {
                        if (rule.rewriter)(mctx, &mut reg_def_use, (inst_a, inst_b, inst_c)) {
                            local_changed = true;
                            break;
                        }
                    }

                    if local_changed {
                        // inst_a/inst_b/inst_c might be removed, so we move forward again.
                        curr_a = curr_b;
                        curr_b = curr_c;
                        curr_c = curr_c.and_then(|inst| inst.next(mctx));
                    }

                    changed |= local_changed;
                }
            }
        }

        changed
    }
}
