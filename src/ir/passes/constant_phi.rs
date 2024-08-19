use std::collections::{HashMap, HashSet};

use super::control_flow::CfgCanonicalize;
use crate::{
    collections::linked_list::LinkedListContainerPtr,
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassManager, PassResult, TransformPass},
        Context,
        Func,
    },
    utils::def_use::{Usable, User},
};

pub const ELIM_CONSTANT_PHI: &str = "elim-constant-phi";

pub struct ElimConstantPhi;

impl LocalPassMut for ElimConstantPhi {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;

        let mut incomings = HashMap::new();

        for block in func.iter(ctx) {
            for param in block.params(ctx) {
                incomings.insert(*param, HashSet::new());
            }

            for user in block.users(ctx) {
                for succ in user.succ_to(ctx, block) {
                    for param in block.params(ctx) {
                        incomings
                            .get_mut(param)
                            .unwrap()
                            .insert(succ.get_arg(*param).unwrap());
                    }
                }
            }
        }

        for (param, incomings) in incomings {
            if incomings.len() == 1 {
                // println!("[ constant phi ] detected");
                let value = *incomings.iter().next().unwrap();
                for user in param.users(ctx) {
                    user.replace(ctx, param, value);
                    changed = true;
                }
            }
        }

        Ok(((), changed))
    }
}

impl GlobalPassMut for ElimConstantPhi {
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

impl TransformPass for ElimConstantPhi {
    fn register(passman: &mut PassManager) {
        passman.register_transform(
            ELIM_CONSTANT_PHI,
            ElimConstantPhi,
            vec![Box::new(CfgCanonicalize)],
        );
    }
}
