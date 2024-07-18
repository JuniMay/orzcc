use std::collections::{HashMap, HashSet};

use super::{Context, Func, InstKind, Value};
use crate::collections::linked_list::LinkedListContainerPtr;

#[derive(Default, Clone)]
pub struct FunctionAnalysisResulf {
    /// Whether this function is pure (i.e., has no side effects).
    is_pure: bool,
    /// TODO: Global values that may be clobbered by this function.
    may_clobber: Vec<Value>,
    /// TODO: Global values that may be read by this function.
    may_read: Vec<Value>,
}

impl FunctionAnalysisResulf {
    pub fn new() -> Self { Self::default() }

    pub fn is_pure(&self) -> bool { self.is_pure }

    pub fn may_clobber(&self) -> &[Value] {
        unimplemented!("May clobber information is not implemented")
    }

    pub fn may_read(&self) -> &[Value] { unimplemented!("May read information is not implemented") }
}

#[derive(Default)]
pub struct FunctionAnalysis {
    result: HashMap<Func, FunctionAnalysisResulf>,
    visited: HashSet<Func>,
}

impl FunctionAnalysis {
    pub fn new() -> Self { Self::default() }

    pub fn analyze_all(&mut self, ctx: &Context) {
        for func in ctx.funcs().iter() {
            if !self.result.contains_key(func) {
                let _ = self.analyze_func(ctx, *func);
            }
        }
    }

    pub fn analyze_func(&mut self, ctx: &Context, func: Func) -> FunctionAnalysisResulf {
        if let Some(result) = self.result.get(&func) {
            return result.clone();
        }

        if self.visited.contains(&func) {
            // If we are already visiting this function, but no result is
            // available, it means that there is a mutual recursion.
            // In this case, we assume that the function is impure.
            return FunctionAnalysisResulf {
                is_pure: false,
                may_clobber: vec![],
                may_read: vec![],
            };
        }

        // Mark the function as visited.
        self.visited.insert(func);

        let mut result = FunctionAnalysisResulf::new();
        result.is_pure = true;

        'outer: for block in func.iter(ctx) {
            for inst in block.iter(ctx) {
                match inst.kind(ctx) {
                    InstKind::Call(symbol) => {
                        // For self-recursive calls, we continue
                        if symbol == func.name(ctx) {
                            continue;
                        }
                        // If the called function is not pure, then this function is not pure.
                        let called_func_result = match ctx.lookup_func(symbol) {
                            // If the function is found, we analyze it.
                            Some(called_func) => self.analyze_func(ctx, called_func),
                            // If the function is not found, (probably an external function), we
                            // assume it is impure.
                            None => FunctionAnalysisResulf {
                                is_pure: false,
                                may_clobber: vec![],
                                may_read: vec![],
                            },
                        };
                        // If the called function is not pure, then this function is not pure.
                        if !called_func_result.is_pure() {
                            result.is_pure = false;
                        }
                    }
                    InstKind::Store => {
                        // If there is a store instruction, then this function is not pure.
                        // TODO: Check if the store instruction is to a global variable.
                        result.is_pure = false;
                        break 'outer;
                    }
                    InstKind::Load => {
                        // If there is a load instruction, then this function is not pure.
                        // TODO: Check if the load instruction is from a global variable.
                        result.is_pure = false;
                        break 'outer;
                    }
                    // TODO: Handle other instructions.
                    _ => {}
                }
            }
        }

        self.result.insert(func, result.clone());

        result
    }

    pub fn result(&self, func: Func) -> &FunctionAnalysisResulf { self.result.get(&func).unwrap() }

    pub fn is_pure(&self, func: Func) -> bool { self.result(func).is_pure() }
}
