use std::collections::{HashMap, HashSet};

use super::{Context, Func, InstKind, Value};
use crate::collections::linked_list::LinkedListContainerPtr;

#[derive(Default, Clone)]
pub struct FunctionAnalysisResult {
    /// Whether this function is pure (i.e., has no side effects).
    is_pure: bool,
    /// If this is a load-only operation.
    is_partial: bool,
    /// TODO: Global values that may be clobbered by this function.
    _may_clobber: Vec<Value>,
    /// TODO: Global values that may be read by this function.
    _may_read: Vec<Value>,
}

impl FunctionAnalysisResult {
    pub fn new() -> Self { Self::default() }

    pub fn is_pure(&self) -> bool { self.is_pure }

    pub fn is_partial(&self) -> bool { self.is_partial }

    pub fn may_clobber(&self) -> &[Value] {
        unimplemented!("May clobber information is not implemented")
    }

    pub fn may_read(&self) -> &[Value] { unimplemented!("May read information is not implemented") }
}

#[derive(Default)]
pub struct FunctionAnalysis {
    result: HashMap<Func, FunctionAnalysisResult>,
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

    pub fn analyze_func(&mut self, ctx: &Context, func: Func) -> FunctionAnalysisResult {
        if let Some(result) = self.result.get(&func) {
            return result.clone();
        }

        if self.visited.contains(&func) {
            // If we are already visiting this function, but no result is
            // available, it means that there is a mutual recursion.
            // In this case, we assume that the function is impure.
            return FunctionAnalysisResult {
                is_pure: false,
                is_partial: false,
                _may_clobber: vec![],
                _may_read: vec![],
            };
        }

        // Mark the function as visited.
        self.visited.insert(func);

        let mut result = FunctionAnalysisResult::new();
        result.is_pure = true;
        result.is_partial = true;

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
                            None => FunctionAnalysisResult {
                                is_pure: false,
                                is_partial: false,
                                _may_clobber: vec![],
                                _may_read: vec![],
                            },
                        };
                        // If the called function is not pure, then this function is not pure.
                        if !called_func_result.is_pure() {
                            result.is_pure = false;
                        }
                        if !called_func_result.is_partial() {
                            result.is_partial = false;
                        }
                    }
                    InstKind::Store | InstKind::StoreElem { .. } => {
                        // If there is a store instruction, then this function is not pure.
                        // TODO: Check if the store instruction is to a global variable.
                        result.is_pure = false;
                        result.is_partial = false;
                        break 'outer;
                    }
                    InstKind::Load | InstKind::LoadElem { .. } => {
                        // If there is a load instruction, then this function is not pure.
                        // TODO: Check if the load instruction is from a global variable.
                        result.is_pure = false;
                    }
                    InstKind::CallIndirect(_) => {
                        // If there is an indirect call, then this function is not pure.
                        result.is_pure = false;
                        result.is_partial = false;
                        break 'outer;
                    }
                    // TODO: Handle other instructions.
                    InstKind::Undef
                    | InstKind::IConst(_)
                    | InstKind::FConst(_)
                    | InstKind::StackSlot(_)
                    | InstKind::IBinary(_)
                    | InstKind::FBinary(_)
                    | InstKind::IUnary(_)
                    | InstKind::FUnary(_)
                    | InstKind::Cast(_)
                    | InstKind::Offset
                    | InstKind::Jump
                    | InstKind::Br
                    | InstKind::Ret
                    | InstKind::GetGlobal(_) => {}
                }
            }
        }

        self.result.insert(func, result.clone());

        result
    }

    pub fn result(&self, func: Func) -> &FunctionAnalysisResult { self.result.get(&func).unwrap() }

    pub fn is_pure(&self, func: Func) -> bool { self.result(func).is_pure() }

    pub fn is_partial(&self, func: Func) -> bool { self.result(func).is_partial() }
}
