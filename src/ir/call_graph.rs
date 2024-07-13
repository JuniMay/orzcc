use rustc_hash::{FxHashMap, FxHashSet};

use super::{Context, Func, InstKind, Symbol, SymbolKind};
use crate::collections::linked_list::LinkedListContainerPtr;

pub struct CallGraph {
    calls: FxHashMap<Func, FxHashSet<Func>>,
    call_decls: FxHashMap<Func, Vec<Symbol>>,
    call_indirects: FxHashSet<Func>,
}

impl CallGraph {
    pub fn from_ir(ctx: &Context) -> Self {
        let mut calls = FxHashMap::default();
        let mut call_decls = FxHashMap::default();
        let mut call_indirects = FxHashSet::default();

        for func in ctx.funcs() {
            for block in func.iter(ctx) {
                for inst in block.iter(ctx) {
                    if let InstKind::Call(sym) = inst.kind(ctx) {
                        match ctx.lookup_symbol(sym).unwrap() {
                            SymbolKind::FuncDef(func) => {
                                calls
                                    .entry(*func)
                                    .or_insert(FxHashSet::default())
                                    .insert(*func);
                            }
                            SymbolKind::FuncDecl(_) => {
                                call_decls
                                    .entry(func)
                                    .or_insert(Vec::new())
                                    .push(sym.clone());
                            }
                            SymbolKind::GlobalSlot(_) => unreachable!(),
                        }
                    } else if let InstKind::CallIndirect(_) = inst.kind(ctx) {
                        call_indirects.insert(func);
                    }
                }
            }
        }

        Self {
            calls,
            call_decls,
            call_indirects,
        }
    }

    pub fn has_indirect_call(&self, func: Func) -> bool { self.call_indirects.contains(&func) }

    pub fn decl_calls(&self, func: Func) -> &[Symbol] { self.call_decls.get(&func).unwrap() }

    /// Calculate the SCCs in the call graph and return the func -> SCC ID
    /// mapping
    ///
    /// TODO: Test this
    pub fn compute_sccs(&self) -> FxHashMap<Func, usize> {
        let mut sccs = Vec::new();
        let mut index = 0;
        let mut stack = Vec::new();
        let mut indices = FxHashMap::default();
        let mut lowlinks = FxHashMap::default();
        let mut on_stack = FxHashSet::default();
        let mut func_to_scc = FxHashMap::default();

        #[allow(clippy::too_many_arguments)]
        fn strongconnect(
            func: Func,
            calls: &FxHashMap<Func, FxHashSet<Func>>,
            index: &mut usize,
            stack: &mut Vec<Func>,
            indices: &mut FxHashMap<Func, usize>,
            lowlinks: &mut FxHashMap<Func, usize>,
            on_stack: &mut FxHashSet<Func>,
            sccs: &mut Vec<FxHashSet<Func>>,
            func_to_scc: &mut FxHashMap<Func, usize>,
        ) {
            indices.insert(func, *index);
            lowlinks.insert(func, *index);
            *index += 1;
            stack.push(func);
            on_stack.insert(func);

            for callee in calls.get(&func).unwrap() {
                if !indices.contains_key(callee) {
                    strongconnect(
                        *callee,
                        calls,
                        index,
                        stack,
                        indices,
                        lowlinks,
                        on_stack,
                        sccs,
                        func_to_scc,
                    );
                    lowlinks.insert(func, lowlinks[&func].min(lowlinks[callee]));
                } else if on_stack.contains(callee) {
                    lowlinks.insert(func, lowlinks[&func].min(indices[callee]));
                }
            }

            if lowlinks[&func] == indices[&func] {
                let mut scc = FxHashSet::default();
                loop {
                    let callee = stack.pop().unwrap();
                    on_stack.remove(&callee);
                    scc.insert(callee);
                    func_to_scc.insert(callee, sccs.len());
                    if callee == func {
                        break;
                    }
                }
                sccs.push(scc);
            }
        }

        for func in self.calls.keys() {
            if !indices.contains_key(func) {
                strongconnect(
                    *func,
                    &self.calls,
                    &mut index,
                    &mut stack,
                    &mut indices,
                    &mut lowlinks,
                    &mut on_stack,
                    &mut sccs,
                    &mut func_to_scc,
                );
            }
        }

        func_to_scc
    }
}
