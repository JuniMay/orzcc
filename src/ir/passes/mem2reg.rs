//! # Mem2reg Pass for OrzIR
//!
//! This pass will transform the IR to SSA form by promoting memory to register.
//!
//! In LLVM, this promotion will generate phi instructions.
//! ```llvm
//! %dst:
//!     %x0 = phi [%val0, %pred0], [%val1, %pred1], ...
//! ```
//!
//! But here, since we use block arguments insead of phi instructions, the generated code will be
//! like:
//! ```orzir
//! ^pred0:
//!     #...
//!     br %cond, ^dst(%val0), ...
//! ^pred1:
//!     #...
//!     br %cond, ^dst(%val1), ...
//! ^dst(%x0): # type ignored
//!     #...
//! ```
//!
//! The algorithms are similar, except that multiple phis become multiple parameters of the block.
//!

use std::collections::{HashMap, HashSet, VecDeque};

use crate::ir::{
    builders::{BuildLocalValue, BuildNonAggregateConstant},
    entities::{FunctionData, ValueKind},
    module::DataFlowGraph,
    passes::{LocalPass, LocalPassMut},
    types::Type,
    values::{Block, Function, Value},
};

use super::{
    control_flow_analysis::{ControlFlowAnalysis, ControlFlowGraph},
    control_flow_canonicalization::ControlFlowCanonicalization,
    data_flow_analysis::{DataFlowAnalysis, DefUseChain},
    dominance_analysis::{Dominance, DominanceAnalysis},
    GlobalPassMut, PassManager, PassResult, TransformationPass,
};

const MEM2REG: &str = "mem2reg";

pub struct Mem2reg {
    /// Variable set.
    variables: HashSet<Value>,

    /// Def blocks
    def_blocks: HashMap<Value, HashSet<Block>>,

    /// Block worklists
    worklists: HashMap<Value, VecDeque<Block>>,

    /// Incoming values
    incomings: HashMap<Value, Value>,

    /// The inserted map with the param index
    inserted: HashMap<Value, HashSet<Block>>,

    /// Block parameter and corresponding variable
    block_params: HashMap<Value, Value>,

    /// Dominance
    dominance: Dominance,

    /// Control flow graph
    cfg: ControlFlowGraph,

    /// Def-use chains
    chain: DefUseChain,

    /// Types of the alloc instructions
    alloc_types: HashMap<Value, Type>,
}

impl Default for Mem2reg {
    fn default() -> Self {
        Self::new()
    }
}

impl Mem2reg {
    pub fn new() -> Self {
        Self {
            variables: HashSet::new(),
            def_blocks: HashMap::new(),
            worklists: HashMap::new(),
            incomings: HashMap::new(),
            inserted: HashMap::new(),
            block_params: HashMap::new(),

            dominance: Dominance::new(),
            cfg: ControlFlowGraph::new(),
            chain: DefUseChain::new(),

            alloc_types: HashMap::new(),
        }
    }

    pub fn register() {
        let pass = Box::new(Mem2reg::new());
        let canonic = Box::new(ControlFlowCanonicalization {});
        PassManager::register_transformation(MEM2REG, pass, vec![canonic]);
    }

    fn prepare(&mut self, function: Function, data: &FunctionData) {
        let mut dominance_analysis = DominanceAnalysis::new();
        self.dominance = dominance_analysis.run_on_function(function, data).unwrap();

        let mut control_flow_analysis = ControlFlowAnalysis {};
        self.cfg = control_flow_analysis
            .run_on_function(function, data)
            .unwrap();

        let mut data_flow_analysis = DataFlowAnalysis {};
        self.chain = data_flow_analysis.run_on_function(function, data).unwrap();
    }

    fn promotable(&self, value: Value, dfg: &DataFlowGraph) -> Option<Type> {
        let data = dfg.local_value_data(value).unwrap();

        if let ValueKind::Alloc(alloc) = data.kind() {
            let uses = self.chain.uses.get(&value)?;
            for use_ in uses {
                let data = dfg.local_value_data(*use_).unwrap();
                match data.kind() {
                    ValueKind::Store(store) => {
                        let _ptr = store.ptr();
                        let val = store.val();
                        if value == val {
                            // the alloc result is used as a non-ptr value
                            return None;
                        }
                        let ty = dfg.with_value_data(val, |data| data.ty()).unwrap();
                        if ty != alloc.ty() {
                            // different types in load and alloc
                            return None;
                        }
                    }
                    ValueKind::Load(_) => {
                        // the alloc result is used as a ptr in a load
                        if data.ty() != alloc.ty() {
                            // different types in load and alloc
                            return None;
                        }
                    }
                    _ => {
                        // the alloc result is used for other purposes
                        return None;
                    }
                }
            }
            Some(alloc.ty())
        } else {
            None
        }
    }

    fn rename(&mut self, block: Block, data: &mut FunctionData) -> bool {
        let mut changed = false;

        let node = data.layout().blocks().node(block).unwrap();

        let block_data = data.dfg().block_data(block).unwrap();
        let params = block_data.params();
        for param in params {
            if self.block_params.contains_key(param) {
                let variable = self.block_params[param];
                if self.variables.contains(&variable) {
                    self.incomings.insert(variable, *param);
                }
            }
        }

        let mut insts_to_remove: HashSet<Value> = HashSet::new();
        // inst -> (old, new)
        let mut insts_to_replace_use: HashMap<Value, (Value, Value)> = HashMap::new();

        for (inst, _) in node.insts() {
            let inst_data = data.dfg().local_value_data(inst.into()).unwrap();
            match inst_data.kind() {
                ValueKind::Load(load) => {
                    let ptr = load.ptr();
                    if self.variables.contains(&ptr) {
                        let incoming = self.incomings[&ptr];
                        for use_ in self.chain.uses[&inst.into()].iter() {
                            insts_to_replace_use.insert(*use_, (inst.into(), incoming));
                        }
                        insts_to_remove.insert(inst.into());
                    }
                }
                ValueKind::Store(store) => {
                    let ptr = store.ptr();
                    if self.variables.contains(&ptr) {
                        self.incomings.insert(ptr, store.val());
                        insts_to_remove.insert(inst.into());
                    }
                }
                _ => {}
            }
        }

        // edit the branch instructions
        let succs = self.cfg.succs(&block).unwrap();
        for succ in succs {
            // args to extend the branch/jump instructions
            let mut args = Vec::new();
            let succ_data = data.dfg().block_data(*succ).unwrap();
            let params = succ_data.params();
            for param in params {
                // the params for promotion are placed at the end of the block,
                // so just get the extension for the args.
                if self.block_params.contains_key(param) {
                    let variable = self.block_params[param];
                    let incoming = self.incomings[&variable];
                    args.push(incoming);
                }
            }
            let exit_inst = data.layout().exit_inst_of_block(block).unwrap();
            let exit_inst_data = data
                .dfg_mut()
                .local_value_data_mut(exit_inst.into())
                .unwrap();
            match exit_inst_data.kind_mut() {
                ValueKind::Jump(jump) => {
                    jump.extend_args(args);
                }
                ValueKind::Branch(br) => {
                    if br.then_dst() == *succ {
                        br.extend_then_args(args);
                    } else {
                        br.extend_else_args(args);
                    }
                }
                ValueKind::Return(_) => {}
                _ => unreachable!(),
            }
        }

        // replace the uses
        for (use_, (old, new)) in insts_to_replace_use {
            data.dfg_mut().replace_use(use_, old, new);
            changed = true;
        }

        // remove the insts
        for inst in insts_to_remove {
            data.remove_inst(inst.into());
            changed = true;
        }

        // dfs dometree
        let children = self.dominance.domtree.get(&block).unwrap().clone();
        for child in children {
            self.rename(child, data);
        }

        changed
    }
}

impl LocalPassMut for Mem2reg {
    type Ok = ();

    fn run_on_function(
        &mut self,
        function: Function,
        data: &mut FunctionData,
    ) -> PassResult<(Self::Ok, bool)> {
        // prepare the information
        self.prepare(function, data);

        let dfg = data.dfg();

        let mut changed = false;

        // initialize the promotable variables
        for value in dfg.values().keys() {
            if let Some(ty) = self.promotable(*value, dfg) {
                self.variables.insert(*value);
                self.def_blocks.insert(*value, HashSet::new());
                self.worklists.insert(*value, VecDeque::new());
                self.inserted.insert(*value, HashSet::new());
                self.alloc_types.insert(*value, ty);
            }
        }

        let layout = data.layout();

        // construct worklist.
        for (block, block_node) in layout.blocks() {
            for (inst, _inst_block) in block_node.insts() {
                let inst_data = dfg.local_value_data(inst.into()).unwrap();
                if let ValueKind::Store(store) = inst_data.kind() {
                    let ptr = store.ptr();
                    if self.variables.contains(&ptr) && !self.def_blocks[&ptr].contains(&block) {
                        // if this ptr is a to-be-promoted variable,
                        // insert the block to def and worklist
                        self.def_blocks.get_mut(&ptr).unwrap().insert(block);
                        self.worklists.get_mut(&ptr).unwrap().push_back(block);
                    }
                }
            }
        }

        let dfg = data.dfg_mut();

        // insert block parameters
        for (value, worklist) in self.worklists.iter_mut() {
            while !worklist.is_empty() {
                let block = worklist.pop_front().unwrap();
                let frontier = self.dominance.frontiers.get(&block).unwrap();
                for df in frontier {
                    if self.inserted[value].contains(df) {
                        continue;
                    }
                    let ty = self.alloc_types[value].clone();
                    let param = dfg.builder().block_param(ty).unwrap();
                    let df_data = dfg.block_data_mut(*df).unwrap();
                    df_data.params_mut().push(param);

                    // map param -> value (allocated variable)
                    self.block_params.insert(param, *value);
                    self.inserted.get_mut(value).unwrap().insert(*df);

                    if !self.def_blocks[value].contains(df) {
                        worklist.push_back(*df);
                    }
                }
            }
        }

        for variable in self.variables.iter() {
            let undef = data
                .dfg_mut()
                .builder()
                .undef(self.alloc_types[variable].clone())
                .unwrap();
            self.incomings.insert(*variable, undef);
        }

        // rename the variables
        let entry_block = data.layout().entry_block().unwrap();
        changed = changed || self.rename(entry_block, data);

        for variable in self.variables.iter() {
            data.remove_inst((*variable).into());
            changed = true;
        }

        Ok(((), changed))
    }
}

impl TransformationPass for Mem2reg {
    fn reset(&mut self) {
        self.variables.clear();
        self.def_blocks.clear();
        self.worklists.clear();
        self.incomings.clear();
        self.inserted.clear();
        self.block_params.clear();
        self.dominance = Dominance::new();
        self.cfg = ControlFlowGraph::new();
        self.chain = DefUseChain::new();
        self.alloc_types.clear();
    }
}

impl GlobalPassMut for Mem2reg {
    type Ok = ();

    fn run_on_module(
        &mut self,
        module: &mut crate::ir::module::Module,
    ) -> PassResult<(Self::Ok, bool)> {
        let functions = module.function_layout().to_vec();
        let mut changed = false;
        for function in functions {
            let function_data = module.function_data_mut(function).unwrap();
            let (_, local_changed) = self.run_on_function(function, function_data).unwrap();
            changed = changed || local_changed;
        }

        Ok(((), changed))
    }
}

#[cfg(test)]
mod test {
    use std::io::{BufWriter, Cursor};

    use crate::ir::{
        frontend::parser::Parser,
        module::Module,
        passes::{printer::Printer, GlobalPass, PassManager},
    };

    use super::{Mem2reg, MEM2REG};

    fn print(module: &Module) {
        let mut buf = BufWriter::new(Vec::new());
        let mut printer = Printer::new(&mut buf);
        printer.run_on_module(module).unwrap();
        let s = String::from_utf8(buf.into_inner().unwrap()).unwrap();
        println!("{}", s);
    }

    #[test]
    fn test_mem2reg() {
        let ir = r#"
            func @test_mem2reg() -> i32 {
            ^entry(i32 %arg0):
                %0 = alloc i32
                %1 = alloc float
                %3 = alloc [i16; 10]
                %4 = alloc i32

                %cond = icmp.slt i32 0, i32 %arg0
                br i1 %cond, ^positive, ^negative
                
            ^positive:
                store i32 1, %0
                jump ^ret

            ^negative:
                store i32 0xffffffff, %0
                jump ^ret

            ^ret:
                %ret_val = load i32, %0
                ret i32 %ret_val
            }
        "#;

        let mut buf = Cursor::new(ir);
        let mut parser = Parser::new(&mut buf);
        let mut module = parser.parse().unwrap().into_ir("test".into()).unwrap();

        Mem2reg::register();
        let iter = PassManager::run_transformation(MEM2REG, &mut module, 1234);

        assert_eq!(iter, 2);
        print(&module);
    }
}
