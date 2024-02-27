//! # Dominance Analysis
//!
//! This module contains the implementation of the dominance analysis pass.
//!
//! This implements the algorithm described in "A Simple, Fast Dominance Algorithm" by Cooper et al.
//!

use std::collections::{HashMap, HashSet};

use thiserror::Error;

use crate::{
    ir::{
        entities::FunctionData,
        pass::LocalPass,
        values::{Block, Function},
    },
    passes::control_flow_analysis::ControlFlowAnalysis,
};

use super::control_flow_analysis::{ControlFlowAnalysisError, ControlFlowGraph};

/// The result of the dominance analysis pass.
pub struct Dominance {
    /// The immediate dominator of each block.
    pub idoms: HashMap<Block, Option<Block>>,
    /// The dominance frontier of each block.
    pub frontiers: HashMap<Block, Vec<Block>>,
    /// The dominator tree.
    pub domtree: HashMap<Block, Vec<Block>>,
}

impl Dominance {
    pub fn new() -> Self {
        Self {
            idoms: HashMap::new(),
            frontiers: HashMap::new(),
            domtree: HashMap::new(),
        }
    }
}

#[derive(Debug, Error)]
pub enum DominanceAnalysisError {
    #[error(transparent)]
    ControlFlowAnalysisError(#[from] ControlFlowAnalysisError),
}

pub struct DominanceAnalysis {
    /// Postorder traversal numbers of the blocks.
    postorder: HashMap<Block, usize>,

    /// Reverse postorder traversal sequence.
    rpo: Vec<Block>,

    /// Visited set for the DFS.
    visited: HashSet<Block>,

    /// Counter for postorder traversal.
    postorder_counter: usize,
}

impl DominanceAnalysis {
    pub fn new() -> Self {
        Self {
            postorder: HashMap::new(),
            rpo: Vec::new(),
            visited: HashSet::new(),
            postorder_counter: 0,
        }
    }

    fn dfs(&mut self, block: Block, cfg: &ControlFlowGraph) {
        self.visited.insert(block);
        for succ in cfg.succ(&block).unwrap() {
            if !self.visited.contains(succ) {
                self.dfs(*succ, cfg);
            }
        }
        self.rpo.push(block);
        self.postorder.insert(block, self.postorder_counter);
        self.postorder_counter += 1;
    }

    fn prepare(&mut self, cfg: &ControlFlowGraph, data: &FunctionData) {
        let entry_block = data.layout().entry_block().unwrap();
        self.dfs(entry_block, cfg);
        // reverse the postorder traversal sequence
        self.rpo.reverse();
    }

    fn intersect(&self, b1: Block, b2: Block, idoms: &HashMap<Block, Option<Block>>) -> Block {
        let mut finger1 = b1;
        let mut finger2 = b2;
        while finger1 != finger2 {
            while self.postorder[&finger1] < self.postorder[&finger2] {
                finger1 = idoms[&finger1].unwrap();
            }
            while self.postorder[&finger2] < self.postorder[&finger1] {
                finger2 = idoms[&finger2].unwrap();
            }
        }
        finger1
    }
}

impl LocalPass for DominanceAnalysis {
    type Ok = Dominance;
    type Err = DominanceAnalysisError;

    fn run(&mut self, function: Function, data: &FunctionData) -> Result<Self::Ok, Self::Err> {
        let mut cfa = ControlFlowAnalysis {};
        let cfg = cfa.run(function, data)?;
        self.prepare(&cfg, data);

        let mut idoms = HashMap::new();

        for block in self.rpo.iter() {
            idoms.insert(*block, None);
        }

        let entry_block = data.layout().entry_block().unwrap();
        idoms.insert(entry_block, Some(entry_block));

        let mut changed = true;
        while changed {
            changed = false;
            for block in self.rpo.iter() {
                // first processed predecessor of the block
                let mut new_idom = None;
                for pred in cfg.pred(&block).unwrap() {
                    if pred == block {
                        continue;
                    }
                    if let Some(_) = idoms.get(pred).unwrap() {
                        new_idom = Some(*pred);
                        break;
                    }
                }
                if !new_idom.is_some() {
                    continue;
                }
                for pred in cfg.pred(&block).unwrap() {
                    if let Some(_) = idoms.get(pred).unwrap() {
                        new_idom = Some(self.intersect(new_idom.unwrap(), *pred, &idoms));
                    }
                }
                if idoms.get(block).unwrap() != &new_idom {
                    idoms.insert(*block, new_idom);
                    changed = true;
                }
            }
        }

        idoms.insert(entry_block, None);

        let mut domtree: HashMap<Block, Vec<Block>> = HashMap::new();
        for block in self.rpo.iter() {
            domtree.insert(*block, Vec::new());
        }

        // construct the dometree
        for (block, idom) in idoms.iter() {
            if let Some(idom) = idom {
                domtree.get_mut(idom).unwrap().push(*block);
            }
        }

        let mut frontiers = HashMap::new();

        // calculate the dominance frontiers
        for block in self.rpo.iter() {
            frontiers.insert(*block, Vec::new());
        }

        for block in self.rpo.iter() {
            if cfg.pred(&block).unwrap().len() >= 2 {
                for pred in cfg.pred(&block).unwrap() {
                    let mut runner = *pred;
                    while runner != idoms[&block].unwrap() {
                        frontiers.get_mut(&runner).unwrap().push(*block);
                        runner = idoms[&runner].unwrap();
                    }
                }
            }
        }

        Ok(Dominance {
            idoms,
            frontiers,
            domtree,
        })
    }
}

#[cfg(test)]
mod test {
    use std::io::Cursor;

    use crate::ir::{frontend::parser::Parser, pass::LocalPass};

    use super::DominanceAnalysis;

    #[test]
    fn test_dominance_analysis() {
        let ir = r#"
            func @test_func() -> void {
                ^5:
                    %0 = add i32 1, i32 2
                    %cond = icmp.sle i32 %0, i32 0x0
                    br %cond, ^4, ^3
                ^1:
                    jump ^2
                ^2:
                    jump ^1
                ^3:
                    jump ^2
                ^4:
                    jump ^1
            }
        "#;

        let mut buf = Cursor::new(ir);
        let mut parser = Parser::new(&mut buf);
        let mut module = parser.parse().unwrap().into_ir("test".into()).unwrap();

        let function = module.get_value_by_name("@test_func").unwrap();
        let function_data = module.function_data_mut(function.into()).unwrap();

        let mut dominance_analysis = DominanceAnalysis::new();

        let dominance = dominance_analysis
            .run(function.into(), function_data)
            .unwrap();

        let idoms = dominance.idoms;
        let frontiers = dominance.frontiers;
        let _domtree = dominance.domtree;

        let b5 = function_data.dfg().get_block_by_name("^5").unwrap();
        let b4 = function_data.dfg().get_block_by_name("^4").unwrap();
        let b3 = function_data.dfg().get_block_by_name("^3").unwrap();
        let b2 = function_data.dfg().get_block_by_name("^2").unwrap();
        let b1 = function_data.dfg().get_block_by_name("^1").unwrap();

        assert_eq!(idoms[&b5], None);
        assert_eq!(idoms[&b4], Some(b5));
        assert_eq!(idoms[&b3], Some(b5));
        assert_eq!(idoms[&b2], Some(b5));
        assert_eq!(idoms[&b1], Some(b5));

        assert_eq!(frontiers[&b5], vec![]);
        assert_eq!(frontiers[&b4], vec![b1]);
        assert_eq!(frontiers[&b3], vec![b2]);
        assert_eq!(frontiers[&b2], vec![b1]);
        assert_eq!(frontiers[&b1], vec![b2]);
    }
}
