use std::collections::{HashMap, HashSet, VecDeque};

use nalgebra::{DMatrix, DVector};

use super::CfgCanonicalize;
use crate::{
    collections::linked_list::{LinkedListContainerPtr, LinkedListNodePtr},
    ir::{
        passman::{GlobalPassMut, LocalPassMut, PassResult, TransformPass},
        Block,
        Context,
        Func,
        IBinaryOp,
        ICmpCond,
        InstKind,
    },
    utils::{
        cfg::{CfgInfo, CfgNode},
        dominance::Dominance,
        loop_info::LoopContext,
    },
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Chain {
    chain: VecDeque<Block>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ChainId(usize);

#[derive(Default, Debug, Clone)]
pub struct ChainGraph {
    pub chains: Vec<Chain>,
    pub block_map: HashMap<Block, ChainId>,
}

impl ChainGraph {
    pub fn add_chain(&mut self, chain: Chain) -> ChainId {
        let id = ChainId(self.chains.len());
        for block in chain.chain.iter() {
            self.block_map.insert(*block, id);
        }
        self.chains.push(chain);
        id
    }

    pub fn get_chain(&self, id: ChainId) -> &Chain { &self.chains[id.0] }

    pub fn get_chain_mut(&mut self, id: ChainId) -> &mut Chain { &mut self.chains[id.0] }

    pub fn get_chain_id(&self, block: Block) -> Option<ChainId> {
        self.block_map.get(&block).copied()
    }

    pub fn get_chain_from_block(&self, block: Block) -> Option<&Chain> {
        self.get_chain_id(block).map(|id| &self.chains[id.0])
    }

    /// try to merge two chains
    /// only if the last block of the from chain is the same as the first block
    /// of the to chain, or vice versa, the two chains can be merged
    /// returns true if the chains are merged
    pub fn try_merge_chains(&mut self, from: Block, to: Block) -> bool {
        if from == to {
            return false;
        }
        let from_chain_id = self.get_chain_id(from).unwrap();
        let to_chain_id = self.get_chain_id(to).unwrap();
        if self.get_chain(from_chain_id).chain.is_empty()
            || self.get_chain(to_chain_id).chain.is_empty()
        {
            return false;
        }
        if to != *self.get_chain(to_chain_id).chain.front().unwrap() {
            return false;
        }
        if from != *self.get_chain_mut(from_chain_id).chain.back().unwrap() {
            return false;
        }
        if from_chain_id == to_chain_id {
            return false;
        }
        // merge chains
        println!(
            "[ ph_layout ] merging chain {:?} and chain {:?}",
            from_chain_id, to_chain_id
        );
        for block in self.get_chain(to_chain_id).chain.clone().iter() {
            self.get_chain_mut(from_chain_id).chain.push_back(*block);
            self.block_map.insert(*block, from_chain_id);
        }
        self.get_chain_mut(to_chain_id).chain.clear();
        true
    }
}

#[derive(Debug, Clone)]
pub struct Edge {
    pub from: Block,
    pub to: Block,
    pub weight: f64,
}

pub const PH_BLOCK_LAYOUT: &str = "ph-block-layout";

#[derive(Default)]
pub struct PHBlockLayout {
    loops: LoopContext<Block>,
}

impl PHBlockLayout {
    pub fn estimate_branch_prob(&self, ctx: &Context, block: Block) -> (f64, f64) {
        let succs = block.succs(ctx);
        if succs.len() != 2 {
            panic!("block has more than 2 successors");
        }

        if let Some(lp) = self.loops.get_loop(block) {
            let header = lp.header(&self.loops);

            if header == block {
                // the header has greater possibility to jump into the loop.
                assert_eq!(succs.len(), 2);
                if self.loops.is_in_loop(succs[0], lp) && !self.loops.is_in_loop(succs[1], lp) {
                    return (0.99, 0.01);
                } else if self.loops.is_in_loop(succs[1], lp)
                    && !self.loops.is_in_loop(succs[0], lp)
                {
                    return (0.01, 0.99);
                }
            } else {
                // the block is inside the loop, but not the header, we have
                // several heuristics
                // 1. one successor is the header, the other is not (but inside the loop), we
                //    have little possibility to jump back to the header, because this is
                //    typically a continue.
                // 2. one successor is outside the loop, the other is inside the loop, this is
                //    typically a break. We assign greater possibility to stay in the loop.

                // rule 1
                if succs[0] == header && succs[1] != header && self.loops.is_in_loop(succs[1], lp) {
                    return (0.2, 0.8);
                } else if succs[1] == header
                    && succs[0] != header
                    && self.loops.is_in_loop(succs[0], lp)
                {
                    return (0.8, 0.2);
                }

                // rule 2
                if !self.loops.is_in_loop(succs[0], lp) && self.loops.is_in_loop(succs[1], lp) {
                    // stay in the loop
                    return (0.2, 0.8);
                } else if !self.loops.is_in_loop(succs[1], lp)
                    && self.loops.is_in_loop(succs[0], lp)
                {
                    return (0.8, 0.2);
                }
            }
        }

        // the block is not inside any loops, several heuristics:
        // 1. one successor is a header of a loop, the other is not, the one that is in
        //    the loop has greater possibility to be taken
        if let Some(lp) = self.loops.get_loop(succs[0]) {
            if lp.header(&self.loops) == succs[0] && succs[0] != succs[1] {
                // backedge, mostly, or a pseudo preheader, greater possibility
                // to be taken
                return (0.99, 0.01);
            }
        }

        if let Some(lp) = self.loops.get_loop(succs[1]) {
            if lp.header(&self.loops) == succs[1] && succs[0] != succs[1] {
                return (0.01, 0.99);
            }
        }

        // 2. smaller possibility to jump to the return block
        if let Some(return_block) = block.container(ctx).unwrap().tail(ctx) {
            if succs[0] == return_block && succs[1] != return_block {
                return (0.01, 0.99);
            } else if succs[1] == return_block && succs[0] != return_block {
                return (0.99, 0.01);
            }
        }

        let br_inst = block.tail(ctx).unwrap();
        if !br_inst.is_br(ctx) {
            panic!("block tail is not a br");
        }
        let cond = br_inst.operand(ctx, 0);
        if let Some(cond_def) = cond.def_inst(ctx) {
            if let InstKind::IBinary(IBinaryOp::Cmp(cond)) = cond_def.kind(ctx) {
                if *cond == ICmpCond::Ne {
                    return (0.9, 0.1);
                } else if *cond == ICmpCond::Eq {
                    return (0.1, 0.9);
                }
            }
        }

        // regard the first successor to be taken
        (0.6, 0.4)
    }
}
impl LocalPassMut for PHBlockLayout {
    type Output = ();

    fn run(&mut self, ctx: &mut Context, func: Func) -> PassResult<(Self::Output, bool)> {
        let cfg = CfgInfo::new(ctx, func);
        let dominance = Dominance::new(ctx, &cfg);

        self.loops = LoopContext::new(&cfg, &dominance);

        // abort if the function is too large
        let n_blocks = func.iter(ctx).count();
        if n_blocks > 1000 {
            return Ok(((), false));
        }

        // construct markov chain
        let mut mat = DMatrix::<f64>::identity(n_blocks, n_blocks);

        // map from block to index
        let index_map: HashMap<_, _> = func
            .iter(ctx)
            .enumerate()
            .map(|(index, block)| (block, index))
            .collect();

        for block in func.iter(ctx) {
            let index = index_map[&block];
            let succs = block.succs(ctx);
            if succs.len() > 2 {
                panic!("block has more than 2 successors");
            } else if succs.len() == 2 {
                let succ1_index = index_map[&succs[0]];
                let succ2_index = index_map[&succs[1]];

                let (p1, p2) = self.estimate_branch_prob(ctx, block);
                mat[(index, succ1_index)] -= p1;
                mat[(index, succ2_index)] -= p2;
            } else if succs.len() == 1 {
                let succ_index = index_map[&block.succs(ctx)[0]];
                mat[(index, succ_index)] -= 1.0;
            }
        }
        mat[(n_blocks - 1, 0)] -= 1.0;

        // println!(
        //     "[ ph_layout ] Func {:?}: transition matrix: {:}",
        //     func.name(ctx),
        //     mat
        // );

        let mat_a = mat.transpose().insert_row(n_blocks, 1.0);

        // println!(
        //     "[ ph_layout ] Func {:?}: matrix A: {:}",
        //     func.name(ctx),
        //     mat_a
        // );

        // solve for stationary distribution
        let decomp = mat_a.svd(true, true);

        let mut b = DVector::<f64>::zeros(n_blocks + 1);
        b[n_blocks] = 1.0;

        let stationary = decomp.solve(&b, 1e-5);

        if stationary.is_err() {
            println!("[ ph_layout ] stationary distribution not found");
            return Ok(((), false));
        }

        let stationary_norm = stationary.unwrap();

        println!(
            "[ ph_layout ] Func {:?}: stationary distribution: {:}",
            func.name(ctx),
            stationary_norm
        );

        let mut chain_graph = ChainGraph::default();
        let mut edges = Vec::new();
        // construct map
        for block in func.iter(ctx) {
            let chain = Chain {
                chain: VecDeque::from(vec![block]),
            };
            chain_graph.add_chain(chain);
        }

        for block in func.iter(ctx) {
            let succ = block.succs(ctx);
            let index = index_map[&block];
            if succ.len() == 2 {
                let (p1, p2) = self.estimate_branch_prob(ctx, block);
                let succ1 = succ[0];
                if succ1 != block {
                    let edge = Edge {
                        from: block,
                        to: succ1,
                        weight: stationary_norm[index] * p1,
                    };
                    edges.push(edge);
                }
                let succ2 = succ[1];
                if succ2 != block {
                    let edge = Edge {
                        from: block,
                        to: succ2,
                        weight: stationary_norm[index] * p2,
                    };
                    edges.push(edge);
                }
            } else if succ.len() == 1 {
                let succ1 = succ[0];
                if succ1 != block {
                    let edge = Edge {
                        from: block,
                        to: succ1,
                        weight: stationary_norm[index],
                    };
                    edges.push(edge);
                }
            }
        }

        // sort edges descending by weight
        edges.sort_by(|a, b| b.weight.partial_cmp(&a.weight).unwrap());

        let mut left_edges = Vec::new();
        // merge chains
        for edge in edges.iter() {
            if !chain_graph.try_merge_chains(edge.from, edge.to) {
                left_edges.push(edge.clone());
            }
        }

        // println!(
        //     "[ ph_layout ] Func {:?}: chain graph: {:?}",
        //     func.name(ctx),
        //     chain_graph.chains
        // );

        let mut visited_chains = HashSet::new();
        let mut final_arrangement = Vec::new();
        for block in func.iter(ctx) {
            let chain = chain_graph.get_chain_from_block(block).unwrap();
            if !visited_chains.contains(chain) {
                visited_chains.insert(chain);
                for block in chain.chain.iter() {
                    final_arrangement.push(*block);
                }
            }
        }

        // println!(
        //     "[ ph_layout ] Func {:?}: final arrangement: {:?}",
        //     func.name(ctx),
        //     final_arrangement
        // );

        let return_block = func.tail(ctx).unwrap();

        // reorder blocks

        for block in final_arrangement.iter() {
            block.unlink(ctx);
        }

        for block in final_arrangement.iter() {
            if *block == return_block {
                continue;
            }
            func.push_back(ctx, *block);
        }

        func.push_back(ctx, return_block);

        Ok(((), false))
    }
}

impl GlobalPassMut for PHBlockLayout {
    type Output = ();

    fn run(&mut self, ctx: &mut Context) -> PassResult<(Self::Output, bool)> {
        let mut changed = false;
        for func in ctx.funcs() {
            let (_, local_changed) = LocalPassMut::run(self, ctx, func).unwrap();
            changed |= local_changed;
        }
        Ok(((), changed))
    }
}

impl TransformPass for PHBlockLayout {
    fn register(passman: &mut crate::ir::passman::PassManager)
    where
        Self: Sized,
    {
        passman.register_transform(
            PH_BLOCK_LAYOUT,
            PHBlockLayout::default(),
            vec![Box::new(CfgCanonicalize)],
        );
    }
}
