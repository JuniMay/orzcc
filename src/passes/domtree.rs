///to be modify

use crate::ir::{
    layout::{BlockNode, Layout, BlockList},
    values::{Block, Inst}
}

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Clone, Debug)]
pub struct DomTree {
    pub root: Option<Block>,
    pub tree: HashMap<Block, HashSet<Block>>,
    pub frontier: HashMap<Block, HashSet<Block>>,
    pub level: HashMap<Block, usize>,
}

pub struct DomTreeConstructor {
    layout: Rc<RefCell<Layout>>,
    tree: DomTree,
    dfnum: HashMap<Block, usize>,
    vertex: Vec<Block>,
    semi: HashMap<Block, Block>,
    ancestor: HashMap<Block, Block>,
    idom: HashMap<Block, Block>,
    samedom: HashMap<Block, Block>,
    parent: HashMap<Block, Block>,
    best: HashMap<Block, Block>,
}

impl DomTreeConstructor {
    pub fn new(layout: Rc<RefCell<Layout>>) -> Self {
        Self {
            layout,
            tree: DomTree::new(),
            dfnum: HashMap::default(),
            vertex: Vec::new(),
            semi: HashMap::default(),
            ancestor: HashMap::default(),
            idom: HashMap::default(),
            samedom: HashMap::default(),
            parent: HashMap::default(),
            best: HashMap::default(),
        }
    }

    pub fn construct(&mut self) {
        let entry = self.layout.borrow().entry_block().expect("Entry block must exist");
        self.initialize(entry);
        self.calculate_dominance_frontier();
    }

    // 初始化并计算DFS遍历、半支配者、祖先和立即支配者
    fn initialize(&mut self, entry: Block) {
        let mut num = 0;
        self.dfs(entry, None, &mut num);

        // 初始化bucket，用于存储可能的立即支配者
        let mut bucket: HashMap<Block, Vec<Block>> = HashMap::new();

        // 逆向处理顶点，以计算半支配者和立即支配者
        for i in (1..self.vertex.len()).rev() {
            let w = self.vertex[i];
            let mut semi = w; // 默认自己为半支配者

            // STEP 1: 计算半支配者
            for &v in self.get_predecessors(w).iter() {
                let u = self.eval(v);
                if self.dfnum[&u] < self.dfnum[&semi] {
                    semi = u;
                }
            }

            // 将当前节点添加到其半支配者的bucket中
            bucket.entry(self.semi[&semi]).or_default().push(w);

            // 将当前节点链接到其父节点
            self.link(self.parent[&w], w);

            // STEP 2: 计算立即支配者
            let parent = self.parent[&w];
            for &v in bucket.entry(parent).or_default().drain(..) {
                let u = self.eval(v);
                self.idom.insert(v, if self.semi[&u] < self.semi[&v] { u } else { parent });
            }
        }

        // 设置入口节点的立即支配者为自己
        self.idom.insert(entry, entry);
    }


    fn dfs(&mut self, block: Block, parent: Option<Block>, num: &mut usize) {
        self.dfnum.insert(block, *num);
        self.vertex.push(block);
        *num += 1;

        if let Some(p) = parent {
            self.parent.insert(block, p);
        }
        //控制流待实现
        for &succ in self.get_successors(block).iter() {
            if !self.dfnum.contains_key(&succ) {
                self.dfs(succ, Some(block), num);
                self.ancestor.insert(succ, block);
            }
        }
    }
    // 计算半支配者
    fn eval(&mut self, v: Block) -> Block {
        if self.ancestor.get(&v).is_some() {
            self.compress(v);
            self.best[v]
        } else {
            v
        }
    }

    // 路径压缩
    fn compress(&mut self, v: Block) {
        let a = self.ancestor[&v];
        if self.ancestor.get(&a).is_some() {
            self.compress(a);
            if self.dfnum[&self.semi[&self.best[&a]]] < self.dfnum[&self.semi[&self.best[&v]]] {
                self.best.insert(v, self.best[&a]);
            }
            self.ancestor.insert(v, self.ancestor[&a]);
        }
    }

    // 链接节点
    fn link(&mut self, v: Block, w: Block) {
        self.ancestor.insert(w, v);
    }

    // 计算支配边界
    fn calculate_dominance_frontier(&mut self) {
        for &block in self.vertex.iter().rev() {
            let mut df = HashSet::new();
            // 遍历所有后继
            for &succ in self.get_successors(block).iter() {
                if self.idom.get(&succ) != Some(&block) {
                    df.insert(succ);
                }
            }
            // 遍历所有子节点
            for &child in self.idom.iter().filter_map(|(&b, &i)| if i == block { Some(b) } else { None }) {
                for &w in self.frontier.get(&child).unwrap_or(&HashSet::new()) {
                    if self.idom.get(&w) != Some(&block) {
                        df.insert(w);
                    }
                }
            }
            self.frontier.insert(block, df);
        }
    }


}

impl DomTree {
    pub fn new() -> Self {
        Self {
            root: None,
            tree: HashMap::new(),
            frontier: HashMap::new(),
            level: HashMap::new(),
        }
    }

    pub fn add_node(&mut self, node: Block, parent: Option<Block>) {
        if let Some(p) = parent {
            self.tree.entry(p).or_insert_with(HashSet::new).insert(node);
            self.level.insert(node, self.level[&p] + 1);
        } else {
            // 如果没有指定父节点，假定它是根节点
            self.root = Some(node);
            self.level.insert(node, 0);
        }
    }
    
    pub fn update_levels(&mut self) {
        if let Some(root) = self.root {
            self.update_level_recursive(root, 0);
        }
    }
    
    fn update_level_recursive(&mut self, node: Block, current_level: usize) {
        self.level.insert(node, current_level);
        if let Some(children) = self.tree.get(&node) {
            for &child in children {
                self.update_level_recursive(child, current_level + 1);
            }
        }
    }
    
    pub fn get_immediate_dominator(&self, node: Block) -> Option<Block> {
        self.idom.get(&node).copied()
    }
    
    pub fn get_dominated_nodes(&self, node: Block) -> Option<&HashSet<Block>> {
        self.tree.get(&node)
    }
    
    pub fn get_dominance_frontier(&self, node: Block) -> Option<&HashSet<Block>> {
        self.frontier.get(&node)
    }
    
}
