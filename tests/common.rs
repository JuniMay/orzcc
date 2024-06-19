use orzcc::{
    collections::storage::{ArenaAlloc, ArenaPtr, BaseArena, BaseArenaPtr},
    impl_arena,
    utils::cfg::{CfgNode, CfgRegion},
};

#[derive(Default)]
pub struct CfgContext {
    blocks: BaseArena<CfgBlockData>,
    funcs: BaseArena<CfgFuncData>,
}

pub struct CfgBlockData {
    succs: Vec<CfgBlock>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CfgBlock(BaseArenaPtr<CfgBlockData>);

impl_arena!(CfgContext, CfgBlockData, CfgBlock, blocks);

pub struct CfgFuncData {
    entry: CfgBlock,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CfgFunc(BaseArenaPtr<CfgFuncData>);

impl_arena!(CfgContext, CfgFuncData, CfgFunc, funcs);

impl CfgNode for CfgBlock {
    type Region = CfgFunc;

    fn succs(self, arena: &Self::A) -> Vec<Self> { self.deref(arena).succs.clone() }
}

impl CfgRegion for CfgFunc {
    type Node = CfgBlock;

    fn entry_node(self, arena: &Self::A) -> Self::Node { self.deref(arena).entry }
}

impl CfgFunc {
    pub fn new(arena: &mut CfgContext, entry: CfgBlock) -> Self {
        arena.alloc(CfgFuncData { entry })
    }
}

impl CfgBlock {
    pub fn new(arena: &mut CfgContext) -> Self { arena.alloc(CfgBlockData { succs: Vec::new() }) }

    pub fn add_succ(self, arena: &mut CfgContext, succ: CfgBlock) {
        self.deref_mut(arena).succs.push(succ);
    }
}
