use rustc_hash::FxHashMap;

use super::{Block, Value};

/// The mapping of values and blocks from the original to the cloned.
#[derive(Default)]
pub struct DeepCloneMap {
    value_map: FxHashMap<Value, Value>,
    block_map: FxHashMap<Block, Block>,
}

impl DeepCloneMap {
    pub fn insert_value(&mut self, old: Value, new: Value) { self.value_map.insert(old, new); }

    pub fn insert_block(&mut self, old: Block, new: Block) { self.block_map.insert(old, new); }

    pub fn get_value(&self, old: Value) -> Option<Value> { self.value_map.get(&old).copied() }

    pub fn get_block(&self, old: Block) -> Option<Block> { self.block_map.get(&old).copied() }

    pub fn get_value_or_old(&self, old: Value) -> Value { self.get_value(old).unwrap_or(old) }

    pub fn get_block_or_old(&self, old: Block) -> Block { self.get_block(old).unwrap_or(old) }

    pub fn clear(&mut self) {
        self.value_map.clear();
        self.block_map.clear();
    }

    pub fn remove_value(&mut self, old: Value) { self.value_map.remove(&old); }

    pub fn remove_block(&mut self, old: Block) { self.block_map.remove(&old); }
}
