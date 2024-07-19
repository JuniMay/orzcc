use std::hash::Hash;

use rustc_hash::FxHashMap;

use super::storage::ArenaPtr;

pub struct UnionFind<T> {
    parents: FxHashMap<T, T>,
    ranks: FxHashMap<T, usize>,
}

impl<T> Default for UnionFind<T> {
    fn default() -> Self {
        Self {
            parents: FxHashMap::default(),
            ranks: FxHashMap::default(),
        }
    }
}

impl<T: ArenaPtr + Hash> UnionFind<T> {
    pub fn insert(&mut self, ptr: T) {
        if self.parents.contains_key(&ptr) {
            return;
        }
        self.parents.insert(ptr, ptr);
        self.ranks.insert(ptr, 0);
    }

    pub fn find(&mut self, ptr: T) -> Option<T> {
        if !self.parents.contains_key(&ptr) {
            return None;
        }
        let mut find = ptr;
        while find != self.parents[&find] {
            find = self.parents[&find];
        }
        self.parents.insert(ptr, find);
        Some(find)
    }

    pub fn union(&mut self, ptr1: T, ptr2: T) {
        let root1 = self.find(ptr1).unwrap();
        let root2 = self.find(ptr2).unwrap();
        if root1 == root2 {
            return;
        }
        let rank1 = self.ranks[&root1];
        let rank2 = self.ranks[&root2];
        match rank1.cmp(&rank2) {
            std::cmp::Ordering::Less => {
                self.parents.insert(root1, root2);
            }
            std::cmp::Ordering::Greater => {
                self.parents.insert(root2, root1);
            }
            std::cmp::Ordering::Equal => {
                self.parents.insert(root2, root1);
                self.ranks.insert(root1, rank1 + 1);
            }
        }
    }
}
