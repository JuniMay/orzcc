use std::{collections::HashMap, hash::Hash};

pub struct BiMap<K, V>
where
    K: Clone + Hash + Eq,
    V: Clone + Hash + Eq,
{
    pub(crate) map: HashMap<K, V>,
    pub(crate) rev: HashMap<V, K>,
}

impl<K, V> Default for BiMap<K, V>
where
    K: Clone + Hash + Eq,
    V: Clone + Hash + Eq,
{
    fn default() -> Self {
        Self::new()
    }
}

impl<K, V> BiMap<K, V>
where
    K: Clone + Hash + Eq,
    V: Clone + Hash + Eq,
{
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            rev: HashMap::new(),
        }
    }

    pub fn insert(&mut self, k: K, v: V) {
        self.map.insert(k.clone(), v.clone());
        self.rev.insert(v, k);
    }

    pub fn get(&self, k: &K) -> Option<&V> {
        self.map.get(k)
    }

    pub fn get_rev(&self, v: &V) -> Option<&K> {
        self.rev.get(v)
    }

    pub fn contains(&self, k: &K) -> bool {
        self.map.contains_key(k)
    }

    pub fn contains_rev(&self, v: &V) -> bool {
        self.rev.contains_key(v)
    }

    pub fn remove(&mut self, k: &K) -> Option<V> {
        if let Some(v) = self.map.remove(k) {
            self.rev.remove(&v);
            Some(v)
        } else {
            None
        }
    }

    pub fn remove_rev(&mut self, v: &V) -> Option<K> {
        if let Some(k) = self.rev.remove(v) {
            self.map.remove(&k);
            Some(k)
        } else {
            None
        }
    }
}
