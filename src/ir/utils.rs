use std::collections::{HashMap, HashSet};
use std::hash::Hash;

use super::inst::Inst;

/// Allocator for names and blocks in IR.
pub struct NameAllocator<T>
where
    T: Hash + Eq + Clone,
{
    counter: usize,
    predefined_set: HashSet<String>,
    predefined_map: HashMap<T, String>,
    allocated_map: HashMap<T, String>,
}

impl<T> NameAllocator<T>
where
    T: Hash + Eq + Clone,
{
    pub fn new() -> NameAllocator<T> {
        NameAllocator {
            counter: 0,
            predefined_set: HashSet::new(),
            predefined_map: HashMap::new(),
            allocated_map: HashMap::new(),
        }
    }

    /// Allocate a new name or return the existed one.
    pub fn allocate(&mut self, key: &T) -> String {
        if self.predefined_map.contains_key(key) {
            return self.predefined_map.get(key).unwrap().clone();
        } else if self.allocated_map.contains_key(&key) {
            return self.allocated_map.get(key).unwrap().clone();
        } else {
            let mut name = format!("{}", self.counter);
            while self.predefined_set.contains(&name) {
                self.counter += 1;
                name = format!("{}", self.counter);
            }
            self.allocated_map.insert(key.clone(), name.clone());
            self.counter += 1;
            return name;
        }
    }

    /// Manually assign a name.
    pub fn assign(&mut self, key: &T, name: String) -> Result<(), String> {
        if self.predefined_set.contains(&name)
            || self.predefined_map.contains_key(key)
            || self.predefined_map.contains_key(key)
        {
            Err(format!("{} is already assigned", name))
        } else {
            self.predefined_set.insert(name.clone());
            self.predefined_map.insert(key.clone(), name.clone());
            Ok(())
        }
    }

    /// Clear all auto-allocated names.
    pub fn clear(&mut self) {
        self.allocated_map.clear();
        self.counter = 0;
    }
}

/// Allocator for id of values.
pub struct IdAllocator {
    counter: usize,
    free: Vec<usize>,
}

impl IdAllocator {
    pub fn new() -> IdAllocator {
        IdAllocator {
            counter: 0,
            free: Vec::new(),
        }
    }

    /// Allocate a new id
    pub fn allocate(&mut self) -> usize {
        // first check the free
        if self.free.len() > 0 {
            return self.free.pop().unwrap();
        } else {
            self.counter += 1;
            return self.counter - 1;
        }
    }

    /// Make a certain id free.
    pub fn free(&mut self, id: usize) {
        self.free.push(id);
    }
}

/// Commenter for instructions
///
/// This is used to put useful information after the instruction when dumping the IR.
pub struct InstCommenter {
    pub comments: HashMap<Inst, String>,
}

impl InstCommenter {
    pub fn new() -> InstCommenter {
        InstCommenter {
            comments: HashMap::new(),
        }
    }

    pub fn comment(&mut self, inst: &Inst, comment: String) {
        self.comments.insert(inst.clone(), comment);
    }
}
