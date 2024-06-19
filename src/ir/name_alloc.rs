use std::{collections::HashMap, hash::Hash};

use crate::collections::storage::ArenaPtr;

pub struct NameAlloc<T>
where
    T: ArenaPtr,
{
    /// Map from name to pointer.
    name_to_ptr: HashMap<String, T>,
    /// Map from pointer to name.
    ptr_to_name: HashMap<T, String>,

    /// Counter of each given prefix.
    ///
    /// When allocating names, one might want to use a prefix that has
    /// human-readable meaning, while also ensuring that the name is unique.
    /// So, we keep a counter for each prefix to allocate unique names.
    counters: HashMap<String, usize>,
}

impl<T> NameAlloc<T>
where
    T: ArenaPtr + Hash,
{
    pub(super) fn new() -> Self {
        Self {
            name_to_ptr: HashMap::new(),
            ptr_to_name: HashMap::new(),
            counters: HashMap::new(),
        }
    }

    pub(super) fn get_name(&self, ptr: T) -> Option<&String> { self.ptr_to_name.get(&ptr) }

    pub(super) fn _get_ptr(&self, name: &str) -> Option<&T> { self.name_to_ptr.get(name) }

    pub(super) fn _remove_by_name(&mut self, name: &str) -> Option<T> {
        let ptr = self.name_to_ptr.remove(name)?;
        self.ptr_to_name.remove(&ptr);
        Some(ptr)
    }

    pub(super) fn remove_by_ptr(&mut self, ptr: T) -> Option<String> {
        let name = self.ptr_to_name.remove(&ptr)?;
        self.name_to_ptr.remove(&name);
        Some(name)
    }

    /// Assign a name for a pointer.
    ///
    /// # Panics
    ///
    /// - Panics if the name is already assigned to another pointer.
    /// - Panics if the name is empty.
    /// - Panics if the pointer is already assigned a name.
    pub(super) fn assign_name(&mut self, ptr: T, name: String) {
        if self.name_to_ptr.contains_key(&name) {
            panic!("name {:?} is already assigned", name);
        }
        if name.is_empty() {
            panic!("name cannot be empty");
        }
        if self.ptr_to_name.contains_key(&ptr) {
            panic!("pointer is already assigned a name");
        }
        self.name_to_ptr.insert(name.clone(), ptr);
        self.ptr_to_name.insert(ptr, name);
    }

    /// Allocate a name for a pointer.
    ///
    /// # Parameters
    ///
    /// - `ptr`: The pointer to allocate a name for.
    /// - `prefix`: The prefix to use for the name.
    ///
    /// # Returns
    ///
    /// The name allocated for the pointer.
    ///
    /// # Panics
    ///
    /// - Panics if the pointer is already assigned a name.
    ///
    /// # Notes
    ///
    /// In order to avoid conflicts, the counter might increase multiple times
    /// to ensure that the name is unique. So under some extreme circumstances,
    /// this function might be very slow, **USE WITH CAUTION**.
    pub(super) fn alloc_name(&mut self, ptr: T, prefix: String) -> &String {
        if self.ptr_to_name.contains_key(&ptr) {
            panic!("pointer is already assigned a name");
        }
        let counter = self.counters.entry(prefix.clone()).or_insert(0);

        let mut name = format!("{}{}", prefix, *counter);
        while self.name_to_ptr.contains_key(&name) {
            *counter += 1;
            name = format!("{}{}", prefix, *counter);
        }

        self.assign_name(ptr, name);
        self.ptr_to_name.get(&ptr).unwrap()
    }
}
