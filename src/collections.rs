use std::{collections::HashMap, fmt::Debug, hash::Hash};

/// A bi-directional linked node to store the value.
pub trait BiLinkedNode<K>
where
    K: Copy,
{
    fn new() -> Self;

    fn next(&self) -> Option<K>;
    fn prev(&self) -> Option<K>;

    fn set_next(&mut self, next: Option<K>);
    fn set_prev(&mut self, prev: Option<K>);
}

/// A bi-directional linked list.
pub struct BiLinkedList<K, N>
where
    K: Copy + Eq + Hash + Debug,
    N: BiLinkedNode<K>,
{
    head: Option<K>,
    tail: Option<K>,
    pub nodes: HashMap<K, N>,
}

/// Iterator in the bi-directional lined list.
pub struct BiLinkedIter<'a, K, N>
where
    K: Copy + Eq + Hash + Debug,
    N: BiLinkedNode<K>,
{
    list: &'a BiLinkedList<K, N>,
    curr: Option<K>,
}

impl<'a, K, N> Iterator for BiLinkedIter<'a, K, N>
where
    K: Copy + Eq + Hash + Debug,
    N: BiLinkedNode<K>,
{
    type Item = (K, &'a N);

    fn next(&mut self) -> Option<Self::Item> {
        let curr = self.curr?;
        self.curr = self.list.nodes.get(&curr).unwrap().next();
        Some((curr, self.list.nodes.get(&curr).unwrap()))
    }
}

/// Errors of operations in the list.
#[derive(Debug)]
pub enum BiLinkedListErr<K>
where
    K: Copy + Debug,
{
    /// The node can not be found in `nodes`.
    NodeNotFound(K),

    /// The key to be inserted already exists.
    KeyDuplicated(K),
}

impl<K, N> BiLinkedList<K, N>
where
    K: Copy + Eq + Hash + Debug,
    N: BiLinkedNode<K>,
{
    pub fn new() -> Self {
        Self {
            head: None,
            tail: None,
            nodes: HashMap::new(),
        }
    }

    fn contains_key(&self, key: K) -> bool {
        self.nodes.contains_key(&key)
    }

    pub fn node(&self, key: K) -> Option<&N> {
        self.nodes.get(&key)
    }

    pub fn node_mut(&mut self, key: K) -> Option<&mut N> {
        self.nodes.get_mut(&key)
    }

    /// Append the key to the end.
    ///
    /// If `key` has been inserted, return `BiLinkedListErr::KeyDuplicated`.
    pub fn append(&mut self, key: K) -> Result<(), BiLinkedListErr<K>> {
        if self.contains_key(key) {
            return Err(BiLinkedListErr::KeyDuplicated(key));
        }
        // new node
        let mut node = N::new();
        // set the node as the next of the tail
        node.set_prev(self.tail);

        if let Some(_) = self.head {
            // fix link from tail to node
            self.nodes
                .get_mut(
                    self.tail
                        .as_ref()
                        .expect("tail should exist if head exists"),
                )
                .expect("tail of the list should be in the nodes")
                .set_next(Some(key));
        } else {
            // new head
            self.head = Some(key);
        }
        // map
        self.nodes.insert(key, node);
        // maintain tail
        self.tail = Some(key);

        Ok(())
    }

    /// Insert key before `before`
    ///
    /// If `key` has been inserted, return `BiLinkedListErr::KeyDuplicated(key)`.
    /// If `before` is not inserted, return `BiLinkedListErr::NodeNotFound(before)`.
    pub fn insert_before(&mut self, key: K, before: K) -> Result<(), BiLinkedListErr<K>> {
        if self.contains_key(key) {
            return Err(BiLinkedListErr::KeyDuplicated(key));
        }

        // Original:
        //  [ after ] <--> [ before ]
        // Inserted:
        //  [ after ] <--> [ new ] <--> [ before ]

        let mut node = N::new();

        let after = self
            .nodes
            .get(&before)
            .ok_or(BiLinkedListErr::NodeNotFound(before))?
            .prev();

        {
            // maintain new and before
            let before_node = self.nodes.get_mut(&before).unwrap();
            node.set_next(Some(before));
            node.set_prev(after);
            before_node.set_prev(Some(key));
        }

        if let Some(ref after) = after {
            // fix link from after to new
            self.nodes
                .get_mut(after)
                .expect("key in the list should be in the nodes")
                .set_next(Some(key));
        } else {
            // new head
            self.head = Some(key);
        }

        // map
        self.nodes.insert(key, node);

        Ok(())
    }

    /// Remove the key from the list
    ///
    /// If `key` is not inserted, return `BiLinkedListErr::NodeNotFound(key)`.
    pub fn remove(&mut self, key: K) -> Result<(), BiLinkedListErr<K>> {
        let prev = self
            .nodes
            .get_mut(&key)
            .ok_or(BiLinkedListErr::NodeNotFound(key))?
            .prev();
        let next = self
            .nodes
            .get_mut(&key)
            .ok_or(BiLinkedListErr::NodeNotFound(key))?
            .next();
        {
            let node = self
                .nodes
                .get_mut(&key)
                .ok_or(BiLinkedListErr::NodeNotFound(key))?;
            node.set_prev(None);
            node.set_next(None);
        }

        if let Some(prev) = prev {
            self.nodes
                .get_mut(&prev)
                .expect("key in the list should be in the nodes")
                .set_next(next);
        } else {
            self.head = next;
        }

        if let Some(next) = next {
            self.nodes
                .get_mut(&next)
                .expect("key in the list should be in the nodes")
                .set_prev(prev);
        } else {
            self.tail = prev;
        }

        self.nodes.remove(&key);

        Ok(())
    }

    /// Get the iter for the linked list.
    pub fn iter(&self) -> BiLinkedIter<'_, K, N> {
        BiLinkedIter {
            list: self,
            curr: self.head,
        }
    }
}
