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

    pub fn front(&self) -> Option<K> {
        self.head
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

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, Clone)]
    struct Node {
        next: Option<usize>,
        prev: Option<usize>,
    }

    impl BiLinkedNode<usize> for Node {
        fn new() -> Self {
            Self {
                next: None,
                prev: None,
            }
        }

        fn next(&self) -> Option<usize> {
            self.next
        }

        fn prev(&self) -> Option<usize> {
            self.prev
        }

        fn set_next(&mut self, next: Option<usize>) {
            self.next = next;
        }

        fn set_prev(&mut self, prev: Option<usize>) {
            self.prev = prev;
        }
    }

    #[test]
    fn test_list_ops() {
        let mut list = BiLinkedList::<usize, Node>::new();
        assert_eq!(list.front(), None);

        // 1
        list.append(1).unwrap();
        assert_eq!(list.front(), Some(1));
        assert_eq!(list.node(1).unwrap().next(), None);
        assert_eq!(list.node(1).unwrap().prev(), None);
        assert!(list.contains_key(1));

        // 1 -- 2
        list.append(2).unwrap();
        assert_eq!(list.front(), Some(1));
        assert_eq!(list.node(1).unwrap().next(), Some(2));
        assert_eq!(list.node(2).unwrap().prev(), Some(1));
        assert!(list.contains_key(2));

        // 1 -- 2 -- 3
        list.append(3).unwrap();
        assert_eq!(list.front(), Some(1));
        assert_eq!(list.node(2).unwrap().next(), Some(3));
        assert_eq!(list.node(3).unwrap().prev(), Some(2));
        assert!(list.contains_key(3));

        // 4 -- 1 -- 2 -- 3
        list.insert_before(4, 1).unwrap();
        assert_eq!(list.front(), Some(4));
        assert_eq!(list.node(4).unwrap().next(), Some(1));
        assert_eq!(list.node(1).unwrap().prev(), Some(4));
        assert!(list.contains_key(4));

        // 4 -- 1 -- 5 -- 2 -- 3
        list.insert_before(5, 2).unwrap();
        assert_eq!(list.front(), Some(4));
        assert_eq!(list.node(1).unwrap().next(), Some(5));
        assert_eq!(list.node(5).unwrap().prev(), Some(1));
        assert!(list.contains_key(5));

        // 4 -- 5 -- 2 -- 3
        list.remove(1).unwrap();
        assert_eq!(list.front(), Some(4));
        assert_eq!(list.node(4).unwrap().next(), Some(5));
        assert_eq!(list.node(5).unwrap().prev(), Some(4));
        assert!(!list.contains_key(1));

        // 5 -- 2 -- 3
        list.remove(4).unwrap();
        assert_eq!(list.front(), Some(5));
        assert_eq!(list.node(2).unwrap().next(), Some(3));
        assert_eq!(list.node(3).unwrap().prev(), Some(2));
        assert_eq!(list.node(5).unwrap().prev(), None);
        assert_eq!(list.node(2).unwrap().prev(), Some(5));
        assert!(!list.contains_key(4));

        // 5 -- 3
        list.remove(2).unwrap();
        assert_eq!(list.front(), Some(5));
        assert_eq!(list.node(3).unwrap().next(), None);
        assert_eq!(list.node(3).unwrap().prev(), Some(5));
        assert!(!list.contains_key(2));

        list.remove(5).unwrap();
        assert_eq!(list.front(), Some(3));

        list.remove(3).unwrap();
        assert_eq!(list.front(), None);
        assert!(!list.contains_key(3));
    }

    #[test]
    fn test_list_iter() {
        let mut list = BiLinkedList::<usize, Node>::new();
        list.append(1).unwrap();
        list.append(2).unwrap();
        list.append(3).unwrap();
        list.append(4).unwrap();
        list.append(5).unwrap();

        let mut iter = list.iter();
        for i in 1..=5 {
            let (key, _) = iter.next().unwrap();
            assert_eq!(key, i);
            if i == 5 {
                assert!(iter.next().is_none());
            }
        }
    }

    #[test]
    fn test_list_err() {
        let mut list = BiLinkedList::<usize, Node>::new();
        list.append(1).unwrap();
        list.append(2).unwrap();
        list.append(3).unwrap();
        list.append(4).unwrap();
        list.append(5).unwrap();

        assert!(matches!(
            list.append(1),
            Err(BiLinkedListErr::KeyDuplicated(1))
        ));
        assert!(matches!(
            list.insert_before(1, 6),
            Err(BiLinkedListErr::KeyDuplicated(1))
        ));
        assert!(matches!(
            list.insert_before(6, 7),
            Err(BiLinkedListErr::NodeNotFound(7))
        ));
        assert!(matches!(
            list.remove(6),
            Err(BiLinkedListErr::NodeNotFound(6))
        ));
    }
}
