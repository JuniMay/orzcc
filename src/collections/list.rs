use std::{collections::HashMap, hash::Hash};

use thiserror::Error;

pub trait ListNode<K>: Default
where
    K: Copy,
{
    fn next(&self) -> Option<K>;
    fn prev(&self) -> Option<K>;
    fn set_next(&mut self, next: Option<K>);
    fn set_prev(&mut self, prev: Option<K>);
}

pub struct List<K, N>
where
    K: Copy + Eq + Hash,
    N: ListNode<K>,
{
    head: Option<K>,
    tail: Option<K>,
    nodes: HashMap<K, N>,
}

pub struct Iter<'a, K, N>
where
    K: Copy + Eq + Hash,
    N: ListNode<K>,
{
    list: &'a List<K, N>,
    curr: Option<K>,
}

impl<'a, K, N> Iterator for Iter<'a, K, N>
where
    K: Copy + Eq + Hash,
    N: ListNode<K>,
{
    type Item = (K, &'a N);

    fn next(&mut self) -> Option<Self::Item> {
        let curr = self.curr?;
        let node = self.list.nodes.get(&curr).unwrap();
        self.curr = node.next();
        Some((curr, node))
    }
}

impl<K, N> Default for List<K, N>
where
    K: Copy + Eq + Hash,
    N: ListNode<K>,
{
    fn default() -> Self {
        List {
            head: None,
            tail: None,
            nodes: HashMap::new(),
        }
    }
}

#[derive(Error, Debug)]
pub enum ListError<K>
where
    K: Copy,
{
    #[error("node {0} not found")]
    NodeNotFound(K),

    #[error("key {0} already exists")]
    KeyDuplicated(K),
}

impl<'a, K, N> IntoIterator for &'a List<K, N>
where
    K: Copy + Eq + Hash,
    N: ListNode<K>,
{
    type IntoIter = Iter<'a, K, N>;
    type Item = (K, &'a N);

    fn into_iter(self) -> Self::IntoIter {
        Iter {
            list: self,
            curr: self.head,
        }
    }
}

impl<K, N> Extend<K> for List<K, N>
where
    K: Copy + Eq + Hash,
    N: ListNode<K>,
{
    fn extend<T>(&mut self, iter: T)
    where
        T: IntoIterator<Item = K>,
    {
        for key in iter {
            self.append(key).ok();
        }
    }
}

impl<K, N> List<K, N>
where
    K: Copy + Eq + Hash,
    N: ListNode<K>,
{
    pub fn is_empty(&self) -> bool { self.head.is_none() }

    pub fn front(&self) -> Option<K> { self.head }

    pub fn back(&self) -> Option<K> { self.tail }

    pub fn node(&self, key: K) -> Option<&N> { self.nodes.get(&key) }

    pub fn node_mut(&mut self, key: K) -> Option<&mut N> { self.nodes.get_mut(&key) }

    pub fn contains(&self, key: K) -> bool { self.nodes.contains_key(&key) }

    pub fn insert_after(&mut self, key: K, after: K) -> Result<(), ListError<K>> {
        if self.contains(key) {
            return Err(ListError::KeyDuplicated(key));
        }

        let mut node = N::default();

        let before = self
            .node(after)
            .ok_or(ListError::NodeNotFound(after))?
            .next();
        let after_node = self.node_mut(after).ok_or(ListError::NodeNotFound(after))?;

        node.set_prev(Some(after));
        node.set_next(before);

        after_node.set_next(Some(key));

        if let Some(before) = before {
            self.node_mut(before).unwrap().set_prev(Some(key));
        } else {
            self.tail = Some(key);
        }

        self.nodes.insert(key, node);
        Ok(())
    }

    pub fn insert_before(&mut self, key: K, before: K) -> Result<(), ListError<K>> {
        if self.contains(key) {
            return Err(ListError::KeyDuplicated(key));
        }

        let mut node = N::default();

        let after = self
            .node(before)
            .ok_or(ListError::NodeNotFound(before))?
            .prev();
        let before_node = self
            .node_mut(before)
            .ok_or(ListError::NodeNotFound(before))?;

        node.set_prev(after);
        node.set_next(Some(before));

        before_node.set_prev(Some(key));

        if let Some(after) = after {
            self.node_mut(after).unwrap().set_next(Some(key));
        } else {
            self.head = Some(key);
        }

        self.nodes.insert(key, node);
        Ok(())
    }

    pub fn remove(&mut self, key: K) -> Result<(), ListError<K>> {
        let node = self
            .nodes
            .remove(&key)
            .ok_or(ListError::NodeNotFound(key))?;
        let prev = node.prev();
        let next = node.next();

        if let Some(prev) = prev {
            self.node_mut(prev).unwrap().set_next(next);
        } else {
            self.head = next;
        }

        if let Some(next) = next {
            self.node_mut(next).unwrap().set_prev(prev);
        } else {
            self.tail = prev;
        }

        Ok(())
    }

    pub fn append(&mut self, key: K) -> Result<(), ListError<K>> {
        if self.contains(key) {
            return Err(ListError::KeyDuplicated(key));
        }

        if let Some(tail) = self.tail {
            self.insert_after(key, tail)?;
        } else {
            self.head = Some(key);
            self.tail = Some(key);
            self.nodes.insert(key, N::default());
        }

        Ok(())
    }

    pub fn prepend(&mut self, key: K) -> Result<(), ListError<K>> {
        if self.contains(key) {
            return Err(ListError::KeyDuplicated(key));
        }

        if let Some(head) = self.head {
            self.insert_before(key, head)?;
        } else {
            self.head = Some(key);
            self.tail = Some(key);
            self.nodes.insert(key, N::default());
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[derive(Debug, Clone, Default)]
    struct TestNode {
        next: Option<usize>,
        prev: Option<usize>,
    }

    impl ListNode<usize> for TestNode {
        fn next(&self) -> Option<usize> { self.next }

        fn prev(&self) -> Option<usize> { self.prev }

        fn set_next(&mut self, next: Option<usize>) { self.next = next; }

        fn set_prev(&mut self, prev: Option<usize>) { self.prev = prev; }
    }

    #[test]
    fn test_list_ops() {
        let mut list = List::<usize, TestNode>::default();
        assert_eq!(list.front(), None);

        // 1
        list.append(1).unwrap();
        assert_eq!(list.front(), Some(1));
        assert_eq!(list.back(), Some(1));
        assert_eq!(list.node(1).unwrap().next(), None);
        assert_eq!(list.node(1).unwrap().prev(), None);
        assert!(list.contains(1));

        // 1 -- 2
        list.append(2).unwrap();
        assert_eq!(list.front(), Some(1));
        assert_eq!(list.back(), Some(2));
        assert_eq!(list.node(1).unwrap().next(), Some(2));
        assert_eq!(list.node(2).unwrap().prev(), Some(1));
        assert!(list.contains(2));

        // 1 -- 2 -- 3
        list.append(3).unwrap();
        assert_eq!(list.front(), Some(1));
        assert_eq!(list.back(), Some(3));
        assert_eq!(list.node(2).unwrap().next(), Some(3));
        assert_eq!(list.node(3).unwrap().prev(), Some(2));
        assert!(list.contains(3));

        // 4 -- 1 -- 2 -- 3
        list.insert_before(4, 1).unwrap();
        assert_eq!(list.front(), Some(4));
        assert_eq!(list.back(), Some(3));
        assert_eq!(list.node(4).unwrap().next(), Some(1));
        assert_eq!(list.node(1).unwrap().prev(), Some(4));
        assert!(list.contains(4));

        // 4 -- 1 -- 5 -- 2 -- 3
        list.insert_after(5, 1).unwrap();
        assert_eq!(list.front(), Some(4));
        assert_eq!(list.back(), Some(3));
        assert_eq!(list.node(1).unwrap().next(), Some(5));
        assert_eq!(list.node(5).unwrap().prev(), Some(1));
        assert!(list.contains(5));

        // 4 -- 5 -- 2 -- 3
        list.remove(1).unwrap();
        assert_eq!(list.front(), Some(4));
        assert_eq!(list.back(), Some(3));
        assert_eq!(list.node(4).unwrap().next(), Some(5));
        assert_eq!(list.node(5).unwrap().prev(), Some(4));
        assert!(!list.contains(1));

        // 5 -- 2 -- 3
        list.remove(4).unwrap();
        assert_eq!(list.front(), Some(5));
        assert_eq!(list.back(), Some(3));
        assert_eq!(list.node(2).unwrap().next(), Some(3));
        assert_eq!(list.node(3).unwrap().prev(), Some(2));
        assert_eq!(list.node(5).unwrap().prev(), None);
        assert_eq!(list.node(2).unwrap().prev(), Some(5));
        assert!(!list.contains(4));

        // 5 -- 3
        list.remove(2).unwrap();
        assert_eq!(list.front(), Some(5));
        assert_eq!(list.back(), Some(3));
        assert_eq!(list.node(3).unwrap().next(), None);
        assert_eq!(list.node(3).unwrap().prev(), Some(5));
        assert!(!list.contains(2));

        list.remove(5).unwrap();
        assert_eq!(list.front(), Some(3));
        assert_eq!(list.back(), Some(3));

        list.remove(3).unwrap();
        assert_eq!(list.front(), None);
        assert_eq!(list.back(), None);
        assert!(!list.contains(3));
    }

    #[test]
    fn test_list_iter() {
        let mut list = List::<usize, TestNode>::default();
        list.append(3).unwrap();
        list.append(4).unwrap();
        list.append(5).unwrap();
        list.prepend(2).unwrap();
        list.prepend(1).unwrap();

        let mut iter = list.into_iter();
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
        let mut list = List::<usize, TestNode>::default();
        list.append(3).unwrap();
        list.append(4).unwrap();
        list.append(5).unwrap();
        list.prepend(2).unwrap();
        list.prepend(1).unwrap();

        assert!(matches!(list.append(1), Err(ListError::KeyDuplicated(1))));
        assert!(matches!(
            list.insert_before(1, 6),
            Err(ListError::KeyDuplicated(1))
        ));
        assert!(matches!(
            list.insert_before(6, 7),
            Err(ListError::NodeNotFound(7))
        ));
        assert!(matches!(list.remove(6), Err(ListError::NodeNotFound(6))));
    }

    #[test]
    fn test_list_extend() {
        let mut list = List::<usize, TestNode>::default();
        list.extend(1..=5);
        assert_eq!(list.front(), Some(1));
        assert_eq!(list.back(), Some(5));
    }
}
