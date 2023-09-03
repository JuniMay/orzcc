use std::{collections::HashMap, hash::Hash};

pub trait BiLinkedNode<K>
where
    K: Clone,
{
    fn new(key: K) -> Self;

    fn curr(&self) -> &K;
    fn next(&self) -> Option<&K>;
    fn prev(&self) -> Option<&K>;

    fn set_next(&mut self, next: Option<K>);
    fn set_prev(&mut self, prev: Option<K>);
}

pub struct BiLinkedList<K, N>
where
    K: Clone + Eq + Hash,
    N: BiLinkedNode<K>,
{
    head: Option<K>,
    tail: Option<K>,
    pub nodes: HashMap<K, N>,
}

/// Iterator in the bi-directional lined list
///
/// When iterating the linked list, the **node** will be returned,
/// the key can be get by calling `curr()`
pub struct BiLinkedIter<'a, K, N>
where
    K: Clone + Eq + Hash,
    N: BiLinkedNode<K>,
{
    list: &'a BiLinkedList<K, N>,
    curr: Option<&'a K>,
}

impl<'a, K, N> Iterator for BiLinkedIter<'a, K, N>
where
    K: Clone + Eq + Hash,
    N: BiLinkedNode<K>,
{
    type Item = (&'a K, &'a N);

    fn next(&mut self) -> Option<Self::Item> {
        let curr = self.curr?;
        self.curr = self.list.nodes.get(curr).unwrap().next();
        Some((curr, self.list.nodes.get(curr).unwrap()))
    }
}

impl<K, N> BiLinkedList<K, N>
where
    K: Clone + Eq + Hash,
    N: BiLinkedNode<K>,
{
    pub fn new() -> Self {
        Self {
            head: None,
            tail: None,
            nodes: HashMap::new(),
        }
    }

    fn is_inserted(&self, key: &K) -> bool {
        self.nodes.contains_key(key)
    }

    /// Append the key to the end.
    pub fn append(&mut self, key: &K) {
        // the key mustn't already inside the list
        assert!(!self.is_inserted(key), "key already exists");
        // new node
        let mut node = N::new(key.clone());
        // set the node as the next of the tail
        node.set_prev(self.tail.clone());

        if let Some(_) = self.head {
            // fix link from tail to node
            self.nodes
                .get_mut(self.tail.as_ref().unwrap())
                .unwrap()
                .set_next(Some(key.clone()));
        } else {
            // new head
            self.head = Some(key.clone());
        }
        // map
        self.nodes.insert(key.clone(), node);
        // maintain tail
        self.tail = Some(key.clone());
    }

    /// Insert key before `before`
    pub fn insert_before(&mut self, key: &K, before: &K) {
        assert!(self.is_inserted(before), "key not exists");
        assert!(!self.is_inserted(key), "key already exists");

        // Original:
        //  [ after ] <--> [ before ]
        // Inserted:
        //  [ after ] <--> [ new ] <--> [ before ]

        let mut node = N::new(key.clone());

        let after = self.nodes.get(before).unwrap().prev().cloned();

        {
            // maintain new and before
            let before_node = self.nodes.get_mut(before).unwrap();
            node.set_next(Some(before.clone()));
            node.set_prev(after.clone());
            before_node.set_prev(Some(key.clone()));
        }

        if let Some(ref after) = after {
            // fix link from after to new
            self.nodes
                .get_mut(after)
                .unwrap()
                .set_next(Some(key.clone()));
        } else {
            // new head
            self.head = Some(key.clone());
        }

        // map
        self.nodes.insert(key.clone(), node);
    }

    // Insert key after `after`
    pub fn insert_after(&mut self, key: &K, after: &K) {
        assert!(self.is_inserted(after), "key not exists");
        assert!(!self.is_inserted(key), "key already exists");

        let before = self.nodes.get(after).unwrap().next().cloned();

        if let Some(ref before) = before {
            self.insert_before(key, before);
        } else {
            self.append(key);
        }
    }

    /// Remove the key from the list
    ///
    /// Note this will not erase the node in the [`nodes`] until (if) the key is inserted again.
    pub fn remove(&mut self, key: &K) {
        assert!(self.is_inserted(key), "key not exists");

        let prev = self.nodes.get_mut(key).unwrap().prev().cloned();
        let next = self.nodes.get_mut(key).unwrap().next().cloned();
        {
            let node = self.nodes.get_mut(key).unwrap();
            node.set_prev(None);
            node.set_next(None);
        }

        if let Some(ref prev) = prev {
            self.nodes.get_mut(prev).unwrap().set_next(next.clone());
        } else {
            self.head = next.clone();
        }

        if let Some(ref next) = next {
            self.nodes.get_mut(next).unwrap().set_prev(prev.clone());
        } else {
            self.tail = prev.clone();
        }

        self.nodes.remove(key);
    }

    /// A custom iter function.
    pub fn iter(&self) -> BiLinkedIter<'_, K, N> {
        BiLinkedIter {
            list: self,
            curr: self.head.as_ref(),
        }
    }
}
