//! # Arena-based Linked List
//!
//! Linked list is not easy to implement, but with arena, it is easier
//! (hopefully).

use super::storage::ArenaPtrLike;

/// A container of linked lists.
///
/// A container can store multiple linked lists, and each linked list is
/// represented by a head and a tail. The container is required to share a
/// common arena with the nodes in the linked list.
///
/// # Type Parameters
///
/// A container might contain multiple linked lists, so the type of the nodes in
/// the linked list is a type parameter.
///
/// - `NodePtr`: The pointer type of the nodes in the linked list.
pub trait LinkedListContainerPtr<NodePtr>: ArenaPtrLike
where
    NodePtr: LinkedListNodePtr<A = Self::A, ContainerPtr = Self>,
{
    /// Get the head of the linked list.
    ///
    /// # Notes
    ///
    /// The [None]-ness of the head and the tail should be consistent. That is,
    /// if the head is [None], the tail should also be [None], and vice versa.
    fn head(self, arena: &Self::A) -> Option<NodePtr>;

    /// Get the tail of the linked list.
    ///
    /// # See Also
    ///
    /// [head](LinkedListContainerPtr::head)
    fn tail(self, arena: &Self::A) -> Option<NodePtr>;

    /// Set the head of the linked list.
    ///
    /// # Notes
    ///
    /// This is a low-level operation, and it is not recommended to call this
    /// method directly. Instead, use
    /// [push_front](LinkedListContainerPtr::push_front)
    /// or [push_back](LinkedListContainerPtr::push_back) to insert a node into
    /// the linked list container.
    fn set_head(self, arena: &mut Self::A, head: Option<NodePtr>);

    /// Set the tail of the linked list.
    ///
    /// # See Also
    ///
    /// [set_head](LinkedListContainerPtr::set_head)
    fn set_tail(self, arena: &mut Self::A, tail: Option<NodePtr>);

    /// Push a node to the front of the linked list.
    ///
    /// # Parameters
    ///
    /// - `arena`: The arena to allocate the node.
    /// - `node`: The node to push to the front of the linked list.
    ///
    /// # Panics
    ///
    /// Panics if the node is already in another container.
    ///
    /// # Notes
    ///
    /// If the linked list is empty, the node will be set as the head and the
    /// tail of the linked list.
    ///
    /// # See Also
    ///
    /// [push_back](LinkedListContainerPtr::push_back)
    fn push_front(self, arena: &mut Self::A, node: NodePtr) {
        assert!(
            node.container(arena).is_none(),
            "the node is already in another container"
        );

        if let Some(head) = self.head(arena) {
            // just insert the node before the current head
            LinkedListNodePtr::insert_before(head, arena, node);
        } else {
            // the list is empty, set the node as the head and tail
            self.set_head(arena, Some(node));
            self.set_tail(arena, Some(node));
            node.set_container(arena, Some(self))
        }
    }

    /// Push a node to the back of the linked list.
    ///
    /// # Parameters
    ///
    /// - `arena`: The arena to allocate the node.
    /// - `node`: The node to push to the back of the linked list.
    ///
    /// # Panics
    ///
    /// Panics if the node is already in another container.
    ///
    /// # Notes
    ///
    /// If the linked list is empty, the node will be set as the head and the
    /// tail of the linked list.
    ///
    /// # See Also
    ///
    /// [push_front](LinkedListContainerPtr::push_front)
    fn push_back(self, arena: &mut Self::A, node: NodePtr) {
        assert!(
            node.container(arena).is_none(),
            "the node is already in another container"
        );

        if let Some(tail) = self.tail(arena) {
            // just insert the node after the current tail
            LinkedListNodePtr::insert_after(tail, arena, node);
        } else {
            // the list is empty, set the node as the head and tail
            self.set_head(arena, Some(node));
            self.set_tail(arena, Some(node));
            node.set_container(arena, Some(self))
        }
    }

    /// Merge another linked list into this linked list.
    ///
    /// This operation will not release/free `other`, but only set the head and
    /// tail of `other` to `None`, and make the nodes in `other` to be in this
    /// container.
    ///
    /// This is used to simplify the implementation of control flow
    /// simplification.
    ///
    /// # Notes
    ///
    /// Merging of blocks in IR should also update the block parameters and
    /// maintain the def-use chain.
    ///
    /// # See Also
    ///
    /// [Block::merge](crate::ir::Block::merge)
    fn merge(self, arena: &mut Self::A, other: Self) {
        // we need to unlink all nodes in `other` from the original container,
        // so that we can modify the nodes' container to `self`

        // iterate all nodes in `other`, and modify.
        // yes, this is O(N), but in order to modify each nodes' container, we
        // have to do so.
        let mut curr = other.head(arena);
        while let Some(node) = curr {
            let next = node.next(arena);
            // unlink, and push back to `self`
            node.unlink(arena);
            self.push_back(arena, node);
            curr = next;
        }

        // actually, the head and tail of `other` should be `None` now,
        // so just assert it
        assert!(
            other.head(arena).is_none(),
            "the head of `other` is not `None`"
        );
        assert!(
            other.tail(arena).is_none(),
            "the tail of `other` is not `None`"
        );
    }

    /// Split the linked list in two at the given position.
    ///
    /// If the second container is not empty, all the splited nodes will be
    /// [push_front](LinkedListContainerPtr::push_front) to the second
    /// container.
    ///
    /// # Parameters
    ///
    /// - `arena`: The arena to allocate the nodes.
    /// - `other`: The container to store the nodes after the position.
    /// - `pos`: The position to split the linked list, and the position itself
    ///  will be in the `self` container.
    ///
    /// # Panics
    ///
    /// Panics if `pos` is not in `self`.
    ///
    /// # Notes
    ///
    /// Splitting block in IR should also update the terminator, and maybe
    /// maintain the def-use chain.
    ///
    /// # See Also
    ///
    /// [Block::split](crate::ir::Block::split)
    fn split(self, arena: &mut Self::A, other: Self, pos: NodePtr) {
        // assert that `pos` is in `self`
        assert!(pos.container(arena) == Some(self), "`pos` is not in `self`");

        // we need to unlink all nodes after `pos` from the original container,
        // so that we can modify the nodes' container to `other`

        // iterate all nodes after `pos`, and modify.
        // yes, this is also O(N).
        let mut curr = self.tail(arena);
        while let Some(node) = curr {
            if node == pos {
                // we have reached `pos`, stop, this should always be reached.
                break;
            }

            let prev = node.prev(arena);
            // unlink, and push front to `other`
            node.unlink(arena);
            other.push_front(arena, node);
            curr = prev;
        }

        // the tail of `self` should be `pos` now, assert it
        assert!(
            self.tail(arena) == Some(pos),
            "the tail of `self` is not `pos`"
        );
    }

    /// Get an immutable iterator of the linked list.
    fn iter(self, arena: &Self::A) -> LinkedListIterator<NodePtr> {
        LinkedListIterator {
            arena,
            curr_front: self.head(arena),
            curr_back: self.tail(arena),
        }
    }

    /// Get an cursor of the linked list.
    ///
    /// # Parameters
    ///
    /// - `arena`: The arena to allocate the cursor.
    ///
    /// # See Also
    ///
    /// [LinkedListCursor]
    fn cursor(self) -> LinkedListCursor<NodePtr> {
        LinkedListCursor {
            container: self,
            curr_front: None,
            curr_back: None,
        }
    }
}

/// The iterator of a linked list.
///
/// This iterator is a double-ended iterator.
///
/// # Type Parameters
///
/// - `T`: The pointer type of the nodes in the linked list.
///
/// # Lifetime
///
/// - `a`: The lifetime of the arena.
pub struct LinkedListIterator<'a, T: LinkedListNodePtr> {
    /// The arena of the linked list.
    arena: &'a T::A,
    /// The current front node.
    ///
    /// This always points to the next node to be yielded.
    curr_front: Option<T>,
    /// The current back node.
    ///
    /// This is used to implement [DoubleEndedIterator], and it always points to
    /// the next node to be yielded when traversing in reverse order.
    curr_back: Option<T>,
}

impl<'a, T: LinkedListNodePtr> Iterator for LinkedListIterator<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let curr = self.curr_front;
        self.curr_front = curr.and_then(|n| n.next(self.arena));
        curr
    }
}

impl<'a, T: LinkedListNodePtr> DoubleEndedIterator for LinkedListIterator<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let curr = self.curr_back;
        self.curr_back = curr.and_then(|n| n.prev(self.arena));
        curr
    }
}

/// Interfaces for linked list node.
///
/// Any linked list node should only belog to one container at a time.
///
/// # Notes
///
/// The setters in this trait are low-level operations, and it is not
/// recommended to call them directly. Instead, use `insert-` and
/// [unlink](LinkedListNodePtr::unlink) methods to manipulate the linked list.
pub trait LinkedListNodePtr: ArenaPtrLike {
    /// The type of the container of the node.
    ///
    /// Container is the data structure that contains the linked list. For
    /// example, the instructions are contained in a block.
    type ContainerPtr: LinkedListContainerPtr<Self, A = Self::A>;

    /// Get the next node.
    fn next(self, arena: &Self::A) -> Option<Self>;

    /// Get the previous node.
    fn prev(self, arena: &Self::A) -> Option<Self>;

    /// Set the next node.
    fn set_next(self, arena: &mut Self::A, next: Option<Self>);

    /// Set the previous node.
    fn set_prev(self, arena: &mut Self::A, prev: Option<Self>);

    /// Get the container of the node.
    fn container(self, arena: &Self::A) -> Option<Self::ContainerPtr>;

    /// Set the container of the node.
    fn set_container(self, arena: &mut Self::A, container: Option<Self::ContainerPtr>);

    /// Insert a node after the current node.
    ///
    /// If the current node is the tail of the container, the new node will be
    /// set as the new tail.
    ///
    /// # Parameters
    ///
    /// - `arena`: The arena to allocate the node.
    /// - `node`: The node to insert after the current node.
    ///
    /// # Panics
    ///
    /// - Panics if the current node does not belong to a container.
    /// - Panics if the node to insert already belongs to a container.
    fn insert_after(self, arena: &mut Self::A, node: Self) {
        assert!(
            self.container(arena).is_some(),
            "cannot insert after a node without container"
        );
        assert!(
            node.container(arena).is_none(),
            "cannot insert a node that already belongs to a container"
        );

        let next = self.next(arena);
        if let Some(next) = next {
            next.set_prev(arena, Some(node));
            node.set_next(arena, Some(next));
        }

        node.set_prev(arena, Some(self));
        self.set_next(arena, Some(node));

        if let Some(container) = self.container(arena) {
            if container.tail(arena) == Some(self) {
                container.set_tail(arena, Some(node));
            }
        } else {
            unreachable!("container should not be None")
        }

        node.set_container(arena, self.container(arena));
    }

    /// Insert a node before the current node.
    ///
    /// If the current node is the head of the container, the new node will be
    /// set as the new head.
    ///
    /// # Parameters
    ///
    /// - `arena`: The arena to allocate the node.
    /// - `node`: The node to insert before the current node.
    ///
    /// # Panics
    ///
    /// - Panics if the current node does not belong to a container.
    /// - Panics if the node to insert already belongs to a container.
    fn insert_before(self, arena: &mut Self::A, node: Self) {
        assert!(
            self.container(arena).is_some(),
            "cannot insert before a node without container"
        );
        assert!(
            node.container(arena).is_none(),
            "cannot insert a node that already belongs to a container"
        );

        let prev = self.prev(arena);
        if let Some(prev) = prev {
            prev.set_next(arena, Some(node));
            node.set_prev(arena, Some(prev));
        }

        node.set_next(arena, Some(self));
        self.set_prev(arena, Some(node));

        if let Some(container) = self.container(arena) {
            if container.head(arena) == Some(self) {
                container.set_head(arena, Some(node));
            }
        } else {
            unreachable!("container should not be None")
        }

        node.set_container(arena, self.container(arena));
    }

    /// Unlink the current node.
    ///
    /// This method removes the node from the linked list. The node will not be
    /// deallocated, and the caller is responsible for freeing the memory.
    ///
    /// If this node is already unlinked, this method does nothing.
    ///
    /// # Parameters
    ///
    /// - `arena`: The arena to deallocate the node.
    fn unlink(self, arena: &mut Self::A) {
        let prev = self.prev(arena);
        let next = self.next(arena);

        if let Some(prev) = prev {
            prev.set_next(arena, next);
        }

        if let Some(next) = next {
            next.set_prev(arena, prev);
        }

        if let Some(container) = self.container(arena) {
            if container.head(arena) == Some(self) {
                container.set_head(arena, next);
            }

            if container.tail(arena) == Some(self) {
                container.set_tail(arena, prev);
            }
        }

        self.set_prev(arena, None);
        self.set_next(arena, None);
        self.set_container(arena, None);
    }
}

/// A cursor to modify the linked list.
///
/// The cursor is implemented because mutable iterator is hard to implement.
pub struct LinkedListCursor<T: LinkedListNodePtr> {
    container: T::ContainerPtr,
    /// The current node in the forward order.
    ///
    /// This is different from the current node in the iterator, because one
    /// might mutate the `next` pointer of the current node, here the current
    /// node is the node that is already visited.
    ///
    /// And to achieve this, we need to set the initial value to `None`, and
    /// update it when we visit the next node.
    curr_front: Option<T>,
    /// The current node in the reverse order.
    ///
    /// Same as `curr_front`, but in the reverse order.
    curr_back: Option<T>,
}

impl<T> LinkedListCursor<T>
where
    T: LinkedListNodePtr,
{
    pub fn next(&mut self, arena: &T::A) -> Option<T> {
        if let Some(curr) = self.curr_front {
            // Get the next node.
            self.curr_front = curr.next(arena);
        } else {
            // Get the head of the container.
            self.curr_front = self.container.head(arena);
        }
        self.curr_front
    }

    /// Get the next node in the reverse order.
    pub fn prev(&mut self, arena: &T::A) -> Option<T> {
        if let Some(curr) = self.curr_back {
            // Get the previous node.
            self.curr_back = curr.prev(arena);
        } else {
            // Get the tail of the container.
            self.curr_back = self.container.tail(arena);
        }
        self.curr_back
    }
}

#[cfg(test)]
mod tests {
    use super::{LinkedListContainerPtr, LinkedListNodePtr};
    use crate::{
        collections::storage::{Arena, ArenaLikeAlloc, ArenaPtr, ArenaPtrLike},
        impl_arena,
    };

    #[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
    struct NodePtr(ArenaPtr<Node>);

    struct Node {
        value: i32,
        next: Option<NodePtr>,
        prev: Option<NodePtr>,
        parent: Option<ContainerPtr>,
    }

    impl Node {
        fn new(value: i32) -> Self {
            Self {
                value,
                next: None,
                prev: None,
                parent: None,
            }
        }
    }

    #[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
    struct ContainerPtr(ArenaPtr<Container>);

    struct Container {
        head: Option<NodePtr>,
        tail: Option<NodePtr>,
    }

    #[derive(Default)]
    struct Context {
        nodes: Arena<Node>,
        containers: Arena<Container>,
    }

    impl_arena!(Context, Node, NodePtr, nodes);
    impl_arena!(Context, Container, ContainerPtr, containers);

    impl LinkedListContainerPtr<NodePtr> for ContainerPtr {
        fn head(self, ctx: &Self::A) -> Option<NodePtr> { self.deref(ctx).head }

        fn tail(self, ctx: &Self::A) -> Option<NodePtr> { self.deref(ctx).tail }

        fn set_head(self, ctx: &mut Self::A, head: Option<NodePtr>) {
            self.deref_mut(ctx).head = head;
        }

        fn set_tail(self, ctx: &mut Self::A, tail: Option<NodePtr>) {
            self.deref_mut(ctx).tail = tail;
        }
    }

    impl LinkedListNodePtr for NodePtr {
        type ContainerPtr = ContainerPtr;

        fn next(self, ctx: &Self::A) -> Option<Self> { self.deref(ctx).next }

        fn prev(self, ctx: &Self::A) -> Option<Self> { self.deref(ctx).prev }

        fn set_next(self, ctx: &mut Self::A, next: Option<Self>) {
            self.deref_mut(ctx).next = next;
        }

        fn set_prev(self, ctx: &mut Self::A, prev: Option<Self>) {
            self.deref_mut(ctx).prev = prev;
        }

        fn container(self, ctx: &Self::A) -> Option<Self::ContainerPtr> { self.deref(ctx).parent }

        fn set_container(self, ctx: &mut Self::A, container: Option<Self::ContainerPtr>) {
            self.deref_mut(ctx).parent = container;
        }
    }

    #[test]
    fn test_linked_list_0() {
        let mut ctx = Context::default();

        let container = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        let node1 = ctx.alloc(Node::new(1));
        let node2 = ctx.alloc(Node::new(2));
        let node3 = ctx.alloc(Node::new(3));

        container.push_front(&mut ctx, node1);

        assert_eq!(container.head(&ctx), Some(node1));
        assert_eq!(container.tail(&ctx), Some(node1));

        container.push_back(&mut ctx, node2);

        assert_eq!(container.head(&ctx), Some(node1));
        assert_eq!(container.tail(&ctx), Some(node2));
        assert_eq!(node1.next(&ctx), Some(node2));
        assert_eq!(node2.prev(&ctx), Some(node1));

        node2.insert_after(&mut ctx, node3);

        assert_eq!(container.head(&ctx), Some(node1));
        assert_eq!(container.tail(&ctx), Some(node3));
        assert_eq!(node2.next(&ctx), Some(node3));
        assert_eq!(node3.prev(&ctx), Some(node2));

        node1.unlink(&mut ctx);

        assert_eq!(container.head(&ctx), Some(node2));
        assert_eq!(container.tail(&ctx), Some(node3));
        assert_eq!(node2.prev(&ctx), None);
        assert_eq!(node3.next(&ctx), None);

        node3.unlink(&mut ctx);

        assert_eq!(container.head(&ctx), Some(node2));
        assert_eq!(container.tail(&ctx), Some(node2));
        assert_eq!(node2.prev(&ctx), None);
        assert_eq!(node2.next(&ctx), None);

        node2.unlink(&mut ctx);

        assert_eq!(container.head(&ctx), None);
        assert_eq!(container.tail(&ctx), None);
    }

    #[test]
    fn test_linked_list_1() {
        let mut ctx = Context::default();

        let container = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        let node1 = ctx.alloc(Node::new(1));
        let node2 = ctx.alloc(Node::new(2));
        let node3 = ctx.alloc(Node::new(3));

        container.push_back(&mut ctx, node1);

        assert_eq!(container.head(&ctx), Some(node1));
        assert_eq!(container.tail(&ctx), Some(node1));
        assert_eq!(node1.container(&ctx), Some(container));

        container.push_front(&mut ctx, node2);

        assert_eq!(container.head(&ctx), Some(node2));
        assert_eq!(container.tail(&ctx), Some(node1));
        assert_eq!(node2.next(&ctx), Some(node1));
        assert_eq!(node1.prev(&ctx), Some(node2));
        assert_eq!(node1.container(&ctx), Some(container));
        assert_eq!(node2.container(&ctx), Some(container));

        node2.insert_before(&mut ctx, node3);

        assert_eq!(container.head(&ctx), Some(node3));
        assert_eq!(container.tail(&ctx), Some(node1));
        assert_eq!(node2.next(&ctx), Some(node1));
        assert_eq!(node1.prev(&ctx), Some(node2));

        assert_eq!(node3.container(&ctx), Some(container));
        assert_eq!(node2.container(&ctx), Some(container));
        assert_eq!(node1.container(&ctx), Some(container));

        let expected = [3, 2, 1];

        for (ptr, expected) in container.iter(&ctx).zip(expected.into_iter()) {
            assert_eq!(
                ptr.deref(&ctx).value,
                expected,
                "expected: {}, got: {}",
                expected,
                ptr.deref(&ctx).value
            );
        }

        node1.unlink(&mut ctx);

        assert_eq!(container.head(&ctx), Some(node3));
        assert_eq!(container.tail(&ctx), Some(node2));

        node1.unlink(&mut ctx);

        assert_eq!(container.head(&ctx), Some(node3));
        assert_eq!(container.tail(&ctx), Some(node2));

        node2.unlink(&mut ctx);

        assert_eq!(container.head(&ctx), Some(node3));
        assert_eq!(container.tail(&ctx), Some(node3));

        node3.unlink(&mut ctx);

        assert_eq!(container.head(&ctx), None);
        assert_eq!(container.tail(&ctx), None);
    }

    #[test]
    fn test_container_merge_basic() {
        let mut ctx = Context::default();

        let container1 = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        let container2 = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        let node1 = ctx.alloc(Node::new(1));
        let node2 = ctx.alloc(Node::new(2));
        let node3 = ctx.alloc(Node::new(3));

        container1.push_back(&mut ctx, node1);
        container2.push_back(&mut ctx, node2);
        container2.push_back(&mut ctx, node3);

        container1.merge(&mut ctx, container2);

        assert_eq!(container1.head(&ctx), Some(node1));
        assert_eq!(node1.next(&ctx), Some(node2));
        assert_eq!(node2.prev(&ctx), Some(node1));
        assert_eq!(node2.next(&ctx), Some(node3));
        assert_eq!(node3.prev(&ctx), Some(node2));
        assert_eq!(container1.tail(&ctx), Some(node3));
        assert_eq!(container2.head(&ctx), None);
        assert_eq!(container2.tail(&ctx), None);
    }

    #[test]
    fn test_container_merge_empty() {
        let mut ctx = Context::default();

        let container1 = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        let container2 = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        container1.merge(&mut ctx, container2);

        assert_eq!(container1.head(&ctx), None);
        assert_eq!(container1.tail(&ctx), None);
        assert_eq!(container2.head(&ctx), None);
        assert_eq!(container2.tail(&ctx), None);
    }

    #[test]
    fn test_container_merge_many_empty() {
        // try to merge an empty container to a non-empty container
        let mut ctx = Context::default();

        let container1 = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        let container2 = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        for i in 0..10 {
            let node = ctx.alloc(Node::new(i));
            container1.push_back(&mut ctx, node);
        }
        assert_eq!(container1.head(&ctx).unwrap().deref(&ctx).value, 0);
        assert_eq!(container1.tail(&ctx).unwrap().deref(&ctx).value, 9);
        assert!(container2.head(&ctx).is_none());
        assert!(container2.tail(&ctx).is_none());

        container1.merge(&mut ctx, container2);

        assert_eq!(container1.head(&ctx).unwrap().deref(&ctx).value, 0);
        assert_eq!(container1.tail(&ctx).unwrap().deref(&ctx).value, 9);
        assert!(container2.head(&ctx).is_none());
        assert!(container2.tail(&ctx).is_none());
    }

    #[test]
    fn test_container_merge_empty_many() {
        // try to merge a non-empty container to an empty container
        let mut ctx = Context::default();

        let container1 = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        let container2 = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        for i in 0..10 {
            let node = ctx.alloc(Node::new(i));
            container2.push_back(&mut ctx, node);
        }
        assert!(container1.head(&ctx).is_none());
        assert!(container1.tail(&ctx).is_none());
        assert_eq!(container2.head(&ctx).unwrap().deref(&ctx).value, 0);
        assert_eq!(container2.tail(&ctx).unwrap().deref(&ctx).value, 9);

        container1.merge(&mut ctx, container2);

        assert_eq!(container1.head(&ctx).unwrap().deref(&ctx).value, 0);
        assert_eq!(container1.tail(&ctx).unwrap().deref(&ctx).value, 9);
        assert!(container2.head(&ctx).is_none());
        assert!(container2.tail(&ctx).is_none());
    }

    #[test]
    fn test_container_merge_many_many() {
        // try to merge two non-empty containers
        let mut ctx = Context::default();

        let container1 = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        let container2 = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        for i in 0..10 {
            let node = ctx.alloc(Node::new(i));
            container1.push_back(&mut ctx, node);
        }
        assert_eq!(container1.head(&ctx).unwrap().deref(&ctx).value, 0);
        assert_eq!(container1.tail(&ctx).unwrap().deref(&ctx).value, 9);
        assert!(container2.head(&ctx).is_none());
        assert!(container2.tail(&ctx).is_none());

        for i in 10..20 {
            let node = ctx.alloc(Node::new(i));
            container2.push_back(&mut ctx, node);
        }
        assert_eq!(container2.head(&ctx).unwrap().deref(&ctx).value, 10);
        assert_eq!(container2.tail(&ctx).unwrap().deref(&ctx).value, 19);

        container1.merge(&mut ctx, container2);

        assert_eq!(container1.head(&ctx).unwrap().deref(&ctx).value, 0);
        assert_eq!(container1.tail(&ctx).unwrap().deref(&ctx).value, 19);

        assert!(container2.head(&ctx).is_none());
        assert!(container2.tail(&ctx).is_none());

        // check each value
        for (ptr, expected) in container1.iter(&ctx).zip(0..20) {
            assert_eq!(
                ptr.deref(&ctx).value,
                expected,
                "expected: {}, got: {}",
                expected,
                ptr.deref(&ctx).value
            );
            assert_eq!(ptr.container(&ctx), Some(container1));
        }
    }

    #[test]
    fn test_container_split() {
        let mut ctx = Context::default();

        let container = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        let node1 = ctx.alloc(Node::new(1));
        let node2 = ctx.alloc(Node::new(2));
        let node3 = ctx.alloc(Node::new(3));
        let node4 = ctx.alloc(Node::new(4));
        let node5 = ctx.alloc(Node::new(5));

        container.push_back(&mut ctx, node1);
        container.push_back(&mut ctx, node2);
        container.push_back(&mut ctx, node3);
        container.push_back(&mut ctx, node4);
        container.push_back(&mut ctx, node5);

        let new_container = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        container.split(&mut ctx, new_container, node3);

        assert_eq!(container.head(&ctx).unwrap().deref(&ctx).value, 1);
        assert_eq!(container.tail(&ctx).unwrap().deref(&ctx).value, 3);
        assert_eq!(node1.container(&ctx), Some(container));
        assert_eq!(node2.container(&ctx), Some(container));
        assert_eq!(node3.container(&ctx), Some(container));

        assert_eq!(new_container.head(&ctx).unwrap().deref(&ctx).value, 4);
        assert_eq!(new_container.tail(&ctx).unwrap().deref(&ctx).value, 5);
        assert_eq!(node4.container(&ctx), Some(new_container));
        assert_eq!(node5.container(&ctx), Some(new_container));
    }

    #[test]
    fn test_container_split_non_empty() {
        let mut ctx = Context::default();

        let container = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        let node1 = ctx.alloc(Node::new(1));
        let node2 = ctx.alloc(Node::new(2));
        let node3 = ctx.alloc(Node::new(3));
        let node4 = ctx.alloc(Node::new(4));
        let node5 = ctx.alloc(Node::new(5));

        container.push_back(&mut ctx, node1);
        container.push_back(&mut ctx, node2);
        container.push_back(&mut ctx, node3);
        container.push_back(&mut ctx, node4);
        container.push_back(&mut ctx, node5);

        let new_container = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        let new_node1 = ctx.alloc(Node::new(6));
        let new_node2 = ctx.alloc(Node::new(7));

        new_container.push_back(&mut ctx, new_node1);
        new_container.push_back(&mut ctx, new_node2);

        container.split(&mut ctx, new_container, node3);

        assert_eq!(container.head(&ctx).unwrap().deref(&ctx).value, 1);
        assert_eq!(container.tail(&ctx).unwrap().deref(&ctx).value, 3);

        assert_eq!(new_container.head(&ctx).unwrap().deref(&ctx).value, 4);
        assert_eq!(new_container.tail(&ctx).unwrap().deref(&ctx).value, 7);

        for (ptr, expected) in container.iter(&ctx).zip(1..=3) {
            assert_eq!(
                ptr.deref(&ctx).value,
                expected,
                "expected: {}, got: {}",
                expected,
                ptr.deref(&ctx).value
            );
            assert_eq!(ptr.container(&ctx), Some(container));
        }

        for (ptr, expected) in new_container.iter(&ctx).zip(4..=7) {
            assert_eq!(
                ptr.deref(&ctx).value,
                expected,
                "expected: {}, got: {}",
                expected,
                ptr.deref(&ctx).value
            );
            assert_eq!(ptr.container(&ctx), Some(new_container));
        }
    }

    #[test]
    fn test_linked_list_cursor() {
        let mut ctx = Context::default();

        let container = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        let node1 = ctx.alloc(Node::new(1));
        let node2 = ctx.alloc(Node::new(2));
        let node3 = ctx.alloc(Node::new(3));
        let node4 = ctx.alloc(Node::new(4));
        let node5 = ctx.alloc(Node::new(5));

        container.push_back(&mut ctx, node1);
        container.push_back(&mut ctx, node2);
        container.push_back(&mut ctx, node3);
        container.push_back(&mut ctx, node4);
        container.push_back(&mut ctx, node5);

        let mut cursor = container.cursor();

        assert_eq!(cursor.next(&ctx).unwrap().deref(&ctx).value, 1);
        assert_eq!(cursor.next(&ctx).unwrap().deref(&ctx).value, 2);

        let node = cursor.next(&ctx).unwrap();
        assert_eq!(node.deref(&ctx).value, 3);

        let new_node = ctx.alloc(Node::new(6));
        node.insert_after(&mut ctx, new_node);

        assert_eq!(cursor.next(&ctx).unwrap().deref(&ctx).value, 6);
        assert_eq!(cursor.next(&ctx).unwrap().deref(&ctx).value, 4);
        assert_eq!(cursor.next(&ctx).unwrap().deref(&ctx).value, 5);
        assert!(cursor.next(&ctx).is_none());
        // because `None` is hit, so the cursor will start from the beginning
        assert_eq!(cursor.next(&ctx).unwrap().deref(&ctx).value, 1);
        assert_eq!(cursor.next(&ctx).unwrap().deref(&ctx).value, 2);
        assert_eq!(cursor.next(&ctx).unwrap().deref(&ctx).value, 3);
        assert_eq!(cursor.next(&ctx).unwrap().deref(&ctx).value, 6);
        assert_eq!(cursor.next(&ctx).unwrap().deref(&ctx).value, 4);
        assert_eq!(cursor.next(&ctx).unwrap().deref(&ctx).value, 5);
    }

    #[test]
    fn test_linked_list_cursor_rev() {
        let mut ctx = Context::default();

        let container = ctx.alloc(Container {
            head: None,
            tail: None,
        });

        let node1 = ctx.alloc(Node::new(1));
        let node2 = ctx.alloc(Node::new(2));
        let node3 = ctx.alloc(Node::new(3));
        let node4 = ctx.alloc(Node::new(4));
        let node5 = ctx.alloc(Node::new(5));

        container.push_back(&mut ctx, node1);
        container.push_back(&mut ctx, node2);
        container.push_back(&mut ctx, node3);
        container.push_back(&mut ctx, node4);
        container.push_back(&mut ctx, node5);

        let mut cursor = container.cursor();

        assert_eq!(cursor.prev(&ctx).unwrap().deref(&ctx).value, 5);
        assert_eq!(cursor.prev(&ctx).unwrap().deref(&ctx).value, 4);

        let node = cursor.prev(&ctx).unwrap();
        assert_eq!(node.deref(&ctx).value, 3);

        // lets insert after the node, but check later.
        let new_node = ctx.alloc(Node::new(123));
        node.insert_after(&mut ctx, new_node);

        let new_node = ctx.alloc(Node::new(6));
        node.insert_before(&mut ctx, new_node);

        // insert_before will affect the cursor.
        assert_eq!(cursor.prev(&ctx).unwrap().deref(&ctx).value, 6);

        assert_eq!(cursor.prev(&ctx).unwrap().deref(&ctx).value, 2);
        assert_eq!(cursor.prev(&ctx).unwrap().deref(&ctx).value, 1);
        assert!(cursor.prev(&ctx).is_none());
        // because `None` is hit, so the cursor will start from the end
        assert_eq!(cursor.prev(&ctx).unwrap().deref(&ctx).value, 5);
        assert_eq!(cursor.prev(&ctx).unwrap().deref(&ctx).value, 4);
        // insert after will now show up
        assert_eq!(cursor.prev(&ctx).unwrap().deref(&ctx).value, 123);
        assert_eq!(cursor.prev(&ctx).unwrap().deref(&ctx).value, 3);
        assert_eq!(cursor.prev(&ctx).unwrap().deref(&ctx).value, 6);
        assert_eq!(cursor.prev(&ctx).unwrap().deref(&ctx).value, 2);
        assert_eq!(cursor.prev(&ctx).unwrap().deref(&ctx).value, 1);
    }
}
