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
    fn merge(self, arena: &mut Self::A, other: Self) {
        // TODO: merge the other list into this list
        unimplemented!()
    }

    /// Get an immutable iterator of the linked list.
    fn iter(self, arena: &Self::A) -> LinkedListIterator<NodePtr> {
        LinkedListIterator {
            arena,
            curr_front: self.head(arena),
            curr_back: self.tail(arena),
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
        let curr = self.curr_front?;
        let next = curr.next(self.arena);
        self.curr_front = next;
        Some(curr)
    }
}

impl<'a, T: LinkedListNodePtr> DoubleEndedIterator for LinkedListIterator<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let curr = self.curr_back?;
        let prev = curr.prev(self.arena);
        self.curr_back = prev;
        Some(curr)
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

        container.push_front(&mut ctx, node2);

        assert_eq!(container.head(&ctx), Some(node2));
        assert_eq!(container.tail(&ctx), Some(node1));
        assert_eq!(node2.next(&ctx), Some(node1));
        assert_eq!(node1.prev(&ctx), Some(node2));

        node2.insert_before(&mut ctx, node3);

        assert_eq!(container.head(&ctx), Some(node3));
        assert_eq!(container.tail(&ctx), Some(node1));
        assert_eq!(node2.next(&ctx), Some(node1));
        assert_eq!(node1.prev(&ctx), Some(node2));

        let expected = [3, 2, 1];

        for (ptr, expected) in container.iter(&ctx).zip(expected.iter()) {
            assert_eq!(
                ptr.deref(&ctx).value,
                *expected,
                "value mismatch, expected {}",
                expected
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
}
