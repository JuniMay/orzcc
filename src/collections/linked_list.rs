use super::storage::ArenaPtrLike;

/// A container of linked lists.
///
/// # Type Parameters
///
/// A container might contain multiple linked lists, so the type of the nodes in
/// the linked list is a type parameter.
///
/// - `NodePtr`: The pointer type of the nodes in the linked list.
pub trait LinkedListContainerPtr<NodePtr>: ArenaPtrLike
where
    NodePtr: LinkedListPtr<A = Self::A, ContainerPtr = Self>,
{
    fn head(self, ctx: &Self::A) -> Option<NodePtr>;

    fn set_head(self, ctx: &mut Self::A, head: Option<NodePtr>);

    fn tail(self, ctx: &Self::A) -> Option<NodePtr>;

    fn set_tail(self, ctx: &mut Self::A, tail: Option<NodePtr>);

    fn push_front(self, ctx: &mut Self::A, node: NodePtr) {
        assert!(
            node.container(ctx).is_none(),
            "the node is already in another container"
        );

        if let Some(head) = self.head(ctx) {
            // just insert the node before the current head
            LinkedListPtr::insert_before(head, ctx, node);
        } else {
            // the list is empty, set the node as the head and tail
            self.set_head(ctx, Some(node));
            self.set_tail(ctx, Some(node));
            node.set_container(ctx, Some(self))
        }
    }

    fn push_back(self, ctx: &mut Self::A, node: NodePtr) {
        assert!(
            node.container(ctx).is_none(),
            "the node is already in another container"
        );

        if let Some(tail) = self.tail(ctx) {
            // just insert the node after the current tail
            LinkedListPtr::insert_after(tail, ctx, node);
        } else {
            // the list is empty, set the node as the head and tail
            self.set_head(ctx, Some(node));
            self.set_tail(ctx, Some(node));
            node.set_container(ctx, Some(self))
        }
    }

    fn merge(self, ctx: &mut Self::A, other: Self) {
        // TODO: merge the other list into this list
        unimplemented!()
    }

    fn iter(self, ctx: &Self::A) -> LinkedListIterator<NodePtr> {
        LinkedListIterator {
            ctx,
            curr_front: self.head(ctx),
            curr_back: self.tail(ctx),
        }
    }
}

pub struct LinkedListIterator<'a, T: LinkedListPtr> {
    ctx: &'a T::A,
    /// The current front node.
    curr_front: Option<T>,
    /// The current back node.
    ///
    /// This is used to implement [DoubleEndedIterator]
    curr_back: Option<T>,
}

impl<'a, T: LinkedListPtr> Iterator for LinkedListIterator<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        let curr = self.curr_front?;
        let next = curr.next(self.ctx);
        self.curr_front = next;
        Some(curr)
    }
}

impl<'a, T: LinkedListPtr> DoubleEndedIterator for LinkedListIterator<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        let curr = self.curr_back?;
        let prev = curr.prev(self.ctx);
        self.curr_back = prev;
        Some(curr)
    }
}

/// High level interfaces for linked list node.
pub trait LinkedListPtr: ArenaPtrLike {
    /// The type of the container of the node.
    ///
    /// Container is the data structure that contains the linked list. For
    /// example, the instructions are contained in a block.
    type ContainerPtr: LinkedListContainerPtr<Self, A = Self::A>;

    /// Get the next node.
    fn next(self, ctx: &Self::A) -> Option<Self>;

    /// Get the previous node.
    fn prev(self, ctx: &Self::A) -> Option<Self>;

    /// Set the next node.
    fn set_next(self, ctx: &mut Self::A, next: Option<Self>);

    /// Set the previous node.
    fn set_prev(self, ctx: &mut Self::A, prev: Option<Self>);

    /// Get the container of the node.
    fn container(self, ctx: &Self::A) -> Option<Self::ContainerPtr>;

    fn set_container(self, ctx: &mut Self::A, container: Option<Self::ContainerPtr>);

    /// Insert a node after the current node.
    ///
    /// # Notes
    ///
    /// After insertion, the tail of the container may need to be updated.
    fn insert_after(self, ctx: &mut Self::A, node: Self) {
        assert!(
            self.container(ctx).is_some(),
            "cannot insert after a node without container"
        );
        assert!(
            node.container(ctx).is_none(),
            "cannot insert a node that already belongs to a container"
        );

        let next = self.next(ctx);
        if let Some(next) = next {
            next.set_prev(ctx, Some(node));
            node.set_next(ctx, Some(next));
        }

        node.set_prev(ctx, Some(self));
        self.set_next(ctx, Some(node));

        if let Some(container) = self.container(ctx) {
            if container.tail(ctx) == Some(self) {
                container.set_tail(ctx, Some(node));
            }
        } else {
            unreachable!("container should not be None")
        }

        node.set_container(ctx, self.container(ctx));
    }

    /// Insert a node before the current node.
    fn insert_before(self, ctx: &mut Self::A, node: Self) {
        assert!(
            self.container(ctx).is_some(),
            "cannot insert before a node without container"
        );
        assert!(
            node.container(ctx).is_none(),
            "cannot insert a node that already belongs to a container"
        );

        let prev = self.prev(ctx);
        if let Some(prev) = prev {
            prev.set_next(ctx, Some(node));
            node.set_prev(ctx, Some(prev));
        }

        node.set_next(ctx, Some(self));
        self.set_prev(ctx, Some(node));

        if let Some(container) = self.container(ctx) {
            if container.head(ctx) == Some(self) {
                container.set_head(ctx, Some(node));
            }
        } else {
            unreachable!("container should not be None")
        }

        node.set_container(ctx, self.container(ctx));
    }

    /// Unlink the current node.
    fn unlink(self, ctx: &mut Self::A) {
        let prev = self.prev(ctx);
        let next = self.next(ctx);

        if let Some(prev) = prev {
            prev.set_next(ctx, next);
        }

        if let Some(next) = next {
            next.set_prev(ctx, prev);
        }

        if let Some(container) = self.container(ctx) {
            if container.head(ctx) == Some(self) {
                container.set_head(ctx, next);
            }

            if container.tail(ctx) == Some(self) {
                container.set_tail(ctx, prev);
            }
        }

        self.set_prev(ctx, None);
        self.set_next(ctx, None);
        self.set_container(ctx, None);
    }
}

#[cfg(test)]
mod tests {
    use super::{LinkedListContainerPtr, LinkedListPtr};
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

    impl LinkedListPtr for NodePtr {
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
    }
}
