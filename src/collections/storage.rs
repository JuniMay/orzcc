//! Storage utilities.
//!
//! This module provides [BaseArena] for managing memory allocation and
//! deallocation. The arena is mainly used to deal with the linked data
//! structures in the IR.
//!
//! It is better to wrap the arena with a high-level container, because the
//! arena itself can easily panic. So this module also provides some traits to
//! help with the wrapping.
//!
//! - [ArenaPtr]: The trait for the pointer in the arena.
//! - [ArenaDeref]: The trait for dereferencing the arena pointer.
//! - [ArenaAlloc]: The trait for allocating memory in the arena.
//! - [ArenaFree]: The trait for freeing memory in the arena.
//!
//! The traits above can be used to wrap one or multiple arenas in a high-level
//! container, e.g., a context or a module in the IR.
//!
//! # Examples
//!
//! The arena can be used for a single type, or be combined into a container
//! of instances of different types.
//!
//! ## Usage of Arenas
//!
//! Here is a simple example demonstrating how to use the [BaseArena] and
//! [BaseArenaPtr].
//!
//! ```rust
//! use orzcc::collections::storage::*;
//!
//! #[derive(Debug, PartialEq, Eq)]
//! struct Test {
//!     a: i32,
//!     b: i32,
//!     // Self-referential struct
//!     this: BaseArenaPtr<Test>,
//! }
//!
//! let mut arena = BaseArena::default();
//!
//! // We can create a self-referential struct using `alloc_with`
//! let ptr1 = arena.alloc_with(|this| Test { a: 1, b: 2, this });
//! let ptr2 = arena.alloc_with(|this| Test { a: 3, b: 4, this });
//!
//! // We can deref the pointer to get the value
//! assert_eq!(arena.try_deref(ptr1).unwrap().a, 1);
//! assert_eq!(arena.try_deref(ptr2).unwrap().b, 4);
//!
//! // Allocated pointers are different if none of them are freed
//! assert_ne!(ptr1, ptr2);
//!
//! // Free the pointer
//! arena.free(ptr1);
//! // Now we cannot deref the pointer (got `None`)
//! assert_eq!(arena.try_deref(ptr1), None);
//!
//! // Allocate a new pointer, the old one will be reused
//! let ptr3 = arena.alloc_with(|this| Test { a: 5, b: 6, this });
//! assert_eq!(ptr1, ptr3); // Note that ptr1 is now dangling
//!
//! // Additionally, we can modify the value using `deref_mut`
//! arena.try_deref_mut(ptr3).unwrap().a = 7;
//! assert_eq!(arena.try_deref(ptr3).unwrap().a, 7);
//! ```
//!
//! ## Combine Two [BaseArena]s
//!
//! Below is an example of how to combine two arenas for two different types
//! into a high-level container.
//!
//! ```rust
//! use orzcc::impl_arena;
//! use orzcc::collections::storage::*;
//!
//! // Two self-referential structs
//! struct Foo { this: FooPtr, value: i32 }
//! struct Bar { this: BarPtr, value: f32 }
//!
//! // And two arena pointers, just wrappers, because [BaseArenaPtr] is already
//! // associated with [BaseArena]
//! #[derive(Clone, Copy, PartialEq, Eq)]
//! struct FooPtr(BaseArenaPtr<Foo>);
//!
//! #[derive(Clone, Copy, PartialEq, Eq)]
//! struct BarPtr(BaseArenaPtr<Bar>);
//!
//! // A high-level container for the two arenas
//! #[derive(Default)]
//! struct FooBarArena {
//!     foo_arena: BaseArena<Foo>,
//!     bar_arena: BaseArena<Bar>,
//! }
//!
//! // Now we can implement the pointers for the high-level container
//! impl_arena!(FooBarArena, Foo, FooPtr, foo_arena);
//! impl_arena!(FooBarArena, Bar, BarPtr, bar_arena);
//!
//! // Create an instance of the arena container
//! let mut arena = FooBarArena::default();
//!
//! // Use `alloc_with` to realize self-referential structs
//! let foo = arena.alloc_with(|ptr| Foo { this: ptr, value: 42 });
//! let bar = arena.alloc_with(|ptr| Bar { this: ptr, value: 3.14 });
//!
//! // The instance can be deref-ed from the arena, according to the type.
//! assert_eq!(arena.try_deref(foo).unwrap().value, 42);
//! assert_eq!(arena.try_deref(bar).unwrap().value, 3.14);
//!
//! // Alternatively, the instance can be deref-ed from the pointer (chain
//! // style).
//! assert_eq!(foo.deref(&arena).value, 42);
//! assert_eq!(bar.deref(&arena).value, 3.14);
//!
//! // Also, we can modify the value using `deref_mut`.
//! foo.deref_mut(&mut arena).value = 100;
//! assert_eq!(foo.deref(&arena).value, 100);
//! ```
//!
//! Thanks to the type system of rust, `deref` and `deref_mut` will be decided
//! by the compiler according to the type of the pointer. This can help to avoid
//! the complex qualified paths.

use std::{
    collections::{HashMap, HashSet, VecDeque},
    fmt,
    hash::{DefaultHasher, Hash, Hasher},
    marker::PhantomData,
};

/// Indicates that the type can be used to dereference an arena pointer.
///
/// This trait abstracts the dereferencing operation for an arena-like type,
/// which is the most basic operation for an arena.
pub trait ArenaDeref<T, Ptr>
where
    Ptr: ArenaPtr<T = T, A = Self>,
{
    /// Try to dereference a pointer and get a value in the arena.
    ///
    /// # Parameters
    ///
    /// - `ptr`: The pointer to the value.
    ///
    /// # Returns
    ///
    /// An option that contains:
    ///
    /// - `Some(&T)` if the pointer is in bounds.
    /// - `None` if the pointer is out of bounds or the slot is vacant.
    ///
    /// # See Also
    ///
    /// - [ArenaDeref::try_deref_mut]
    fn try_deref(&self, ptr: Ptr) -> Option<&T>;

    /// Try to dereference a pointer and get a mutable value in the arena.
    ///
    /// # Parameters
    ///
    /// - `ptr`: The pointer to the value.
    ///
    /// # Returns
    ///
    /// An option that contains:
    ///
    /// - `Some(&mut T)` if the pointer is in bounds.
    /// - `None` if the pointer is out of bounds or the slot is vacant.
    ///
    /// # See Also
    ///
    /// - [ArenaDeref::try_deref]
    fn try_deref_mut(&mut self, ptr: Ptr) -> Option<&mut T>;
}

/// Indicates that the type can be used to allocate values in the arena.
///
/// This trait abstracts the allocation operation for an arena-like type, and
/// does not necessarily require the type to be able to free values. All the
/// space will be freed when the arena is dropped.
///
/// # See Also
///
/// - [ArenaFree]
pub trait ArenaAlloc<T, Ptr>: ArenaDeref<T, Ptr>
where
    Ptr: ArenaPtr<T = T, A = Self>,
{
    /// Allocate a value with a closure accepting the index.
    ///
    /// This will first reserve the pointer, pass it to the closure, and then
    /// get the value from the closure's return value and store it in the arena.
    ///
    /// This allocation is useful when the value needs to reference itself.
    ///
    /// # Parameters
    ///
    /// - `f`: The closure that accepts the pointer and returns the value.
    ///
    /// # Returns
    ///
    /// The arena pointer to the value.
    ///
    /// # Type Parameters
    ///
    /// - `F`: The type of the closure, which is [FnOnce] that accepts the arena
    ///   pointer and returns the value.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use orzcc::collections::storage::{BaseArena, BaseArenaPtr, ArenaAlloc};
    ///
    /// struct Node {
    ///     this: BaseArenaPtr<Node>,
    ///     value: i32,
    ///     next: Option<BaseArenaPtr<Node>>,
    /// }
    /// let mut arena = BaseArena::default();
    /// let node = arena.alloc_with(|this| Node {
    ///     this,
    ///     value: 42,
    ///     next: None,
    /// });
    /// ```
    ///
    /// # See Also
    ///
    /// - [ArenaAlloc::alloc]
    fn alloc_with<F>(&mut self, f: F) -> Ptr
    where
        F: FnOnce(Ptr) -> T;

    /// Allocate a value in the arena.
    ///
    /// By default, this will call [ArenaAlloc::alloc_with] with a closure
    /// that returns the value directly.
    ///
    /// # Parameters
    ///
    /// - `val`: The value to allocate.
    ///
    /// # Returns
    ///
    /// The arena pointer to the value.
    ///
    /// # See Also
    ///
    /// - [ArenaAlloc::alloc_with]
    fn alloc(&mut self, val: T) -> Ptr { self.alloc_with(|_| val) }
}

/// Indicates that the type can be used to free values in the arena.
///
/// This trait abstracts the freeing operation for an arena-like type, and thus
/// requires the type to be able to allocate values as well.
pub trait ArenaFree<T, Ptr>: ArenaAlloc<T, Ptr>
where
    Ptr: ArenaPtr<T = T, A = Self>,
{
    /// Free a value in the arena.
    ///
    /// # Parameters
    ///
    /// - `ptr`: The pointer to the value.
    ///
    /// # Panics
    ///
    /// This should panic if the pointer is out of bounds, i.e., double freeing
    /// or freeing an out-of-bounds/non-existent pointer.
    fn free(&mut self, ptr: Ptr);
}

/// The pointer-like trait that can be used to deref and get the value from the
/// corresponding [ArenaDeref] type.
pub trait ArenaPtr: Copy + Sized + Eq {
    /// The type of dereferenced value.
    type T;

    /// The type of the corresponding arena.
    type A: ArenaDeref<Self::T, Self>;

    /// Try to dereference the pointer.
    ///
    /// # Parameters
    ///
    /// - `arena`: The arena that stores the object.
    ///
    /// # Returns
    ///
    /// An option that contains:
    ///
    /// - `Some(&T)` if the pointer is in bounds.
    /// - `None` if the pointer is out of bounds.
    fn try_deref(self, arena: &Self::A) -> Option<&Self::T>;

    /// Try to dereference the pointer mutably.
    ///
    /// # Parameters
    ///
    /// - `arena`: The arena that stores the object.
    ///
    /// # Returns
    ///
    /// An option that contains:
    ///
    /// - `Some(&mut T)` if the pointer is in bounds.
    /// - `None` if the pointer is out of bounds.
    fn try_deref_mut(self, arena: &mut Self::A) -> Option<&mut Self::T>;

    /// Dereference the pointer.
    ///
    /// # Parameters
    ///
    /// - `arena`: The arena that stores the object.
    ///
    /// # Returns
    ///
    /// A reference to the object.
    fn deref(self, arena: &Self::A) -> &Self::T {
        self.try_deref(arena).expect("the arena pointer is invalid")
    }

    /// Dereference the pointer mutably.
    ///
    /// # Parameters
    ///
    /// - `arena`: The arena that stores the object.
    ///
    /// # Returns
    ///
    /// A mutable reference to the object.
    ///
    /// # Panics
    ///
    /// Panics if the pointer is out of bounds.
    fn deref_mut(self, arena: &mut Self::A) -> &mut Self::T {
        self.try_deref_mut(arena)
            .expect("the arena pointer is invalid")
    }
}

/// [BaseArenaPtr] is a pointer to an object in the [BaseArena].
///
/// This can be understood as a handle, one can dereference it to get the
/// reference (in the form of `&T`) to the object in the arena.
pub struct BaseArenaPtr<T> {
    id: usize,
    _marker: PhantomData<T>,
}

impl<T> fmt::Debug for BaseArenaPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "BaseArenaPtr({})", self.id)
    }
}

impl<T> PartialEq for BaseArenaPtr<T> {
    fn eq(&self, other: &Self) -> bool { self.id == other.id }
}

impl<T> Eq for BaseArenaPtr<T> {}

impl<T> Hash for BaseArenaPtr<T> {
    fn hash<H: Hasher>(&self, state: &mut H) { self.id.hash(state); }
}

impl<T> From<usize> for BaseArenaPtr<T> {
    fn from(id: usize) -> Self {
        BaseArenaPtr {
            id,
            _marker: PhantomData,
        }
    }
}

#[allow(clippy::non_canonical_clone_impl)]
impl<T> Clone for BaseArenaPtr<T> {
    fn clone(&self) -> Self {
        // `Clone` will not be implemented for `T` when `T` is not `Clone`-able.
        BaseArenaPtr {
            id: self.id,
            _marker: PhantomData,
        }
    }
}

impl<T> Copy for BaseArenaPtr<T> {}

impl<T> BaseArenaPtr<T> {
    /// Get the inner index.
    ///
    /// # Returns
    ///
    /// A [usize] that represents the index of the object in the arena.
    pub fn id(self) -> usize { self.id }
}

impl<T> ArenaPtr for BaseArenaPtr<T> {
    type A = BaseArena<T>;
    type T = T;

    fn try_deref(self, arena: &BaseArena<T>) -> Option<&T> { arena.try_deref(self) }

    fn try_deref_mut(self, arena: &mut BaseArena<T>) -> Option<&mut T> { arena.try_deref_mut(self) }

    fn deref(self, arena: &BaseArena<T>) -> &T {
        self.try_deref(arena).expect("arena pointer out of bounds")
    }

    fn deref_mut(self, arena: &mut BaseArena<T>) -> &mut T {
        self.try_deref_mut(arena)
            .expect("arena pointer out of bounds")
    }
}

/// The entry kind in [BaseArena].
///
/// The entry can be either vacant or occupied.
pub enum BaseArenaEntry<T> {
    /// The slot is vacant.
    ///
    /// The vacant slot will occur when an entry is freed.
    Vacant,
    /// The slot is occupied.
    ///
    /// All allocated entries will be stored in the occupied slot.
    Occupied(T),
}

/// A simple arena implemented with a vector and a free list.
///
/// Arena can be useful when the data structure is a linked data structure or
/// a self-referential data structure. Also, arena can sometimes lead to better
/// spatial locality.
///
/// This can also be used to compose a more complex arena.
pub struct BaseArena<T> {
    /// The pool of entries.
    pool: Vec<BaseArenaEntry<T>>,

    /// The free list.
    ///
    /// The free list is a set of indices that are free to use. The freed
    /// indices will be pushed to the back and popped from the front of the
    /// when allocating new entries.
    free: VecDeque<usize>,
}

impl<T> Default for BaseArena<T> {
    fn default() -> Self {
        BaseArena {
            pool: Vec::new(),
            free: VecDeque::new(),
        }
    }
}

impl<T> ArenaAlloc<T, BaseArenaPtr<T>> for BaseArena<T> {
    fn alloc_with<F>(&mut self, f: F) -> BaseArenaPtr<T>
    where
        F: FnOnce(BaseArenaPtr<T>) -> T,
    {
        let index = if let Some(index) = self.free.pop_front() {
            index
        } else {
            let index = self.pool.len();
            self.pool.push(BaseArenaEntry::Vacant);
            index
        };
        let ptr = BaseArenaPtr::from(index);
        self.pool[index] = BaseArenaEntry::Occupied(f(ptr));
        ptr
    }
}

impl<T> ArenaFree<T, BaseArenaPtr<T>> for BaseArena<T> {
    fn free(&mut self, ptr: BaseArenaPtr<T>) {
        if let BaseArenaEntry::Vacant = self.pool[ptr.id()] {
            panic!("the arena pointer is invalid, double free may occur")
        }
        // this will panic if the index is out of bounds
        self.pool[ptr.id()] = BaseArenaEntry::Vacant;
        // just push the index to the back of the queue
        self.free.push_back(ptr.id());
    }
}

impl<T> ArenaDeref<T, BaseArenaPtr<T>> for BaseArena<T> {
    fn try_deref(&self, ptr: BaseArenaPtr<T>) -> Option<&T> {
        let entry = self.pool.get(ptr.id())?;
        match entry {
            BaseArenaEntry::Vacant => None,
            BaseArenaEntry::Occupied(val) => Some(val),
        }
    }

    fn try_deref_mut(&mut self, ptr: BaseArenaPtr<T>) -> Option<&mut T> {
        let entry = self.pool.get_mut(ptr.id())?;
        match entry {
            BaseArenaEntry::Vacant => None,
            BaseArenaEntry::Occupied(val) => Some(val),
        }
    }
}

impl<T> BaseArena<T> {
    /// Reserve a given capacity for the arena.
    ///
    /// # Parameters
    ///
    /// - `additional`: The additional capacity to reserve.
    ///
    /// # Panics
    ///
    /// This function will panic if the new capacity exceeds [isize::MAX] bytes.
    ///
    /// # See Also
    ///
    /// - [Vec::reserve]
    pub fn reserve(&mut self, additional: usize) { self.pool.reserve(additional); }

    /// Iterate over the arena.
    ///
    /// # Returns
    ///
    /// An iterator that yields the arena pointer and the value.
    ///
    /// # See Also
    ///
    /// - [BaseArena::iter_mut]
    pub fn iter(&self) -> impl Iterator<Item = (BaseArenaPtr<T>, &T)> {
        self.pool
            .iter()
            .enumerate()
            .filter_map(|(index, entry)| match entry {
                BaseArenaEntry::Vacant => None,
                BaseArenaEntry::Occupied(val) => Some((BaseArenaPtr::from(index), val)),
            })
    }

    /// Iterate over the arena mutably.
    ///
    /// # Returns
    ///
    /// An iterator that yields the arena pointer and the mutable value.
    ///
    /// # See Also
    ///
    /// - [BaseArena::iter]
    pub fn iter_mut(&mut self) -> impl Iterator<Item = (BaseArenaPtr<T>, &mut T)> {
        self.pool
            .iter_mut()
            .enumerate()
            .filter_map(|(index, entry)| match entry {
                BaseArenaEntry::Vacant => None,
                BaseArenaEntry::Occupied(val) => Some((BaseArenaPtr::from(index), val)),
            })
    }
}

/// Implement the arena trait for a given type.
#[macro_export]
macro_rules! impl_arena {
    ($arena:ty, $value:ty, $ptr:path, $field:ident) => {
        impl $crate::collections::storage::ArenaPtr for $ptr {
            type A = $arena;
            type T = $value;

            fn try_deref(self, arena: &Self::A) -> Option<&Self::T> {
                $crate::collections::storage::ArenaDeref::try_deref(arena, self)
            }

            fn try_deref_mut(self, arena: &mut Self::A) -> Option<&mut Self::T> {
                $crate::collections::storage::ArenaDeref::try_deref_mut(arena, self)
            }
        }

        impl $crate::collections::storage::ArenaAlloc<$value, $ptr> for $arena {
            fn alloc_with<F>(&mut self, f: F) -> $ptr
            where
                F: FnOnce($ptr) -> $value,
            {
                $ptr(self.$field.alloc_with(|ptr| f($ptr(ptr))))
            }
        }

        impl $crate::collections::storage::ArenaDeref<$value, $ptr> for $arena {
            fn try_deref(&self, ptr: $ptr) -> Option<&$value> { self.$field.try_deref(ptr.0) }

            fn try_deref_mut(&mut self, ptr: $ptr) -> Option<&mut $value> {
                self.$field.try_deref_mut(ptr.0)
            }
        }

        impl $crate::collections::storage::ArenaFree<$value, $ptr> for $arena {
            fn free(&mut self, ptr: $ptr) { self.$field.free(ptr.0) }
        }
    };
}

/// A unique hash for the arena.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct UniqueArenaHash(u64);

impl UniqueArenaHash {
    pub fn new<T: Hash + 'static + ?Sized>(val: &T) -> Self {
        let mut hasher = DefaultHasher::new();
        val.hash(&mut hasher);
        std::any::TypeId::of::<T>().hash(&mut hasher);
        UniqueArenaHash(hasher.finish())
    }
}

pub trait GetUniqueArenaHash {
    fn unique_arena_hash(&self) -> UniqueArenaHash;
}

impl<T> GetUniqueArenaHash for T
where
    T: Hash + 'static + ?Sized,
{
    fn unique_arena_hash(&self) -> UniqueArenaHash { UniqueArenaHash::new(self) }
}

/// A unique arena.
///
/// The unique arena is an arena that ensures that each value is unique, and can
/// be used to implement singleton-like behavior.
pub struct UniqueArena<T>
where
    T: GetUniqueArenaHash + Eq,
{
    arena: BaseArena<T>,
    unique_map: HashMap<UniqueArenaHash, HashSet<BaseArenaPtr<T>>>,
}

#[derive(PartialEq, Eq, Hash)]
pub struct UniqueArenaPtr<T>(BaseArenaPtr<T>);

impl<T> fmt::Debug for UniqueArenaPtr<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "UniqueArenaPtr({:?})", self.0.id())
    }
}

#[allow(clippy::non_canonical_clone_impl)]
impl<T> Clone for UniqueArenaPtr<T> {
    fn clone(&self) -> Self { UniqueArenaPtr(self.0) }
}

impl<T> Copy for UniqueArenaPtr<T> {}

impl<T> Default for UniqueArena<T>
where
    T: GetUniqueArenaHash + Eq,
{
    fn default() -> Self {
        UniqueArena {
            arena: BaseArena::default(),
            unique_map: HashMap::new(),
        }
    }
}

impl<T> ArenaPtr for UniqueArenaPtr<T>
where
    T: GetUniqueArenaHash + Eq,
{
    type A = UniqueArena<T>;
    type T = T;

    fn try_deref(self, arena: &Self::A) -> Option<&Self::T> { arena.try_deref(self) }

    fn try_deref_mut(self, arena: &mut Self::A) -> Option<&mut Self::T> {
        arena.try_deref_mut(self)
    }
}

impl<T> ArenaDeref<T, UniqueArenaPtr<T>> for UniqueArena<T>
where
    T: GetUniqueArenaHash + Eq,
{
    fn try_deref(&self, ptr: UniqueArenaPtr<T>) -> Option<&T> { self.arena.try_deref(ptr.0) }

    fn try_deref_mut(&mut self, ptr: UniqueArenaPtr<T>) -> Option<&mut T> {
        self.arena.try_deref_mut(ptr.0)
    }
}

impl<T> ArenaAlloc<T, UniqueArenaPtr<T>> for UniqueArena<T>
where
    T: GetUniqueArenaHash + Eq,
{
    /// # Panics
    ///
    /// **THIS FUNCTION WILL PANIC**
    ///
    /// Why? Because the unique arena requires the [UniqueArenaHash] to check
    /// for uniqueness, and then allocate or return an existing pointer, which
    /// is not possible when the value needs to be constructed from the future
    /// pointer.
    fn alloc_with<F>(&mut self, _f: F) -> UniqueArenaPtr<T>
    where
        F: FnOnce(UniqueArenaPtr<T>) -> T,
    {
        panic!("alloc_with is not available for UniqueArena")
    }

    fn alloc(&mut self, val: T) -> UniqueArenaPtr<T> {
        let unique_hash = val.unique_arena_hash();
        if let Some(ptrs) = self.unique_map.get(&unique_hash) {
            for ptr in ptrs {
                if &val == self.arena.try_deref(*ptr).unwrap() {
                    return UniqueArenaPtr(*ptr);
                }
            }
        }
        let ptr = self.arena.alloc(val);
        self.unique_map.entry(unique_hash).or_default().insert(ptr);
        UniqueArenaPtr(ptr)
    }
}

impl<T> ArenaFree<T, UniqueArenaPtr<T>> for UniqueArena<T>
where
    T: GetUniqueArenaHash + Eq,
{
    fn free(&mut self, ptr: UniqueArenaPtr<T>) {
        let val = self.arena.try_deref(ptr.0).expect("invalid unique pointer");
        let unique_hash = val.unique_arena_hash();
        if !self
            .unique_map
            .entry(unique_hash)
            .or_default()
            .remove(&ptr.0)
        {
            unreachable!("value present in arena but not in unique map");
        }
        self.arena.free(ptr.0);
    }
}

#[cfg(test)]
mod tests {
    use super::UniqueArena;
    use crate::collections::storage::{ArenaAlloc, ArenaDeref, ArenaFree, ArenaPtr};

    #[derive(Debug, PartialEq, Eq, Hash)]
    struct Test {
        a: i32,
        b: i32,
    }

    #[test]
    fn test_unique_arena() {
        let mut arena = UniqueArena::default();

        let ptr1 = arena.alloc(Test { a: 1, b: 2 });
        let ptr2 = arena.alloc(Test { a: 1, b: 2 });

        assert_eq!(ptr1, ptr2);

        assert_eq!(ptr1.try_deref(&arena), Some(&Test { a: 1, b: 2 }));
        assert_eq!(ptr2.try_deref(&arena), Some(&Test { a: 1, b: 2 }));

        assert_eq!(arena.try_deref(ptr1), Some(&Test { a: 1, b: 2 }));
        assert_eq!(arena.try_deref(ptr2), Some(&Test { a: 1, b: 2 }));

        let ptr3 = arena.alloc(Test { a: 1, b: 3 });

        assert_ne!(ptr1, ptr3);

        arena.free(ptr1);
        assert_eq!(arena.try_deref(ptr1), None);
        assert_eq!(arena.try_deref(ptr2), None);
        assert_eq!(arena.try_deref(ptr3), Some(&Test { a: 1, b: 3 }));
    }
}
