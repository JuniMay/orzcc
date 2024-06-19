//! # Def-Use Data Structures.
//!
//! The old implementation of OrzCC ignored the importance of an ergonomic api
//! for def-use manipulations. Now in this refactored version, the def-use no
//! longer requires a pass to generate, but maintained dynamically.
//!
//! Actually, the result of instructions, blocks, and even global slots or
//! functions can all be regarded as usable entities, and thus the data
//! sturcture should be easy to reuse.
//!
//! All the traits in this module apply to the arena pointers instead of the
//! actual data.

use std::{collections::HashSet, hash::Hash};

use crate::collections::storage::ArenaPtr;

/// The [Usable] trait is used to describe an entity that can be used.
///
/// `A: Usable<B>` can be interpreted as *A can be used by B*.
///
/// # Type Parameters
///
/// - `T`: The type of the users, should be an arena pointer with the same
///   arena.
pub trait Usable<T>: Sized + ArenaPtr + Hash
where
    T: User<Self> + ArenaPtr<A = Self::A>,
{
    /// Get the users of this entity.
    fn users(self, arena: &Self::A) -> Vec<T>;

    /// Add a user to this entity.
    fn add_user(self, arena: &mut Self::A, user: T);

    /// Remove a user from this entity.
    fn remove_user(self, arena: &mut Self::A, user: T);
}

/// This trait is used to describe a entity that uses other entities.
///
/// `A: User<B>` can be interpreted as *A (might) be a user of B*.
///
/// # Type Parameters
///
/// - `T`: The type of the usable entities, should be an arena pointer with the
///   same arena.
pub trait User<T>: Sized + ArenaPtr
where
    T: Usable<Self> + ArenaPtr<A = Self::A> + Hash,
{
    /// Get all the used entities.
    fn all_uses(self, arena: &Self::A) -> Vec<T>;

    /// Replace a used entity with another entity.
    fn replace(self, arena: &mut Self::A, old: T, new: T);

    /// Check if this entity uses a specific entity.
    fn if_uses(self, arena: &Self::A, usable: T) -> bool { self.all_uses(arena).contains(&usable) }

    /// Check if this entity uses any of the given entities.
    fn if_uses_any(self, arena: &Self::A, usables: &[T]) -> bool {
        let lhs = self.all_uses(arena).into_iter().collect::<HashSet<T>>();
        let rhs = usables.iter().cloned().collect::<HashSet<T>>();
        lhs.intersection(&rhs).next().is_some()
    }

    /// Check if this entity uses all of the given entities.
    fn if_uses_all(self, arena: &Self::A, usables: &[T]) -> bool {
        let lhs = self.all_uses(arena).into_iter().collect::<HashSet<T>>();
        let rhs = usables.iter().cloned().collect::<HashSet<T>>();
        lhs.is_superset(&rhs)
    }

    /// Replace a used entity with a closure that produces the new entity.
    fn replace_with<F>(self, arena: &mut Self::A, old: T, f: F)
    where
        F: FnOnce(&mut Self::A, T) -> T,
    {
        let new = f(arena, old);
        self.replace(arena, old, new);
    }
}
