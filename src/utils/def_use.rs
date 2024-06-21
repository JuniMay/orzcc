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
/// # Type Parameters
///
/// - `T`: The type of the users, should be an arena pointer with the same
///   arena.
pub trait Usable: Sized + ArenaPtr + Hash {
    /// The type of the users of this entity.
    type U: User<Self, A = Self::A>;

    /// Get the users of this entity.
    fn users(self, arena: &Self::A) -> Vec<Self::U>;

    /// Add a user to this entity.
    ///
    /// If the user is already added, this function should do nothing.
    fn add_user(self, arena: &mut Self::A, user: Self::U);

    /// Remove a user from this entity.
    fn remove_user(self, arena: &mut Self::A, user: Self::U);
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
    T: Usable<U = Self, A = Self::A>,
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

/// A reminder that the entity is a use.
///
/// This type will enforce the user is correctly added when constructing the
/// operand.
///
/// As for the drop/replace, the user type can implement or maintain on its own.
#[derive(Debug, PartialEq, Eq, Hash)]
pub struct Operand<T>
where
    T: Usable,
{
    used: T,
    user: T::U,
}

impl<T> Operand<T>
where
    T: Usable,
{
    pub fn new(arena: &mut T::A, used: T, user: T::U) -> Self {
        used.add_user(arena, user);
        Self { used, user }
    }

    pub fn drop(self, arena: &mut T::A) { self.used.remove_user(arena, self.user); }

    pub fn inner(&self) -> T { self.used }

    pub fn inner_mut(&mut self) -> &mut T { &mut self.used }

    #[inline(always)]
    pub fn set_inner_if_eq(&mut self, old: T, new: T) -> bool {
        if self.used == old {
            self.used = new;
            true
        } else {
            false
        }
    }
}
