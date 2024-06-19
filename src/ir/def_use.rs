//! # Def-Use Data Structures.
//!
//! The implementation of OrzCC before ignored the importance of a ergonomic api
//! for def-use manipulations. Now in this refactored version, the def-use no
//! longer require a pass to generate, but maintained dynamically.
//!
//! Actually, the result of instructions, blocks, and even global slots or
//! functions can all be regarded as usable entities. And thus the data
//! sturcture should be easily to reuse.
//!
//! All the traits in this module apply to the arena pointers instead of the
//! actual data.

use std::{collections::HashSet, hash::Hash};

use super::Context;
use crate::collections::storage::ArenaPtr;

/// The [Usable] trait is used to describe a entity that can be used.
///
/// # Type Parameters
///
/// - `T`: The type of the users.
pub trait Usable<T>: Sized + ArenaPtr + Eq + Hash
where
    T: User<Self> + ArenaPtr,
{
    /// Get the users of this entity.
    fn users(self, ctx: &Context) -> Vec<T>;

    /// Add a user to this entity.
    fn add_user(self, ctx: &mut Context, user: T);

    /// Remove a user from this entity.
    fn remove_user(self, ctx: &mut Context, user: T);
}

/// This trait is used to describe a entity that uses other entities.
///
/// # Type Parameters
///
/// - `T`: The type of the usable entities.
pub trait User<T>: Sized + ArenaPtr
where
    T: Usable<Self> + ArenaPtr + Eq + Hash,
{
    /// Get all the used entities.
    fn uses(self, ctx: &Context) -> Vec<T>;

    /// Replace a used entity with another entity.
    fn replace(self, ctx: &mut Context, old: T, new: T);

    /// Check if this entity uses a specific entity.
    fn if_uses(self, ctx: &Context, usable: T) -> bool { self.uses(ctx).contains(&usable) }

    /// Check if this entity uses any of the given entities.
    fn if_uses_any(self, ctx: &Context, usables: &[T]) -> bool {
        let lhs = self.uses(ctx).into_iter().collect::<HashSet<T>>();
        let rhs = usables.iter().cloned().collect::<HashSet<T>>();
        lhs.intersection(&rhs).next().is_some()
    }

    /// Check if this entity uses all of the given entities.
    fn if_uses_all(self, ctx: &Context, usables: &[T]) -> bool {
        let lhs = self.uses(ctx).into_iter().collect::<HashSet<T>>();
        let rhs = usables.iter().cloned().collect::<HashSet<T>>();
        lhs.is_superset(&rhs)
    }

    /// Replace a used entity with a closure that produces the new entity.
    fn replace_with<F>(self, ctx: &mut Context, old: T, f: F)
    where
        F: FnOnce(&mut Context, T) -> T,
    {
        let new = f(ctx, old);
        self.replace(ctx, old, new);
    }
}
