//! # Collection of Basic Data Structures
//!
//! This module contains the following submodules:
//!
//! - `apint`: Arbitrary precision integer.
//! - `linked_list`: Linked list implementation with arena-based storage.
//! - `storage`: Arena-based storage to deal with linked data structures.
//! - `diagnostic`: Diagnostic information inspired by [ariadne](https://github.com/zesterer/ariadne)

pub mod apint;
pub mod diagnostic;
pub mod linked_list;
pub mod storage;
