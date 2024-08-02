//! # General Utilities and Infrastructure
//!
//! This module contains some abstracted data structures or algorithms that are
//! applicable to the IR and backend target. The goal is to provide a unified
//! interface for the compiler to use, and make the infrastructure reusable.

pub mod cdg;
pub mod cfg;
pub mod def_use;
pub mod dfs;
pub mod dominance;
pub mod loop_info;
