//! # Register Allocation Utilities
//!
//! This module implements graph coloring register allocation algorithm
//! and its utilities.
//!
//! - `block_defuse_analysis`: Block-level def-use analysis.
//! - `liveness_analysis`: Liveness analysis (aka. in and out set).
//! - `live_interval_analysis`: Live interval analysis.
//! - `graph_coloring_allocation`: Graph coloring register allocator.

pub mod block_defuse_analysis;
pub mod graph_coloring_allocation;
pub mod live_interval_analysis;
pub mod liveness_analysis;
pub mod reg_coalescing;
