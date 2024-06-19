//! # General Utilities
//!
//! This module contains some abstracted data structures or algorithms that are
//! applicable to the IR and backend target. The goal is to provide a unified
//! interface for the compiler to use, and make the infrastructure reusable.

mod cfg;
mod domtree;
mod numbering;

pub use cfg::{CfgInfo, CfgNode, CfgRegion};
