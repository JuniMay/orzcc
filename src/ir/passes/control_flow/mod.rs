mod canonicalize;
mod ph_layout;
mod reorder;
mod simplify;
mod split_critical_edge;

pub use canonicalize::{CfgCanonicalize, CfgCanonicalizeError, CFG_CANONICALIZE};
pub use ph_layout::{PHBlockLayout, PH_BLOCK_LAYOUT};
pub use reorder::{BlockReorder, BLOCK_REORDER};
pub use simplify::{CfgSimplify, CFG_SIMPLIFY};
pub use split_critical_edge::{SplitCriticalEdge, SPLIT_CRITICAL_EDGE};
