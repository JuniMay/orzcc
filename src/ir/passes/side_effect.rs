//! Pass for Side Effect Analysis
//!
//! This pass can analyze the side effects of a function. It is useful when
//! running dead code elimination, global value numbering, etc.
//!
//! We consider a function has side effects if it has any of the following:
//! - It is the `main` function
//! - It writes to global slots or any unknown memory locations
//! - It calls other functions that have side effects
//! - It uses any indirect calls
//!
//! Note that all `decl`-ed functions are assumed to have side effects, unless
//! explicitly marked.
//!
//! There can be recursion (not in SysY, because it has no declaration of
//! functions, but in C) in the function call graph. We need to handle this case
//! carefully. The most straightforward way is to first find all SCCs in the
//! call graph, and then mark all functions in an SCC as having side effects if
//! any function in the SCC has side effects.
//!
//! TODO: Implement this pass
