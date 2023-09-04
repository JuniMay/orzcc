pub mod block;
pub mod builder;
pub mod constant;
pub mod function;
pub mod global;
pub mod instructions;
pub mod layout;
pub mod module;
pub mod types;
pub mod value;

const IDENTIFIER_PREFIX: &'static str = "%";

#[allow(dead_code)]
const GLOBAL_PREFIX: &'static str = "@";
