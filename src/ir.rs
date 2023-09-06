pub mod builder;
pub mod entities;
pub mod layout;
pub mod module;
pub mod printer;
pub mod types;
pub mod value;

#[allow(dead_code)]
const IDENTIFIER_PREFIX: &'static str = "%";

#[allow(dead_code)]
const BLOCK_PREFIX: &'static str = "^";

#[allow(dead_code)]
const GLOBAL_PREFIX: &'static str = "@";

#[allow(dead_code)]
const INDENT: &'static str = "\t";
