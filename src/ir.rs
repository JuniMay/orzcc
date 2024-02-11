pub(crate) mod builder;
pub(crate) mod entities;
pub(crate) mod layout;
pub(crate) mod module;
pub(crate) mod types;
pub(crate) mod values;

#[allow(dead_code)]
const IDENTIFIER_PREFIX: &'static str = "%";

#[allow(dead_code)]
const BLOCK_PREFIX: &'static str = "^";

#[allow(dead_code)]
const GLOBAL_PREFIX: &'static str = "@";

#[allow(dead_code)]
const INDENT: &'static str = "\t";
