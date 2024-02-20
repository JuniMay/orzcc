pub mod builder;
pub mod entities;
pub mod frontend;
pub mod layout;
pub mod module;
pub mod pass;
pub mod types;
pub mod values;

#[allow(dead_code)]
const IDENTIFIER_PREFIX: &'static str = "%";

#[allow(dead_code)]
const BLOCK_PREFIX: &'static str = "^";

#[allow(dead_code)]
const GLOBAL_PREFIX: &'static str = "@";

#[allow(dead_code)]
const TYPE_PREFIX: &'static str = "$";

#[allow(dead_code)]
const INDENT: &'static str = "\t";

#[cfg(test)]
mod tests {
    use crate::ir::module::Module;

    #[test]
    fn test_properties() {
        let module = Module::new("module_name".to_string());
        assert_eq!(module.name(), "module_name");
    }
}
