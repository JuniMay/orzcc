pub(crate) mod builder;
pub(crate) mod entities;
pub(crate) mod layout;
pub(crate) mod module;
pub(crate) mod types;
pub(crate) mod values;
pub(crate) mod pass;

#[allow(dead_code)]
const IDENTIFIER_PREFIX: &'static str = "%";

#[allow(dead_code)]
const BLOCK_PREFIX: &'static str = "^";

#[allow(dead_code)]
const GLOBAL_PREFIX: &'static str = "@";

#[allow(dead_code)]
const INDENT: &'static str = "\t";

#[cfg(test)]
mod tests {
    use self::{
        builder::{GlobalValueBuilder, LocalBlockBuilder, LocalValueBuilder},
        entities::FunctionKind,
        module::Module,
        types::Type,
    };

    use super::*;

    #[test]
    fn test_basic_builder_functionality() {
        let mut module = Module::new("test".to_string());
        let function = module
            .builder()
            .function(
                "test_func".to_string(),
                Type::mk_function(vec![], Type::mk_void()),
                FunctionKind::Definition,
            )
            .unwrap();

        let function_data = module.function_data_mut(function).unwrap();

        let block = function_data.dfg_mut().builder().block(vec![]).unwrap();
        // let block_data = function_data.dfg_mut().block_data(block).unwrap();

        let alloc0 = function_data
            .dfg_mut()
            .builder()
            .alloc(Type::mk_int(32))
            .unwrap();
        let alloc1 = function_data
            .dfg_mut()
            .builder()
            .alloc(Type::mk_float())
            .unwrap();
        let alloc2 = function_data
            .dfg_mut()
            .builder()
            .alloc(Type::mk_double())
            .unwrap();

        let _ = function_data.layout_mut().blocks_mut().append(block);

        let block_node = function_data
            .layout_mut()
            .blocks_mut()
            .node_mut(block)
            .unwrap();

        let _ = block_node.insts_mut().append(alloc0.into());
        let _ = block_node.insts_mut().append(alloc1.into());
        let _ = block_node.insts_mut().append(alloc2.into());

        assert_ne!(alloc0, alloc1);
        assert_ne!(alloc0, alloc2);
        assert_ne!(alloc1, alloc2);
    }
}
