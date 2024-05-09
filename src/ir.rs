//! # Intermediate Representation for OrzCC
//!
//! This module contains the intermediate representation (IR) for the OrzCC.
//!
//! The OrzIR is a low-level representation of programming languages. It is
//! inspired by the LLVM IR, Koopa and Cranelift IRs. The IR is designed to
//! preserve the semantics and structure of higher-level programming languages,
//! while being simple and efficient to manipulate and optimize.
//!
//! The IR has a hierarchical structure, with a module at the top level, which
//! contains functions, globals, and types. Each function contains a data flow
//! graph (DFG) and a layout. The DFG contains the instructions and the layout
//! contains the sequence of basic blocks and instructions.
//!
//! ## OrzIR Syntax
//!
//! Syntax of OrzIR is similar to LLVM IR.
//!
//! ### Global Slots
//!
//! The [global slots](values::GlobalSlot) are used to store global variables
//! and constants. The global slots are allocated in the data segment of the
//! program, and are used to store the initial values of the global variables
//! and constants.
//!
//! The format to define a global slot is:
//! ```orzir
//! slot @var_name = global <type> <initial_value>
//! slot @const_name = const <type> <initial_value>
//! ```
//!
//! ### Functions
//!
//! The [functions](entities::FunctionData) are the main units of computation in
//! the OrzIR.
//!
//! The format to define a function is:
//! ```orzir
//! func @function_name(<arg0_type>, <arg1_type>, ...) -> <return_type> {
//!     # body (blocks)
//! }
//! ```
//!
//! Also, for functions with external linkage, the format is:
//! ```orzir
//! decl @function_name(<arg0_type>, <arg1_type>, ...) -> <return_type>
//! ```
//!
//! ### Blocks
//!
//! The [blocks](entities::BlockData) are the basic units of control flow in the
//! OrzIR. The type of a block is [`label`](types::TypeKind::Label), and the
//! name of a block starts with a `^` followed by a sequence of alphanumeric
//! characters.
//!
//! In the OrzIR, phi nodes are not used for SSA, instead, block parameters are
//! used to represent the incoming values from the predecessors of the block.
//! This is the same as MLIR, Koopa, and Cranelift IRs.
//!
//! The format to define a block is:
//! ```orzir
//! ^block_name(<arg0_type> <arg0_name>, <arg1_type> <arg1_name>, ...):
//!     # instructions
//! ```
//!
//! ### Instructions
//!
//! The instructions in OrzIR follows several formats.
//! ```orzir
//! # for binary and unary operations.
//! <result> = <op> <operand0>, <operand1>, ...
//!
//! # for load, cast and getelementptr instructions.
//! <result> = <op> <type>, <operand0>, <operand1>, ...
//!
//! # for store and return instructions.
//! <op> <operand0>, <operand1>, ...
//!
//! jump ^block_name(<arg0>, <arg1>, ...)
//!
//! branch <cond>, ^then(<arg0>, ...), ^else(<arg0>, ...)
//!
//! # note that the result is optional
//! <result> = call <return_type> <function_value>(<arg0>, <arg1>, ...)
//! ```
//!
//! ## Construct OrzIR
//!
//! To construct an OrzIR program in rust, just follow some steps as below:
//!
//! ```rust
//! use orzcc::ir::{
//!     builders::{
//!         BuildError, BuildGlobalValue, BuildBlock, BuildLocalValue,
//!         BuildNonAggregateConstant,
//!     },
//!     module::Module,
//!     types::Type,
//!     values::{Block, Function, Value},
//! };
//!
//! // Firstly, create a module:
//! let mut module = Module::new("module_name".to_string());
//!
//! // Then build global values with the global builder:
//!
//! // use the `init` to build a `mutable=true` global slot/
//! let init = module.builder().zero(Type::i32_()).unwrap();
//! let var = module.builder().global_slot(init, true).unwrap();
//!
//! // use the `init` to build a `mutable=false` global slot, which is a constant.
//! let init = module.builder().zero(Type::i32_()).unwrap();
//! let const_ = module.builder().global_slot(init, false).unwrap();
//!
//! // construct a function type.
//! let ty = Type::function(vec![Type::i32_()], Type::i32_());
//! // there are also `function_decl` and `function_intrinsic` methods.
//! let function = module.builder().function_def(ty).unwrap();
//!
//! // After that, you can build the function body with the `LocalBuilder`:
//! // all the local constructions are within the function builder.
//! let function_data = module.function_data_mut(function.into()).unwrap();
//!
//! // example: build an entry block, and since there is a parameter of the function, we need to
//! // add the parameter to the block.
//! let param = function_data.dfg_mut().builder().block_param(Type::i32_()).unwrap();
//! let entry = function_data.dfg_mut().builder().block(vec![param]).unwrap();
//!
//! // example: allocate a stack slot.
//! let inst = function_data.dfg_mut().builder().alloc(Type::i32_()).unwrap();
//!
//! // next, we need to add the inst to the block in the function layout.
//! function_data.layout_mut().append_block(entry).unwrap();
//! function_data.layout_mut().append_inst(inst.into(), entry).unwrap();
//!
//! // After the structural construction, we can assign names to the values and blocks:
//!
//! // layout is just a linear description of components, all metadata is stored in the dfg.
//! function_data.dfg_mut().assign_local_value_name(param, "%param".to_string()).unwrap();
//! function_data.dfg_mut().assign_block_name(entry, "^entry".to_string()).unwrap();
//!
//! module.assign_name(var, "@var_name".to_string()).unwrap();
//! module.assign_name(const_, "@const_name".to_string()).unwrap();
//! module.assign_name(function.into(), "@function_name".to_string()).unwrap();
//! ```
//!
//! ---
//!
//! For more details, please refer to the documentation of each module and the
//! source code.

pub mod builders;
pub mod entities;
pub mod exec;
pub mod format;
pub mod frontend;
pub mod layout;
pub mod module;
pub mod passes;
pub mod types;
pub mod values;

#[allow(dead_code)]
const LOCAL_PREFIX: &str = "%";
const LOCAL_PREFIX_CHAR: char = '%';

#[allow(dead_code)]
const LABEL_PREFIX: &str = "^";
const LABEL_PREFIX_CHAR: char = '^';

#[allow(dead_code)]
const GLOBAL_PREFIX: &str = "@";
const GLOBAL_PREFIX_CHAR: char = '@';

#[allow(dead_code)]
const TYPE_PREFIX: &str = "!";
const TYPE_PREFIX_CHAR: char = '!';

#[cfg(test)]
mod tests {
    use crate::ir::module::Module;

    #[test]
    fn test_properties() {
        let module = Module::new("module_name".to_string());
        assert_eq!(module.name(), "module_name");
    }
}
