use super::block::{BlockCall, BlockData};
use super::constant::ConstantData;
use super::function::FunctionData;
use super::global::GlobalData;
use super::instructions::InstData;
use super::layout::Layout;
use super::types::Type;
use super::value::{Block, Constant, Function, Global, Inst, Value, ValueData, ValueKind};
use super::{GLOBAL_PREFIX, IDENTIFIER_PREFIX};
use crate::ir::constant::ConstantKind;
use crate::ir::types::TyKind;
use std::collections::{HashMap, HashSet};
use std::fmt;
use std::hash::Hash;

/// Allocator for names and blocks in IR.
pub struct NameAllocator<T>
where
    T: Hash + Eq,
{
    counter: usize,
    predefined_set: HashSet<String>,
    predefined_map: HashMap<T, String>,
    allocated_map: HashMap<T, String>,
}

#[derive(Debug)]
pub enum NameAllocErr {
    NameDuplicated,
}

impl<T> NameAllocator<T>
where
    T: Hash + Eq + Copy,
{
    pub fn new() -> NameAllocator<T> {
        NameAllocator {
            counter: 0,
            predefined_set: HashSet::new(),
            predefined_map: HashMap::new(),
            allocated_map: HashMap::new(),
        }
    }

    /// Allocate a new name or return the existed one.
    pub fn allocate(&mut self, key: T) -> String {
        if self.predefined_map.contains_key(&key) {
            self.predefined_map.get(&key).unwrap().clone()
        } else if self.allocated_map.contains_key(&key) {
            self.allocated_map.get(&key).unwrap().clone()
        } else {
            let mut name = format!("{}", self.counter);
            while self.predefined_set.contains(&name) {
                self.counter += 1;
                name = format!("{}", self.counter);
            }
            self.allocated_map.insert(key, name.clone());
            self.counter += 1;
            name
        }
    }

    /// Manually assign a name.
    pub fn assign(&mut self, key: T, name: String) -> Result<(), NameAllocErr> {
        if self.predefined_set.contains(&name)
            || self.predefined_map.contains_key(&key)
            || self.predefined_map.contains_key(&key)
        {
            Err(NameAllocErr::NameDuplicated)
        } else {
            self.predefined_set.insert(name.clone());
            self.predefined_map.insert(key, name.clone());
            Ok(())
        }
    }

    /// Clear all auto-allocated names.
    pub fn clear(&mut self) {
        self.allocated_map.clear();
        self.counter = 0;
    }

    pub fn get_name(&self, key: T) -> Option<String> {
        self.predefined_map
            .get(&key)
            .cloned()
            .or_else(|| self.allocated_map.get(&key).cloned())
    }
}

/// Allocator for id of values.
pub struct IdAllocator {
    counter: usize,
    free: Vec<usize>,
}

impl IdAllocator {
    pub fn new() -> IdAllocator {
        IdAllocator {
            counter: 0,
            free: Vec::new(),
        }
    }

    /// Allocate a new id
    pub fn allocate(&mut self) -> usize {
        // first check the free
        if self.free.len() > 0 {
            self.free.pop().unwrap()
        } else {
            self.counter += 1;
            self.counter - 1
        }
    }

    /// Make a certain id free.
    pub fn free(&mut self, id: usize) {
        self.free.push(id);
    }
}

/// Commenter for instructions
///
/// This is used to put useful information after the instruction when dumping the IR.
pub struct InstCommenter {
    pub comments: HashMap<Inst, String>,
}

impl InstCommenter {
    pub fn new() -> InstCommenter {
        InstCommenter {
            comments: HashMap::new(),
        }
    }

    pub fn comment(&mut self, inst: Inst, comment: String) {
        self.comments.insert(inst, comment);
    }
}

/// Module of IR.
///
/// Module contains the entity information of each references.
/// The information in a module can be used to (re-)initialize a builder
///
/// A module does not represent any special structure by itself though
/// implicitly the data-flow-graph is stored. The sequence of instructions
/// and blocks are stored inside the corresponding function. Special
/// graph or other representations should be introduced by independent
/// passes and even the implicit dfg should not be used directly from
/// the module.
///
/// Though being SSA form, the module does not store explicit the def-use chains.
/// The use-def chain can be queried from instruction by the `value` because of SSA.
///
/// If execution is needed, just use the sequential representation in the
/// module (control-flow relation might needs to be analyzed during execution).
///
/// All the indexer (Value, Inst, Block, etc) can be converted to
/// represent a value. and the actual data is stored in the corresponding maps.
///
/// Value name assignment will be done when dumping the ir.
pub struct Module {
    /// The value and corresponding data in the module.
    pub values: HashMap<Value, ValueData>,

    /// The instructions
    pub insts: HashMap<Inst, InstData>,

    /// The blocks
    pub blocks: HashMap<Block, BlockData>,

    /// The constants
    pub constants: HashMap<Constant, ConstantData>,

    /// The globals
    pub globals: HashMap<Global, GlobalData>,

    /// The functions
    pub functions: HashMap<Function, FunctionData>,

    /// Identified types
    pub identified_types: HashMap<String, Type>,

    /// Layout of the code
    pub layout: Layout,

    /// The id allocator
    pub(super) id_allocator: IdAllocator,

    /// The name allocator for values
    pub(super) value_name_allocator: NameAllocator<Value>,

    /// The name allocator for blocks
    pub(super) block_name_allocator: NameAllocator<Block>,

    /// Instruction commenter
    pub(super) inst_commenter: InstCommenter,
}

impl Module {
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
            insts: HashMap::new(),
            blocks: HashMap::new(),
            constants: HashMap::new(),
            globals: HashMap::new(),
            functions: HashMap::new(),
            identified_types: HashMap::new(),
            layout: Layout::new(),
            id_allocator: IdAllocator::new(),
            value_name_allocator: NameAllocator::new(),
            block_name_allocator: NameAllocator::new(),
            inst_commenter: InstCommenter::new(),
        }
    }

    /// Allocate index as a value
    pub fn allocate_id(&mut self) -> Value {
        Value::new(self.id_allocator.allocate())
    }

    /// Assign a custom name to value.
    ///
    /// This can be used in optimization passes.
    pub fn assign_value_name(&mut self, value: Value, name: String) -> Result<(), NameAllocErr> {
        self.value_name_allocator.assign(value, name)
    }

    /// Assign a custom name to block.
    ///
    /// This can be used in some control-flow-related optimizations.
    pub fn assign_block_name(&mut self, block: Block, name: String) -> Result<(), NameAllocErr> {
        self.block_name_allocator.assign(block, name)
    }

    /// Comment at instruction.
    ///
    /// This can be used to put some debug/necessary information to IR when dumping.
    pub fn comment_at_inst(&mut self, inst: Inst, comment: String) {
        self.inst_commenter.comment(inst, comment)
    }

    /// Allocate names for block, params and instruction results.
    pub fn allocate_name(&mut self) {
        for (_function, blocks) in &self.layout.local_layouts {
            for (block, block_node) in blocks.iter() {
                self.block_name_allocator.allocate(block);
                let block_data = self.blocks.get(&block).unwrap();

                for param in &block_data.params {
                    self.value_name_allocator.allocate(*param);
                }

                for (inst, _inst_node) in block_node.insts.iter() {
                    if let TyKind::Void = self.values.get(&inst.into()).unwrap().ty.kind() {
                        // Skip assignment for void-typed value.
                        continue;
                    }
                    self.value_name_allocator.allocate(inst.into());
                }
            }
        }
    }

    fn constant_to_string(&self, constant: &Constant) -> String {
        let constant_data = self.constants.get(constant).unwrap();
        match constant_data.kind {
            ConstantKind::Zero => String::from("zero"),
            ConstantKind::Undef => String::from("undef"),
            ConstantKind::Bytes(ref bytes) => {
                let mut res = String::new();
                for byte in bytes.iter().rev() {
                    res.push_str(&format!("{:02x}", byte));
                }
                res
            }
            ConstantKind::Array(ref elems) => {
                let mut res = String::from("[ ");
                let mut first = true;
                for elem in elems.iter() {
                    if !first {
                        res.push_str(", ");
                    }
                    res.push_str(&self.constant_to_string(elem));
                    first = false;
                }
                res.push_str(" ]");
                res
            }
            ConstantKind::Struct(ref fields) => {
                let mut res = String::from("{ ");
                let mut first = true;
                for field in fields.iter() {
                    if !first {
                        res.push_str(", ");
                    }
                    res.push_str(&self.constant_to_string(field));
                    first = false;
                }
                res.push_str(" }");
                res
            }
        }
    }

    /// Convert value to string in the format of an operand.
    fn operand_to_string(&self, value: Value, with_type: bool) -> String {
        let value_data = self.values.get(&value).unwrap();

        if let TyKind::Void = value_data.ty.kind() {
            // A void value cannot be converted as an operand.
            // This happens when calling a return-void function.
            return String::from("");
        }

        let ty_str = if with_type {
            format!("{} ", value_data.ty.to_string())
        } else {
            String::from("")
        };
        let operand_str = match value_data.kind {
            ValueKind::Block => format!(
                "{}{}",
                IDENTIFIER_PREFIX,
                self.block_name_allocator
                    .get_name(value.into())
                    .unwrap_or(String::from("<unknown>"))
            ),
            ValueKind::Inst | ValueKind::BlockParam => format!(
                "{}{}",
                IDENTIFIER_PREFIX,
                self.value_name_allocator
                    .get_name(value)
                    .unwrap_or(String::from("<unknown>")),
            ),
            ValueKind::Function => format!(
                "{}{}",
                GLOBAL_PREFIX,
                self.functions.get(&value.into()).unwrap().name.clone()
            ),
            ValueKind::Global => format!(
                "{}{}",
                GLOBAL_PREFIX,
                self.globals.get(&value.into()).unwrap().name.clone()
            ),
            ValueKind::Constant => self.constant_to_string(&value.into()),
        };

        format!("{}{}", ty_str, operand_str)
    }

    fn global_to_string(&self, global: Global) -> String {
        let global_data = self.globals.get(&global).unwrap();

        format!(
            "{} {}{} = {}",
            if global_data.mutable {
                "global"
            } else {
                "constant"
            },
            GLOBAL_PREFIX,
            global_data.name,
            self.constant_to_string(&global_data.init)
        )
    }

    fn identified_type_to_string(&self, name: &String) -> String {
        format!(
            "type {}{} = {}",
            IDENTIFIER_PREFIX,
            name.clone(),
            self.identified_types.get(name).unwrap().to_string()
        )
    }

    fn block_to_string(&self, block: Block) -> String {
        let block_data = self.blocks.get(&block).unwrap();

        let mut res = format!("{}", self.block_name_allocator.get_name(block).unwrap());

        if !block_data.params.is_empty() {
            res.push_str("(");
            let mut first = true;
            for param in &block_data.params {
                if !first {
                    res.push_str(", ");
                }
                res.push_str(self.operand_to_string(*param, true).as_str());
                first = false;
            }
            res.push_str(")");
        }

        res.push_str(":\n");

        for (inst, _inst_node) in self.layout.get_insts(block).unwrap().iter() {
            res.push_str(self.inst_to_string(inst).as_str());
            res.push_str("\n");
        }

        res
    }

    fn function_to_string(&self, function: Function) -> String {
        let function_data = self.functions.get(&function).unwrap();
        let mut res = format!(
            "fn {} {} {{",
            function_data.name.clone(),
            function_data.ty.to_string()
        );
        for (block, _block_node) in self.layout.local_layouts.get(&function).unwrap().iter() {
            res.push_str(self.block_to_string(block).as_str());
            res.push_str("\n");
        }
        res.push_str("}");
        res
    }

    fn block_call_to_string(&self, block_call: &BlockCall) -> String {
        let mut res = self.operand_to_string(block_call.block.into(), true);
        if block_call.args.len() > 0 {
            let mut first = true;
            res.push_str("(");
            for arg in &block_call.args {
                if !first {
                    res.push_str(", ");
                }
                res.push_str(self.operand_to_string(*arg, true).as_str());
                first = false;
            }
            res.push_str(")");
        }
        res
    }

    fn inst_to_string(&self, inst: Inst) -> String {
        let inst_data = self.insts.get(&inst).unwrap();
        match inst_data {
            InstData::Alloc { ty } => format!(
                "{} = alloc {}",
                self.operand_to_string(inst.into(), false),
                ty.to_string()
            ),
            InstData::Load { ty, addr } => format!(
                "{} = load {}, {}",
                self.operand_to_string(inst.into(), false),
                ty.to_string(),
                self.operand_to_string(*addr, true)
            ),
            InstData::Store { val, addr } => format!(
                "store {}, {}",
                self.operand_to_string(*val, true),
                self.operand_to_string(*addr, true),
            ),
            InstData::Binary { op, lhs, rhs } => format!(
                "{} = {} {}, {}",
                self.operand_to_string(inst.into(), false),
                op,
                self.operand_to_string(*lhs, true),
                self.operand_to_string(*rhs, true),
            ),
            InstData::Unary { op, val } => format!(
                "{} = {} {}",
                self.operand_to_string(inst.into(), false),
                op,
                self.operand_to_string(*val, true)
            ),
            InstData::Br { dst } => format!("br {}", self.block_call_to_string(dst)),
            InstData::CondBr {
                cond,
                dst_then,
                dst_else,
            } => format!(
                "condbr {}, {}, {}",
                self.operand_to_string(*cond, true),
                self.block_call_to_string(dst_then),
                self.block_call_to_string(dst_else)
            ),
            InstData::Ret { val } => {
                if let Some(val) = val {
                    format!("ret {}", self.operand_to_string(*val, true))
                } else {
                    String::from("ret void")
                }
            }
            InstData::Call {
                fn_ty,
                fn_val,
                args,
            } => {
                if let TyKind::Fn(_, ret) = fn_ty.kind() {
                    if let TyKind::Void = ret.kind() {
                        format!(
                            "call {} {}, ({})",
                            fn_ty.to_string(),
                            self.operand_to_string(*fn_val, false),
                            args.iter()
                                .map(|v| self.operand_to_string(*v, false))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    } else {
                        format!(
                            "{} = call {} {}, ({})",
                            self.operand_to_string(inst.into(), false),
                            fn_ty.to_string(),
                            self.operand_to_string(*fn_val, false),
                            args.iter()
                                .map(|v| self.operand_to_string(*v, false))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    }
                } else {
                    panic!("calling with non-function type");
                }
            }
        }
    }
}

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "# Generated by ORZCC")?;
        writeln!(f, "")?;

        for ty_name in &self.layout.identified_types {
            writeln!(f, "{}", self.identified_type_to_string(ty_name))?;
        }

        for global in &self.layout.globals {
            writeln!(f, "{}", self.global_to_string(*global))?;
        }

        for function in &self.layout.functions {
            writeln!(f, "{}", self.function_to_string(*function))?;
        }

        Ok(())
    }
}
