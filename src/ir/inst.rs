use std::collections::HashMap;

use super::{def_use::Usable, Context, Signature, Symbol, Ty, User};
use crate::{
    collections::{
        linked_list::LinkedListNodePtr,
        storage::{ArenaPtr, BaseArenaPtr},
    },
    impl_arena,
    ir::{Block, Constant, Value},
};

pub enum BinaryOp {}

pub enum BinaryImmOp {}

pub enum UnaryOp {}

/// Successor of a branch instruction.
///
/// Successor describes the destination block and the argument passing.
pub struct Successor {
    /// The destination block.
    block: Block,
    /// Argument mapping.
    ///
    /// This represents the parameter to argument mapping, the keys represent
    /// parameters, and the values represent arguments.
    args: HashMap<Value, Value>,
}

pub enum InstKind {
    /// Create a new value from a constant.
    ///
    /// Using an individual constant creation instruction is similar to MLIR and
    /// Cranelift IR.
    IConst {
        /// The constant value, expect to be [Constant::Integer]
        constant: Constant,
    },
    /// Create a new value from a constant.
    FConst {
        /// The constant value, expect to be [Constant::Float32] or
        /// [Constant::Float64]
        constant: Constant,
    },
    /// Create a new value as a stack slot.
    StackSlot {
        /// The size of the stack slot.
        ///
        /// We don't represent the slot with type, but with size, as the size
        /// is more important than the type in the stack allocation. The type
        /// can be decided when loading or storing the value.
        size: u32,
    },
    /// Binary operation instruction.
    Binary {
        /// The opcode of the binary operation.
        op: BinaryOp,
        /// The left-hand side value.
        lhs: Value,
        /// The right-hand side value.
        rhs: Value,
    },
    /// Unary operation instruction.
    Unary {
        /// The opcode of the unary operation.
        op: UnaryOp,
        /// The value to apply the operation.
        val: Value,
    },
    /// Branch instruction.
    ///
    /// This instruction unifies the jump, br, and even switch.
    ///
    /// # Notes
    ///
    /// The `br` mostly jumps to the first successor, when the condition is
    /// true. However, when using this instruction, the first successor
    /// corresponds to the false branch (cond is `0`).
    Branch {
        /// The condition of the branch.
        ///
        /// The bit width of condition value must be equal or greater than
        /// the number of successors.
        cond: Option<Value>,
        /// The successors of the branch.
        succs: Vec<Successor>,
        /// The default successor.
        default: Option<Successor>,
    },
    /// Call instruction.
    Call {
        /// The callee symbol.
        ///
        /// This symbol can be used to lookup in the [Context] using
        /// [Context::lookup_symbol].
        callee: Symbol,
        /// The arguments to the callee.
        args: Vec<Value>,
    },
    /// Call indirect instruction.
    CallIndirect {
        /// The signature of the callee.
        sig: Signature,
        /// The callee value, should have a type of
        /// [TyData::Index](crate::ir::TyData::Index).
        callee: Value,
        /// The arguments to the callee.
        args: Vec<Value>,
    },
    /// Return instruction.
    Return {
        /// The optional return value.
        val: Option<Value>,
    },
    /// Load instruction.
    Load {
        /// The type of the loaded value.
        ty: Ty,
        /// The memory to load from.
        ///
        /// This value should have a [TyData::Index](crate::ir::TyData::Index)
        /// type.
        mem: Value,
    },
    /// Store instruction.
    Store {
        /// The value to store.
        val: Value,
        /// The memory to store to.
        mem: Value,
    },
}

pub struct InstData {
    /// Self reference.
    this: Inst,
    /// The instruction kind.
    kind: InstKind,
    /// The next instruction.
    next: Option<Inst>,
    /// The previous instruction.
    prev: Option<Inst>,
    /// The parent block.
    parent: Option<Block>,
}

#[derive(Debug, Hash, Clone, Copy, PartialEq, Eq)]
pub struct Inst(BaseArenaPtr<InstData>);

impl_arena!(Context, InstData, Inst, insts);

impl Inst {
    pub fn is_terminator(self, ctx: &Context) -> bool {
        matches!(
            self.deref(ctx).kind,
            InstKind::Branch { .. } | InstKind::Return { .. }
        )
    }

    /// Get the successor blocks of the branch instruction.
    ///
    /// # Panics
    ///
    /// Panics if the instruction is not a branch instruction. Check with
    /// [Inst::is_terminator] before calling this method.
    ///
    /// # See Also
    ///
    /// - [Inst::is_terminator]
    pub fn succ_blocks(self, ctx: &Context) -> Vec<Block> {
        if let InstKind::Branch {
            ref succs,
            ref default,
            ..
        } = self.deref(ctx).kind
        {
            let mut blocks = succs.iter().map(|s| s.block).collect::<Vec<_>>();
            if let Some(default) = default {
                blocks.push(default.block);
            }
            blocks
        } else {
            panic!("not a branch instruction")
        }
    }
}

impl LinkedListNodePtr for Inst {
    type ContainerPtr = Block;

    fn next(self, ctx: &Context) -> Option<Inst> { self.deref(ctx).next }

    fn prev(self, ctx: &Context) -> Option<Inst> { self.deref(ctx).prev }

    fn set_next(self, ctx: &mut Context, next: Option<Inst>) { self.deref_mut(ctx).next = next; }

    fn set_prev(self, ctx: &mut Context, prev: Option<Inst>) { self.deref_mut(ctx).prev = prev; }

    fn container(self, ctx: &Context) -> Option<Block> { self.deref(ctx).parent }

    fn set_container(self, ctx: &mut Context, container: Option<Block>) {
        self.deref_mut(ctx).parent = container;
    }
}

impl User<Block> for Inst {
    fn uses(self, ctx: &Context) -> Vec<Block> {
        // only append the block if this is a branch instruction
        let mut uses = vec![];
        if let InstKind::Branch {
            ref succs,
            ref default,
            ..
        } = self.deref(ctx).kind
        {
            uses.extend(succs.iter().map(|s| s.block));
            if let Some(default) = default {
                uses.push(default.block);
            }
        }
        uses
    }

    fn replace(self, ctx: &mut Context, old: Block, new: Block) {
        // we need to be very careful, because the block parameters might be
        // different. So we need to check the argument mapping, the number of
        // arguments and the type of the arguments.

        // firstly, check if the old block and new block have compatible
        // parameters, i.e., same number and same types

        // the number of parameters must be the same
        if old.params(ctx).len() != new.params(ctx).len() {
            panic!("incompatible block parameter count");
        }

        // the type of the parameters must be the same
        for (old_param, new_param) in old.params(ctx).iter().zip(new.params(ctx).iter()) {
            if old_param.ty(ctx) != new_param.ty(ctx) {
                panic!("incompatible block parameter type");
            }
        }

        // the mapping from old block parameters to new block parameters according to
        // the position/index in the parameter list.
        let param_mapping = old
            .params(ctx)
            .iter()
            .copied()
            .zip(new.params(ctx).iter().copied())
            .collect::<HashMap<_, _>>();

        let mut replaced = false;

        // now we can replace the block and the argument mapping params (keys)
        if let InstKind::Branch {
            ref mut succs,
            ref mut default,
            ..
        } = self.deref_mut(ctx).kind
        {
            for succ in succs {
                if succ.block == old {
                    succ.block = new;
                    succ.args = succ
                        .args
                        .iter()
                        .map(|(old_param, arg)| {
                            let new_param = param_mapping.get(old_param).unwrap();
                            (*new_param, *arg)
                        })
                        .collect();

                    replaced = true;
                }
            }
            if let Some(ref mut succ) = default {
                if succ.block == old {
                    succ.block = new;
                    succ.args = succ
                        .args
                        .iter()
                        .map(|(old_param, arg)| {
                            let new_param = param_mapping.get(old_param).unwrap();
                            (*new_param, *arg)
                        })
                        .collect();

                    replaced = true;
                }
            }
        }

        if replaced {
            // after all the replacements, we need to update the uses of the old and
            // new blocks

            // remove this instruction from the users of the old block
            old.remove_user(ctx, self);
            // add this instruction to the users of the new block
            new.add_user(ctx, self);
        }
    }
}

impl User<Value> for Inst {
    fn uses(self, ctx: &Context) -> Vec<Value> {
        use InstKind as Ik;
        match self.deref(ctx).kind {
            Ik::IConst { .. } | Ik::FConst { .. } | Ik::StackSlot { .. } => vec![],
            Ik::Binary { lhs, rhs, .. } => vec![lhs, rhs],
            Ik::Unary { val, .. } => vec![val],
            Ik::Branch {
                cond,
                ref succs,
                ref default,
            } => {
                let mut uses = vec![];
                if let Some(cond) = cond {
                    uses.push(cond);
                }
                // now append the arguments (values of the mapping)
                for succ in succs {
                    uses.extend(succ.args.values().cloned());
                }
                if let Some(default) = default {
                    uses.extend(default.args.values().cloned());
                }
                uses
            }
            Ik::Call { ref args, .. } => args.clone(),
            Ik::CallIndirect {
                callee, ref args, ..
            } => {
                let mut uses = vec![callee];
                uses.extend(args);
                uses
            }
            Ik::Return { val } => val.into_iter().collect(),
            Ik::Load { mem, .. } => vec![mem],
            Ik::Store { val, mem } => vec![val, mem],
        }
    }

    fn replace(self, ctx: &mut Context, old: Value, new: Value) {
        use InstKind as Ik;

        let mut replaced = false;

        match self.deref_mut(ctx).kind {
            Ik::IConst { .. } | Ik::FConst { .. } | Ik::StackSlot { .. } => {}
            Ik::Binary {
                ref mut lhs,
                ref mut rhs,
                ..
            } => {
                if *lhs == old {
                    *lhs = new;
                    replaced = true;
                }
                if *rhs == old {
                    *rhs = new;
                    replaced = true;
                }
            }
            Ik::Unary { ref mut val, .. } => {
                if *val == old {
                    *val = new;
                    replaced = true;
                }
            }
            Ik::Branch {
                ref mut cond,
                ref mut succs,
                ref mut default,
            } => {
                if let Some(ref mut cond) = cond {
                    if *cond == old {
                        *cond = new;
                        replaced = true;
                    }
                }
                for succ in succs {
                    // remember that we need to replace the args, not the
                    // params, i.e., replace the value of the mapping
                    for arg in succ.args.values_mut() {
                        if *arg == old {
                            *arg = new;
                            replaced = true;
                        }
                    }
                }
                if let Some(ref mut succ) = default {
                    for arg in succ.args.values_mut() {
                        if *arg == old {
                            *arg = new;
                            replaced = true;
                        }
                    }
                }
            }
            Ik::Call { ref mut args, .. } => {
                for arg in args {
                    if *arg == old {
                        *arg = new;
                        replaced = true;
                    }
                }
            }
            Ik::CallIndirect {
                ref mut callee,
                ref mut args,
                ..
            } => {
                if *callee == old {
                    *callee = new;
                    replaced = true;
                }
                for arg in args {
                    if *arg == old {
                        *arg = new;
                        replaced = true;
                    }
                }
            }
            Ik::Return { ref mut val } => {
                if let Some(val) = val {
                    if *val == old {
                        *val = new;
                        replaced = true;
                    }
                }
            }
            Ik::Load { ref mut mem, .. } => {
                if *mem == old {
                    *mem = new;
                    replaced = true;
                }
            }
            Ik::Store {
                ref mut val,
                ref mut mem,
            } => {
                if *val == old {
                    *val = new;
                    replaced = true;
                }
                if *mem == old {
                    *mem = new;
                    replaced = true;
                }
            }
        }

        if replaced {
            // remove this instruction from the users of the old value
            old.remove_user(ctx, self);
            // add this instruction to the users of the new value
            new.add_user(ctx, self);
        }
    }
}
