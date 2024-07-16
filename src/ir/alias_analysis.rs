use core::fmt;

use super::{Context, Inst, InstKind, TyData, Value};

pub enum AliasAnalysisResult {
    NoAlias,
    MayAlias,
    MustAlias,
}

impl fmt::Display for AliasAnalysisResult {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AliasAnalysisResult::NoAlias => write!(f, "NoAlias"),
            AliasAnalysisResult::MayAlias => write!(f, "MayAlias"),
            AliasAnalysisResult::MustAlias => write!(f, "MustAlias"),
        }
    }
}

pub struct AliasAnalysis {}

impl AliasAnalysis {
    pub fn analyze(ctx: &Context, a: Value, b: Value) -> AliasAnalysisResult {
        if !a.ty(ctx).is_ptr(ctx) || !b.ty(ctx).is_ptr(ctx) {
            panic!("Alias analysis is only supported for pointers.");
        }

        // there are four Insts that defs a ptr
        // Offset{Value: Ptr, Value: Int}
        // StackSlot(Int)
        // GetGlobal(Symbol)
        // Cast(IntToPtr){Value: Int}

        // if a == b, then they must alias
        if a == b {
            return AliasAnalysisResult::MustAlias;
        }
        // later we assume a != b

        let a_inst = a.def_inst(ctx);
        let b_inst = b.def_inst(ctx);

        // if we meet either of them being block parameter, we can't analyze them
        if a_inst.is_none() || b_inst.is_none() {
            return AliasAnalysisResult::MayAlias;
        }

        let a_inst = a_inst.unwrap();
        let b_inst = b_inst.unwrap();

        return match (a_inst.kind(ctx), b_inst.kind(ctx)) {
            (InstKind::Offset, InstKind::Offset) => {
                // decompose the offset instruction
                let (a_base, a_offset) = AliasAnalysis::offset_decompose(ctx, a);
                let (b_base, b_offset) = AliasAnalysis::offset_decompose(ctx, b);
                // first check the base ptr
                match AliasAnalysis::analyze(ctx, a_base, b_base) {
                    // if the base ptr must not alias, then the whole ptr must not alias
                    AliasAnalysisResult::NoAlias => AliasAnalysisResult::NoAlias,
                    // if the base ptr may alias, then the whole ptr may alias
                    AliasAnalysisResult::MayAlias => AliasAnalysisResult::MayAlias,
                    // if the base ptr must alias, then we need to check the offset
                    AliasAnalysisResult::MustAlias => {
                        if a_offset == b_offset {
                            // if the offset is the same, then the whole ptr must alias
                            AliasAnalysisResult::MustAlias
                        } else {
                            // if the offset is different, then the whole ptr may alias
                            AliasAnalysisResult::MayAlias
                            // TODO: We can do better for Constant offset
                        }
                    }
                }
            }
            (InstKind::Offset, InstKind::StackSlot(_))
            | (InstKind::Offset, InstKind::GetGlobal(_)) => {
                // decompose the offset instruction
                let (a_base, a_offset) = AliasAnalysis::offset_decompose(ctx, a);
                // first check the base ptr
                match AliasAnalysis::analyze(ctx, a_base, b) {
                    // if the base ptr must not alias, then the whole ptr must not alias
                    AliasAnalysisResult::NoAlias => AliasAnalysisResult::NoAlias,
                    // if the base ptr may alias, then the whole ptr may alias
                    AliasAnalysisResult::MayAlias => AliasAnalysisResult::MayAlias,
                    // if the base ptr must alias, then we need to check the offset
                    AliasAnalysisResult::MustAlias => {
                        if a_offset.is_empty() {
                            // if the offset is empty, then the whole ptr must alias
                            AliasAnalysisResult::MustAlias
                        } else {
                            // if the offset is not empty, then the whole ptr may alias
                            AliasAnalysisResult::MayAlias
                        }
                    }
                }
            }
            (InstKind::StackSlot(_), InstKind::Offset)
            | (InstKind::GetGlobal(_), InstKind::Offset) => {
                // decompose the offset instruction
                let (b_base, b_offset) = AliasAnalysis::offset_decompose(ctx, b);
                // first check the base ptr
                match AliasAnalysis::analyze(ctx, a, b_base) {
                    // if the base ptr must not alias, then the whole ptr must not alias
                    AliasAnalysisResult::NoAlias => AliasAnalysisResult::NoAlias,
                    // if the base ptr may alias, then the whole ptr may alias
                    AliasAnalysisResult::MayAlias => AliasAnalysisResult::MayAlias,
                    // if the base ptr must alias, then we need to check the offset
                    AliasAnalysisResult::MustAlias => {
                        if b_offset.is_empty() {
                            // if the offset is empty, then the whole ptr must alias
                            AliasAnalysisResult::MustAlias
                        } else {
                            // if the offset is not empty, then the whole ptr may alias
                            AliasAnalysisResult::MayAlias
                        }
                    }
                }
            }
            (InstKind::StackSlot(_), InstKind::StackSlot(_))
            | (InstKind::StackSlot(_), InstKind::GetGlobal(_))
            | (InstKind::GetGlobal(_), InstKind::StackSlot(_))
            | (InstKind::GetGlobal(_), InstKind::GetGlobal(_)) => {
                // different stack slots and globals must not alias
                AliasAnalysisResult::NoAlias
            }
            _ => {
                // for cast instruction, we just let it go
                AliasAnalysisResult::MayAlias
            }
        };
    }

    /// Decompose the offset instruction.
    /// Note that the ptr Value can be block argument, defined by StackSlot,
    /// GetGlobal, or Cast(IntToPtr).
    pub fn offset_decompose(ctx: &Context, ptr: Value) -> (Value, Vec<Value>) {
        let mut ptr = ptr;
        let mut offsets = vec![];

        while let Some(inst) = ptr.def_inst(ctx) {
            match inst.kind(ctx) {
                InstKind::Offset => {
                    let base = inst.operand(ctx, 0);
                    let offset = inst.operand(ctx, 1);
                    ptr = base;
                    offsets.push(offset);
                }
                InstKind::Undef
                | InstKind::IConst(_)
                | InstKind::FConst(_)
                | InstKind::StackSlot(_)
                | InstKind::IBinary(_)
                | InstKind::FBinary(_)
                | InstKind::IUnary(_)
                | InstKind::FUnary(_)
                | InstKind::Cast(_)
                | InstKind::Jump
                | InstKind::Br
                | InstKind::Call(_)
                | InstKind::CallIndirect(_)
                | InstKind::Ret
                | InstKind::GetGlobal(_)
                | InstKind::Load
                | InstKind::Store => break,
            }
        }

        offsets.reverse();
        (ptr, offsets)
    }
}
