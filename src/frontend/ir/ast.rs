use crate::{
    collections::{
        diagnostic::{Diagnostic, DiagnosticList},
        linked_list::LinkedListContainerPtr,
    },
    ir::{
        Block,
        Constant,
        Context,
        Func,
        GlobalSlot,
        Inst,
        InstKind,
        Signature,
        Span,
        Successor,
        Symbol,
        Ty,
    },
    utils::def_use::Operand,
};

/// The reference to a value.
#[derive(Debug)]
pub struct ValueRef {
    /// The name of the value.
    pub name: String,
    /// The span of the value.
    pub span: Span,
}

/// The reference to a block.
#[derive(Debug)]
pub struct BlockRef {
    /// The name of the block.
    pub name: String,
    /// The span of the block.
    pub span: Span,
}

/// The reference to a successor.
#[derive(Debug)]
pub struct SuccRef {
    /// The reference to the block.
    pub block: BlockRef,
    /// The references to the arguments
    pub args: Vec<ValueRef>,
    /// The span of the successor.
    pub span: Span,
}

/// A parsing instruction.
#[derive(Debug)]
pub struct ParsingInst {
    /// The result names and spans
    pub results: Vec<ValueRef>,
    /// The instruction kind
    pub kind: InstKind,
    /// The operand references
    pub operands: Vec<ValueRef>,
    /// The successor references
    pub successors: Vec<SuccRef>,
    /// The trailing types of the results
    pub result_tys: Vec<(Ty, Span)>,
    /// The span of the instruction
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsingBlock {
    /// The name of the block.
    pub block: BlockRef,
    /// The parameters of the block.
    pub params: Vec<(ValueRef, (Ty, Span))>,
    /// The instructions in the block.
    pub insts: Vec<ParsingInst>,
    /// The span of the block.
    pub span: Span,
}

/// A parsing function.
#[derive(Debug)]
pub struct ParsingFunc {
    /// The name of the function.
    pub name: Symbol,
    /// The signature of the function.
    pub sig: Signature,
    /// The blocks in the function.
    pub blocks: Vec<ParsingBlock>,
    /// The span of the function.
    pub span: Span,
}

/// A parsing declaration.
#[derive(Debug)]
pub struct ParsingDecl {
    /// The name of the declaration.
    pub name: Symbol,
    /// The signature of the declaration.
    pub sig: Signature,
    /// The span of the declaration.
    pub span: Span,
}

/// A parsing global slot.
#[derive(Debug)]
pub struct ParsingSlot {
    /// The name of the global slot.
    pub name: Symbol,
    /// The size of the global slot.
    pub size: (usize, Span),
    /// The initial value of the global slot.
    pub init: Constant,
    /// The span of the global slot.
    pub span: Span,
}

/// An item in the IR.
#[derive(Debug)]
pub enum Item {
    Func(ParsingFunc),
    Decl(ParsingDecl),
    Slot(ParsingSlot),
}

#[derive(Debug)]
enum ConversionError {
    DuplicateValueName(Span),
    /// The number of results is not valid.
    ///
    /// span, expected, actual
    InvalidResultNum(Span, usize, usize),
    /// Undefined value
    UndefinedName(Span),
}

impl From<ConversionError> for Diagnostic {
    fn from(err: ConversionError) -> Diagnostic {
        use ConversionError as Ce;
        match err {
            Ce::DuplicateValueName(span) => Diagnostic::error("duplicate value name")
                .annotate(span.into(), "this value name is already used"),
            Ce::InvalidResultNum(span, expected, actual) => {
                Diagnostic::error("invalid result number").annotate(
                    span.into(),
                    format!("expected {} results but got {}", expected, actual),
                )
            }
            Ce::UndefinedName(span) => Diagnostic::error("undefined name")
                .annotate(span.into(), "this name is not defined"),
        }
    }
}

/// Convert the AST into IR.
///
/// Just convert, not check the correctness. The correctness should be checked
/// by the verification pass.
///
/// # Parameters
///
/// - `ast`: The AST to convert.
/// - `ctx`: The context of IR, should be the same context as the one generated
///   by parser, because all the types are created in it.
/// - `diag`: The diagnostic list to store the error messages.
///
/// # Returns
///
/// - `Some(())`: The conversion is successful.
/// - `None`: The conversion is failed, error messages are stored in `diag`.
#[must_use]
pub fn into_ir(ast: Vec<Item>, ctx: &mut Context, diag: &mut DiagnosticList) -> Option<()> {
    // we need to create functions and fill them later, so we can lookup the
    // function names
    let mut funcs = Vec::new();
    let mut errors = Vec::new();

    for item in ast {
        match item {
            Item::Func(ParsingFunc {
                name, sig, blocks, ..
            }) => {
                if ctx.lookup_symbol(&name).is_some() {
                    errors.push(ConversionError::DuplicateValueName(name.source_span()));
                    continue;
                }

                let func = Func::new(ctx, name, sig);
                funcs.push((func, blocks));
            }
            Item::Decl(ParsingDecl { name, sig, .. }) => {
                if ctx.lookup_symbol(&name).is_some() {
                    errors.push(ConversionError::DuplicateValueName(name.source_span()));
                    continue;
                }
                ctx.add_func_decl(name, sig)
            }
            Item::Slot(ParsingSlot {
                name, size, init, ..
            }) => {
                if ctx.lookup_symbol(&name).is_some() {
                    errors.push(ConversionError::DuplicateValueName(name.source_span()));
                    continue;
                }
                let _slot = GlobalSlot::new(ctx, name, size.0, init);
            }
        }
    }

    for (func, parsing_blocks) in funcs {
        let mut blocks = Vec::new();
        for ParsingBlock {
            block: BlockRef { name, .. },
            params,
            insts,
            span,
        } in parsing_blocks
        {
            let block = Block::new(ctx);
            for (ValueRef { name, span }, (ty, _)) in params {
                let value = block.new_param(ctx, ty);
                if ctx.lookup_value(&name).is_some() {
                    errors.push(ConversionError::DuplicateValueName(span));
                    continue;
                }
                value.assign_name(ctx, name);
            }
            block.set_source_span(ctx, span);
            block.assign_name(ctx, name);
            func.push_back(ctx, block);
            blocks.push((block, insts));
        }

        // now we can fill the blocks
        for (block, parsing_insts) in blocks {
            for ParsingInst {
                results: result_refs,
                kind,
                operands,
                successors,
                result_tys,
                span,
            } in parsing_insts
            {
                let operands = operands
                    .into_iter()
                    // we do not support forward reference, all values should be defined before
                    // any uses (in the source code).
                    .filter_map(|ValueRef { name, span }| match ctx.lookup_value(&name) {
                        Some(value) => Some(value),
                        None => {
                            errors.push(ConversionError::UndefinedName(span));
                            None
                        }
                    })
                    .collect::<Vec<_>>();

                let result_tys = result_tys.into_iter().map(|(ty, _)| ty).collect::<Vec<_>>();

                // directly construct the instruction, add successors later (if any).
                let inst = Inst::new(ctx, kind, result_tys, operands);

                successors
                    .into_iter()
                    .for_each(|SuccRef { block, args, span }| {
                        let block = match ctx.lookup_block(&block.name) {
                            Some(block) => block,
                            None => {
                                errors.push(ConversionError::UndefinedName(span));
                                return;
                            }
                        };

                        let mut succ = Successor::new(Operand::new(ctx, block, inst));
                        let params = block.params(ctx).to_vec();

                        for (ValueRef { name, span }, param) in
                            args.into_iter().zip(params.into_iter())
                        {
                            let value = match ctx.lookup_value(&name) {
                                Some(value) => value,
                                None => {
                                    errors.push(ConversionError::UndefinedName(span));
                                    return;
                                }
                            };
                            succ.add_arg(param, Operand::new(ctx, value, inst));
                        }
                        inst.add_successor(ctx, succ);
                    });

                // set the result names
                if result_refs.len() != inst.results(ctx).len() {
                    errors.push(ConversionError::InvalidResultNum(
                        span,
                        inst.results(ctx).len(),
                        result_refs.len(),
                    ));
                }
                let results = inst.results(ctx).to_vec();
                for (ValueRef { name, span }, value) in
                    result_refs.into_iter().zip(results.into_iter())
                {
                    if ctx.lookup_value(&name).is_some() {
                        errors.push(ConversionError::DuplicateValueName(span));
                        continue;
                    }
                    value.assign_name(ctx, name);
                }

                inst.set_source_span(ctx, span);
                block.push_back(ctx, inst);
            }
        }
    }

    if errors.is_empty() && diag.is_empty() {
        Some(())
    } else {
        // make errors into diagnostics
        for error in errors {
            diag.push(error.into());
        }
        None
    }
}
