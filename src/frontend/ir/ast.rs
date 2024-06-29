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

#[derive(Debug)]
pub struct ValueRef {
    pub name: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct BlockRef {
    pub name: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct SuccRef {
    pub block: BlockRef,
    pub args: Vec<ValueRef>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsingInst {
    pub results: Vec<ValueRef>,
    pub kind: InstKind,
    pub operands: Vec<ValueRef>,
    pub successors: Vec<SuccRef>,
    pub result_tys: Vec<(Ty, Span)>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsingBlock {
    pub block: BlockRef,
    pub params: Vec<(ValueRef, (Ty, Span))>,
    pub insts: Vec<ParsingInst>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsingFunc {
    pub name: Symbol,
    pub sig: Signature,
    pub blocks: Vec<ParsingBlock>,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsingDecl {
    pub name: Symbol,
    pub sig: Signature,
    pub span: Span,
}

#[derive(Debug)]
pub struct ParsingSlot {
    pub name: Symbol,
    pub ty: (Ty, Span),
    pub init: Constant,
    pub span: Span,
}

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
/// Just convert, not check the correctness.
pub fn into_ir(ast: Vec<Item>, ctx: &mut Context, diag: &mut DiagnosticList) {
    // we need to create functions and fill them later, so we can lookup the
    // function names
    let mut funcs = Vec::new();

    // TODO: this might panic if symbols are defined, check and report errors
    for item in ast {
        match item {
            Item::Func(ParsingFunc {
                name, sig, blocks, ..
            }) => {
                let func = Func::new(ctx, name, sig);
                funcs.push((func, blocks));
            }
            Item::Decl(ParsingDecl { name, sig, .. }) => ctx.add_func_decl(name, sig),
            Item::Slot(ParsingSlot { name, ty, init, .. }) => {
                let _slot = GlobalSlot::new(ctx, name, ty.0, init);
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
                    let snippet = Diagnostic::error("duplicate value name")
                        .annotate(span.into(), "this block param name is already used");
                    diag.push(snippet);
                }
                value.assign_name(ctx, name);
            }
            block.set_source_span(ctx, span);
            block.assign_name(ctx, name);
            func.push_back(ctx, block);
            blocks.push((block, insts));
        }

        let mut errors = Vec::new();

        // now we can fill the blocks
        for (block, parsing_insts) in blocks {
            for ParsingInst {
                results: result_names,
                kind,
                operands,
                successors,
                result_tys,
                span,
            } in parsing_insts
            {
                let operands = operands
                    .into_iter()
                    .filter_map(|ValueRef { name, span }| match ctx.lookup_value(&name) {
                        Some(value) => Some(value),
                        None => {
                            errors.push(ConversionError::UndefinedName(span));
                            None
                        }
                    })
                    .collect::<Vec<_>>();

                let result_tys = result_tys.into_iter().map(|(ty, _)| ty).collect::<Vec<_>>();

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

                inst.set_source_span(ctx, span);

                // set the result names
                if result_names.len() != inst.results(ctx).len() {
                    errors.push(ConversionError::InvalidResultNum(
                        span,
                        inst.results(ctx).len(),
                        result_names.len(),
                    ));
                }

                let results = inst.results(ctx).to_vec();

                for (ValueRef { name, span }, value) in
                    result_names.into_iter().zip(results.into_iter())
                {
                    if ctx.lookup_value(&name).is_some() {
                        errors.push(ConversionError::DuplicateValueName(span));
                        continue;
                    }
                    value.assign_name(ctx, name);
                }

                block.push_back(ctx, inst);
            }
        }

        // make errors into diagnostics
        for error in errors {
            diag.push(error.into());
        }
    }
}
