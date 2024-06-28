use std::cmp::Ordering;

use super::ast::{BlockRef, ParsingBlock, ParsingInst, SuccRef, ValueRef};
use crate::{
    collections::{
        apint::ApInt,
        diagnostic::Diagnostic,
        parsec::{choice, just, rec, string, take_while1, until_str, Input, Parser, ParserCombine},
    },
    ir::{
        CastOp,
        Constant,
        Context,
        FBinaryOp,
        FCmpCond,
        FUnaryOp,
        FloatConstant,
        GlobalSlot,
        IBinaryOp,
        ICmpCond,
        IUnaryOp,
        InstKind,
        Signature,
        Span,
        Symbol,
        Ty,
    },
};

fn line_comment<'a>() -> impl Parser<Input<'a>, Context, Output = ()> {
    string("//").and(until_str("\n")).and(just('\n')).discard()
}

fn block_comment<'a>() -> impl Parser<Input<'a>, Context, Output = ()> {
    // let's just handle non-neasted block comments
    string("/*")
        .and(until_str("*/"))
        .and(string("*/"))
        .discard()
}

fn lexeme<'a>(s: impl Into<String>) -> impl Parser<Input<'a>, Context, Output = &'a str> {
    any_blank()
        .many()
        .and_r(string(s))
        .and_l(any_blank().many())
}

fn any_blank<'a>() -> impl Parser<Input<'a>, Context, Output = ()> {
    take_while1(|c| c.is_whitespace() || c == '\n')
        .discard()
        .or(line_comment())
        .or(block_comment())
}

fn delimiter<'a>(d: impl Into<String>) -> impl Parser<Input<'a>, Context, Output = &'a str> {
    any_blank()
        .many()
        .and_r(string(d))
        .and_l(any_blank().many())
}

fn icmp_cond<'a>() -> impl Parser<Input<'a>, Context, Output = ICmpCond> {
    choice((
        string("eq").to(ICmpCond::Eq),
        string("ne").to(ICmpCond::Ne),
        string("slt").to(ICmpCond::Slt),
        string("sle").to(ICmpCond::Sle),
        string("ult").to(ICmpCond::Ult),
        string("ule").to(ICmpCond::Ule),
    ))
}

fn fcmp_cond<'a>() -> impl Parser<Input<'a>, Context, Output = FCmpCond> {
    choice((
        string("oeq").to(FCmpCond::OEq),
        string("one").to(FCmpCond::ONe),
        string("olt").to(FCmpCond::OLt),
        string("ole").to(FCmpCond::OLe),
        string("ueq").to(FCmpCond::UEq),
        string("une").to(FCmpCond::UNe),
        string("ult").to(FCmpCond::ULt),
        string("ule").to(FCmpCond::ULe),
    ))
}

fn ibinary_op<'a>() -> impl Parser<Input<'a>, Context, Output = IBinaryOp> {
    choice((
        string("add").to(IBinaryOp::Add),
        string("sub").to(IBinaryOp::Sub),
        string("mul").to(IBinaryOp::Mul),
        string("udiv").to(IBinaryOp::UDiv),
        string("sdiv").to(IBinaryOp::SDiv),
        string("urem").to(IBinaryOp::URem),
        string("srem").to(IBinaryOp::SRem),
        string("and").to(IBinaryOp::And),
        string("or").to(IBinaryOp::Or),
        string("xor").to(IBinaryOp::Xor),
        string("shl").to(IBinaryOp::Shl),
        string("lshr").to(IBinaryOp::LShr),
        string("ashr").to(IBinaryOp::AShr),
        string::<Context>("icmp")
            .and(just('.'))
            .and_r(icmp_cond())
            .map(IBinaryOp::Cmp),
    ))
}

fn fbinary_op<'a>() -> impl Parser<Input<'a>, Context, Output = FBinaryOp> {
    choice((
        string("fadd").to(FBinaryOp::Add),
        string("fsub").to(FBinaryOp::Sub),
        string("fmul").to(FBinaryOp::Mul),
        string("fdiv").to(FBinaryOp::Div),
        string("frem").to(FBinaryOp::Rem),
        string::<Context>("fcmp")
            .and(just('.'))
            .and_r(fcmp_cond())
            .map(FBinaryOp::Cmp),
    ))
}

fn iunary_op<'a>() -> impl Parser<Input<'a>, Context, Output = IUnaryOp> {
    string("not").to(IUnaryOp::Not)
}

fn funary_op<'a>() -> impl Parser<Input<'a>, Context, Output = FUnaryOp> {
    string("fneg").to(FUnaryOp::Neg)
}

fn ty<'a>() -> impl Parser<Input<'a>, Context, Output = Ty> {
    rec(|t| {
        Box::new(choice((
            string("ptr").map_with(|_, e| Ty::ptr(e.state)),
            string("void").map_with(|_, e| Ty::void(e.state)),
            string("f32").map_with(|_, e| Ty::float32(e.state)),
            string("f64").map_with(|_, e| Ty::float64(e.state)),
            just('i')
                .and_r(take_while1(|ch| ch.is_ascii_digit()).spanned())
                .then_with(|(s, start, end), e| {
                    let bitwidth = s.parse::<u16>();
                    match bitwidth {
                        Ok(bitwidth) => Some(Ty::int(e.state, bitwidth)),
                        Err(err) => {
                            let snippet = Diagnostic::error("invalid integer bitwidth")
                                .annotate(start..end, format!("invalid bitwidth literal: {}", err))
                                .note(
                                    "help",
                                    format!("the bitwidth must be smaller than {}", u16::MAX),
                                );
                            e.add_diagnostic(snippet);
                            None
                        }
                    }
                }),
            // simd: < ty ; len >, len must be power of two, so the exp can be calculated
            t.and_l(delimiter(';'))
                .and(take_while1(|ch| ch.is_ascii_digit()).spanned())
                .between(delimiter('<'), delimiter('>'))
                .then_with(|(ty, (s, start, end)), e| {
                    let len = s.parse::<u64>();
                    match len {
                        Ok(len) => {
                            if len.is_power_of_two() {
                                Some(Ty::simd(e.state, ty, len.trailing_zeros() as u16))
                            } else {
                                let snippet = Diagnostic::error("invalid simd length")
                                    .annotate(
                                        start..end,
                                        format!("invalid simd length: {} is not power of two", len),
                                    )
                                    .note("help", "the length of simd must be power of two");
                                e.add_diagnostic(snippet);
                                None
                            }
                        }
                        Err(err) => {
                            let snippet = Diagnostic::error("invalid simd length").annotate(
                                start..end,
                                format!("invalid simd length literal: {}", err),
                            );
                            e.add_diagnostic(snippet);
                            None
                        }
                    }
                }),
            // array: [ ty ; len ]
            t.and_l(delimiter(';'))
                .and(take_while1(|ch| ch.is_ascii_digit()).spanned())
                .between(delimiter('['), delimiter(']'))
                .then_with(|(ty, (s, start, end)), e| {
                    let len = s.parse::<usize>();
                    match len {
                        Ok(len) => Some(Ty::array(e.state, ty, len)),
                        Err(err) => {
                            let snippet = Diagnostic::error("invalid array length").annotate(
                                start..end,
                                format!("invalid array length literal: {}", err),
                            );
                            e.add_diagnostic(snippet);
                            None
                        }
                    }
                }),
            // struct: { ty, ty, ty, ... }
            t.sep_by(delimiter(','))
                .between(delimiter('{'), delimiter('}'))
                .map_with(|fields, e| Ty::struct_(e.state, fields, false)),
            // packed struct: <{ ty, ty, ty, ... }>
            t.sep_by(delimiter(','))
                .between(delimiter("<{"), delimiter("}>"))
                .map_with(|fields, e| Ty::struct_(e.state, fields, true)),
        )))
    })
}

fn apint<'a>() -> impl Parser<Input<'a>, Context, Output = ApInt> {
    // apint can start with 0x, 0b, 0o or no prefix, and may ends with `iX` for
    // explicit width we can just take when no whitspace is found, and parse
    // with ApInt::try_from
    take_while1(|ch| ch.is_alphanumeric())
        .spanned()
        .then_with(|(s, start, end), e| {
            let apint = ApInt::try_from(s.as_str());
            match apint {
                Ok(apint) => Some(apint),
                Err(err) => {
                    let snippet =
                        Diagnostic::error("invalid apint").annotate(start..end, format!("{}", err));
                    e.add_diagnostic(snippet);
                    None
                }
            }
        })
}

fn float<'a>() -> impl Parser<Input<'a>, Context, Output = FloatConstant> {
    // float can be either 32 bits or 64 bits, the representation should be hex for
    // precision
    take_while1(|ch| !ch.is_whitespace())
        .spanned()
        .then_with(|(s, start, end), e| {
            // strip `0x` or `0X`
            let s = s.to_lowercase();
            let s = s.trim_start_matches("0x");
            let bits = u32::from_str_radix(s, 16);
            match bits {
                Ok(bits) => {
                    let float = FloatConstant::Float32(bits);
                    Some(float)
                }
                Err(_) => {
                    // try parse as u64
                    let bits = u64::from_str_radix(s, 16);
                    match bits {
                        Ok(bits) => {
                            let float = FloatConstant::Float64(bits);
                            Some(float)
                        }
                        Err(err) => {
                            let snippet = Diagnostic::error( "invalid float")
                                .annotate(start..end, format!("{}", err))
                                .note(
                                    "help",
                                    "the float constant must be either 32 bits or 64 bits, and represented in binary format",
                                );
                            e.add_diagnostic(snippet);
                            None
                        }
                    }
                }
            }
        })
}

fn identifier<'a>() -> impl Parser<Input<'a>, Context, Output = String> {
    take_while1(|ch| ch.is_alphanumeric() || ch == '_')
}

fn value<'a>() -> impl Parser<Input<'a>, Context, Output = ValueRef> {
    // %<ident>
    any_blank()
        .many()
        .and_r(just('%'))
        .and_r(identifier().spanned())
        .map(|(name, start, end)| ValueRef {
            name,
            span: Span::from((start, end)),
        })
}

fn label<'a>() -> impl Parser<Input<'a>, Context, Output = BlockRef> {
    // ^<ident>
    any_blank()
        .many()
        .and_r(just('^'))
        .and_r(identifier().spanned())
        .map(|(name, start, end)| BlockRef {
            name,
            span: Span::from((start, end)),
        })
}

fn symbol<'a>() -> impl Parser<Input<'a>, Context, Output = Symbol> {
    // @<ident>
    any_blank()
        .many()
        .and_r(just('@'))
        .and_r(identifier().spanned())
        .map(|(name, start, end)| Symbol::new(name).with_source_span(Span::from((start, end))))
}

fn global_init<'a>() -> impl Parser<Input<'a>, Context, Output = Constant> {
    // undef | zeroinit | [ byte, byte, ... ]
    //
    // byte is a u8 literal
    choice((
        string("undef")
            .spanned()
            .map(|(_, start, end)| Constant::undef().with_source_span(Span::from((start, end)))),
        string("zeroinit")
            .spanned()
            .map(|(_, start, end)| Constant::zeroinit().with_source_span(Span::from((start, end)))),
        // [ byte, byte, ... ]
        take_while1(|ch| ch.is_ascii_hexdigit() || ch == 'x' || ch == 'X')
            .spanned()
            .sep_by(delimiter(','))
            .between(delimiter('['), delimiter(']'))
            .spanned()
            .then_with(|(bytes, start, end), e| {
                let mut result: Vec<u8> = Vec::new();
                for (s, start, end) in bytes {
                    let s = s.to_lowercase();
                    let s = s.trim_start_matches("0x");
                    let byte = u8::from_str_radix(s, 16);
                    match byte {
                        Ok(byte) => result.push(byte),
                        Err(err) => {
                            let snippet = Diagnostic::error( "invalid byte")
                                .annotate(start..end, format!("{}", err))
                                .note("help", "each byte should be a u8 literal in hex format, with optional prefix");
                            e.add_diagnostic(snippet);
                            return None;
                        }
                    }
                }
                Some(Constant::bytes(result).with_source_span(Span::from((start, end))))
            }),
    ))
}

fn slot<'a>() -> impl Parser<Input<'a>, Context, Output = GlobalSlot> {
    lexeme("slot")
        .and_r(symbol().spanned())
        .and_l(delimiter(':'))
        .and(ty())
        .and_l(delimiter('='))
        .and(global_init())
        .then_with(|(((s, start, end), t), i), e| {
            if e.state.lookup_symbol(&s).is_some() {
                let snippet = Diagnostic::error("symbol already exists")
                    .annotate(start..end, "symbol already exists");
                e.add_diagnostic(snippet);
                return None;
            }
            let slot = GlobalSlot::new(e.state, s.clone(), t, i);
            Some(slot)
        })
}

fn decl<'a>() -> impl Parser<Input<'a>, Context, Output = ()> {
    lexeme("decl")
        .and_r(symbol().spanned())
        .and_l(any_blank().many())
        .and(signature())
        .then_with(|((s, start, end), sig), e| {
            if e.state.lookup_symbol(&s).is_some() {
                let snippet = Diagnostic::error("symbol already exists")
                    .annotate(start..end, "symbol already exists");
                e.add_diagnostic(snippet);
                return None;
            }
            e.state.add_func_decl(s, sig);
            Some(())
        })
}

fn signature<'a>() -> impl Parser<Input<'a>, Context, Output = Signature> {
    // (ty, ty, ...) -> ty or
    // (ty, ty, ...) -> (ty, ty, ...)
    // the parameter list can be empty
    ty().sep_by(delimiter(','))
        .between(delimiter('('), delimiter(')'))
        .and_l(delimiter("->"))
        .and(choice((
            ty().map(|t| vec![t]),
            ty().sep_by(delimiter(','))
                .between(delimiter('('), delimiter(')')),
        )))
        .spanned()
        .map(|((params, rets), start, end)| {
            Signature::new(params, rets).with_source_span(Span::from((start, end)))
        })
}

fn block<'a>() -> impl Parser<Input<'a>, Context, Output = ParsingBlock> {
    // ^<label> ( %param: ty, %param: ty, ... ):
    //    <inst>
    //    <inst>
    //    ...
    label()
        .and(
            value()
                .and_l(delimiter(':'))
                .and(ty())
                .sep_by(delimiter(','))
                .between(delimiter('('), delimiter(')')),
        )
        .and_l(delimiter(':'))
        .map(|(label, params)| todo!())
}

fn successor<'a>() -> impl Parser<Input<'a>, Context, Output = SuccRef> {
    // ^<label>( %arg, %arg, ... )
    // the arguments are optional
    label()
        .and(
            value()
                .sep_by(delimiter(','))
                .between(delimiter('('), delimiter(')'))
                .optional(),
        )
        .spanned()
        .map(|((block, args), start, end)| {
            let args = args.unwrap_or_default();
            SuccRef {
                block,
                args,
                span: Span::from((start, end)),
            }
        })
}

fn cast_op<'a>() -> impl Parser<Input<'a>, Context, Output = CastOp> {
    choice((
        string("trunc").to(CastOp::Trunc),
        string("fptrunc").to(CastOp::FpTrunc),
        string("zext").to(CastOp::ZExt),
        string("sext").to(CastOp::SExt),
        string("fpext").to(CastOp::FpExt),
        string("fptoui").to(CastOp::FpToUi),
        string("fptosi").to(CastOp::FpToSi),
        string("uitofp").to(CastOp::UiToFp),
        string("sitofp").to(CastOp::SiToFp),
        string("bitcast").to(CastOp::Bitcast),
        string("ptrtoint").to(CastOp::PtrToInt),
        string("inttoptr").to(CastOp::IntToPtr),
    ))
}

fn iconst<'a>() -> impl Parser<Input<'a>, Context, Output = ParsingInst> {
    // %dest = iconst <apint> : ty
    value()
        .and_l(delimiter('='))
        .and_l(lexeme("iconst"))
        .and(apint().spanned())
        .and_l(delimiter(':'))
        .and(ty().spanned())
        .spanned()
        .then_with(
            |(((dest, (apint, int_start, int_end)), (ty, ty_start, ty_end)), start, end), e| {
                let width = ty.bitwidth(e.state);
                if width.is_none() {
                    let snippet = Diagnostic::error("invalid type").annotate(
                        ty_start..ty_end,
                        format!("type {} is not a valid integer type", ty.display(e.state)),
                    );
                    e.add_diagnostic(snippet);
                    return None;
                }
                let width = width.unwrap();
                match apint.width().cmp(&width) {
                    Ordering::Less => {
                        // we can extend but report a warning
                        let snippet = Diagnostic::warn("smaller integer width")
                            .annotate(
                                int_start..int_end,
                                format!(
                                    "integer width {} is smaller than {}",
                                    apint.width(),
                                    width
                                ),
                            )
                            .annotate(ty_start..ty_end, "type declared here")
                            .note("note", "zero extending the integer");
                        e.add_diagnostic(snippet);
                        let apint = apint.into_zeroext(width);
                        Some(ParsingInst {
                            results: vec![dest],
                            kind: InstKind::IConst(apint),
                            operands: vec![],
                            successors: vec![],
                            result_tys: vec![ty],
                            span: Span::from((start, end)),
                        })
                    }
                    Ordering::Greater => {
                        let snippet = Diagnostic::error("invalid integer width")
                            .annotate(
                                int_start..int_end,
                                format!("integer width {} is larger than {}", apint.width(), width),
                            )
                            .annotate(ty_start..ty_end, "type declared here");
                        e.add_diagnostic(snippet);
                        None
                    }
                    Ordering::Equal => Some(ParsingInst {
                        results: vec![dest],
                        kind: InstKind::IConst(apint),
                        operands: vec![],
                        successors: vec![],
                        result_tys: vec![ty],
                        span: Span::from((start, end)),
                    }),
                }
            },
        )
}

fn fconst<'a>() -> impl Parser<Input<'a>, Context, Output = ParsingInst> {
    // %dest = fconst <float> : ty
    value()
        .and_l(delimiter('='))
        .and_l(lexeme("fconst"))
        .and(float().spanned())
        .and_l(delimiter(':'))
        .and(ty().spanned())
        .spanned()
        .then_with(
            |(((dest, (f, f_start, f_end)), (ty, ty_start, ty_end)), start, end), e| {
                match f {
                    FloatConstant::Float32(bits) => {
                        // check if the type is float32, if not, promote to float64
                        if ty.is_float32(e.state) {
                            Some(ParsingInst {
                                results: vec![dest],
                                kind: InstKind::FConst(FloatConstant::Float32(bits)),
                                operands: vec![],
                                successors: vec![],
                                result_tys: vec![ty],
                                span: Span::from((start, end)),
                            })
                        } else {
                            let snippet = Diagnostic::warn("promoting float32 to float64")
                                .annotate(f_start..f_end, "float32 constant promoted to float64")
                                .annotate(ty_start..ty_end, "type declared here");
                            e.add_diagnostic(snippet);
                            Some(ParsingInst {
                                results: vec![dest],
                                kind: InstKind::FConst(f.promote()),
                                operands: vec![],
                                successors: vec![],
                                result_tys: vec![ty],
                                span: Span::from((start, end)),
                            })
                        }
                    }
                    FloatConstant::Float64(bits) => {
                        if ty.is_float64(e.state) {
                            Some(ParsingInst {
                                results: vec![dest],
                                kind: InstKind::FConst(FloatConstant::Float64(bits)),
                                operands: vec![],
                                successors: vec![],
                                result_tys: vec![ty],
                                span: Span::from((start, end)),
                            })
                        } else {
                            let snippet = Diagnostic::error("invalid type").annotate(
                                ty_start..ty_end,
                                format!(
                                    "type {} is not a valid floating point type given the constant",
                                    ty.display(e.state)
                                ),
                            );
                            e.add_diagnostic(snippet);
                            None
                        }
                    }
                }
            },
        )
}

fn offset<'a>() -> impl Parser<Input<'a>, Context, Output = ParsingInst> {
    // %dest = offset %<base>, %<value> : ptr
    value()
        .and_l(delimiter('='))
        .and_l(lexeme("offset"))
        .and(value())
        .and_l(delimiter(','))
        .and(value())
        .and_l(delimiter(':'))
        .and(ty().spanned())
        .spanned()
        .then_with(
            |((((dest, base), value), (ty, ty_start, ty_end)), start, end), e| {
                if !ty.is_ptr(e.state) {
                    let snippet = Diagnostic::error("invalid type")
                        .annotate(
                            ty_start..ty_end,
                            format!("type {} is not a valid pointer type", ty.display(e.state)),
                        )
                        .note(
                            "note",
                            "expected a pointer type for the `offset` instruction",
                        );
                    e.add_diagnostic(snippet);
                    return None;
                }
                Some(ParsingInst {
                    results: vec![dest],
                    kind: InstKind::Offset,
                    operands: vec![base, value],
                    successors: vec![],
                    result_tys: vec![ty],
                    span: Span::from((start, end)),
                })
            },
        )
}

fn jump<'a>() -> impl Parser<Input<'a>, Context, Output = ParsingInst> {
    // jump %<label>
    lexeme("jump")
        .and_r(successor())
        .spanned()
        .map(|(succ, start, end)| ParsingInst {
            results: vec![],
            kind: InstKind::Jump,
            operands: vec![],
            successors: vec![succ],
            result_tys: vec![],
            span: Span::from((start, end)),
        })
}

fn branch<'a>() -> impl Parser<Input<'a>, Context, Output = ParsingInst> {
    // branch %<cond>, <succ0>, <succ1>
    lexeme("br")
        .and_r(value())
        .and_l(delimiter(','))
        .and(successor())
        .and_l(delimiter(','))
        .and(successor())
        .spanned()
        .map(|(((cond, succ0), succ1), start, end)| ParsingInst {
            results: vec![],
            kind: InstKind::Br,
            operands: vec![cond],
            successors: vec![succ0, succ1],
            result_tys: vec![],
            span: Span::from((start, end)),
        })
}

fn ret<'a>() -> impl Parser<Input<'a>, Context, Output = ParsingInst> {
    // ret %<value>
    // ret
    // ret void
    choice((
        lexeme("ret")
            .and_r(value())
            .spanned()
            .map(|(value, start, end)| ParsingInst {
                results: vec![],
                kind: InstKind::Ret,
                operands: vec![value],
                successors: vec![],
                result_tys: vec![],
                span: Span::from((start, end)),
            }),
        lexeme("ret")
            .and_r(lexeme("void"))
            .spanned()
            .map(|(_, start, end)| ParsingInst {
                results: vec![],
                kind: InstKind::Ret,
                operands: vec![],
                successors: vec![],
                result_tys: vec![],
                span: Span::from((start, end)),
            }),
        lexeme("ret").spanned().map(|(_, start, end)| ParsingInst {
            results: vec![],
            kind: InstKind::Ret,
            operands: vec![],
            successors: vec![],
            result_tys: vec![],
            span: Span::from((start, end)),
        }),
    ))
}

fn switch<'a>() -> impl Parser<Input<'a>, Context, Output = ParsingInst> {
    // switch %<cond>, apint0: succ0, apint1: succ1, ..., default: succ_default
    lexeme("switch")
        .and_r(value())
        .and_l(delimiter(','))
        .and(
            apint()
                .and_l(delimiter(':'))
                .and(successor())
                .map(|(apint, succ)| (apint, succ))
                .sep_by(delimiter(',')),
        )
        .and(
            delimiter(',')
                .and_r(lexeme("default"))
                .and(delimiter(':'))
                .and_r(successor())
                .optional(),
        )
        .spanned()
        .map(|(((cond, cases), default), start, end)| {
            let mut successors = vec![];
            let mut case_labels = vec![];
            for (apint, succ) in cases {
                case_labels.push(apint);
                successors.push(succ);
            }
            if let Some(default) = default {
                successors.push(default);
            }
            ParsingInst {
                results: vec![],
                kind: InstKind::Switch {
                    labels: case_labels,
                },
                operands: vec![cond],
                successors,
                result_tys: vec![],
                span: Span::from((start, end)),
            }
        })
}

fn call<'a>() -> impl Parser<Input<'a>, Context, Output = ParsingInst> {
    // %dest = call @<callee>(%<arg0>, %<arg1>, ...) : <ret_ty> | (<ret_ty0>,
    // <ret_ty1>, ...)
    //
    // note that dest is optional, if the return type is void.
    value()
        .sep_by(delimiter(','))
        .and_l(delimiter('='))
        .optional()
        .and(
            lexeme("call")
                .and_r(symbol())
                .and(
                    value()
                        .sep_by(delimiter(','))
                        .between(delimiter('('), delimiter(')')),
                )
                .and_l(delimiter(':'))
                .and(choice((
                    ty().map(|ty| vec![ty]),
                    ty().sep_by(delimiter(','))
                        .between(delimiter('('), delimiter(')')),
                ))),
        )
        .spanned()
        .map(
            |((results, ((symbol, args), tys)), start, end)| ParsingInst {
                results: results.unwrap_or_default(),
                kind: InstKind::Call(symbol),
                operands: args,
                successors: vec![],
                result_tys: tys,
                span: Span::from((start, end)),
            },
        )
}

fn call_indirect<'a>() -> impl Parser<Input<'a>, Context, Output = ParsingInst> {
    // call_indirect sig, %callee(%arg0, %arg1, ...) : <ret_ty> | (<ret_ty0>,
    // <ret_ty1>, ...)
    value()
        .sep_by(delimiter(','))
        .and_l(delimiter('='))
        .optional()
        .and(
            lexeme("call_indirect")
                .and_r(signature())
                .and_l(delimiter(','))
                .and(value())
                .and(
                    value()
                        .sep_by(delimiter(','))
                        .between(delimiter('('), delimiter(')')),
                )
                .and_l(delimiter(':'))
                .and(choice((
                    ty().map(|ty| vec![ty]),
                    ty().sep_by(delimiter(','))
                        .between(delimiter('('), delimiter(')')),
                ))),
        )
        .spanned()
        .map(|((results, (((sig, callee), args), tys)), start, end)| {
            let mut operands = vec![callee];
            operands.extend(args);
            ParsingInst {
                results: results.unwrap_or_default(),
                kind: InstKind::CallIndirect(sig),
                operands,
                successors: vec![],
                result_tys: tys,
                span: Span::from((start, end)),
            }
        })
}

fn get_global<'a>() -> impl Parser<Input<'a>, Context, Output = ParsingInst> {
    // %dest = get_global @<global> : ptr
    value()
        .and_l(delimiter('='))
        .and(
            lexeme("get_global")
                .and_r(symbol())
                .and_l(delimiter(':'))
                .and(ty().spanned()),
        )
        .spanned()
        .then_with(
            |((dest, (symbol, (ty, ty_start, ty_end))), start, end), e| {
                if !ty.is_ptr(e.state) {
                    let snippet = Diagnostic::error("invalid result type")
                        .annotate(ty_start..ty_end, "expected ptr for get_global instruction");
                    e.add_diagnostic(snippet);
                    return None;
                }
                Some(ParsingInst {
                    results: vec![dest],
                    kind: InstKind::GetGlobal(symbol),
                    operands: vec![],
                    successors: vec![],
                    result_tys: vec![ty],
                    span: Span::from((start, end)),
                })
            },
        )
}

// TODO: a lot

#[cfg(test)]
mod tests {
    use super::*;
    use crate::collections::{
        diagnostic::{DiagnosticList, RenderOptions},
        parsec::{Extra, Parser},
    };

    #[test]
    fn test_ibinary_op() {
        let mut input = Input::new("add");
        let parser = ibinary_op();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        assert_eq!(result, Some(IBinaryOp::Add));
        assert_eq!(input.as_str(), "");

        let mut input = Input::new("icmp.eq");
        let parser = ibinary_op();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        assert_eq!(result, Some(IBinaryOp::Cmp(ICmpCond::Eq)));
        assert_eq!(input.as_str(), "");
    }

    #[test]
    fn test_ty() {
        let mut input = Input::new("i32");
        let parser = ty();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        assert_eq!(result, Some(Ty::int(&mut ctx, 32)));
        assert_eq!(input.as_str(), "");

        // invalid
        let mut input = Input::new("i1145141919810");
        let parser = ty();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        assert_eq!(result, None);
        assert_eq!(diag.len(), 1);

        println!(
            "{}",
            diag.render("i1145141919810", &RenderOptions::unicode_round())
        );
    }

    #[test]
    fn test_comments() {
        let mut input = Input::new("// this is a comment\na");
        let parser = line_comment();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        assert_eq!(result, Some(()));
        assert_eq!(input.as_str(), "a");

        let mut input = Input::new("/* this is\n a comment */a");
        let parser = block_comment();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        assert_eq!(result, Some(()));
        assert_eq!(input.as_str(), "a");
    }

    #[test]
    fn test_semicolon() {
        let mut input = Input::new("   /* 123 */ ; // 123\na");
        let parser = delimiter(';');
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        assert_eq!(result, Some(";"));
        assert_eq!(input.as_str(), "a");
    }

    #[test]
    fn test_simd_ty() {
        let mut input = Input::new("<i32; 4>");
        let parser = ty();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        let int = Ty::int(&mut ctx, 32);
        let simd = Ty::simd(&mut ctx, int, 2);

        assert_eq!(result, Some(simd));
        assert_eq!(input.as_str(), "");

        let mut input = Input::new("<i32; 3>");
        let parser = ty();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        assert_eq!(result, None);
        assert_eq!(diag.len(), 1);

        println!(
            "{}",
            diag.render("<i32; 3>", &RenderOptions::unicode_round())
        );
    }

    #[test]
    fn test_struct_ty() {
        let mut input = Input::new("{i32, i32}");
        let parser = ty();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        let int = Ty::int(&mut ctx, 32);
        let struct_ty = Ty::struct_(&mut ctx, vec![int, int], false);

        assert_eq!(result, Some(struct_ty));
        assert_eq!(input.as_str(), "");

        let mut input = Input::new("{i32, i32");
        let parser = ty();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        assert_eq!(result, None);
        assert_eq!(diag.len(), 1);
        // this reports the last error (expect `<{`), but we want it to report `unclosed
        // struct`
        println!(
            "{}",
            diag.render("{i32, i32", &RenderOptions::unicode_round())
        );
    }

    #[test]
    fn test_apint() {
        let mut input = Input::new("0x12345i32");
        let parser = apint();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        assert_eq!(result, Some(ApInt::try_from("0x12345i32").unwrap()));

        // failed to parse
        let mut input = Input::new("0x12345i32i32");
        let parser = apint();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        assert_eq!(result, None);
        assert_eq!(diag.len(), 1);

        println!(
            "{}",
            diag.render("0x12345i32i32", &RenderOptions::unicode_round())
        );
    }

    #[test]
    fn test_float() {
        let mut input = Input::new("0x12343");
        let parser = float();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        assert_eq!(result, Some(FloatConstant::Float32(0x12343)));

        // try a float64
        let mut input = Input::new("0x1234567890");
        let parser = float();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        assert_eq!(result, Some(FloatConstant::Float64(0x1234567890)));
    }

    #[test]
    fn test_global_init() {
        let mut input = Input::new("[ 0xef, 0xbe, 0xad, 0xde ]");
        let parser = global_init();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        assert_eq!(result, Some(Constant::bytes(vec![0xef, 0xbe, 0xad, 0xde])));
        assert_eq!(input.as_str(), "");
    }

    #[test]
    fn test_signature() {
        let src = "() -> void";
        let mut input = Input::new(src);
        let parser = signature();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        let void = Ty::void(&mut ctx);
        let sig = Signature::new(vec![], vec![void]);
        assert_eq!(result, Some(sig));

        let src = "(i32, f32) -> (i32, f32)";
        let mut input = Input::new(src);
        let parser = signature();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        let i32 = Ty::int(&mut ctx, 32);
        let f32 = Ty::float32(&mut ctx);
        let sig = Signature::new(vec![i32, f32], vec![i32, f32]);

        assert_eq!(result, Some(sig));

        let src = "(i32, f32, <i32; 4>) -> i1";
        let mut input = Input::new(src);
        let parser = signature();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);

        let i1 = Ty::int(&mut ctx, 1);
        let simd = Ty::simd(&mut ctx, i32, 2);
        let sig = Signature::new(vec![i32, f32, simd], vec![i1]);

        assert_eq!(result, Some(sig));
    }

    #[test]
    fn test_iconst() {
        let src = "%dst = iconst 42 : i32";
        let mut input = Input::new(src);
        let parser = iconst();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);
        println!("{:#?}", result);
        assert_eq!(diag.len(), 1);
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
    }

    #[test]
    fn test_offset() {
        let src = "%dst = offset %base , %val : ptr";
        let mut input = Input::new(src);
        let parser = offset();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);
        println!("{:#?}", result);
        assert_eq!(diag.len(), 0);
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
    }

    #[test]
    fn test_jump() {
        let src = "jump ^dst (%v1, %v2)";
        let mut input = Input::new(src);
        let parser = jump();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);
        println!("{:#?}", result);
        assert_eq!(diag.len(), 0);
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
    }

    #[test]
    fn test_call() {
        let src = "%dst = call @dst (%v1, %v2) : i32";
        let mut input = Input::new(src);
        let parser = call();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);
        println!("{:#?}", result);
        assert_eq!(diag.len(), 0);
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
    }

    #[test]
    fn test_branch() {
        let src = "br %cond, ^dst0 (%v1, %v2), ^dst1 (%v3)";
        let mut input = Input::new(src);
        let parser = branch();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);
        println!("{:#?}", result);
        assert_eq!(diag.len(), 0);
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
    }

    #[test]
    fn test_switch() {
        let src = "switch %cond, 1: ^dst0 (%v1, %v2), 2: ^dst1 (%v3), default: ^dst2 (%v4)";
        let mut input = Input::new(src);
        let parser = switch();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);
        println!("{:#?}", result);
        assert_eq!(diag.len(), 0);
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
        println!("{}", input.as_str());
    }

    #[test]
    fn test_call_indirect() {
        let src =
            "%dst0, %dst1 = call_indirect (i32, f32) -> (i32, i1), %index (%v1, %v2) : (i32, i1)";
        let mut input = Input::new(src);
        let parser = call_indirect();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);
        println!("{:#?}", result);
        assert_eq!(diag.len(), 0);
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
    }

    #[test]
    fn test_get_global() {
        let src = "%dst = get_global @dst : ptr";
        let mut input = Input::new(src);
        let parser = get_global();
        let mut diag = DiagnosticList::default();
        let mut ctx = Context::default();
        let mut extra = Extra::new(&mut diag, &mut ctx);
        let result = parser.parse(&mut input, &mut extra);
        println!("{:#?}", result);
        assert_eq!(diag.len(), 0);
        println!("{}", diag.render(src, &RenderOptions::unicode_round()));
    }
}
