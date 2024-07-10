//! The compiler executable.

use clap::{Arg, Command};
use orzcc::ir::{
    passes::{
        control_flow::{CfgCanonicalize, CfgSimplify, CFG_SIMPLIFY},
        fold::{ConstantFolding, CONSTANT_FOLDING},
        instcombine::{InstCombine, INSTCOMBINE},
        loops::{LoopInvariantMotion, LOOP_INVARIANT_MOTION},
        mem2reg::{Mem2reg, MEM2REG},
        simple_dce::{SimpleDce, SIMPLE_DCE},
    },
    passman::{PassManager, Pipeline, TransformPass},
};

struct CliCommand {
    /// The output assembly
    output: String,
    /// The source code
    source: String,
    /// Emitting ast
    emit_ast: Option<String>,
    /// Emitting type-checked ast
    emit_typed_ast: Option<String>,
    /// Emitting ir
    emit_ir: Option<String>,
    /// Optimization level
    opt: u8,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    #[cfg(feature = "frontend-sysy")]
    {
        use orzcc::{
            backend::{riscv64::lower::RvLowerSpec, LowerConfig, LowerContext},
            frontend::sysy::{self, SysYParser},
        };

        let mut passman = PassManager::default();

        register_passes(&mut passman);
        let cmd = parse_args(&mut passman);

        let src = std::fs::read_to_string(&cmd.source)?;
        let src = sysy::preprocess(&src);

        let mut ast = SysYParser::new().parse(&src).unwrap();
        if let Some(emit_ast) = &cmd.emit_ast {
            std::fs::write(emit_ast, format!("{:#?}", ast))?;
        }

        ast.type_check();
        if let Some(emit_typed_ast) = &cmd.emit_typed_ast {
            std::fs::write(emit_typed_ast, format!("{:#?}", ast))?;
        }

        let mut ir = sysy::irgen(&ast);

        if cmd.opt > 0 {
            passman.run_transform(MEM2REG, &mut ir, 1);
            passman.run_transform(LOOP_INVARIANT_MOTION, &mut ir, 1);

            let mut opt_pipeline = Pipeline::default();
            opt_pipeline.add_pass(CFG_SIMPLIFY);
            opt_pipeline.add_pass(CONSTANT_FOLDING);
            opt_pipeline.add_pass(SIMPLE_DCE);
            opt_pipeline.add_pass(INSTCOMBINE);
            opt_pipeline.add_pass(SIMPLE_DCE);

            let _iter = passman.run_pipeline(&mut ir, &opt_pipeline, 32, 8);
        }

        ir.alloc_all_names();

        if let Some(emit_ir) = &cmd.emit_ir {
            std::fs::write(emit_ir, format!("{}", ir.display(true)))?;
        }

        let mut lower_ctx: LowerContext<RvLowerSpec> = LowerContext::new(
            &ir,
            LowerConfig {
                omit_frame_pointer: true,
            },
        );
        lower_ctx.lower();
        lower_ctx.reg_alloc();
        lower_ctx.after_regalloc();
        let mctx = lower_ctx.finish();
        std::fs::write(cmd.output, format!("{}", mctx.display()))?;
    }

    Ok(())
}

fn register_passes(passman: &mut PassManager) {
    CfgCanonicalize::register(passman);
    CfgSimplify::register(passman);

    Mem2reg::register(passman);
    SimpleDce::register(passman);
    ConstantFolding::register(passman);
    InstCombine::register(passman);

    LoopInvariantMotion::register(passman);
}

fn cli(passman: &mut PassManager) -> Command {
    Command::new("compiler")
        .arg(
            Arg::new("output")
                .short('o')
                .required(true)
                .help("The output assembly"),
        )
        .arg(Arg::new("source").required(true).help("The source code"))
        .arg(
            Arg::new("s_flag")
                .short('S')
                .action(clap::ArgAction::Count)
                .help("Output an assembly file"),
        )
        .arg(
            Arg::new("opt")
                .short('O')
                .help("Optimization level")
                .default_value("0"),
        )
        .arg(
            Arg::new("emit-ast")
                .long("emit-ast")
                .help("Emit the AST to the specified file"),
        )
        .arg(
            Arg::new("emit-typed-ast")
                .long("emit-typed-ast")
                .help("Emit the typed AST to the specified file"),
        )
        .arg(
            Arg::new("emit-ir")
                .long("emit-ir")
                .help("Emit the IR to the specified file"),
        )
        .args(passman.get_cli_args())
}

fn parse_args(passman: &mut PassManager) -> CliCommand {
    let matches = cli(passman).get_matches();
    let output = matches.get_one::<String>("output").unwrap().clone();
    let source = matches.get_one::<String>("source").unwrap().clone();
    let _s_flag = matches.get_count("s_flag") > 0; // just a placeholder
    let opt = matches
        .get_one::<String>("opt")
        .unwrap()
        .parse::<u8>()
        .unwrap();

    let emit_ast = matches.get_one::<String>("emit-ast").cloned();
    let emit_typed_ast = matches.get_one::<String>("emit-typed-ast").cloned();
    let emit_ir = matches.get_one::<String>("emit-ir").cloned();

    let mut passes = Vec::new();

    let transform_names = passman.gather_transform_names();
    let parameter_names = passman.gather_parameter_names();

    for parameter_name in parameter_names {
        let param = matches.get_one::<String>(&parameter_name);
        if let Some(param) = param {
            passman.set_parameter(&parameter_name, param.clone());
        }
    }

    for transform_name in transform_names {
        if matches.get_count(&transform_name) > 0 {
            passes.push(transform_name);
        }
    }

    CliCommand {
        output,
        source,
        emit_ast,
        emit_typed_ast,
        emit_ir,
        opt,
    }
}
