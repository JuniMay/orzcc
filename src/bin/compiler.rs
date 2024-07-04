//! The compiler executable.

use clap::{Arg, Command};
use orzcc::{
    backend::{riscv64::lower::RvLowerSpec, LowerConfig, LowerContext},
    ir::{
        passes::{
            control_flow::CfgCanonicalize,
            mem2reg::{Mem2reg, MEM2REG},
        },
        passman::PassManager,
    },
};

struct CliCommand {
    /// The output assembly
    output: String,
    /// The source code
    source: String,
    /// The passes to run
    passes: Vec<String>,
    /// Emitting ast
    emit_ast: Option<String>,
    /// Emitting type-checked ast
    emit_typed_ast: Option<String>,
    /// Emitting ir
    emit_ir: Option<String>,
    /// Emitting pre-reg-alloc-asm
    emit_pre_reg_alloc_asm: Option<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    #[cfg(feature = "frontend-sysy")]
    {
        use orzcc::frontend::sysy::{self, SysYParser};

        register_passes();
        let cmd = parse_args();

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

        for pass in cmd.passes {
            PassManager::run_transform(&pass, &mut ir, 32);
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
        let mctx = lower_ctx.finish();

        std::fs::write(cmd.output, format!("{}", mctx.display()))?;
    }

    Ok(())
}

fn register_passes() {
    CfgCanonicalize::register();
    Mem2reg::register();
}

fn cli() -> Command {
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
        .arg(
            Arg::new("emit-pre-reg-alloc-asm")
                .long("emit-pre-reg-alloc-asm")
                .help("Emit the pre-register allocation assembly to the specified file"),
        )
        .args(PassManager::get_cli_args())
}

fn parse_args() -> CliCommand {
    let matches = cli().get_matches();
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
    let emit_pre_reg_alloc_asm = matches.get_one::<String>("emit-pre-reg-alloc-asm").cloned();

    let mut passes = Vec::new();
    if opt > 0 {
        passes.push(MEM2REG.to_string());
    }

    let transform_names = PassManager::get_transform_names();
    let parameter_names = PassManager::get_parameter_names();

    for parameter_name in parameter_names {
        let param = matches.get_one::<String>(&parameter_name);
        if let Some(param) = param {
            PassManager::set_parameter(&parameter_name, param.clone());
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
        passes,
        emit_ast,
        emit_typed_ast,
        emit_ir,
        emit_pre_reg_alloc_asm,
    }
}
