//! The cli interface to compile sysy to assembly code.

use clap::{Arg, Command};
use orzcc::{
    backend::passes::{graph_coloring_allocation::GraphColoringAllocation, GlobalPassMut},
    codegen::CodegenContext,
    frontend::sysy::{irgen::IrGenContext, preprocess, sysyparser},
    ir::passes::{
        control_flow_canonicalization::ControlFlowCanonicalization,
        mem2reg::{Mem2reg, MEM2REG},
        printer::Printer,
        straighten::{Straighten, STRAIGHTEN},
        unreachable_block_elimination::{
            UnreachableBlockElimination,
            UNREACHABLE_BLOCK_ELIMINATION,
        },
        GlobalPass,
        PassManager,
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
    register_passes();
    let cmd = parse_args();

    let src = std::fs::read_to_string(&cmd.source)?;
    let src = preprocess(&src);

    let mut ast = sysyparser::CompUnitParser::new().parse(&src).unwrap();
    if let Some(emit_ast) = &cmd.emit_ast {
        std::fs::write(emit_ast, format!("{:#?}", ast))?;
    }
    ast.type_check();
    if let Some(emit_typed_ast) = &cmd.emit_typed_ast {
        std::fs::write(emit_typed_ast, format!("{:#?}", ast))?;
    }

    let mut ctx = IrGenContext::new(cmd.source);
    ctx.irgen(ast);

    let mut module = ctx.finish();
    for pass in cmd.passes {
        PassManager::run_transformation(&pass, &mut module, 32);
    }

    if let Some(emit_ir) = &cmd.emit_ir {
        let mut buf = std::io::BufWriter::new(Vec::new());
        let mut printer = Printer::new(&mut buf);
        printer.run_on_module(&module).unwrap();
        let s = String::from_utf8(buf.into_inner().unwrap()).unwrap();
        std::fs::write(emit_ir, s).unwrap();
    }

    let mut codegen_ctx = CodegenContext::new();
    codegen_ctx.codegen(&module);
    if let Some(emit_pre_reg_alloc_asm) = &cmd.emit_pre_reg_alloc_asm {
        let asm = codegen_ctx.machine_ctx.to_string();
        std::fs::write(emit_pre_reg_alloc_asm, asm)?;
    }
    let mut register_allocator = GraphColoringAllocation::new();
    register_allocator.run_on_context(&mut codegen_ctx.machine_ctx)?;
    codegen_ctx.codegen_rest(&module);
    let assembly = codegen_ctx.finish().to_string();
    std::fs::write(&cmd.output, assembly)?;

    Ok(())
}

fn register_passes() {
    Mem2reg::register();
    ControlFlowCanonicalization::register();
    UnreachableBlockElimination::register();
    Straighten::register();
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
        passes.push(UNREACHABLE_BLOCK_ELIMINATION.to_string());
        passes.push(STRAIGHTEN.to_string());
    }

    let transformation_names = PassManager::get_transformation_names();
    let parameter_names = PassManager::get_parameter_names();

    for parameter_name in parameter_names {
        let param = matches.get_one::<String>(&parameter_name);
        if let Some(param) = param {
            PassManager::set_parameter(&parameter_name, param.clone());
        }
    }

    for transformation_name in transformation_names {
        if matches.get_count(&transformation_name) > 0 {
            passes.push(transformation_name);
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
