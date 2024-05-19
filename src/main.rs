use clap::{Arg, Command};
use orzcc::{
    codegen::CodegenContext,
    collections::diagnostic::{Diagnostic, Level},
    frontend::sysy::sysyparser,
    ir::{
        exec::debugger::Debugger,
        frontend::{
            convert::SemanticError,
            parser::{ParseError, Parser},
        },
        module::Module,
        passes::{
            control_flow_canonicalization::ControlFlowCanonicalization,
            mem2reg::Mem2reg,
            printer::Printer,
            unreachable_block_elimination::UnreachableBlockElimination,
            GlobalPass,
            PassManager,
        },
    },
};

enum CliCommand {
    Dbg(DbgCommand),
    Opt(OptCommand),
    Frontend(FrontendCommand),
}

/// The debugger command
struct DbgCommand {
    file: String,
}

struct OptCommand {
    file: String,
    passes: Vec<String>,
    emit_ir: Option<String>,
    emit_asm: Option<String>,
}

struct FrontendCommand {
    file: String,
    emit_ast: Option<String>,
}

fn main() {
    register_passes();
    let cmd = parse_args();
    match cmd {
        CliCommand::Dbg(cmd) => {
            let module = parse_orzir(&cmd.file);
            if module.is_none() {
                return;
            }
            let module = module.unwrap();
            let mut debugger = Debugger::new(&module);
            debugger.repl();
        }
        CliCommand::Opt(cmd) => {
            let module = parse_orzir(&cmd.file);
            if module.is_none() {
                return;
            }
            let mut module = module.unwrap();
            for pass in cmd.passes {
                PassManager::run_transformation(&pass, &mut module, 32);
            }
            if let Some(emit_ir) = cmd.emit_ir {
                let mut buf = std::io::BufWriter::new(Vec::new());
                let mut printer = Printer::new(&mut buf);
                printer.run_on_module(&module).unwrap();
                std::fs::write(emit_ir, buf.get_ref()).unwrap();
            }
            if let Some(emit_asm) = cmd.emit_asm {
                let mut codegen_ctx = CodegenContext::new();
                codegen_ctx.codegen(&module);
                codegen_ctx.codegen_rest(&module);
                let machine_ctx = codegen_ctx.finish();
                let asm = machine_ctx.to_string();
                std::fs::write(emit_asm, asm).unwrap();
            }
        }
        CliCommand::Frontend(cmd) => {
            let src = std::fs::read_to_string(&cmd.file).unwrap();
            let mut ast = sysyparser::CompUnitParser::new()
                .parse(src.as_str())
                .unwrap();
            ast.type_check();
            if let Some(emit_ast) = cmd.emit_ast {
                let ast_str = format!("{:#?}", ast);
                std::fs::write(emit_ast, ast_str).unwrap();
            }
        }
    }
}

fn register_passes() {
    Mem2reg::register();
    ControlFlowCanonicalization::register();
    UnreachableBlockElimination::register();
}

fn cli() -> Command {
    Command::new("orzcc")
        .subcommand(
            Command::new("dbg").about("Debug the given IR file").arg(
                Arg::new("file")
                    .short('f')
                    .long("file")
                    .required(true)
                    .help("The IR file to debug"),
            ),
        )
        .subcommand(
            Command::new("opt")
                .about("Optimize the IR")
                .arg(
                    Arg::new("file")
                        .short('f')
                        .long("file")
                        .required(true)
                        .help("The IR file to optimize"),
                )
                .arg(
                    Arg::new("emit-ir")
                        .short('i')
                        .long("emit-ir")
                        .help("Emit the optimized IR to file"),
                )
                .arg(
                    Arg::new("emit-asm")
                        .short('a')
                        .long("emit-asm")
                        .help("Emit the optimized IR to assembly file"),
                )
                .args(PassManager::get_cli_args()),
        )
        .subcommand(
            Command::new("frontend")
                .about("Compile the Sysy code into Orzir")
                .arg(
                    Arg::new("file")
                        .short('f')
                        .long("file")
                        .required(true)
                        .help("The Sysy code file to compile into Orzir"),
                )
                .arg(
                    Arg::new("emit-ast")
                        .short('a')
                        .long("emit-ast")
                        .help("Emit the AST to file"),
                ),
        )
}

fn parse_args() -> CliCommand {
    let matches = cli().get_matches();
    match matches.subcommand() {
        Some(("dbg", args)) => {
            let file = args.get_one::<String>("file").unwrap().clone();
            CliCommand::Dbg(DbgCommand { file })
        }
        Some(("opt", args)) => {
            let file = args
                .get_one::<String>("file")
                .unwrap_or_else(|| {
                    cli().print_help().unwrap();
                    std::process::exit(1);
                })
                .clone();
            let transformation_names = PassManager::get_transformation_names();
            let parameter_names = PassManager::get_parameter_names();

            for parameter_name in parameter_names {
                let param = args.get_one::<String>(&parameter_name);
                PassManager::set_parameter(&parameter_name, param.unwrap().clone());
            }

            let mut passes = Vec::new();

            for transformation_name in transformation_names {
                if args.get_count(&transformation_name) > 0 {
                    passes.push(transformation_name);
                }
            }

            let emit_ir = args.get_one::<String>("emit-ir").cloned();
            let emit_asm = args.get_one::<String>("emit-asm").cloned();

            CliCommand::Opt(OptCommand {
                file,
                passes,
                emit_ir,
                emit_asm,
            })
        }
        Some(("frontend", args)) => {
            let file = args.get_one::<String>("file").unwrap().clone();
            let emit_ast = args.get_one::<String>("emit-ast").cloned();
            CliCommand::Frontend(FrontendCommand { file, emit_ast })
        }
        _ => {
            cli().print_help().unwrap();
            std::process::exit(1);
        }
    }
}

fn parse_orzir(path: &str) -> Option<Module> {
    let mut file = std::fs::File::open(path).unwrap();
    let mut parser = Parser::new(&mut file);
    let result = parser.parse();

    if let Err(e) = result {
        // source code in the file
        let s = std::fs::read_to_string(path).unwrap();
        let diagnostic = match e {
            ParseError::LexerError(pos) => Diagnostic::new(
                &s,
                Level::Error,
                path.to_string(),
                format!("{}", e),
                (pos.row, pos.row),
                pos.row,
                (pos.col, pos.col),
            ),
            ParseError::UnexpectedToken(span) => {
                let start = span.start;
                let end = span.end;
                Diagnostic::new(
                    &s,
                    Level::Error,
                    path.to_string(),
                    format!("{}", e),
                    (start.row, end.row),
                    start.row,
                    (start.col, end.col),
                )
            }
        };

        println!("{}", diagnostic);
        return None;
    }

    let result = result.unwrap().into_ir(path.to_string());
    if let Err(ref e) = result {
        let s = std::fs::read_to_string(path).unwrap();
        use SemanticError::*;
        match e {
            BuildError(e) => println!("error: {}", e),
            NameDuplicated(span)
            | ValueNameNotFound(span)
            | BlockNameNotFound(span)
            | TypeNotFound(span) => {
                let start = span.start;
                let end = span.end;
                let diagnostic = Diagnostic::new(
                    &s,
                    Level::Error,
                    path.to_string(),
                    format!("{}", e),
                    (start.row, end.row),
                    start.row,
                    (start.col, end.col),
                );
                println!("{}", diagnostic);
            }
        }
    }

    result.ok()
}
