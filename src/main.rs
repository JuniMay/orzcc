use clap::{Arg, Command};
use orzcc::{
    collections::diagnostic::{Diagnostic, Level},
    ir::{
        exec::debugger::Debugger,
        frontend::{
            convert::SemanticError,
            parser::{ParseError, Parser},
        },
        module::Module,
    },
};

/// The debugger command
struct DbgCommand {
    file: String,
}

fn main() {
    let cmd = parse_args();
    let module = parse_orzir(&cmd.file);
    if module.is_none() {
        return;
    }
    let module = module.unwrap();
    let mut debugger = Debugger::new(&module);
    debugger.repl();
}

fn cli() -> Command {
    Command::new("orzcc").subcommand(
        Command::new("dbg").about("Debug the given IR file").arg(
            Arg::new("file")
                .short('f')
                .long("file")
                .required(true)
                .help("The IR file to debug"),
        ),
    )
}

fn parse_args() -> DbgCommand {
    let matches = cli().get_matches();
    match matches.subcommand() {
        Some(("dbg", args)) => {
            let file = args.get_one::<String>("file").unwrap().clone();
            DbgCommand { file }
        }
        _ => panic!("invalid subcommand"),
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
        match e {
            SemanticError::BuildError(e) => println!("error: {}", e),
            SemanticError::NameDuplicated(span) => {
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
            SemanticError::ValueNameNotFound(span) => {
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
            SemanticError::BlockNameNotFound(span) => {
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
