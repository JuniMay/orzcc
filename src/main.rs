use clap::{Arg, Command};
use orzcc::ir::{exec::debugger::Debugger, frontend::parser::Parser};

/// The debugger command
struct DbgCommand {
    file: String,
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

fn main() {
    let cmd = parse_args();
    let mut file = std::fs::File::open(cmd.file).unwrap();
    let mut parser = Parser::new(&mut file);
    let module = parser
        .parse()
        .unwrap()
        .into_ir("debugging-module".into())
        .unwrap();
    let mut debugger = Debugger::new(&module);
    debugger.repl();
}
