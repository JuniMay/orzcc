use std::collections::HashSet;
use std::io::{stdin, stdout, BufWriter, Write};

use crate::ir::entities::FunctionKind;
use crate::ir::values::{Inst, ValueIndexer};
use crate::ir::{
    module::Module,
    values::{Function, Value},
};
use crate::passes::printer::Printer;

use super::vm::{Addr, ExecResult, VirtualMachine};
use super::ExecErr;

pub struct Debugger<'a> {
    vm: VirtualMachine<'a>,
    module: &'a Module,
    breakpoints: HashSet<Inst>,
}

enum DebugCommand {
    Entry(String),
    Continue,
    Step(Option<usize>),
    Break(Inst),
    Show(Option<Function>, Option<Inst>),
    Quit,
    DumpMemory(Option<Addr>, Option<usize>),
    DumpVregs(Option<Function>, Option<Value>),
}

impl<'a> Debugger<'a> {
    pub fn new(module: &'a Module) -> Self {
        let vm = VirtualMachine::new(module);
        Self {
            vm,
            module,
            breakpoints: HashSet::new(),
        }
    }

    fn readline(&self) -> String {
        let mut input = String::new();
        print!("(OrzDB) > ");
        let _ = stdout().flush();
        stdin().read_line(&mut input).unwrap();
        input.trim().to_string()
    }

    fn parse_command(&self, line: String) -> Option<DebugCommand> {
        // split the line into args
        let args = line.split_whitespace().collect::<Vec<&str>>();

        if args.is_empty() {
            return None;
        }

        let command = match args[0] {
            "entry" | "e" => {
                let function_name = args.get(1).map(|s| s.to_string());
                if let Some(name) = function_name {
                    Some(DebugCommand::Entry(name.clone()))
                } else {
                    println!("invalid function name: {:?}", function_name);
                    None
                }
            }
            "continue" | "c" => Some(DebugCommand::Continue),
            "step" | "s" => {
                let count = args.get(1).map(|s| s.parse::<usize>().unwrap());
                Some(DebugCommand::Step(count))
            }
            "break" | "b" => {
                let inst = args.get(1).map(|s| s.parse::<usize>().unwrap());
                Some(DebugCommand::Break(Value::new(inst.unwrap()).into()))
            }
            "show" => {
                let function = args.get(1).map(|s| s.parse::<usize>().unwrap());
                let inst = args.get(2).map(|s| s.parse::<usize>().unwrap());
                Some(DebugCommand::Show(
                    function.map(|i| Function::new(i)),
                    inst.map(|i| Value::new(i).into()),
                ))
            }
            "quit" | "q" => Some(DebugCommand::Quit),
            "dump-memory" | "dm" => {
                let addr = args.get(1).map(|s| s.parse::<u64>().unwrap());
                let size = args.get(2).map(|s| s.parse::<usize>().unwrap());
                Some(DebugCommand::DumpMemory(addr.map(|i| Addr::new(i)), size))
            }
            "dump-vreg" | "dv" => {
                let function = args.get(1).map(|s| s.parse::<usize>().unwrap());
                let value = args.get(2).map(|s| s.parse::<usize>().unwrap());
                Some(DebugCommand::DumpVregs(
                    function.map(|i| Function::new(i)),
                    value.map(|i| Value::new(i)),
                ))
            }
            _ => None,
        };

        command
    }

    pub fn run_vm(&mut self, steps: Option<usize>) -> ExecResult<()> {
        let mut steps = steps.unwrap_or(u64::MAX as usize);
        while steps > 0 {
            self.vm.step()?;
            if self.vm.stopped() {
                println!("execution finished.");
                break;
            }
            let inst = self.vm.curr_inst();
            if self.breakpoints.contains(&inst) {
                println!("breakpoint hit at {:?}", inst);
                self.show(Some(self.vm.curr_function()), Some(inst))
                    .unwrap();
                return Ok(());
            }
            steps -= 1;
        }
        Ok(())
    }

    pub fn show(&self, function: Option<Function>, inst: Option<Inst>) -> ExecResult<()> {
        let function_fallback_layout = vec![self.vm.curr_function()];

        let functions = if function.is_none() {
            self.module.function_layout()
        } else {
            &function_fallback_layout
        };

        for function in functions {
            let function_data = self
                .module
                .function_data(*function)
                .ok_or(ExecErr::FunctionNotFound((*function).into()))?;

            let dfg = function_data.dfg();
            let layout = function_data.layout();

            if let FunctionKind::Definition = function_data.kind() {
                println!(
                    "[{:^4}] func {}{} {{",
                    function.index(),
                    function_data.name(),
                    function_data.ty()
                );
            } else {
                println!(
                    "[{:^4}] decl {}{}",
                    function.index(),
                    function_data.name(),
                    function_data.ty()
                );

                continue;
            }

            if let Some(inst) = inst {
                println!("          ...");
                let mut buf = BufWriter::new(Vec::new());
                let mut printer = Printer::new(&mut buf);
                printer.print_local_value(inst.into(), dfg).unwrap();
                let _ = buf.flush().unwrap();
                let output = String::from_utf8(buf.into_inner().unwrap()).unwrap();
                println!("[{:^4}]     {}", inst.index(), output);
                println!("          ...");
            } else {
                let mut buf = BufWriter::new(Vec::new());
                for (block, node) in layout.blocks() {
                    let block_data = dfg.block_data(block).unwrap();

                    write!(buf, "[{:^4}] {}", block.index(), dfg.block_name(block)).unwrap();

                    if !block_data.params().is_empty() {
                        write!(buf, "(").unwrap();
                        for (i, param) in block_data.params().iter().enumerate() {
                            if i != 0 {
                                write!(buf, ", ").unwrap();
                            }
                            write!(
                                buf,
                                "{} {}",
                                dfg.local_value_data(*param).unwrap().ty(),
                                dfg.value_name(*param)
                            )
                            .unwrap();
                        }
                        writeln!(buf, "):").unwrap();
                    } else {
                        writeln!(buf, ":").unwrap();
                    }

                    for (inst, _node) in node.insts() {
                        write!(buf, "[{:^4}]    ", inst.index()).unwrap();
                        // inefficient implementation, should be optimized
                        let mut inner_buf = BufWriter::new(Vec::new());
                        let mut printer = Printer::new(&mut inner_buf);
                        printer.print_local_value(inst.into(), dfg).unwrap();
                        let _ = inner_buf.flush().unwrap();
                        let output = String::from_utf8(inner_buf.into_inner().unwrap()).unwrap();

                        write!(buf, "{}", output).unwrap();
                        writeln!(buf).unwrap();
                    }
                }
                let _ = buf.flush().unwrap();
                let output = String::from_utf8(buf.into_inner().unwrap()).unwrap();

                println!("{}", output);
            }

            println!("[{:^4}] }}", function.index());
        }
        Ok(())
    }

    fn dump_vregs(&self, function: Option<Function>, value: Option<Value>) -> ExecResult<()> {
        let function = function.unwrap_or_else(|| self.vm.curr_function());
        let function_data = self
            .module
            .function_data(function)
            .ok_or(ExecErr::FunctionNotFound(function.into()))?;

        let dfg = function_data.dfg();

        if let Some(value) = value {
            let vreg = self.vm.read_vreg(value);
            println!("{:?}", vreg);
        } else {
            println!("Virtual Registers of Function {}", function_data.name());
            for (value, _data) in dfg.values() {
                let name = dfg.value_name(*value);
                let vreg = self.vm.read_vreg(*value);
                println!("[{:^4}] {:10} = {:?}", value.index(), name, vreg);
            }
        }

        Ok(())
    }

    pub fn repl(&mut self) {
        println!("Welcome to OrzDB!");
        let _ = stdout().flush().unwrap();
        loop {
            let line = self.readline();
            let command = self.parse_command(line);

            if command.is_none() {
                println!("invalid command");
                continue;
            }

            let command = command.unwrap();

            // TODO: error handling
            match command {
                DebugCommand::Entry(name) => {
                    self.vm.prepare(&name).unwrap();
                }
                DebugCommand::Continue => {
                    self.run_vm(None).unwrap();
                }
                DebugCommand::Step(count) => {
                    self.run_vm(count.or(Some(1))).unwrap();
                }
                DebugCommand::Break(inst) => {
                    self.breakpoints.insert(inst);
                }
                DebugCommand::Show(function, inst) => {
                    self.show(function, inst).unwrap();
                }
                DebugCommand::Quit => {
                    break;
                }
                DebugCommand::DumpMemory(addr, size) => {
                    todo!()
                }
                DebugCommand::DumpVregs(function, value) => {
                    self.dump_vregs(function, value).unwrap();
                }
            }
        }
    }
}
