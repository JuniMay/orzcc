//! # Debugger of OrzIR
//!
//! Debugger facilitates the virtual machine of OrzIR to realize interactive
//! debugging like GDB.
//!
//! Currently, there are several simple commands.
//!
//! | Command Format               |                    Description                                    |              Note             |
//! |------------------------------|-------------------------------------------------------------------|-------------------------------|
//! | `entry <name>`               | Set the entry function to debug                                   | Use function name to specify  |
//! | `continue`                   | Continue the execution                                            |                               |
//! | `step <count>`               | Step the execution by `count` instructions                        |                               |
//! | `break <inst>`               | Set a breakpoint at the instruction                               | Use ID to specify             |
//! | `show <func> <inst>`         | Show the given instruction in the function and corresponding IDs  | Use name to specify           |
//! | `quit`                       | Quit the debugger                                                 |                               |
//! | `dump-memory <addr> <size>`  | Dump the memory at the given address                              |                               |
//! | `dump-vreg <func> <value>`   | Dump the virtual registers of the function                        |                               |
//!
//! Please refer to the parse function for more details of the command.

use std::{
    collections::HashSet,
    io::{stdin, stdout, BufWriter, Write},
};

use super::{
    vm::{Addr, ExecResult, VirtualMachine},
    ExecError,
};
use crate::ir::{
    entities::FunctionKind,
    module::Module,
    values::{Function, Inst, Value, ValueIndexer},
};

pub struct Debugger<'a> {
    vm: VirtualMachine<'a>,
    module: &'a Module,
    breakpoints: HashSet<Inst>,
    last_command: Option<DebugCommand>,
}

#[derive(Clone)]
enum DebugCommand {
    Entry(String),
    Continue,
    Step(Option<usize>),
    Break(Inst),
    Show(Option<String>, Option<String>),
    Quit,
    DumpMemory(Option<Addr>, Option<usize>),
    DumpVregs(Option<String>, Option<String>),
}

impl<'a> Debugger<'a> {
    pub fn new(module: &'a Module) -> Self {
        let vm = VirtualMachine::new(module);
        Self {
            vm,
            module,
            breakpoints: HashSet::new(),
            last_command: None,
        }
    }

    fn readline(&self) -> String {
        let mut input = String::new();
        print!("(OrzDB) > ");
        let _ = stdout().flush();
        stdin().read_line(&mut input).unwrap();
        input.trim().to_string()
    }

    fn parse_command(&mut self, line: String) -> Option<DebugCommand> {
        // split the line into args
        let args = line.split_whitespace().collect::<Vec<&str>>();

        if args.is_empty() {
            return self.last_command.clone();
        }

        let command = match args[0] {
            "entry" | "e" => {
                let function_name = args.get(1).map(|s| s.to_string());
                if let Some(name) = function_name {
                    Some(DebugCommand::Entry(name))
                } else {
                    println!("invalid function name: {:?}", function_name);
                    None
                }
            }
            "continue" | "c" => Some(DebugCommand::Continue),
            "step" | "s" => {
                let count = args.get(1).and_then(|s| {
                    let count = s.parse::<usize>();
                    if let Err(e) = count {
                        println!("invalid step count: {:?}", e);
                        None
                    } else {
                        count.ok()
                    }
                });
                Some(DebugCommand::Step(count))
            }
            "break" | "b" => {
                let inst = args.get(1).and_then(|s| {
                    let inst = s.parse::<usize>();
                    if let Err(e) = inst {
                        println!("invalid instruction ID: {:?}", e);
                        None
                    } else {
                        inst.ok()
                    }
                });
                inst?;
                Some(DebugCommand::Break(Value::new(inst.unwrap()).into()))
            }
            "show" => {
                let function = args.get(1).cloned().map(|s| s.to_string());
                let inst = args.get(2).cloned().map(|s| s.to_string());
                Some(DebugCommand::Show(function, inst))
            }
            "quit" | "q" => Some(DebugCommand::Quit),
            "dump-memory" | "dm" => {
                let addr = args
                    .get(1)
                    .and_then(|s| {
                        let radix = if s.starts_with("0x") { 16 } else { 10 };
                        let addr = u64::from_str_radix(s.trim_start_matches("0x"), radix)
                            .map(|i| i as u64);
                        if let Err(e) = addr {
                            println!("invalid address: {:?}", e);
                            None
                        } else {
                            addr.ok()
                        }
                    })
                    .map(Addr::new);
                let size = args.get(2).and_then(|s| {
                    let radix = if s.starts_with("0x") { 16 } else { 10 };
                    let size =
                        u64::from_str_radix(s.trim_start_matches("0x"), radix).map(|i| i as usize);
                    if let Err(e) = size {
                        println!("invalid size: {:?}", e);
                        None
                    } else {
                        size.ok()
                    }
                });
                Some(DebugCommand::DumpMemory(addr, size))
            }
            "dump-vreg" | "dv" => {
                let function = args.get(1).cloned().map(|s| s.to_string());
                let value = args.get(2).cloned().map(|s| s.to_string());
                Some(DebugCommand::DumpVregs(function, value))
            }
            _ => None,
        };

        self.last_command = command.clone();

        command
    }

    pub fn run_vm(&mut self, steps: Option<usize>) {
        let mut steps = steps.unwrap_or(u64::MAX as usize);
        while steps > 0 {
            let res = self.vm.step();

            if let Err(e) = res {
                println!("execution error: {:?}", e);
                break;
            }

            if self.vm.stopped() {
                println!("execution finished.");
                break;
            }
            let inst = self.vm.curr_inst();

            if self.breakpoints.contains(&inst) {
                println!("breakpoint hit at {:?}", inst);
                self.show(Some(self.vm.curr_function()), Some(inst))
                    .unwrap();
                break;
            } else if steps == 1 {
                self.show(Some(self.vm.curr_function()), Some(inst))
                    .unwrap();
            }
            steps -= 1;
        }
    }

    pub fn show(&self, function: Option<Function>, inst: Option<Inst>) -> ExecResult<()> {
        let mut function_fallback_layout = vec![self.vm.curr_function()];

        let functions = if let Some(function) = function {
            function_fallback_layout.clear();
            function_fallback_layout.push(function);
            function_fallback_layout.as_slice()
        } else {
            self.module.function_layout()
        };

        for function in functions {
            let function_data = self
                .module
                .function_data(*function)
                .ok_or_else(|| ExecError::FunctionNotFound((*function).into()))?;
            let function_name = self.module.value_name((*function).into());

            let dfg = &function_data.dfg;
            let layout = &function_data.layout;

            if let FunctionKind::Definition = function_data.kind() {
                println!(
                    "[{:^4}] func {}{} {{",
                    function.index(),
                    function_name.clone(),
                    function_data.ty()
                );
            } else {
                println!(
                    "[{:^4}] decl {}{}",
                    function.index(),
                    function_name.clone(),
                    function_data.ty()
                );

                continue;
            }

            if let Some(inst) = inst {
                println!("          ...");
                println!(
                    "[{:^4}]     {}",
                    inst.index(),
                    Into::<Value>::into(inst)
                        .format_as_local_value(dfg)
                        .unwrap()
                );
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
                        writeln!(
                            buf,
                            "[{:^4}]     {}",
                            inst.index(),
                            Into::<Value>::into(inst)
                                .format_as_local_value(dfg)
                                .unwrap()
                        )
                        .unwrap();
                    }
                }
                buf.flush().unwrap();
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
            .ok_or_else(|| ExecError::FunctionNotFound(function.into()))?;
        let function_name = self.module.value_name(function.into());

        let dfg = &function_data.dfg;

        if let Some(value) = value {
            let vreg = self.vm.read_vreg(value);
            println!(
                "[{:^4}] {:10} = 0x{:016x} {:20}",
                value.index(),
                dfg.value_name(value),
                vreg.0,
                vreg.0
            );
        } else {
            println!("Virtual Registers of Function {}", function_name);
            for value in dfg.values().keys() {
                let name = dfg.value_name(*value);
                let vreg = self.vm.read_vreg(*value);
                println!(
                    "[{:^4}] {:10} = 0x{:016x} {:20}",
                    value.index(),
                    name,
                    vreg.0,
                    vreg.0
                );
            }
        }

        Ok(())
    }

    fn print_memory(&self, addr: Addr, bytes: &[u8]) {
        // format:
        // <address> | <byte 0> <byte 1> ... <byte 15> | <ascii>
        let mut ascii = String::new();
        let mut hex = String::new();
        for (i, byte) in bytes.iter().enumerate() {
            if i % 16 == 0 && i != 0 {
                println!("| {:016x} | {:48} | {:16} |", addr.0 + i as u64, hex, ascii);
                hex.clear();
                ascii.clear();
            }
            hex.push_str(&format!("{:02x} ", byte));
            ascii.push(if *byte >= 32 && *byte <= 126 {
                *byte as char
            } else {
                '.'
            });
        }
        if !hex.is_empty() {
            println!(
                "| {:016x} | {:48} | {:16} |",
                addr.0 + bytes.len() as u64,
                hex,
                ascii
            );
        }
    }

    fn dump_memory(&self, addr: Option<Addr>, size: Option<usize>) {
        match (addr, size) {
            (None, None) => {
                let memory = self.vm.memory();
                for (segment, mem) in memory {
                    println!("Memory Segment: {:?}", segment);
                    self.print_memory(segment.to_addr(0), &mem.data);
                }
            }
            (Some(addr), None) => {
                let memory = self.vm.memory();
                let segment = addr.segment().unwrap();
                let mem = memory.get(&segment).unwrap();
                self.print_memory(addr, &mem.data);
            }
            (Some(addr), Some(size)) => {
                let memory = self.vm.memory();
                let segment = addr.segment().unwrap();
                let mem = memory.get(&segment).unwrap();
                let size = size.min(mem.data.len());
                self.print_memory(addr, &mem.data[..size]);
            }
            _ => todo!(),
        }
    }

    pub fn repl(&mut self) {
        println!("Welcome to OrzDB!");
        stdout().flush().unwrap();
        loop {
            let line = self.readline();
            let command = self.parse_command(line);

            if command.is_none() {
                println!("invalid command");
                continue;
            }

            let command = command.unwrap();

            match command {
                DebugCommand::Entry(name) => {
                    let function = self.module.get_value_by_name(&name);
                    if let Some(function) = function {
                        self.vm.prepare(function.into()).unwrap();
                    } else {
                        println!("function {} is not found.", name);
                    }
                }
                DebugCommand::Continue => {
                    self.run_vm(None);
                }
                DebugCommand::Step(count) => {
                    self.run_vm(count.or(Some(1)));
                }
                DebugCommand::Break(inst) => {
                    self.breakpoints.insert(inst);
                }
                DebugCommand::Show(function, inst) => {
                    let function = function.and_then(|name| self.module.get_value_by_name(&name));
                    let inst = inst.and_then(|name| {
                        self.module
                            .function_data(
                                function
                                    .unwrap_or_else(|| self.vm.curr_function().into())
                                    .into(),
                            )
                            .and_then(|data| data.dfg.get_local_value_by_name(&name))
                    });

                    let function = function.map(|f| f.into());
                    let inst = inst.map(|i| i.into());

                    self.show(function, inst).unwrap();
                }
                DebugCommand::Quit => {
                    break;
                }
                DebugCommand::DumpMemory(addr, size) => {
                    self.dump_memory(addr, size);
                }
                DebugCommand::DumpVregs(function, value) => {
                    let function = function.and_then(|name| self.module.get_value_by_name(&name));
                    let value = value.and_then(|name| {
                        self.module
                            .function_data(
                                function
                                    .unwrap_or_else(|| self.vm.curr_function().into())
                                    .into(),
                            )
                            .and_then(|data| data.dfg.get_local_value_by_name(&name))
                    });

                    let function = function.map(|f| f.into());
                    let value = value;
                    self.dump_vregs(function, value).unwrap();
                }
            }
        }
    }
}
