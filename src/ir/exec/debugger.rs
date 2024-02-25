use std::io::{stdin, stdout, Write};

use crate::ir::{
    entities::ValueKind,
    module::Module,
    values::{Function, Value},
};

use super::vm::{Addr, VirtualMachine};

pub struct Debugger<'a> {
    vm: VirtualMachine<'a>,
    module: &'a Module,
}

pub enum Command {
    Continue,
    Exit,
    Step(usize),
    DumpMemory(Addr, Addr),
    DumpVReg(Function, Value),
    Break,
}

impl<'a> Debugger<'a> {
    pub fn new(module: &'a Module) -> Self {
        let vm = VirtualMachine::new(module);
        Self { vm, module }
    }

    fn print_operand(&self, value: Value, function: Function) {
        let dfg = self.module.function_data(function).unwrap().dfg();
        dfg.with_value_data(value, |data| {
            if data.kind().is_const() {
                self.print_local_value(value, function)
            } else {
                print!("{} {}", data.ty(), dfg.value_name(value))
            }
        })
        .unwrap()
    }

    fn print_local_value(&self, value: Value, function: Function) {
        let dfg = self.module.function_data(function).unwrap().dfg();
        let data = dfg.local_value_data(value).unwrap();

        match data.kind() {
            &ValueKind::Zero => print!("{} zero", data.ty()),
            ValueKind::Undef => print!("{} undef", data.ty()),
            ValueKind::Bytes(bytes) => {
                // hexidecimal format with little endian
                print!("{} 0x", data.ty());
                if bytes.is_empty() {
                    print!("00");
                }
                for byte in bytes.iter().rev() {
                    print!("{:02x}", byte);
                }
            }
            ValueKind::Alloc(alloc) => {
                print!("({}) ", value.index());
                print!("{} = alloc {}", dfg.value_name(value), alloc.ty())
            }
            ValueKind::Load(load) => {
                print!("({}) ", value.index());
                print!("{} = load {}, ", dfg.value_name(value), data.ty());
                self.print_operand(load.ptr(), function);
            }
            ValueKind::Cast(cast) => {
                print!("({}) ", value.index());
                print!("{} = {} {}, ", dfg.value_name(value), cast.op(), data.ty());
                self.print_operand(cast.val(), function);
            }
            ValueKind::Store(store) => {
                print!("({}) ", value.index());
                print!("store ");
                self.print_operand(store.val(), function);
                print!(", ");
                self.print_operand(store.ptr(), function);
            }
            ValueKind::Binary(binary) => {
                print!("({}) ", value.index());
                print!("{} = {} ", dfg.value_name(value), binary.op());
                self.print_operand(binary.lhs(), function);
                print!(", ");
                self.print_operand(binary.rhs(), function);
            }
            ValueKind::Unary(unary) => {
                print!("({}) ", value.index());
                print!("{} = {} ", dfg.value_name(value), unary.op());
                self.print_operand(unary.val(), function);
            }
            ValueKind::Jump(jump) => {
                print!("({}) ", value.index());
                print!("jump {}(", dfg.block_name(jump.dst()));
                for (i, arg) in jump.args().iter().enumerate() {
                    if i != 0 {
                        print!(", ");
                    }
                    self.print_operand(*arg, function);
                }
                print!(")")
            }
            ValueKind::Branch(branch) => {
                print!("({}) ", value.index());
                print!("br ");
                self.print_operand(branch.cond(), function);
                print!(", {}", dfg.block_name(branch.then_dst()));
                if !branch.then_args().is_empty() {
                    print!("(");
                    for (i, arg) in branch.then_args().iter().enumerate() {
                        if i != 0 {
                            print!(", ");
                        }
                        self.print_operand(*arg, function);
                    }
                    print!(")");
                }
                print!(", {}", dfg.block_name(branch.else_dst()));
                if !branch.else_args().is_empty() {
                    print!("(");
                    for (i, arg) in branch.else_args().iter().enumerate() {
                        if i != 0 {
                            print!(", ");
                        }
                        self.print_operand(*arg, function);
                    }
                    print!(")");
                }
            }
            ValueKind::Return(ret) => {
                print!("({}) ", value.index());
                print!("ret ");
                if let Some(val) = ret.val() {
                    self.print_operand(val, function);
                }
            }
            ValueKind::Call(call) => {
                print!("({}) ", value.index());
                if !data.ty().is_void() {
                    print!("{} = ", dfg.value_name(value));
                }
                print!("call {} {}(", data.ty(), dfg.value_name(call.callee()));
                for (i, arg) in call.args().iter().enumerate() {
                    if i != 0 {
                        print!(", ");
                    }
                    self.print_operand(*arg, function);
                }
                print!(")");
            }
            ValueKind::GetElemPtr(gep) => {
                print!("({}) ", value.index());
                print!("{} = getelemptr {}, ", dfg.value_name(value), gep.ty());
                self.print_operand(gep.ptr(), function);
                for idx in gep.indices() {
                    print!(", ");
                    self.print_operand(*idx, function);
                }
            }
            _ => panic!(),
        }
    }

    fn summary(&self) {
        println!("EXECUTION SUMMARY ======================");
        for function in self.module.function_layout() {
            let function_data = self
                .module
                .function_data(*function)
                .expect("function should exist");
            println!("Function: {}", function_data.name());
            let dfg = function_data.dfg();

            for (value, _data) in dfg.values() {
                let name = dfg.value_name(*value);
                let vreg = self.vm.read_vreg(*value);

                println!("\t{:10} = {:?}", name, vreg);
            }
        }
        println!("========================================");
    }

    pub fn run(&mut self) {
        self.vm.prepare("@main").ok();

        println!("Welcome to OrzDB Debugger");

        loop {
            print!("(OrzDB) > ");
            stdout().flush().unwrap();
            let mut s = String::new();
            stdin().read_line(&mut s).unwrap();
            let s = s.trim();

            if s.is_empty() {
                continue;
            }

            let command = match s {
                "c" | "continue" => Command::Continue,
                "q" | "quit" => Command::Exit,
                "b" | "break" => Command::Break,
                "s" | "step" => Command::Step(1),
                _ => {
                    if s.starts_with("s") {
                        let n = s[1..].trim().parse().unwrap();
                        Command::Step(n)
                    } else if s.starts_with("d") {
                        let args: Vec<&str> = s[1..].split_whitespace().collect();
                        let start = Addr::new(args[0].parse().unwrap());
                        let end = Addr::new(args[1].parse().unwrap());
                        Command::DumpMemory(start, end)
                    } else if s.starts_with("v") {
                        let args: Vec<&str> = s[1..].split_whitespace().collect();
                        let function = Value::new(args[0].parse().unwrap()).into();
                        let value = Value::new(args[1].parse().unwrap());
                        Command::DumpVReg(function, value)
                    } else {
                        println!("Unknown command: {}", s);
                        continue;
                    }
                }
            };

            match command {
                Command::Continue => loop {
                    let res = self.vm.step();
                    if self.vm.stopped() {
                        break;
                    }
                    if let Err(e) = res {
                        println!("Error: {:?}", e);
                        break;
                    }
                },
                Command::Exit => {
                    break;
                }
                Command::Step(n) => {
                    if !self.vm.stopped() {
                        for _ in 0..n {
                            self.vm.step().unwrap();
                        }
                    }
                }
                Command::DumpMemory(_start, _end) => {
                    todo!()
                }
                Command::DumpVReg(function, value) => {
                    let vreg = self.vm.read_vreg(value);
                    self.print_local_value(value, function);
                    println!();
                    println!(" ==> {:?}", vreg);
                }
                Command::Break => {
                    println!("Not implemented");
                }
            }

            if self.vm.stopped() {
                self.summary();
            }
        }
    }
}
