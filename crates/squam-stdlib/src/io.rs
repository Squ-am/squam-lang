use squam_vm::{Value, VM};
use std::cell::RefCell;
use std::io::{self, BufRead, Write};
use std::rc::Rc;

pub fn register(vm: &mut VM) {
    // print(value: any) -> ()
    // Already registered in VM, but we can override with a better version
    vm.define_native("print", 1, |args| {
        print!("{}", args[0]);
        io::stdout().flush().ok();
        Ok(Value::Unit)
    });

    // println(value: any) -> ()
    vm.define_native("println", 1, |args| {
        println!("{}", args[0]);
        Ok(Value::Unit)
    });

    // eprint(value: any) -> ()
    vm.define_native("eprint", 1, |args| {
        eprint!("{}", args[0]);
        io::stderr().flush().ok();
        Ok(Value::Unit)
    });

    // eprintln(value: any) -> ()
    vm.define_native("eprintln", 1, |args| {
        eprintln!("{}", args[0]);
        Ok(Value::Unit)
    });

    // debug(value: any) -> string
    // Returns a debug representation of the value (more detailed than Display)
    vm.define_native("debug", 1, |args| {
        Ok(Value::String(Rc::new(format!("{:?}", args[0]))))
    });

    // debug_print(value: any) -> ()
    // Prints debug representation
    vm.define_native("debug_print", 1, |args| {
        println!("{:?}", args[0]);
        Ok(Value::Unit)
    });

    // clone_value(value: any) -> any
    // Explicitly clone a value (in Squam, values are cloned on assignment anyway)
    vm.define_native("clone_value", 1, |args| Ok(args[0].clone()));

    // equals(a: any, b: any) -> bool
    // Explicit equality check (same as == operator)
    vm.define_native("equals", 2, |args| Ok(Value::Bool(args[0] == args[1])));

    // read_line() -> string
    vm.define_native("read_line", 0, |_args| {
        let mut line = String::new();
        io::stdin()
            .lock()
            .read_line(&mut line)
            .map_err(|e| format!("read_line: {}", e))?;
        // Remove trailing newline
        if line.ends_with('\n') {
            line.pop();
            if line.ends_with('\r') {
                line.pop();
            }
        }
        Ok(Value::String(Rc::new(line)))
    });

    // input(prompt: string) -> string
    vm.define_native("input", 1, |args| match &args[0] {
        Value::String(prompt) => {
            print!("{}", prompt);
            io::stdout().flush().ok();

            let mut line = String::new();
            io::stdin()
                .lock()
                .read_line(&mut line)
                .map_err(|e| format!("input: {}", e))?;

            if line.ends_with('\n') {
                line.pop();
                if line.ends_with('\r') {
                    line.pop();
                }
            }
            Ok(Value::String(Rc::new(line)))
        }
        other => Err(format!(
            "input: expected string prompt, got {}",
            other.type_name()
        )),
    });

    // assert(condition: bool) -> ()
    vm.define_native("assert", 1, |args| match &args[0] {
        Value::Bool(true) => Ok(Value::Unit),
        Value::Bool(false) => Err("assertion failed".to_string()),
        other => {
            if other.is_truthy() {
                Ok(Value::Unit)
            } else {
                Err("assertion failed".to_string())
            }
        }
    });

    // assert_eq(a: any, b: any) -> ()
    vm.define_native("assert_eq", 2, |args| {
        if args[0] == args[1] {
            Ok(Value::Unit)
        } else {
            Err(format!("assertion failed: {:?} != {:?}", args[0], args[1]))
        }
    });

    // panic(message: string) -> !
    vm.define_native("panic", 1, |args| Err(format!("panic: {}", args[0])));

    // unreachable() -> !
    vm.define_native("unreachable", 0, |_args| {
        Err("unreachable code reached".to_string())
    });

    // todo() -> !
    vm.define_native("todo", 0, |_args| Err("not yet implemented".to_string()));

    // dbg(value: any) -> any
    // Like Rust's dbg! macro - prints and returns the value
    vm.define_native("dbg", 1, |args| {
        eprintln!("[dbg] {:?}", args[0]);
        Ok(args[0].clone())
    });

    // format(template: string, args...) -> string
    // Simple format with {} placeholders
    // For now, just support a single argument
    vm.define_native("format", 2, |args| match &args[0] {
        Value::String(template) => {
            let result = template.replacen("{}", &format!("{}", args[1]), 1);
            Ok(Value::String(Rc::new(result)))
        }
        other => Err(format!(
            "format: expected string template, got {}",
            other.type_name()
        )),
    });

    // read_file(path: string) -> string
    vm.define_native("read_file", 1, |args| match &args[0] {
        Value::String(path) => std::fs::read_to_string(path.as_str())
            .map(|s| Value::String(Rc::new(s)))
            .map_err(|e| format!("read_file: {}", e)),
        other => Err(format!(
            "read_file: expected string path, got {}",
            other.type_name()
        )),
    });

    // write_file(path: string, content: string) -> ()
    vm.define_native("write_file", 2, |args| match (&args[0], &args[1]) {
        (Value::String(path), Value::String(content)) => {
            std::fs::write(path.as_str(), content.as_str())
                .map(|_| Value::Unit)
                .map_err(|e| format!("write_file: {}", e))
        }
        _ => Err("write_file: expected (string, string)".to_string()),
    });

    // append_file(path: string, content: string) -> ()
    vm.define_native("append_file", 2, |args| match (&args[0], &args[1]) {
        (Value::String(path), Value::String(content)) => {
            use std::fs::OpenOptions;
            OpenOptions::new()
                .append(true)
                .create(true)
                .open(path.as_str())
                .and_then(|mut f| f.write_all(content.as_bytes()))
                .map(|_| Value::Unit)
                .map_err(|e| format!("append_file: {}", e))
        }
        _ => Err("append_file: expected (string, string)".to_string()),
    });

    // file_exists(path: string) -> bool
    vm.define_native("file_exists", 1, |args| match &args[0] {
        Value::String(path) => Ok(Value::Bool(std::path::Path::new(path.as_str()).exists())),
        other => Err(format!(
            "file_exists: expected string, got {}",
            other.type_name()
        )),
    });

    // read_lines(path: string) -> array
    vm.define_native("read_lines", 1, |args| match &args[0] {
        Value::String(path) => {
            let content =
                std::fs::read_to_string(path.as_str()).map_err(|e| format!("read_lines: {}", e))?;
            let lines: Vec<Value> = content
                .lines()
                .map(|l| Value::String(Rc::new(l.to_string())))
                .collect();
            Ok(Value::Array(Rc::new(RefCell::new(lines))))
        }
        other => Err(format!(
            "read_lines: expected string, got {}",
            other.type_name()
        )),
    });

    // env_var(name: string) -> string
    vm.define_native("env_var", 1, |args| match &args[0] {
        Value::String(name) => std::env::var(name.as_str())
            .map(|v| Value::String(Rc::new(v)))
            .map_err(|_| format!("env_var: {} not found", name)),
        other => Err(format!(
            "env_var: expected string, got {}",
            other.type_name()
        )),
    });

    // env_var_or(name: string, default: string) -> string
    vm.define_native("env_var_or", 2, |args| match (&args[0], &args[1]) {
        (Value::String(name), Value::String(default)) => Ok(Value::String(Rc::new(
            std::env::var(name.as_str()).unwrap_or_else(|_| default.to_string()),
        ))),
        _ => Err("env_var_or: expected (string, string)".to_string()),
    });

    // args() -> array
    vm.define_native("args", 0, |_args| {
        let args: Vec<Value> = std::env::args()
            .map(|a| Value::String(Rc::new(a)))
            .collect();
        Ok(Value::Array(Rc::new(RefCell::new(args))))
    });
}
