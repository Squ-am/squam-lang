use std::cell::RefCell;
use std::rc::Rc;

use serde::Serialize;
use squam_compiler::Compiler;
use squam_parser::Parser;
use squam_vm::{Value, VM};
use wasm_bindgen::prelude::*;

// Thread-local output buffer for capturing print statements
thread_local! {
    static OUTPUT: RefCell<String> = RefCell::new(String::new());
}

fn append_output(s: &str) {
    OUTPUT.with(|output| {
        output.borrow_mut().push_str(s);
    });
}

fn take_output() -> String {
    OUTPUT.with(|output| {
        std::mem::take(&mut *output.borrow_mut())
    })
}

/// Result of running Squam code
#[derive(Serialize)]
pub struct RunResult {
    pub success: bool,
    pub output: String,
    pub result: Option<String>,
    pub error: Option<String>,
}

/// Register stdlib with output capture
fn register_stdlib_wasm(vm: &mut VM) {
    // Register all stdlib except IO
    squam_stdlib::string::register(vm);
    squam_stdlib::vec::register(vm);
    squam_stdlib::math::register(vm);
    squam_stdlib::option::register(vm);
    squam_stdlib::result::register(vm);
    squam_stdlib::iter::register(vm);
    squam_stdlib::hashmap::register(vm);
    squam_stdlib::hashset::register(vm);

    // Override print functions to capture output
    vm.define_native("print", 1, |args| {
        append_output(&format!("{}", args[0]));
        Ok(Value::Unit)
    });

    vm.define_native("println", 1, |args| {
        append_output(&format!("{}\n", args[0]));
        Ok(Value::Unit)
    });

    vm.define_native("eprint", 1, |args| {
        append_output(&format!("{}", args[0]));
        Ok(Value::Unit)
    });

    vm.define_native("eprintln", 1, |args| {
        append_output(&format!("{}\n", args[0]));
        Ok(Value::Unit)
    });

    vm.define_native("debug", 1, |args| {
        append_output(&format!("{:?}\n", args[0]));
        Ok(Value::Unit)
    });

    vm.define_native("dbg", 1, |args| {
        append_output(&format!("[dbg] {:?}\n", args[0]));
        Ok(args[0].clone())
    });

    // Stub out IO functions that don't work in browser
    vm.define_native("read_line", 0, |_args| {
        Err("read_line is not available in the playground".to_string())
    });

    vm.define_native("input", 1, |_args| {
        Err("input is not available in the playground".to_string())
    });

    vm.define_native("read_file", 1, |_args| {
        Err("read_file is not available in the playground".to_string())
    });

    vm.define_native("write_file", 2, |_args| {
        Err("write_file is not available in the playground".to_string())
    });

    vm.define_native("append_file", 2, |_args| {
        Err("append_file is not available in the playground".to_string())
    });

    vm.define_native("file_exists", 1, |_args| {
        Ok(Value::Bool(false))
    });

    vm.define_native("read_lines", 1, |_args| {
        Err("read_lines is not available in the playground".to_string())
    });

    vm.define_native("env_var", 1, |_args| {
        Err("env_var is not available in the playground".to_string())
    });

    vm.define_native("env_var_or", 2, |args| {
        // Return default value
        Ok(args[1].clone())
    });

    vm.define_native("args", 0, |_args| {
        Ok(Value::Array(Rc::new(RefCell::new(vec![]))))
    });

    // Basic assert functions
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

    vm.define_native("assert_eq", 2, |args| {
        if args[0] == args[1] {
            Ok(Value::Unit)
        } else {
            Err(format!("assertion failed: {:?} != {:?}", args[0], args[1]))
        }
    });

    vm.define_native("panic", 1, |args| {
        Err(format!("panic: {}", args[0]))
    });

    vm.define_native("unreachable", 0, |_args| {
        Err("unreachable code reached".to_string())
    });

    vm.define_native("todo", 0, |_args| {
        Err("not yet implemented".to_string())
    });

    vm.define_native("format", 2, |args| match &args[0] {
        Value::String(template) => {
            let result = template.replacen("{}", &format!("{}", args[1]), 1);
            Ok(Value::String(Rc::new(result)))
        }
        other => Err(format!("format: expected string template, got {}", other.type_name())),
    });
}

/// Initialize panic hook for better error messages
#[wasm_bindgen(start)]
pub fn init() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

/// Run Squam code and return the result as JSON
#[wasm_bindgen]
pub fn run(source: &str) -> JsValue {
    // Clear output buffer
    take_output();

    let result = run_internal(source);
    serde_wasm_bindgen::to_value(&result).unwrap_or(JsValue::NULL)
}

fn run_internal(source: &str) -> RunResult {
    // Parse
    let mut parser = Parser::new(source, 0);
    let module = parser.parse_module();

    if !parser.errors().is_empty() {
        let errors: Vec<String> = parser.errors().iter().map(|e| e.to_string()).collect();
        return RunResult {
            success: false,
            output: take_output(),
            result: None,
            error: Some(errors.join("\n")),
        };
    }

    // Compile
    let mut compiler = Compiler::new();
    let proto = match compiler.compile_module(&module) {
        Ok(p) => p,
        Err(e) => {
            return RunResult {
                success: false,
                output: take_output(),
                result: None,
                error: Some(e.to_string()),
            };
        }
    };

    // Create VM and register stdlib with output capture
    let mut vm = VM::new();
    register_stdlib_wasm(&mut vm);

    // Run
    let result = match vm.run(&proto) {
        Ok(val) => val,
        Err(e) => {
            return RunResult {
                success: false,
                output: take_output(),
                result: None,
                error: Some(e.to_string()),
            };
        }
    };

    // Check for main function
    if let Some(main_fn) = vm.globals.get("main").cloned() {
        match vm.call(main_fn, vec![]) {
            Ok(val) => {
                let result_str = if val == Value::Unit {
                    None
                } else {
                    Some(format!("{}", val))
                };
                RunResult {
                    success: true,
                    output: take_output(),
                    result: result_str,
                    error: None,
                }
            }
            Err(e) => RunResult {
                success: false,
                output: take_output(),
                result: None,
                error: Some(e.to_string()),
            },
        }
    } else {
        // No main function, return top-level result
        let result_str = if result == Value::Unit {
            None
        } else {
            Some(format!("{}", result))
        };
        RunResult {
            success: true,
            output: take_output(),
            result: result_str,
            error: None,
        }
    }
}