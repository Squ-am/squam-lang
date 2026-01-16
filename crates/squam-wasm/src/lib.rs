use std::cell::RefCell;
use std::rc::Rc;

use serde::Serialize;
use squam_compiler::Compiler;
use squam_parser::Parser;
use squam_vm::{Value, VM};
use wasm_bindgen::prelude::*;

// Thread-local output buffer for capturing print statements
thread_local! {
    static OUTPUT: RefCell<String> = const { RefCell::new(String::new()) };
}

fn append_output(s: &str) {
    OUTPUT.with(|output| {
        output.borrow_mut().push_str(s);
    });
}

fn take_output() -> String {
    OUTPUT.with(|output| std::mem::take(&mut *output.borrow_mut()))
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
    squam_stdlib::box_type::register(vm);
    squam_stdlib::json::register(vm);
    squam_stdlib::crypto::register(vm);

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

    vm.define_native("file_exists", 1, |_args| Ok(Value::Bool(false)));

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

    vm.define_native("panic", 1, |args| Err(format!("panic: {}", args[0])));

    vm.define_native("unreachable", 0, |_args| {
        Err("unreachable code reached".to_string())
    });

    vm.define_native("todo", 0, |_args| Err("not yet implemented".to_string()));

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

    // HTTP stubs (not available in browser without fetch API)
    vm.define_native("http_get", 1, |_args| {
        Err("http_get is not available in the playground".to_string())
    });
    vm.define_native("http_get_text", 1, |_args| {
        Err("http_get_text is not available in the playground".to_string())
    });
    vm.define_native("http_post", 2, |_args| {
        Err("http_post is not available in the playground".to_string())
    });
    vm.define_native("http_post_json", 2, |_args| {
        Err("http_post_json is not available in the playground".to_string())
    });
    vm.define_native("http_put", 2, |_args| {
        Err("http_put is not available in the playground".to_string())
    });
    vm.define_native("http_delete", 1, |_args| {
        Err("http_delete is not available in the playground".to_string())
    });

    // TCP/Net stubs
    vm.define_native("tcp_connect", 2, |_args| {
        Err("tcp_connect is not available in the playground".to_string())
    });
    vm.define_native("tcp_send", 2, |_args| {
        Err("tcp_send is not available in the playground".to_string())
    });
    vm.define_native("tcp_receive", 1, |_args| {
        Err("tcp_receive is not available in the playground".to_string())
    });
    vm.define_native("tcp_close", 1, |_args| {
        Err("tcp_close is not available in the playground".to_string())
    });

    // Filesystem stubs
    vm.define_native("mkdir", 1, |_args| {
        Err("mkdir is not available in the playground".to_string())
    });
    vm.define_native("mkdir_all", 1, |_args| {
        Err("mkdir_all is not available in the playground".to_string())
    });
    vm.define_native("rmdir", 1, |_args| {
        Err("rmdir is not available in the playground".to_string())
    });
    vm.define_native("rmdir_all", 1, |_args| {
        Err("rmdir_all is not available in the playground".to_string())
    });
    vm.define_native("remove_file", 1, |_args| {
        Err("remove_file is not available in the playground".to_string())
    });
    vm.define_native("rename", 2, |_args| {
        Err("rename is not available in the playground".to_string())
    });
    vm.define_native("copy_file", 2, |_args| {
        Err("copy_file is not available in the playground".to_string())
    });
    vm.define_native("read_dir", 1, |_args| {
        Err("read_dir is not available in the playground".to_string())
    });
    vm.define_native("is_file", 1, |_args| Ok(Value::Bool(false)));
    vm.define_native("is_dir", 1, |_args| Ok(Value::Bool(false)));
    vm.define_native("file_size", 1, |_args| Ok(Value::Int(-1)));
    vm.define_native("path_join", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let parts: Vec<String> = arr
                .borrow()
                .iter()
                .filter_map(|v| {
                    if let Value::String(s) = v {
                        Some(s.to_string())
                    } else {
                        None
                    }
                })
                .collect();
            Ok(Value::String(Rc::new(parts.join("/"))))
        }
        _ => Err("path_join: expected array".to_string()),
    });
    vm.define_native("path_parent", 1, |args| match &args[0] {
        Value::String(s) => {
            let path = s.as_str();
            if let Some(idx) = path.rfind('/') {
                Ok(Value::String(Rc::new(path[..idx].to_string())))
            } else {
                Ok(Value::String(Rc::new(String::new())))
            }
        }
        _ => Err("path_parent: expected string".to_string()),
    });
    vm.define_native("path_filename", 1, |args| match &args[0] {
        Value::String(s) => {
            let path = s.as_str();
            if let Some(idx) = path.rfind('/') {
                Ok(Value::String(Rc::new(path[idx + 1..].to_string())))
            } else {
                Ok(Value::String(Rc::new(path.to_string())))
            }
        }
        _ => Err("path_filename: expected string".to_string()),
    });
    vm.define_native("path_extension", 1, |args| match &args[0] {
        Value::String(s) => {
            let path = s.as_str();
            if let Some(idx) = path.rfind('.') {
                Ok(Value::String(Rc::new(path[idx + 1..].to_string())))
            } else {
                Ok(Value::String(Rc::new(String::new())))
            }
        }
        _ => Err("path_extension: expected string".to_string()),
    });
    vm.define_native("cwd", 0, |_args| {
        Ok(Value::String(Rc::new("/".to_string())))
    });
    vm.define_native("chdir", 1, |_args| {
        Err("chdir is not available in the playground".to_string())
    });

    // Time functions using browser APIs
    vm.define_native("time_now", 0, |_args| {
        let ms = js_sys::Date::now();
        Ok(Value::Int((ms / 1000.0) as i64))
    });
    vm.define_native("time_now_ms", 0, |_args| {
        let ms = js_sys::Date::now();
        Ok(Value::Int(ms as i64))
    });
    vm.define_native("time_now_ns", 0, |_args| {
        let ms = js_sys::Date::now();
        Ok(Value::Int((ms * 1_000_000.0) as i64))
    });
    vm.define_native("sleep", 1, |_args| {
        Err("sleep is not available in the playground (would block)".to_string())
    });
    vm.define_native("sleep_secs", 1, |_args| {
        Err("sleep_secs is not available in the playground (would block)".to_string())
    });
    vm.define_native("timer_start", 0, |_args| {
        Err("timer functions are not available in the playground".to_string())
    });
    vm.define_native("timer_elapsed_ms", 0, |_args| {
        Err("timer functions are not available in the playground".to_string())
    });
    vm.define_native("timer_elapsed_us", 0, |_args| {
        Err("timer functions are not available in the playground".to_string())
    });
    vm.define_native("timer_elapsed_secs", 0, |_args| {
        Err("timer functions are not available in the playground".to_string())
    });
    vm.define_native("format_timestamp", 2, |args| match (&args[0], &args[1]) {
        (Value::Int(ts), Value::String(fmt)) => {
            let date = js_sys::Date::new(&JsValue::from_f64((*ts as f64) * 1000.0));
            let result = fmt
                .replace("%Y", &format!("{:04}", date.get_full_year()))
                .replace("%m", &format!("{:02}", date.get_month() + 1))
                .replace("%d", &format!("{:02}", date.get_date()))
                .replace("%H", &format!("{:02}", date.get_hours()))
                .replace("%M", &format!("{:02}", date.get_minutes()))
                .replace("%S", &format!("{:02}", date.get_seconds()));
            Ok(Value::String(Rc::new(result)))
        }
        _ => Err("format_timestamp: expected (int, string)".to_string()),
    });

    // Random functions using browser Math.random()
    vm.define_native("random", 0, |_args| {
        Ok(Value::Float(js_sys::Math::random()))
    });
    vm.define_native("random_int", 2, |args| match (&args[0], &args[1]) {
        (Value::Int(min), Value::Int(max)) => {
            if max < min {
                return Err("random_int: max must be >= min".to_string());
            }
            let range = (max - min + 1) as f64;
            let r = js_sys::Math::random() * range;
            Ok(Value::Int(min + r as i64))
        }
        _ => Err("random_int: expected (int, int)".to_string()),
    });
    vm.define_native("random_float", 2, |args| {
        let min = match &args[0] {
            Value::Float(f) => *f,
            Value::Int(i) => *i as f64,
            _ => return Err("random_float: expected (number, number)".to_string()),
        };
        let max = match &args[1] {
            Value::Float(f) => *f,
            Value::Int(i) => *i as f64,
            _ => return Err("random_float: expected (number, number)".to_string()),
        };
        if max < min {
            return Err("random_float: max must be >= min".to_string());
        }
        let r = js_sys::Math::random() * (max - min) + min;
        Ok(Value::Float(r))
    });
    vm.define_native("random_bool", 0, |_args| {
        Ok(Value::Bool(js_sys::Math::random() >= 0.5))
    });
    vm.define_native("random_choice", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            if arr.is_empty() {
                return Err("random_choice: array is empty".to_string());
            }
            let idx = (js_sys::Math::random() * arr.len() as f64) as usize;
            Ok(arr[idx.min(arr.len() - 1)].clone())
        }
        _ => Err("random_choice: expected array".to_string()),
    });
    vm.define_native("shuffle", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let mut new_arr: Vec<Value> = arr.borrow().clone();
            for i in (1..new_arr.len()).rev() {
                let j = (js_sys::Math::random() * (i + 1) as f64) as usize;
                new_arr.swap(i, j);
            }
            Ok(Value::Array(Rc::new(RefCell::new(new_arr))))
        }
        _ => Err("shuffle: expected array".to_string()),
    });
    vm.define_native("random_string", 1, |args| match &args[0] {
        Value::Int(len) => {
            if *len < 0 {
                return Err("random_string: length must be >= 0".to_string());
            }
            let chars: String = (0..*len as usize)
                .map(|_| {
                    let idx = (js_sys::Math::random() * 62.0) as u8;
                    match idx {
                        0..=25 => (b'a' + idx) as char,
                        26..=51 => (b'A' + (idx - 26)) as char,
                        _ => (b'0' + (idx - 52)) as char,
                    }
                })
                .collect();
            Ok(Value::String(Rc::new(chars)))
        }
        _ => Err("random_string: expected int".to_string()),
    });
    vm.define_native("uuid", 0, |_args| {
        let bytes: Vec<u8> = (0..16).map(|_| (js_sys::Math::random() * 256.0) as u8).collect();
        let uuid = format!(
            "{:02x}{:02x}{:02x}{:02x}-{:02x}{:02x}-4{:01x}{:02x}-{:01x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
            bytes[0], bytes[1], bytes[2], bytes[3],
            bytes[4], bytes[5],
            bytes[6] & 0x0f, bytes[7],
            (bytes[8] & 0x3f) | 0x80, bytes[9],
            bytes[10], bytes[11], bytes[12], bytes[13], bytes[14], bytes[15]
        );
        Ok(Value::String(Rc::new(uuid)))
    });
    vm.define_native("sample", 2, |args| match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Int(n)) => {
            let arr = arr.borrow();
            let n = *n as usize;
            if n > arr.len() {
                return Err("sample: n must be <= array length".to_string());
            }
            let mut indices: Vec<usize> = (0..arr.len()).collect();
            for i in 0..n {
                let j = i + (js_sys::Math::random() * (indices.len() - i) as f64) as usize;
                indices.swap(i, j);
            }
            let result: Vec<Value> = indices[0..n].iter().map(|&i| arr[i].clone()).collect();
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        _ => Err("sample: expected (array, int)".to_string()),
    });

    // typeof and is_* functions
    vm.define_native("typeof", 1, |args| {
        let type_name = match &args[0] {
            Value::Unit => "unit",
            Value::Bool(_) => "bool",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Array(_) => "array",
            Value::Tuple(_) => "tuple",
            Value::Struct(s) => return Ok(Value::String(Rc::new(format!("struct:{}", s.name)))),
            Value::Enum(e) => {
                return Ok(Value::String(Rc::new(format!(
                    "enum:{}::{}",
                    e.enum_name, e.variant
                ))))
            }
            Value::Closure(_) => "function",
            Value::Native(_) => "function",
            Value::VMNative(_) => "function",
            Value::Range(..) => "range",
            Value::Iterator(_) => "iterator",
            Value::LocalRef(..) => "ref",
            Value::Box(_) => "box",
        };
        Ok(Value::String(Rc::new(type_name.to_string())))
    });
    vm.define_native("is_int", 1, |args| {
        Ok(Value::Bool(matches!(&args[0], Value::Int(_))))
    });
    vm.define_native("is_float", 1, |args| {
        Ok(Value::Bool(matches!(&args[0], Value::Float(_))))
    });
    vm.define_native("is_string", 1, |args| {
        Ok(Value::Bool(matches!(&args[0], Value::String(_))))
    });
    vm.define_native("is_bool", 1, |args| {
        Ok(Value::Bool(matches!(&args[0], Value::Bool(_))))
    });
    vm.define_native("is_array", 1, |args| {
        Ok(Value::Bool(matches!(&args[0], Value::Array(_))))
    });
    vm.define_native("is_function", 1, |args| {
        Ok(Value::Bool(matches!(
            &args[0],
            Value::Closure(_) | Value::Native(_) | Value::VMNative(_)
        )))
    });
    vm.define_native("is_unit", 1, |args| {
        Ok(Value::Bool(matches!(&args[0], Value::Unit)))
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
