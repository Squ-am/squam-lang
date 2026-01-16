pub mod box_type;
pub mod crypto;
pub mod fs;
pub mod hashmap;
pub mod hashset;
#[cfg(feature = "http")]
pub mod http;
pub mod io;
pub mod iter;
pub mod json;
pub mod math;
pub mod net;
pub mod option;
pub mod random;
pub mod result;
pub mod string;
pub mod time;
pub mod vec;

use squam_vm::{Value, VM};
use std::rc::Rc;

/// Register all standard library functions with the VM.
pub fn register_stdlib(vm: &mut VM) {
    string::register(vm);
    vec::register(vm);
    math::register(vm);
    io::register(vm);
    iter::register(vm);
    option::register(vm);
    result::register(vm);
    hashmap::register(vm);
    hashset::register(vm);
    box_type::register(vm);
    net::register(vm);
    #[cfg(feature = "http")]
    http::register(vm);
    json::register(vm);
    time::register(vm);
    random::register(vm);
    crypto::register(vm);
    fs::register(vm);

    // Core utility functions
    register_core(vm);
}

/// Register core utility functions
fn register_core(vm: &mut VM) {
    // typeof(value) -> string
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

    // is_int(value) -> bool
    vm.define_native("is_int", 1, |args| {
        Ok(Value::Bool(matches!(&args[0], Value::Int(_))))
    });

    // is_float(value) -> bool
    vm.define_native("is_float", 1, |args| {
        Ok(Value::Bool(matches!(&args[0], Value::Float(_))))
    });

    // is_string(value) -> bool
    vm.define_native("is_string", 1, |args| {
        Ok(Value::Bool(matches!(&args[0], Value::String(_))))
    });

    // is_bool(value) -> bool
    vm.define_native("is_bool", 1, |args| {
        Ok(Value::Bool(matches!(&args[0], Value::Bool(_))))
    });

    // is_array(value) -> bool
    vm.define_native("is_array", 1, |args| {
        Ok(Value::Bool(matches!(&args[0], Value::Array(_))))
    });

    // is_function(value) -> bool
    vm.define_native("is_function", 1, |args| {
        Ok(Value::Bool(matches!(
            &args[0],
            Value::Closure(_) | Value::Native(_) | Value::VMNative(_)
        )))
    });

    // is_unit(value) -> bool
    vm.define_native("is_unit", 1, |args| {
        Ok(Value::Bool(matches!(&args[0], Value::Unit)))
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use squam_compiler::Compiler;
    use squam_parser::Parser;
    use squam_vm::Value;
    use std::rc::Rc;

    fn run_with_stdlib(source: &str) -> Result<Value, String> {
        let mut parser = Parser::new(source, 0);
        let module = parser.parse_module();
        if !parser.errors().is_empty() {
            return Err(format!("Parse errors: {:?}", parser.errors()));
        }

        let mut compiler = Compiler::new();
        let proto = compiler
            .compile_module(&module)
            .map_err(|e| format!("Compile error: {}", e))?;

        let mut vm = VM::new();
        register_stdlib(&mut vm);

        vm.run(&proto)
            .map_err(|e| format!("Runtime error: {}", e))?;

        // Try to call main if it exists
        if let Some(main) = vm.globals.get("main").cloned() {
            vm.call(main, vec![])
                .map_err(|e| format!("Runtime error: {}", e))
        } else {
            Ok(Value::Unit)
        }
    }

    #[test]
    fn test_stdlib_registered() {
        let mut vm = VM::new();
        register_stdlib(&mut vm);

        // Check that some functions are registered
        assert!(vm.globals.contains_key("abs"));
        assert!(vm.globals.contains_key("sqrt"));
        assert!(vm.globals.contains_key("str_len"));
    }

    #[test]
    fn test_math_abs() {
        let result = run_with_stdlib("fn main() -> int { abs(-42) }").unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_math_min_max() {
        let result = run_with_stdlib("fn main() -> int { min(5, 3) }").unwrap();
        assert_eq!(result, Value::Int(3));

        let result = run_with_stdlib("fn main() -> int { max(5, 3) }").unwrap();
        assert_eq!(result, Value::Int(5));
    }

    #[test]
    fn test_string_len() {
        let result = run_with_stdlib("fn main() -> int { str_len(\"hello\") }").unwrap();
        assert_eq!(result, Value::Int(5));
    }

    #[test]
    fn test_string_concat() {
        let result =
            run_with_stdlib("fn main() -> string { str_concat(\"hello\", \" world\") }").unwrap();
        assert_eq!(result, Value::String(Rc::new("hello world".to_string())));
    }

    #[test]
    fn test_array_len() {
        let result = run_with_stdlib("fn main() -> int { arr_len([1, 2, 3]) }").unwrap();
        assert_eq!(result, Value::Int(3));
    }

    #[test]
    fn test_option_some() {
        let result = run_with_stdlib("fn main() { Some(42) }").unwrap();
        match result {
            Value::Enum(e) => {
                assert_eq!(e.enum_name, "Option");
                assert_eq!(e.variant, "Some");
                assert_eq!(e.fields, vec![Value::Int(42)]);
            }
            _ => panic!("Expected Option::Some"),
        }
    }

    #[test]
    fn test_option_none() {
        let result = run_with_stdlib("fn main() { None() }").unwrap();
        match result {
            Value::Enum(e) => {
                assert_eq!(e.enum_name, "Option");
                assert_eq!(e.variant, "None");
                assert!(e.fields.is_empty());
            }
            _ => panic!("Expected Option::None"),
        }
    }

    #[test]
    fn test_option_is_some() {
        let result = run_with_stdlib("fn main() -> bool { is_some(Some(42)) }").unwrap();
        assert_eq!(result, Value::Bool(true));

        let result = run_with_stdlib("fn main() -> bool { is_some(None()) }").unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn test_option_unwrap() {
        let result = run_with_stdlib("fn main() -> int { unwrap(Some(42)) }").unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_option_unwrap_or() {
        let result = run_with_stdlib("fn main() -> int { unwrap_or(Some(42), 0) }").unwrap();
        assert_eq!(result, Value::Int(42));

        let result = run_with_stdlib("fn main() -> int { unwrap_or(None(), 99) }").unwrap();
        assert_eq!(result, Value::Int(99));
    }

    #[test]
    fn test_result_ok() {
        let result = run_with_stdlib("fn main() { Ok(42) }").unwrap();
        match result {
            Value::Enum(e) => {
                assert_eq!(e.enum_name, "Result");
                assert_eq!(e.variant, "Ok");
                assert_eq!(e.fields, vec![Value::Int(42)]);
            }
            _ => panic!("Expected Result::Ok"),
        }
    }

    #[test]
    fn test_result_err() {
        let result = run_with_stdlib(r#"fn main() { Err("error") }"#).unwrap();
        match result {
            Value::Enum(e) => {
                assert_eq!(e.enum_name, "Result");
                assert_eq!(e.variant, "Err");
            }
            _ => panic!("Expected Result::Err"),
        }
    }

    #[test]
    fn test_result_is_ok() {
        let result = run_with_stdlib("fn main() -> bool { is_ok(Ok(42)) }").unwrap();
        assert_eq!(result, Value::Bool(true));

        let result = run_with_stdlib(r#"fn main() -> bool { is_ok(Err("error")) }"#).unwrap();
        assert_eq!(result, Value::Bool(false));
    }

    #[test]
    fn test_result_unwrap_ok() {
        let result = run_with_stdlib("fn main() -> int { unwrap_ok(Ok(42)) }").unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_hashmap_basic() {
        let result = run_with_stdlib(
            r#"
            fn main() -> int {
                let m = hashmap_new();
                hashmap_insert(m, "key", 42);
                unwrap(hashmap_get(m, "key"))
            }
        "#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(42));
    }

    #[test]
    fn test_hashmap_contains() {
        let result = run_with_stdlib(
            r#"
            fn main() -> bool {
                let m = hashmap_new();
                hashmap_insert(m, "key", 42);
                hashmap_contains(m, "key")
            }
        "#,
        )
        .unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_hashmap_len() {
        let result = run_with_stdlib(
            r#"
            fn main() -> int {
                let m = hashmap_new();
                hashmap_insert(m, "a", 1);
                hashmap_insert(m, "b", 2);
                hashmap_insert(m, "c", 3);
                hashmap_len(m)
            }
        "#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(3));
    }

    #[test]
    fn test_hashset_basic() {
        let result = run_with_stdlib(
            r#"
            fn main() -> bool {
                let s = hashset_new();
                hashset_insert(s, 42);
                hashset_contains(s, 42)
            }
        "#,
        )
        .unwrap();
        assert_eq!(result, Value::Bool(true));
    }

    #[test]
    fn test_hashset_len() {
        let result = run_with_stdlib(
            r#"
            fn main() -> int {
                let s = hashset_new();
                hashset_insert(s, 1);
                hashset_insert(s, 2);
                hashset_insert(s, 2);
                hashset_len(s)
            }
        "#,
        )
        .unwrap();
        assert_eq!(result, Value::Int(2)); // Duplicates not counted
    }
}
