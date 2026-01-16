use squam::compiler::Compiler;
use squam::parser::Parser;
use squam::stdlib::register_stdlib;
use squam::vm::{Value, VM};

/// Helper to compile and run Squam code
fn run(source: &str) -> Result<Value, String> {
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
            .map_err(|e| format!("Runtime error in main: {}", e))
    } else {
        Ok(Value::Unit)
    }
}

#[test]
fn test_hello_world() {
    let result = run(r#"
        fn main() -> int {
            42
        }
    "#);
    assert_eq!(result.unwrap(), Value::Int(42));
}

#[test]
fn test_arithmetic() {
    let result = run(r#"
        fn main() -> int {
            1 + 2 * 3
        }
    "#);
    assert_eq!(result.unwrap(), Value::Int(7));
}

#[test]
fn test_local_variables() {
    let result = run(r#"
        fn main() -> int {
            let x = 10;
            let y = 20;
            x + y
        }
    "#);
    assert_eq!(result.unwrap(), Value::Int(30));
}

#[test]
fn test_nested_function_calls() {
    let result = run(r#"
        fn add(a: int, b: int) -> int {
            a + b
        }

        fn mul(a: int, b: int) -> int {
            a * b
        }

        fn main() -> int {
            mul(add(1, 2), add(3, 4))
        }
    "#);
    // (1+2) * (3+4) = 3 * 7 = 21
    assert_eq!(result.unwrap(), Value::Int(21));
}

#[test]
fn test_if_expression() {
    let result = run(r#"
        fn main() -> int {
            let x = 5;
            if x > 3 { 100 } else { 200 }
        }
    "#);
    assert_eq!(result.unwrap(), Value::Int(100));
}

#[test]
fn test_while_loop() {
    let result = run(r#"
        fn main() -> int {
            let mut sum = 0;
            let mut i = 1;
            while i <= 10 {
                sum = sum + i;
                i = i + 1;
            }
            sum
        }
    "#);
    // 1+2+3+4+5+6+7+8+9+10 = 55
    assert_eq!(result.unwrap(), Value::Int(55));
}

#[test]
fn test_array_operations() {
    let result = run(r#"
        fn main() -> int {
            let arr = [1, 2, 3, 4, 5];
            sum(arr)
        }
    "#);
    assert_eq!(result.unwrap(), Value::Int(15));
}

#[test]
fn test_string_operations() {
    let result = run(r#"
        fn main() -> int {
            let s = "hello";
            str_len(s)
        }
    "#);
    assert_eq!(result.unwrap(), Value::Int(5));
}

#[test]
fn test_recursion() {
    let result = run(r#"
        fn factorial(n: int) -> int {
            if n <= 1 { 1 } else { n * factorial(n - 1) }
        }

        fn main() -> int {
            factorial(5)
        }
    "#);
    assert_eq!(result.unwrap(), Value::Int(120));
}

#[test]
fn test_fibonacci() {
    let result = run(r#"
        fn fib(n: int) -> int {
            if n <= 1 { n } else { fib(n - 1) + fib(n - 2) }
        }

        fn main() -> int {
            fib(10)
        }
    "#);
    assert_eq!(result.unwrap(), Value::Int(55));
}

#[test]
fn test_comparison_operators() {
    // Test less than
    let result = run("fn main() -> bool { 1 < 2 }");
    assert_eq!(result.unwrap(), Value::Bool(true));

    let result = run("fn main() -> bool { 2 < 1 }");
    assert_eq!(result.unwrap(), Value::Bool(false));

    // Test greater than
    let result = run("fn main() -> bool { 5 > 3 }");
    assert_eq!(result.unwrap(), Value::Bool(true));

    // Test equality
    let result = run("fn main() -> bool { 42 == 42 }");
    assert_eq!(result.unwrap(), Value::Bool(true));

    let result = run("fn main() -> bool { 42 != 43 }");
    assert_eq!(result.unwrap(), Value::Bool(true));
}

#[test]
fn test_boolean_logic() {
    let result = run("fn main() -> bool { true && true }");
    assert_eq!(result.unwrap(), Value::Bool(true));

    let result = run("fn main() -> bool { true && false }");
    assert_eq!(result.unwrap(), Value::Bool(false));

    let result = run("fn main() -> bool { false || true }");
    assert_eq!(result.unwrap(), Value::Bool(true));

    let result = run("fn main() -> bool { !false }");
    assert_eq!(result.unwrap(), Value::Bool(true));
}

#[test]
fn test_math_functions() {
    let result = run("fn main() -> int { abs(-42) }");
    assert_eq!(result.unwrap(), Value::Int(42));

    let result = run("fn main() -> int { min(3, 7) }");
    assert_eq!(result.unwrap(), Value::Int(3));

    let result = run("fn main() -> int { max(3, 7) }");
    assert_eq!(result.unwrap(), Value::Int(7));
}
