#[macro_export]
macro_rules! register_fn {
    // 0 arguments
    ($engine:expr, $name:expr, || $body:expr) => {
        $engine.register_fn_0($name, || $body)
    };
    // 1 argument
    ($engine:expr, $name:expr, |$a:ident : $ta:ty| $body:expr) => {
        $engine.register_fn_1($name, |$a: $ta| $body)
    };
    // 2 arguments
    ($engine:expr, $name:expr, |$a:ident : $ta:ty, $b:ident : $tb:ty| $body:expr) => {
        $engine.register_fn_2($name, |$a: $ta, $b: $tb| $body)
    };
    // 3 arguments
    ($engine:expr, $name:expr, |$a:ident : $ta:ty, $b:ident : $tb:ty, $c:ident : $tc:ty| $body:expr) => {
        $engine.register_fn_3($name, |$a: $ta, $b: $tb, $c: $tc| $body)
    };
    // 4 arguments
    ($engine:expr, $name:expr, |$a:ident : $ta:ty, $b:ident : $tb:ty, $c:ident : $tc:ty, $d:ident : $td:ty| $body:expr) => {
        $engine.register_fn_4($name, |$a: $ta, $b: $tb, $c: $tc, $d: $td| $body)
    };
}

#[cfg(test)]
mod tests {
    use crate::Engine;

    #[test]
    fn test_register_fn_0() {
        let mut engine = Engine::new();
        register_fn!(engine, "get_42", || 42i64);

        let result: i64 = engine.eval("get_42()").unwrap();
        assert_eq!(result, 42);
    }

    #[test]
    fn test_register_fn_1() {
        let mut engine = Engine::new();
        register_fn!(engine, "double", |x: i64| x * 2);

        let result: i64 = engine.eval("double(21)").unwrap();
        assert_eq!(result, 42);
    }

    #[test]
    fn test_register_fn_2() {
        let mut engine = Engine::new();
        register_fn!(engine, "add", |a: i64, b: i64| a + b);

        let result: i64 = engine.eval("add(20, 22)").unwrap();
        assert_eq!(result, 42);
    }

    #[test]
    fn test_register_fn_string() {
        let mut engine = Engine::new();
        register_fn!(engine, "greet", |name: String| format!("Hello, {}!", name));

        let result: String = engine.eval("greet(\"Rust\")").unwrap();
        assert_eq!(result, "Hello, Rust!");
    }
}
