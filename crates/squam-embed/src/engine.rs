use squam_compiler::Compiler;
use squam_parser::Parser;
use squam_stdlib::register_stdlib;
use squam_vm::{Value, VM};

use crate::convert::{FromSquam, IntoSquam};
use crate::error::{EmbedError, EmbedResult};

/// The Squam scripting engine.
///
/// This is the main entry point for embedding Squam in a Rust application.
///
/// # Example
///
/// ```
/// use squam_embed::Engine;
///
/// let mut engine = Engine::new();
///
/// // Evaluate expressions
/// let result: i64 = engine.eval("1 + 2 * 3").unwrap();
/// assert_eq!(result, 7);
///
/// // Run scripts with function definitions
/// engine.run(r#"
///     fn square(x: int) -> int {
///         x * x
///     }
/// "#).unwrap();
///
/// // Call defined functions
/// let result: i64 = engine.call("square", (5,)).unwrap();
/// assert_eq!(result, 25);
/// ```
pub struct Engine {
    vm: VM,
}

impl Engine {
    /// Create a new Squam engine with the standard library loaded.
    pub fn new() -> Self {
        let mut vm = VM::new();
        register_stdlib(&mut vm);
        Self { vm }
    }

    /// Create a new Squam engine without the standard library.
    pub fn new_bare() -> Self {
        Self { vm: VM::new() }
    }

    /// Evaluate a Squam expression and return the result.
    ///
    /// The expression is wrapped in a function and called, so the result
    /// of the expression is returned directly.
    ///
    /// # Example
    ///
    /// ```
    /// use squam_embed::Engine;
    ///
    /// let mut engine = Engine::new();
    /// let result: i64 = engine.eval("1 + 2 + 3").unwrap();
    /// assert_eq!(result, 6);
    /// ```
    pub fn eval<T: FromSquam>(&mut self, expr: &str) -> EmbedResult<T> {
        // Wrap expression in a function to get the result
        let source = format!("fn __eval__() {{ {} }}", expr);
        self.compile_and_run(&source)?;

        // Call __eval__ and get the result
        let eval_fn = self
            .vm
            .globals
            .remove("__eval__")
            .ok_or_else(|| EmbedError::FunctionNotFound("__eval__".to_string()))?;

        let result = self.vm.call(eval_fn, vec![])?;
        T::from_squam(result)
    }

    /// Run a Squam script.
    ///
    /// This compiles and executes the script. Any functions or variables
    /// defined in the script become available for later use.
    ///
    /// # Example
    ///
    /// ```
    /// use squam_embed::Engine;
    ///
    /// let mut engine = Engine::new();
    /// engine.run(r#"
    ///     fn add(a: int, b: int) -> int {
    ///         a + b
    ///     }
    /// "#).unwrap();
    /// ```
    pub fn run(&mut self, source: &str) -> EmbedResult<()> {
        self.compile_and_run(source)?;
        Ok(())
    }

    /// Run a Squam script and call the `main` function if it exists.
    ///
    /// Returns the result of calling `main`, or `()` if no main function exists.
    pub fn run_main<T: FromSquam>(&mut self, source: &str) -> EmbedResult<T> {
        self.compile_and_run(source)?;

        if let Some(main_fn) = self.vm.globals.get("main").cloned() {
            let result = self.vm.call(main_fn, vec![])?;
            T::from_squam(result)
        } else {
            T::from_squam(Value::Unit)
        }
    }

    /// Call a function defined in Squam with the given arguments.
    ///
    /// # Example
    ///
    /// ```
    /// use squam_embed::Engine;
    ///
    /// let mut engine = Engine::new();
    /// engine.run("fn multiply(a: int, b: int) -> int { a * b }").unwrap();
    ///
    /// let result: i64 = engine.call("multiply", (3, 4)).unwrap();
    /// assert_eq!(result, 12);
    /// ```
    pub fn call<A: IntoArgs, T: FromSquam>(&mut self, name: &str, args: A) -> EmbedResult<T> {
        let func = self
            .vm
            .globals
            .get(name)
            .cloned()
            .ok_or_else(|| EmbedError::FunctionNotFound(name.to_string()))?;

        let args = args.into_args();
        let result = self.vm.call(func, args)?;
        T::from_squam(result)
    }

    /// Get a global variable's value.
    ///
    /// # Example
    ///
    /// ```
    /// use squam_embed::Engine;
    ///
    /// let mut engine = Engine::new();
    /// engine.set("PI", 3.14159f64);
    ///
    /// let pi: f64 = engine.get("PI").unwrap();
    /// assert!((pi - 3.14159).abs() < 0.0001);
    /// ```
    pub fn get<T: FromSquam>(&self, name: &str) -> EmbedResult<T> {
        let value = self
            .vm
            .globals
            .get(name)
            .cloned()
            .ok_or_else(|| EmbedError::VariableNotFound(name.to_string()))?;
        T::from_squam(value)
    }

    /// Set a global variable's value.
    ///
    /// # Example
    ///
    /// ```
    /// use squam_embed::Engine;
    ///
    /// let mut engine = Engine::new();
    /// engine.set("x", 42i64);
    ///
    /// let result: i64 = engine.eval("x * 2").unwrap();
    /// assert_eq!(result, 84);
    /// ```
    pub fn set<T: IntoSquam>(&mut self, name: &str, value: T) {
        self.vm.globals.insert(name.to_string(), value.into_squam());
    }

    /// Register a native Rust function that takes no arguments.
    ///
    /// # Example
    ///
    /// ```
    /// use squam_embed::Engine;
    ///
    /// let mut engine = Engine::new();
    /// engine.register_fn_0("get_answer", || 42i64);
    ///
    /// let result: i64 = engine.eval("get_answer()").unwrap();
    /// assert_eq!(result, 42);
    /// ```
    pub fn register_fn_0<R, F>(&mut self, name: &str, func: F)
    where
        R: IntoSquam + 'static,
        F: Fn() -> R + 'static,
    {
        self.vm.define_native_closure(name, 0, move |_args| {
            let result = func();
            Ok(result.into_squam())
        });
    }

    /// Register a native Rust function that takes one argument.
    pub fn register_fn_1<A, R, F>(&mut self, name: &str, func: F)
    where
        A: FromSquam + 'static,
        R: IntoSquam + 'static,
        F: Fn(A) -> R + 'static,
    {
        self.vm.define_native_closure(name, 1, move |args| {
            let a = A::from_squam(args[0].clone()).map_err(|e| e.to_string())?;
            let result = func(a);
            Ok(result.into_squam())
        });
    }

    /// Register a native Rust function that takes two arguments.
    pub fn register_fn_2<A, B, R, F>(&mut self, name: &str, func: F)
    where
        A: FromSquam + 'static,
        B: FromSquam + 'static,
        R: IntoSquam + 'static,
        F: Fn(A, B) -> R + 'static,
    {
        self.vm.define_native_closure(name, 2, move |args| {
            let a = A::from_squam(args[0].clone()).map_err(|e| e.to_string())?;
            let b = B::from_squam(args[1].clone()).map_err(|e| e.to_string())?;
            let result = func(a, b);
            Ok(result.into_squam())
        });
    }

    /// Register a native Rust function that takes three arguments.
    pub fn register_fn_3<A, B, C, R, F>(&mut self, name: &str, func: F)
    where
        A: FromSquam + 'static,
        B: FromSquam + 'static,
        C: FromSquam + 'static,
        R: IntoSquam + 'static,
        F: Fn(A, B, C) -> R + 'static,
    {
        self.vm.define_native_closure(name, 3, move |args| {
            let a = A::from_squam(args[0].clone()).map_err(|e| e.to_string())?;
            let b = B::from_squam(args[1].clone()).map_err(|e| e.to_string())?;
            let c = C::from_squam(args[2].clone()).map_err(|e| e.to_string())?;
            let result = func(a, b, c);
            Ok(result.into_squam())
        });
    }

    /// Register a native Rust function that takes four arguments.
    pub fn register_fn_4<A, B, C, D, R, F>(&mut self, name: &str, func: F)
    where
        A: FromSquam + 'static,
        B: FromSquam + 'static,
        C: FromSquam + 'static,
        D: FromSquam + 'static,
        R: IntoSquam + 'static,
        F: Fn(A, B, C, D) -> R + 'static,
    {
        self.vm.define_native_closure(name, 4, move |args| {
            let a = A::from_squam(args[0].clone()).map_err(|e| e.to_string())?;
            let b = B::from_squam(args[1].clone()).map_err(|e| e.to_string())?;
            let c = C::from_squam(args[2].clone()).map_err(|e| e.to_string())?;
            let d = D::from_squam(args[3].clone()).map_err(|e| e.to_string())?;
            let result = func(a, b, c, d);
            Ok(result.into_squam())
        });
    }

    /// Compile and run source code.
    fn compile_and_run(&mut self, source: &str) -> EmbedResult<Value> {
        // Parse
        let mut parser = Parser::new(source, 0);
        let module = parser.parse_module();

        if !parser.errors().is_empty() {
            return Err(EmbedError::Parse(
                parser
                    .errors()
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<_>>()
                    .join("; "),
            ));
        }

        // Compile
        let mut compiler = Compiler::new();
        let proto = compiler.compile_module(&module)?;

        // Run
        let result = self.vm.run(&proto)?;
        Ok(result)
    }
}

impl Default for Engine {
    fn default() -> Self {
        Self::new()
    }
}

/// Trait for converting tuples into argument vectors.
pub trait IntoArgs {
    fn into_args(self) -> Vec<Value>;
}

impl IntoArgs for () {
    fn into_args(self) -> Vec<Value> {
        vec![]
    }
}

impl<A: IntoSquam> IntoArgs for (A,) {
    fn into_args(self) -> Vec<Value> {
        vec![self.0.into_squam()]
    }
}

impl<A: IntoSquam, B: IntoSquam> IntoArgs for (A, B) {
    fn into_args(self) -> Vec<Value> {
        vec![self.0.into_squam(), self.1.into_squam()]
    }
}

impl<A: IntoSquam, B: IntoSquam, C: IntoSquam> IntoArgs for (A, B, C) {
    fn into_args(self) -> Vec<Value> {
        vec![
            self.0.into_squam(),
            self.1.into_squam(),
            self.2.into_squam(),
        ]
    }
}

impl<A: IntoSquam, B: IntoSquam, C: IntoSquam, D: IntoSquam> IntoArgs for (A, B, C, D) {
    fn into_args(self) -> Vec<Value> {
        vec![
            self.0.into_squam(),
            self.1.into_squam(),
            self.2.into_squam(),
            self.3.into_squam(),
        ]
    }
}

impl IntoArgs for Vec<Value> {
    fn into_args(self) -> Vec<Value> {
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_eval_arithmetic() {
        let mut engine = Engine::new();
        let result: i64 = engine.eval("1 + 2 * 3").unwrap();
        assert_eq!(result, 7);
    }

    #[test]
    fn test_eval_string() {
        let mut engine = Engine::new();
        let result: String = engine.eval("\"hello\"").unwrap();
        assert_eq!(result, "hello");
    }

    #[test]
    fn test_run_and_call() {
        let mut engine = Engine::new();
        engine
            .run("fn add(a: int, b: int) -> int { a + b }")
            .unwrap();

        let result: i64 = engine.call("add", (10i64, 20i64)).unwrap();
        assert_eq!(result, 30);
    }

    #[test]
    fn test_set_and_get() {
        let mut engine = Engine::new();
        engine.set("x", 42i64);

        let x: i64 = engine.get("x").unwrap();
        assert_eq!(x, 42);

        let result: i64 = engine.eval("x * 2").unwrap();
        assert_eq!(result, 84);
    }

    #[test]
    fn test_register_native_fn() {
        let mut engine = Engine::new();
        engine.register_fn_2("add", |a: i64, b: i64| a + b);

        let result: i64 = engine.eval("add(3, 4)").unwrap();
        assert_eq!(result, 7);
    }

    #[test]
    fn test_register_native_fn_with_string() {
        let mut engine = Engine::new();
        engine.register_fn_1("greet", |name: String| format!("Hello, {}!", name));

        let result: String = engine.eval("greet(\"World\")").unwrap();
        assert_eq!(result, "Hello, World!");
    }

    #[test]
    fn test_run_main() {
        let mut engine = Engine::new();
        let result: i64 = engine.run_main("fn main() -> int { 42 }").unwrap();
        assert_eq!(result, 42);
    }

    #[test]
    fn test_stdlib_available() {
        let mut engine = Engine::new();
        let result: i64 = engine.eval("abs(-42)").unwrap();
        assert_eq!(result, 42);
    }

    #[test]
    fn test_array_operations() {
        let mut engine = Engine::new();
        engine.set("nums", vec![1i64, 2, 3, 4, 5]);

        let result: i64 = engine.eval("sum(nums)").unwrap();
        assert_eq!(result, 15);
    }

    #[test]
    fn test_closure_capture() {
        let mut engine = Engine::new();

        // Use a captured value in the registered function
        let multiplier = 10i64;
        engine.register_fn_1("scale", move |x: i64| x * multiplier);

        let result: i64 = engine.eval("scale(5)").unwrap();
        assert_eq!(result, 50);
    }
}
