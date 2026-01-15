# Squam

**Type-safe scripting that stays out of your way.**

[Website](https://squ.am) · [Documentation](https://squ.am/docs) · [Playground](https://squ.am/playground) · [Discord](https://discord.gg/EHcN7PzHBZ)

---

Squam is a statically-typed scripting language with Rust-like syntax, full type inference, pattern matching, and error messages that actually help. It brings the safety of modern type systems to the speed of scripting. Write expressive, type-safe code with no ceremony required.

```squam
fn main() {
    let numbers = [1, 2, 3, 4, 5];
    let doubled = arr_map(numbers, |x| x * 2);
    println(doubled);  // [2, 4, 6, 8, 10]
}
```

## Why Squam?

Most scripting languages trade safety for simplicity. Most safe languages trade simplicity for complexity. Squam sits in the middle, **Rust's "safety first" DNA in a lightweight scripting package**.

- **Want Lua's simplicity?** Squam is just as easy to pick up
- **Want Rust's safety?** Squam catches bugs at compile time
- **Want both?** That's why Squam exists

### Key Features

| Feature | Description |
|---------|-------------|
| **Type Inference** | Full static typing without writing type annotations everywhere |
| **Algebraic Types** | Enums with data, Option, Result, and exhaustive pattern matching |
| **Traits & Generics** | Write reusable, type-safe abstractions |
| **Memory Safe** | Automatic garbage collection—no segfaults, no manual memory management |
| **Rust Embedding** | Embed Squam in Rust applications for scripting, configs, or plugins |
| **Helpful Errors** | Clear diagnostics that point to problems and suggest fixes |

## Installation

### macOS & Linux

```bash
curl -fsSL https://squ.am/install.sh | sh
```

### From Source

```bash
git clone https://github.com/squ-am/squam-lang
cd squam-lang
cargo install --path crates/squam
```

### Verify Installation

```bash
squam --version
```

## Quick Start

### Hello World

Create `hello.squ`:

```squam
fn main() {
    println("Hello, Squam!");
}
```

Run it:

```bash
squam run hello.squ
```

### Interactive REPL

```bash
squam repl
```

```
>>> let x = 42
>>> x * 2
84
>>> fn greet(name) { println("Hello, " + name) }
>>> greet("World")
Hello, World
```

## Language Overview

### Variables & Types

```squam
let x = 42;              // Immutable, type inferred as i64
let mut y = 10;          // Mutable
let name: String = "Jo"; // Explicit type annotation

y = y + 1;               // OK - y is mutable
```

### Functions & Closures

```squam
fn add(a: i64, b: i64) -> i64 {
    a + b  // Implicit return
}

let double = |x| x * 2;  // Closure
println(double(21));     // 42
```

### Structs & Methods

```squam
struct Point {
    x: f64,
    y: f64,
}

impl Point {
    fn new(x: f64, y: f64) -> Point {
        Point { x, y }
    }

    fn distance(&self, other: Point) -> f64 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        sqrt(dx * dx + dy * dy)
    }
}

let p1 = Point::new(0.0, 0.0);
let p2 = Point::new(3.0, 4.0);
println(p1.distance(p2));  // 5.0
```

### Enums & Pattern Matching

```squam
enum Shape {
    Circle(f64),
    Rectangle(f64, f64),
}

fn area(shape: Shape) -> f64 {
    match shape {
        Shape::Circle(r) => 3.14159 * r * r,
        Shape::Rectangle(w, h) => w * h,
    }
}
```

### Traits

```squam
trait Printable {
    fn to_string(&self) -> String;
}

impl Printable for Point {
    fn to_string(&self) -> String {
        "(" + str(self.x) + ", " + str(self.y) + ")"
    }
}
```

### Error Handling

```squam
fn divide(a: f64, b: f64) -> Option<f64> {
    if b == 0.0 {
        None
    } else {
        Some(a / b)
    }
}

match divide(10.0, 2.0) {
    Some(result) => println(result),
    None => println("Cannot divide by zero"),
}
```

## Embedding in Rust

Squam can be embedded in Rust applications—perfect for game scripting, configuration, plugins, or anywhere you need safe, sandboxed code execution.

```toml
[dependencies]
squam-embed = "0.1"
```

```rust
use squam_embed::Engine;

fn main() {
    let mut engine = Engine::new();

    // Evaluate expressions
    let result = engine.eval("1 + 2 * 3").unwrap();
    println!("Result: {}", result);  // 7

    // Run programs
    engine.run(r#"
        fn factorial(n: i64) -> i64 {
            if n <= 1 { 1 } else { n * factorial(n - 1) }
        }
        println(factorial(10));
    "#).unwrap();
}
```

### Register Rust Functions

```rust
use squam_embed::{Engine, register_fn};

fn greet(name: String) -> String {
    format!("Hello, {}!", name)
}

fn main() {
    let mut engine = Engine::new();
    register_fn!(engine, "greet", greet);

    engine.run(r#"println(greet("World"))"#).unwrap();
    // Prints: Hello, World!
}
```

## CLI Commands

```bash
squam run <file.squ>     # Run a Squam program
squam check <file.squ>   # Type-check without running
squam repl               # Interactive REPL
squam disasm <file.squ>  # Show compiled bytecode
```

## Error Messages

Squam provides clear, actionable error messages:

```
error: type mismatch
  --> script.squ:5:12
   |
 5 |     let x: bool = 42;
   |            ^^^^   ^^ expected `bool`, found `i64`
```

```
error: undefined variable `foo`
  --> script.squ:3:10
   |
 3 |     println(foo);
   |             ^^^ not found in this scope
```

## Project Structure

```
squam-lang/
├── crates/
│   ├── squam/           # CLI binary
│   ├── squam-lexer/     # Tokenization
│   ├── squam-parser/    # Parsing & AST
│   ├── squam-types/     # Type checking & inference
│   ├── squam-compiler/  # Bytecode compilation
│   ├── squam-vm/        # Virtual machine
│   ├── squam-stdlib/    # Standard library
│   ├── squam-embed/     # Rust embedding API
│   └── squam-wasm/      # WebAssembly support
├── examples/            # Example code
```

## Contributing

Contributions are welcome! Please feel free to submit issues and pull requests.

## License

Squam is released under the MIT License. See [LICENSE](LICENSE) for details.

---

[squ.am](https://squ.am)