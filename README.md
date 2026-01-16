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
| **Memory Safe** | Automatic garbage collection, no segfaults, no manual memory management |
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

## License

Squam is released under the MIT License. See [LICENSE](LICENSE) for details.

---

[squ.am](https://squ.am)