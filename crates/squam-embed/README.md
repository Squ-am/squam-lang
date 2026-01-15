# squam-embed

Embedding API for the Squam programming language.

Use this crate to run Squam scripts from Rust applications.

## Usage

```rust
use squam_embed::Engine;

fn main() {
    let mut engine = Engine::new();
    let result = engine.eval("1 + 2 * 3");
    println!("{}", result); // 7
}
```

See the [embedding guide](https://squ.am/docs/rust-embedding) for more examples.