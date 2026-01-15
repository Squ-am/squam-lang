mod convert;
mod engine;
mod error;
#[macro_use]
mod macros;

pub use convert::{FromSquam, IntoSquam};
pub use engine::Engine;
pub use error::{EmbedError, EmbedResult};

// Re-export Value for advanced use cases
pub use squam_vm::Value;