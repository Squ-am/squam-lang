use squam_compiler::CompileError;
use squam_vm::RuntimeError;

/// Result type for embedding operations.
pub type EmbedResult<T> = Result<T, EmbedError>;

/// Errors that can occur when embedding Squam.
#[derive(Debug, Clone, thiserror::Error)]
pub enum EmbedError {
    /// Parse error in source code
    #[error("parse error: {0}")]
    Parse(String),

    /// Compilation error
    #[error("compile error: {0}")]
    Compile(#[from] CompileError),

    /// Runtime error during execution
    #[error("runtime error: {0}")]
    Runtime(#[from] RuntimeError),

    /// Type conversion error
    #[error("type error: expected {expected}, got {got}")]
    Type { expected: String, got: String },

    /// Function not found
    #[error("function '{0}' not found")]
    FunctionNotFound(String),

    /// Variable not found
    #[error("variable '{0}' not found")]
    VariableNotFound(String),

    /// Arity mismatch when calling a function
    #[error("arity mismatch: expected {expected} arguments, got {got}")]
    ArityMismatch { expected: usize, got: usize },
}
