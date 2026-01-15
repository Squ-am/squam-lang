pub mod bytecode;
pub mod compiler;
pub mod optimizer;

pub use bytecode::{Chunk, Constant, FunctionProto, OpCode, UpvalueInfo};
pub use compiler::{Compiler, CompileError};
pub use optimizer::{ConstantFolder, DeadCodeEliminator, OptConfig, OptStats, PeepholeOptimizer};