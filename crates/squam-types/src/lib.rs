pub mod types;
pub mod context;
pub mod scope;
pub mod checker;

pub use types::{TypeId, Ty, InferVar, GenericVar, GenericParam, Symbol};
pub use context::TypeContext;
pub use scope::GenericParamInfo;
pub use checker::{TypeChecker, TypeError};