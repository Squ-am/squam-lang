pub mod types;
pub mod context;
pub mod scope;
pub mod checker;

pub use types::{TypeId, Ty, InferVar, GenericVar, GenericParam, Symbol};
pub use context::TypeContext;
pub use scope::GenericParamInfo;
pub use checker::{TypeChecker, TypeError};

use rustc_hash::FxHashMap;

/// The kind of generic instantiation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InstantiationKind {
    Function,
    Struct,
    Enum,
}

/// A concrete instantiation of a generic type or function.
#[derive(Debug, Clone)]
pub struct GenericInstantiation {
    /// The name of the generic (e.g., "identity", "Option")
    pub name: String,
    /// What kind of generic this is
    pub kind: InstantiationKind,
    /// The concrete type argument names (e.g., ["i64"] for Option<i64>)
    pub type_args: Vec<String>,
    /// Source location where this instantiation was used
    pub span: u32,
}

/// Type annotations for expressions, collected during type checking.
/// Used by the compiler for type-specialized code generation.
#[derive(Debug, Clone, Default)]
pub struct TypeAnnotations {
    /// Map from expression span start position to its resolved type
    pub expr_types: FxHashMap<u32, TypeId>,
    /// Map from struct name to ordered field names (for indexed field access)
    pub struct_layouts: FxHashMap<String, Vec<String>>,
    /// Generic instantiations discovered during type checking
    pub instantiations: Vec<GenericInstantiation>,
}

impl TypeAnnotations {
    pub fn new() -> Self {
        Self::default()
    }

    /// Record the type of an expression at the given span position
    pub fn record_expr(&mut self, span_start: u32, ty: TypeId) {
        self.expr_types.insert(span_start, ty);
    }

    /// Get the type of an expression at the given span position
    pub fn get_expr_type(&self, span_start: u32) -> Option<TypeId> {
        self.expr_types.get(&span_start).copied()
    }

    /// Record struct field layout for indexed access
    pub fn record_struct_layout(&mut self, name: String, fields: Vec<String>) {
        self.struct_layouts.insert(name, fields);
    }

    /// Record a generic instantiation
    pub fn record_instantiation(&mut self, name: String, kind: InstantiationKind, type_args: Vec<String>, span: u32) {
        // Avoid duplicates
        let exists = self.instantiations.iter().any(|inst| {
            inst.name == name && inst.kind == kind && inst.type_args == type_args
        });
        if !exists {
            self.instantiations.push(GenericInstantiation {
                name,
                kind,
                type_args,
                span,
            });
        }
    }

    /// Check if the type is a known integer type
    pub fn is_integer(&self, ty: TypeId) -> bool {
        matches!(ty,
            TypeId::I8 | TypeId::I16 | TypeId::I32 | TypeId::I64 | TypeId::I128 | TypeId::ISIZE |
            TypeId::U8 | TypeId::U16 | TypeId::U32 | TypeId::U64 | TypeId::U128 | TypeId::USIZE
        )
    }

    /// Check if the type is a known float type
    pub fn is_float(&self, ty: TypeId) -> bool {
        matches!(ty, TypeId::F32 | TypeId::F64)
    }

    /// Check if the type is a known string type
    pub fn is_string(&self, ty: TypeId) -> bool {
        matches!(ty, TypeId::STRING | TypeId::STR)
    }
}