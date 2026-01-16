use crate::types::{GenericVar, Symbol, TypeId};
use rustc_hash::FxHashMap;
use squam_lexer::Span;

/// A binding in the symbol table.
#[derive(Debug, Clone)]
pub struct Binding {
    pub name: Symbol,
    pub ty: TypeId,
    pub mutable: bool,
    pub span: Span,
    pub kind: BindingKind,
}

/// The kind of a binding.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BindingKind {
    /// A local variable
    Local,
    /// A function parameter
    Parameter,
    /// A loop variable (for loop)
    LoopVar,
    /// A closure capture
    Capture,
}

/// A scope in the symbol table.
#[derive(Debug, Clone)]
struct Scope {
    bindings: FxHashMap<Symbol, Binding>,
    kind: ScopeKind,
}

/// The kind of scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    /// Top-level module scope
    Module,
    /// Function body
    Function,
    /// Block scope
    Block,
    /// Loop body (for break/continue)
    Loop,
    /// Closure body
    Closure,
}

/// The symbol table manages variable scopes.
pub struct SymbolTable {
    scopes: Vec<Scope>,
}

impl SymbolTable {
    /// Create a new symbol table with module scope.
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope {
                bindings: FxHashMap::default(),
                kind: ScopeKind::Module,
            }],
        }
    }

    /// Push a new scope.
    pub fn push_scope(&mut self, kind: ScopeKind) {
        self.scopes.push(Scope {
            bindings: FxHashMap::default(),
            kind,
        });
    }

    /// Pop the current scope.
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Get the current scope kind.
    pub fn current_scope_kind(&self) -> ScopeKind {
        self.scopes
            .last()
            .map(|s| s.kind)
            .unwrap_or(ScopeKind::Module)
    }

    /// Define a new binding in the current scope.
    pub fn define(&mut self, binding: Binding) -> Result<(), BindingError> {
        let scope = self.scopes.last_mut().unwrap();

        if scope.bindings.contains_key(&binding.name) {
            return Err(BindingError::AlreadyDefined {
                name: binding.name.clone(),
                span: binding.span,
            });
        }

        scope.bindings.insert(binding.name.clone(), binding);
        Ok(())
    }

    /// Look up a binding by name.
    pub fn lookup(&self, name: &str) -> Option<&Binding> {
        for scope in self.scopes.iter().rev() {
            if let Some(binding) = scope.bindings.get(name) {
                return Some(binding);
            }
        }
        None
    }

    /// Look up a binding mutably by name.
    pub fn lookup_mut(&mut self, name: &str) -> Option<&mut Binding> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(binding) = scope.bindings.get_mut(name) {
                return Some(binding);
            }
        }
        None
    }

    /// Check if we're inside a loop.
    pub fn in_loop(&self) -> bool {
        self.scopes.iter().any(|s| s.kind == ScopeKind::Loop)
    }

    /// Check if we're inside a function.
    pub fn in_function(&self) -> bool {
        self.scopes.iter().any(|s| s.kind == ScopeKind::Function)
    }

    /// Check if we're inside a closure.
    pub fn in_closure(&self) -> bool {
        self.scopes.iter().any(|s| s.kind == ScopeKind::Closure)
    }

    /// Get the depth of the current scope (for debugging).
    pub fn depth(&self) -> usize {
        self.scopes.len()
    }

    /// Get all bindings in the current scope.
    pub fn current_bindings(&self) -> impl Iterator<Item = &Binding> {
        self.scopes
            .last()
            .into_iter()
            .flat_map(|s| s.bindings.values())
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

/// Errors that can occur when working with bindings.
#[derive(Debug, Clone, thiserror::Error)]
pub enum BindingError {
    #[error("variable `{name}` is already defined in this scope")]
    AlreadyDefined { name: Symbol, span: Span },
}

/// A type definition in the type namespace.
#[derive(Debug, Clone)]
pub struct TypeDef {
    pub name: Symbol,
    pub ty: TypeId,
    pub span: Span,
    pub kind: TypeDefKind,
    /// Whether this type is public (accessible from other modules)
    pub is_public: bool,
}

/// The kind of type definition.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TypeDefKind {
    Struct,
    Enum,
    TypeAlias,
    GenericParam,
}

/// The type namespace manages type definitions.
pub struct TypeNamespace {
    scopes: Vec<FxHashMap<Symbol, TypeDef>>,
}

impl TypeNamespace {
    /// Create a new type namespace.
    pub fn new() -> Self {
        Self {
            scopes: vec![FxHashMap::default()],
        }
    }

    /// Push a new scope.
    pub fn push_scope(&mut self) {
        self.scopes.push(FxHashMap::default());
    }

    /// Pop the current scope.
    pub fn pop_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    /// Define a new type.
    pub fn define(&mut self, def: TypeDef) -> Result<(), BindingError> {
        let scope = self.scopes.last_mut().unwrap();

        if scope.contains_key(&def.name) {
            return Err(BindingError::AlreadyDefined {
                name: def.name.clone(),
                span: def.span,
            });
        }

        scope.insert(def.name.clone(), def);
        Ok(())
    }

    /// Look up a type by name.
    pub fn lookup(&self, name: &str) -> Option<&TypeDef> {
        for scope in self.scopes.iter().rev() {
            if let Some(def) = scope.get(name) {
                return Some(def);
            }
        }
        None
    }
}

impl Default for TypeNamespace {
    fn default() -> Self {
        Self::new()
    }
}

/// A generic parameter in a function or type.
#[derive(Debug, Clone)]
pub struct GenericParamInfo {
    /// The name of the generic parameter (e.g., "T")
    pub name: Symbol,
    /// The type variable assigned to this parameter
    pub var: GenericVar,
    /// The TypeId for this generic (Ty::Generic(var))
    pub ty: TypeId,
    /// Trait bounds (as TypeIds)
    pub bounds: Vec<TypeId>,
}

/// A function signature.
#[derive(Debug, Clone)]
pub struct FunctionSig {
    pub name: Symbol,
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
    pub generic_params: Vec<GenericParamInfo>,
    pub span: Span,
    /// Whether this function is public (accessible from other modules)
    pub is_public: bool,
}

/// The function namespace.
pub struct FunctionNamespace {
    functions: FxHashMap<Symbol, FunctionSig>,
}

impl FunctionNamespace {
    pub fn new() -> Self {
        Self {
            functions: FxHashMap::default(),
        }
    }

    pub fn define(&mut self, sig: FunctionSig) -> Result<(), BindingError> {
        if self.functions.contains_key(&sig.name) {
            return Err(BindingError::AlreadyDefined {
                name: sig.name.clone(),
                span: sig.span,
            });
        }
        self.functions.insert(sig.name.clone(), sig);
        Ok(())
    }

    pub fn lookup(&self, name: &str) -> Option<&FunctionSig> {
        self.functions.get(name)
    }
}

impl Default for FunctionNamespace {
    fn default() -> Self {
        Self::new()
    }
}

/// A trait definition stored in the trait namespace.
#[derive(Debug, Clone)]
pub struct TraitDef {
    pub name: Symbol,
    pub generic_params: Vec<GenericParamInfo>,
    pub methods: Vec<TraitMethodSig>,
    pub associated_types: Vec<AssociatedTypeDef>,
    pub supertraits: Vec<Symbol>,
    pub span: Span,
}

/// A method signature in a trait.
#[derive(Debug, Clone)]
pub struct TraitMethodSig {
    pub name: Symbol,
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
    pub generic_params: Vec<GenericParamInfo>,
    pub has_default: bool,
    pub span: Span,
}

/// An associated type definition in a trait.
#[derive(Debug, Clone)]
pub struct AssociatedTypeDef {
    pub name: Symbol,
    pub bounds: Vec<TypeId>,
    pub default: Option<TypeId>,
    pub span: Span,
}

/// A method signature stored in an impl block.
#[derive(Debug, Clone)]
pub struct ImplMethodSig {
    pub name: Symbol,
    pub params: Vec<TypeId>,
    pub return_type: TypeId,
    pub generic_params: Vec<GenericParamInfo>,
    /// Whether this method takes self (true) or is a static method (false)
    pub has_receiver: bool,
    pub span: Span,
}

/// An impl that has been collected.
#[derive(Debug, Clone)]
pub struct ImplDef {
    pub trait_name: Option<Symbol>,
    pub self_ty: TypeId,
    pub generic_params: Vec<GenericParamInfo>,
    pub methods: Vec<ImplMethodSig>,
    pub span: Span,
}

/// The trait namespace manages trait definitions.
pub struct TraitNamespace {
    traits: FxHashMap<Symbol, TraitDef>,
    /// Implementations indexed by trait name (or "_inherent" for inherent impls)
    impls: Vec<ImplDef>,
}

impl TraitNamespace {
    pub fn new() -> Self {
        Self {
            traits: FxHashMap::default(),
            impls: Vec::new(),
        }
    }

    /// Define a new trait.
    pub fn define(&mut self, def: TraitDef) -> Result<(), BindingError> {
        if self.traits.contains_key(&def.name) {
            return Err(BindingError::AlreadyDefined {
                name: def.name.clone(),
                span: def.span,
            });
        }
        self.traits.insert(def.name.clone(), def);
        Ok(())
    }

    /// Look up a trait by name.
    pub fn lookup(&self, name: &str) -> Option<&TraitDef> {
        self.traits.get(name)
    }

    /// Add an impl.
    pub fn add_impl(&mut self, impl_def: ImplDef) {
        self.impls.push(impl_def);
    }

    /// Find all impls for a given type.
    pub fn impls_for_type(&self, ty: TypeId) -> Vec<&ImplDef> {
        self.impls.iter().filter(|i| i.self_ty == ty).collect()
    }

    /// Find all impls of a given trait.
    pub fn impls_of_trait(&self, trait_name: &str) -> Vec<&ImplDef> {
        self.impls
            .iter()
            .filter(|i| {
                i.trait_name
                    .as_ref()
                    .is_some_and(|n| n.as_ref() == trait_name)
            })
            .collect()
    }

    /// Check if a type implements a trait.
    pub fn type_implements_trait(&self, ty: TypeId, trait_name: &str) -> bool {
        self.impls.iter().any(|i| {
            i.self_ty == ty
                && i.trait_name
                    .as_ref()
                    .is_some_and(|n| n.as_ref() == trait_name)
        })
    }

    /// Look up a method by name for a given type.
    /// Returns the method signature and the impl's self_ty if found.
    pub fn lookup_method(&self, ty: TypeId, method_name: &str) -> Option<(&ImplMethodSig, TypeId)> {
        // First check inherent impls (no trait)
        for impl_def in &self.impls {
            if impl_def.self_ty == ty && impl_def.trait_name.is_none() {
                for method in &impl_def.methods {
                    if method.name.as_ref() == method_name {
                        return Some((method, impl_def.self_ty));
                    }
                }
            }
        }
        // Then check trait impls
        for impl_def in &self.impls {
            if impl_def.self_ty == ty && impl_def.trait_name.is_some() {
                for method in &impl_def.methods {
                    if method.name.as_ref() == method_name {
                        return Some((method, impl_def.self_ty));
                    }
                }
            }
        }
        None
    }

    /// Get all impls (for iteration).
    pub fn all_impls(&self) -> &[ImplDef] {
        &self.impls
    }
}

impl Default for TraitNamespace {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scope_push_pop() {
        let mut table = SymbolTable::new();
        assert_eq!(table.depth(), 1);

        table.push_scope(ScopeKind::Function);
        assert_eq!(table.depth(), 2);

        table.push_scope(ScopeKind::Block);
        assert_eq!(table.depth(), 3);

        table.pop_scope();
        assert_eq!(table.depth(), 2);
    }

    #[test]
    fn test_define_and_lookup() {
        let mut table = SymbolTable::new();

        let binding = Binding {
            name: "x".into(),
            ty: TypeId::I32,
            mutable: false,
            span: Span::dummy(),
            kind: BindingKind::Local,
        };

        table.define(binding).unwrap();

        let found = table.lookup("x").unwrap();
        assert_eq!(found.ty, TypeId::I32);
        assert!(!found.mutable);
    }

    #[test]
    fn test_shadowing() {
        let mut table = SymbolTable::new();

        // Define x in outer scope
        table
            .define(Binding {
                name: "x".into(),
                ty: TypeId::I32,
                mutable: false,
                span: Span::dummy(),
                kind: BindingKind::Local,
            })
            .unwrap();

        // Push new scope and define x again
        table.push_scope(ScopeKind::Block);
        table
            .define(Binding {
                name: "x".into(),
                ty: TypeId::I64,
                mutable: true,
                span: Span::dummy(),
                kind: BindingKind::Local,
            })
            .unwrap();

        // Should find inner x
        let found = table.lookup("x").unwrap();
        assert_eq!(found.ty, TypeId::I64);
        assert!(found.mutable);

        // Pop scope
        table.pop_scope();

        // Should find outer x
        let found = table.lookup("x").unwrap();
        assert_eq!(found.ty, TypeId::I32);
        assert!(!found.mutable);
    }
}
