use rustc_hash::FxHashMap;
use squam_lexer::Span;
use squam_parser::ast::*;

use crate::context::TypeContext;
use crate::scope::{SymbolTable, TypeNamespace, FunctionNamespace, TraitNamespace, Binding, BindingKind, ScopeKind, FunctionSig, TypeDef, TypeDefKind, GenericParamInfo, TraitDef as ScopeTraitDef, TraitMethodSig, AssociatedTypeDef, ImplDef, ImplMethodSig};
use crate::types::{TypeId, Ty, InferVar, GenericVar};
use crate::TypeAnnotations;

/// Type checking errors.
#[derive(Debug, Clone, thiserror::Error)]
pub enum TypeError {
    #[error("type mismatch: expected `{expected}`, found `{found}`")]
    Mismatch {
        expected: String,
        found: String,
        span: Span,
    },

    #[error("undefined variable `{name}`")]
    UndefinedVariable { name: String, span: Span },

    #[error("undefined type `{name}`")]
    UndefinedType { name: String, span: Span },

    #[error("undefined function `{name}`")]
    UndefinedFunction { name: String, span: Span },

    #[error("cannot assign to immutable variable `{name}`")]
    ImmutableAssignment { name: String, span: Span },

    #[error("wrong number of arguments: expected {expected}, found {found}")]
    ArgCountMismatch {
        expected: usize,
        found: usize,
        span: Span,
    },

    #[error("cannot call non-function type `{ty}`")]
    NotCallable { ty: String, span: Span },

    #[error("cannot index into type `{ty}`")]
    NotIndexable { ty: String, span: Span },

    #[error("no field `{field}` on type `{ty}`")]
    NoSuchField { ty: String, field: String, span: Span },

    #[error("break outside of loop")]
    BreakOutsideLoop { span: Span },

    #[error("continue outside of loop")]
    ContinueOutsideLoop { span: Span },

    #[error("return outside of function")]
    ReturnOutsideFunction { span: Span },

    #[error("infinite type: {0}")]
    InfiniteType(String, Span),

    #[error("{0}")]
    Custom(String, Span),

    #[error("wrong number of type arguments: expected {expected}, found {found}")]
    TypeArgCountMismatch {
        expected: usize,
        found: usize,
        span: Span,
    },

    #[error("the trait bound `{ty}: {trait_name}` is not satisfied")]
    TraitBoundNotSatisfied {
        ty: String,
        trait_name: String,
        span: Span,
    },

    #[error("undefined trait `{name}`")]
    UndefinedTrait { name: String, span: Span },

    #[error("missing method `{method}` in impl for trait `{trait_name}`")]
    MissingTraitMethod {
        trait_name: String,
        method: String,
        span: Span,
    },

    #[error("method `{method}` signature does not match trait `{trait_name}`")]
    TraitMethodSignatureMismatch {
        trait_name: String,
        method: String,
        span: Span,
    },
}

impl TypeError {
    pub fn span(&self) -> Span {
        match self {
            TypeError::Mismatch { span, .. } => *span,
            TypeError::UndefinedVariable { span, .. } => *span,
            TypeError::UndefinedType { span, .. } => *span,
            TypeError::UndefinedFunction { span, .. } => *span,
            TypeError::ImmutableAssignment { span, .. } => *span,
            TypeError::ArgCountMismatch { span, .. } => *span,
            TypeError::NotCallable { span, .. } => *span,
            TypeError::NotIndexable { span, .. } => *span,
            TypeError::NoSuchField { span, .. } => *span,
            TypeError::BreakOutsideLoop { span } => *span,
            TypeError::ContinueOutsideLoop { span } => *span,
            TypeError::ReturnOutsideFunction { span } => *span,
            TypeError::InfiniteType(_, span) => *span,
            TypeError::Custom(_, span) => *span,
            TypeError::TypeArgCountMismatch { span, .. } => *span,
            TypeError::TraitBoundNotSatisfied { span, .. } => *span,
            TypeError::UndefinedTrait { span, .. } => *span,
            TypeError::MissingTraitMethod { span, .. } => *span,
            TypeError::TraitMethodSignatureMismatch { span, .. } => *span,
        }
    }
}

/// The type checker.
pub struct TypeChecker {
    /// Type context for interning
    pub ctx: TypeContext,
    /// Variable symbol table
    pub symbols: SymbolTable,
    /// Type namespace
    pub types: TypeNamespace,
    /// Function namespace
    pub functions: FunctionNamespace,
    /// Trait namespace
    pub traits: TraitNamespace,
    /// Substitutions for inference variables
    substitutions: FxHashMap<InferVar, TypeId>,
    /// Accumulated errors
    errors: Vec<TypeError>,
    /// Current function return type (for checking returns)
    current_return_type: Option<TypeId>,
    /// Type annotations for expressions (for compiler optimization)
    annotations: TypeAnnotations,
}

impl TypeChecker {
    /// Create a new type checker.
    pub fn new() -> Self {
        let mut checker = Self {
            ctx: TypeContext::new(),
            symbols: SymbolTable::new(),
            types: TypeNamespace::new(),
            functions: FunctionNamespace::new(),
            traits: TraitNamespace::new(),
            substitutions: FxHashMap::default(),
            errors: Vec::new(),
            current_return_type: None,
            annotations: TypeAnnotations::new(),
        };

        // Register built-in types
        checker.register_builtins();
        checker
    }

    /// Take the collected type annotations.
    pub fn take_annotations(&mut self) -> TypeAnnotations {
        std::mem::take(&mut self.annotations)
    }

    /// Record the type of an expression (for compiler optimization).
    fn record_expr_type(&mut self, span: Span, ty: TypeId) {
        // Resolve the type to get the concrete type
        let resolved = self.resolve(ty);
        self.annotations.record_expr(span.start, resolved);
    }

    fn register_builtins(&mut self) {
        // Primitive types are always available
        let primitives = [
            ("()", TypeId::UNIT),
            ("bool", TypeId::BOOL),
            ("i8", TypeId::I8),
            ("i16", TypeId::I16),
            ("i32", TypeId::I32),
            ("i64", TypeId::I64),
            ("i128", TypeId::I128),
            ("isize", TypeId::ISIZE),
            ("u8", TypeId::U8),
            ("u16", TypeId::U16),
            ("u32", TypeId::U32),
            ("u64", TypeId::U64),
            ("u128", TypeId::U128),
            ("usize", TypeId::USIZE),
            ("f32", TypeId::F32),
            ("f64", TypeId::F64),
            ("char", TypeId::CHAR),
            ("str", TypeId::STR),
            ("String", TypeId::STRING),
        ];

        for (name, ty) in primitives {
            let _ = self.types.define(TypeDef {
                name: name.into(),
                ty,
                span: Span::dummy(),
                kind: TypeDefKind::TypeAlias,
                is_public: true,
            });
        }
    }

    /// Get accumulated errors.
    pub fn errors(&self) -> &[TypeError] {
        &self.errors
    }

    /// Take accumulated errors.
    pub fn take_errors(&mut self) -> Vec<TypeError> {
        std::mem::take(&mut self.errors)
    }

    /// Register an extern/native function with its type signature.
    /// This is used to inform the type checker about stdlib functions.
    pub fn register_extern_function(&mut self, name: &str, params: Vec<TypeId>, return_type: TypeId) {
        let _ = self.functions.define(FunctionSig {
            name: name.into(),
            params,
            return_type,
            generic_params: Vec::new(),
            span: Span::dummy(),
            is_public: true,
        });
    }

    /// Report an error.
    fn error(&mut self, err: TypeError) {
        self.errors.push(err);
    }

    // ---
    // Unification
    // ---

    /// Unify two types, recording any necessary substitutions.
    pub fn unify(&mut self, a: TypeId, b: TypeId, span: Span) -> bool {
        let a = self.resolve(a);
        let b = self.resolve(b);

        if a == b {
            return true;
        }

        // Handle error types (they unify with anything)
        if self.ctx.is_error(a) || self.ctx.is_error(b) {
            return true;
        }

        // Handle never type (it unifies with anything)
        if self.ctx.is_never(a) || self.ctx.is_never(b) {
            return true;
        }

        // Handle any type (it unifies with anything, for stdlib functions)
        if a == TypeId::ANY || b == TypeId::ANY {
            return true;
        }

        // Handle inference variables
        if let Some(var) = self.ctx.as_infer_var(a) {
            return self.bind_var(var, b, span);
        }
        if let Some(var) = self.ctx.as_infer_var(b) {
            return self.bind_var(var, a, span);
        }

        // Structural unification
        let ty_a = self.ctx.get(a).clone();
        let ty_b = self.ctx.get(b).clone();

        match (&ty_a, &ty_b) {
            (Ty::Tuple(elems_a), Ty::Tuple(elems_b)) if elems_a.len() == elems_b.len() => {
                for (&ea, &eb) in elems_a.iter().zip(elems_b.iter()) {
                    if !self.unify(ea, eb, span) {
                        return false;
                    }
                }
                true
            }

            (Ty::Array { element: ea, size: sa }, Ty::Array { element: eb, size: sb })
                if sa == sb =>
            {
                self.unify(*ea, *eb, span)
            }

            (Ty::Slice(ea), Ty::Slice(eb)) => self.unify(*ea, *eb, span),

            (
                Ty::Reference { mutable: ma, inner: ia },
                Ty::Reference { mutable: mb, inner: ib },
            ) if ma == mb => self.unify(*ia, *ib, span),

            (
                Ty::Function { params: pa, return_type: ra },
                Ty::Function { params: pb, return_type: rb },
            ) if pa.len() == pb.len() => {
                for (&p1, &p2) in pa.iter().zip(pb.iter()) {
                    if !self.unify(p1, p2, span) {
                        return false;
                    }
                }
                self.unify(*ra, *rb, span)
            }

            (
                Ty::Applied { base: ba, args: aa },
                Ty::Applied { base: bb, args: ab },
            ) if aa.len() == ab.len() => {
                if !self.unify(*ba, *bb, span) {
                    return false;
                }
                for (&a1, &a2) in aa.iter().zip(ab.iter()) {
                    if !self.unify(a1, a2, span) {
                        return false;
                    }
                }
                true
            }

            _ => {
                self.error(TypeError::Mismatch {
                    expected: self.ctx.display(a),
                    found: self.ctx.display(b),
                    span,
                });
                false
            }
        }
    }

    /// Bind an inference variable to a type.
    fn bind_var(&mut self, var: InferVar, ty: TypeId, span: Span) -> bool {
        // Occurs check
        if self.occurs(var, ty) {
            self.error(TypeError::InfiniteType(
                format!("?{} occurs in {}", var.0, self.ctx.display(ty)),
                span,
            ));
            return false;
        }

        self.substitutions.insert(var, ty);
        true
    }

    /// Check if a variable occurs in a type (for occurs check).
    fn occurs(&self, var: InferVar, ty: TypeId) -> bool {
        let ty = self.resolve(ty);

        match self.ctx.get(ty) {
            Ty::Infer(v) => *v == var,
            Ty::Tuple(elems) => elems.iter().any(|&e| self.occurs(var, e)),
            Ty::Array { element, .. } => self.occurs(var, *element),
            Ty::Slice(elem) => self.occurs(var, *elem),
            Ty::Reference { inner, .. } => self.occurs(var, *inner),
            Ty::Function { params, return_type } => {
                params.iter().any(|&p| self.occurs(var, p)) || self.occurs(var, *return_type)
            }
            Ty::Applied { base, args } => {
                self.occurs(var, *base) || args.iter().any(|&a| self.occurs(var, a))
            }
            _ => false,
        }
    }

    /// Resolve a type by following substitutions.
    pub fn resolve(&self, ty: TypeId) -> TypeId {
        if let Some(var) = self.ctx.as_infer_var(ty) {
            if let Some(&resolved) = self.substitutions.get(&var) {
                return self.resolve(resolved);
            }
        }
        ty
    }

    /// Fully resolve a type, replacing all inference variables.
    pub fn fully_resolve(&mut self, ty: TypeId) -> TypeId {
        let ty = self.resolve(ty);

        match self.ctx.get(ty).clone() {
            Ty::Infer(_) => ty, // Unresolved inference variable
            Ty::Tuple(elems) => {
                let resolved: Vec<_> = elems.iter().map(|&e| self.fully_resolve(e)).collect();
                self.ctx.tuple(resolved)
            }
            Ty::Array { element, size } => {
                let resolved = self.fully_resolve(element);
                self.ctx.array(resolved, size)
            }
            Ty::Slice(elem) => {
                let resolved = self.fully_resolve(elem);
                self.ctx.slice(resolved)
            }
            Ty::Reference { mutable, inner } => {
                let resolved = self.fully_resolve(inner);
                self.ctx.reference(resolved, mutable)
            }
            Ty::Function { params, return_type } => {
                let params: Vec<_> = params.iter().map(|&p| self.fully_resolve(p)).collect();
                let ret = self.fully_resolve(return_type);
                self.ctx.function(params, ret)
            }
            Ty::Applied { base, args } => {
                let base = self.fully_resolve(base);
                let args: Vec<_> = args.iter().map(|&a| self.fully_resolve(a)).collect();
                self.ctx.applied(base, args)
            }
            _ => ty,
        }
    }

    // ---
    // Module checking
    // ---

    /// Check a module.
    pub fn check_module(&mut self, module: &Module) {
        // First pass: collect all type and function definitions
        for item in &module.items {
            self.collect_item(item);
        }

        // Second pass: check all items
        for item in &module.items {
            self.check_item(item);
        }
    }

    /// Collect generic parameters from AST, returning GenericParamInfo for each.
    fn collect_generics(&mut self, generics: Option<&Generics>) -> Vec<GenericParamInfo> {
        let Some(generics) = generics else {
            return Vec::new();
        };

        let mut result = Vec::new();
        for param in &generics.params {
            match param {
                GenericParam::Type(type_param) => {
                    let var = self.ctx.fresh_generic();
                    let ty = self.ctx.generic(var);

                    // Resolve trait bounds (if any)
                    let bounds: Vec<TypeId> = type_param.bounds
                        .iter()
                        .map(|bound| self.resolve_type_path(bound))
                        .collect();

                    result.push(GenericParamInfo {
                        name: type_param.name.name.clone(),
                        var,
                        ty,
                        bounds,
                    });
                }
                GenericParam::Const(_const_param) => {
                    // Const generics - not fully supported yet
                    // We could add ConstGenericParamInfo later
                }
            }
        }
        result
    }

    /// Resolve a type path (used for trait bounds).
    fn resolve_type_path(&mut self, path: &TypePath) -> TypeId {
        if path.segments.len() == 1 && path.segments[0].args.is_none() {
            let name = &path.segments[0].ident.name;
            if let Some(def) = self.types.lookup(name) {
                return def.ty;
            }
        }
        // For now, return an error type for unresolved trait bounds
        self.ctx.error()
    }

    /// Get trait name from a type ID that represents a trait bound.
    fn get_trait_name(&self, ty: TypeId) -> Option<String> {
        // For now, we store trait bounds as TypeId pointing to trait definitions
        // We need to look up the trait name from the type
        match self.ctx.get(ty) {
            Ty::Error => None,
            _ => {
                // Look through traits to find one with matching TypeId
                // This is a simplified approach - in a real implementation
                // we'd have a more direct mapping
                None
            }
        }
    }

    /// Check if a concrete type satisfies a trait bound.
    /// Returns true if the type implements the trait (or if we can't verify).
    fn check_trait_bound(&self, ty: TypeId, trait_bound: TypeId, _span: Span) -> bool {
        // If the bound is an error type (unresolved), skip checking
        if self.ctx.is_error(trait_bound) {
            return true;
        }

        // Get the resolved type
        let resolved = self.resolve(ty);

        // If the type is an inference variable, we can't check bounds yet
        if self.ctx.is_infer(resolved) {
            return true;
        }

        // If the type is a generic variable, bounds will be checked when it's instantiated
        if let Some(_) = self.ctx.as_generic_var(resolved) {
            return true;
        }

        // For now, we do a simple check - look up if the type has an impl for the trait
        // This is a simplified version; a full implementation would handle:
        // - Generic impls (impl<T> Trait for Vec<T>)
        // - Blanket impls
        // - Associated type constraints
        true // For now, assume bounds are satisfied
    }

    /// Verify trait bounds for a set of generic parameters.
    fn verify_trait_bounds(
        &mut self,
        generic_params: &[GenericParamInfo],
        type_args: &[TypeId],
        span: Span,
    ) {
        for (param, &arg_ty) in generic_params.iter().zip(type_args.iter()) {
            for &bound in &param.bounds {
                if !self.check_trait_bound(arg_ty, bound, span) {
                    // Get the trait name for the error message
                    let trait_name = self.get_trait_name(bound)
                        .unwrap_or_else(|| "?".to_string());
                    self.error(TypeError::TraitBoundNotSatisfied {
                        ty: self.ctx.display(arg_ty),
                        trait_name,
                        span,
                    });
                }
            }
        }
    }

    /// Collect type/function signatures (first pass).
    fn collect_item(&mut self, item: &Item) {
        match item {
            Item::Function(func) => {
                // First collect generic parameters
                let generic_params = self.collect_generics(func.generics.as_ref());

                // Push type scope for generic params so they're visible when resolving param types
                self.types.push_scope();
                for gp in &generic_params {
                    let _ = self.types.define(TypeDef {
                        name: gp.name.clone(),
                        ty: gp.ty,
                        span: func.span,
                        kind: TypeDefKind::GenericParam,
                        is_public: true,
                    });
                }

                let params: Vec<_> = func.params.iter().map(|p| self.resolve_ast_type(&p.ty)).collect();
                let return_type = func.return_type.as_ref()
                    .map(|t| self.resolve_ast_type(t))
                    .unwrap_or(self.ctx.unit());

                self.types.pop_scope();

                let sig = FunctionSig {
                    name: func.name.name.clone(),
                    params,
                    return_type,
                    generic_params,
                    span: func.span,
                    is_public: matches!(func.visibility, Visibility::Public),
                };
                let _ = self.functions.define(sig);
            }
            Item::Struct(s) => {
                let generic_params = self.collect_generics(s.generics.as_ref());

                // Push type scope for generic params
                self.types.push_scope();
                for gp in &generic_params {
                    let _ = self.types.define(TypeDef {
                        name: gp.name.clone(),
                        ty: gp.ty,
                        span: s.span,
                        kind: TypeDefKind::GenericParam,
                        is_public: true,
                    });
                }

                // Collect struct fields
                let fields = match &s.fields {
                    StructFields::Named(named_fields) => {
                        named_fields.iter().map(|f| crate::types::StructField {
                            name: f.name.name.clone(),
                            ty: self.resolve_ast_type(&f.ty),
                            is_public: matches!(f.visibility, Visibility::Public),
                        }).collect()
                    }
                    StructFields::Tuple(tuple_fields) => {
                        tuple_fields.iter().enumerate().map(|(i, f)| crate::types::StructField {
                            name: format!("{}", i).into(),
                            ty: self.resolve_ast_type(&f.ty),
                            is_public: matches!(f.visibility, Visibility::Public),
                        }).collect()
                    }
                    StructFields::Unit => Vec::new(),
                };

                self.types.pop_scope();

                let type_generic_params: Vec<_> = generic_params.iter().map(|gp| {
                    crate::types::GenericParam {
                        name: gp.name.clone(),
                        var: gp.var,
                        bounds: gp.bounds.clone(),
                    }
                }).collect();

                let ty = self.ctx.intern(Ty::Struct(crate::types::StructType {
                    name: s.name.name.clone(),
                    fields,
                    generic_params: type_generic_params,
                }));
                let _ = self.types.define(TypeDef {
                    name: s.name.name.clone(),
                    ty,
                    span: s.span,
                    kind: TypeDefKind::Struct,
                    is_public: matches!(s.visibility, Visibility::Public),
                });
            }
            Item::Enum(e) => {
                let generic_params = self.collect_generics(e.generics.as_ref());

                // Push type scope for generic params
                self.types.push_scope();
                for gp in &generic_params {
                    let _ = self.types.define(TypeDef {
                        name: gp.name.clone(),
                        ty: gp.ty,
                        span: e.span,
                        kind: TypeDefKind::GenericParam,
                        is_public: true,
                    });
                }

                // Collect enum variants
                let variants: Vec<_> = e.variants.iter().map(|v| {
                    let fields = match &v.fields {
                        StructFields::Unit => crate::types::VariantFields::Unit,
                        StructFields::Tuple(tuple_fields) => {
                            let tys: Vec<_> = tuple_fields.iter()
                                .map(|f| self.resolve_ast_type(&f.ty))
                                .collect();
                            crate::types::VariantFields::Tuple(tys)
                        }
                        StructFields::Named(named_fields) => {
                            let fields: Vec<_> = named_fields.iter()
                                .map(|f| crate::types::StructField {
                                    name: f.name.name.clone(),
                                    ty: self.resolve_ast_type(&f.ty),
                                    is_public: matches!(f.visibility, Visibility::Public),
                                })
                                .collect();
                            crate::types::VariantFields::Struct(fields)
                        }
                    };
                    crate::types::EnumVariant {
                        name: v.name.name.clone(),
                        fields,
                    }
                }).collect();

                self.types.pop_scope();

                let type_generic_params: Vec<_> = generic_params.iter().map(|gp| {
                    crate::types::GenericParam {
                        name: gp.name.clone(),
                        var: gp.var,
                        bounds: gp.bounds.clone(),
                    }
                }).collect();

                let ty = self.ctx.intern(Ty::Enum(crate::types::EnumType {
                    name: e.name.name.clone(),
                    variants,
                    generic_params: type_generic_params,
                }));
                let _ = self.types.define(TypeDef {
                    name: e.name.name.clone(),
                    ty,
                    span: e.span,
                    kind: TypeDefKind::Enum,
                    is_public: matches!(e.visibility, Visibility::Public),
                });
            }
            Item::Trait(t) => {
                self.collect_trait(t);
            }
            Item::Impl(i) => {
                self.collect_impl(i);
            }
            _ => {}
        }
    }

    /// Collect a trait definition.
    fn collect_trait(&mut self, t: &TraitDef) {
        let generic_params = self.collect_generics(t.generics.as_ref());

        // Push type scope for generic params
        self.types.push_scope();

        // Define Self as a generic type within the trait
        let self_var = self.ctx.fresh_generic();
        let self_ty = self.ctx.generic(self_var);
        let _ = self.types.define(TypeDef {
            name: "Self".into(),
            ty: self_ty,
            span: t.span,
            kind: TypeDefKind::GenericParam,
            is_public: true,
        });

        for gp in &generic_params {
            let _ = self.types.define(TypeDef {
                name: gp.name.clone(),
                ty: gp.ty,
                span: t.span,
                kind: TypeDefKind::GenericParam,
                is_public: true,
            });
        }

        // Collect methods
        let methods: Vec<TraitMethodSig> = t.items.iter().filter_map(|item| {
            match item {
                TraitItem::Function(f) => {
                    let method_generics = self.collect_generics(f.generics.as_ref());
                    let params: Vec<TypeId> = f.params.iter()
                        .map(|p| self.resolve_ast_type(&p.ty))
                        .collect();
                    let return_type = f.return_type.as_ref()
                        .map(|t| self.resolve_ast_type(t))
                        .unwrap_or(self.ctx.unit());

                    Some(TraitMethodSig {
                        name: f.name.name.clone(),
                        params,
                        return_type,
                        generic_params: method_generics,
                        has_default: f.default.is_some(),
                        span: f.span,
                    })
                }
                _ => None,
            }
        }).collect();

        // Collect associated types
        let associated_types: Vec<AssociatedTypeDef> = t.items.iter().filter_map(|item| {
            match item {
                TraitItem::Type(ty) => {
                    let bounds: Vec<TypeId> = ty.bounds.iter()
                        .map(|b| self.resolve_type_path(b))
                        .collect();
                    let default = ty.default.as_ref()
                        .map(|d| self.resolve_ast_type(d));

                    Some(AssociatedTypeDef {
                        name: ty.name.name.clone(),
                        bounds,
                        default,
                        span: ty.span,
                    })
                }
                _ => None,
            }
        }).collect();

        self.types.pop_scope();

        // Collect supertraits
        let supertraits: Vec<_> = t.bounds.iter()
            .filter_map(|b| {
                if b.segments.len() == 1 {
                    Some(b.segments[0].ident.name.clone())
                } else {
                    None
                }
            })
            .collect();

        let trait_def = ScopeTraitDef {
            name: t.name.name.clone(),
            generic_params,
            methods,
            associated_types,
            supertraits,
            span: t.span,
        };

        let _ = self.traits.define(trait_def);
    }

    /// Collect an impl block.
    fn collect_impl(&mut self, i: &ImplBlock) {
        let generic_params = self.collect_generics(i.generics.as_ref());

        // Push type scope for generic params
        self.types.push_scope();
        for gp in &generic_params {
            let _ = self.types.define(TypeDef {
                name: gp.name.clone(),
                ty: gp.ty,
                span: i.span,
                kind: TypeDefKind::GenericParam,
                is_public: true,
            });
        }

        let self_ty = self.resolve_ast_type(&i.self_ty);

        // Define Self as the implementing type
        let _ = self.types.define(TypeDef {
            name: "Self".into(),
            ty: self_ty,
            span: i.span,
            kind: TypeDefKind::TypeAlias,
            is_public: true,
        });

        // Collect trait name if this is a trait impl
        let trait_name = i.trait_.as_ref().and_then(|t| {
            if t.segments.len() == 1 {
                Some(t.segments[0].ident.name.clone())
            } else {
                None
            }
        });

        // Collect method signatures
        let methods: Vec<ImplMethodSig> = i.items.iter().filter_map(|item| {
            match item {
                ImplItem::Function(f) => {
                    let method_generics = self.collect_generics(f.generics.as_ref());

                    // Check if the first parameter is `self`
                    let has_receiver = f.params.first().map_or(false, |p| {
                        Self::is_self_param_static(p)
                    });

                    // Collect non-self parameters first
                    let non_self_params: Vec<_> = f.params.iter()
                        .filter(|p| !Self::is_self_param_static(p))
                        .collect();

                    // Resolve parameter types
                    let params: Vec<TypeId> = non_self_params.iter()
                        .map(|p| self.resolve_ast_type(&p.ty))
                        .collect();

                    let return_type = f.return_type.as_ref()
                        .map(|t| self.resolve_ast_type(t))
                        .unwrap_or(self.ctx.unit());

                    Some(ImplMethodSig {
                        name: f.name.name.clone(),
                        params,
                        return_type,
                        generic_params: method_generics,
                        has_receiver,
                        span: f.span,
                    })
                }
                _ => None,
            }
        }).collect();

        self.types.pop_scope();

        let impl_def = ImplDef {
            trait_name,
            self_ty,
            generic_params,
            methods,
            span: i.span,
        };

        self.traits.add_impl(impl_def);
    }

    /// Check if a parameter is a self parameter (static version for use in closures).
    fn is_self_param_static(param: &Parameter) -> bool {
        // Check if the pattern is just `self` or the type is `Self`
        match &param.pattern.kind {
            PatternKind::Binding { name, .. } => {
                name.name.as_ref() == "self"
            }
            _ => false,
        }
    }

    /// Resolve a method for a given type.
    /// Returns (params excluding self, return_type, has_receiver) if found.
    fn resolve_method(&self, ty: TypeId, method_name: &str) -> Option<(Vec<TypeId>, TypeId, bool)> {
        // First, try direct lookup by type
        if let Some((method, _)) = self.traits.lookup_method(ty, method_name) {
            return Some((method.params.clone(), method.return_type, method.has_receiver));
        }

        // If the type is a struct or enum, try to find it by name
        let type_name = match self.ctx.get(ty) {
            Ty::Struct(s) => Some(s.name.clone()),
            Ty::Enum(e) => Some(e.name.clone()),
            _ => None,
        };

        if let Some(name) = type_name {
            // Look through all impls to find one matching by type name
            for impl_def in self.traits.all_impls() {
                let impl_type_name = match self.ctx.get(impl_def.self_ty) {
                    Ty::Struct(s) => Some(s.name.clone()),
                    Ty::Enum(e) => Some(e.name.clone()),
                    _ => None,
                };

                if impl_type_name.as_ref() == Some(&name) {
                    for method in &impl_def.methods {
                        if method.name.as_ref() == method_name {
                            return Some((method.params.clone(), method.return_type, method.has_receiver));
                        }
                    }
                }
            }
        }

        None
    }

    /// Check an item (second pass).
    fn check_item(&mut self, item: &Item) {
        match item {
            Item::Function(func) => self.check_function(func),
            Item::Const(c) => self.check_const(c),
            Item::Trait(t) => self.check_trait(t),
            Item::Impl(i) => self.check_impl(i),
            _ => {}
        }
    }

    /// Check a trait definition.
    fn check_trait(&mut self, t: &TraitDef) {
        let generic_params = self.collect_generics(t.generics.as_ref());

        // Push type scope for generic params
        self.types.push_scope();

        // Define Self as a generic type within the trait
        let self_var = self.ctx.fresh_generic();
        let self_ty = self.ctx.generic(self_var);
        let _ = self.types.define(TypeDef {
            name: "Self".into(),
            ty: self_ty,
            span: t.span,
            kind: TypeDefKind::GenericParam,
            is_public: true,
        });

        for gp in &generic_params {
            let _ = self.types.define(TypeDef {
                name: gp.name.clone(),
                ty: gp.ty,
                span: t.span,
                kind: TypeDefKind::GenericParam,
                is_public: true,
            });
        }

        // Check default method implementations
        for item in &t.items {
            if let TraitItem::Function(f) = item {
                if let Some(body) = &f.default {
                    self.symbols.push_scope(ScopeKind::Function);

                    // Bind parameters
                    for param in &f.params {
                        let ty = self.resolve_ast_type(&param.ty);
                        self.bind_pattern(&param.pattern, ty, BindingKind::Parameter);
                    }

                    let return_type = f.return_type.as_ref()
                        .map(|t| self.resolve_ast_type(t))
                        .unwrap_or(self.ctx.unit());
                    self.current_return_type = Some(return_type);

                    let body_ty = self.check_block(body);
                    self.unify(body_ty, return_type, body.span);

                    self.current_return_type = None;
                    self.symbols.pop_scope();
                }
            }
        }

        self.types.pop_scope();
    }

    /// Check an impl block.
    fn check_impl(&mut self, i: &ImplBlock) {
        let generic_params = self.collect_generics(i.generics.as_ref());

        // Push type scope for generic params
        self.types.push_scope();
        for gp in &generic_params {
            let _ = self.types.define(TypeDef {
                name: gp.name.clone(),
                ty: gp.ty,
                span: i.span,
                kind: TypeDefKind::GenericParam,
                is_public: true,
            });
        }

        let self_ty = self.resolve_ast_type(&i.self_ty);

        // Define Self as the implementing type
        let _ = self.types.define(TypeDef {
            name: "Self".into(),
            ty: self_ty,
            span: i.span,
            kind: TypeDefKind::TypeAlias,
            is_public: true,
        });

        // If this is a trait impl, verify the trait exists and methods match
        if let Some(trait_path) = &i.trait_ {
            if trait_path.segments.len() == 1 {
                let trait_name = &trait_path.segments[0].ident.name;

                // Clone the trait definition to avoid borrow issues
                let trait_def = self.traits.lookup(trait_name).cloned();

                if let Some(trait_def) = trait_def {
                    // Check that all required methods are implemented
                    let impl_methods: Vec<_> = i.items.iter().filter_map(|item| {
                        match item {
                            ImplItem::Function(f) => Some(f.name.name.as_ref()),
                            _ => None,
                        }
                    }).collect();

                    for method in &trait_def.methods {
                        if !method.has_default && !impl_methods.contains(&method.name.as_ref()) {
                            self.error(TypeError::MissingTraitMethod {
                                trait_name: trait_name.to_string(),
                                method: method.name.to_string(),
                                span: i.span,
                            });
                        }
                    }
                } else {
                    self.error(TypeError::UndefinedTrait {
                        name: trait_name.to_string(),
                        span: trait_path.segments[0].ident.span,
                    });
                }
            }
        }

        // Check all methods in the impl
        for item in &i.items {
            match item {
                ImplItem::Function(f) => {
                    self.symbols.push_scope(ScopeKind::Function);

                    // Push type scope for method generics
                    let method_generics = self.collect_generics(f.generics.as_ref());
                    self.types.push_scope();
                    for gp in &method_generics {
                        let _ = self.types.define(TypeDef {
                            name: gp.name.clone(),
                            ty: gp.ty,
                            span: f.span,
                            kind: TypeDefKind::GenericParam,
                            is_public: true,
                        });
                    }

                    // Bind self as first parameter if it's a method
                    // (this is implicit - we'd need to check the receiver pattern)

                    // Bind parameters
                    for param in &f.params {
                        let ty = self.resolve_ast_type(&param.ty);
                        self.bind_pattern(&param.pattern, ty, BindingKind::Parameter);
                    }

                    let return_type = f.return_type.as_ref()
                        .map(|t| self.resolve_ast_type(t))
                        .unwrap_or(self.ctx.unit());
                    self.current_return_type = Some(return_type);

                    let body_ty = self.check_block(&f.body);
                    self.unify(body_ty, return_type, f.body.span);

                    self.current_return_type = None;
                    self.types.pop_scope();
                    self.symbols.pop_scope();
                }
                ImplItem::Const(c) => {
                    let declared_ty = self.resolve_ast_type(&c.ty);
                    let value_ty = self.check_expr(&c.value);
                    self.unify(value_ty, declared_ty, c.value.span);
                }
                ImplItem::Type(_) => {
                    // Associated type - already handled in collect_impl
                }
            }
        }

        let _ = self_ty;
        self.types.pop_scope();
    }

    /// Check a function definition.
    fn check_function(&mut self, func: &FunctionDef) {
        self.symbols.push_scope(ScopeKind::Function);

        // Push type scope for generic params
        let generic_params = self.collect_generics(func.generics.as_ref());
        self.types.push_scope();
        for gp in &generic_params {
            let _ = self.types.define(TypeDef {
                name: gp.name.clone(),
                ty: gp.ty,
                span: func.span,
                kind: TypeDefKind::GenericParam,
                is_public: true,
            });
        }

        // Bind parameters
        for param in &func.params {
            let ty = self.resolve_ast_type(&param.ty);
            self.bind_pattern(&param.pattern, ty, BindingKind::Parameter);
        }

        // Set return type for checking returns
        let return_type = func.return_type.as_ref()
            .map(|t| self.resolve_ast_type(t))
            .unwrap_or(self.ctx.unit());
        self.current_return_type = Some(return_type);

        // Check body
        let body_ty = self.check_block(&func.body);

        // Unify body type with return type
        self.unify(body_ty, return_type, func.body.span);

        self.current_return_type = None;
        self.types.pop_scope();
        self.symbols.pop_scope();
    }

    /// Check a const definition.
    fn check_const(&mut self, c: &ConstDef) {
        let declared_ty = self.resolve_ast_type(&c.ty);
        let value_ty = self.check_expr(&c.value);
        self.unify(value_ty, declared_ty, c.value.span);
    }

    // ---
    // Statement checking
    // ---

    /// Check a block, returning its type.
    pub fn check_block(&mut self, block: &Block) -> TypeId {
        self.symbols.push_scope(ScopeKind::Block);

        let mut result_ty = self.ctx.unit();

        for (i, stmt) in block.stmts.iter().enumerate() {
            let is_last = i == block.stmts.len() - 1;

            match &stmt.kind {
                StmtKind::Let { pattern, ty, init } => {
                    let ty = ty.as_ref()
                        .map(|t| self.resolve_ast_type(t))
                        .unwrap_or_else(|| self.ctx.fresh_infer());

                    if let Some(init) = init {
                        let init_ty = self.check_expr(init);
                        self.unify(init_ty, ty, init.span);
                    }

                    self.bind_pattern(pattern, ty, BindingKind::Local);
                }

                StmtKind::Expr(expr) => {
                    self.check_expr(expr);
                }

                StmtKind::ExprNoSemi(expr) => {
                    result_ty = self.check_expr(expr);
                    if !is_last {
                        // Non-trailing expression without semicolon
                        result_ty = self.ctx.unit();
                    }
                }

                StmtKind::Item(item) => {
                    self.check_item(item);
                }

                StmtKind::Empty => {}
            }
        }

        self.symbols.pop_scope();
        result_ty
    }

    // ---
    // Expression checking
    // ---

    /// Check an expression, returning its type.
    pub fn check_expr(&mut self, expr: &Expr) -> TypeId {
        let ty = match &expr.kind {
            ExprKind::Literal(lit) => self.check_literal(lit),

            ExprKind::Path(path) => self.check_path(path, expr.span),

            ExprKind::Unary { op, operand } => {
                let operand_ty = self.check_expr(operand);
                match op {
                    UnaryOp::Neg => {
                        // Must be numeric
                        let resolved = self.resolve(operand_ty);
                        if !matches!(self.ctx.get(resolved), ty if ty.is_numeric()) {
                            // For inference, default to i64
                            if self.ctx.is_infer(resolved) {
                                self.unify(operand_ty, self.ctx.i64(), operand.span);
                            }
                        }
                        operand_ty
                    }
                    UnaryOp::Not => {
                        self.unify(operand_ty, self.ctx.bool(), operand.span);
                        self.ctx.bool()
                    }
                    UnaryOp::BitNot => operand_ty,
                }
            }

            ExprKind::Binary { op, left, right } => {
                let left_ty = self.check_expr(left);
                let right_ty = self.check_expr(right);

                match op {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Rem => {
                        self.unify(left_ty, right_ty, expr.span);
                        left_ty
                    }
                    BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge => {
                        self.unify(left_ty, right_ty, expr.span);
                        self.ctx.bool()
                    }
                    BinaryOp::And | BinaryOp::Or => {
                        self.unify(left_ty, self.ctx.bool(), left.span);
                        self.unify(right_ty, self.ctx.bool(), right.span);
                        self.ctx.bool()
                    }
                    BinaryOp::BitAnd | BinaryOp::BitOr | BinaryOp::BitXor | BinaryOp::Shl | BinaryOp::Shr => {
                        self.unify(left_ty, right_ty, expr.span);
                        left_ty
                    }
                }
            }

            ExprKind::Assign { target, value } => {
                let target_ty = self.check_expr(target);
                let value_ty = self.check_expr(value);
                self.unify(value_ty, target_ty, value.span);

                // Check mutability
                if let ExprKind::Path(path) = &target.kind {
                    if let Some(name) = path.segments.first().map(|s| s.ident.name.as_ref()) {
                        if let Some(binding) = self.symbols.lookup(name) {
                            if !binding.mutable {
                                self.error(TypeError::ImmutableAssignment {
                                    name: name.to_string(),
                                    span: target.span,
                                });
                            }
                        }
                    }
                }

                self.ctx.unit()
            }

            ExprKind::AssignOp { op: _, target, value } => {
                let target_ty = self.check_expr(target);
                let value_ty = self.check_expr(value);
                self.unify(value_ty, target_ty, value.span);
                self.ctx.unit()
            }

            ExprKind::Call { callee, args } => {
                let callee_ty = self.check_expr(callee);
                let resolved = self.resolve(callee_ty);

                match self.ctx.get(resolved).clone() {
                    Ty::Function { params, return_type } => {
                        if args.len() != params.len() {
                            self.error(TypeError::ArgCountMismatch {
                                expected: params.len(),
                                found: args.len(),
                                span: expr.span,
                            });
                        }

                        for (arg, &param_ty) in args.iter().zip(params.iter()) {
                            let arg_ty = self.check_expr(arg);
                            self.unify(arg_ty, param_ty, arg.span);
                        }

                        return_type
                    }
                    Ty::Infer(_) => {
                        // Create function type from args
                        let arg_tys: Vec<_> = args.iter().map(|a| self.check_expr(a)).collect();
                        let ret_ty = self.ctx.fresh_infer();
                        let fn_ty = self.ctx.function(arg_tys, ret_ty);
                        self.unify(callee_ty, fn_ty, callee.span);
                        ret_ty
                    }
                    _ => {
                        self.error(TypeError::NotCallable {
                            ty: self.ctx.display(callee_ty),
                            span: callee.span,
                        });
                        self.ctx.error()
                    }
                }
            }

            ExprKind::MethodCall { receiver, method, args } => {
                let receiver_ty = self.check_expr(receiver);
                let resolved_receiver = self.resolve(receiver_ty);

                // Try to find the method in impl blocks for this type
                let method_info = self.resolve_method(resolved_receiver, &method.name);

                match method_info {
                    Some((params, return_type, has_receiver)) => {
                        if !has_receiver {
                            self.error(TypeError::Custom(
                                format!("method `{}` does not take a receiver", method.name),
                                method.span,
                            ));
                        }

                        // Check argument count
                        if args.len() != params.len() {
                            self.error(TypeError::ArgCountMismatch {
                                expected: params.len(),
                                found: args.len(),
                                span: expr.span,
                            });
                        }

                        // Check argument types
                        for (arg, &param_ty) in args.iter().zip(params.iter()) {
                            let arg_ty = self.check_expr(arg);
                            self.unify(arg_ty, param_ty, arg.span);
                        }

                        return_type
                    }
                    None => {
                        // No method found, check args anyway and return error
                        for arg in args {
                            self.check_expr(arg);
                        }
                        self.error(TypeError::Custom(
                            format!("no method named `{}` found for type `{}`",
                                    method.name, self.ctx.display(receiver_ty)),
                            method.span,
                        ));
                        self.ctx.error()
                    }
                }
            }

            ExprKind::Field { base, field } => {
                let base_ty = self.check_expr(base);
                let resolved = self.resolve(base_ty);

                match self.ctx.get(resolved).clone() {
                    Ty::Tuple(elems) => {
                        // Tuple field access: tuple.0, tuple.1, etc.
                        if let Ok(idx) = field.name.parse::<usize>() {
                            if idx < elems.len() {
                                return elems[idx];
                            }
                        }
                        self.error(TypeError::NoSuchField {
                            ty: self.ctx.display(base_ty),
                            field: field.name.to_string(),
                            span: field.span,
                        });
                        self.ctx.error()
                    }
                    Ty::Struct(s) => {
                        for f in &s.fields {
                            if f.name == field.name {
                                return f.ty;
                            }
                        }
                        self.error(TypeError::NoSuchField {
                            ty: self.ctx.display(base_ty),
                            field: field.name.to_string(),
                            span: field.span,
                        });
                        self.ctx.error()
                    }
                    _ => {
                        self.error(TypeError::NoSuchField {
                            ty: self.ctx.display(base_ty),
                            field: field.name.to_string(),
                            span: field.span,
                        });
                        self.ctx.error()
                    }
                }
            }

            ExprKind::Index { base, index } => {
                let base_ty = self.check_expr(base);
                let index_ty = self.check_expr(index);
                let resolved = self.resolve(base_ty);

                match self.ctx.get(resolved).clone() {
                    Ty::Array { element, .. } | Ty::Slice(element) => {
                        self.unify(index_ty, self.ctx.usize(), index.span);
                        element
                    }
                    _ => {
                        self.error(TypeError::NotIndexable {
                            ty: self.ctx.display(base_ty),
                            span: base.span,
                        });
                        self.ctx.error()
                    }
                }
            }

            ExprKind::If { condition, then_branch, else_branch } => {
                let cond_ty = self.check_expr(condition);
                self.unify(cond_ty, self.ctx.bool(), condition.span);

                let then_ty = self.check_block(then_branch);

                if let Some(else_expr) = else_branch {
                    let else_ty = self.check_expr(else_expr);
                    self.unify(then_ty, else_ty, else_expr.span);
                    then_ty
                } else {
                    self.unify(then_ty, self.ctx.unit(), then_branch.span);
                    self.ctx.unit()
                }
            }

            ExprKind::Match { scrutinee, arms } => {
                let scrutinee_ty = self.check_expr(scrutinee);
                let result_ty = self.ctx.fresh_infer();

                for arm in arms {
                    self.symbols.push_scope(ScopeKind::Block);

                    // Check pattern and bind variables
                    let pattern_ty = self.check_pattern(&arm.pattern, scrutinee_ty);
                    self.unify(pattern_ty, scrutinee_ty, arm.pattern.span);

                    // Check guard
                    if let Some(guard) = &arm.guard {
                        let guard_ty = self.check_expr(guard);
                        self.unify(guard_ty, self.ctx.bool(), guard.span);
                    }

                    // Check body
                    let body_ty = self.check_expr(&arm.body);
                    self.unify(body_ty, result_ty, arm.body.span);

                    self.symbols.pop_scope();
                }

                result_ty
            }

            ExprKind::Loop { body, .. } => {
                self.symbols.push_scope(ScopeKind::Loop);
                self.check_block(body);
                self.symbols.pop_scope();
                // Loop expressions have type ! unless they break with a value
                self.ctx.never()
            }

            ExprKind::While { condition, body, .. } => {
                let cond_ty = self.check_expr(condition);
                self.unify(cond_ty, self.ctx.bool(), condition.span);

                self.symbols.push_scope(ScopeKind::Loop);
                self.check_block(body);
                self.symbols.pop_scope();

                self.ctx.unit()
            }

            ExprKind::For { pattern, iterable, body, .. } => {
                let iter_ty = self.check_expr(iterable);
                let elem_ty = self.ctx.fresh_infer();

                self.symbols.push_scope(ScopeKind::Loop);
                self.bind_pattern(pattern, elem_ty, BindingKind::LoopVar);
                self.check_block(body);
                self.symbols.pop_scope();

                let _ = iter_ty;
                self.ctx.unit()
            }

            ExprKind::Break { value, .. } => {
                if !self.symbols.in_loop() {
                    self.error(TypeError::BreakOutsideLoop { span: expr.span });
                }
                if let Some(value) = value {
                    self.check_expr(value);
                }
                self.ctx.never()
            }

            ExprKind::Continue { .. } => {
                if !self.symbols.in_loop() {
                    self.error(TypeError::ContinueOutsideLoop { span: expr.span });
                }
                self.ctx.never()
            }

            ExprKind::Return { value } => {
                if !self.symbols.in_function() {
                    self.error(TypeError::ReturnOutsideFunction { span: expr.span });
                }

                let return_ty = value.as_ref()
                    .map(|v| self.check_expr(v))
                    .unwrap_or(self.ctx.unit());

                if let Some(expected) = self.current_return_type {
                    self.unify(return_ty, expected, expr.span);
                }

                self.ctx.never()
            }

            ExprKind::Block(block) => self.check_block(block),

            ExprKind::Tuple(elems) => {
                let elem_tys: Vec<_> = elems.iter().map(|e| self.check_expr(e)).collect();
                self.ctx.tuple(elem_tys)
            }

            ExprKind::Array(elems) => {
                if elems.is_empty() {
                    let elem_ty = self.ctx.fresh_infer();
                    self.ctx.array(elem_ty, 0)
                } else {
                    let first_ty = self.check_expr(&elems[0]);
                    for elem in &elems[1..] {
                        let elem_ty = self.check_expr(elem);
                        self.unify(elem_ty, first_ty, elem.span);
                    }
                    self.ctx.array(first_ty, elems.len())
                }
            }

            ExprKind::ArrayRepeat { value, count } => {
                let elem_ty = self.check_expr(value);
                let count_ty = self.check_expr(count);
                self.unify(count_ty, self.ctx.usize(), count.span);
                self.ctx.array(elem_ty, 0)
            }

            ExprKind::Reference { mutable, operand } => {
                let inner_ty = self.check_expr(operand);
                self.ctx.reference(inner_ty, *mutable)
            }

            ExprKind::Dereference { operand } => {
                let operand_ty = self.check_expr(operand);
                let resolved = self.resolve(operand_ty);

                match self.ctx.get(resolved).clone() {
                    Ty::Reference { inner, .. } => inner,
                    Ty::Infer(_) => {
                        let inner = self.ctx.fresh_infer();
                        let ref_ty = self.ctx.reference(inner, false);
                        self.unify(operand_ty, ref_ty, operand.span);
                        inner
                    }
                    _ => {
                        self.error(TypeError::Custom(
                            format!("cannot dereference type `{}`", self.ctx.display(operand_ty)),
                            operand.span,
                        ));
                        self.ctx.error()
                    }
                }
            }

            ExprKind::Closure { params, return_type, body } => {
                self.symbols.push_scope(ScopeKind::Closure);

                let param_tys: Vec<_> = params.iter().map(|p| {
                    let ty = p.ty.as_ref()
                        .map(|t| self.resolve_ast_type(t))
                        .unwrap_or_else(|| self.ctx.fresh_infer());
                    self.bind_pattern(&p.pattern, ty, BindingKind::Parameter);
                    ty
                }).collect();

                let expected_ret = return_type.as_ref()
                    .map(|t| self.resolve_ast_type(t))
                    .unwrap_or_else(|| self.ctx.fresh_infer());

                let body_ty = self.check_expr(body);
                self.unify(body_ty, expected_ret, body.span);

                self.symbols.pop_scope();

                self.ctx.function(param_tys, expected_ret)
            }

            ExprKind::Cast { expr: inner, ty } => {
                self.check_expr(inner);
                self.resolve_ast_type(ty)
            }

            ExprKind::Range { start, end, .. } => {
                if let Some(start) = start {
                    self.check_expr(start);
                }
                if let Some(end) = end {
                    self.check_expr(end);
                }
                self.ctx.fresh_infer()
            }

            ExprKind::Try { operand } => {
                self.check_expr(operand);
                self.ctx.fresh_infer()
            }

            ExprKind::Struct { path, fields, rest } => {
                // Resolve the struct type from the path
                let struct_ty = if path.segments.len() == 1 {
                    let name = &path.segments[0].ident.name;
                    if let Some(def) = self.types.lookup(name) {
                        def.ty
                    } else {
                        self.error(TypeError::UndefinedType {
                            name: name.to_string(),
                            span: path.segments[0].ident.span,
                        });
                        self.ctx.error()
                    }
                } else {
                    self.ctx.fresh_infer()
                };

                // Check field expressions
                for field in fields {
                    if let Some(value) = &field.value {
                        let value_ty = self.check_expr(value);

                        // If we have a valid struct type, check field types
                        if let Ty::Struct(s) = self.ctx.get(struct_ty).clone() {
                            if let Some(field_def) = s.fields.iter().find(|f| f.name == field.name.name) {
                                self.unify(value_ty, field_def.ty, value.span);
                            }
                        }
                    }
                }

                if let Some(rest) = rest {
                    let rest_ty = self.check_expr(rest);
                    self.unify(rest_ty, struct_ty, rest.span);
                }

                struct_ty
            }

            ExprKind::Grouped(inner) => self.check_expr(inner),
        };
        // Record the expression type for compiler optimization
        self.record_expr_type(expr.span, ty);
        ty
    }

    /// Check a literal, returning its type.
    fn check_literal(&mut self, lit: &Literal) -> TypeId {
        match lit {
            Literal::Int(_) => self.ctx.i64(),
            Literal::Float(_) => self.ctx.f64(),
            Literal::String(_) => self.ctx.string(),
            Literal::Char(_) => self.ctx.char(),
            Literal::Bool(_) => self.ctx.bool(),
            Literal::Unit => self.ctx.unit(),
        }
    }

    /// Check a path expression.
    fn check_path(&mut self, path: &ExprPath, span: Span) -> TypeId {
        if path.segments.len() == 1 {
            let segment = &path.segments[0];
            let name = &segment.ident.name;

            // Check for variable
            if let Some(binding) = self.symbols.lookup(name) {
                return binding.ty;
            }

            // Check for function
            if let Some(sig) = self.functions.lookup(name).cloned() {
                // Handle generic function instantiation
                if !sig.generic_params.is_empty() {
                    // Check if explicit type arguments are provided
                    if let Some(args) = &segment.args {
                        if args.args.len() != sig.generic_params.len() {
                            self.error(TypeError::TypeArgCountMismatch {
                                expected: sig.generic_params.len(),
                                found: args.args.len(),
                                span,
                            });
                            return self.ctx.error();
                        }

                        // Resolve explicit type arguments
                        let type_args: Vec<TypeId> = args.args.iter().map(|arg| {
                            match arg {
                                GenericArg::Type(t) => self.resolve_ast_type(t),
                                GenericArg::Const(_) => self.ctx.fresh_infer(),
                            }
                        }).collect();

                        // Verify trait bounds on explicit type arguments
                        self.verify_trait_bounds(&sig.generic_params, &type_args, span);

                        // Create substitutions
                        let substitutions: Vec<(GenericVar, TypeId)> = sig.generic_params
                            .iter()
                            .zip(type_args.iter())
                            .map(|(gp, &arg)| (gp.var, arg))
                            .collect();

                        // Substitute in params and return type
                        let params: Vec<TypeId> = sig.params
                            .iter()
                            .map(|&p| self.ctx.substitute(p, &substitutions))
                            .collect();
                        let return_type = self.ctx.substitute(sig.return_type, &substitutions);

                        return self.ctx.function(params, return_type);
                    } else {
                        // No explicit type args - create fresh inference vars for each generic param
                        // The actual types will be inferred from usage
                        let infer_args: Vec<TypeId> = sig.generic_params
                            .iter()
                            .map(|_| self.ctx.fresh_infer())
                            .collect();

                        let substitutions: Vec<(GenericVar, TypeId)> = sig.generic_params
                            .iter()
                            .zip(infer_args.iter())
                            .map(|(gp, &arg)| (gp.var, arg))
                            .collect();

                        let params: Vec<TypeId> = sig.params
                            .iter()
                            .map(|&p| self.ctx.substitute(p, &substitutions))
                            .collect();
                        let return_type = self.ctx.substitute(sig.return_type, &substitutions);

                        return self.ctx.function(params, return_type);
                    }
                }
                return self.ctx.function(sig.params.clone(), sig.return_type);
            }

            self.error(TypeError::UndefinedVariable {
                name: name.to_string(),
                span,
            });
            self.ctx.error()
        } else {
            // Multi-segment paths (e.g., Foo::bar)
            self.ctx.fresh_infer()
        }
    }

    // ---
    // Pattern checking
    // ---

    /// Check a pattern, returning its type.
    fn check_pattern(&mut self, pattern: &Pattern, expected: TypeId) -> TypeId {
        match &pattern.kind {
            PatternKind::Wildcard => expected,

            PatternKind::Binding { name, mutable, .. } => {
                let _ = self.symbols.define(Binding {
                    name: name.name.clone(),
                    ty: expected,
                    mutable: *mutable,
                    span: pattern.span,
                    kind: BindingKind::Local,
                });
                expected
            }

            PatternKind::Literal(lit) => self.check_literal(lit),

            PatternKind::Tuple(patterns) => {
                let resolved = self.resolve(expected);
                match self.ctx.get(resolved).clone() {
                    Ty::Tuple(elems) if elems.len() == patterns.len() => {
                        for (pat, &elem_ty) in patterns.iter().zip(elems.iter()) {
                            self.check_pattern(pat, elem_ty);
                        }
                        expected
                    }
                    _ => {
                        let elem_tys: Vec<_> = patterns.iter()
                            .map(|p| {
                                let ty = self.ctx.fresh_infer();
                                self.check_pattern(p, ty);
                                ty
                            })
                            .collect();
                        self.ctx.tuple(elem_tys)
                    }
                }
            }

            PatternKind::Or(patterns) => {
                for pat in patterns {
                    self.check_pattern(pat, expected);
                }
                expected
            }

            PatternKind::Rest => expected,

            PatternKind::Path(_) => expected,

            PatternKind::TupleStruct { .. } => expected,

            PatternKind::Struct { .. } => expected,

            PatternKind::Slice(patterns) => {
                let elem_ty = match self.ctx.get(self.resolve(expected)).clone() {
                    Ty::Array { element, .. } | Ty::Slice(element) => element,
                    _ => self.ctx.fresh_infer(),
                };
                for pat in patterns {
                    self.check_pattern(pat, elem_ty);
                }
                expected
            }

            PatternKind::Reference { mutable, pattern: inner } => {
                let inner_ty = match self.ctx.get(self.resolve(expected)).clone() {
                    Ty::Reference { inner, .. } => inner,
                    _ => self.ctx.fresh_infer(),
                };
                self.check_pattern(inner, inner_ty);
                self.ctx.reference(inner_ty, *mutable)
            }

            PatternKind::Range { .. } => expected,
        }
    }

    /// Bind a pattern to a type.
    fn bind_pattern(&mut self, pattern: &Pattern, ty: TypeId, kind: BindingKind) {
        match &pattern.kind {
            PatternKind::Wildcard => {}

            PatternKind::Binding { name, mutable, subpattern, .. } => {
                let _ = self.symbols.define(Binding {
                    name: name.name.clone(),
                    ty,
                    mutable: *mutable,
                    span: pattern.span,
                    kind,
                });

                if let Some(sub) = subpattern {
                    self.bind_pattern(sub, ty, kind);
                }
            }

            PatternKind::Tuple(patterns) => {
                let resolved = self.resolve(ty);
                if let Ty::Tuple(elems) = self.ctx.get(resolved).clone() {
                    for (pat, &elem_ty) in patterns.iter().zip(elems.iter()) {
                        self.bind_pattern(pat, elem_ty, kind);
                    }
                }
            }

            PatternKind::Or(patterns) => {
                // All alternatives must bind the same variables
                for pat in patterns {
                    self.bind_pattern(pat, ty, kind);
                }
            }

            PatternKind::Struct { fields, .. } => {
                for field in fields {
                    if let Some(pat) = &field.pattern {
                        let field_ty = self.ctx.fresh_infer();
                        self.bind_pattern(pat, field_ty, kind);
                    }
                }
            }

            PatternKind::TupleStruct { fields, .. } => {
                for (i, pat) in fields.iter().enumerate() {
                    let field_ty = self.ctx.fresh_infer();
                    let _ = i;
                    self.bind_pattern(pat, field_ty, kind);
                }
            }

            PatternKind::Slice(patterns) => {
                let elem_ty = match self.ctx.get(self.resolve(ty)).clone() {
                    Ty::Array { element, .. } | Ty::Slice(element) => element,
                    _ => self.ctx.fresh_infer(),
                };
                for pat in patterns {
                    self.bind_pattern(pat, elem_ty, kind);
                }
            }

            _ => {}
        }
    }

    // ---
    // Type resolution
    // ---

    /// Resolve an AST type to a TypeId.
    pub fn resolve_ast_type(&mut self, ty: &Type) -> TypeId {
        match &ty.kind {
            TypeKind::Path(path) => {
                if path.segments.len() == 1 {
                    let segment = &path.segments[0];
                    let name = &segment.ident.name;

                    // Look up the type and clone necessary data to avoid borrow issues
                    let type_info = self.types.lookup(name).map(|def| {
                        let generic_params = match self.ctx.get(def.ty).clone() {
                            Ty::Struct(s) => s.generic_params.clone(),
                            Ty::Enum(e) => e.generic_params.clone(),
                            _ => Vec::new(),
                        };
                        (def.ty, generic_params)
                    });

                    if let Some((base_ty, generic_params)) = type_info {
                        // Check for generic arguments
                        if let Some(args) = &segment.args {
                            if args.args.len() != generic_params.len() {
                                self.error(TypeError::TypeArgCountMismatch {
                                    expected: generic_params.len(),
                                    found: args.args.len(),
                                    span: ty.span,
                                });
                                return self.ctx.error();
                            }

                            // Resolve each type argument
                            let type_args: Vec<TypeId> = args.args.iter().map(|arg| {
                                match arg {
                                    GenericArg::Type(t) => self.resolve_ast_type(t),
                                    GenericArg::Const(_) => self.ctx.fresh_infer(), // Const generics not fully supported
                                }
                            }).collect();

                            // Create substitutions and apply them
                            let substitutions: Vec<(GenericVar, TypeId)> = generic_params
                                .iter()
                                .zip(type_args.iter())
                                .map(|(gp, &arg)| (gp.var, arg))
                                .collect();

                            // Substitute generic variables in the base type
                            return self.ctx.substitute(base_ty, &substitutions);
                        }
                        return base_ty;
                    }

                    self.error(TypeError::UndefinedType {
                        name: name.to_string(),
                        span: ty.span,
                    });
                    self.ctx.error()
                } else {
                    // Multi-segment paths (e.g., std::vec::Vec)
                    self.ctx.fresh_infer()
                }
            }

            TypeKind::Reference { mutable, inner } => {
                let inner_ty = self.resolve_ast_type(inner);
                self.ctx.reference(inner_ty, *mutable)
            }

            TypeKind::Array { element, size: _ } => {
                let elem_ty = self.resolve_ast_type(element);
                self.ctx.array(elem_ty, 0)
            }

            TypeKind::Slice { element } => {
                let elem_ty = self.resolve_ast_type(element);
                self.ctx.slice(elem_ty)
            }

            TypeKind::Tuple(elems) => {
                let elem_tys: Vec<_> = elems.iter().map(|e| self.resolve_ast_type(e)).collect();
                self.ctx.tuple(elem_tys)
            }

            TypeKind::Function { params, return_type } => {
                let param_tys: Vec<_> = params.iter().map(|p| self.resolve_ast_type(p)).collect();
                let ret_ty = return_type.as_ref()
                    .map(|r| self.resolve_ast_type(r))
                    .unwrap_or(self.ctx.unit());
                self.ctx.function(param_tys, ret_ty)
            }

            TypeKind::Infer => self.ctx.fresh_infer(),
            TypeKind::Never => self.ctx.never(),
            TypeKind::Unit => self.ctx.unit(),
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use squam_parser::Parser;

    fn check(source: &str) -> (TypeChecker, Vec<TypeError>) {
        let mut parser = Parser::new(source, 0);
        let module = parser.parse_module();
        assert!(parser.errors().is_empty(), "Parse errors: {:?}", parser.errors());

        let mut checker = TypeChecker::new();
        checker.check_module(&module);
        let errors = checker.take_errors();
        (checker, errors)
    }

    #[test]
    fn test_literal_types() {
        let (checker, errors) = check("fn main() { let x = 42; let y = 3.14; let z = true; }");
        assert!(errors.is_empty(), "Errors: {:?}", errors);
        let _ = checker;
    }

    #[test]
    fn test_type_mismatch() {
        let (_, errors) = check("fn main() { let x: bool = 42; }");
        assert!(!errors.is_empty());
        assert!(matches!(&errors[0], TypeError::Mismatch { .. }));
    }

    #[test]
    fn test_undefined_variable() {
        let (_, errors) = check("fn main() { let x = y; }");
        assert!(!errors.is_empty());
        assert!(matches!(&errors[0], TypeError::UndefinedVariable { .. }));
    }

    #[test]
    fn test_function_call() {
        let (_, errors) = check(r#"
            fn add(a: i64, b: i64) -> i64 { a + b }
            fn main() { let x = add(1, 2); }
        "#);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }

    #[test]
    fn test_return_type_mismatch() {
        let (_, errors) = check("fn foo() -> bool { 42 }");
        assert!(!errors.is_empty());
    }

    #[test]
    fn test_if_expression() {
        let (_, errors) = check("fn main() { let x = if true { 1 } else { 2 }; }");
        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }

    #[test]
    fn test_if_branch_mismatch() {
        let (_, errors) = check("fn main() { let x = if true { 1 } else { true }; }");
        assert!(!errors.is_empty());
    }

    #[test]
    fn test_generic_function() {
        let (_, errors) = check(r#"
            fn identity<T>(x: T) -> T { x }
            fn main() { let x = identity(42); }
        "#);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }

    #[test]
    fn test_generic_struct() {
        let (_, errors) = check(r#"
            struct Pair<T, U> { first: T, second: U }
            fn main() { }
        "#);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }

    #[test]
    fn test_generic_enum() {
        let (_, errors) = check(r#"
            enum Option<T> { Some(T), None }
            fn main() { }
        "#);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }

    #[test]
    fn test_generic_multiple_params() {
        let (_, errors) = check(r#"
            fn swap<A, B>(a: A, b: B) -> (B, A) { (b, a) }
            fn main() { let x = swap(1, true); }
        "#);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }

    #[test]
    fn test_trait_definition() {
        let (checker, errors) = check(r#"
            trait Display {
                fn display(self) -> String;
            }
        "#);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
        // Verify the trait was registered
        assert!(checker.traits.lookup("Display").is_some());
    }

    #[test]
    fn test_trait_with_default_method() {
        let (checker, errors) = check(r#"
            trait Greet {
                fn greet() -> String {
                    "Hello"
                }
            }
        "#);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
        let trait_def = checker.traits.lookup("Greet").unwrap();
        assert_eq!(trait_def.methods.len(), 1);
        assert!(trait_def.methods[0].has_default);
    }

    #[test]
    fn test_impl_block() {
        let (_, errors) = check(r#"
            struct Point { x: i64, y: i64 }

            impl Point {
                fn new(x: i64, y: i64) -> Point {
                    Point { x: x, y: y }
                }
            }
        "#);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }

    #[test]
    fn test_trait_impl() {
        let (_, errors) = check(r#"
            trait Display {
                fn display(self) -> String;
            }

            struct Point { x: i64, y: i64 }

            impl Display for Point {
                fn display(self) -> String {
                    "Point"
                }
            }
        "#);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }

    #[test]
    fn test_trait_impl_missing_method() {
        let (_, errors) = check(r#"
            trait Display {
                fn display(self) -> String;
            }

            struct Point { x: i64, y: i64 }

            impl Display for Point {
                // Missing display method
            }
        "#);
        assert!(!errors.is_empty());
        assert!(matches!(&errors[0], TypeError::MissingTraitMethod { .. }));
    }

    #[test]
    fn test_undefined_trait_impl() {
        let (_, errors) = check(r#"
            struct Point { x: i64, y: i64 }

            impl UnknownTrait for Point {
                fn foo() { }
            }
        "#);
        assert!(!errors.is_empty());
        assert!(matches!(&errors[0], TypeError::UndefinedTrait { .. }));
    }

    #[test]
    fn test_generic_with_trait_bound() {
        let (_, errors) = check(r#"
            trait Display {
                fn display(self) -> String;
            }

            fn print_it<T: Display>(x: T) -> T {
                // Method calls on generic types with trait bounds
                // would require looking up methods from the trait.
                // For now, just verify trait bounds are collected.
                x
            }
        "#);
        // This should parse and type-check without errors
        // Full method resolution on trait-bounded generics is future work
        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }

    #[test]
    fn test_supertrait() {
        let (checker, errors) = check(r#"
            trait Base {
                fn base() -> i64;
            }

            trait Derived: Base {
                fn derived() -> i64;
            }
        "#);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
        let derived = checker.traits.lookup("Derived").unwrap();
        assert_eq!(derived.supertraits.len(), 1);
        assert_eq!(derived.supertraits[0].as_ref(), "Base");
    }

    #[test]
    fn test_method_call_basic() {
        let (_, errors) = check(r#"
            struct Counter { value: i64 }

            impl Counter {
                fn increment(self) -> i64 {
                    self.value + 1
                }
            }

            fn main() {
                let c = Counter { value: 0 };
                let x = c.increment();
            }
        "#);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }

    #[test]
    fn test_method_call_with_args() {
        let (_, errors) = check(r#"
            struct Calculator { base: i64 }

            impl Calculator {
                fn add(self, n: i64) -> i64 {
                    self.base + n
                }
            }

            fn main() {
                let calc = Calculator { base: 10 };
                let result = calc.add(5);
            }
        "#);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }

    #[test]
    fn test_method_call_multiple_args() {
        let (_, errors) = check(r#"
            struct Point { x: i64, y: i64 }

            impl Point {
                fn translate(self, dx: i64, dy: i64) -> Point {
                    Point { x: self.x + dx, y: self.y + dy }
                }
            }

            fn main() {
                let p = Point { x: 0, y: 0 };
                let p2 = p.translate(10, 20);
            }
        "#);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }

    #[test]
    fn test_method_not_found() {
        let (_, errors) = check(r#"
            struct Point { x: i64, y: i64 }

            fn main() {
                let p = Point { x: 0, y: 0 };
                p.unknown_method();
            }
        "#);
        assert!(!errors.is_empty());
        // Should have an error about no method found
        let error_msg = format!("{:?}", errors[0]);
        assert!(error_msg.contains("no method") || error_msg.contains("unknown_method"));
    }

    #[test]
    fn test_method_wrong_arg_count() {
        let (_, errors) = check(r#"
            struct Counter { value: i64 }

            impl Counter {
                fn add(self, n: i64) -> i64 {
                    self.value + n
                }
            }

            fn main() {
                let c = Counter { value: 0 };
                c.add(1, 2, 3);
            }
        "#);
        assert!(!errors.is_empty());
        assert!(matches!(&errors[0], TypeError::ArgCountMismatch { .. }));
    }

    #[test]
    fn test_static_method() {
        let (_, errors) = check(r#"
            struct Point { x: i64, y: i64 }

            impl Point {
                fn origin() -> Point {
                    Point { x: 0, y: 0 }
                }
            }
        "#);
        // Static methods in impl blocks should be collected
        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }

    #[test]
    fn test_method_return_type() {
        let (_, errors) = check(r#"
            struct Counter { value: i64 }

            impl Counter {
                fn get_value(self) -> i64 {
                    self.value
                }
            }

            fn main() {
                let c = Counter { value: 42 };
                let x: i64 = c.get_value();
            }
        "#);
        assert!(errors.is_empty(), "Errors: {:?}", errors);
    }
}