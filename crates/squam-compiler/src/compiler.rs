use crate::bytecode::{Chunk, Constant, FunctionProto, OpCode, UpvalueInfo};
use squam_lexer::Span;
use squam_parser::ast::*;
use squam_types::TypeAnnotations;
use std::collections::HashMap;

/// Compilation errors.
#[derive(Debug, Clone, thiserror::Error)]
pub enum CompileError {
    #[error("too many constants in one chunk")]
    TooManyConstants,
    #[error("too many local variables")]
    TooManyLocals,
    #[error("jump too large")]
    JumpTooLarge,
    #[error("variable '{0}' not found")]
    UndefinedVariable(String),
    #[error("cannot borrow `{0}` as mutable because it is already borrowed as immutable")]
    AlreadyBorrowedImmut(String),
    #[error("cannot borrow `{0}` as immutable because it is already borrowed as mutable")]
    AlreadyBorrowedMut(String),
    #[error("cannot borrow `{0}` as mutable more than once")]
    DoubleMutBorrow(String),
    #[error("cannot use `{0}` because it is mutably borrowed")]
    UsedWhileMutBorrowed(String),
    #[error("cannot mutate `{0}` because it is borrowed")]
    MutateWhileBorrowed(String),
    #[error("{0}")]
    Custom(String),
}

/// Borrow state for a local variable
#[derive(Debug, Clone, Copy, PartialEq)]
enum BorrowState {
    /// Variable is not borrowed
    Unborrowed,
    /// Variable has active immutable borrows (count of borrows)
    ImmutablyBorrowed(u32),
    /// Variable is mutably borrowed
    MutablyBorrowed,
}

/// A local variable during compilation.
#[derive(Debug, Clone)]
struct Local {
    name: String,
    depth: u32,
    is_captured: bool,
    mutable: bool,
    borrow_state: BorrowState,
}

/// Information about a loop for break/continue.
#[derive(Debug, Clone)]
struct LoopInfo {
    start: usize,
    break_jumps: Vec<usize>,
}

/// Struct type information for compilation.
#[derive(Debug, Clone)]
struct StructTypeInfo {
    /// Field names in declaration order
    fields: Vec<String>,
    /// Default values for fields (field name -> default expression)
    defaults: HashMap<String, Expr>,
}

/// Enum type information for compilation.
#[derive(Debug, Clone)]
struct EnumTypeInfo {
    /// Variant name -> (variant index, field count)
    variants: HashMap<String, (u8, usize)>,
    /// Variants in declaration order: (name, field_count)
    variants_ordered: Vec<(String, usize)>,
}

/// Trait method signature information.
#[derive(Debug, Clone)]
struct TraitMethodInfo {
    name: String,
    has_default: bool,
}

/// Trait type information for compilation.
#[derive(Debug, Clone)]
struct TraitTypeInfo {
    /// Required methods
    methods: Vec<TraitMethodInfo>,
}

/// The kind of generic definition.
#[derive(Debug, Clone)]
enum GenericDefKind {
    Function(FunctionDef),
    Struct(StructDef),
    Enum(EnumDef),
}

/// A stored generic definition awaiting monomorphization.
#[derive(Debug, Clone)]
struct GenericDef {
    /// What kind of generic this is
    kind: GenericDefKind,
    /// The generic parameter names (e.g., ["T", "U"])
    type_params: Vec<String>,
}

/// Compiler state for a single function.
struct CompilerState {
    chunk: Chunk,
    locals: Vec<Local>,
    upvalues: Vec<UpvalueInfo>,
    scope_depth: u32,
    loops: Vec<LoopInfo>,
    function_name: Option<String>,
    arity: u8,
}

impl CompilerState {
    fn new(name: Option<String>, arity: u8) -> Self {
        Self {
            chunk: Chunk::new(),
            locals: Vec::new(),
            upvalues: Vec::new(),
            scope_depth: 0,
            loops: Vec::new(),
            function_name: name,
            arity,
        }
    }
}

/// The Squam compiler.
pub struct Compiler {
    /// Stack of compiler states (for nested functions)
    states: Vec<CompilerState>,
    /// Current line number for debugging
    current_line: u32,
    /// Struct type definitions
    struct_types: HashMap<String, StructTypeInfo>,
    /// Enum type definitions
    enum_types: HashMap<String, EnumTypeInfo>,
    /// Trait type definitions
    trait_types: HashMap<String, TraitTypeInfo>,
    /// Type aliases: alias name -> underlying type name
    type_aliases: HashMap<String, String>,
    /// Current module path (for nested modules)
    current_module_path: Vec<String>,
    /// Module imports: short name -> full qualified path
    module_imports: HashMap<String, String>,
    /// Type annotations from type checking (for optimized codegen)
    type_annotations: Option<TypeAnnotations>,

    // === Monomorphization support ===
    /// Generic definitions awaiting monomorphization
    generic_defs: HashMap<String, GenericDef>,
    /// Already-compiled instantiations: mangled_key -> mangled_name
    compiled_instantiations: HashMap<String, String>,
    /// Current type substitution during monomorphization (T -> "i64")
    current_substitution: HashMap<String, String>,
    /// Counter for generating unique temporary variable names
    temp_counter: usize,
}

impl Compiler {
    /// Create a new compiler.
    pub fn new() -> Self {
        let mut compiler = Self {
            states: Vec::new(),
            current_line: 1,
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            trait_types: HashMap::new(),
            type_aliases: HashMap::new(),
            current_module_path: Vec::new(),
            module_imports: HashMap::new(),
            type_annotations: None,
            generic_defs: HashMap::new(),
            compiled_instantiations: HashMap::new(),
            current_substitution: HashMap::new(),
            temp_counter: 0,
        };
        compiler.register_builtin_enums();
        compiler
    }

    /// Create a new compiler with type annotations for optimized codegen.
    pub fn with_type_annotations(annotations: TypeAnnotations) -> Self {
        let mut compiler = Self {
            states: Vec::new(),
            current_line: 1,
            struct_types: HashMap::new(),
            enum_types: HashMap::new(),
            trait_types: HashMap::new(),
            type_aliases: HashMap::new(),
            current_module_path: Vec::new(),
            module_imports: HashMap::new(),
            type_annotations: Some(annotations),
            generic_defs: HashMap::new(),
            compiled_instantiations: HashMap::new(),
            current_substitution: HashMap::new(),
            temp_counter: 0,
        };
        compiler.register_builtin_enums();
        compiler
    }

    /// Register built-in generic enums (Option, Result) for monomorphization.
    fn register_builtin_enums(&mut self) {
        // Helper to make a type path for a generic param name
        fn make_type_path(name: &str) -> Type {
            Type {
                kind: TypeKind::Path(TypePath {
                    segments: vec![PathSegment {
                        ident: Identifier {
                            name: name.into(),
                            span: Span::dummy(),
                        },
                        args: None,
                    }],
                    span: Span::dummy(),
                }),
                span: Span::dummy(),
            }
        }

        // Option<T> = Some(T) | None
        let option_def = EnumDef {
            attributes: Vec::new(),
            visibility: Visibility::Public,
            name: Identifier {
                name: "Option".into(),
                span: Span::dummy(),
            },
            generics: Some(Generics {
                params: vec![GenericParam::Type(TypeParam {
                    name: Identifier {
                        name: "T".into(),
                        span: Span::dummy(),
                    },
                    bounds: Vec::new(),
                    default: None,
                    span: Span::dummy(),
                })],
                where_clause: None,
                span: Span::dummy(),
            }),
            variants: vec![
                EnumVariant {
                    name: Identifier {
                        name: "Some".into(),
                        span: Span::dummy(),
                    },
                    fields: StructFields::Tuple(vec![TupleField {
                        visibility: Visibility::Public,
                        ty: make_type_path("T"),
                        span: Span::dummy(),
                    }]),
                    discriminant: None,
                    span: Span::dummy(),
                },
                EnumVariant {
                    name: Identifier {
                        name: "None".into(),
                        span: Span::dummy(),
                    },
                    fields: StructFields::Unit,
                    discriminant: None,
                    span: Span::dummy(),
                },
            ],
            span: Span::dummy(),
        };
        self.generic_defs.insert(
            "Option".into(),
            GenericDef {
                kind: GenericDefKind::Enum(option_def),
                type_params: vec!["T".into()],
            },
        );

        // Result<T, E> = Ok(T) | Err(E)
        let result_def = EnumDef {
            attributes: Vec::new(),
            visibility: Visibility::Public,
            name: Identifier {
                name: "Result".into(),
                span: Span::dummy(),
            },
            generics: Some(Generics {
                params: vec![
                    GenericParam::Type(TypeParam {
                        name: Identifier {
                            name: "T".into(),
                            span: Span::dummy(),
                        },
                        bounds: Vec::new(),
                        default: None,
                        span: Span::dummy(),
                    }),
                    GenericParam::Type(TypeParam {
                        name: Identifier {
                            name: "E".into(),
                            span: Span::dummy(),
                        },
                        bounds: Vec::new(),
                        default: None,
                        span: Span::dummy(),
                    }),
                ],
                where_clause: None,
                span: Span::dummy(),
            }),
            variants: vec![
                EnumVariant {
                    name: Identifier {
                        name: "Ok".into(),
                        span: Span::dummy(),
                    },
                    fields: StructFields::Tuple(vec![TupleField {
                        visibility: Visibility::Public,
                        ty: make_type_path("T"),
                        span: Span::dummy(),
                    }]),
                    discriminant: None,
                    span: Span::dummy(),
                },
                EnumVariant {
                    name: Identifier {
                        name: "Err".into(),
                        span: Span::dummy(),
                    },
                    fields: StructFields::Tuple(vec![TupleField {
                        visibility: Visibility::Public,
                        ty: make_type_path("E"),
                        span: Span::dummy(),
                    }]),
                    discriminant: None,
                    span: Span::dummy(),
                },
            ],
            span: Span::dummy(),
        };
        self.generic_defs.insert(
            "Result".into(),
            GenericDef {
                kind: GenericDefKind::Enum(result_def),
                type_params: vec!["T".into(), "E".into()],
            },
        );
    }

    /// Set type annotations for optimized code generation.
    pub fn set_type_annotations(&mut self, annotations: TypeAnnotations) {
        self.type_annotations = Some(annotations);
    }

    /// Get the type of an expression at the given span, if known.
    fn get_expr_type(&self, span_start: u32) -> Option<squam_types::TypeId> {
        self.type_annotations.as_ref()?.get_expr_type(span_start)
    }

    /// Resolve a type name through aliases to get the underlying type name.
    fn resolve_type_alias(&self, name: &str) -> String {
        let mut current = name.to_string();
        let mut seen = std::collections::HashSet::new();

        while let Some(target) = self.type_aliases.get(&current) {
            if seen.contains(&current) {
                break; // Prevent infinite loops from circular aliases
            }
            seen.insert(current.clone());
            current = target.clone();
        }

        current
    }

    // === Monomorphization helpers ===

    /// Generate a mangled name for a generic instantiation.
    /// e.g., "identity" with ["i64"] -> "identity$i64"
    /// e.g., "Pair" with ["i64", "String"] -> "Pair$i64$String"
    fn mangle_name(&self, base_name: &str, type_args: &[String]) -> String {
        if type_args.is_empty() {
            base_name.to_string()
        } else {
            format!("{}${}", base_name, type_args.join("$"))
        }
    }

    /// Generate a unique key for an instantiation (for deduplication).
    fn instantiation_key(&self, name: &str, type_args: &[String]) -> String {
        self.mangle_name(name, type_args)
    }

    /// Extract generic parameter names from a Generics AST node.
    fn extract_type_params(generics: &Option<Generics>) -> Vec<String> {
        generics.as_ref().map_or_else(Vec::new, |g| {
            g.params
                .iter()
                .filter_map(|p| {
                    match p {
                        GenericParam::Type(tp) => Some(tp.name.name.to_string()),
                        GenericParam::Const(_) => None, // Skip const params for now
                    }
                })
                .collect()
        })
    }

    /// Check if an item has generics.
    fn has_generics(generics: &Option<Generics>) -> bool {
        generics.as_ref().is_some_and(|g| !g.params.is_empty())
    }

    /// Store a generic function definition for later monomorphization.
    fn store_generic_function(&mut self, func: &FunctionDef) {
        let name = self.qualified_name(&func.name.name);
        let type_params = Self::extract_type_params(&func.generics);
        self.generic_defs.insert(
            name,
            GenericDef {
                kind: GenericDefKind::Function(func.clone()),
                type_params,
            },
        );
    }

    /// Store a generic struct definition for later monomorphization.
    fn store_generic_struct(&mut self, s: &StructDef) {
        let name = self.qualified_name(&s.name.name);
        let type_params = Self::extract_type_params(&s.generics);
        self.generic_defs.insert(
            name,
            GenericDef {
                kind: GenericDefKind::Struct(s.clone()),
                type_params,
            },
        );
    }

    /// Store a generic enum definition for later monomorphization.
    fn store_generic_enum(&mut self, e: &EnumDef) {
        let name = self.qualified_name(&e.name.name);
        let type_params = Self::extract_type_params(&e.generics);
        self.generic_defs.insert(
            name,
            GenericDef {
                kind: GenericDefKind::Enum(e.clone()),
                type_params,
            },
        );
    }

    /// Get the current compiler state.
    fn current(&mut self) -> &mut CompilerState {
        self.states.last_mut().expect("No compiler state")
    }

    /// Get the current chunk.
    fn chunk(&mut self) -> &mut Chunk {
        &mut self.current().chunk
    }

    /// Generate a unique temporary variable name.
    fn fresh_temp(&mut self, prefix: &str) -> String {
        let id = self.temp_counter;
        self.temp_counter += 1;
        format!("__{}_{}__", prefix, id)
    }

    fn emit(&mut self, op: OpCode) {
        let line = self.current_line;
        self.chunk().write_op(op, line);
    }

    fn emit_byte(&mut self, byte: u8) {
        let line = self.current_line;
        self.chunk().write(byte, line);
    }

    fn emit_u16(&mut self, value: u16) {
        let line = self.current_line;
        self.chunk().write_u16(value, line);
    }

    fn emit_constant(&mut self, constant: Constant) -> Result<(), CompileError> {
        let idx = self.add_constant(constant)?;
        if idx <= 255 {
            self.emit(OpCode::ConstSmall);
            self.emit_byte(idx as u8);
        } else {
            self.emit(OpCode::Const);
            self.emit_u16(idx);
        }
        Ok(())
    }

    /// Add a constant to the pool and return its index (without emitting load instruction).
    fn add_constant(&mut self, constant: Constant) -> Result<u16, CompileError> {
        self.chunk()
            .add_constant(constant)
            .ok_or(CompileError::TooManyConstants)
    }

    fn emit_jump(&mut self, op: OpCode) -> usize {
        self.emit(op);
        let offset = self.chunk().len();
        self.emit_u16(0xFFFF); // Placeholder
        offset
    }

    fn patch_jump(&mut self, offset: usize) -> Result<(), CompileError> {
        let jump = self.chunk().len() - offset - 2;
        if jump > i16::MAX as usize {
            return Err(CompileError::JumpTooLarge);
        }
        self.chunk().patch_u16(offset, jump as u16);
        Ok(())
    }

    fn emit_loop(&mut self, loop_start: usize) -> Result<(), CompileError> {
        self.emit(OpCode::Loop);
        let offset = self.chunk().len() - loop_start + 2;
        if offset > u16::MAX as usize {
            return Err(CompileError::JumpTooLarge);
        }
        self.emit_u16(offset as u16);
        Ok(())
    }

    fn emit_local_load(&mut self, slot: u16) {
        if slot <= 255 {
            self.emit(OpCode::LoadLocalSmall);
            self.emit_byte(slot as u8);
        } else {
            self.emit(OpCode::LoadLocal);
            self.emit_u16(slot);
        }
    }

    fn begin_scope(&mut self) {
        self.current().scope_depth += 1;
    }

    fn end_scope(&mut self) {
        // First, decrement scope depth and get the target depth
        let scope_depth = {
            let state = self.current();
            state.scope_depth -= 1;
            state.scope_depth
        };

        // Pop locals that are going out of scope
        loop {
            let should_pop = {
                let state = self.current();
                if let Some(local) = state.locals.last() {
                    if local.depth > scope_depth {
                        Some(local.is_captured)
                    } else {
                        None
                    }
                } else {
                    None
                }
            };

            match should_pop {
                Some(true) => {
                    self.emit(OpCode::CloseUpvalue);
                    self.current().locals.pop();
                }
                Some(false) => {
                    self.emit(OpCode::Pop);
                    self.current().locals.pop();
                }
                None => break,
            }
        }
    }

    /// End scope but preserve the top value on the stack.
    /// This is used when a block ends with an expression whose value we need to keep.
    fn end_scope_preserving_top(&mut self) {
        // First, decrement scope depth and get the target depth
        let scope_depth = {
            let state = self.current();
            state.scope_depth -= 1;
            state.scope_depth
        };

        // For each local going out of scope, swap the result under it, then pop
        loop {
            let should_pop = {
                let state = self.current();
                if let Some(local) = state.locals.last() {
                    if local.depth > scope_depth {
                        Some(local.is_captured)
                    } else {
                        None
                    }
                } else {
                    None
                }
            };

            match should_pop {
                Some(true) => {
                    // Swap result under the captured local, close upvalue, pop
                    self.emit(OpCode::Swap);
                    self.emit(OpCode::CloseUpvalue);
                    self.current().locals.pop();
                }
                Some(false) => {
                    // Swap result under the local, then pop the local
                    self.emit(OpCode::Swap);
                    self.emit(OpCode::Pop);
                    self.current().locals.pop();
                }
                None => break,
            }
        }
    }

    fn declare_local(&mut self, name: &str, mutable: bool) -> Result<u16, CompileError> {
        let state = self.current();

        // Check for duplicate in current scope
        for local in state.locals.iter().rev() {
            if local.depth < state.scope_depth {
                break;
            }
            if local.name == name {
                return Err(CompileError::Custom(format!(
                    "Variable '{}' already declared in this scope",
                    name
                )));
            }
        }

        if state.locals.len() >= 256 {
            return Err(CompileError::TooManyLocals);
        }

        let idx = state.locals.len() as u16;
        state.locals.push(Local {
            name: name.to_string(),
            depth: state.scope_depth,
            is_captured: false,
            mutable,
            borrow_state: BorrowState::Unborrowed,
        });
        Ok(idx)
    }

    fn resolve_local(&self, name: &str) -> Option<(u16, bool)> {
        let state = self.states.last()?;
        for (i, local) in state.locals.iter().enumerate().rev() {
            if local.name == name {
                return Some((i as u16, local.mutable));
            }
        }
        None
    }

    /// Check if we can borrow a local variable
    fn check_borrow(&self, slot: u16, mutable: bool) -> Result<(), CompileError> {
        let state = self.states.last().unwrap();
        let local = &state.locals[slot as usize];
        let name = &local.name;

        match (mutable, local.borrow_state) {
            // Mutable borrow when already immutably borrowed
            (true, BorrowState::ImmutablyBorrowed(_)) => {
                Err(CompileError::AlreadyBorrowedImmut(name.clone()))
            }
            // Mutable borrow when already mutably borrowed
            (true, BorrowState::MutablyBorrowed) => {
                Err(CompileError::DoubleMutBorrow(name.clone()))
            }
            // Immutable borrow when mutably borrowed
            (false, BorrowState::MutablyBorrowed) => {
                Err(CompileError::AlreadyBorrowedMut(name.clone()))
            }
            // All other cases are OK
            _ => Ok(()),
        }
    }

    /// Record that we're borrowing a local variable
    fn record_borrow(&mut self, slot: u16, mutable: bool) {
        let state = self.current();
        let local = &mut state.locals[slot as usize];

        if mutable {
            local.borrow_state = BorrowState::MutablyBorrowed;
        } else {
            match local.borrow_state {
                BorrowState::Unborrowed => {
                    local.borrow_state = BorrowState::ImmutablyBorrowed(1);
                }
                BorrowState::ImmutablyBorrowed(n) => {
                    local.borrow_state = BorrowState::ImmutablyBorrowed(n + 1);
                }
                BorrowState::MutablyBorrowed => {
                    // This shouldn't happen if check_borrow was called first
                }
            }
        }
    }

    fn resolve_upvalue(&mut self, name: &str) -> Option<u8> {
        if self.states.len() < 2 {
            return None;
        }

        let enclosing_idx = self.states.len() - 2;

        // Check if it's a local in the enclosing function
        for (i, local) in self.states[enclosing_idx].locals.iter().enumerate().rev() {
            if local.name == name {
                self.states[enclosing_idx].locals[i].is_captured = true;
                return Some(self.add_upvalue(i as u8, true));
            }
        }

        // Check if it's an upvalue in the enclosing function (recursively)
        // For simplicity, we only support one level of nesting for now
        None
    }

    fn add_upvalue(&mut self, index: u8, is_local: bool) -> u8 {
        let state = self.current();

        // Check if we already have this upvalue
        for (i, uv) in state.upvalues.iter().enumerate() {
            if uv.index == index && uv.is_local == is_local {
                return i as u8;
            }
        }

        let idx = state.upvalues.len() as u8;
        state.upvalues.push(UpvalueInfo { index, is_local });
        idx
    }

    /// Compile a module to bytecode.
    pub fn compile_module(&mut self, module: &Module) -> Result<FunctionProto, CompileError> {
        self.states
            .push(CompilerState::new(Some("main".to_string()), 0));

        // Pass 1: Collect generic definitions, compile non-generic items
        for item in &module.items {
            self.compile_item_pass1(item)?;
        }

        // Pass 2: Monomorphize based on TypeAnnotations
        self.monomorphize_instantiations()?;

        self.emit(OpCode::Unit);
        self.emit(OpCode::Return);

        let state = self.states.pop().unwrap();
        Ok(FunctionProto {
            name: state.function_name,
            arity: state.arity,
            min_arity: state.arity, // Module has no defaults
            chunk: state.chunk,
            upvalues: state.upvalues,
            defaults: Vec::new(),
        })
    }

    /// Pass 1: Collect generic definitions, compile non-generic items.
    fn compile_item_pass1(&mut self, item: &Item) -> Result<(), CompileError> {
        match item {
            Item::Function(func) => {
                if Self::has_generics(&func.generics) {
                    self.store_generic_function(func);
                    Ok(())
                } else {
                    self.compile_function_def(func)
                }
            }
            Item::Struct(s) => {
                if Self::has_generics(&s.generics) {
                    self.store_generic_struct(s);
                    Ok(())
                } else {
                    self.compile_struct_def(s)
                }
            }
            Item::Enum(e) => {
                if Self::has_generics(&e.generics) {
                    self.store_generic_enum(e);
                    Ok(())
                } else {
                    self.compile_enum_def(e)
                }
            }
            Item::Const(c) => self.compile_const_def(c),
            Item::Impl(impl_block) => self.compile_impl_block(impl_block),
            Item::Trait(trait_def) => self.compile_trait_def(trait_def),
            Item::Mod(mod_decl) => self.compile_mod_decl(mod_decl),
            Item::Use(use_decl) => self.compile_use_decl(use_decl),
            Item::TypeAlias(alias) => self.compile_type_alias(alias),
        }
    }

    /// Pass 2: Monomorphize all instantiations recorded by the type checker.
    fn monomorphize_instantiations(&mut self) -> Result<(), CompileError> {
        // Get instantiations from type annotations
        let instantiations = match &self.type_annotations {
            Some(ann) => ann.instantiations.clone(),
            None => return Ok(()), // No type annotations, nothing to monomorphize
        };

        for inst in instantiations {
            // type_args is already Vec<String> from the type checker
            let key = self.instantiation_key(&inst.name, &inst.type_args);

            // Skip if already compiled
            if self.compiled_instantiations.contains_key(&key) {
                continue;
            }

            // Find the generic definition and monomorphize
            if let Some(def) = self.generic_defs.get(&inst.name).cloned() {
                match &def.kind {
                    GenericDefKind::Function(func) => {
                        self.monomorphize_function(
                            &inst.name,
                            func,
                            &def.type_params,
                            &inst.type_args,
                        )?;
                    }
                    GenericDefKind::Struct(s) => {
                        self.monomorphize_struct(&inst.name, s, &def.type_params, &inst.type_args)?;
                    }
                    GenericDefKind::Enum(e) => {
                        self.monomorphize_enum(&inst.name, e, &def.type_params, &inst.type_args)?;
                    }
                }
            }
        }

        Ok(())
    }

    /// Monomorphize a generic function with specific type arguments.
    fn monomorphize_function(
        &mut self,
        _base_name: &str,
        func: &FunctionDef,
        type_params: &[String],
        type_args: &[String],
    ) -> Result<(), CompileError> {
        // Build substitution: T -> "i64", U -> "String", etc.
        self.current_substitution = type_params
            .iter()
            .zip(type_args.iter())
            .map(|(p, a)| (p.clone(), a.clone()))
            .collect();

        // Generate mangled name
        let mangled_name = self.mangle_name(&func.name.name, type_args);

        // Compile the function with the mangled name
        self.compile_function_def_with_name(func, &mangled_name)?;

        // Record that we've compiled this instantiation
        let key = self.instantiation_key(&func.name.name, type_args);
        self.compiled_instantiations.insert(key, mangled_name);

        // Clear substitution
        self.current_substitution.clear();

        Ok(())
    }

    /// Monomorphize a generic struct with specific type arguments.
    fn monomorphize_struct(
        &mut self,
        _base_name: &str,
        s: &StructDef,
        type_params: &[String],
        type_args: &[String],
    ) -> Result<(), CompileError> {
        // Build substitution
        self.current_substitution = type_params
            .iter()
            .zip(type_args.iter())
            .map(|(p, a)| (p.clone(), a.clone()))
            .collect();

        // Generate mangled name and compile struct with that name
        let mangled_name = self.mangle_name(&s.name.name, type_args);
        self.compile_struct_def_with_name(s, &mangled_name)?;

        // Record instantiation
        let key = self.instantiation_key(&s.name.name, type_args);
        self.compiled_instantiations.insert(key, mangled_name);

        self.current_substitution.clear();
        Ok(())
    }

    /// Monomorphize a generic enum with specific type arguments.
    fn monomorphize_enum(
        &mut self,
        _base_name: &str,
        e: &EnumDef,
        type_params: &[String],
        type_args: &[String],
    ) -> Result<(), CompileError> {
        // Build substitution
        self.current_substitution = type_params
            .iter()
            .zip(type_args.iter())
            .map(|(p, a)| (p.clone(), a.clone()))
            .collect();

        // Generate mangled name and compile enum with that name
        let mangled_name = self.mangle_name(&e.name.name, type_args);
        self.compile_enum_def_with_name(e, &mangled_name)?;

        // Record instantiation
        let key = self.instantiation_key(&e.name.name, type_args);
        self.compiled_instantiations.insert(key, mangled_name);

        self.current_substitution.clear();
        Ok(())
    }

    // Legacy compile_item for backwards compatibility (non-two-pass scenarios)
    fn compile_item(&mut self, item: &Item) -> Result<(), CompileError> {
        match item {
            Item::Function(func) => self.compile_function_def(func),
            Item::Const(c) => self.compile_const_def(c),
            Item::Struct(s) => self.compile_struct_def(s),
            Item::Enum(e) => self.compile_enum_def(e),
            Item::Impl(impl_block) => self.compile_impl_block(impl_block),
            Item::Trait(trait_def) => self.compile_trait_def(trait_def),
            Item::Mod(mod_decl) => self.compile_mod_decl(mod_decl),
            Item::Use(use_decl) => self.compile_use_decl(use_decl),
            Item::TypeAlias(alias) => self.compile_type_alias(alias),
        }
    }

    fn compile_type_alias(&mut self, alias: &TypeAlias) -> Result<(), CompileError> {
        let alias_name = self.qualified_name(&alias.name.name);

        // Extract the underlying type name from the Type
        let target_name = match &alias.ty.kind {
            TypeKind::Path(path) => {
                if let Some(seg) = path.segments.first() {
                    seg.ident.name.to_string()
                } else {
                    return Err(CompileError::Custom(
                        "Type alias has empty type path".to_string(),
                    ));
                }
            }
            TypeKind::Unit => "()".to_string(),
            _ => {
                // For complex types, we just store a representation
                // In practice, aliases to complex types won't be used for construction
                format!("{:?}", alias.ty.kind)
            }
        };

        self.type_aliases.insert(alias_name, target_name);
        Ok(())
    }

    /// Get the fully qualified name for an item (prefixed with current module path).
    fn qualified_name(&self, name: &str) -> String {
        if self.current_module_path.is_empty() {
            name.to_string()
        } else {
            format!("{}::{}", self.current_module_path.join("::"), name)
        }
    }

    fn compile_mod_decl(&mut self, mod_decl: &ModDecl) -> Result<(), CompileError> {
        let mod_name = mod_decl.name.name.to_string();

        if let Some(items) = &mod_decl.items {
            // Inline module: mod foo { ... }
            self.current_module_path.push(mod_name);

            for item in items {
                self.compile_item(item)?;
            }

            self.current_module_path.pop();
        } else {
            // External module: mod foo; - not supported yet
            return Err(CompileError::Custom(
                "External modules (mod foo;) not yet supported. Use inline modules.".to_string(),
            ));
        }

        Ok(())
    }

    fn compile_use_decl(&mut self, use_decl: &UseDecl) -> Result<(), CompileError> {
        self.process_use_tree(&use_decl.tree, &[])
    }

    fn process_use_tree(&mut self, tree: &UseTree, prefix: &[String]) -> Result<(), CompileError> {
        match tree {
            UseTree::Path { path, alias } => {
                // Build full path
                let mut full_path = prefix.to_vec();
                for seg in path {
                    full_path.push(seg.name.to_string());
                }

                // The imported name is the alias or the last segment
                let imported_name = alias
                    .as_ref()
                    .map(|a| a.name.to_string())
                    .unwrap_or_else(|| full_path.last().unwrap().clone());

                let full_path_str = full_path.join("::");
                self.module_imports.insert(imported_name, full_path_str);
                Ok(())
            }
            UseTree::Glob { path: _ } => {
                // use foo::* - not supported yet (would need module introspection)
                Err(CompileError::Custom(
                    "Glob imports (use foo::*) not yet supported".to_string(),
                ))
            }
            UseTree::Nested { path, items } => {
                // use foo::{bar, baz}
                let mut new_prefix = prefix.to_vec();
                for seg in path {
                    new_prefix.push(seg.name.to_string());
                }
                for item in items {
                    self.process_use_tree(item, &new_prefix)?;
                }
                Ok(())
            }
        }
    }

    fn compile_struct_def(&mut self, struct_def: &StructDef) -> Result<(), CompileError> {
        let name = self.qualified_name(&struct_def.name.name);
        self.compile_struct_def_impl(struct_def, &name)
    }

    /// Compile a struct with a custom name (for monomorphization).
    fn compile_struct_def_with_name(
        &mut self,
        struct_def: &StructDef,
        name: &str,
    ) -> Result<(), CompileError> {
        self.compile_struct_def_impl(struct_def, name)
    }

    /// Internal implementation for struct compilation.
    fn compile_struct_def_impl(
        &mut self,
        struct_def: &StructDef,
        name: &str,
    ) -> Result<(), CompileError> {
        let (fields, defaults) = match &struct_def.fields {
            StructFields::Named(fields) => {
                let field_names = fields.iter().map(|f| f.name.name.to_string()).collect();
                let field_defaults: HashMap<String, Expr> = fields
                    .iter()
                    .filter_map(|f| {
                        f.default
                            .as_ref()
                            .map(|d| (f.name.name.to_string(), (**d).clone()))
                    })
                    .collect();
                (field_names, field_defaults)
            }
            StructFields::Tuple(fields) => {
                // For tuple structs, use numeric field names
                (
                    (0..fields.len()).map(|i| i.to_string()).collect(),
                    HashMap::new(),
                )
            }
            StructFields::Unit => (Vec::new(), HashMap::new()),
        };
        self.struct_types
            .insert(name.to_string(), StructTypeInfo { fields, defaults });
        Ok(())
    }

    fn compile_enum_def(&mut self, enum_def: &EnumDef) -> Result<(), CompileError> {
        let name = self.qualified_name(&enum_def.name.name);
        self.compile_enum_def_impl(enum_def, &name)
    }

    /// Compile an enum with a custom name (for monomorphization).
    fn compile_enum_def_with_name(
        &mut self,
        enum_def: &EnumDef,
        name: &str,
    ) -> Result<(), CompileError> {
        self.compile_enum_def_impl(enum_def, name)
    }

    /// Internal implementation for enum compilation.
    fn compile_enum_def_impl(
        &mut self,
        enum_def: &EnumDef,
        name: &str,
    ) -> Result<(), CompileError> {
        let mut variants: HashMap<String, (u8, usize)> = HashMap::new();
        let mut variants_ordered: Vec<(String, usize)> = Vec::new();

        for (i, v) in enum_def.variants.iter().enumerate() {
            let field_count = match &v.fields {
                StructFields::Named(fields) => fields.len(),
                StructFields::Tuple(fields) => fields.len(),
                StructFields::Unit => 0,
            };
            let variant_name = v.name.name.to_string();
            variants.insert(variant_name.clone(), (i as u8, field_count));
            variants_ordered.push((variant_name, field_count));
        }

        self.enum_types.insert(
            name.to_string(),
            EnumTypeInfo {
                variants,
                variants_ordered,
            },
        );
        Ok(())
    }

    fn compile_trait_def(&mut self, trait_def: &TraitDef) -> Result<(), CompileError> {
        let name = self.qualified_name(&trait_def.name.name);
        let mut methods = Vec::new();

        for item in &trait_def.items {
            if let TraitItem::Function(func) = item {
                let method_info = TraitMethodInfo {
                    name: func.name.name.to_string(),
                    has_default: func.default.is_some(),
                };
                methods.push(method_info);
            }
        }

        self.trait_types.insert(name, TraitTypeInfo { methods });
        Ok(())
    }

    fn compile_impl_block(&mut self, impl_block: &ImplBlock) -> Result<(), CompileError> {
        // Get the type name from self_ty
        let type_name = match &impl_block.self_ty.kind {
            TypeKind::Path(path) => {
                if let Some(seg) = path.segments.first() {
                    seg.ident.name.to_string()
                } else {
                    return Err(CompileError::Custom(
                        "impl block has empty type path".to_string(),
                    ));
                }
            }
            _ => {
                return Err(CompileError::Custom(
                    "unsupported impl block type".to_string(),
                ))
            }
        };

        // If this is a trait impl, verify all required methods are present
        if let Some(trait_path) = &impl_block.trait_ {
            let trait_name = if let Some(seg) = trait_path.segments.first() {
                seg.ident.name.to_string()
            } else {
                return Err(CompileError::Custom(
                    "impl block has empty trait path".to_string(),
                ));
            };

            // Check trait exists and get required methods
            if let Some(trait_info) = self.trait_types.get(&trait_name).cloned() {
                // Collect implemented method names
                let mut implemented_methods: Vec<String> = Vec::new();
                for item in &impl_block.items {
                    if let ImplItem::Function(func) = item {
                        implemented_methods.push(func.name.name.to_string());
                    }
                }

                // Check all required methods are implemented
                for method in &trait_info.methods {
                    if !method.has_default && !implemented_methods.contains(&method.name) {
                        return Err(CompileError::Custom(format!(
                            "trait {} requires method '{}' but it is not implemented for {}",
                            trait_name, method.name, type_name
                        )));
                    }
                }
            }
            // If trait not found, proceed anyway (might be in another module)
        }

        // Compile each method
        for item in &impl_block.items {
            if let ImplItem::Function(func) = item {
                self.compile_method(func, &type_name)?;
            }
        }

        Ok(())
    }

    fn compile_method(&mut self, func: &FunctionDef, type_name: &str) -> Result<(), CompileError> {
        self.current_line = func.span.start;

        // Calculate min_arity (params without defaults)
        let min_arity = func
            .params
            .iter()
            .take_while(|p| p.default.is_none())
            .count() as u8;

        // Compile the function body in a new state
        self.states.push(CompilerState::new(
            Some(func.name.name.to_string()),
            func.params.len() as u8,
        ));

        self.begin_scope();

        // Bind parameters (including self)
        for param in &func.params {
            self.compile_pattern_binding(&param.pattern, true)?;
        }

        // Compile body
        self.compile_block(&func.body)?;

        // Implicit return
        self.emit(OpCode::Return);

        self.end_scope();

        // Compile default values as constants
        let mut defaults = Vec::new();
        for param in &func.params {
            if let Some(default_expr) = &param.default {
                // We need to compile defaults in a temporary way
                // For now, we'll store them directly if they're simple literals
                let const_idx = self.compile_default_value(default_expr)?;
                defaults.push(const_idx);
            }
        }

        let func_state = self.states.pop().unwrap();
        let upvalues = func_state.upvalues.clone();
        let proto = FunctionProto {
            name: func_state.function_name,
            arity: func_state.arity,
            min_arity,
            chunk: func_state.chunk,
            upvalues: func_state.upvalues,
            defaults,
        };

        // Store the function as a constant
        let proto_idx = self.add_constant(Constant::Function(Box::new(proto)))?;

        // Emit closure creation with upvalue info
        self.emit(OpCode::Closure);
        self.emit_u16(proto_idx);
        self.emit_byte(upvalues.len() as u8);
        for uv in &upvalues {
            self.emit_byte(if uv.is_local { 1 } else { 0 });
            self.emit_byte(uv.index);
        }

        // Register as a method: DefineMethod [type_name_idx: u16, method_name_idx: u16]
        let type_name_idx = self.add_constant(Constant::String(type_name.to_string()))?;
        let method_name_idx = self.add_constant(Constant::String(func.name.name.to_string()))?;

        self.emit(OpCode::DefineMethod);
        self.emit_u16(type_name_idx);
        self.emit_u16(method_name_idx);

        Ok(())
    }

    /// Compile a default parameter value to a constant.
    fn compile_default_value(&mut self, expr: &Expr) -> Result<u16, CompileError> {
        // For now, only support literal defaults
        match &expr.kind {
            ExprKind::Literal(lit) => {
                let constant = match lit {
                    Literal::Int(n) => Constant::Int(*n),
                    Literal::Float(n) => Constant::Float(*n),
                    Literal::String(s) => Constant::String(s.clone()),
                    Literal::Bool(true) => Constant::Int(1), // Will be handled specially
                    Literal::Bool(false) => Constant::Int(0),
                    Literal::Char(c) => Constant::Int(*c as i64),
                    Literal::Unit => Constant::Int(0), // Unit placeholder
                };
                self.add_constant(constant)
            }
            ExprKind::Unary {
                op: UnaryOp::Neg,
                operand,
            } => {
                // Handle negative literals like -1
                if let ExprKind::Literal(Literal::Int(n)) = &operand.kind {
                    self.add_constant(Constant::Int(-n))
                } else if let ExprKind::Literal(Literal::Float(n)) = &operand.kind {
                    self.add_constant(Constant::Float(-n))
                } else {
                    Err(CompileError::Custom(
                        "Default parameter must be a literal value".to_string(),
                    ))
                }
            }
            _ => Err(CompileError::Custom(
                "Default parameter must be a literal value".to_string(),
            )),
        }
    }

    fn compile_function_def(&mut self, func: &FunctionDef) -> Result<(), CompileError> {
        self.current_line = func.span.start;

        // Calculate min_arity (params without defaults)
        let min_arity = func
            .params
            .iter()
            .take_while(|p| p.default.is_none())
            .count() as u8;

        // Compile the function body in a new state
        self.states.push(CompilerState::new(
            Some(func.name.name.to_string()),
            func.params.len() as u8,
        ));

        self.begin_scope();

        // Bind parameters
        for param in &func.params {
            self.compile_pattern_binding(&param.pattern, true)?;
        }

        if func.is_async {
            // For async functions, wrap the body in a Future
            // First compile the inner body as a nested closure
            self.states.push(CompilerState::new(
                Some(format!("{}<async>", func.name.name)),
                0,
            ));
            self.begin_scope();

            // Compile the actual body
            self.compile_block(&func.body)?;
            self.emit(OpCode::Return);

            self.end_scope();
            let inner_state = self.states.pop().unwrap();
            let inner_upvalues = inner_state.upvalues.clone();

            let inner_proto = FunctionProto {
                name: inner_state.function_name,
                arity: 0,
                min_arity: 0,
                chunk: inner_state.chunk,
                upvalues: inner_state.upvalues,
                defaults: Vec::new(),
            };

            // Emit CreateFuture with the inner proto
            let idx = self.add_constant(Constant::Function(Box::new(inner_proto)))?;
            self.emit(OpCode::CreateFuture);
            self.emit_u16(idx);
            self.emit_byte(inner_upvalues.len() as u8);
            for uv in &inner_upvalues {
                self.emit_byte(if uv.is_local { 1 } else { 0 });
                self.emit_byte(uv.index);
            }
            self.emit(OpCode::Return);
        } else {
            // Regular function - compile body directly
            self.compile_block(&func.body)?;
            // Implicit return of last expression or unit
            self.emit(OpCode::Return);
        }

        self.end_scope();

        // Compile default values as constants (before popping state)
        let mut defaults = Vec::new();
        for param in &func.params {
            if let Some(default_expr) = &param.default {
                let const_idx = self.compile_default_value(default_expr)?;
                defaults.push(const_idx);
            }
        }

        let func_state = self.states.pop().unwrap();
        let upvalues = func_state.upvalues.clone();
        let proto = FunctionProto {
            name: func_state.function_name,
            arity: func_state.arity,
            min_arity,
            chunk: func_state.chunk,
            upvalues: func_state.upvalues,
            defaults,
        };

        // Store the function as a constant and define it
        let idx = self.add_constant(Constant::Function(Box::new(proto)))?;

        // Emit closure creation with upvalue info
        self.emit(OpCode::Closure);
        self.emit_u16(idx);
        self.emit_byte(upvalues.len() as u8);
        for uv in &upvalues {
            self.emit_byte(if uv.is_local { 1 } else { 0 });
            self.emit_byte(uv.index);
        }

        // Store in global with qualified name
        let qualified = self.qualified_name(&func.name.name);
        let name_idx = self.add_constant(Constant::String(qualified))?;
        self.emit(OpCode::StoreGlobal);
        self.emit_u16(name_idx);

        Ok(())
    }

    /// Compile a function with a custom name (for monomorphization).
    fn compile_function_def_with_name(
        &mut self,
        func: &FunctionDef,
        name: &str,
    ) -> Result<(), CompileError> {
        self.current_line = func.span.start;

        // Calculate min_arity (params without defaults)
        let min_arity = func
            .params
            .iter()
            .take_while(|p| p.default.is_none())
            .count() as u8;

        // Compile the function body in a new state with the custom name
        self.states.push(CompilerState::new(
            Some(name.to_string()),
            func.params.len() as u8,
        ));

        self.begin_scope();

        // Bind parameters
        for param in &func.params {
            self.compile_pattern_binding(&param.pattern, true)?;
        }

        // Compile body
        self.compile_block(&func.body)?;

        // Implicit return of last expression or unit
        self.emit(OpCode::Return);

        self.end_scope();

        // Compile default values as constants
        let mut defaults = Vec::new();
        for param in &func.params {
            if let Some(default_expr) = &param.default {
                let const_idx = self.compile_default_value(default_expr)?;
                defaults.push(const_idx);
            }
        }

        let func_state = self.states.pop().unwrap();
        let upvalues = func_state.upvalues.clone();
        let proto = FunctionProto {
            name: func_state.function_name,
            arity: func_state.arity,
            min_arity,
            chunk: func_state.chunk,
            upvalues: func_state.upvalues,
            defaults,
        };

        // Store the function as a constant and define it
        let idx = self.add_constant(Constant::Function(Box::new(proto)))?;

        // Emit closure creation with upvalue info
        self.emit(OpCode::Closure);
        self.emit_u16(idx);
        self.emit_byte(upvalues.len() as u8);
        for uv in &upvalues {
            self.emit_byte(if uv.is_local { 1 } else { 0 });
            self.emit_byte(uv.index);
        }

        // Store in global with the mangled name
        let name_idx = self.add_constant(Constant::String(name.to_string()))?;
        self.emit(OpCode::StoreGlobal);
        self.emit_u16(name_idx);

        Ok(())
    }

    fn compile_const_def(&mut self, c: &ConstDef) -> Result<(), CompileError> {
        self.current_line = c.span.start;
        self.compile_expr(&c.value)?;

        // Store with qualified name
        let qualified = self.qualified_name(&c.name.name);
        let name_idx = self.add_constant(Constant::String(qualified))?;
        self.emit(OpCode::StoreGlobal);
        self.emit_u16(name_idx);

        Ok(())
    }

    fn compile_block(&mut self, block: &Block) -> Result<(), CompileError> {
        self.begin_scope();

        let mut last_was_expr = false;
        for (i, stmt) in block.stmts.iter().enumerate() {
            let is_last = i == block.stmts.len() - 1;
            last_was_expr = false;

            match &stmt.kind {
                StmtKind::Let { pattern, init, .. } => {
                    if let Some(init) = init {
                        self.compile_expr(init)?;
                    } else {
                        self.emit(OpCode::Unit);
                    }
                    self.compile_pattern_binding(pattern, false)?;
                }
                StmtKind::Expr(expr) => {
                    self.compile_expr(expr)?;
                    self.emit(OpCode::Pop);
                }
                StmtKind::ExprNoSemi(expr) => {
                    self.compile_expr(expr)?;
                    if !is_last {
                        self.emit(OpCode::Pop);
                    } else {
                        last_was_expr = true;
                    }
                }
                StmtKind::Item(item) => {
                    self.compile_item(item)?;
                }
                StmtKind::Empty => {}
            }
        }

        if last_was_expr {
            // Expression result is on top of stack. We need to preserve it while popping locals.
            // Use swap-pop sequence to move result down past each local.
            self.end_scope_preserving_top();
        } else {
            // No expression result - just pop locals and push unit
            self.end_scope();
            self.emit(OpCode::Unit);
        }

        Ok(())
    }

    /// Check if a pattern requires runtime checking (refutable pattern).
    fn pattern_needs_check(&self, pattern: &Pattern) -> bool {
        match &pattern.kind {
            PatternKind::Wildcard => false,
            PatternKind::Binding {
                subpattern: None, ..
            } => false,
            PatternKind::Binding {
                subpattern: Some(sub),
                ..
            } => self.pattern_needs_check(sub),
            PatternKind::Literal(_) => true,
            PatternKind::Path(_) => true,
            PatternKind::TupleStruct { .. } => true,
            PatternKind::Struct { .. } => true,
            PatternKind::Tuple(patterns) => patterns.iter().any(|p| self.pattern_needs_check(p)),
            PatternKind::Or(_) => true,
            PatternKind::Rest => false,
            _ => true, // Default to needing check for safety
        }
    }

    /// Compile a pattern check. Assumes scrutinee is on stack.
    /// After execution, stack has: [scrutinee, bool] indicating if pattern matches.
    /// The scrutinee is preserved for binding if the pattern matches.
    fn compile_pattern_check(&mut self, pattern: &Pattern) -> Result<(), CompileError> {
        match &pattern.kind {
            PatternKind::Wildcard => {
                // Always matches - just push true (scrutinee stays)
                self.emit(OpCode::True);
                Ok(())
            }
            PatternKind::Binding {
                subpattern: None, ..
            } => {
                // Always matches - just push true (scrutinee stays)
                self.emit(OpCode::True);
                Ok(())
            }
            PatternKind::Binding {
                subpattern: Some(sub),
                ..
            } => {
                // Check the subpattern
                self.compile_pattern_check(sub)
            }
            PatternKind::Literal(lit) => {
                // Dup scrutinee, compare with literal
                self.emit(OpCode::Dup);
                self.compile_literal(lit)?;
                self.emit(OpCode::Eq);
                // Stack: [scrutinee, bool]
                Ok(())
            }
            PatternKind::Path(path) => {
                // For enum variant paths like Color::Red
                if path.segments.len() == 2 {
                    let enum_name = path.segments[0].ident.name.to_string();
                    let variant_name = path.segments[1].ident.name.to_string();

                    // Dup scrutinee, then check enum variant
                    self.emit(OpCode::Dup);
                    let enum_name_idx = self.add_constant(Constant::String(enum_name))?;
                    let variant_name_idx = self.add_constant(Constant::String(variant_name))?;

                    self.emit(OpCode::MatchEnum);
                    self.emit_u16(enum_name_idx);
                    self.emit_u16(variant_name_idx);
                    // Stack: [scrutinee, bool]
                    Ok(())
                } else {
                    // Single identifier - dup and compare equality
                    self.emit(OpCode::Dup);
                    self.compile_path(path)?;
                    self.emit(OpCode::Eq);
                    Ok(())
                }
            }
            PatternKind::TupleStruct { path, fields } => {
                // Check if scrutinee matches enum variant with fields
                if path.segments.len() >= 2 {
                    let enum_name = path.segments[path.segments.len() - 2]
                        .ident
                        .name
                        .to_string();
                    let variant_name = path.segments[path.segments.len() - 1]
                        .ident
                        .name
                        .to_string();

                    self.emit(OpCode::Dup);
                    let enum_name_idx = self.add_constant(Constant::String(enum_name))?;
                    let variant_name_idx = self.add_constant(Constant::String(variant_name))?;

                    self.emit(OpCode::MatchEnum);
                    self.emit_u16(enum_name_idx);
                    self.emit_u16(variant_name_idx);
                    let _ = fields;
                    Ok(())
                } else if path.segments.len() == 1 {
                    // Single segment like Some(x) - could be enum variant in scope
                    let variant_name = path.segments[0].ident.name.to_string();

                    self.emit(OpCode::Dup);
                    let variant_name_idx = self.add_constant(Constant::String(variant_name))?;
                    let empty_idx = self.add_constant(Constant::String(String::new()))?;

                    self.emit(OpCode::MatchEnum);
                    self.emit_u16(empty_idx);
                    self.emit_u16(variant_name_idx);
                    let _ = fields;
                    Ok(())
                } else {
                    Err(CompileError::Custom(
                        "Invalid tuple struct pattern".to_string(),
                    ))
                }
            }
            PatternKind::Tuple(patterns) => {
                // Check each sub-pattern that needs checking
                let mut fail_jumps = Vec::new();

                for (i, pat) in patterns.iter().enumerate() {
                    if self.pattern_needs_check(pat) {
                        // Get the field and check the sub-pattern
                        self.emit(OpCode::Dup);
                        self.emit(OpCode::GetField);
                        self.emit_byte(i as u8);
                        // Sub-pattern check: leaves [scrutinee, field, bool]
                        self.compile_pattern_check(pat)?;
                        // Swap and pop field: [scrutinee, bool, field] -> [scrutinee, bool]
                        self.emit(OpCode::Swap);
                        self.emit(OpCode::Pop);
                        // Jump to fail if check failed
                        fail_jumps.push(self.emit_jump(OpCode::JumpIfFalse));
                        // JumpIfFalse pops condition, stack: [scrutinee]
                    }
                }

                // All checks passed
                self.emit(OpCode::True);
                let done_jump = self.emit_jump(OpCode::Jump);

                // Patch fail jumps
                for jump in fail_jumps {
                    self.patch_jump(jump)?;
                }
                self.emit(OpCode::False);

                self.patch_jump(done_jump)?;
                // Stack: [scrutinee, bool]
                Ok(())
            }
            PatternKind::Or(patterns) => {
                // Simplified: just try first pattern
                if let Some(first) = patterns.first() {
                    self.compile_pattern_check(first)
                } else {
                    self.emit(OpCode::False);
                    Ok(())
                }
            }
            _ => {
                // Default: assume match
                self.emit(OpCode::True);
                Ok(())
            }
        }
    }

    fn compile_pattern_binding(
        &mut self,
        pattern: &Pattern,
        is_param: bool,
    ) -> Result<(), CompileError> {
        match &pattern.kind {
            PatternKind::Binding { name, mutable, .. } => {
                if is_param {
                    // Parameters are already on the stack
                    self.declare_local(&name.name, *mutable)?;
                } else {
                    // Value is on stack, declare local
                    self.declare_local(&name.name, *mutable)?;
                }
                Ok(())
            }
            PatternKind::Wildcard => {
                if !is_param {
                    self.emit(OpCode::Pop);
                }
                Ok(())
            }
            PatternKind::Tuple(patterns) => {
                // Destructure tuple
                // Stack: [tuple]
                // Store tuple in a temporary local so we can access it for each element
                let temp_slot = self.current().locals.len() as u8;
                let temp_name = self.fresh_temp("tuple");
                self.declare_local(&temp_name, false)?;

                for (i, pat) in patterns.iter().enumerate() {
                    // Load the tuple from its temp slot
                    self.emit(OpCode::LoadLocalSmall);
                    self.emit_byte(temp_slot);
                    // Get the i-th field
                    self.emit(OpCode::GetField);
                    self.emit_byte(i as u8);
                    // Bind the element
                    self.compile_pattern_binding(pat, false)?;
                }
                // The tuple temp slot will be cleaned up when scope ends
                Ok(())
            }
            PatternKind::TupleStruct { fields, .. } => {
                // Destructure enum variant fields
                // Stack: [enum]
                // Store enum in a temporary local so we can access its fields
                let temp_slot = self.current().locals.len() as u8;
                let temp_name = self.fresh_temp("enum");
                self.declare_local(&temp_name, false)?;

                for (i, pat) in fields.iter().enumerate() {
                    // Load the enum from its temp slot
                    self.emit(OpCode::LoadLocalSmall);
                    self.emit_byte(temp_slot);
                    // Get the i-th field
                    self.emit(OpCode::GetField);
                    self.emit_byte(i as u8);
                    // Bind the element
                    self.compile_pattern_binding(pat, false)?;
                }
                // The enum temp slot will be cleaned up when scope ends
                Ok(())
            }
            PatternKind::Path(_) => {
                // Unit enum variant - just consume the scrutinee
                if !is_param {
                    self.emit(OpCode::Pop);
                }
                Ok(())
            }
            _ => {
                // For now, treat other patterns as wildcards
                if !is_param {
                    self.emit(OpCode::Pop);
                }
                Ok(())
            }
        }
    }

    fn compile_expr(&mut self, expr: &Expr) -> Result<(), CompileError> {
        self.current_line = expr.span.start;

        match &expr.kind {
            ExprKind::Literal(lit) => self.compile_literal(lit),

            ExprKind::Path(path) => self.compile_path(path),

            ExprKind::Unary { op, operand } => {
                self.compile_expr(operand)?;
                match op {
                    UnaryOp::Neg => self.emit(OpCode::Neg),
                    UnaryOp::Not => self.emit(OpCode::Not),
                    UnaryOp::BitNot => self.emit(OpCode::BitNot),
                }
                Ok(())
            }

            ExprKind::Binary { op, left, right } => {
                // Check for compile-time division by zero
                if matches!(op, BinaryOp::Div | BinaryOp::Rem) {
                    if let ExprKind::Literal(Literal::Int(0)) = &right.kind {
                        return Err(CompileError::Custom(
                            "division by zero: the divisor is a constant zero".to_string(),
                        ));
                    }
                    if let ExprKind::Literal(Literal::Float(f)) = &right.kind {
                        if *f == 0.0 {
                            return Err(CompileError::Custom(
                                "division by zero: the divisor is a constant zero".to_string(),
                            ));
                        }
                    }
                }

                // Short-circuit for && and ||
                match op {
                    BinaryOp::And => {
                        self.compile_expr(left)?;
                        let jump = self.emit_jump(OpCode::JumpIfFalseNoPop);
                        self.emit(OpCode::Pop);
                        self.compile_expr(right)?;
                        self.patch_jump(jump)?;
                        return Ok(());
                    }
                    BinaryOp::Or => {
                        self.compile_expr(left)?;
                        let jump = self.emit_jump(OpCode::JumpIfTrueNoPop);
                        self.emit(OpCode::Pop);
                        self.compile_expr(right)?;
                        self.patch_jump(jump)?;
                        return Ok(());
                    }
                    _ => {}
                }

                self.compile_expr(left)?;
                self.compile_expr(right)?;

                // Try to emit specialized opcodes if type is known
                let left_ty = self.get_expr_type(left.span.start);
                match op {
                    BinaryOp::Add => {
                        if let Some(ty) = left_ty {
                            if self
                                .type_annotations
                                .as_ref()
                                .is_some_and(|a| a.is_integer(ty))
                            {
                                self.emit(OpCode::IAdd);
                            } else if self
                                .type_annotations
                                .as_ref()
                                .is_some_and(|a| a.is_float(ty))
                            {
                                self.emit(OpCode::FAdd);
                            } else if self
                                .type_annotations
                                .as_ref()
                                .is_some_and(|a| a.is_string(ty))
                            {
                                self.emit(OpCode::SConcat);
                            } else {
                                self.emit(OpCode::Add);
                            }
                        } else {
                            self.emit(OpCode::Add);
                        }
                    }
                    BinaryOp::Sub => {
                        if let Some(ty) = left_ty {
                            if self
                                .type_annotations
                                .as_ref()
                                .is_some_and(|a| a.is_integer(ty))
                            {
                                self.emit(OpCode::ISub);
                            } else if self
                                .type_annotations
                                .as_ref()
                                .is_some_and(|a| a.is_float(ty))
                            {
                                self.emit(OpCode::FSub);
                            } else {
                                self.emit(OpCode::Sub);
                            }
                        } else {
                            self.emit(OpCode::Sub);
                        }
                    }
                    BinaryOp::Mul => {
                        if let Some(ty) = left_ty {
                            if self
                                .type_annotations
                                .as_ref()
                                .is_some_and(|a| a.is_float(ty))
                            {
                                self.emit(OpCode::FMul);
                            } else {
                                self.emit(OpCode::Mul);
                            }
                        } else {
                            self.emit(OpCode::Mul);
                        }
                    }
                    BinaryOp::Div => {
                        if let Some(ty) = left_ty {
                            if self
                                .type_annotations
                                .as_ref()
                                .is_some_and(|a| a.is_float(ty))
                            {
                                self.emit(OpCode::FDiv);
                            } else {
                                self.emit(OpCode::Div);
                            }
                        } else {
                            self.emit(OpCode::Div);
                        }
                    }
                    BinaryOp::Rem => self.emit(OpCode::Rem),
                    BinaryOp::Eq => self.emit(OpCode::Eq),
                    BinaryOp::Ne => self.emit(OpCode::Ne),
                    BinaryOp::Lt => self.emit(OpCode::Lt),
                    BinaryOp::Le => self.emit(OpCode::Le),
                    BinaryOp::Gt => self.emit(OpCode::Gt),
                    BinaryOp::Ge => self.emit(OpCode::Ge),
                    BinaryOp::BitAnd => self.emit(OpCode::BitAnd),
                    BinaryOp::BitOr => self.emit(OpCode::BitOr),
                    BinaryOp::BitXor => self.emit(OpCode::BitXor),
                    BinaryOp::Shl => self.emit(OpCode::Shl),
                    BinaryOp::Shr => self.emit(OpCode::Shr),
                    _ => {}
                }
                Ok(())
            }

            ExprKind::Assign { target, value } => {
                self.compile_expr(value)?;
                self.compile_assignment_target(target)?;
                // Assignment returns Unit (the stored value is consumed by assignment_target)
                Ok(())
            }

            ExprKind::AssignOp { op, target, value } => {
                // target op= value -> target = target op value
                self.compile_expr(target)?;
                self.compile_expr(value)?;
                match op {
                    BinaryOp::Add => self.emit(OpCode::Add),
                    BinaryOp::Sub => self.emit(OpCode::Sub),
                    BinaryOp::Mul => self.emit(OpCode::Mul),
                    BinaryOp::Div => self.emit(OpCode::Div),
                    _ => {}
                }
                self.compile_assignment_target(target)?;
                // Assignment returns the new value (left on stack by assignment_target)
                Ok(())
            }

            ExprKind::Call { callee, args } => {
                // Check if this is an enum variant construction or static method call
                if let ExprKind::Path(path) = &callee.kind {
                    if path.segments.len() == 2 {
                        let raw_type_name = path.segments[0].ident.name.to_string();
                        let type_name = self.resolve_type_alias(&raw_type_name);
                        let method_name = path.segments[1].ident.name.to_string();

                        // First check if it's an enum variant (may need on-demand monomorphization)
                        let enum_info = if let Some(info) = self.enum_types.get(&type_name).cloned()
                        {
                            Some((type_name.clone(), info))
                        } else if self.generic_defs.contains_key(&type_name) {
                            // Generic enum - check if we have instantiation info and monomorphize
                            let call_site_info = self
                                .type_annotations
                                .as_ref()
                                .and_then(|ann| ann.get_call_site(callee.span.start))
                                .map(|(_, type_args)| type_args.clone());

                            if let Some(type_args) = call_site_info {
                                // Monomorphize the enum on-demand
                                let mangled_name = self.mangle_name(&type_name, &type_args);
                                if !self.enum_types.contains_key(&mangled_name) {
                                    if let Some(def) = self.generic_defs.get(&type_name).cloned() {
                                        if let GenericDefKind::Enum(e) = &def.kind {
                                            self.monomorphize_enum(
                                                &type_name,
                                                e,
                                                &def.type_params,
                                                &type_args,
                                            )?;
                                        }
                                    }
                                }
                                self.enum_types
                                    .get(&mangled_name)
                                    .map(|info| (mangled_name, info.clone()))
                            } else {
                                None
                            }
                        } else {
                            None
                        };

                        if let Some((enum_name, info)) = enum_info {
                            if let Some(&(variant_idx, field_count)) =
                                info.variants.get(&method_name)
                            {
                                if field_count == args.len() {
                                    // Compile arguments (variant fields)
                                    for arg in args {
                                        self.compile_expr(&arg.value)?;
                                    }
                                    // Create enum variant
                                    let enum_info_const = Constant::EnumInfo {
                                        name: enum_name,
                                        variants: info.variants_ordered.clone(),
                                    };
                                    let enum_info_idx = self.add_constant(enum_info_const)?;
                                    self.emit(OpCode::Enum);
                                    self.emit_u16(enum_info_idx);
                                    self.emit_byte(variant_idx);
                                    self.emit_byte(field_count as u8);
                                    return Ok(());
                                } else {
                                    return Err(CompileError::Custom(format!(
                                        "Enum variant {}::{} expects {} field(s), got {}",
                                        type_name,
                                        method_name,
                                        field_count,
                                        args.len()
                                    )));
                                }
                            }
                        }

                        // Check if it's a static method call (Type::method)
                        if self.struct_types.contains_key(&type_name) {
                            // Compile arguments first
                            for arg in args {
                                self.compile_expr(&arg.value)?;
                            }
                            // Emit CallStatic opcode
                            let type_name_idx = self.add_constant(Constant::String(type_name))?;
                            let method_name_idx =
                                self.add_constant(Constant::String(method_name))?;
                            self.emit(OpCode::CallStatic);
                            self.emit_u16(type_name_idx);
                            self.emit_u16(method_name_idx);
                            self.emit_byte(args.len() as u8);
                            return Ok(());
                        }
                    }
                }

                // Check for generic function call site resolution
                if let ExprKind::Path(path) = &callee.kind {
                    if path.segments.len() == 1 {
                        let func_name = path.segments[0].ident.name.to_string();

                        // Check if there's a generic instantiation at the callee's span
                        // (type checker records at path span, not call expression span)
                        if let Some((_, type_args)) = self
                            .type_annotations
                            .as_ref()
                            .and_then(|ann| ann.get_call_site(callee.span.start))
                        {
                            // This is a call to a generic function - use the mangled name
                            let mangled_name = self.mangle_name(&func_name, type_args);

                            // Load the mangled function first (function must be on stack before args)
                            let name_idx = self.add_constant(Constant::String(mangled_name))?;
                            self.emit(OpCode::LoadGlobal);
                            self.emit_u16(name_idx);

                            // Then compile arguments
                            for arg in args {
                                self.compile_expr(&arg.value)?;
                            }

                            // Call
                            self.emit(OpCode::Call);
                            self.emit_byte(args.len() as u8);
                            return Ok(());
                        }
                    }
                }

                // Regular function call
                self.compile_expr(callee)?;
                for arg in args {
                    self.compile_expr(&arg.value)?;
                }
                self.emit(OpCode::Call);
                self.emit_byte(args.len() as u8);
                Ok(())
            }

            ExprKind::MethodCall {
                receiver,
                method,
                args,
            } => {
                // Push receiver first, then args
                self.compile_expr(receiver)?;
                for arg in args {
                    self.compile_expr(&arg.value)?;
                }
                // CallMethod [method_name_idx: u16, arg_count: u8]
                // arg_count includes receiver
                let method_name_idx =
                    self.add_constant(Constant::String(method.name.to_string()))?;
                self.emit(OpCode::CallMethod);
                self.emit_u16(method_name_idx);
                self.emit_byte((args.len() + 1) as u8); // +1 for receiver
                Ok(())
            }

            ExprKind::Field { base, field } => {
                self.compile_expr(base)?;
                // Try to parse field as tuple index
                if let Ok(idx) = field.name.parse::<u8>() {
                    self.emit(OpCode::GetField);
                    self.emit_byte(idx);
                } else {
                    // Named field - add field name to constants and use GetFieldNamed
                    let field_name_idx =
                        self.add_constant(Constant::String(field.name.to_string()))?;
                    self.emit(OpCode::GetFieldNamed);
                    self.emit_u16(field_name_idx);
                }
                Ok(())
            }

            ExprKind::Index { base, index } => {
                self.compile_expr(base)?;
                self.compile_expr(index)?;
                self.emit(OpCode::Index);
                Ok(())
            }

            ExprKind::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.compile_expr(condition)?;
                // JumpIfFalse pops the condition, so we don't need an extra Pop
                let then_jump = self.emit_jump(OpCode::JumpIfFalse);

                self.compile_block(then_branch)?;

                if let Some(else_expr) = else_branch {
                    let else_jump = self.emit_jump(OpCode::Jump);
                    self.patch_jump(then_jump)?;
                    self.compile_expr(else_expr)?;
                    self.patch_jump(else_jump)?;
                } else {
                    let else_jump = self.emit_jump(OpCode::Jump);
                    self.patch_jump(then_jump)?;
                    self.emit(OpCode::Unit);
                    self.patch_jump(else_jump)?;
                }
                Ok(())
            }

            ExprKind::Loop { body, .. } => {
                let loop_start = self.chunk().len();
                self.current().loops.push(LoopInfo {
                    start: loop_start,
                    break_jumps: Vec::new(),
                });

                self.compile_block(body)?;
                self.emit(OpCode::Pop); // Discard block value
                self.emit_loop(loop_start)?;

                let loop_info = self.current().loops.pop().unwrap();
                for jump in loop_info.break_jumps {
                    self.patch_jump(jump)?;
                }
                self.emit(OpCode::Unit);
                Ok(())
            }

            ExprKind::While {
                condition, body, ..
            } => {
                let loop_start = self.chunk().len();
                self.current().loops.push(LoopInfo {
                    start: loop_start,
                    break_jumps: Vec::new(),
                });

                self.compile_expr(condition)?;
                let exit_jump = self.emit_jump(OpCode::JumpIfFalse);

                self.compile_block(body)?;
                self.emit(OpCode::Pop);
                self.emit_loop(loop_start)?;

                self.patch_jump(exit_jump)?;
                // JumpIfFalse already popped the condition, no extra Pop needed

                let loop_info = self.current().loops.pop().unwrap();
                for jump in loop_info.break_jumps {
                    self.patch_jump(jump)?;
                }
                self.emit(OpCode::Unit);
                Ok(())
            }

            ExprKind::For {
                pattern,
                iterable,
                body,
                ..
            } => {
                // for x in iter { body }
                // Put iterator in a local so stack positions match locals array
                self.begin_scope();
                self.compile_expr(iterable)?;
                self.emit(OpCode::Iter);
                self.declare_local("", false)?; // Hidden iterator local

                let loop_start = self.chunk().len();
                self.current().loops.push(LoopInfo {
                    start: loop_start,
                    break_jumps: Vec::new(),
                });

                // Load iterator and get next
                let iter_slot = (self.current().locals.len() - 1) as u16;
                self.emit_local_load(iter_slot);
                self.emit(OpCode::IterNext);
                let exit_jump = self.emit_jump(OpCode::JumpIfFalse);

                // Bind loop variable
                self.begin_scope();
                self.compile_pattern_binding(pattern, false)?;
                self.compile_block(body)?;
                self.emit(OpCode::Pop); // Pop block result
                self.end_scope();

                self.emit_loop(loop_start)?;

                self.patch_jump(exit_jump)?;

                let loop_info = self.current().loops.pop().unwrap();
                for jump in loop_info.break_jumps {
                    self.patch_jump(jump)?;
                }
                self.end_scope(); // End iterator scope
                self.emit(OpCode::Unit);
                Ok(())
            }

            ExprKind::Break { value, .. } => {
                if let Some(value) = value {
                    self.compile_expr(value)?;
                } else {
                    self.emit(OpCode::Unit);
                }
                // Pop locals up to loop scope
                // For simplicity, just emit the jump
                let jump = self.emit_jump(OpCode::Jump);
                if let Some(loop_info) = self.current().loops.last_mut() {
                    loop_info.break_jumps.push(jump);
                }
                Ok(())
            }

            ExprKind::Continue { .. } => {
                if let Some(loop_info) = self.current().loops.last() {
                    let loop_start = loop_info.start;
                    self.emit_loop(loop_start)?;
                }
                Ok(())
            }

            ExprKind::Return { value } => {
                if let Some(value) = value {
                    self.compile_expr(value)?;
                } else {
                    self.emit(OpCode::Unit);
                }
                self.emit(OpCode::Return);
                Ok(())
            }

            ExprKind::Block(block) => self.compile_block(block),

            ExprKind::Tuple(elems) => {
                for elem in elems {
                    self.compile_expr(elem)?;
                }
                self.emit(OpCode::Tuple);
                self.emit_byte(elems.len() as u8);
                Ok(())
            }

            ExprKind::Array(elems) => {
                for elem in elems {
                    self.compile_expr(elem)?;
                }
                self.emit(OpCode::Array);
                self.emit_u16(elems.len() as u16);
                Ok(())
            }

            ExprKind::ArrayRepeat { value, count } => {
                self.compile_expr(value)?;
                self.compile_expr(count)?;
                self.emit(OpCode::Array);
                self.emit_u16(0);
                Ok(())
            }

            ExprKind::Closure { params, body, .. } => {
                // Compile closure
                self.states
                    .push(CompilerState::new(None, params.len() as u8));
                self.begin_scope();

                for param in params {
                    self.compile_pattern_binding(&param.pattern, true)?;
                }

                self.compile_expr(body)?;
                self.emit(OpCode::Return);

                self.end_scope();
                let closure_state = self.states.pop().unwrap();
                let upvalues = closure_state.upvalues.clone();

                let proto = FunctionProto {
                    name: closure_state.function_name,
                    arity: closure_state.arity,
                    min_arity: closure_state.arity, // Closures don't have defaults for now
                    chunk: closure_state.chunk,
                    upvalues: closure_state.upvalues,
                    defaults: Vec::new(),
                };

                let idx = self.add_constant(Constant::Function(Box::new(proto)))?;
                self.emit(OpCode::Closure);
                self.emit_u16(idx);
                self.emit_byte(upvalues.len() as u8);
                for uv in &upvalues {
                    self.emit_byte(if uv.is_local { 1 } else { 0 });
                    self.emit_byte(uv.index);
                }
                Ok(())
            }

            ExprKind::Reference { mutable, operand } => {
                // Check if operand is a local variable
                if let ExprKind::Path(path) = &operand.kind {
                    if path.segments.len() == 1 {
                        let name = &path.segments[0].ident.name;
                        if let Some((slot, _)) = self.resolve_local(name) {
                            // Check borrow rules
                            self.check_borrow(slot, *mutable)?;
                            // Record the borrow
                            self.record_borrow(slot, *mutable);
                            // Emit MakeRef opcode
                            self.emit(OpCode::MakeRef);
                            self.emit_u16(slot);
                            self.emit_byte(if *mutable { 1 } else { 0 });
                            return Ok(());
                        }
                    }
                }
                // For non-local references, just compile the operand
                self.compile_expr(operand)
            }

            ExprKind::Dereference { operand } => {
                self.compile_expr(operand)?;
                self.emit(OpCode::Deref);
                Ok(())
            }

            ExprKind::Cast { expr: inner, .. } => self.compile_expr(inner),

            ExprKind::Range {
                start,
                end,
                inclusive,
            } => {
                if let Some(start) = start {
                    self.compile_expr(start)?;
                } else {
                    self.emit_constant(Constant::Int(0))?;
                }
                if let Some(end) = end {
                    self.compile_expr(end)?;
                } else {
                    self.emit_constant(Constant::Int(i64::MAX))?;
                }
                self.emit(OpCode::Range);
                self.emit_byte(if *inclusive { 1 } else { 0 });
                Ok(())
            }

            ExprKind::Try { operand } => {
                self.compile_expr(operand)?;
                self.emit(OpCode::TryUnwrap);
                Ok(())
            }

            ExprKind::Struct { path, fields, .. } => {
                // Get struct name from path, resolving any type aliases
                let raw_name = path
                    .segments
                    .last()
                    .map(|s| s.ident.name.to_string())
                    .unwrap_or_default();
                let base_struct_name = self.resolve_type_alias(&raw_name);

                // Check for generic struct instantiation at this call site
                let struct_name = if let Some((_, type_args)) = self
                    .type_annotations
                    .as_ref()
                    .and_then(|ann| ann.get_call_site(expr.span.start))
                {
                    // This is a generic struct instantiation - use the mangled name
                    self.mangle_name(&base_struct_name, type_args)
                } else {
                    base_struct_name
                };

                // Look up struct type to get field order and defaults
                let (field_order, field_defaults) = self
                    .struct_types
                    .get(&struct_name)
                    .map(|info| (info.fields.clone(), info.defaults.clone()))
                    .unwrap_or_else(|| {
                        // Unknown struct, use fields as provided
                        (
                            fields.iter().map(|f| f.name.name.to_string()).collect(),
                            HashMap::new(),
                        )
                    });

                // Build a map of provided field values
                let mut field_values: HashMap<String, Option<&Expr>> = HashMap::new();
                for field in fields {
                    let value = field.value.as_ref();
                    field_values.insert(field.name.name.to_string(), value);
                }

                // Emit field values in declaration order
                for field_name in &field_order {
                    if let Some(maybe_value) = field_values.get(field_name) {
                        if let Some(value) = maybe_value {
                            self.compile_expr(value)?;
                        } else {
                            // Shorthand: field name is variable name
                            self.load_variable(field_name)?;
                        }
                    } else if let Some(default_expr) = field_defaults.get(field_name) {
                        // Field not provided but has default value
                        self.compile_expr(default_expr)?;
                    } else {
                        // Field not provided and no default, emit unit (will be an error at runtime)
                        self.emit(OpCode::Unit);
                    }
                }

                // Add StructInfo constant
                let struct_info = Constant::StructInfo {
                    name: struct_name,
                    fields: field_order.clone(),
                };
                let struct_info_idx = self.add_constant(struct_info)?;

                // Emit Struct opcode
                self.emit(OpCode::Struct);
                self.emit_u16(struct_info_idx);
                self.emit_byte(field_order.len() as u8);
                Ok(())
            }

            ExprKind::Match { scrutinee, arms } => {
                // Create an outer scope for the match expression
                // Store scrutinee as a hidden local so we can reload it for each arm
                self.begin_scope();
                self.compile_expr(scrutinee)?;
                let scrutinee_slot = self.current().locals.len() as u16;
                self.declare_local("__match_scrutinee__", false)?;

                let mut end_jumps = Vec::new();

                for (i, arm) in arms.iter().enumerate() {
                    let is_last = i == arms.len() - 1;
                    let _has_guard = arm.guard.is_some();

                    // Load scrutinee for pattern check
                    self.emit_local_load(scrutinee_slot);

                    // Pattern check:
                    // - For refutable patterns: leaves [scrutinee, bool] on stack
                    // - For irrefutable patterns: leaves [scrutinee] on stack (no check needed)
                    let needs_check = self.pattern_needs_check(&arm.pattern);
                    let next_arm_jump = if needs_check {
                        self.compile_pattern_check(&arm.pattern)?;
                        // Stack: [..., scrutinee, bool]
                        Some(self.emit_jump(OpCode::JumpIfFalse))
                        // After JumpIfFalse: [..., scrutinee] (bool popped)
                    } else {
                        // Stack: [..., scrutinee]
                        None
                    };

                    // Pattern matched - bind variables and execute body
                    self.begin_scope();
                    let locals_before = self.current().locals.len();
                    self.compile_pattern_binding(&arm.pattern, false)?;
                    let locals_bound = self.current().locals.len() - locals_before;
                    // After binding, scrutinee is consumed or assigned to locals

                    // Handle guard
                    let guard_fail_jump = if let Some(guard) = &arm.guard {
                        self.compile_expr(guard)?;
                        Some(self.emit_jump(OpCode::JumpIfFalse))
                    } else {
                        None
                    };

                    // Execute body - result is pushed
                    self.compile_expr(&arm.body)?;

                    // End inner scope, preserving the body result on top
                    self.end_scope_preserving_top();

                    // Jump to end after successful arm
                    // We need this even for the last arm if there's check_fail code to skip
                    let needs_end_jump =
                        !is_last || guard_fail_jump.is_some() || next_arm_jump.is_some();
                    if needs_end_jump {
                        let jump = self.emit_jump(OpCode::Jump);
                        end_jumps.push(jump);
                    }

                    // Patch guard failure - jump here if guard was false
                    let mut guard_skip_jump = None;
                    if let Some(guard_jump) = guard_fail_jump {
                        self.patch_jump(guard_jump)?;
                        // Guard failed - pop the bound locals
                        for _ in 0..locals_bound {
                            self.emit(OpCode::Pop);
                        }
                        // Jump over the check_fail Pop to avoid double-popping
                        if next_arm_jump.is_some() {
                            guard_skip_jump = Some(self.emit_jump(OpCode::Jump));
                        }
                    }

                    // Patch pattern check failure to next arm
                    if let Some(check_jump) = next_arm_jump {
                        self.patch_jump(check_jump)?;
                        // Pattern check failed - pop the scrutinee copy we loaded
                        self.emit(OpCode::Pop);
                    }

                    // Patch guard skip jump to here (after check_fail code)
                    if let Some(skip_jump) = guard_skip_jump {
                        self.patch_jump(skip_jump)?;
                    }
                }

                for jump in end_jumps {
                    self.patch_jump(jump)?;
                }

                // End outer scope (pops __match_scrutinee__), preserving result
                self.end_scope_preserving_top();
                Ok(())
            }

            ExprKind::Grouped(inner) => self.compile_expr(inner),

            ExprKind::FormatString { parts } => {
                // Compile format string by concatenating parts
                // Strategy: for each concatenation, push function first, then args
                if parts.is_empty() {
                    // Empty format string -> empty string
                    self.emit_constant(Constant::String(String::new()))?;
                    return Ok(());
                }

                // Compile the first part (leaves a string on stack)
                self.compile_format_part(&parts[0])?;

                // For each subsequent part, compile and concatenate
                // Stack before: [result_so_far]
                // We need: [str_concat, result_so_far, new_part] then Call
                for part in &parts[1..] {
                    // Load str_concat function first
                    let concat_idx =
                        self.add_constant(Constant::String("str_concat".to_string()))?;
                    self.emit(OpCode::LoadGlobal);
                    self.emit_u16(concat_idx);
                    // Stack: [result_so_far, str_concat]

                    // Swap to get: [str_concat, result_so_far]
                    self.emit(OpCode::Swap);

                    // Compile the new part
                    self.compile_format_part(part)?;
                    // Stack: [str_concat, result_so_far, new_part]

                    // Call str_concat(result_so_far, new_part)
                    self.emit(OpCode::Call);
                    self.emit_byte(2);
                    // Stack: [concatenated_result]
                }

                Ok(())
            }

            ExprKind::Await { operand } => {
                // Compile the operand (should be a Future)
                self.compile_expr(operand)?;
                // Await the Future
                self.emit(OpCode::Await);
                Ok(())
            }

            ExprKind::AsyncBlock(block) => {
                // Compile async block similar to a closure with 0 parameters
                self.states.push(CompilerState::new(
                    Some("<async>".to_string()),
                    0,
                ));
                self.begin_scope();

                // Compile the block body
                self.compile_block(block)?;
                self.emit(OpCode::Return);

                self.end_scope();
                let async_state = self.states.pop().unwrap();
                let upvalues = async_state.upvalues.clone();

                let proto = FunctionProto {
                    name: async_state.function_name,
                    arity: 0,
                    min_arity: 0,
                    chunk: async_state.chunk,
                    upvalues: async_state.upvalues,
                    defaults: Vec::new(),
                };

                let idx = self.add_constant(Constant::Function(Box::new(proto)))?;
                self.emit(OpCode::CreateFuture);
                self.emit_u16(idx);
                self.emit_byte(upvalues.len() as u8);
                for uv in &upvalues {
                    self.emit_byte(if uv.is_local { 1 } else { 0 });
                    self.emit_byte(uv.index);
                }
                Ok(())
            }
        }
    }

    /// Compile a single format string part, leaving a string value on the stack.
    fn compile_format_part(&mut self, part: &FormatPart) -> Result<(), CompileError> {
        match part {
            FormatPart::Literal(s) => {
                self.emit_constant(Constant::String(s.clone()))?;
            }
            FormatPart::Expr(expr) => {
                // Load the to_string function first (Call expects callee before args)
                let idx = self.add_constant(Constant::String("to_string".to_string()))?;
                self.emit(OpCode::LoadGlobal);
                self.emit_u16(idx);
                // Then compile the expression (the argument)
                self.compile_expr(expr)?;
                // Call to_string(expr)
                self.emit(OpCode::Call);
                self.emit_byte(1);
            }
        }
        Ok(())
    }

    fn compile_literal(&mut self, lit: &Literal) -> Result<(), CompileError> {
        match lit {
            Literal::Int(n) => self.emit_constant(Constant::Int(*n)),
            Literal::Float(n) => self.emit_constant(Constant::Float(*n)),
            Literal::String(s) => self.emit_constant(Constant::String(s.clone())),
            Literal::Char(c) => self.emit_constant(Constant::Int(*c as i64)),
            Literal::Bool(true) => {
                self.emit(OpCode::True);
                Ok(())
            }
            Literal::Bool(false) => {
                self.emit(OpCode::False);
                Ok(())
            }
            Literal::Unit => {
                self.emit(OpCode::Unit);
                Ok(())
            }
        }
    }

    /// Load a variable by name onto the stack.
    fn load_variable(&mut self, name: &str) -> Result<(), CompileError> {
        // Check for local variable
        if let Some((slot, _)) = self.resolve_local(name) {
            self.emit_local_load(slot);
            return Ok(());
        }

        // Check for upvalue
        if let Some(idx) = self.resolve_upvalue(name) {
            self.emit(OpCode::LoadUpvalue);
            self.emit_byte(idx);
            return Ok(());
        }

        // Check if this name was imported via `use`
        let global_name = if let Some(full_path) = self.module_imports.get(name) {
            full_path.clone()
        } else {
            name.to_string()
        };

        // Global variable
        let name_idx = self.add_constant(Constant::String(global_name))?;
        self.emit(OpCode::LoadGlobal);
        self.emit_u16(name_idx);
        Ok(())
    }

    fn compile_path(&mut self, path: &ExprPath) -> Result<(), CompileError> {
        if path.segments.len() == 1 {
            let name = &path.segments[0].ident.name;
            self.load_variable(name)
        } else if path.segments.len() == 2 {
            // Could be an enum variant like Color::Red or module item like mymod::foo
            let raw_first = path.segments[0].ident.name.to_string();
            let first = self.resolve_type_alias(&raw_first);
            let second = path.segments[1].ident.name.to_string();

            // Check if it's an enum variant (may need on-demand monomorphization for generics)
            let enum_info = if let Some(info) = self.enum_types.get(&first).cloned() {
                Some((first.clone(), info))
            } else if self.generic_defs.contains_key(&first) {
                // Generic enum - check for instantiation info and monomorphize
                let call_site_info = self
                    .type_annotations
                    .as_ref()
                    .and_then(|ann| ann.get_call_site(path.segments[0].ident.span.start))
                    .map(|(_, type_args)| type_args.clone());

                if let Some(type_args) = call_site_info {
                    let mangled_name = self.mangle_name(&first, &type_args);
                    if !self.enum_types.contains_key(&mangled_name) {
                        if let Some(def) = self.generic_defs.get(&first).cloned() {
                            if let GenericDefKind::Enum(e) = &def.kind {
                                self.monomorphize_enum(&first, e, &def.type_params, &type_args)?;
                            }
                        }
                    }
                    self.enum_types
                        .get(&mangled_name)
                        .map(|info| (mangled_name, info.clone()))
                } else {
                    None
                }
            } else {
                None
            };

            if let Some((enum_name, info)) = enum_info {
                if let Some(&(variant_idx, field_count)) = info.variants.get(&second) {
                    if field_count == 0 {
                        // Unit variant - create directly
                        let enum_info_const = Constant::EnumInfo {
                            name: enum_name,
                            variants: info.variants_ordered.clone(),
                        };
                        let enum_info_idx = self.add_constant(enum_info_const)?;
                        self.emit(OpCode::Enum);
                        self.emit_u16(enum_info_idx);
                        self.emit_byte(variant_idx);
                        self.emit_byte(0); // no fields for unit variant
                        return Ok(());
                    } else {
                        return Err(CompileError::Custom(format!(
                            "Enum variant {}::{} requires {} field(s)",
                            first, second, field_count
                        )));
                    }
                }
            }

            // Not an enum - treat as module path (first::second)
            let full_path = format!("{}::{}", first, second);
            let name_idx = self.add_constant(Constant::String(full_path))?;
            self.emit(OpCode::LoadGlobal);
            self.emit_u16(name_idx);
            Ok(())
        } else {
            // Multi-segment path: a::b::c -> load global "a::b::c"
            let full_path: String = path
                .segments
                .iter()
                .map(|s| s.ident.name.to_string())
                .collect::<Vec<_>>()
                .join("::");
            let name_idx = self.add_constant(Constant::String(full_path))?;
            self.emit(OpCode::LoadGlobal);
            self.emit_u16(name_idx);
            Ok(())
        }
    }

    fn compile_assignment_target(&mut self, target: &Expr) -> Result<(), CompileError> {
        match &target.kind {
            ExprKind::Path(path) if path.segments.len() == 1 => {
                let name = &path.segments[0].ident.name;

                if let Some((slot, _)) = self.resolve_local(name) {
                    if slot <= 255 {
                        self.emit(OpCode::StoreLocalSmall);
                        self.emit_byte(slot as u8);
                    } else {
                        self.emit(OpCode::StoreLocal);
                        self.emit_u16(slot);
                    }
                    return Ok(());
                }

                // Check for upvalue (captured variable from enclosing scope)
                if let Some(idx) = self.resolve_upvalue(name) {
                    self.emit(OpCode::StoreUpvalue);
                    self.emit_byte(idx);
                    return Ok(());
                }

                // Global
                let name_idx = self.add_constant(Constant::String(name.to_string()))?;
                self.emit(OpCode::StoreGlobal);
                self.emit_u16(name_idx);
                Ok(())
            }
            ExprKind::Field { base, field } => {
                self.compile_expr(base)?;
                if let Ok(idx) = field.name.parse::<u8>() {
                    self.emit(OpCode::SetField);
                    self.emit_byte(idx);
                } else {
                    // Named field - add field name to constants and use SetFieldNamed
                    let field_name_idx =
                        self.add_constant(Constant::String(field.name.to_string()))?;
                    self.emit(OpCode::SetFieldNamed);
                    self.emit_u16(field_name_idx);
                }
                Ok(())
            }
            ExprKind::Index { base, index } => {
                self.compile_expr(base)?;
                self.compile_expr(index)?;
                self.emit(OpCode::IndexSet);
                Ok(())
            }
            ExprKind::Dereference { operand } => {
                // *r = value -> DerefStore
                // Stack before: value
                // We need: ref, value
                self.compile_expr(operand)?;
                self.emit(OpCode::Swap);
                self.emit(OpCode::DerefStore);
                Ok(())
            }
            _ => Err(CompileError::Custom(
                "Invalid assignment target".to_string(),
            )),
        }
    }
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use squam_parser::Parser;

    fn compile(source: &str) -> Result<FunctionProto, CompileError> {
        let mut parser = Parser::new(source, 0);
        let module = parser.parse_module();
        assert!(
            parser.errors().is_empty(),
            "Parse errors: {:?}",
            parser.errors()
        );

        let mut compiler = Compiler::new();
        compiler.compile_module(&module)
    }

    /// Find the first function constant in a module's chunk
    fn get_first_function(proto: &FunctionProto) -> Option<&FunctionProto> {
        for constant in &proto.chunk.constants {
            if let Constant::Function(func) = constant {
                return Some(func);
            }
        }
        None
    }

    #[test]
    fn test_simple_function() {
        let proto = compile("fn main() { 42 }").unwrap();
        let main_func = get_first_function(&proto).expect("Should have main function");
        assert!(!main_func.chunk.code.is_empty());
    }

    #[test]
    fn test_arithmetic() {
        let proto = compile("fn main() { 1 + 2 * 3 }").unwrap();
        let main_func = get_first_function(&proto).expect("Should have main function");
        let disasm = main_func.chunk.disassemble("main");
        assert!(
            disasm.contains("ADD") || disasm.contains("MUL"),
            "Disassembly:\n{}",
            disasm
        );
    }

    #[test]
    fn test_local_variables() {
        let proto = compile("fn main() { let x = 1; let y = 2; x + y }").unwrap();
        let main_func = get_first_function(&proto).expect("Should have main function");
        let disasm = main_func.chunk.disassemble("main");
        assert!(
            disasm.contains("LOAD_LOCAL") || disasm.contains("STORE"),
            "Disassembly:\n{}",
            disasm
        );
    }

    #[test]
    fn test_if_expression() {
        let proto = compile("fn main() { if true { 1 } else { 2 } }").unwrap();
        let main_func = get_first_function(&proto).expect("Should have main function");
        let disasm = main_func.chunk.disassemble("main");
        assert!(disasm.contains("JUMP"), "Disassembly:\n{}", disasm);
    }

    #[test]
    fn test_while_loop() {
        let proto = compile("fn main() { let mut x = 0; while x < 10 { x = x + 1; } }").unwrap();
        let main_func = get_first_function(&proto).expect("Should have main function");
        let disasm = main_func.chunk.disassemble("main");
        assert!(disasm.contains("LOOP"), "Disassembly:\n{}", disasm);
    }
}
