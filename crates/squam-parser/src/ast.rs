use squam_lexer::Span;
use std::sync::Arc;

/// An interned string symbol.
pub type Symbol = Arc<str>;

// ---
// Module
// ---

/// A complete Squam module (file).
#[derive(Debug, Clone)]
pub struct Module {
    pub items: Vec<Item>,
    pub span: Span,
}

// ---
// Items (Top-level declarations)
// ---

/// A top-level item in a module.
#[derive(Debug, Clone)]
pub enum Item {
    Function(FunctionDef),
    Struct(StructDef),
    Enum(EnumDef),
    Impl(ImplBlock),
    Trait(TraitDef),
    TypeAlias(TypeAlias),
    Const(ConstDef),
    Use(UseDecl),
    Mod(ModDecl),
}

impl Item {
    pub fn span(&self) -> Span {
        match self {
            Item::Function(f) => f.span,
            Item::Struct(s) => s.span,
            Item::Enum(e) => e.span,
            Item::Impl(i) => i.span,
            Item::Trait(t) => t.span,
            Item::TypeAlias(t) => t.span,
            Item::Const(c) => c.span,
            Item::Use(u) => u.span,
            Item::Mod(m) => m.span,
        }
    }
}

// ---
// Attributes
// ---

/// An attribute like #[derive(Debug, Clone)].
#[derive(Debug, Clone)]
pub struct Attribute {
    pub kind: AttributeKind,
    pub span: Span,
}

/// The kind of attribute.
#[derive(Debug, Clone)]
pub enum AttributeKind {
    /// #[derive(Trait1, Trait2, ...)]
    Derive(Vec<Identifier>),
    /// Generic attribute for future use: #[name(args)]
    Named { name: Identifier, args: Vec<AttributeArg> },
}

/// An argument to an attribute.
#[derive(Debug, Clone)]
pub enum AttributeArg {
    /// A simple identifier: Debug
    Ident(Identifier),
    /// A literal value: 42, "string"
    Literal(Literal),
    /// An assignment: name = value
    Assign { name: Identifier, value: Box<AttributeArg> },
}

/// Visibility modifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum Visibility {
    #[default]
    Private,
    Public,
}

/// An identifier with its source location.
#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: Symbol,
    pub span: Span,
}

impl Identifier {
    pub fn new(name: impl Into<Symbol>, span: Span) -> Self {
        Self { name: name.into(), span }
    }
}

// ---
// Functions
// ---

/// A function definition.
#[derive(Debug, Clone)]
pub struct FunctionDef {
    pub visibility: Visibility,
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub params: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub body: Block,
    pub span: Span,
}

/// A function parameter.
#[derive(Debug, Clone)]
pub struct Parameter {
    pub pattern: Pattern,
    pub ty: Type,
    pub default: Option<Expr>,
    pub span: Span,
}

/// A closure parameter (may omit type).
#[derive(Debug, Clone)]
pub struct ClosureParam {
    pub pattern: Pattern,
    pub ty: Option<Type>,
    pub span: Span,
}

// ---
// Structs & Enums
// ---

/// A struct definition.
#[derive(Debug, Clone)]
pub struct StructDef {
    pub attributes: Vec<Attribute>,
    pub visibility: Visibility,
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub fields: StructFields,
    pub span: Span,
}

/// The fields of a struct.
#[derive(Debug, Clone)]
pub enum StructFields {
    /// Named fields: `{ x: i32, y: i32 }`
    Named(Vec<StructField>),
    /// Tuple fields: `(i32, i32)`
    Tuple(Vec<TupleField>),
    /// Unit struct: no fields
    Unit,
}

/// A named struct field.
#[derive(Debug, Clone)]
pub struct StructField {
    pub visibility: Visibility,
    pub name: Identifier,
    pub ty: Type,
    pub default: Option<Box<Expr>>,
    pub span: Span,
}

/// A tuple struct field.
#[derive(Debug, Clone)]
pub struct TupleField {
    pub visibility: Visibility,
    pub ty: Type,
    pub span: Span,
}

/// An enum definition.
#[derive(Debug, Clone)]
pub struct EnumDef {
    pub attributes: Vec<Attribute>,
    pub visibility: Visibility,
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

/// An enum variant.
#[derive(Debug, Clone)]
pub struct EnumVariant {
    pub name: Identifier,
    pub fields: StructFields,
    pub discriminant: Option<Box<Expr>>,
    pub span: Span,
}

// ---
// Impl & Trait
// ---

/// An impl block.
#[derive(Debug, Clone)]
pub struct ImplBlock {
    pub generics: Option<Generics>,
    pub trait_: Option<TypePath>,
    pub self_ty: Type,
    pub items: Vec<ImplItem>,
    pub span: Span,
}

/// An item inside an impl block.
#[derive(Debug, Clone)]
pub enum ImplItem {
    Function(FunctionDef),
    Const(ConstDef),
    Type(TypeAlias),
}

/// A trait definition.
#[derive(Debug, Clone)]
pub struct TraitDef {
    pub visibility: Visibility,
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub bounds: Vec<TypePath>,
    pub items: Vec<TraitItem>,
    pub span: Span,
}

/// An item inside a trait definition.
#[derive(Debug, Clone)]
pub enum TraitItem {
    Function(TraitFunction),
    Const(TraitConst),
    Type(TraitType),
}

/// A function signature in a trait.
#[derive(Debug, Clone)]
pub struct TraitFunction {
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub params: Vec<Parameter>,
    pub return_type: Option<Type>,
    pub default: Option<Block>,
    pub span: Span,
}

/// A const in a trait.
#[derive(Debug, Clone)]
pub struct TraitConst {
    pub name: Identifier,
    pub ty: Type,
    pub default: Option<Expr>,
    pub span: Span,
}

/// An associated type in a trait.
#[derive(Debug, Clone)]
pub struct TraitType {
    pub name: Identifier,
    pub bounds: Vec<TypePath>,
    pub default: Option<Type>,
    pub span: Span,
}

// ---
// Type Alias & Const
// ---

/// A type alias: `type Foo = Bar`
#[derive(Debug, Clone)]
pub struct TypeAlias {
    pub visibility: Visibility,
    pub name: Identifier,
    pub generics: Option<Generics>,
    pub ty: Type,
    pub span: Span,
}

/// A const definition: `const FOO: i32 = 42`
#[derive(Debug, Clone)]
pub struct ConstDef {
    pub visibility: Visibility,
    pub name: Identifier,
    pub ty: Type,
    pub value: Expr,
    pub span: Span,
}

// ---
// Use & Mod
// ---

/// A use declaration: `use foo::bar`
#[derive(Debug, Clone)]
pub struct UseDecl {
    pub visibility: Visibility,
    pub tree: UseTree,
    pub span: Span,
}

/// A use tree.
#[derive(Debug, Clone)]
pub enum UseTree {
    /// Simple path: `foo::bar`
    Path { path: Vec<Identifier>, alias: Option<Identifier> },
    /// Glob: `foo::*`
    Glob { path: Vec<Identifier> },
    /// Nested: `foo::{bar, baz}`
    Nested { path: Vec<Identifier>, items: Vec<UseTree> },
}

/// A module declaration.
#[derive(Debug, Clone)]
pub struct ModDecl {
    pub visibility: Visibility,
    pub name: Identifier,
    pub items: Option<Vec<Item>>,
    pub span: Span,
}

// ---
// Generics
// ---

/// Generic parameters: `<T, U: Clone>`
#[derive(Debug, Clone)]
pub struct Generics {
    pub params: Vec<GenericParam>,
    pub where_clause: Option<WhereClause>,
    pub span: Span,
}

/// A generic parameter.
#[derive(Debug, Clone)]
pub enum GenericParam {
    Type(TypeParam),
    Const(ConstParam),
}

/// A type parameter: `T: Clone`
#[derive(Debug, Clone)]
pub struct TypeParam {
    pub name: Identifier,
    pub bounds: Vec<TypePath>,
    pub default: Option<Type>,
    pub span: Span,
}

/// A const parameter: `const N: usize`
#[derive(Debug, Clone)]
pub struct ConstParam {
    pub name: Identifier,
    pub ty: Type,
    pub default: Option<Expr>,
    pub span: Span,
}

/// A where clause.
#[derive(Debug, Clone)]
pub struct WhereClause {
    pub predicates: Vec<WherePredicate>,
    pub span: Span,
}

/// A where predicate: `T: Clone`
#[derive(Debug, Clone)]
pub struct WherePredicate {
    pub ty: Type,
    pub bounds: Vec<TypePath>,
    pub span: Span,
}

// ---
// Types
// ---

/// A type expression.
#[derive(Debug, Clone)]
pub struct Type {
    pub kind: TypeKind,
    pub span: Span,
}

/// The kind of a type expression.
#[derive(Debug, Clone)]
pub enum TypeKind {
    /// A path type: `Foo`, `std::vec::Vec<T>`
    Path(TypePath),
    /// A reference type: `&T`, `&mut T`
    Reference { mutable: bool, inner: Box<Type> },
    /// An array type: `[T; N]`
    Array { element: Box<Type>, size: Box<Expr> },
    /// A slice type: `[T]`
    Slice { element: Box<Type> },
    /// A tuple type: `(T, U)`
    Tuple(Vec<Type>),
    /// A function type: `fn(T) -> U`
    Function { params: Vec<Type>, return_type: Option<Box<Type>> },
    /// Inferred type: `_`
    Infer,
    /// Never type: `!`
    Never,
    /// Unit type: `()`
    Unit,
}

/// A type path: `std::vec::Vec<T>`
#[derive(Debug, Clone)]
pub struct TypePath {
    pub segments: Vec<PathSegment>,
    pub span: Span,
}

/// A segment in a path.
#[derive(Debug, Clone)]
pub struct PathSegment {
    pub ident: Identifier,
    pub args: Option<GenericArgs>,
}

/// Generic arguments: `<T, U>`
#[derive(Debug, Clone)]
pub struct GenericArgs {
    pub args: Vec<GenericArg>,
    pub span: Span,
}

/// A generic argument.
#[derive(Debug, Clone)]
pub enum GenericArg {
    Type(Type),
    Const(Expr),
}

// ---
// Expressions
// ---

/// An expression.
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

/// The kind of an expression.
#[derive(Debug, Clone)]
pub enum ExprKind {
    // Literals
    /// A literal value
    Literal(Literal),
    /// A path expression: `foo`, `Foo::bar`
    Path(ExprPath),

    // Operators
    /// Unary operation: `-x`, `!x`
    Unary { op: UnaryOp, operand: Box<Expr> },
    /// Binary operation: `a + b`
    Binary { op: BinaryOp, left: Box<Expr>, right: Box<Expr> },

    // Assignment
    /// Assignment: `x = y`
    Assign { target: Box<Expr>, value: Box<Expr> },
    /// Compound assignment: `x += y`
    AssignOp { op: BinaryOp, target: Box<Expr>, value: Box<Expr> },

    // Access
    /// Field access: `x.foo`
    Field { base: Box<Expr>, field: Identifier },
    /// Index access: `x[i]`
    Index { base: Box<Expr>, index: Box<Expr> },

    // Calls
    /// Function call: `f(x, y)` or `f(name: x, age: y)`
    Call { callee: Box<Expr>, args: Vec<CallArg> },
    /// Method call: `x.foo(y)` or `x.foo(name: y)`
    MethodCall { receiver: Box<Expr>, method: Identifier, args: Vec<CallArg> },

    // Constructors
    /// Struct literal: `Point { x: 1, y: 2 }`
    Struct { path: TypePath, fields: Vec<StructExprField>, rest: Option<Box<Expr>> },
    /// Tuple expression: `(a, b)`
    Tuple(Vec<Expr>),
    /// Array expression: `[1, 2, 3]`
    Array(Vec<Expr>),
    /// Array repeat: `[0; 10]`
    ArrayRepeat { value: Box<Expr>, count: Box<Expr> },

    // Control flow
    /// If expression: `if cond { ... } else { ... }`
    If { condition: Box<Expr>, then_branch: Block, else_branch: Option<Box<Expr>> },
    /// Match expression
    Match { scrutinee: Box<Expr>, arms: Vec<MatchArm> },
    /// Loop: `loop { ... }`
    Loop { label: Option<Identifier>, body: Block },
    /// While loop: `while cond { ... }`
    While { label: Option<Identifier>, condition: Box<Expr>, body: Block },
    /// For loop: `for x in iter { ... }`
    For { label: Option<Identifier>, pattern: Pattern, iterable: Box<Expr>, body: Block },

    // Jumps
    /// Break: `break`, `break 'label`, `break value`
    Break { label: Option<Identifier>, value: Option<Box<Expr>> },
    /// Continue: `continue`, `continue 'label`
    Continue { label: Option<Identifier> },
    /// Return: `return`, `return value`
    Return { value: Option<Box<Expr>> },

    // Closures
    /// Closure: `|x, y| x + y`
    Closure { params: Vec<ClosureParam>, return_type: Option<Box<Type>>, body: Box<Expr> },

    // References
    /// Reference: `&x`, `&mut x`
    Reference { mutable: bool, operand: Box<Expr> },
    /// Dereference: `*x`
    Dereference { operand: Box<Expr> },

    // Misc
    /// Block expression: `{ ... }`
    Block(Block),
    /// Range: `a..b`, `a..=b`, `..b`, `a..`
    Range { start: Option<Box<Expr>>, end: Option<Box<Expr>>, inclusive: bool },
    /// Try operator: `x?`
    Try { operand: Box<Expr> },
    /// Type cast: `x as T`
    Cast { expr: Box<Expr>, ty: Type },
    /// Grouped expression: `(expr)` - for AST, this is transparent
    Grouped(Box<Expr>),
    /// Format string: `f"Hello {name}!"`
    FormatString { parts: Vec<FormatPart> },
}

/// A part of a format string.
#[derive(Debug, Clone)]
pub enum FormatPart {
    /// A literal string segment
    Literal(String),
    /// An interpolated expression
    Expr(Box<Expr>),
}

/// A path in expression context.
#[derive(Debug, Clone)]
pub struct ExprPath {
    pub segments: Vec<PathSegment>,
    pub span: Span,
}

/// A field in a struct literal.
#[derive(Debug, Clone)]
pub struct StructExprField {
    pub name: Identifier,
    pub value: Option<Expr>,
    pub span: Span,
}

/// A function call argument (positional or named).
#[derive(Debug, Clone)]
pub struct CallArg {
    pub name: Option<Identifier>,
    pub value: Expr,
    pub span: Span,
}

/// A match arm.
#[derive(Debug, Clone)]
pub struct MatchArm {
    pub pattern: Pattern,
    pub guard: Option<Box<Expr>>,
    pub body: Expr,
    pub span: Span,
}

// ---
// Statements
// ---

/// A block of statements.
#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

/// A statement.
#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub span: Span,
}

/// The kind of a statement.
#[derive(Debug, Clone)]
pub enum StmtKind {
    /// Let binding: `let x = 1;`
    Let { pattern: Pattern, ty: Option<Type>, init: Option<Expr> },
    /// Expression with semicolon: `expr;`
    Expr(Expr),
    /// Expression without semicolon (tail expression)
    ExprNoSemi(Expr),
    /// Item declaration inside a block
    Item(Box<Item>),
    /// Empty statement: `;`
    Empty,
}

// ---
// Patterns
// ---

/// A pattern.
#[derive(Debug, Clone)]
pub struct Pattern {
    pub kind: PatternKind,
    pub span: Span,
}

/// The kind of a pattern.
#[derive(Debug, Clone)]
pub enum PatternKind {
    /// Wildcard: `_`
    Wildcard,
    /// Binding: `x`, `mut x`, `ref x`, `x @ pat`
    Binding { mutable: bool, by_ref: bool, name: Identifier, subpattern: Option<Box<Pattern>> },
    /// Literal: `42`, `"foo"`
    Literal(Literal),
    /// Path pattern: `Foo::Bar`
    Path(ExprPath),
    /// Tuple pattern: `(a, b)`
    Tuple(Vec<Pattern>),
    /// Struct pattern: `Point { x, y }`
    Struct { path: TypePath, fields: Vec<PatternField>, rest: bool },
    /// Tuple struct pattern: `Some(x)`
    TupleStruct { path: TypePath, fields: Vec<Pattern> },
    /// Slice pattern: `[a, b, ..]`
    Slice(Vec<Pattern>),
    /// Or pattern: `A | B`
    Or(Vec<Pattern>),
    /// Reference pattern: `&x`, `&mut x`
    Reference { mutable: bool, pattern: Box<Pattern> },
    /// Rest pattern: `..`
    Rest,
    /// Range pattern: `0..10`, `'a'..='z'`
    Range { start: Option<Box<Expr>>, end: Option<Box<Expr>>, inclusive: bool },
}

/// A field in a struct pattern.
#[derive(Debug, Clone)]
pub struct PatternField {
    pub name: Identifier,
    pub pattern: Option<Pattern>,
    pub span: Span,
}

// ---
// Literals & Operators
// ---

/// A literal value.
#[derive(Debug, Clone)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Char(char),
    Bool(bool),
    Unit,
}

/// A unary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    /// `-`
    Neg,
    /// `!`
    Not,
    /// `~`
    BitNot,
}

impl UnaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "!",
            UnaryOp::BitNot => "~",
        }
    }
}

/// A binary operator.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    // Logical
    And,
    Or,
    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

impl BinaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Rem => "%",
            BinaryOp::Eq => "==",
            BinaryOp::Ne => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Le => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Ge => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or => "||",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::Shl => "<<",
            BinaryOp::Shr => ">>",
        }
    }

    /// Returns true if this is a comparison operator.
    pub fn is_comparison(&self) -> bool {
        matches!(self, BinaryOp::Eq | BinaryOp::Ne | BinaryOp::Lt | BinaryOp::Le | BinaryOp::Gt | BinaryOp::Ge)
    }

    /// Returns true if this is a logical operator.
    pub fn is_logical(&self) -> bool {
        matches!(self, BinaryOp::And | BinaryOp::Or)
    }
}