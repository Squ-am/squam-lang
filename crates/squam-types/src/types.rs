use std::sync::Arc;

/// A unique identifier for an interned type.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub u32);

impl TypeId {
    pub const UNIT: TypeId = TypeId(0);
    pub const BOOL: TypeId = TypeId(1);
    pub const I8: TypeId = TypeId(2);
    pub const I16: TypeId = TypeId(3);
    pub const I32: TypeId = TypeId(4);
    pub const I64: TypeId = TypeId(5);
    pub const I128: TypeId = TypeId(6);
    pub const ISIZE: TypeId = TypeId(7);
    pub const U8: TypeId = TypeId(8);
    pub const U16: TypeId = TypeId(9);
    pub const U32: TypeId = TypeId(10);
    pub const U64: TypeId = TypeId(11);
    pub const U128: TypeId = TypeId(12);
    pub const USIZE: TypeId = TypeId(13);
    pub const F32: TypeId = TypeId(14);
    pub const F64: TypeId = TypeId(15);
    pub const CHAR: TypeId = TypeId(16);
    pub const STR: TypeId = TypeId(17);
    pub const STRING: TypeId = TypeId(18);
    pub const NEVER: TypeId = TypeId(19);
    pub const ERROR: TypeId = TypeId(20);
    /// Dynamic/any type for stdlib functions
    pub const ANY: TypeId = TypeId(21);

    /// First ID available for user-defined types
    pub const FIRST_USER: u32 = 22;
}

/// An interned string symbol.
pub type Symbol = Arc<str>;

/// A type variable for inference.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct InferVar(pub u32);

/// A generic type variable.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GenericVar(pub u32);

/// The actual type representation.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Ty {
    // Primitives
    Unit,
    Bool,
    I8,
    I16,
    I32,
    I64,
    I128,
    Isize,
    U8,
    U16,
    U32,
    U64,
    U128,
    Usize,
    F32,
    F64,
    Char,
    /// String slice type `str`
    Str,
    /// Owned string type `String`
    String,
    /// The never type `!`
    Never,

    // Compound Types
    /// Tuple type: `(T, U, V)`
    Tuple(Vec<TypeId>),
    /// Array type: `[T; N]`
    Array {
        element: TypeId,
        size: usize,
    },
    /// Slice type: `[T]`
    Slice(TypeId),
    /// Reference type: `&T` or `&mut T`
    Reference {
        mutable: bool,
        inner: TypeId,
    },

    // User-Defined Types
    /// A struct type
    Struct(StructType),
    /// An enum type
    Enum(EnumType),

    // Function Types
    /// Function type: `fn(A, B) -> R`
    Function {
        params: Vec<TypeId>,
        return_type: TypeId,
    },
    /// Closure type (with captured environment)
    Closure {
        params: Vec<TypeId>,
        return_type: TypeId,
        captures: Vec<(Symbol, TypeId)>,
    },

    // Generics & Inference
    /// A generic type parameter: `T`
    Generic(GenericVar),
    /// A type with generic arguments applied: `Vec<T>`
    Applied {
        base: TypeId,
        args: Vec<TypeId>,
    },
    /// An inference variable (to be resolved)
    Infer(InferVar),

    /// Error type (for error recovery)
    Error,

    /// Any type (for dynamically-typed stdlib functions)
    Any,
}

/// A struct type definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructType {
    pub name: Symbol,
    pub fields: Vec<StructField>,
    pub generic_params: Vec<GenericParam>,
}

/// A field in a struct.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub name: Symbol,
    pub ty: TypeId,
    pub is_public: bool,
}

/// An enum type definition.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumType {
    pub name: Symbol,
    pub variants: Vec<EnumVariant>,
    pub generic_params: Vec<GenericParam>,
}

/// A variant in an enum.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct EnumVariant {
    pub name: Symbol,
    pub fields: VariantFields,
}

/// Fields of an enum variant.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VariantFields {
    /// Unit variant: `None`
    Unit,
    /// Tuple variant: `Some(T)`
    Tuple(Vec<TypeId>),
    /// Struct variant: `Point { x: i32, y: i32 }`
    Struct(Vec<StructField>),
}

/// A generic type parameter.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct GenericParam {
    pub name: Symbol,
    pub var: GenericVar,
    pub bounds: Vec<TypeId>, // Trait bounds
}

impl Ty {
    /// Check if this type is a primitive.
    pub fn is_primitive(&self) -> bool {
        matches!(
            self,
            Ty::Unit
                | Ty::Bool
                | Ty::I8
                | Ty::I16
                | Ty::I32
                | Ty::I64
                | Ty::I128
                | Ty::Isize
                | Ty::U8
                | Ty::U16
                | Ty::U32
                | Ty::U64
                | Ty::U128
                | Ty::Usize
                | Ty::F32
                | Ty::F64
                | Ty::Char
                | Ty::Str
                | Ty::String
        )
    }

    /// Check if this type is an integer type.
    pub fn is_integer(&self) -> bool {
        matches!(
            self,
            Ty::I8
                | Ty::I16
                | Ty::I32
                | Ty::I64
                | Ty::I128
                | Ty::Isize
                | Ty::U8
                | Ty::U16
                | Ty::U32
                | Ty::U64
                | Ty::U128
                | Ty::Usize
        )
    }

    /// Check if this type is a signed integer.
    pub fn is_signed_integer(&self) -> bool {
        matches!(
            self,
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::I128 | Ty::Isize
        )
    }

    /// Check if this type is an unsigned integer.
    pub fn is_unsigned_integer(&self) -> bool {
        matches!(
            self,
            Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64 | Ty::U128 | Ty::Usize
        )
    }

    /// Check if this type is a floating point type.
    pub fn is_float(&self) -> bool {
        matches!(self, Ty::F32 | Ty::F64)
    }

    /// Check if this type is numeric.
    pub fn is_numeric(&self) -> bool {
        self.is_integer() || self.is_float()
    }

    /// Check if this type contains inference variables.
    pub fn has_infer_vars(&self) -> bool {
        matches!(self, Ty::Infer(_))
    }
}
