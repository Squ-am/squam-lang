use rustc_hash::FxHashMap;
use crate::types::{Ty, TypeId, InferVar, GenericVar};

/// The type context manages type interning and provides access to common types.
pub struct TypeContext {
    /// All interned types
    types: Vec<Ty>,
    /// Map from type to its ID for deduplication
    intern_map: FxHashMap<Ty, TypeId>,
    /// Counter for inference variables
    next_infer: u32,
    /// Counter for generic variables
    next_generic: u32,
}

impl TypeContext {
    /// Create a new type context with primitive types pre-registered.
    pub fn new() -> Self {
        let mut ctx = Self {
            types: Vec::new(),
            intern_map: FxHashMap::default(),
            next_infer: 0,
            next_generic: 0,
        };

        // Register primitive types in order matching TypeId constants
        ctx.register_primitive(Ty::Unit);      // 0
        ctx.register_primitive(Ty::Bool);      // 1
        ctx.register_primitive(Ty::I8);        // 2
        ctx.register_primitive(Ty::I16);       // 3
        ctx.register_primitive(Ty::I32);       // 4
        ctx.register_primitive(Ty::I64);       // 5
        ctx.register_primitive(Ty::I128);      // 6
        ctx.register_primitive(Ty::Isize);     // 7
        ctx.register_primitive(Ty::U8);        // 8
        ctx.register_primitive(Ty::U16);       // 9
        ctx.register_primitive(Ty::U32);       // 10
        ctx.register_primitive(Ty::U64);       // 11
        ctx.register_primitive(Ty::U128);      // 12
        ctx.register_primitive(Ty::Usize);     // 13
        ctx.register_primitive(Ty::F32);       // 14
        ctx.register_primitive(Ty::F64);       // 15
        ctx.register_primitive(Ty::Char);      // 16
        ctx.register_primitive(Ty::Str);       // 17
        ctx.register_primitive(Ty::String);    // 18
        ctx.register_primitive(Ty::Never);     // 19
        ctx.register_primitive(Ty::Error);     // 20
        ctx.register_primitive(Ty::Any);       // 21

        ctx
    }

    /// Register a primitive type (internal use).
    fn register_primitive(&mut self, ty: Ty) -> TypeId {
        let id = TypeId(self.types.len() as u32);
        self.intern_map.insert(ty.clone(), id);
        self.types.push(ty);
        id
    }

    /// Intern a type, returning its unique ID.
    pub fn intern(&mut self, ty: Ty) -> TypeId {
        if let Some(&id) = self.intern_map.get(&ty) {
            return id;
        }

        let id = TypeId(self.types.len() as u32);
        self.intern_map.insert(ty.clone(), id);
        self.types.push(ty);
        id
    }

    /// Get the type for an ID.
    pub fn get(&self, id: TypeId) -> &Ty {
        &self.types[id.0 as usize]
    }

    /// Create a fresh inference variable.
    pub fn fresh_infer(&mut self) -> TypeId {
        let var = InferVar(self.next_infer);
        self.next_infer += 1;
        self.intern(Ty::Infer(var))
    }

    /// Create a fresh generic variable.
    pub fn fresh_generic(&mut self) -> GenericVar {
        let var = GenericVar(self.next_generic);
        self.next_generic += 1;
        var
    }

    /// Get the inference variable from a type ID, if it is one.
    pub fn as_infer_var(&self, id: TypeId) -> Option<InferVar> {
        match self.get(id) {
            Ty::Infer(var) => Some(*var),
            _ => None,
        }
    }

    // Common type accessors

    pub fn unit(&self) -> TypeId {
        TypeId::UNIT
    }

    pub fn bool(&self) -> TypeId {
        TypeId::BOOL
    }

    pub fn i8(&self) -> TypeId {
        TypeId::I8
    }

    pub fn i16(&self) -> TypeId {
        TypeId::I16
    }

    pub fn i32(&self) -> TypeId {
        TypeId::I32
    }

    pub fn i64(&self) -> TypeId {
        TypeId::I64
    }

    pub fn i128(&self) -> TypeId {
        TypeId::I128
    }

    pub fn isize(&self) -> TypeId {
        TypeId::ISIZE
    }

    pub fn u8(&self) -> TypeId {
        TypeId::U8
    }

    pub fn u16(&self) -> TypeId {
        TypeId::U16
    }

    pub fn u32(&self) -> TypeId {
        TypeId::U32
    }

    pub fn u64(&self) -> TypeId {
        TypeId::U64
    }

    pub fn u128(&self) -> TypeId {
        TypeId::U128
    }

    pub fn usize(&self) -> TypeId {
        TypeId::USIZE
    }

    pub fn f32(&self) -> TypeId {
        TypeId::F32
    }

    pub fn f64(&self) -> TypeId {
        TypeId::F64
    }

    pub fn char(&self) -> TypeId {
        TypeId::CHAR
    }

    pub fn str(&self) -> TypeId {
        TypeId::STR
    }

    pub fn string(&self) -> TypeId {
        TypeId::STRING
    }

    pub fn never(&self) -> TypeId {
        TypeId::NEVER
    }

    pub fn error(&self) -> TypeId {
        TypeId::ERROR
    }

    // Type constructors

    /// Create a tuple type.
    pub fn tuple(&mut self, elements: Vec<TypeId>) -> TypeId {
        if elements.is_empty() {
            return self.unit();
        }
        self.intern(Ty::Tuple(elements))
    }

    /// Create an array type.
    pub fn array(&mut self, element: TypeId, size: usize) -> TypeId {
        self.intern(Ty::Array { element, size })
    }

    /// Create a slice type.
    pub fn slice(&mut self, element: TypeId) -> TypeId {
        self.intern(Ty::Slice(element))
    }

    /// Create a reference type.
    pub fn reference(&mut self, inner: TypeId, mutable: bool) -> TypeId {
        self.intern(Ty::Reference { mutable, inner })
    }

    /// Create a function type.
    pub fn function(&mut self, params: Vec<TypeId>, return_type: TypeId) -> TypeId {
        self.intern(Ty::Function { params, return_type })
    }

    /// Create a generic type variable.
    pub fn generic(&mut self, var: GenericVar) -> TypeId {
        self.intern(Ty::Generic(var))
    }

    /// Create an applied generic type.
    pub fn applied(&mut self, base: TypeId, args: Vec<TypeId>) -> TypeId {
        if args.is_empty() {
            return base;
        }
        self.intern(Ty::Applied { base, args })
    }

    /// Substitute generic variables in a type with concrete types.
    /// `substitutions` maps GenericVar index to the concrete TypeId.
    pub fn substitute(&mut self, ty: TypeId, substitutions: &[(GenericVar, TypeId)]) -> TypeId {
        match self.get(ty).clone() {
            Ty::Generic(var) => {
                for (sub_var, sub_ty) in substitutions {
                    if var == *sub_var {
                        return *sub_ty;
                    }
                }
                ty // No substitution found, keep as-is
            }
            Ty::Tuple(elems) => {
                let new_elems: Vec<_> = elems
                    .iter()
                    .map(|&e| self.substitute(e, substitutions))
                    .collect();
                self.tuple(new_elems)
            }
            Ty::Array { element, size } => {
                let new_elem = self.substitute(element, substitutions);
                self.array(new_elem, size)
            }
            Ty::Slice(elem) => {
                let new_elem = self.substitute(elem, substitutions);
                self.slice(new_elem)
            }
            Ty::Reference { mutable, inner } => {
                let new_inner = self.substitute(inner, substitutions);
                self.reference(new_inner, mutable)
            }
            Ty::Function { params, return_type } => {
                let new_params: Vec<_> = params
                    .iter()
                    .map(|&p| self.substitute(p, substitutions))
                    .collect();
                let new_ret = self.substitute(return_type, substitutions);
                self.function(new_params, new_ret)
            }
            Ty::Applied { base, args } => {
                let new_base = self.substitute(base, substitutions);
                let new_args: Vec<_> = args
                    .iter()
                    .map(|&a| self.substitute(a, substitutions))
                    .collect();
                self.applied(new_base, new_args)
            }
            // Primitives and other non-generic types stay the same
            _ => ty,
        }
    }

    // Type queries

    /// Check if a type is the error type.
    pub fn is_error(&self, id: TypeId) -> bool {
        id == TypeId::ERROR
    }

    /// Check if a type is the never type.
    pub fn is_never(&self, id: TypeId) -> bool {
        id == TypeId::NEVER
    }

    /// Check if a type is an inference variable.
    pub fn is_infer(&self, id: TypeId) -> bool {
        matches!(self.get(id), Ty::Infer(_))
    }

    /// Get the generic variable if this type is a generic type.
    pub fn as_generic_var(&self, id: TypeId) -> Option<GenericVar> {
        match self.get(id) {
            Ty::Generic(var) => Some(*var),
            _ => None,
        }
    }

    /// Check if a type contains any inference variables (deeply).
    pub fn contains_infer(&self, id: TypeId) -> bool {
        match self.get(id) {
            Ty::Infer(_) => true,
            Ty::Tuple(elems) => elems.iter().any(|&e| self.contains_infer(e)),
            Ty::Array { element, .. } => self.contains_infer(*element),
            Ty::Slice(elem) => self.contains_infer(*elem),
            Ty::Reference { inner, .. } => self.contains_infer(*inner),
            Ty::Function { params, return_type } => {
                params.iter().any(|&p| self.contains_infer(p)) || self.contains_infer(*return_type)
            }
            Ty::Applied { base, args } => {
                self.contains_infer(*base) || args.iter().any(|&a| self.contains_infer(a))
            }
            _ => false,
        }
    }

    /// Get a human-readable name for a type.
    pub fn display(&self, id: TypeId) -> String {
        match self.get(id) {
            Ty::Unit => "()".to_string(),
            Ty::Bool => "bool".to_string(),
            Ty::I8 => "i8".to_string(),
            Ty::I16 => "i16".to_string(),
            Ty::I32 => "i32".to_string(),
            Ty::I64 => "i64".to_string(),
            Ty::I128 => "i128".to_string(),
            Ty::Isize => "isize".to_string(),
            Ty::U8 => "u8".to_string(),
            Ty::U16 => "u16".to_string(),
            Ty::U32 => "u32".to_string(),
            Ty::U64 => "u64".to_string(),
            Ty::U128 => "u128".to_string(),
            Ty::Usize => "usize".to_string(),
            Ty::F32 => "f32".to_string(),
            Ty::F64 => "f64".to_string(),
            Ty::Char => "char".to_string(),
            Ty::Str => "str".to_string(),
            Ty::String => "String".to_string(),
            Ty::Never => "!".to_string(),
            Ty::Error => "<error>".to_string(),
            Ty::Any => "any".to_string(),
            Ty::Tuple(elems) => {
                let inner: Vec<_> = elems.iter().map(|&e| self.display(e)).collect();
                format!("({})", inner.join(", "))
            }
            Ty::Array { element, size } => {
                format!("[{}; {}]", self.display(*element), size)
            }
            Ty::Slice(elem) => format!("[{}]", self.display(*elem)),
            Ty::Reference { mutable, inner } => {
                if *mutable {
                    format!("&mut {}", self.display(*inner))
                } else {
                    format!("&{}", self.display(*inner))
                }
            }
            Ty::Struct(s) => s.name.to_string(),
            Ty::Enum(e) => e.name.to_string(),
            Ty::Function { params, return_type } => {
                let params_str: Vec<_> = params.iter().map(|&p| self.display(p)).collect();
                format!("fn({}) -> {}", params_str.join(", "), self.display(*return_type))
            }
            Ty::Closure { params, return_type, .. } => {
                let params_str: Vec<_> = params.iter().map(|&p| self.display(p)).collect();
                format!("|{}| -> {}", params_str.join(", "), self.display(*return_type))
            }
            Ty::Generic(var) => format!("T{}", var.0),
            Ty::Applied { base, args } => {
                let args_str: Vec<_> = args.iter().map(|&a| self.display(a)).collect();
                format!("{}<{}>", self.display(*base), args_str.join(", "))
            }
            Ty::Infer(var) => format!("?{}", var.0),
        }
    }
}

impl Default for TypeContext {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_primitives() {
        let ctx = TypeContext::new();
        assert_eq!(ctx.display(ctx.unit()), "()");
        assert_eq!(ctx.display(ctx.bool()), "bool");
        assert_eq!(ctx.display(ctx.i64()), "i64");
        assert_eq!(ctx.display(ctx.string()), "String");
    }

    #[test]
    fn test_intern_dedup() {
        let mut ctx = TypeContext::new();
        let t1 = ctx.tuple(vec![ctx.i32(), ctx.i32()]);
        let t2 = ctx.tuple(vec![ctx.i32(), ctx.i32()]);
        assert_eq!(t1, t2);
    }

    #[test]
    fn test_compound_types() {
        let mut ctx = TypeContext::new();
        let arr = ctx.array(ctx.i32(), 10);
        assert_eq!(ctx.display(arr), "[i32; 10]");

        let slice = ctx.slice(ctx.u8());
        assert_eq!(ctx.display(slice), "[u8]");

        let ref_ty = ctx.reference(ctx.string(), false);
        assert_eq!(ctx.display(ref_ty), "&String");

        let mut_ref = ctx.reference(ctx.i64(), true);
        assert_eq!(ctx.display(mut_ref), "&mut i64");
    }

    #[test]
    fn test_function_type() {
        let mut ctx = TypeContext::new();
        let fn_ty = ctx.function(vec![ctx.i32(), ctx.i32()], ctx.i64());
        assert_eq!(ctx.display(fn_ty), "fn(i32, i32) -> i64");
    }

    #[test]
    fn test_inference_var() {
        let mut ctx = TypeContext::new();
        let v1 = ctx.fresh_infer();
        let v2 = ctx.fresh_infer();
        assert_ne!(v1, v2);
        assert!(ctx.is_infer(v1));
        assert!(ctx.contains_infer(v1));
    }
}