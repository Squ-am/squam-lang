use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use squam_compiler::FunctionProto;

use crate::gc::{Trace, Tracer};

/// A runtime value in the Squam VM.
#[derive(Clone)]
pub enum Value {
    /// Unit type ()
    Unit,
    /// Boolean
    Bool(bool),
    /// 64-bit signed integer
    Int(i64),
    /// 64-bit floating point
    Float(f64),
    /// String (reference counted)
    String(Rc<String>),
    /// Tuple
    Tuple(Rc<Vec<Value>>),
    /// Array (mutable)
    Array(Rc<RefCell<Vec<Value>>>),
    /// Range (start, end, inclusive)
    Range(i64, i64, bool),
    /// Closure
    Closure(Rc<Closure>),
    /// Native function
    Native(NativeFunction),
    /// Struct instance
    Struct(Rc<StructInstance>),
    /// Enum variant
    Enum(Rc<EnumInstance>),
    /// Iterator
    Iterator(Rc<RefCell<Box<dyn SquamIterator>>>),
    /// Reference to a local variable (stack slot relative to frame base, mutable flag)
    /// The usize is the absolute stack index where the value lives
    LocalRef(usize, bool),
    /// Box - single-owner heap allocation (mutable)
    Box(Rc<RefCell<Value>>),
    /// Native function requiring VM access (for calling closures)
    VMNative(VMNativeFnId),
}

/// A closure captures its environment.
#[derive(Clone)]
pub struct Closure {
    pub proto: Rc<FunctionProto>,
    pub upvalues: Vec<Rc<RefCell<Upvalue>>>,
}

/// An upvalue captures a local variable from an enclosing scope.
#[derive(Clone)]
pub enum Upvalue {
    /// Open upvalue - points to stack slot
    Open(usize),
    /// Closed upvalue - value has been moved here
    Closed(Value),
}

/// A native (Rust) function callable from Squam.
#[derive(Clone)]
pub struct NativeFunction {
    pub name: String,
    pub arity: u8,
    pub func: Rc<dyn Fn(&mut [Value]) -> Result<Value, String>>,
}

/// ID for a native function that requires VM access.
/// Used for registering callbacks that need to call Squam closures.
pub type VMNativeFnId = u16;

/// Registry entry for VM-aware native functions.
pub struct VMNativeFnEntry {
    pub name: String,
    pub arity: u8,
}

/// A struct instance.
#[derive(Clone)]
pub struct StructInstance {
    pub name: String,
    pub fields: RefCell<HashMap<String, Value>>,
}

/// An enum variant instance.
#[derive(Clone)]
pub struct EnumInstance {
    pub enum_name: String,
    pub variant: String,
    pub fields: Vec<Value>,
}

/// Iterator trait for Squam iterators.
pub trait SquamIterator {
    fn next(&mut self) -> Option<Value>;
}

/// Range iterator.
pub struct RangeIterator {
    current: i64,
    end: i64,
    inclusive: bool,
}

impl RangeIterator {
    pub fn new(start: i64, end: i64, inclusive: bool) -> Self {
        Self {
            current: start,
            end,
            inclusive,
        }
    }
}

impl SquamIterator for RangeIterator {
    fn next(&mut self) -> Option<Value> {
        let in_range = if self.inclusive {
            self.current <= self.end
        } else {
            self.current < self.end
        };
        if in_range {
            let val = self.current;
            self.current += 1;
            Some(Value::Int(val))
        } else {
            None
        }
    }
}

/// Array iterator.
pub struct ArrayIterator {
    array: Rc<RefCell<Vec<Value>>>,
    index: usize,
}

impl ArrayIterator {
    pub fn new(array: Rc<RefCell<Vec<Value>>>) -> Self {
        Self { array, index: 0 }
    }
}

impl SquamIterator for ArrayIterator {
    fn next(&mut self) -> Option<Value> {
        let arr = self.array.borrow();
        if self.index < arr.len() {
            let val = arr[self.index].clone();
            self.index += 1;
            Some(val)
        } else {
            None
        }
    }
}

impl Value {
    /// Check if value is truthy.
    pub fn is_truthy(&self) -> bool {
        match self {
            Value::Unit => false,
            Value::Bool(b) => *b,
            Value::Int(n) => *n != 0,
            Value::Float(n) => *n != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::Array(arr) => !arr.borrow().is_empty(),
            Value::Tuple(t) => !t.is_empty(),
            Value::LocalRef(_, _) => true, // References are always truthy
            _ => true,
        }
    }

    /// Get the type name of this value.
    pub fn type_name(&self) -> &'static str {
        match self {
            Value::Unit => "()",
            Value::Bool(_) => "bool",
            Value::Int(_) => "int",
            Value::Float(_) => "float",
            Value::String(_) => "string",
            Value::Tuple(_) => "tuple",
            Value::Array(_) => "array",
            Value::Range(_, _, _) => "range",
            Value::Closure(_) => "function",
            Value::Native(_) => "native function",
            Value::Struct(_) => "struct",
            Value::Enum(_) => "enum",
            Value::Iterator(_) => "iterator",
            Value::LocalRef(_, mutable) => if *mutable { "&mut" } else { "&" },
            Value::Box(_) => "Box",
            Value::VMNative(_) => "native function",
        }
    }
}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{:?}", s),
            Value::Tuple(t) => {
                write!(f, "(")?;
                for (i, v) in t.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", v)?;
                }
                write!(f, ")")
            }
            Value::Array(arr) => {
                let arr = arr.borrow();
                write!(f, "[")?;
                for (i, v) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", v)?;
                }
                write!(f, "]")
            }
            Value::Closure(c) => write!(
                f,
                "<fn {}>",
                c.proto.name.as_deref().unwrap_or("anonymous")
            ),
            Value::Native(n) => write!(f, "<native {}>", n.name),
            Value::Struct(s) => write!(f, "{} {{ ... }}", s.name),
            Value::Range(start, end, inclusive) => {
                if *inclusive {
                    write!(f, "{}..={}", start, end)
                } else {
                    write!(f, "{}..{}", start, end)
                }
            }
            Value::Enum(e) => write!(f, "{}::{}", e.enum_name, e.variant),
            Value::Iterator(_) => write!(f, "<iterator>"),
            Value::LocalRef(slot, mutable) => {
                if *mutable {
                    write!(f, "&mut <slot {}>", slot)
                } else {
                    write!(f, "&<slot {}>", slot)
                }
            }
            Value::Box(inner) => {
                write!(f, "Box({:?})", inner.borrow())
            }
            Value::VMNative(id) => write!(f, "<vm-native {}>", id),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Unit => write!(f, "()"),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Tuple(t) => {
                write!(f, "(")?;
                for (i, v) in t.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, ")")
            }
            Value::Array(arr) => {
                let arr = arr.borrow();
                write!(f, "[")?;
                for (i, v) in arr.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "]")
            }
            Value::Closure(c) => write!(
                f,
                "<fn {}>",
                c.proto.name.as_deref().unwrap_or("anonymous")
            ),
            Value::Native(n) => write!(f, "<native {}>", n.name),
            Value::Struct(s) => write!(f, "{} {{ ... }}", s.name),
            Value::Range(start, end, inclusive) => {
                if *inclusive {
                    write!(f, "{}..={}", start, end)
                } else {
                    write!(f, "{}..{}", start, end)
                }
            }
            Value::Enum(e) => write!(f, "{}::{}", e.enum_name, e.variant),
            Value::Iterator(_) => write!(f, "<iterator>"),
            Value::LocalRef(slot, mutable) => {
                if *mutable {
                    write!(f, "&mut <slot {}>", slot)
                } else {
                    write!(f, "&<slot {}>", slot)
                }
            }
            Value::Box(inner) => {
                write!(f, "Box({})", inner.borrow())
            }
            Value::VMNative(id) => write!(f, "<vm-native {}>", id),
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Unit, Value::Unit) => true,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::String(a), Value::String(b)) => a == b,
            (Value::Tuple(a), Value::Tuple(b)) => a == b,
            (Value::Range(s1, e1, i1), Value::Range(s2, e2, i2)) => s1 == s2 && e1 == e2 && i1 == i2,
            _ => false,
        }
    }
}

// ---
// Trace implementations for GC
// ---

impl Trace for Value {
    fn trace(&self, tracer: &mut Tracer) {
        match self {
            Value::Unit | Value::Bool(_) | Value::Int(_) | Value::Float(_) | Value::Range(_, _, _) => {
                // Primitives don't need tracing
            }
            Value::String(_) => {
                // Strings are Rc, will be handled when we switch to Gc
            }
            Value::Tuple(t) => {
                for v in t.iter() {
                    v.trace(tracer);
                }
            }
            Value::Array(arr) => {
                for v in arr.borrow().iter() {
                    v.trace(tracer);
                }
            }
            Value::Closure(c) => {
                c.trace(tracer);
            }
            Value::Native(_) => {
                // Native functions don't contain GC pointers
            }
            Value::Struct(s) => {
                for v in s.fields.borrow().values() {
                    v.trace(tracer);
                }
            }
            Value::Enum(e) => {
                for v in &e.fields {
                    v.trace(tracer);
                }
            }
            Value::Iterator(_) => {
                // Iterators may hold references but we'll handle that specially
            }
            Value::LocalRef(_, _) => {
                // LocalRef points to a stack slot, not a heap value
            }
            Value::Box(inner) => {
                inner.borrow().trace(tracer);
            }
            Value::VMNative(_) => {
                // VM natives are just IDs, no GC pointers
            }
        }
    }
}

impl Trace for Closure {
    fn trace(&self, tracer: &mut Tracer) {
        // Trace all upvalues
        for upvalue in &self.upvalues {
            upvalue.borrow().trace(tracer);
        }
    }
}

impl Trace for Upvalue {
    fn trace(&self, tracer: &mut Tracer) {
        match self {
            Upvalue::Open(_) => {
                // Open upvalues point to stack slots, not heap
            }
            Upvalue::Closed(v) => {
                v.trace(tracer);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_value_display() {
        assert_eq!(format!("{}", Value::Unit), "()");
        assert_eq!(format!("{}", Value::Int(42)), "42");
        assert_eq!(format!("{}", Value::Bool(true)), "true");
        assert_eq!(
            format!("{}", Value::String(Rc::new("hello".to_string()))),
            "hello"
        );
    }

    #[test]
    fn test_value_truthy() {
        assert!(!Value::Unit.is_truthy());
        assert!(!Value::Bool(false).is_truthy());
        assert!(Value::Bool(true).is_truthy());
        assert!(!Value::Int(0).is_truthy());
        assert!(Value::Int(1).is_truthy());
    }

    #[test]
    fn test_range_iterator() {
        let mut iter = RangeIterator::new(0, 3, false);
        assert_eq!(iter.next(), Some(Value::Int(0)));
        assert_eq!(iter.next(), Some(Value::Int(1)));
        assert_eq!(iter.next(), Some(Value::Int(2)));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_range_iterator_inclusive() {
        let mut iter = RangeIterator::new(0, 3, true);
        assert_eq!(iter.next(), Some(Value::Int(0)));
        assert_eq!(iter.next(), Some(Value::Int(1)));
        assert_eq!(iter.next(), Some(Value::Int(2)));
        assert_eq!(iter.next(), Some(Value::Int(3)));
        assert_eq!(iter.next(), None);
    }
}