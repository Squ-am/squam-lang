use std::cell::RefCell;
use std::rc::Rc;

use squam_vm::Value;

use crate::error::{EmbedError, EmbedResult};

/// Convert a Rust value into a Squam value.
pub trait IntoSquam {
    /// Convert this value into a Squam `Value`.
    fn into_squam(self) -> Value;
}

/// Convert a Squam value into a Rust value.
pub trait FromSquam: Sized {
    /// Try to convert a Squam `Value` into this type.
    fn from_squam(value: Value) -> EmbedResult<Self>;

    /// The expected type name for error messages.
    fn type_name() -> &'static str;
}

// ---
// IntoSquam implementations
// ---

impl IntoSquam for () {
    fn into_squam(self) -> Value {
        Value::Unit
    }
}

impl IntoSquam for bool {
    fn into_squam(self) -> Value {
        Value::Bool(self)
    }
}

impl IntoSquam for i64 {
    fn into_squam(self) -> Value {
        Value::Int(self)
    }
}

impl IntoSquam for i32 {
    fn into_squam(self) -> Value {
        Value::Int(self as i64)
    }
}

impl IntoSquam for i16 {
    fn into_squam(self) -> Value {
        Value::Int(self as i64)
    }
}

impl IntoSquam for i8 {
    fn into_squam(self) -> Value {
        Value::Int(self as i64)
    }
}

impl IntoSquam for u64 {
    fn into_squam(self) -> Value {
        Value::Int(self as i64)
    }
}

impl IntoSquam for u32 {
    fn into_squam(self) -> Value {
        Value::Int(self as i64)
    }
}

impl IntoSquam for u16 {
    fn into_squam(self) -> Value {
        Value::Int(self as i64)
    }
}

impl IntoSquam for u8 {
    fn into_squam(self) -> Value {
        Value::Int(self as i64)
    }
}

impl IntoSquam for usize {
    fn into_squam(self) -> Value {
        Value::Int(self as i64)
    }
}

impl IntoSquam for f64 {
    fn into_squam(self) -> Value {
        Value::Float(self)
    }
}

impl IntoSquam for f32 {
    fn into_squam(self) -> Value {
        Value::Float(self as f64)
    }
}

impl IntoSquam for String {
    fn into_squam(self) -> Value {
        Value::String(Rc::new(self))
    }
}

impl IntoSquam for &str {
    fn into_squam(self) -> Value {
        Value::String(Rc::new(self.to_string()))
    }
}

impl<T: IntoSquam> IntoSquam for Vec<T> {
    fn into_squam(self) -> Value {
        let values: Vec<Value> = self.into_iter().map(|v| v.into_squam()).collect();
        Value::Array(Rc::new(RefCell::new(values)))
    }
}

impl<T: IntoSquam> IntoSquam for Option<T> {
    fn into_squam(self) -> Value {
        match self {
            Some(v) => v.into_squam(),
            None => Value::Unit,
        }
    }
}

impl IntoSquam for Value {
    fn into_squam(self) -> Value {
        self
    }
}

// ---
// FromSquam implementations
// ---

impl FromSquam for () {
    fn from_squam(value: Value) -> EmbedResult<Self> {
        match value {
            Value::Unit => Ok(()),
            other => Err(EmbedError::Type {
                expected: Self::type_name().to_string(),
                got: other.type_name().to_string(),
            }),
        }
    }

    fn type_name() -> &'static str {
        "()"
    }
}

impl FromSquam for bool {
    fn from_squam(value: Value) -> EmbedResult<Self> {
        match value {
            Value::Bool(b) => Ok(b),
            other => Err(EmbedError::Type {
                expected: Self::type_name().to_string(),
                got: other.type_name().to_string(),
            }),
        }
    }

    fn type_name() -> &'static str {
        "bool"
    }
}

impl FromSquam for i64 {
    fn from_squam(value: Value) -> EmbedResult<Self> {
        match value {
            Value::Int(n) => Ok(n),
            other => Err(EmbedError::Type {
                expected: Self::type_name().to_string(),
                got: other.type_name().to_string(),
            }),
        }
    }

    fn type_name() -> &'static str {
        "int"
    }
}

impl FromSquam for i32 {
    fn from_squam(value: Value) -> EmbedResult<Self> {
        match value {
            Value::Int(n) => Ok(n as i32),
            other => Err(EmbedError::Type {
                expected: Self::type_name().to_string(),
                got: other.type_name().to_string(),
            }),
        }
    }

    fn type_name() -> &'static str {
        "int"
    }
}

impl FromSquam for usize {
    fn from_squam(value: Value) -> EmbedResult<Self> {
        match value {
            Value::Int(n) if n >= 0 => Ok(n as usize),
            Value::Int(_) => Err(EmbedError::Type {
                expected: "non-negative int".to_string(),
                got: "negative int".to_string(),
            }),
            other => Err(EmbedError::Type {
                expected: Self::type_name().to_string(),
                got: other.type_name().to_string(),
            }),
        }
    }

    fn type_name() -> &'static str {
        "int"
    }
}

impl FromSquam for f64 {
    fn from_squam(value: Value) -> EmbedResult<Self> {
        match value {
            Value::Float(n) => Ok(n),
            Value::Int(n) => Ok(n as f64),
            other => Err(EmbedError::Type {
                expected: Self::type_name().to_string(),
                got: other.type_name().to_string(),
            }),
        }
    }

    fn type_name() -> &'static str {
        "float"
    }
}

impl FromSquam for f32 {
    fn from_squam(value: Value) -> EmbedResult<Self> {
        match value {
            Value::Float(n) => Ok(n as f32),
            Value::Int(n) => Ok(n as f32),
            other => Err(EmbedError::Type {
                expected: Self::type_name().to_string(),
                got: other.type_name().to_string(),
            }),
        }
    }

    fn type_name() -> &'static str {
        "float"
    }
}

impl FromSquam for String {
    fn from_squam(value: Value) -> EmbedResult<Self> {
        match value {
            Value::String(s) => Ok((*s).clone()),
            other => Err(EmbedError::Type {
                expected: Self::type_name().to_string(),
                got: other.type_name().to_string(),
            }),
        }
    }

    fn type_name() -> &'static str {
        "string"
    }
}

impl<T: FromSquam> FromSquam for Vec<T> {
    fn from_squam(value: Value) -> EmbedResult<Self> {
        match value {
            Value::Array(arr) => {
                let arr = arr.borrow();
                arr.iter()
                    .cloned()
                    .map(T::from_squam)
                    .collect::<EmbedResult<Vec<T>>>()
            }
            Value::Tuple(t) => t
                .iter()
                .cloned()
                .map(T::from_squam)
                .collect::<EmbedResult<Vec<T>>>(),
            other => Err(EmbedError::Type {
                expected: Self::type_name().to_string(),
                got: other.type_name().to_string(),
            }),
        }
    }

    fn type_name() -> &'static str {
        "array"
    }
}

impl<T: FromSquam> FromSquam for Option<T> {
    fn from_squam(value: Value) -> EmbedResult<Self> {
        match value {
            Value::Unit => Ok(None),
            other => Ok(Some(T::from_squam(other)?)),
        }
    }

    fn type_name() -> &'static str {
        "option"
    }
}

impl FromSquam for Value {
    fn from_squam(value: Value) -> EmbedResult<Self> {
        Ok(value)
    }

    fn type_name() -> &'static str {
        "any"
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_conversion() {
        let v = 42i64.into_squam();
        assert_eq!(v, Value::Int(42));

        let n: i64 = FromSquam::from_squam(v).unwrap();
        assert_eq!(n, 42);
    }

    #[test]
    fn test_string_conversion() {
        let v = "hello".into_squam();
        assert!(matches!(v, Value::String(_)));

        let s: String = FromSquam::from_squam(v).unwrap();
        assert_eq!(s, "hello");
    }

    #[test]
    fn test_vec_conversion() {
        let v = vec![1i64, 2, 3].into_squam();
        assert!(matches!(v, Value::Array(_)));

        let arr: Vec<i64> = FromSquam::from_squam(v).unwrap();
        assert_eq!(arr, vec![1, 2, 3]);
    }

    #[test]
    fn test_type_error() {
        let v = Value::Int(42);
        let result: EmbedResult<String> = FromSquam::from_squam(v);
        assert!(result.is_err());
    }
}