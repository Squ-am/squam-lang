use squam_vm::value::{StructInstance, Value};
use squam_vm::VM;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

/// Create a new empty HashMap wrapped as a Value.
pub fn new_hashmap() -> Value {
    Value::Struct(Rc::new(StructInstance {
        name: "HashMap".to_string(),
        fields: RefCell::new(HashMap::new()),
    }))
}

/// Helper to get a string key from a Value.
fn to_key(value: &Value) -> String {
    match value {
        Value::Int(n) => format!("i:{}", n),
        Value::Float(f) => format!("f:{}", f),
        Value::Bool(b) => format!("b:{}", b),
        Value::String(s) => format!("s:{}", s),
        _ => format!("v:{:?}", value),
    }
}

/// Register HashMap functions with the VM.
pub fn register(vm: &mut VM) {
    // Create a new empty HashMap
    vm.define_native("hashmap_new", 0, |_args| {
        Ok(new_hashmap())
    });

    // Insert a key-value pair
    vm.define_native("hashmap_insert", 3, |args| {
        if let Value::Struct(s) = &args[0] {
            if s.name == "HashMap" {
                let key = to_key(&args[1]);
                s.fields.borrow_mut().insert(key, args[2].clone());
                return Ok(Value::Unit);
            }
        }
        Err("hashmap_insert expects a HashMap".to_string())
    });

    // Get a value by key (returns Option)
    vm.define_native("hashmap_get", 2, |args| {
        if let Value::Struct(s) = &args[0] {
            if s.name == "HashMap" {
                let key = to_key(&args[1]);
                let fields = s.fields.borrow();
                if let Some(value) = fields.get(&key) {
                    return Ok(super::option::some(value.clone()));
                } else {
                    return Ok(super::option::none());
                }
            }
        }
        Err("hashmap_get expects a HashMap".to_string())
    });

    // Check if map contains a key
    vm.define_native("hashmap_contains", 2, |args| {
        if let Value::Struct(s) = &args[0] {
            if s.name == "HashMap" {
                let key = to_key(&args[1]);
                let contains = s.fields.borrow().contains_key(&key);
                return Ok(Value::Bool(contains));
            }
        }
        Err("hashmap_contains expects a HashMap".to_string())
    });

    // Remove a key-value pair
    vm.define_native("hashmap_remove", 2, |args| {
        if let Value::Struct(s) = &args[0] {
            if s.name == "HashMap" {
                let key = to_key(&args[1]);
                let removed = s.fields.borrow_mut().remove(&key);
                if let Some(value) = removed {
                    return Ok(super::option::some(value));
                } else {
                    return Ok(super::option::none());
                }
            }
        }
        Err("hashmap_remove expects a HashMap".to_string())
    });

    // Get the number of entries
    vm.define_native("hashmap_len", 1, |args| {
        if let Value::Struct(s) = &args[0] {
            if s.name == "HashMap" {
                let len = s.fields.borrow().len();
                return Ok(Value::Int(len as i64));
            }
        }
        Err("hashmap_len expects a HashMap".to_string())
    });

    // Check if the map is empty
    vm.define_native("hashmap_is_empty", 1, |args| {
        if let Value::Struct(s) = &args[0] {
            if s.name == "HashMap" {
                let is_empty = s.fields.borrow().is_empty();
                return Ok(Value::Bool(is_empty));
            }
        }
        Err("hashmap_is_empty expects a HashMap".to_string())
    });

    // Clear all entries
    vm.define_native("hashmap_clear", 1, |args| {
        if let Value::Struct(s) = &args[0] {
            if s.name == "HashMap" {
                s.fields.borrow_mut().clear();
                return Ok(Value::Unit);
            }
        }
        Err("hashmap_clear expects a HashMap".to_string())
    });

    // Get all keys as an array
    vm.define_native("hashmap_keys", 1, |args| {
        if let Value::Struct(s) = &args[0] {
            if s.name == "HashMap" {
                let fields = s.fields.borrow();
                let keys: Vec<Value> = fields.keys()
                    .map(|k| {
                        // Parse the key back to a Value
                        if let Some(rest) = k.strip_prefix("i:") {
                            Value::Int(rest.parse().unwrap_or(0))
                        } else if let Some(rest) = k.strip_prefix("s:") {
                            Value::String(Rc::new(rest.to_string()))
                        } else if let Some(rest) = k.strip_prefix("b:") {
                            Value::Bool(rest == "true")
                        } else {
                            Value::String(Rc::new(k.clone()))
                        }
                    })
                    .collect();
                return Ok(Value::Array(Rc::new(RefCell::new(keys))));
            }
        }
        Err("hashmap_keys expects a HashMap".to_string())
    });

    // Get all values as an array
    vm.define_native("hashmap_values", 1, |args| {
        if let Value::Struct(s) = &args[0] {
            if s.name == "HashMap" {
                let fields = s.fields.borrow();
                let values: Vec<Value> = fields.values().cloned().collect();
                return Ok(Value::Array(Rc::new(RefCell::new(values))));
            }
        }
        Err("hashmap_values expects a HashMap".to_string())
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_key() {
        assert_eq!(to_key(&Value::Int(42)), "i:42");
        assert_eq!(to_key(&Value::Bool(true)), "b:true");
        assert_eq!(to_key(&Value::String(Rc::new("hello".to_string()))), "s:hello");
    }
}