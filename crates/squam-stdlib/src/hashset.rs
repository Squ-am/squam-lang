use squam_vm::value::{StructInstance, Value};
use squam_vm::VM;
use std::cell::RefCell;
use std::rc::Rc;

/// Create a new empty HashSet wrapped as a Value.
pub fn new_hashset() -> Value {
    Value::Struct(Rc::new(StructInstance::new_dynamic("HashSet".to_string())))
}

/// Helper to get a string key from a Value (same as HashMap).
fn to_key(value: &Value) -> String {
    match value {
        Value::Int(n) => format!("i:{}", n),
        Value::Float(f) => format!("f:{}", f),
        Value::Bool(b) => format!("b:{}", b),
        Value::String(s) => format!("s:{}", s),
        _ => format!("v:{:?}", value),
    }
}

/// Register HashSet functions with the VM.
pub fn register(vm: &mut VM) {
    // Create a new empty HashSet
    vm.define_native("hashset_new", 0, |_args| {
        Ok(new_hashset())
    });

    // Insert a value (returns true if newly inserted)
    vm.define_native("hashset_insert", 2, |args| {
        if let Value::Struct(s) = &args[0] {
            if s.name == "HashSet" {
                let key = to_key(&args[1]);
                let was_new = !s.fields().borrow().contains_key(&key);
                s.fields().borrow_mut().insert(key, Value::Unit);
                return Ok(Value::Bool(was_new));
            }
        }
        Err("hashset_insert expects a HashSet".to_string())
    });

    // Check if set contains a value
    vm.define_native("hashset_contains", 2, |args| {
        if let Value::Struct(s) = &args[0] {
            if s.name == "HashSet" {
                let key = to_key(&args[1]);
                let contains = s.fields().borrow().contains_key(&key);
                return Ok(Value::Bool(contains));
            }
        }
        Err("hashset_contains expects a HashSet".to_string())
    });

    // Remove a value (returns true if was present)
    vm.define_native("hashset_remove", 2, |args| {
        if let Value::Struct(s) = &args[0] {
            if s.name == "HashSet" {
                let key = to_key(&args[1]);
                let was_present = s.fields().borrow_mut().remove(&key).is_some();
                return Ok(Value::Bool(was_present));
            }
        }
        Err("hashset_remove expects a HashSet".to_string())
    });

    // Get the number of elements
    vm.define_native("hashset_len", 1, |args| {
        if let Value::Struct(s) = &args[0] {
            if s.name == "HashSet" {
                let len = s.fields().borrow().len();
                return Ok(Value::Int(len as i64));
            }
        }
        Err("hashset_len expects a HashSet".to_string())
    });

    // Check if the set is empty
    vm.define_native("hashset_is_empty", 1, |args| {
        if let Value::Struct(s) = &args[0] {
            if s.name == "HashSet" {
                let is_empty = s.fields().borrow().is_empty();
                return Ok(Value::Bool(is_empty));
            }
        }
        Err("hashset_is_empty expects a HashSet".to_string())
    });

    // Clear all elements
    vm.define_native("hashset_clear", 1, |args| {
        if let Value::Struct(s) = &args[0] {
            if s.name == "HashSet" {
                s.fields().borrow_mut().clear();
                return Ok(Value::Unit);
            }
        }
        Err("hashset_clear expects a HashSet".to_string())
    });

    // Convert to array
    vm.define_native("hashset_to_array", 1, |args| {
        if let Value::Struct(s) = &args[0] {
            if s.name == "HashSet" {
                let fields = s.fields().borrow();
                let values: Vec<Value> = fields.keys()
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
                return Ok(Value::Array(Rc::new(RefCell::new(values))));
            }
        }
        Err("hashset_to_array expects a HashSet".to_string())
    });

    // Union of two sets (creates new set)
    vm.define_native("hashset_union", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::Struct(s1), Value::Struct(s2))
                if s1.name == "HashSet" && s2.name == "HashSet" =>
            {
                let result = StructInstance::new_dynamic("HashSet".to_string());

                for key in s1.fields().borrow().keys() {
                    result.fields().borrow_mut().insert(key.clone(), Value::Unit);
                }
                for key in s2.fields().borrow().keys() {
                    result.fields().borrow_mut().insert(key.clone(), Value::Unit);
                }

                Ok(Value::Struct(Rc::new(result)))
            }
            _ => Err("hashset_union expects two HashSets".to_string())
        }
    });

    // Intersection of two sets (creates new set)
    vm.define_native("hashset_intersection", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::Struct(s1), Value::Struct(s2))
                if s1.name == "HashSet" && s2.name == "HashSet" =>
            {
                let result = StructInstance::new_dynamic("HashSet".to_string());

                let fields1 = s1.fields().borrow();
                let fields2 = s2.fields().borrow();

                for key in fields1.keys() {
                    if fields2.contains_key(key) {
                        result.fields().borrow_mut().insert(key.clone(), Value::Unit);
                    }
                }

                Ok(Value::Struct(Rc::new(result)))
            }
            _ => Err("hashset_intersection expects two HashSets".to_string())
        }
    });

    // Difference of two sets (creates new set: elements in first but not second)
    vm.define_native("hashset_difference", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::Struct(s1), Value::Struct(s2))
                if s1.name == "HashSet" && s2.name == "HashSet" =>
            {
                let result = StructInstance::new_dynamic("HashSet".to_string());

                let fields1 = s1.fields().borrow();
                let fields2 = s2.fields().borrow();

                for key in fields1.keys() {
                    if !fields2.contains_key(key) {
                        result.fields().borrow_mut().insert(key.clone(), Value::Unit);
                    }
                }

                Ok(Value::Struct(Rc::new(result)))
            }
            _ => Err("hashset_difference expects two HashSets".to_string())
        }
    });
}
