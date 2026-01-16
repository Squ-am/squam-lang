use squam_vm::{Value, VM};
use squam_vm::value::StructInstance;
use serde_json::{self, Value as JsonValue};
use std::cell::RefCell;
use std::rc::Rc;

fn json_to_squam(json: JsonValue) -> Value {
    match json {
        JsonValue::Null => Value::Unit,
        JsonValue::Bool(b) => Value::Bool(b),
        JsonValue::Number(n) => {
            if let Some(i) = n.as_i64() {
                Value::Int(i)
            } else if let Some(f) = n.as_f64() {
                Value::Float(f)
            } else {
                Value::Unit
            }
        }
        JsonValue::String(s) => Value::String(Rc::new(s)),
        JsonValue::Array(arr) => {
            let values: Vec<Value> = arr.into_iter().map(json_to_squam).collect();
            Value::Array(Rc::new(RefCell::new(values)))
        }
        JsonValue::Object(obj) => {
            let instance = StructInstance::new_dynamic("Object".to_string());
            for (k, v) in obj {
                instance.fields().borrow_mut().insert(k, json_to_squam(v));
            }
            Value::Struct(Rc::new(instance))
        }
    }
}

fn squam_to_json(value: &Value) -> JsonValue {
    match value {
        Value::Unit => JsonValue::Null,
        Value::Bool(b) => JsonValue::Bool(*b),
        Value::Int(i) => JsonValue::Number((*i).into()),
        Value::Float(f) => {
            if let Some(n) = serde_json::Number::from_f64(*f) {
                JsonValue::Number(n)
            } else {
                JsonValue::Null
            }
        }
        Value::String(s) => JsonValue::String(s.to_string()),
        Value::Array(arr) => {
            let values: Vec<JsonValue> = arr.borrow().iter().map(squam_to_json).collect();
            JsonValue::Array(values)
        }
        Value::Tuple(t) => {
            let values: Vec<JsonValue> = t.iter().map(squam_to_json).collect();
            JsonValue::Array(values)
        }
        Value::Struct(s) => {
            let mut map = serde_json::Map::new();
            // Include both ordered fields and dynamic fields
            for (name, &idx) in s.field_indices.iter() {
                if !name.starts_with("__") {
                    if let Some(v) = s.field_values.borrow().get(idx) {
                        map.insert(name.clone(), squam_to_json(v));
                    }
                }
            }
            for (k, v) in s.fields().borrow().iter() {
                if !k.starts_with("__") {
                    map.insert(k.clone(), squam_to_json(v));
                }
            }
            JsonValue::Object(map)
        }
        Value::Enum(e) => {
            // Serialize enum as { "variant": "Name", "data": [...] }
            let mut map = serde_json::Map::new();
            map.insert("variant".to_string(), JsonValue::String(e.variant.clone()));
            let data: Vec<JsonValue> = e.fields.iter().map(squam_to_json).collect();
            if !data.is_empty() {
                map.insert("data".to_string(), JsonValue::Array(data));
            }
            JsonValue::Object(map)
        }
        _ => JsonValue::Null,
    }
}

pub fn register(vm: &mut VM) {
    // json_parse(s: string) -> value
    vm.define_native("json_parse", 1, |args| {
        match &args[0] {
            Value::String(s) => {
                match serde_json::from_str::<JsonValue>(s.as_str()) {
                    Ok(json) => Ok(json_to_squam(json)),
                    Err(e) => Err(format!("json_parse failed: {}", e)),
                }
            }
            _ => Err("json_parse: expected string".to_string()),
        }
    });

    // json_stringify(value) -> string
    vm.define_native("json_stringify", 1, |args| {
        let json = squam_to_json(&args[0]);
        Ok(Value::String(Rc::new(json.to_string())))
    });

    // json_stringify_pretty(value) -> string (formatted with indentation)
    vm.define_native("json_stringify_pretty", 1, |args| {
        let json = squam_to_json(&args[0]);
        match serde_json::to_string_pretty(&json) {
            Ok(s) => Ok(Value::String(Rc::new(s))),
            Err(e) => Err(format!("json_stringify_pretty failed: {}", e)),
        }
    });

    // json_get(obj: struct, key: string) -> value (safe access with null for missing)
    vm.define_native("json_get", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::Struct(s), Value::String(key)) => {
                // First check ordered fields, then dynamic
                if let Some(v) = s.get_field(key.as_str()) {
                    Ok(v)
                } else {
                    Ok(Value::Unit)
                }
            }
            _ => Err("json_get: expected (struct, string)".to_string()),
        }
    });

    // json_has(obj: struct, key: string) -> bool
    vm.define_native("json_has", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::Struct(s), Value::String(key)) => {
                let has = s.field_indices.contains_key(key.as_str())
                    || s.fields().borrow().contains_key(key.as_str());
                Ok(Value::Bool(has))
            }
            _ => Err("json_has: expected (struct, string)".to_string()),
        }
    });

    // json_keys(obj: struct) -> [string]
    vm.define_native("json_keys", 1, |args| {
        match &args[0] {
            Value::Struct(s) => {
                let mut keys: Vec<Value> = s.field_indices
                    .keys()
                    .filter(|k| !k.starts_with("__"))
                    .map(|k| Value::String(Rc::new(k.clone())))
                    .collect();
                for k in s.fields().borrow().keys() {
                    if !k.starts_with("__") {
                        keys.push(Value::String(Rc::new(k.clone())));
                    }
                }
                Ok(Value::Array(Rc::new(RefCell::new(keys))))
            }
            _ => Err("json_keys: expected struct".to_string()),
        }
    });

    // json_values(obj: struct) -> [value]
    vm.define_native("json_values", 1, |args| {
        match &args[0] {
            Value::Struct(s) => {
                let mut values: Vec<Value> = Vec::new();
                // Add ordered field values
                for (name, &idx) in &s.field_indices {
                    if !name.starts_with("__") {
                        if let Some(v) = s.field_values.borrow().get(idx) {
                            values.push(v.clone());
                        }
                    }
                }
                // Add dynamic field values
                for (k, v) in s.fields().borrow().iter() {
                    if !k.starts_with("__") {
                        values.push(v.clone());
                    }
                }
                Ok(Value::Array(Rc::new(RefCell::new(values))))
            }
            _ => Err("json_values: expected struct".to_string()),
        }
    });
}
