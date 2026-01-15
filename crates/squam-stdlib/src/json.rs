use squam_vm::{Value, VM};
use squam_vm::value::StructInstance;
use serde_json::{self, Value as JsonValue};
use std::cell::RefCell;
use std::collections::HashMap;
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
            let mut fields = HashMap::new();
            for (k, v) in obj {
                fields.insert(k, json_to_squam(v));
            }
            Value::Struct(Rc::new(StructInstance {
                name: "Object".to_string(),
                fields: RefCell::new(fields),
            }))
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
            for (k, v) in s.fields.borrow().iter() {
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
                let fields = s.fields.borrow();
                match fields.get(key.as_str()) {
                    Some(v) => Ok(v.clone()),
                    None => Ok(Value::Unit),
                }
            }
            _ => Err("json_get: expected (struct, string)".to_string()),
        }
    });

    // json_has(obj: struct, key: string) -> bool
    vm.define_native("json_has", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::Struct(s), Value::String(key)) => {
                let fields = s.fields.borrow();
                Ok(Value::Bool(fields.contains_key(key.as_str())))
            }
            _ => Err("json_has: expected (struct, string)".to_string()),
        }
    });

    // json_keys(obj: struct) -> [string]
    vm.define_native("json_keys", 1, |args| {
        match &args[0] {
            Value::Struct(s) => {
                let fields = s.fields.borrow();
                let keys: Vec<Value> = fields
                    .keys()
                    .filter(|k| !k.starts_with("__"))
                    .map(|k| Value::String(Rc::new(k.clone())))
                    .collect();
                Ok(Value::Array(Rc::new(RefCell::new(keys))))
            }
            _ => Err("json_keys: expected struct".to_string()),
        }
    });

    // json_values(obj: struct) -> [value]
    vm.define_native("json_values", 1, |args| {
        match &args[0] {
            Value::Struct(s) => {
                let fields = s.fields.borrow();
                let values: Vec<Value> = fields
                    .iter()
                    .filter(|(k, _)| !k.starts_with("__"))
                    .map(|(_, v)| v.clone())
                    .collect();
                Ok(Value::Array(Rc::new(RefCell::new(values))))
            }
            _ => Err("json_values: expected struct".to_string()),
        }
    });
}