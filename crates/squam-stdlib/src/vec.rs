use squam_vm::{Value, VM};
use std::cell::RefCell;
use std::rc::Rc;

pub fn register(vm: &mut VM) {
    // arr_len(arr: array) -> int
    vm.define_native("arr_len", 1, |args| match &args[0] {
        Value::Array(arr) => Ok(Value::Int(arr.borrow().len() as i64)),
        Value::Tuple(t) => Ok(Value::Int(t.len() as i64)),
        other => Err(format!("arr_len: expected array, got {}", other.type_name())),
    });

    // arr_push(arr: array, value: any) -> ()
    vm.define_native("arr_push", 2, |args| match &args[0] {
        Value::Array(arr) => {
            arr.borrow_mut().push(args[1].clone());
            Ok(Value::Unit)
        }
        other => Err(format!("arr_push: expected array, got {}", other.type_name())),
    });

    // arr_pop(arr: array) -> any
    vm.define_native("arr_pop", 1, |args| match &args[0] {
        Value::Array(arr) => arr
            .borrow_mut()
            .pop()
            .ok_or_else(|| "arr_pop: array is empty".to_string()),
        other => Err(format!("arr_pop: expected array, got {}", other.type_name())),
    });

    // arr_get(arr: array, index: int) -> any
    vm.define_native("arr_get", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::Array(arr), Value::Int(idx)) => {
                let arr = arr.borrow();
                let idx = if *idx < 0 {
                    (arr.len() as i64 + idx) as usize
                } else {
                    *idx as usize
                };
                arr.get(idx)
                    .cloned()
                    .ok_or_else(|| format!("arr_get: index {} out of bounds", idx))
            }
            (Value::Tuple(t), Value::Int(idx)) => {
                let idx = if *idx < 0 {
                    (t.len() as i64 + idx) as usize
                } else {
                    *idx as usize
                };
                t.get(idx)
                    .cloned()
                    .ok_or_else(|| format!("arr_get: index {} out of bounds", idx))
            }
            _ => Err("arr_get: expected (array, int)".to_string()),
        }
    });

    // arr_set(arr: array, index: int, value: any) -> ()
    vm.define_native("arr_set", 3, |args| {
        match (&args[0], &args[1]) {
            (Value::Array(arr), Value::Int(idx)) => {
                let mut arr = arr.borrow_mut();
                let idx = if *idx < 0 {
                    (arr.len() as i64 + idx) as usize
                } else {
                    *idx as usize
                };
                if idx < arr.len() {
                    arr[idx] = args[2].clone();
                    Ok(Value::Unit)
                } else {
                    Err(format!("arr_set: index {} out of bounds", idx))
                }
            }
            _ => Err("arr_set: expected (array, int, value)".to_string()),
        }
    });

    // arr_first(arr: array) -> any
    vm.define_native("arr_first", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            arr.first().cloned().ok_or_else(|| "arr_first: array is empty".to_string())
        }
        Value::Tuple(t) => t.first().cloned().ok_or_else(|| "arr_first: tuple is empty".to_string()),
        other => Err(format!("arr_first: expected array, got {}", other.type_name())),
    });

    // arr_last(arr: array) -> any
    vm.define_native("arr_last", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            arr.last().cloned().ok_or_else(|| "arr_last: array is empty".to_string())
        }
        Value::Tuple(t) => t.last().cloned().ok_or_else(|| "arr_last: tuple is empty".to_string()),
        other => Err(format!("arr_last: expected array, got {}", other.type_name())),
    });

    // arr_is_empty(arr: array) -> bool
    vm.define_native("arr_is_empty", 1, |args| match &args[0] {
        Value::Array(arr) => Ok(Value::Bool(arr.borrow().is_empty())),
        Value::Tuple(t) => Ok(Value::Bool(t.is_empty())),
        other => Err(format!("arr_is_empty: expected array, got {}", other.type_name())),
    });

    // arr_clear(arr: array) -> ()
    vm.define_native("arr_clear", 1, |args| match &args[0] {
        Value::Array(arr) => {
            arr.borrow_mut().clear();
            Ok(Value::Unit)
        }
        other => Err(format!("arr_clear: expected array, got {}", other.type_name())),
    });

    // arr_reverse(arr: array) -> ()
    vm.define_native("arr_reverse", 1, |args| match &args[0] {
        Value::Array(arr) => {
            arr.borrow_mut().reverse();
            Ok(Value::Unit)
        }
        other => Err(format!("arr_reverse: expected array, got {}", other.type_name())),
    });

    // arr_contains(arr: array, value: any) -> bool
    vm.define_native("arr_contains", 2, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            Ok(Value::Bool(arr.iter().any(|v| v == &args[1])))
        }
        other => Err(format!("arr_contains: expected array, got {}", other.type_name())),
    });

    // arr_index_of(arr: array, value: any) -> int
    vm.define_native("arr_index_of", 2, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            match arr.iter().position(|v| v == &args[1]) {
                Some(idx) => Ok(Value::Int(idx as i64)),
                None => Ok(Value::Int(-1)),
            }
        }
        other => Err(format!("arr_index_of: expected array, got {}", other.type_name())),
    });

    // arr_slice(arr: array, start: int, end: int) -> array
    vm.define_native("arr_slice", 3, |args| {
        match (&args[0], &args[1], &args[2]) {
            (Value::Array(arr), Value::Int(start), Value::Int(end)) => {
                let arr = arr.borrow();
                let start = (*start).max(0) as usize;
                let end = ((*end).max(0) as usize).min(arr.len());
                let slice: Vec<Value> = arr[start..end].to_vec();
                Ok(Value::Array(Rc::new(RefCell::new(slice))))
            }
            _ => Err("arr_slice: expected (array, int, int)".to_string()),
        }
    });

    // arr_clone(arr: array) -> array
    vm.define_native("arr_clone", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let clone = arr.borrow().clone();
            Ok(Value::Array(Rc::new(RefCell::new(clone))))
        }
        other => Err(format!("arr_clone: expected array, got {}", other.type_name())),
    });

    // arr_concat(a: array, b: array) -> array
    vm.define_native("arr_concat", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::Array(a), Value::Array(b)) => {
                let mut result = a.borrow().clone();
                result.extend(b.borrow().iter().cloned());
                Ok(Value::Array(Rc::new(RefCell::new(result))))
            }
            _ => Err("arr_concat: expected two arrays".to_string()),
        }
    });

    // arr_new() -> array
    vm.define_native("arr_new", 0, |_args| {
        Ok(Value::Array(Rc::new(RefCell::new(Vec::new()))))
    });

    // arr_with_capacity(capacity: int) -> array
    vm.define_native("arr_with_capacity", 1, |args| match &args[0] {
        Value::Int(cap) => {
            Ok(Value::Array(Rc::new(RefCell::new(Vec::with_capacity(*cap as usize)))))
        }
        other => Err(format!(
            "arr_with_capacity: expected int, got {}",
            other.type_name()
        )),
    });

    // arr_repeat(value: any, count: int) -> array
    vm.define_native("arr_repeat", 2, |args| match &args[1] {
        Value::Int(count) => {
            let arr: Vec<Value> = (0..*count as usize)
                .map(|_| args[0].clone())
                .collect();
            Ok(Value::Array(Rc::new(RefCell::new(arr))))
        }
        other => Err(format!("arr_repeat: expected int count, got {}", other.type_name())),
    });

    // range(start: int, end: int) -> array
    vm.define_native("range", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::Int(start), Value::Int(end)) => {
                let arr: Vec<Value> = (*start..*end).map(Value::Int).collect();
                Ok(Value::Array(Rc::new(RefCell::new(arr))))
            }
            _ => Err("range: expected (int, int)".to_string()),
        }
    });

    // range_step(start: int, end: int, step: int) -> array
    vm.define_native("range_step", 3, |args| {
        match (&args[0], &args[1], &args[2]) {
            (Value::Int(start), Value::Int(end), Value::Int(step)) => {
                if *step == 0 {
                    return Err("range_step: step cannot be zero".to_string());
                }
                let arr: Vec<Value> = if *step > 0 {
                    (*start..*end).step_by(*step as usize).map(Value::Int).collect()
                } else {
                    let step = (-step) as usize;
                    (*end + 1..=*start).rev().step_by(step).map(Value::Int).collect()
                };
                Ok(Value::Array(Rc::new(RefCell::new(arr))))
            }
            _ => Err("range_step: expected (int, int, int)".to_string()),
        }
    });

    // arr_insert(arr: array, index: int, value: any) -> ()
    vm.define_native("arr_insert", 3, |args| {
        match (&args[0], &args[1]) {
            (Value::Array(arr), Value::Int(idx)) => {
                let mut arr = arr.borrow_mut();
                let idx = (*idx).max(0) as usize;
                if idx > arr.len() {
                    return Err(format!("arr_insert: index {} out of bounds", idx));
                }
                arr.insert(idx, args[2].clone());
                Ok(Value::Unit)
            }
            _ => Err("arr_insert: expected (array, int, value)".to_string()),
        }
    });

    // arr_remove(arr: array, index: int) -> any
    vm.define_native("arr_remove", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::Array(arr), Value::Int(idx)) => {
                let mut arr = arr.borrow_mut();
                let idx = (*idx).max(0) as usize;
                if idx >= arr.len() {
                    return Err(format!("arr_remove: index {} out of bounds", idx));
                }
                Ok(arr.remove(idx))
            }
            _ => Err("arr_remove: expected (array, int)".to_string()),
        }
    });

    // arr_sort(arr: array) -> () - sorts array of ints or floats in place
    vm.define_native("arr_sort", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let mut arr = arr.borrow_mut();
            arr.sort_by(|a, b| {
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => a.cmp(b),
                    (Value::Float(a), Value::Float(b)) => a.partial_cmp(b).unwrap_or(std::cmp::Ordering::Equal),
                    (Value::String(a), Value::String(b)) => a.cmp(b),
                    _ => std::cmp::Ordering::Equal,
                }
            });
            Ok(Value::Unit)
        }
        other => Err(format!("arr_sort: expected array, got {}", other.type_name())),
    });

    // arr_sort_desc(arr: array) -> () - sorts array descending
    vm.define_native("arr_sort_desc", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let mut arr = arr.borrow_mut();
            arr.sort_by(|a, b| {
                match (a, b) {
                    (Value::Int(a), Value::Int(b)) => b.cmp(a),
                    (Value::Float(a), Value::Float(b)) => b.partial_cmp(a).unwrap_or(std::cmp::Ordering::Equal),
                    (Value::String(a), Value::String(b)) => b.cmp(a),
                    _ => std::cmp::Ordering::Equal,
                }
            });
            Ok(Value::Unit)
        }
        other => Err(format!("arr_sort_desc: expected array, got {}", other.type_name())),
    });

    // arr_join(arr: array, sep: string) -> string
    vm.define_native("arr_join", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::Array(arr), Value::String(sep)) => {
                let arr = arr.borrow();
                let parts: Vec<String> = arr.iter().map(|v| format!("{}", v)).collect();
                Ok(Value::String(Rc::new(parts.join(sep.as_str()))))
            }
            _ => Err("arr_join: expected (array, string)".to_string()),
        }
    });

    // arr_flatten(arr: array) -> array - flatten one level
    vm.define_native("arr_flatten", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let mut result = Vec::new();
            for item in arr.iter() {
                match item {
                    Value::Array(inner) => result.extend(inner.borrow().iter().cloned()),
                    other => result.push(other.clone()),
                }
            }
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        other => Err(format!("arr_flatten: expected array, got {}", other.type_name())),
    });

    // arr_unique(arr: array) -> array - remove duplicates (preserves order)
    vm.define_native("arr_unique", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let mut seen = Vec::new();
            let mut result = Vec::new();
            for item in arr.iter() {
                if !seen.contains(item) {
                    seen.push(item.clone());
                    result.push(item.clone());
                }
            }
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        other => Err(format!("arr_unique: expected array, got {}", other.type_name())),
    });

    // arr_sum(arr: array) -> int|float
    vm.define_native("arr_sum", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let mut sum_int: i64 = 0;
            let mut sum_float: f64 = 0.0;
            let mut has_float = false;
            for item in arr.iter() {
                match item {
                    Value::Int(n) => {
                        sum_int += n;
                        sum_float += *n as f64;
                    }
                    Value::Float(n) => {
                        has_float = true;
                        sum_float += n;
                    }
                    _ => {}
                }
            }
            if has_float {
                Ok(Value::Float(sum_float))
            } else {
                Ok(Value::Int(sum_int))
            }
        }
        other => Err(format!("arr_sum: expected array, got {}", other.type_name())),
    });

    // arr_min(arr: array) -> int|float
    vm.define_native("arr_min", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let mut min: Option<Value> = None;
            for item in arr.iter() {
                match (&min, item) {
                    (None, v @ (Value::Int(_) | Value::Float(_))) => min = Some(v.clone()),
                    (Some(Value::Int(m)), Value::Int(n)) if n < m => min = Some(Value::Int(*n)),
                    (Some(Value::Float(m)), Value::Float(n)) if n < m => min = Some(Value::Float(*n)),
                    _ => {}
                }
            }
            min.ok_or_else(|| "arr_min: array is empty or contains no numbers".to_string())
        }
        other => Err(format!("arr_min: expected array, got {}", other.type_name())),
    });

    // arr_max(arr: array) -> int|float
    vm.define_native("arr_max", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let mut max: Option<Value> = None;
            for item in arr.iter() {
                match (&max, item) {
                    (None, v @ (Value::Int(_) | Value::Float(_))) => max = Some(v.clone()),
                    (Some(Value::Int(m)), Value::Int(n)) if n > m => max = Some(Value::Int(*n)),
                    (Some(Value::Float(m)), Value::Float(n)) if n > m => max = Some(Value::Float(*n)),
                    _ => {}
                }
            }
            max.ok_or_else(|| "arr_max: array is empty or contains no numbers".to_string())
        }
        other => Err(format!("arr_max: expected array, got {}", other.type_name())),
    });
}