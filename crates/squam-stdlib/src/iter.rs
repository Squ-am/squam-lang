use crate::option::{none, some};
use squam_vm::{RuntimeError, Value, VM};
use std::cell::RefCell;
use std::rc::Rc;

fn iter_map(vm: &mut VM, args: &[Value]) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Closure(closure)) => {
            let arr = arr.borrow();
            let mut result = Vec::with_capacity(arr.len());
            for item in arr.iter() {
                let mapped = vm.call_closure_value(closure, vec![item.clone()])?;
                result.push(mapped);
            }
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        _ => Err(RuntimeError::Custom(
            "map: expected (array, closure)".to_string(),
        )),
    }
}

fn iter_filter(vm: &mut VM, args: &[Value]) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Closure(closure)) => {
            let arr = arr.borrow();
            let mut result = Vec::new();
            for item in arr.iter() {
                let pred = vm.call_closure_value(closure, vec![item.clone()])?;
                if pred.is_truthy() {
                    result.push(item.clone());
                }
            }
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        _ => Err(RuntimeError::Custom(
            "filter: expected (array, closure)".to_string(),
        )),
    }
}

fn iter_reduce(vm: &mut VM, args: &[Value]) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1], &args[2]) {
        (Value::Array(arr), Value::Closure(closure), init) => {
            let arr = arr.borrow();
            let mut acc = init.clone();
            for item in arr.iter() {
                acc = vm.call_closure_value(closure, vec![acc, item.clone()])?;
            }
            Ok(acc)
        }
        _ => Err(RuntimeError::Custom(
            "reduce: expected (array, closure, init)".to_string(),
        )),
    }
}

fn iter_foreach(vm: &mut VM, args: &[Value]) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Closure(closure)) => {
            let arr = arr.borrow();
            for item in arr.iter() {
                vm.call_closure_value(closure, vec![item.clone()])?;
            }
            Ok(Value::Unit)
        }
        _ => Err(RuntimeError::Custom(
            "foreach: expected (array, closure)".to_string(),
        )),
    }
}

fn iter_find(vm: &mut VM, args: &[Value]) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Closure(closure)) => {
            let arr = arr.borrow();
            for item in arr.iter() {
                let pred = vm.call_closure_value(closure, vec![item.clone()])?;
                if pred.is_truthy() {
                    return Ok(some(item.clone()));
                }
            }
            Ok(none())
        }
        _ => Err(RuntimeError::Custom(
            "find: expected (array, closure)".to_string(),
        )),
    }
}

fn iter_find_index(vm: &mut VM, args: &[Value]) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Closure(closure)) => {
            let arr = arr.borrow();
            for (i, item) in arr.iter().enumerate() {
                let pred = vm.call_closure_value(closure, vec![item.clone()])?;
                if pred.is_truthy() {
                    return Ok(some(Value::Int(i as i64)));
                }
            }
            Ok(none())
        }
        _ => Err(RuntimeError::Custom(
            "find_index: expected (array, closure)".to_string(),
        )),
    }
}

fn iter_any_with(vm: &mut VM, args: &[Value]) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Closure(closure)) => {
            let arr = arr.borrow();
            for item in arr.iter() {
                let pred = vm.call_closure_value(closure, vec![item.clone()])?;
                if pred.is_truthy() {
                    return Ok(Value::Bool(true));
                }
            }
            Ok(Value::Bool(false))
        }
        _ => Err(RuntimeError::Custom(
            "any_with: expected (array, closure)".to_string(),
        )),
    }
}

fn iter_all_with(vm: &mut VM, args: &[Value]) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Closure(closure)) => {
            let arr = arr.borrow();
            for item in arr.iter() {
                let pred = vm.call_closure_value(closure, vec![item.clone()])?;
                if !pred.is_truthy() {
                    return Ok(Value::Bool(false));
                }
            }
            Ok(Value::Bool(true))
        }
        _ => Err(RuntimeError::Custom(
            "all_with: expected (array, closure)".to_string(),
        )),
    }
}

fn iter_count_with(vm: &mut VM, args: &[Value]) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Closure(closure)) => {
            let arr = arr.borrow();
            let mut count = 0i64;
            for item in arr.iter() {
                let pred = vm.call_closure_value(closure, vec![item.clone()])?;
                if pred.is_truthy() {
                    count += 1;
                }
            }
            Ok(Value::Int(count))
        }
        _ => Err(RuntimeError::Custom(
            "count_with: expected (array, closure)".to_string(),
        )),
    }
}

fn iter_partition(vm: &mut VM, args: &[Value]) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Closure(closure)) => {
            let arr = arr.borrow();
            let mut passing = Vec::new();
            let mut failing = Vec::new();
            for item in arr.iter() {
                let pred = vm.call_closure_value(closure, vec![item.clone()])?;
                if pred.is_truthy() {
                    passing.push(item.clone());
                } else {
                    failing.push(item.clone());
                }
            }
            Ok(Value::Tuple(Rc::new(vec![
                Value::Array(Rc::new(RefCell::new(passing))),
                Value::Array(Rc::new(RefCell::new(failing))),
            ])))
        }
        _ => Err(RuntimeError::Custom(
            "partition: expected (array, closure)".to_string(),
        )),
    }
}

fn iter_sort_by(vm: &mut VM, args: &[Value]) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Closure(closure)) => {
            let arr_borrowed = arr.borrow();
            let mut result: Vec<Value> = arr_borrowed.clone();
            drop(arr_borrowed);

            // Sort using closure as comparator: closure(a, b) returns negative if a < b, 0 if equal, positive if a > b
            // We need to do this in a way that works with VM calls
            // Simple approach: bubble sort (not efficient, but works with our constraints)
            let len = result.len();
            for i in 0..len {
                for j in 0..len - 1 - i {
                    let cmp_result = vm.call_closure_value(
                        closure,
                        vec![result[j].clone(), result[j + 1].clone()],
                    )?;
                    let should_swap = match cmp_result {
                        Value::Int(n) => n > 0,
                        Value::Bool(b) => b,
                        _ => false,
                    };
                    if should_swap {
                        result.swap(j, j + 1);
                    }
                }
            }
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        _ => Err(RuntimeError::Custom(
            "sort_by: expected (array, closure)".to_string(),
        )),
    }
}

fn iter_group_by(vm: &mut VM, args: &[Value]) -> Result<Value, RuntimeError> {
    use std::collections::HashMap;
    match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Closure(closure)) => {
            let arr = arr.borrow();
            let mut groups: HashMap<String, Vec<Value>> = HashMap::new();
            for item in arr.iter() {
                let key = vm.call_closure_value(closure, vec![item.clone()])?;
                // Convert key to string for HashMap (simple approach)
                let key_str = format!("{}", key);
                groups.entry(key_str).or_default().push(item.clone());
            }
            // Return as array of [key, values] tuples
            let result: Vec<Value> = groups
                .into_iter()
                .map(|(k, v)| {
                    Value::Tuple(Rc::new(vec![
                        Value::String(Rc::new(k)),
                        Value::Array(Rc::new(RefCell::new(v))),
                    ]))
                })
                .collect();
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        _ => Err(RuntimeError::Custom(
            "group_by: expected (array, closure)".to_string(),
        )),
    }
}

fn iter_flat_map(vm: &mut VM, args: &[Value]) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Closure(closure)) => {
            let arr = arr.borrow();
            let mut result = Vec::new();
            for item in arr.iter() {
                let mapped = vm.call_closure_value(closure, vec![item.clone()])?;
                // Flatten the result
                match mapped {
                    Value::Array(inner) => {
                        result.extend(inner.borrow().iter().cloned());
                    }
                    other => result.push(other),
                }
            }
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        _ => Err(RuntimeError::Custom(
            "flat_map: expected (array, closure)".to_string(),
        )),
    }
}

fn iter_take_while(vm: &mut VM, args: &[Value]) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Closure(closure)) => {
            let arr = arr.borrow();
            let mut result = Vec::new();
            for item in arr.iter() {
                let pred = vm.call_closure_value(closure, vec![item.clone()])?;
                if pred.is_truthy() {
                    result.push(item.clone());
                } else {
                    break;
                }
            }
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        _ => Err(RuntimeError::Custom(
            "take_while: expected (array, closure)".to_string(),
        )),
    }
}

fn iter_skip_while(vm: &mut VM, args: &[Value]) -> Result<Value, RuntimeError> {
    match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Closure(closure)) => {
            let arr = arr.borrow();
            let mut result = Vec::new();
            let mut taking = false;
            for item in arr.iter() {
                if !taking {
                    let pred = vm.call_closure_value(closure, vec![item.clone()])?;
                    if !pred.is_truthy() {
                        taking = true;
                        result.push(item.clone());
                    }
                } else {
                    result.push(item.clone());
                }
            }
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        _ => Err(RuntimeError::Custom(
            "skip_while: expected (array, closure)".to_string(),
        )),
    }
}

pub fn register(vm: &mut VM) {
    // Register VM-native functions that can call closures
    vm.define_vm_native("map", 2, iter_map);
    vm.define_vm_native("filter", 2, iter_filter);
    vm.define_vm_native("reduce", 3, iter_reduce);
    vm.define_vm_native("foreach", 2, iter_foreach);
    vm.define_vm_native("find", 2, iter_find);
    vm.define_vm_native("find_index", 2, iter_find_index);
    vm.define_vm_native("any_with", 2, iter_any_with);
    vm.define_vm_native("all_with", 2, iter_all_with);
    vm.define_vm_native("count_with", 2, iter_count_with);
    vm.define_vm_native("partition", 2, iter_partition);
    vm.define_vm_native("sort_by", 2, iter_sort_by);
    vm.define_vm_native("group_by", 2, iter_group_by);
    vm.define_vm_native("flat_map", 2, iter_flat_map);
    vm.define_vm_native("take_while", 2, iter_take_while);
    vm.define_vm_native("skip_while", 2, iter_skip_while);

    // enumerate(arr: array) -> array of (index, value) tuples
    vm.define_native("enumerate", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let result: Vec<Value> = arr
                .iter()
                .enumerate()
                .map(|(i, v)| Value::Tuple(Rc::new(vec![Value::Int(i as i64), v.clone()])))
                .collect();
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        other => Err(format!(
            "enumerate: expected array, got {}",
            other.type_name()
        )),
    });

    // zip(a: array, b: array) -> array of tuples
    vm.define_native("zip", 2, |args| match (&args[0], &args[1]) {
        (Value::Array(a), Value::Array(b)) => {
            let a = a.borrow();
            let b = b.borrow();
            let result: Vec<Value> = a
                .iter()
                .zip(b.iter())
                .map(|(x, y)| Value::Tuple(Rc::new(vec![x.clone(), y.clone()])))
                .collect();
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        _ => Err("zip: expected two arrays".to_string()),
    });

    // unzip(arr: array of tuples) -> (array, array)
    vm.define_native("unzip", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let mut a = Vec::with_capacity(arr.len());
            let mut b = Vec::with_capacity(arr.len());
            for item in arr.iter() {
                match item {
                    Value::Tuple(t) if t.len() >= 2 => {
                        a.push(t[0].clone());
                        b.push(t[1].clone());
                    }
                    _ => return Err("unzip: expected array of tuples".to_string()),
                }
            }
            Ok(Value::Tuple(Rc::new(vec![
                Value::Array(Rc::new(RefCell::new(a))),
                Value::Array(Rc::new(RefCell::new(b))),
            ])))
        }
        other => Err(format!("unzip: expected array, got {}", other.type_name())),
    });

    // flatten(arr: array of arrays) -> array
    vm.define_native("flatten", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let mut result = Vec::new();
            for item in arr.iter() {
                match item {
                    Value::Array(inner) => {
                        result.extend(inner.borrow().iter().cloned());
                    }
                    _ => result.push(item.clone()),
                }
            }
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        other => Err(format!(
            "flatten: expected array, got {}",
            other.type_name()
        )),
    });

    // take(arr: array, n: int) -> array
    vm.define_native("take", 2, |args| match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Int(n)) => {
            let arr = arr.borrow();
            let n = (*n).max(0) as usize;
            let result: Vec<Value> = arr.iter().take(n).cloned().collect();
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        _ => Err("take: expected (array, int)".to_string()),
    });

    // skip(arr: array, n: int) -> array
    vm.define_native("skip", 2, |args| match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Int(n)) => {
            let arr = arr.borrow();
            let n = (*n).max(0) as usize;
            let result: Vec<Value> = arr.iter().skip(n).cloned().collect();
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        _ => Err("skip: expected (array, int)".to_string()),
    });

    // take_while_positive(arr: array) -> array
    vm.define_native("take_while_positive", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let result: Vec<Value> = arr
                .iter()
                .take_while(|v| match v {
                    Value::Int(n) => *n > 0,
                    Value::Float(n) => *n > 0.0,
                    _ => false,
                })
                .cloned()
                .collect();
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        other => Err(format!(
            "take_while_positive: expected array, got {}",
            other.type_name()
        )),
    });

    // filter_some(arr: array) -> array (removes None/Unit values)
    vm.define_native("filter_some", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let result: Vec<Value> = arr
                .iter()
                .filter(|v| !matches!(v, Value::Unit))
                .cloned()
                .collect();
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        other => Err(format!(
            "filter_some: expected array, got {}",
            other.type_name()
        )),
    });

    // sum(arr: array) -> int|float
    vm.define_native("sum", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let mut int_sum: i64 = 0;
            let mut float_sum: f64 = 0.0;
            let mut is_float = false;

            for v in arr.iter() {
                match v {
                    Value::Int(n) => {
                        if is_float {
                            float_sum += *n as f64;
                        } else {
                            int_sum += n;
                        }
                    }
                    Value::Float(n) => {
                        if !is_float {
                            float_sum = int_sum as f64;
                            is_float = true;
                        }
                        float_sum += n;
                    }
                    _ => return Err("sum: array must contain numbers".to_string()),
                }
            }

            if is_float {
                Ok(Value::Float(float_sum))
            } else {
                Ok(Value::Int(int_sum))
            }
        }
        other => Err(format!("sum: expected array, got {}", other.type_name())),
    });

    // product(arr: array) -> int|float
    vm.define_native("product", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let mut int_prod: i64 = 1;
            let mut float_prod: f64 = 1.0;
            let mut is_float = false;

            for v in arr.iter() {
                match v {
                    Value::Int(n) => {
                        if is_float {
                            float_prod *= *n as f64;
                        } else {
                            int_prod *= n;
                        }
                    }
                    Value::Float(n) => {
                        if !is_float {
                            float_prod = int_prod as f64;
                            is_float = true;
                        }
                        float_prod *= n;
                    }
                    _ => return Err("product: array must contain numbers".to_string()),
                }
            }

            if is_float {
                Ok(Value::Float(float_prod))
            } else {
                Ok(Value::Int(int_prod))
            }
        }
        other => Err(format!(
            "product: expected array, got {}",
            other.type_name()
        )),
    });

    // any(arr: array) -> bool (true if any element is truthy)
    vm.define_native("any", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            Ok(Value::Bool(arr.iter().any(|v| v.is_truthy())))
        }
        other => Err(format!("any: expected array, got {}", other.type_name())),
    });

    // all(arr: array) -> bool (true if all elements are truthy)
    vm.define_native("all", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            Ok(Value::Bool(arr.iter().all(|v| v.is_truthy())))
        }
        other => Err(format!("all: expected array, got {}", other.type_name())),
    });

    // count(arr: array) -> int (count truthy elements)
    vm.define_native("count", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            Ok(Value::Int(
                arr.iter().filter(|v| v.is_truthy()).count() as i64
            ))
        }
        other => Err(format!("count: expected array, got {}", other.type_name())),
    });

    // min_of(arr: array) -> int|float
    vm.define_native("min_of", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            if arr.is_empty() {
                return Err("min_of: empty array".to_string());
            }
            let mut result = &arr[0];
            for v in arr.iter().skip(1) {
                match (result, v) {
                    (Value::Int(a), Value::Int(b)) if b < a => result = v,
                    (Value::Float(a), Value::Float(b)) if b < a => result = v,
                    (Value::Int(a), Value::Float(b)) if *b < *a as f64 => result = v,
                    (Value::Float(a), Value::Int(b)) if (*b as f64) < *a => result = v,
                    _ => {}
                }
            }
            Ok(result.clone())
        }
        other => Err(format!("min_of: expected array, got {}", other.type_name())),
    });

    // max_of(arr: array) -> int|float
    vm.define_native("max_of", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            if arr.is_empty() {
                return Err("max_of: empty array".to_string());
            }
            let mut result = &arr[0];
            for v in arr.iter().skip(1) {
                match (result, v) {
                    (Value::Int(a), Value::Int(b)) if b > a => result = v,
                    (Value::Float(a), Value::Float(b)) if b > a => result = v,
                    (Value::Int(a), Value::Float(b)) if *b > *a as f64 => result = v,
                    (Value::Float(a), Value::Int(b)) if (*b as f64) > *a => result = v,
                    _ => {}
                }
            }
            Ok(result.clone())
        }
        other => Err(format!("max_of: expected array, got {}", other.type_name())),
    });

    // sorted(arr: array) -> array (returns new sorted array for numbers)
    vm.define_native("sorted", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let mut result: Vec<Value> = arr.clone();
            result.sort_by(|a, b| match (a, b) {
                (Value::Int(x), Value::Int(y)) => x.cmp(y),
                (Value::Float(x), Value::Float(y)) => {
                    x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal)
                }
                (Value::String(x), Value::String(y)) => x.cmp(y),
                _ => std::cmp::Ordering::Equal,
            });
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        other => Err(format!("sorted: expected array, got {}", other.type_name())),
    });

    // reversed(arr: array) -> array (returns new reversed array)
    vm.define_native("reversed", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let mut result: Vec<Value> = arr.clone();
            result.reverse();
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        Value::String(s) => Ok(Value::String(Rc::new(s.chars().rev().collect()))),
        other => Err(format!(
            "reversed: expected array or string, got {}",
            other.type_name()
        )),
    });

    // unique(arr: array) -> array (remove duplicates, preserve order)
    vm.define_native("unique", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let mut result = Vec::new();
            for v in arr.iter() {
                if !result.contains(v) {
                    result.push(v.clone());
                }
            }
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        other => Err(format!("unique: expected array, got {}", other.type_name())),
    });

    // chunks(arr: array, size: int) -> array of arrays
    vm.define_native("chunks", 2, |args| match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Int(size)) => {
            if *size <= 0 {
                return Err("chunks: size must be positive".to_string());
            }
            let arr = arr.borrow();
            let size = *size as usize;
            let result: Vec<Value> = arr
                .chunks(size)
                .map(|chunk| Value::Array(Rc::new(RefCell::new(chunk.to_vec()))))
                .collect();
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        _ => Err("chunks: expected (array, int)".to_string()),
    });

    // windows(arr: array, size: int) -> array of arrays
    vm.define_native("windows", 2, |args| match (&args[0], &args[1]) {
        (Value::Array(arr), Value::Int(size)) => {
            if *size <= 0 {
                return Err("windows: size must be positive".to_string());
            }
            let arr = arr.borrow();
            let size = *size as usize;
            if size > arr.len() {
                return Ok(Value::Array(Rc::new(RefCell::new(Vec::new()))));
            }
            let result: Vec<Value> = arr
                .windows(size)
                .map(|window| Value::Array(Rc::new(RefCell::new(window.to_vec()))))
                .collect();
            Ok(Value::Array(Rc::new(RefCell::new(result))))
        }
        _ => Err("windows: expected (array, int)".to_string()),
    });
}
