use rand::Rng;
use squam_vm::{Value, VM};
use std::cell::RefCell;
use std::rc::Rc;

pub fn register(vm: &mut VM) {
    // random() -> float (0.0 to 1.0)
    vm.define_native("random", 0, |_args| {
        let mut rng = rand::thread_rng();
        Ok(Value::Float(rng.gen::<f64>()))
    });

    // random_int(min: int, max: int) -> int (inclusive range)
    vm.define_native("random_int", 2, |args| match (&args[0], &args[1]) {
        (Value::Int(min), Value::Int(max)) => {
            if max < min {
                return Err("random_int: max must be >= min".to_string());
            }
            let mut rng = rand::thread_rng();
            Ok(Value::Int(rng.gen_range(*min..=*max)))
        }
        _ => Err("random_int: expected (int, int)".to_string()),
    });

    // random_float(min: float, max: float) -> float
    vm.define_native("random_float", 2, |args| {
        let min = match &args[0] {
            Value::Float(f) => *f,
            Value::Int(i) => *i as f64,
            _ => return Err("random_float: expected (number, number)".to_string()),
        };
        let max = match &args[1] {
            Value::Float(f) => *f,
            Value::Int(i) => *i as f64,
            _ => return Err("random_float: expected (number, number)".to_string()),
        };
        if max < min {
            return Err("random_float: max must be >= min".to_string());
        }
        let mut rng = rand::thread_rng();
        Ok(Value::Float(rng.gen_range(min..max)))
    });

    // random_bool() -> bool
    vm.define_native("random_bool", 0, |_args| {
        let mut rng = rand::thread_rng();
        Ok(Value::Bool(rng.gen::<bool>()))
    });

    // random_choice(arr: [T]) -> T (random element from array)
    vm.define_native("random_choice", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            if arr.is_empty() {
                return Err("random_choice: array is empty".to_string());
            }
            let mut rng = rand::thread_rng();
            let idx = rng.gen_range(0..arr.len());
            Ok(arr[idx].clone())
        }
        _ => Err("random_choice: expected array".to_string()),
    });

    // shuffle(arr: [T]) -> [T] (returns new shuffled array)
    vm.define_native("shuffle", 1, |args| {
        match &args[0] {
            Value::Array(arr) => {
                let mut new_arr: Vec<Value> = arr.borrow().clone();
                let mut rng = rand::thread_rng();
                // Fisher-Yates shuffle
                for i in (1..new_arr.len()).rev() {
                    let j = rng.gen_range(0..=i);
                    new_arr.swap(i, j);
                }
                Ok(Value::Array(Rc::new(RefCell::new(new_arr))))
            }
            _ => Err("shuffle: expected array".to_string()),
        }
    });

    // random_string(len: int) -> string (alphanumeric)
    vm.define_native("random_string", 1, |args| match &args[0] {
        Value::Int(len) => {
            if *len < 0 {
                return Err("random_string: length must be >= 0".to_string());
            }
            let mut rng = rand::thread_rng();
            let chars: String = (0..*len as usize)
                .map(|_| {
                    let idx = rng.gen_range(0..62);
                    match idx {
                        0..=25 => (b'a' + idx as u8) as char,
                        26..=51 => (b'A' + (idx - 26) as u8) as char,
                        _ => (b'0' + (idx - 52) as u8) as char,
                    }
                })
                .collect();
            Ok(Value::String(Rc::new(chars)))
        }
        _ => Err("random_string: expected int".to_string()),
    });

    // uuid() -> string (generates a random UUID v4)
    vm.define_native("uuid", 0, |_args| {
        let mut rng = rand::thread_rng();
        let bytes: Vec<u8> = (0..16).map(|_| rng.gen::<u8>()).collect();

        // Set version (4) and variant (RFC 4122)
        let uuid = format!(
            "{:02x}{:02x}{:02x}{:02x}-{:02x}{:02x}-4{:01x}{:02x}-{:01x}{:02x}-{:02x}{:02x}{:02x}{:02x}{:02x}{:02x}",
            bytes[0], bytes[1], bytes[2], bytes[3],
            bytes[4], bytes[5],
            bytes[6] & 0x0f, bytes[7],
            (bytes[8] & 0x3f) | 0x80, bytes[9],
            bytes[10], bytes[11], bytes[12], bytes[13], bytes[14], bytes[15]
        );
        Ok(Value::String(Rc::new(uuid)))
    });

    // sample(arr: [T], n: int) -> [T] (random sample without replacement)
    vm.define_native("sample", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::Array(arr), Value::Int(n)) => {
                let arr = arr.borrow();
                let n = *n as usize;
                if n > arr.len() {
                    return Err("sample: n must be <= array length".to_string());
                }
                let mut rng = rand::thread_rng();
                let mut indices: Vec<usize> = (0..arr.len()).collect();
                // Partial Fisher-Yates
                for i in 0..n {
                    let j = rng.gen_range(i..indices.len());
                    indices.swap(i, j);
                }
                let result: Vec<Value> = indices[0..n].iter().map(|&i| arr[i].clone()).collect();
                Ok(Value::Array(Rc::new(RefCell::new(result))))
            }
            _ => Err("sample: expected (array, int)".to_string()),
        }
    });
}
