use squam_vm::{Value, VM};
use std::rc::Rc;

pub fn register(vm: &mut VM) {
    // str_len(s: string) -> int
    vm.define_native("str_len", 1, |args| match &args[0] {
        Value::String(s) => Ok(Value::Int(s.len() as i64)),
        other => Err(format!("str_len: expected string, got {}", other.type_name())),
    });

    // str_concat(a: string, b: string) -> string
    vm.define_native("str_concat", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::String(a), Value::String(b)) => {
                Ok(Value::String(Rc::new(format!("{}{}", a, b))))
            }
            _ => Err("str_concat: expected two strings".to_string()),
        }
    });

    // str_substring(s: string, start: int, end: int) -> string
    vm.define_native("str_substring", 3, |args| {
        match (&args[0], &args[1], &args[2]) {
            (Value::String(s), Value::Int(start), Value::Int(end)) => {
                let start = (*start).max(0) as usize;
                let end = (*end).max(0) as usize;
                let result: String = s.chars().skip(start).take(end.saturating_sub(start)).collect();
                Ok(Value::String(Rc::new(result)))
            }
            _ => Err("str_substring: expected (string, int, int)".to_string()),
        }
    });

    // str_contains(haystack: string, needle: string) -> bool
    vm.define_native("str_contains", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::String(haystack), Value::String(needle)) => {
                Ok(Value::Bool(haystack.contains(needle.as_str())))
            }
            _ => Err("str_contains: expected two strings".to_string()),
        }
    });

    // str_starts_with(s: string, prefix: string) -> bool
    vm.define_native("str_starts_with", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::String(s), Value::String(prefix)) => {
                Ok(Value::Bool(s.starts_with(prefix.as_str())))
            }
            _ => Err("str_starts_with: expected two strings".to_string()),
        }
    });

    // str_ends_with(s: string, suffix: string) -> bool
    vm.define_native("str_ends_with", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::String(s), Value::String(suffix)) => {
                Ok(Value::Bool(s.ends_with(suffix.as_str())))
            }
            _ => Err("str_ends_with: expected two strings".to_string()),
        }
    });

    // str_trim(s: string) -> string
    vm.define_native("str_trim", 1, |args| match &args[0] {
        Value::String(s) => Ok(Value::String(Rc::new(s.trim().to_string()))),
        other => Err(format!("str_trim: expected string, got {}", other.type_name())),
    });

    // str_to_upper(s: string) -> string
    vm.define_native("str_to_upper", 1, |args| match &args[0] {
        Value::String(s) => Ok(Value::String(Rc::new(s.to_uppercase()))),
        other => Err(format!("str_to_upper: expected string, got {}", other.type_name())),
    });

    // str_to_lower(s: string) -> string
    vm.define_native("str_to_lower", 1, |args| match &args[0] {
        Value::String(s) => Ok(Value::String(Rc::new(s.to_lowercase()))),
        other => Err(format!("str_to_lower: expected string, got {}", other.type_name())),
    });

    // str_replace(s: string, from: string, to: string) -> string
    vm.define_native("str_replace", 3, |args| {
        match (&args[0], &args[1], &args[2]) {
            (Value::String(s), Value::String(from), Value::String(to)) => {
                Ok(Value::String(Rc::new(s.replace(from.as_str(), to.as_str()))))
            }
            _ => Err("str_replace: expected (string, string, string)".to_string()),
        }
    });

    // str_split(s: string, delimiter: string) -> array
    vm.define_native("str_split", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::String(s), Value::String(delim)) => {
                let parts: Vec<Value> = s
                    .split(delim.as_str())
                    .map(|part| Value::String(Rc::new(part.to_string())))
                    .collect();
                Ok(Value::Array(Rc::new(std::cell::RefCell::new(parts))))
            }
            _ => Err("str_split: expected two strings".to_string()),
        }
    });

    // str_join(arr: array, separator: string) -> string
    vm.define_native("str_join", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::Array(arr), Value::String(sep)) => {
                let arr = arr.borrow();
                let parts: Vec<String> = arr
                    .iter()
                    .map(|v| format!("{}", v))
                    .collect();
                Ok(Value::String(Rc::new(parts.join(sep.as_str()))))
            }
            _ => Err("str_join: expected (array, string)".to_string()),
        }
    });

    // str_index_of(s: string, needle: string) -> int
    vm.define_native("str_index_of", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::String(s), Value::String(needle)) => {
                match s.find(needle.as_str()) {
                    Some(idx) => Ok(Value::Int(idx as i64)),
                    None => Ok(Value::Int(-1)),
                }
            }
            _ => Err("str_index_of: expected two strings".to_string()),
        }
    });

    // str_char_at(s: string, index: int) -> int (char code)
    vm.define_native("str_char_at", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::String(s), Value::Int(idx)) => {
                let idx = *idx as usize;
                match s.chars().nth(idx) {
                    Some(c) => Ok(Value::Int(c as i64)),
                    None => Err(format!("str_char_at: index {} out of bounds", idx)),
                }
            }
            _ => Err("str_char_at: expected (string, int)".to_string()),
        }
    });

    // char_to_str(code: int) -> string
    vm.define_native("char_to_str", 1, |args| match &args[0] {
        Value::Int(code) => {
            match char::from_u32(*code as u32) {
                Some(c) => Ok(Value::String(Rc::new(c.to_string()))),
                None => Err(format!("char_to_str: invalid char code {}", code)),
            }
        }
        other => Err(format!("char_to_str: expected int, got {}", other.type_name())),
    });

    // parse_int(s: string) -> int (or panic)
    vm.define_native("parse_int", 1, |args| match &args[0] {
        Value::String(s) => {
            s.trim()
                .parse::<i64>()
                .map(Value::Int)
                .map_err(|_| format!("parse_int: cannot parse '{}'", s))
        }
        other => Err(format!("parse_int: expected string, got {}", other.type_name())),
    });

    // parse_float(s: string) -> float (or panic)
    vm.define_native("parse_float", 1, |args| match &args[0] {
        Value::String(s) => {
            s.trim()
                .parse::<f64>()
                .map(Value::Float)
                .map_err(|_| format!("parse_float: cannot parse '{}'", s))
        }
        other => Err(format!("parse_float: expected string, got {}", other.type_name())),
    });

    // int_to_str(n: int) -> string
    vm.define_native("int_to_str", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::String(Rc::new(n.to_string()))),
        other => Err(format!("int_to_str: expected int, got {}", other.type_name())),
    });

    // float_to_str(n: float) -> string
    vm.define_native("float_to_str", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::String(Rc::new(n.to_string()))),
        other => Err(format!("float_to_str: expected float, got {}", other.type_name())),
    });

    // is_digit(c: int) -> bool - check if char code is ASCII digit
    vm.define_native("is_digit", 1, |args| match &args[0] {
        Value::Int(c) => {
            let is_digit = (*c as u32).checked_into_char()
                .map(|c| c.is_ascii_digit())
                .unwrap_or(false);
            Ok(Value::Bool(is_digit))
        }
        other => Err(format!("is_digit: expected int (char code), got {}", other.type_name())),
    });

    // is_alpha(c: int) -> bool - check if char code is ASCII alphabetic
    vm.define_native("is_alpha", 1, |args| match &args[0] {
        Value::Int(c) => {
            let is_alpha = (*c as u32).checked_into_char()
                .map(|c| c.is_ascii_alphabetic())
                .unwrap_or(false);
            Ok(Value::Bool(is_alpha))
        }
        other => Err(format!("is_alpha: expected int (char code), got {}", other.type_name())),
    });

    // is_alphanumeric(c: int) -> bool
    vm.define_native("is_alphanumeric", 1, |args| match &args[0] {
        Value::Int(c) => {
            let is_alnum = (*c as u32).checked_into_char()
                .map(|c| c.is_ascii_alphanumeric())
                .unwrap_or(false);
            Ok(Value::Bool(is_alnum))
        }
        other => Err(format!("is_alphanumeric: expected int (char code), got {}", other.type_name())),
    });

    // is_whitespace(c: int) -> bool
    vm.define_native("is_whitespace", 1, |args| match &args[0] {
        Value::Int(c) => {
            let is_ws = (*c as u32).checked_into_char()
                .map(|c| c.is_ascii_whitespace())
                .unwrap_or(false);
            Ok(Value::Bool(is_ws))
        }
        other => Err(format!("is_whitespace: expected int (char code), got {}", other.type_name())),
    });

    // to_upper_char(c: int) -> int - convert char to uppercase
    vm.define_native("to_upper_char", 1, |args| match &args[0] {
        Value::Int(c) => {
            let upper = (*c as u32).checked_into_char()
                .map(|c| c.to_ascii_uppercase() as i64)
                .unwrap_or(*c);
            Ok(Value::Int(upper))
        }
        other => Err(format!("to_upper_char: expected int (char code), got {}", other.type_name())),
    });

    // to_lower_char(c: int) -> int - convert char to lowercase
    vm.define_native("to_lower_char", 1, |args| match &args[0] {
        Value::Int(c) => {
            let lower = (*c as u32).checked_into_char()
                .map(|c| c.to_ascii_lowercase() as i64)
                .unwrap_or(*c);
            Ok(Value::Int(lower))
        }
        other => Err(format!("to_lower_char: expected int (char code), got {}", other.type_name())),
    });
}

trait CheckedIntoChar {
    fn checked_into_char(self) -> Option<char>;
}

impl CheckedIntoChar for u32 {
    fn checked_into_char(self) -> Option<char> {
        char::from_u32(self)
    }
}