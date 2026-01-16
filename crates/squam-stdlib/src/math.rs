use squam_vm::{Value, VM};

pub fn register(vm: &mut VM) {
    // abs(x: int|float) -> int|float
    vm.define_native("abs", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::Int(n.abs())),
        Value::Float(n) => Ok(Value::Float(n.abs())),
        other => Err(format!("abs: expected number, got {}", other.type_name())),
    });

    // min(a: int|float, b: int|float) -> int|float
    vm.define_native("min", 2, |args| match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.min(b))),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.min(*b))),
        (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64).min(*b))),
        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a.min(*b as f64))),
        _ => Err("min: expected two numbers".to_string()),
    });

    // max(a: int|float, b: int|float) -> int|float
    vm.define_native("max", 2, |args| match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => Ok(Value::Int(*a.max(b))),
        (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.max(*b))),
        (Value::Int(a), Value::Float(b)) => Ok(Value::Float((*a as f64).max(*b))),
        (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a.max(*b as f64))),
        _ => Err("max: expected two numbers".to_string()),
    });

    // clamp(x: int|float, min: int|float, max: int|float) -> int|float
    vm.define_native("clamp", 3, |args| match (&args[0], &args[1], &args[2]) {
        (Value::Int(x), Value::Int(min), Value::Int(max)) => Ok(Value::Int(*x.max(min).min(max))),
        (Value::Float(x), Value::Float(min), Value::Float(max)) => {
            Ok(Value::Float(x.max(*min).min(*max)))
        }
        _ => Err("clamp: expected three numbers of same type".to_string()),
    });

    // floor(x: float) -> int
    vm.define_native("floor", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Int(n.floor() as i64)),
        Value::Int(n) => Ok(Value::Int(*n)),
        other => Err(format!("floor: expected number, got {}", other.type_name())),
    });

    // ceil(x: float) -> int
    vm.define_native("ceil", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Int(n.ceil() as i64)),
        Value::Int(n) => Ok(Value::Int(*n)),
        other => Err(format!("ceil: expected number, got {}", other.type_name())),
    });

    // round(x: float) -> int
    vm.define_native("round", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Int(n.round() as i64)),
        Value::Int(n) => Ok(Value::Int(*n)),
        other => Err(format!("round: expected number, got {}", other.type_name())),
    });

    // trunc(x: float) -> int
    vm.define_native("trunc", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Int(n.trunc() as i64)),
        Value::Int(n) => Ok(Value::Int(*n)),
        other => Err(format!("trunc: expected number, got {}", other.type_name())),
    });

    // sqrt(x: float) -> float
    vm.define_native("sqrt", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Float(n.sqrt())),
        Value::Int(n) => Ok(Value::Float((*n as f64).sqrt())),
        other => Err(format!("sqrt: expected number, got {}", other.type_name())),
    });

    // pow(base: float, exp: float) -> float
    vm.define_native("pow", 2, |args| match (&args[0], &args[1]) {
        (Value::Int(base), Value::Int(exp)) => {
            if *exp >= 0 {
                Ok(Value::Int(base.pow(*exp as u32)))
            } else {
                Ok(Value::Float((*base as f64).powi(*exp as i32)))
            }
        }
        (Value::Float(base), Value::Float(exp)) => Ok(Value::Float(base.powf(*exp))),
        (Value::Int(base), Value::Float(exp)) => Ok(Value::Float((*base as f64).powf(*exp))),
        (Value::Float(base), Value::Int(exp)) => Ok(Value::Float(base.powi(*exp as i32))),
        _ => Err("pow: expected two numbers".to_string()),
    });

    // exp(x: float) -> float
    vm.define_native("exp", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Float(n.exp())),
        Value::Int(n) => Ok(Value::Float((*n as f64).exp())),
        other => Err(format!("exp: expected number, got {}", other.type_name())),
    });

    // ln(x: float) -> float
    vm.define_native("ln", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Float(n.ln())),
        Value::Int(n) => Ok(Value::Float((*n as f64).ln())),
        other => Err(format!("ln: expected number, got {}", other.type_name())),
    });

    // log10(x: float) -> float
    vm.define_native("log10", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Float(n.log10())),
        Value::Int(n) => Ok(Value::Float((*n as f64).log10())),
        other => Err(format!("log10: expected number, got {}", other.type_name())),
    });

    // log2(x: float) -> float
    vm.define_native("log2", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Float(n.log2())),
        Value::Int(n) => Ok(Value::Float((*n as f64).log2())),
        other => Err(format!("log2: expected number, got {}", other.type_name())),
    });

    // sin(x: float) -> float
    vm.define_native("sin", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Float(n.sin())),
        Value::Int(n) => Ok(Value::Float((*n as f64).sin())),
        other => Err(format!("sin: expected number, got {}", other.type_name())),
    });

    // cos(x: float) -> float
    vm.define_native("cos", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Float(n.cos())),
        Value::Int(n) => Ok(Value::Float((*n as f64).cos())),
        other => Err(format!("cos: expected number, got {}", other.type_name())),
    });

    // tan(x: float) -> float
    vm.define_native("tan", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Float(n.tan())),
        Value::Int(n) => Ok(Value::Float((*n as f64).tan())),
        other => Err(format!("tan: expected number, got {}", other.type_name())),
    });

    // asin(x: float) -> float
    vm.define_native("asin", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Float(n.asin())),
        Value::Int(n) => Ok(Value::Float((*n as f64).asin())),
        other => Err(format!("asin: expected number, got {}", other.type_name())),
    });

    // acos(x: float) -> float
    vm.define_native("acos", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Float(n.acos())),
        Value::Int(n) => Ok(Value::Float((*n as f64).acos())),
        other => Err(format!("acos: expected number, got {}", other.type_name())),
    });

    // atan(x: float) -> float
    vm.define_native("atan", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Float(n.atan())),
        Value::Int(n) => Ok(Value::Float((*n as f64).atan())),
        other => Err(format!("atan: expected number, got {}", other.type_name())),
    });

    // atan2(y: float, x: float) -> float
    vm.define_native("atan2", 2, |args| match (&args[0], &args[1]) {
        (Value::Float(y), Value::Float(x)) => Ok(Value::Float(y.atan2(*x))),
        (Value::Int(y), Value::Int(x)) => Ok(Value::Float((*y as f64).atan2(*x as f64))),
        (Value::Float(y), Value::Int(x)) => Ok(Value::Float(y.atan2(*x as f64))),
        (Value::Int(y), Value::Float(x)) => Ok(Value::Float((*y as f64).atan2(*x))),
        _ => Err("atan2: expected two numbers".to_string()),
    });

    // sinh(x: float) -> float
    vm.define_native("sinh", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Float(n.sinh())),
        Value::Int(n) => Ok(Value::Float((*n as f64).sinh())),
        other => Err(format!("sinh: expected number, got {}", other.type_name())),
    });

    // cosh(x: float) -> float
    vm.define_native("cosh", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Float(n.cosh())),
        Value::Int(n) => Ok(Value::Float((*n as f64).cosh())),
        other => Err(format!("cosh: expected number, got {}", other.type_name())),
    });

    // tanh(x: float) -> float
    vm.define_native("tanh", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Float(n.tanh())),
        Value::Int(n) => Ok(Value::Float((*n as f64).tanh())),
        other => Err(format!("tanh: expected number, got {}", other.type_name())),
    });

    // Constants
    // pi() -> float
    vm.define_native("pi", 0, |_args| Ok(Value::Float(std::f64::consts::PI)));

    // e() -> float
    vm.define_native("e", 0, |_args| Ok(Value::Float(std::f64::consts::E)));

    // tau() -> float (2*pi)
    vm.define_native("tau", 0, |_args| Ok(Value::Float(std::f64::consts::TAU)));

    // inf() -> float
    vm.define_native("inf", 0, |_args| Ok(Value::Float(f64::INFINITY)));

    // neg_inf() -> float
    vm.define_native("neg_inf", 0, |_args| Ok(Value::Float(f64::NEG_INFINITY)));

    // nan() -> float
    vm.define_native("nan", 0, |_args| Ok(Value::Float(f64::NAN)));

    // is_nan(x: float) -> bool
    vm.define_native("is_nan", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Bool(n.is_nan())),
        Value::Int(_) => Ok(Value::Bool(false)),
        other => Err(format!(
            "is_nan: expected number, got {}",
            other.type_name()
        )),
    });

    // is_infinite(x: float) -> bool
    vm.define_native("is_infinite", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Bool(n.is_infinite())),
        Value::Int(_) => Ok(Value::Bool(false)),
        other => Err(format!(
            "is_infinite: expected number, got {}",
            other.type_name()
        )),
    });

    // is_finite(x: float) -> bool
    vm.define_native("is_finite", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Bool(n.is_finite())),
        Value::Int(_) => Ok(Value::Bool(true)),
        other => Err(format!(
            "is_finite: expected number, got {}",
            other.type_name()
        )),
    });

    // sign(x: int|float) -> int (-1, 0, or 1)
    vm.define_native("sign", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::Int(n.signum())),
        Value::Float(n) => Ok(Value::Int(if n.is_nan() { 0 } else { n.signum() as i64 })),
        other => Err(format!("sign: expected number, got {}", other.type_name())),
    });

    // to_float(x: int) -> float
    vm.define_native("to_float", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::Float(*n as f64)),
        Value::Float(n) => Ok(Value::Float(*n)),
        other => Err(format!(
            "to_float: expected number, got {}",
            other.type_name()
        )),
    });

    // to_int(x: float) -> int
    vm.define_native("to_int", 1, |args| match &args[0] {
        Value::Float(n) => Ok(Value::Int(*n as i64)),
        Value::Int(n) => Ok(Value::Int(*n)),
        other => Err(format!(
            "to_int: expected number, got {}",
            other.type_name()
        )),
    });

    // --- Unsigned integer operations ---

    // to_u8(x: int) -> int (clamps to 0-255)
    vm.define_native("to_u8", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::Int((*n as u8) as i64)),
        other => Err(format!("to_u8: expected int, got {}", other.type_name())),
    });

    // to_u16(x: int) -> int (clamps to 0-65535)
    vm.define_native("to_u16", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::Int((*n as u16) as i64)),
        other => Err(format!("to_u16: expected int, got {}", other.type_name())),
    });

    // to_u32(x: int) -> int (clamps to 0-4294967295)
    vm.define_native("to_u32", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::Int((*n as u32) as i64)),
        other => Err(format!("to_u32: expected int, got {}", other.type_name())),
    });

    // to_u64(x: int) -> int (reinterprets bits as unsigned)
    vm.define_native("to_u64", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::Int(*n)), // Same bits, different interpretation
        other => Err(format!("to_u64: expected int, got {}", other.type_name())),
    });

    // to_i8(x: int) -> int (clamps to -128 to 127)
    vm.define_native("to_i8", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::Int((*n as i8) as i64)),
        other => Err(format!("to_i8: expected int, got {}", other.type_name())),
    });

    // to_i16(x: int) -> int (clamps to -32768 to 32767)
    vm.define_native("to_i16", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::Int((*n as i16) as i64)),
        other => Err(format!("to_i16: expected int, got {}", other.type_name())),
    });

    // to_i32(x: int) -> int (clamps to i32 range)
    vm.define_native("to_i32", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::Int((*n as i32) as i64)),
        other => Err(format!("to_i32: expected int, got {}", other.type_name())),
    });

    // Bound checking functions
    // in_u8_range(x: int) -> bool
    vm.define_native("in_u8_range", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::Bool(*n >= 0 && *n <= 255)),
        other => Err(format!(
            "in_u8_range: expected int, got {}",
            other.type_name()
        )),
    });

    // in_u16_range(x: int) -> bool
    vm.define_native("in_u16_range", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::Bool(*n >= 0 && *n <= 65535)),
        other => Err(format!(
            "in_u16_range: expected int, got {}",
            other.type_name()
        )),
    });

    // in_u32_range(x: int) -> bool
    vm.define_native("in_u32_range", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::Bool(*n >= 0 && *n <= 4294967295)),
        other => Err(format!(
            "in_u32_range: expected int, got {}",
            other.type_name()
        )),
    });

    // Bitwise operations that treat value as unsigned
    // wrapping_add_u32(a: int, b: int) -> int
    vm.define_native("wrapping_add_u32", 2, |args| match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => {
            let result = (*a as u32).wrapping_add(*b as u32);
            Ok(Value::Int(result as i64))
        }
        _ => Err("wrapping_add_u32: expected two ints".to_string()),
    });

    // wrapping_sub_u32(a: int, b: int) -> int
    vm.define_native("wrapping_sub_u32", 2, |args| match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => {
            let result = (*a as u32).wrapping_sub(*b as u32);
            Ok(Value::Int(result as i64))
        }
        _ => Err("wrapping_sub_u32: expected two ints".to_string()),
    });

    // wrapping_mul_u32(a: int, b: int) -> int
    vm.define_native("wrapping_mul_u32", 2, |args| match (&args[0], &args[1]) {
        (Value::Int(a), Value::Int(b)) => {
            let result = (*a as u32).wrapping_mul(*b as u32);
            Ok(Value::Int(result as i64))
        }
        _ => Err("wrapping_mul_u32: expected two ints".to_string()),
    });

    // Bit manipulation
    // bit_count(x: int) -> int (count set bits)
    vm.define_native("bit_count", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::Int(n.count_ones() as i64)),
        other => Err(format!(
            "bit_count: expected int, got {}",
            other.type_name()
        )),
    });

    // leading_zeros(x: int) -> int
    vm.define_native("leading_zeros", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::Int(n.leading_zeros() as i64)),
        other => Err(format!(
            "leading_zeros: expected int, got {}",
            other.type_name()
        )),
    });

    // trailing_zeros(x: int) -> int
    vm.define_native("trailing_zeros", 1, |args| match &args[0] {
        Value::Int(n) => Ok(Value::Int(n.trailing_zeros() as i64)),
        other => Err(format!(
            "trailing_zeros: expected int, got {}",
            other.type_name()
        )),
    });

    // rotate_left(x: int, n: int) -> int
    vm.define_native("rotate_left", 2, |args| match (&args[0], &args[1]) {
        (Value::Int(x), Value::Int(n)) => Ok(Value::Int(x.rotate_left(*n as u32))),
        _ => Err("rotate_left: expected two ints".to_string()),
    });

    // rotate_right(x: int, n: int) -> int
    vm.define_native("rotate_right", 2, |args| match (&args[0], &args[1]) {
        (Value::Int(x), Value::Int(n)) => Ok(Value::Int(x.rotate_right(*n as u32))),
        _ => Err("rotate_right: expected two ints".to_string()),
    });
}
