use base64::{engine::general_purpose, Engine as _};
use squam_vm::{Value, VM};
use std::rc::Rc;

pub fn register(vm: &mut VM) {
    // --- Base64 ---

    // base64_encode(s: string) -> string
    vm.define_native("base64_encode", 1, |args| match &args[0] {
        Value::String(s) => {
            let encoded = general_purpose::STANDARD.encode(s.as_bytes());
            Ok(Value::String(Rc::new(encoded)))
        }
        _ => Err("base64_encode: expected string".to_string()),
    });

    // base64_decode(s: string) -> string
    vm.define_native("base64_decode", 1, |args| match &args[0] {
        Value::String(s) => match general_purpose::STANDARD.decode(s.as_bytes()) {
            Ok(bytes) => match String::from_utf8(bytes) {
                Ok(decoded) => Ok(Value::String(Rc::new(decoded))),
                Err(_) => Err("base64_decode: invalid UTF-8".to_string()),
            },
            Err(e) => Err(format!("base64_decode failed: {}", e)),
        },
        _ => Err("base64_decode: expected string".to_string()),
    });

    // base64_encode_url(s: string) -> string (URL-safe base64)
    vm.define_native("base64_encode_url", 1, |args| match &args[0] {
        Value::String(s) => {
            let encoded = general_purpose::URL_SAFE.encode(s.as_bytes());
            Ok(Value::String(Rc::new(encoded)))
        }
        _ => Err("base64_encode_url: expected string".to_string()),
    });

    // base64_decode_url(s: string) -> string (URL-safe base64)
    vm.define_native("base64_decode_url", 1, |args| match &args[0] {
        Value::String(s) => match general_purpose::URL_SAFE.decode(s.as_bytes()) {
            Ok(bytes) => match String::from_utf8(bytes) {
                Ok(decoded) => Ok(Value::String(Rc::new(decoded))),
                Err(_) => Err("base64_decode_url: invalid UTF-8".to_string()),
            },
            Err(e) => Err(format!("base64_decode_url failed: {}", e)),
        },
        _ => Err("base64_decode_url: expected string".to_string()),
    });

    // --- Hashing (simple implementations) ---

    // hash_djb2(s: string) -> int (fast non-cryptographic hash)
    vm.define_native("hash_djb2", 1, |args| match &args[0] {
        Value::String(s) => {
            let mut hash: u64 = 5381;
            for byte in s.bytes() {
                hash = hash.wrapping_mul(33).wrapping_add(byte as u64);
            }
            Ok(Value::Int(hash as i64))
        }
        _ => Err("hash_djb2: expected string".to_string()),
    });

    // hash_fnv1a(s: string) -> int (FNV-1a hash)
    vm.define_native("hash_fnv1a", 1, |args| match &args[0] {
        Value::String(s) => {
            let mut hash: u64 = 0xcbf29ce484222325;
            for byte in s.bytes() {
                hash ^= byte as u64;
                hash = hash.wrapping_mul(0x100000001b3);
            }
            Ok(Value::Int(hash as i64))
        }
        _ => Err("hash_fnv1a: expected string".to_string()),
    });

    // hex_encode(s: string) -> string
    vm.define_native("hex_encode", 1, |args| match &args[0] {
        Value::String(s) => {
            let hex: String = s.bytes().map(|b| format!("{:02x}", b)).collect();
            Ok(Value::String(Rc::new(hex)))
        }
        _ => Err("hex_encode: expected string".to_string()),
    });

    // hex_decode(s: string) -> string
    vm.define_native("hex_decode", 1, |args| match &args[0] {
        Value::String(s) => {
            let s = s.as_str();
            if s.len() % 2 != 0 {
                return Err("hex_decode: invalid hex string (odd length)".to_string());
            }
            let bytes: Result<Vec<u8>, _> = (0..s.len())
                .step_by(2)
                .map(|i| u8::from_str_radix(&s[i..i + 2], 16))
                .collect();
            match bytes {
                Ok(bytes) => match String::from_utf8(bytes) {
                    Ok(decoded) => Ok(Value::String(Rc::new(decoded))),
                    Err(_) => Err("hex_decode: invalid UTF-8".to_string()),
                },
                Err(_) => Err("hex_decode: invalid hex string".to_string()),
            }
        }
        _ => Err("hex_decode: expected string".to_string()),
    });
}
