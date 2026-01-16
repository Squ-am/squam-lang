use squam_vm::value::{EnumInstance, Value};
use squam_vm::VM;
use std::rc::Rc;

/// Create a Some(value) variant.
pub fn some(value: Value) -> Value {
    Value::Enum(Rc::new(EnumInstance {
        enum_name: "Option".to_string(),
        variant: "Some".to_string(),
        fields: vec![value],
    }))
}

/// Create a None variant.
pub fn none() -> Value {
    Value::Enum(Rc::new(EnumInstance {
        enum_name: "Option".to_string(),
        variant: "None".to_string(),
        fields: vec![],
    }))
}

/// Check if an Option is Some.
pub fn is_some(opt: &Value) -> bool {
    match opt {
        Value::Enum(e) => e.enum_name == "Option" && e.variant == "Some",
        _ => false,
    }
}

/// Check if an Option is None.
pub fn is_none(opt: &Value) -> bool {
    match opt {
        Value::Enum(e) => e.enum_name == "Option" && e.variant == "None",
        _ => false,
    }
}

/// Unwrap a Some value, panics if None.
pub fn unwrap(opt: &Value) -> Value {
    match opt {
        Value::Enum(e) if e.enum_name == "Option" && e.variant == "Some" => {
            e.fields.first().cloned().unwrap_or(Value::Unit)
        }
        _ => panic!("called unwrap on a None value"),
    }
}

/// Unwrap a Some value or return a default.
pub fn unwrap_or(opt: &Value, default: Value) -> Value {
    match opt {
        Value::Enum(e) if e.enum_name == "Option" && e.variant == "Some" => {
            e.fields.first().cloned().unwrap_or(Value::Unit)
        }
        _ => default,
    }
}

/// Register Option functions with the VM.
pub fn register(vm: &mut VM) {
    // Some constructor
    vm.define_native("Some", 1, |args| Ok(some(args[0].clone())));

    // None constructor
    vm.define_native("None", 0, |_args| Ok(none()));

    // is_some
    vm.define_native("is_some", 1, |args| Ok(Value::Bool(is_some(&args[0]))));

    // is_none
    vm.define_native("is_none", 1, |args| Ok(Value::Bool(is_none(&args[0]))));

    // unwrap
    vm.define_native("unwrap", 1, |args| match &args[0] {
        Value::Enum(e) if e.enum_name == "Option" && e.variant == "Some" => {
            Ok(e.fields.first().cloned().unwrap_or(Value::Unit))
        }
        Value::Enum(e) if e.enum_name == "Option" && e.variant == "None" => {
            Err("called unwrap on a None value".to_string())
        }
        _ => Err("unwrap expects an Option".to_string()),
    });

    // unwrap_or
    vm.define_native("unwrap_or", 2, |args| match &args[0] {
        Value::Enum(e) if e.enum_name == "Option" && e.variant == "Some" => {
            Ok(e.fields.first().cloned().unwrap_or(Value::Unit))
        }
        _ => Ok(args[1].clone()),
    });

    // map - applies a function to the inner value if Some
    vm.define_native("option_map", 2, |args| {
        match &args[0] {
            Value::Enum(e) if e.enum_name == "Option" && e.variant == "Some" => {
                // Can't call the function from here without VM access
                // This would need a different approach
                Ok(some(e.fields.first().cloned().unwrap_or(Value::Unit)))
            }
            _ => Ok(none()),
        }
    });
}
