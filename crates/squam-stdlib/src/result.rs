use squam_vm::value::{EnumInstance, Value};
use squam_vm::VM;
use std::rc::Rc;

/// Create an Ok(value) variant.
pub fn ok(value: Value) -> Value {
    Value::Enum(Rc::new(EnumInstance {
        enum_name: "Result".to_string(),
        variant: "Ok".to_string(),
        fields: vec![value],
    }))
}

/// Create an Err(error) variant.
pub fn err(error: Value) -> Value {
    Value::Enum(Rc::new(EnumInstance {
        enum_name: "Result".to_string(),
        variant: "Err".to_string(),
        fields: vec![error],
    }))
}

/// Check if a Result is Ok.
pub fn is_ok(result: &Value) -> bool {
    match result {
        Value::Enum(e) => e.enum_name == "Result" && e.variant == "Ok",
        _ => false,
    }
}

/// Check if a Result is Err.
pub fn is_err(result: &Value) -> bool {
    match result {
        Value::Enum(e) => e.enum_name == "Result" && e.variant == "Err",
        _ => false,
    }
}

/// Unwrap an Ok value, panics if Err.
pub fn unwrap_result(result: &Value) -> Value {
    match result {
        Value::Enum(e) if e.enum_name == "Result" && e.variant == "Ok" => {
            e.fields.first().cloned().unwrap_or(Value::Unit)
        }
        Value::Enum(e) if e.enum_name == "Result" && e.variant == "Err" => {
            panic!("called unwrap on an Err value: {:?}", e.fields.first())
        }
        _ => panic!("called unwrap on a non-Result value"),
    }
}

/// Unwrap an Err value, panics if Ok.
pub fn unwrap_err(result: &Value) -> Value {
    match result {
        Value::Enum(e) if e.enum_name == "Result" && e.variant == "Err" => {
            e.fields.first().cloned().unwrap_or(Value::Unit)
        }
        Value::Enum(e) if e.enum_name == "Result" && e.variant == "Ok" => {
            panic!("called unwrap_err on an Ok value: {:?}", e.fields.first())
        }
        _ => panic!("called unwrap_err on a non-Result value"),
    }
}

/// Register Result functions with the VM.
pub fn register(vm: &mut VM) {
    // Ok constructor
    vm.define_native("Ok", 1, |args| {
        Ok(ok(args[0].clone()))
    });

    // Err constructor
    vm.define_native("Err", 1, |args| {
        Ok(err(args[0].clone()))
    });

    // is_ok
    vm.define_native("is_ok", 1, |args| {
        Ok(Value::Bool(is_ok(&args[0])))
    });

    // is_err
    vm.define_native("is_err", 1, |args| {
        Ok(Value::Bool(is_err(&args[0])))
    });

    // unwrap_result (different name to avoid conflict with Option.unwrap)
    vm.define_native("unwrap_ok", 1, |args| {
        match &args[0] {
            Value::Enum(e) if e.enum_name == "Result" && e.variant == "Ok" => {
                Ok(e.fields.first().cloned().unwrap_or(Value::Unit))
            }
            Value::Enum(e) if e.enum_name == "Result" && e.variant == "Err" => {
                let err_val = e.fields.first().cloned().unwrap_or(Value::Unit);
                Err(format!("called unwrap_ok on an Err value: {}", err_val))
            }
            _ => Err("unwrap_ok expects a Result".to_string()),
        }
    });

    // unwrap_err
    vm.define_native("unwrap_err", 1, |args| {
        match &args[0] {
            Value::Enum(e) if e.enum_name == "Result" && e.variant == "Err" => {
                Ok(e.fields.first().cloned().unwrap_or(Value::Unit))
            }
            Value::Enum(e) if e.enum_name == "Result" && e.variant == "Ok" => {
                let ok_val = e.fields.first().cloned().unwrap_or(Value::Unit);
                Err(format!("called unwrap_err on an Ok value: {}", ok_val))
            }
            _ => Err("unwrap_err expects a Result".to_string()),
        }
    });

    // unwrap_or for Result
    vm.define_native("result_unwrap_or", 2, |args| {
        match &args[0] {
            Value::Enum(e) if e.enum_name == "Result" && e.variant == "Ok" => {
                Ok(e.fields.first().cloned().unwrap_or(Value::Unit))
            }
            _ => Ok(args[1].clone()),
        }
    });

    // ok() - convert Result<T, E> to Option<T>
    vm.define_native("result_ok", 1, |args| {
        match &args[0] {
            Value::Enum(e) if e.enum_name == "Result" && e.variant == "Ok" => {
                let val = e.fields.first().cloned().unwrap_or(Value::Unit);
                Ok(super::option::some(val))
            }
            _ => Ok(super::option::none()),
        }
    });

    // err() - convert Result<T, E> to Option<E>
    vm.define_native("result_err", 1, |args| {
        match &args[0] {
            Value::Enum(e) if e.enum_name == "Result" && e.variant == "Err" => {
                let val = e.fields.first().cloned().unwrap_or(Value::Unit);
                Ok(super::option::some(val))
            }
            _ => Ok(super::option::none()),
        }
    });
}