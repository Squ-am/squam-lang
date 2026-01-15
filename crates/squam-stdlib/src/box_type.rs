use squam_vm::{Value, VM};
use std::cell::RefCell;
use std::rc::Rc;

pub fn register(vm: &mut VM) {
    // box_new(value: any) -> Box
    vm.define_native("box_new", 1, |args| {
        Ok(Value::Box(Rc::new(RefCell::new(args[0].clone()))))
    });

    // box_get(b: Box) -> any
    vm.define_native("box_get", 1, |args| match &args[0] {
        Value::Box(inner) => Ok(inner.borrow().clone()),
        other => Err(format!("box_get: expected Box, got {}", other.type_name())),
    });

    // box_set(b: Box, value: any) -> ()
    vm.define_native("box_set", 2, |args| match &args[0] {
        Value::Box(inner) => {
            *inner.borrow_mut() = args[1].clone();
            Ok(Value::Unit)
        }
        other => Err(format!("box_set: expected Box, got {}", other.type_name())),
    });

    // box_replace(b: Box, value: any) -> any (returns old value)
    vm.define_native("box_replace", 2, |args| match &args[0] {
        Value::Box(inner) => {
            let old = inner.borrow().clone();
            *inner.borrow_mut() = args[1].clone();
            Ok(old)
        }
        other => Err(format!("box_replace: expected Box, got {}", other.type_name())),
    });

    // box_take(b: Box) -> any (returns value, replaces with Unit)
    vm.define_native("box_take", 1, |args| match &args[0] {
        Value::Box(inner) => {
            let old = inner.borrow().clone();
            *inner.borrow_mut() = Value::Unit;
            Ok(old)
        }
        other => Err(format!("box_take: expected Box, got {}", other.type_name())),
    });

    // is_box(value: any) -> bool
    vm.define_native("is_box", 1, |args| {
        Ok(Value::Bool(matches!(&args[0], Value::Box(_))))
    });
}