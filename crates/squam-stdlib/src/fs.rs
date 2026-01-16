use squam_vm::{Value, VM};
use std::cell::RefCell;
use std::fs;
use std::path::Path;
use std::rc::Rc;

pub fn register(vm: &mut VM) {
    // mkdir(path: string) -> bool (creates directory, returns success)
    vm.define_native("mkdir", 1, |args| match &args[0] {
        Value::String(path) => match fs::create_dir(path.as_str()) {
            Ok(_) => Ok(Value::Bool(true)),
            Err(_) => Ok(Value::Bool(false)),
        },
        _ => Err("mkdir: expected string".to_string()),
    });

    // mkdir_all(path: string) -> bool (creates directory and parents)
    vm.define_native("mkdir_all", 1, |args| match &args[0] {
        Value::String(path) => match fs::create_dir_all(path.as_str()) {
            Ok(_) => Ok(Value::Bool(true)),
            Err(_) => Ok(Value::Bool(false)),
        },
        _ => Err("mkdir_all: expected string".to_string()),
    });

    // rmdir(path: string) -> bool (removes empty directory)
    vm.define_native("rmdir", 1, |args| match &args[0] {
        Value::String(path) => match fs::remove_dir(path.as_str()) {
            Ok(_) => Ok(Value::Bool(true)),
            Err(_) => Ok(Value::Bool(false)),
        },
        _ => Err("rmdir: expected string".to_string()),
    });

    // rmdir_all(path: string) -> bool (removes directory and contents)
    vm.define_native("rmdir_all", 1, |args| match &args[0] {
        Value::String(path) => match fs::remove_dir_all(path.as_str()) {
            Ok(_) => Ok(Value::Bool(true)),
            Err(_) => Ok(Value::Bool(false)),
        },
        _ => Err("rmdir_all: expected string".to_string()),
    });

    // remove_file(path: string) -> bool
    vm.define_native("remove_file", 1, |args| match &args[0] {
        Value::String(path) => match fs::remove_file(path.as_str()) {
            Ok(_) => Ok(Value::Bool(true)),
            Err(_) => Ok(Value::Bool(false)),
        },
        _ => Err("remove_file: expected string".to_string()),
    });

    // rename(from: string, to: string) -> bool
    vm.define_native("rename", 2, |args| match (&args[0], &args[1]) {
        (Value::String(from), Value::String(to)) => match fs::rename(from.as_str(), to.as_str()) {
            Ok(_) => Ok(Value::Bool(true)),
            Err(_) => Ok(Value::Bool(false)),
        },
        _ => Err("rename: expected (string, string)".to_string()),
    });

    // copy_file(from: string, to: string) -> bool
    vm.define_native("copy_file", 2, |args| match (&args[0], &args[1]) {
        (Value::String(from), Value::String(to)) => match fs::copy(from.as_str(), to.as_str()) {
            Ok(_) => Ok(Value::Bool(true)),
            Err(_) => Ok(Value::Bool(false)),
        },
        _ => Err("copy_file: expected (string, string)".to_string()),
    });

    // read_dir(path: string) -> [string] (list directory contents)
    vm.define_native("read_dir", 1, |args| match &args[0] {
        Value::String(path) => match fs::read_dir(path.as_str()) {
            Ok(entries) => {
                let files: Vec<Value> = entries
                    .filter_map(|e| e.ok())
                    .map(|e| Value::String(Rc::new(e.file_name().to_string_lossy().to_string())))
                    .collect();
                Ok(Value::Array(Rc::new(RefCell::new(files))))
            }
            Err(e) => Err(format!("read_dir failed: {}", e)),
        },
        _ => Err("read_dir: expected string".to_string()),
    });

    // is_file(path: string) -> bool
    vm.define_native("is_file", 1, |args| match &args[0] {
        Value::String(path) => Ok(Value::Bool(Path::new(path.as_str()).is_file())),
        _ => Err("is_file: expected string".to_string()),
    });

    // is_dir(path: string) -> bool
    vm.define_native("is_dir", 1, |args| match &args[0] {
        Value::String(path) => Ok(Value::Bool(Path::new(path.as_str()).is_dir())),
        _ => Err("is_dir: expected string".to_string()),
    });

    // file_size(path: string) -> int (returns -1 on error)
    vm.define_native("file_size", 1, |args| match &args[0] {
        Value::String(path) => match fs::metadata(path.as_str()) {
            Ok(meta) => Ok(Value::Int(meta.len() as i64)),
            Err(_) => Ok(Value::Int(-1)),
        },
        _ => Err("file_size: expected string".to_string()),
    });

    // path_join(parts: [string]) -> string
    vm.define_native("path_join", 1, |args| match &args[0] {
        Value::Array(arr) => {
            let arr = arr.borrow();
            let mut path = std::path::PathBuf::new();
            for part in arr.iter() {
                if let Value::String(s) = part {
                    path.push(s.as_str());
                }
            }
            Ok(Value::String(Rc::new(path.to_string_lossy().to_string())))
        }
        _ => Err("path_join: expected array of strings".to_string()),
    });

    // path_parent(path: string) -> string
    vm.define_native("path_parent", 1, |args| match &args[0] {
        Value::String(path) => {
            let p = Path::new(path.as_str());
            match p.parent() {
                Some(parent) => Ok(Value::String(Rc::new(parent.to_string_lossy().to_string()))),
                None => Ok(Value::String(Rc::new(String::new()))),
            }
        }
        _ => Err("path_parent: expected string".to_string()),
    });

    // path_filename(path: string) -> string
    vm.define_native("path_filename", 1, |args| match &args[0] {
        Value::String(path) => {
            let p = Path::new(path.as_str());
            match p.file_name() {
                Some(name) => Ok(Value::String(Rc::new(name.to_string_lossy().to_string()))),
                None => Ok(Value::String(Rc::new(String::new()))),
            }
        }
        _ => Err("path_filename: expected string".to_string()),
    });

    // path_extension(path: string) -> string
    vm.define_native("path_extension", 1, |args| match &args[0] {
        Value::String(path) => {
            let p = Path::new(path.as_str());
            match p.extension() {
                Some(ext) => Ok(Value::String(Rc::new(ext.to_string_lossy().to_string()))),
                None => Ok(Value::String(Rc::new(String::new()))),
            }
        }
        _ => Err("path_extension: expected string".to_string()),
    });

    // cwd() -> string (current working directory)
    vm.define_native("cwd", 0, |_args| match std::env::current_dir() {
        Ok(path) => Ok(Value::String(Rc::new(path.to_string_lossy().to_string()))),
        Err(e) => Err(format!("cwd failed: {}", e)),
    });

    // chdir(path: string) -> bool (change working directory)
    vm.define_native("chdir", 1, |args| match &args[0] {
        Value::String(path) => match std::env::set_current_dir(path.as_str()) {
            Ok(_) => Ok(Value::Bool(true)),
            Err(_) => Ok(Value::Bool(false)),
        },
        _ => Err("chdir: expected string".to_string()),
    });
}
