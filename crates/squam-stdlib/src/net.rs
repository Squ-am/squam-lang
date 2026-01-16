use squam_vm::{Value, VM};
use squam_vm::value::StructInstance;
use std::collections::HashMap;
use std::io::{Read, Write, BufRead, BufReader};
use std::net::{TcpStream, TcpListener, ToSocketAddrs, Shutdown};
use std::rc::Rc;
use std::sync::Mutex;
use std::time::Duration;

// Global registry for socket handles
lazy_static::lazy_static! {
    static ref STREAM_REGISTRY: Mutex<HandleRegistry<TcpStream>> = Mutex::new(HandleRegistry::new());
    static ref LISTENER_REGISTRY: Mutex<HandleRegistry<TcpListener>> = Mutex::new(HandleRegistry::new());
}

struct HandleRegistry<T> {
    handles: HashMap<i64, T>,
    next_id: i64,
}

impl<T> HandleRegistry<T> {
    fn new() -> Self {
        Self {
            handles: HashMap::new(),
            next_id: 1,
        }
    }

    fn insert(&mut self, item: T) -> i64 {
        let id = self.next_id;
        self.next_id += 1;
        self.handles.insert(id, item);
        id
    }

    fn get(&self, id: i64) -> Option<&T> {
        self.handles.get(&id)
    }

    fn get_mut(&mut self, id: i64) -> Option<&mut T> {
        self.handles.get_mut(&id)
    }

    fn remove(&mut self, id: i64) -> Option<T> {
        self.handles.remove(&id)
    }
}

fn create_handle_struct(handle_type: &str, id: i64) -> Value {
    let instance = StructInstance::new_dynamic(handle_type.to_string());
    instance.fields().borrow_mut().insert("__type".to_string(), Value::String(Rc::new(handle_type.to_string())));
    instance.fields().borrow_mut().insert("__handle".to_string(), Value::Int(id));
    Value::Struct(Rc::new(instance))
}

fn get_handle_id(value: &Value, expected_type: &str) -> Result<i64, String> {
    match value {
        Value::Struct(s) => {
            match s.fields().borrow().get("__handle").cloned() {
                Some(Value::Int(id)) => Ok(id),
                _ => Err(format!("Expected {} handle", expected_type)),
            }
        }
        _ => Err(format!("Expected {} handle", expected_type)),
    }
}

pub fn register(vm: &mut VM) {
    // tcp_connect(host: string, port: int) -> handle
    vm.define_native("tcp_connect", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::String(host), Value::Int(port)) => {
                let addr = format!("{}:{}", host, port);
                match TcpStream::connect(&addr) {
                    Ok(stream) => {
                        let id = STREAM_REGISTRY.lock().unwrap().insert(stream);
                        Ok(create_handle_struct("TcpStream", id))
                    }
                    Err(e) => Err(format!("tcp_connect failed: {}", e)),
                }
            }
            _ => Err("tcp_connect: expected (string, int)".to_string()),
        }
    });

    // tcp_connect_timeout(host: string, port: int, timeout_ms: int) -> handle
    vm.define_native("tcp_connect_timeout", 3, |args| {
        match (&args[0], &args[1], &args[2]) {
            (Value::String(host), Value::Int(port), Value::Int(timeout_ms)) => {
                let addr_str = format!("{}:{}", host, port);
                let addrs: Vec<_> = match addr_str.to_socket_addrs() {
                    Ok(a) => a.collect(),
                    Err(e) => return Err(format!("tcp_connect_timeout: invalid address: {}", e)),
                };
                if addrs.is_empty() {
                    return Err("tcp_connect_timeout: could not resolve address".to_string());
                }
                let timeout = Duration::from_millis(*timeout_ms as u64);
                match TcpStream::connect_timeout(&addrs[0], timeout) {
                    Ok(stream) => {
                        let id = STREAM_REGISTRY.lock().unwrap().insert(stream);
                        Ok(create_handle_struct("TcpStream", id))
                    }
                    Err(e) => Err(format!("tcp_connect_timeout failed: {}", e)),
                }
            }
            _ => Err("tcp_connect_timeout: expected (string, int, int)".to_string()),
        }
    });

    // tcp_listen(host: string, port: int) -> listener
    vm.define_native("tcp_listen", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::String(host), Value::Int(port)) => {
                let addr = format!("{}:{}", host, port);
                match TcpListener::bind(&addr) {
                    Ok(listener) => {
                        let id = LISTENER_REGISTRY.lock().unwrap().insert(listener);
                        Ok(create_handle_struct("TcpListener", id))
                    }
                    Err(e) => Err(format!("tcp_listen failed: {}", e)),
                }
            }
            _ => Err("tcp_listen: expected (string, int)".to_string()),
        }
    });

    // tcp_accept(listener: handle) -> stream handle
    vm.define_native("tcp_accept", 1, |args| {
        let id = get_handle_id(&args[0], "TcpListener")?;
        let registry = LISTENER_REGISTRY.lock().unwrap();
        match registry.get(id) {
            Some(listener) => {
                match listener.accept() {
                    Ok((stream, _addr)) => {
                        drop(registry); // Release lock before acquiring another
                        let stream_id = STREAM_REGISTRY.lock().unwrap().insert(stream);
                        Ok(create_handle_struct("TcpStream", stream_id))
                    }
                    Err(e) => Err(format!("tcp_accept failed: {}", e)),
                }
            }
            None => Err("tcp_accept: invalid listener handle".to_string()),
        }
    });

    // tcp_read(stream: handle, size: int) -> string
    vm.define_native("tcp_read", 2, |args| {
        match &args[1] {
            Value::Int(size) => {
                let id = get_handle_id(&args[0], "TcpStream")?;
                let mut registry = STREAM_REGISTRY.lock().unwrap();
                match registry.get_mut(id) {
                    Some(stream) => {
                        let mut buf = vec![0u8; *size as usize];
                        match stream.read(&mut buf) {
                            Ok(n) => {
                                buf.truncate(n);
                                match String::from_utf8(buf) {
                                    Ok(s) => Ok(Value::String(Rc::new(s))),
                                    Err(_) => Err("tcp_read: invalid UTF-8".to_string()),
                                }
                            }
                            Err(e) => Err(format!("tcp_read failed: {}", e)),
                        }
                    }
                    None => Err("tcp_read: invalid stream handle".to_string()),
                }
            }
            _ => Err("tcp_read: expected (handle, int)".to_string()),
        }
    });

    // tcp_read_line(stream: handle) -> string
    vm.define_native("tcp_read_line", 1, |args| {
        let id = get_handle_id(&args[0], "TcpStream")?;
        let mut registry = STREAM_REGISTRY.lock().unwrap();
        match registry.get_mut(id) {
            Some(stream) => {
                let mut reader = BufReader::new(stream);
                let mut line = String::new();
                match reader.read_line(&mut line) {
                    Ok(_) => Ok(Value::String(Rc::new(line.trim_end().to_string()))),
                    Err(e) => Err(format!("tcp_read_line failed: {}", e)),
                }
            }
            None => Err("tcp_read_line: invalid stream handle".to_string()),
        }
    });

    // tcp_write(stream: handle, data: string) -> int (bytes written)
    vm.define_native("tcp_write", 2, |args| {
        match &args[1] {
            Value::String(data) => {
                let id = get_handle_id(&args[0], "TcpStream")?;
                let mut registry = STREAM_REGISTRY.lock().unwrap();
                match registry.get_mut(id) {
                    Some(stream) => {
                        match stream.write(data.as_bytes()) {
                            Ok(n) => Ok(Value::Int(n as i64)),
                            Err(e) => Err(format!("tcp_write failed: {}", e)),
                        }
                    }
                    None => Err("tcp_write: invalid stream handle".to_string()),
                }
            }
            _ => Err("tcp_write: expected (handle, string)".to_string()),
        }
    });

    // tcp_write_line(stream: handle, data: string) -> int
    vm.define_native("tcp_write_line", 2, |args| {
        match &args[1] {
            Value::String(data) => {
                let id = get_handle_id(&args[0], "TcpStream")?;
                let mut registry = STREAM_REGISTRY.lock().unwrap();
                match registry.get_mut(id) {
                    Some(stream) => {
                        let line = format!("{}\n", data);
                        match stream.write(line.as_bytes()) {
                            Ok(n) => Ok(Value::Int(n as i64)),
                            Err(e) => Err(format!("tcp_write_line failed: {}", e)),
                        }
                    }
                    None => Err("tcp_write_line: invalid stream handle".to_string()),
                }
            }
            _ => Err("tcp_write_line: expected (handle, string)".to_string()),
        }
    });

    // tcp_flush(stream: handle) -> ()
    vm.define_native("tcp_flush", 1, |args| {
        let id = get_handle_id(&args[0], "TcpStream")?;
        let mut registry = STREAM_REGISTRY.lock().unwrap();
        match registry.get_mut(id) {
            Some(stream) => {
                match stream.flush() {
                    Ok(_) => Ok(Value::Unit),
                    Err(e) => Err(format!("tcp_flush failed: {}", e)),
                }
            }
            None => Err("tcp_flush: invalid stream handle".to_string()),
        }
    });

    // tcp_close(stream: handle) -> ()
    vm.define_native("tcp_close", 1, |args| {
        let id = get_handle_id(&args[0], "TcpStream")?;
        let mut registry = STREAM_REGISTRY.lock().unwrap();
        match registry.remove(id) {
            Some(stream) => {
                let _ = stream.shutdown(Shutdown::Both);
                Ok(Value::Unit)
            }
            None => Err("tcp_close: invalid stream handle".to_string()),
        }
    });

    // tcp_close_listener(listener: handle) -> ()
    vm.define_native("tcp_close_listener", 1, |args| {
        let id = get_handle_id(&args[0], "TcpListener")?;
        let mut registry = LISTENER_REGISTRY.lock().unwrap();
        match registry.remove(id) {
            Some(_) => Ok(Value::Unit),
            None => Err("tcp_close_listener: invalid listener handle".to_string()),
        }
    });

    // tcp_set_timeout(stream: handle, read_ms: int, write_ms: int) -> ()
    vm.define_native("tcp_set_timeout", 3, |args| {
        match (&args[1], &args[2]) {
            (Value::Int(read_ms), Value::Int(write_ms)) => {
                let id = get_handle_id(&args[0], "TcpStream")?;
                let registry = STREAM_REGISTRY.lock().unwrap();
                match registry.get(id) {
                    Some(stream) => {
                        let read_timeout = if *read_ms > 0 {
                            Some(Duration::from_millis(*read_ms as u64))
                        } else {
                            None
                        };
                        let write_timeout = if *write_ms > 0 {
                            Some(Duration::from_millis(*write_ms as u64))
                        } else {
                            None
                        };
                        stream.set_read_timeout(read_timeout).ok();
                        stream.set_write_timeout(write_timeout).ok();
                        Ok(Value::Unit)
                    }
                    None => Err("tcp_set_timeout: invalid stream handle".to_string()),
                }
            }
            _ => Err("tcp_set_timeout: expected (handle, int, int)".to_string()),
        }
    });

    // tcp_peer_addr(stream: handle) -> string
    vm.define_native("tcp_peer_addr", 1, |args| {
        let id = get_handle_id(&args[0], "TcpStream")?;
        let registry = STREAM_REGISTRY.lock().unwrap();
        match registry.get(id) {
            Some(stream) => {
                match stream.peer_addr() {
                    Ok(addr) => Ok(Value::String(Rc::new(addr.to_string()))),
                    Err(e) => Err(format!("tcp_peer_addr failed: {}", e)),
                }
            }
            None => Err("tcp_peer_addr: invalid stream handle".to_string()),
        }
    });

    // tcp_local_addr(stream: handle) -> string
    vm.define_native("tcp_local_addr", 1, |args| {
        let id = get_handle_id(&args[0], "TcpStream")?;
        let registry = STREAM_REGISTRY.lock().unwrap();
        match registry.get(id) {
            Some(stream) => {
                match stream.local_addr() {
                    Ok(addr) => Ok(Value::String(Rc::new(addr.to_string()))),
                    Err(e) => Err(format!("tcp_local_addr failed: {}", e)),
                }
            }
            None => Err("tcp_local_addr: invalid stream handle".to_string()),
        }
    });
}