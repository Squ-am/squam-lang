use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use squam_compiler::{Chunk, Constant, FunctionProto, OpCode};

use crate::gc::{GcConfig, GcHeap, GcStats, Trace};
use crate::inline_cache::{CallSiteId, InlineCacheManager, TypeId};
use crate::value::{ArrayIterator, Closure, EnumInstance, NativeFunction, RangeIterator, SquamIterator, StructInstance, Upvalue, Value, VMNativeFnId};

/// VM execution errors.
#[derive(Debug, Clone, thiserror::Error)]
pub enum RuntimeError {
    #[error("stack underflow")]
    StackUnderflow,
    #[error("type error: expected {expected}, got {got}")]
    TypeError { expected: &'static str, got: String },
    #[error("undefined variable: {0}")]
    UndefinedVariable(String),
    #[error("invalid opcode: {0}")]
    InvalidOpcode(u8),
    #[error("arity mismatch: expected {expected} arguments, got {got}")]
    ArityMismatch { expected: u8, got: u8 },
    #[error("index out of bounds: {index} (length {length})")]
    IndexOutOfBounds { index: i64, length: usize },
    #[error("not callable: {0}")]
    NotCallable(String),
    #[error("division by zero")]
    DivisionByZero,
    #[error("assertion failed")]
    AssertionFailed,
    #[error("panic: {0}")]
    Panic(String),
    #[error("{0}")]
    Custom(String),
    #[error("undefined field '{field}' on type '{type_name}'")]
    UndefinedField { field: String, type_name: String },
    #[error("undefined method '{method}' on type '{type_name}'")]
    UndefinedMethod { method: String, type_name: String },
    #[error("internal error: {0}")]
    InternalError(String),
}

/// A call frame for function invocation.
#[derive(Clone)]
struct CallFrame {
    /// The closure being executed
    closure: Rc<Closure>,
    /// Instruction pointer within the chunk
    ip: usize,
    /// Base of the stack for this frame
    stack_base: usize,
}

impl CallFrame {
    fn chunk(&self) -> &Chunk {
        &self.closure.proto.chunk
    }
}

/// Type for VM-native function handlers.
pub type VMNativeFn = fn(&mut VM, &[Value]) -> Result<Value, RuntimeError>;

/// The Squam virtual machine.
pub struct VM {
    /// Value stack
    stack: Vec<Value>,
    /// Call frames
    frames: Vec<CallFrame>,
    /// Global variables
    pub globals: HashMap<String, Value>,
    /// Methods table: type_name -> method_name -> closure
    methods: HashMap<String, HashMap<String, Rc<Closure>>>,
    /// Open upvalues (for closures)
    open_upvalues: Vec<Rc<RefCell<Upvalue>>>,
    /// Maximum stack size (prevent runaway recursion)
    max_stack_size: usize,
    /// Maximum call depth
    max_call_depth: usize,
    /// Output buffer (for print statements)
    output: Vec<String>,
    /// Garbage collector heap
    gc_heap: GcHeap,
    /// Enable automatic GC
    gc_enabled: bool,
    /// VM-native function registry (functions that need VM access)
    vm_natives: Vec<(VMNativeFn, u8)>, // (function, arity)
    /// Inline cache for field/method/global access
    inline_cache: InlineCacheManager,
}

impl VM {
    /// Create a new VM.
    pub fn new() -> Self {
        Self::with_gc_config(GcConfig::default())
    }

    /// Create a new VM with custom GC configuration.
    pub fn with_gc_config(gc_config: GcConfig) -> Self {
        let mut vm = Self {
            stack: Vec::with_capacity(256),
            frames: Vec::with_capacity(64),
            globals: HashMap::new(),
            methods: HashMap::new(),
            open_upvalues: Vec::new(),
            max_stack_size: 65536,
            max_call_depth: 1000,
            output: Vec::new(),
            gc_heap: GcHeap::with_config(gc_config),
            gc_enabled: true,
            vm_natives: Vec::new(),
            inline_cache: InlineCacheManager::new(),
        };
        vm.register_natives();
        vm
    }

    /// Enable or disable automatic garbage collection.
    pub fn set_gc_enabled(&mut self, enabled: bool) {
        self.gc_enabled = enabled;
    }

    /// Manually trigger garbage collection.
    pub fn collect_garbage(&mut self) {
        // Build roots from stack and globals
        // Note: Open upvalues point to stack slots which are already roots,
        // and closed upvalues are traced through closures which are on the stack or in globals.
        let mut roots: Vec<&dyn Trace> = Vec::new();

        // Stack values are roots
        for value in &self.stack {
            roots.push(value as &dyn Trace);
        }

        // Global values are roots
        for value in self.globals.values() {
            roots.push(value as &dyn Trace);
        }

        self.gc_heap.collect(&roots);
    }

    /// Get GC statistics.
    pub fn gc_stats(&self) -> GcStats {
        self.gc_heap.stats()
    }

    /// Enable or disable inline caching.
    pub fn set_inline_cache_enabled(&mut self, enabled: bool) {
        self.inline_cache.set_enabled(enabled);
    }

    /// Get inline cache statistics report.
    pub fn inline_cache_report(&self) -> String {
        self.inline_cache.report()
    }

    /// Get inline cache hit ratio.
    pub fn inline_cache_hit_ratio(&self) -> f64 {
        self.inline_cache.hit_ratio()
    }

    /// Check if GC should run and run it if needed.
    #[allow(dead_code)]
    fn maybe_collect(&mut self) {
        if self.gc_enabled && self.gc_heap.should_collect() {
            self.collect_garbage();
        }
    }

    /// Register native functions.
    fn register_natives(&mut self) {
        self.define_native("print", 1, |args| {
            println!("{}", args[0]);
            Ok(Value::Unit)
        });

        self.define_native("println", 1, |args| {
            println!("{}", args[0]);
            Ok(Value::Unit)
        });

        self.define_native("len", 1, |args| match &args[0] {
            Value::String(s) => Ok(Value::Int(s.len() as i64)),
            Value::Array(arr) => Ok(Value::Int(arr.borrow().len() as i64)),
            Value::Tuple(t) => Ok(Value::Int(t.len() as i64)),
            other => Err(format!("len() not supported for {}", other.type_name())),
        });

        self.define_native("type_of", 1, |args| {
            Ok(Value::String(Rc::new(args[0].type_name().to_string())))
        });

        self.define_native("to_string", 1, |args| {
            Ok(Value::String(Rc::new(format!("{}", args[0]))))
        });

        self.define_native("push", 2, |args| {
            if let Value::Array(arr) = &args[0] {
                arr.borrow_mut().push(args[1].clone());
                Ok(Value::Unit)
            } else {
                Err(format!("push() requires array, got {}", args[0].type_name()))
            }
        });

        self.define_native("pop", 1, |args| {
            if let Value::Array(arr) = &args[0] {
                arr.borrow_mut()
                    .pop()
                    .ok_or_else(|| "pop() on empty array".to_string())
            } else {
                Err(format!("pop() requires array, got {}", args[0].type_name()))
            }
        });
    }

    /// Define a native function from a function pointer.
    pub fn define_native(
        &mut self,
        name: &str,
        arity: u8,
        func: fn(&mut [Value]) -> Result<Value, String>,
    ) {
        self.globals.insert(
            name.to_string(),
            Value::Native(NativeFunction {
                name: name.to_string(),
                arity,
                func: Rc::new(func),
            }),
        );
    }

    /// Define a native function from a closure.
    pub fn define_native_closure<F>(&mut self, name: &str, arity: u8, func: F)
    where
        F: Fn(&mut [Value]) -> Result<Value, String> + 'static,
    {
        self.globals.insert(
            name.to_string(),
            Value::Native(NativeFunction {
                name: name.to_string(),
                arity,
                func: Rc::new(func),
            }),
        );
    }

    /// Define a global variable.
    pub fn define_global(&mut self, name: &str, value: Value) {
        self.globals.insert(name.to_string(), value);
    }

    /// Define a VM-native function that has access to the VM for calling closures.
    pub fn define_vm_native(&mut self, name: &str, arity: u8, func: VMNativeFn) {
        let id = self.vm_natives.len() as VMNativeFnId;
        self.vm_natives.push((func, arity));
        self.globals.insert(name.to_string(), Value::VMNative(id));
    }

    /// Call a closure with the given arguments. Used by VM-native functions.
    /// This runs the closure to completion and returns its result without
    /// continuing execution of outer frames.
    pub fn call_closure_value(&mut self, closure: &Rc<Closure>, args: Vec<Value>) -> Result<Value, RuntimeError> {
        // Push arguments onto stack
        for arg in &args {
            self.push(arg.clone())?;
        }

        // Remember how many frames we have before the call
        let frame_count_before = self.frames.len();

        self.call_closure(closure.clone(), args.len() as u8)?;

        // Execute until this new frame completes (returns)
        self.execute_with_target_frame_count(Some(frame_count_before))
    }

    /// Run a compiled module.
    pub fn run(&mut self, proto: &FunctionProto) -> Result<Value, RuntimeError> {
        // Create a closure from the main function
        let closure = Rc::new(Closure {
            proto: Rc::new(proto.clone()),
            upvalues: Vec::new(),
        });

        // Push initial frame
        self.frames.push(CallFrame {
            closure,
            ip: 0,
            stack_base: 0,
        });

        // Run the VM
        self.execute()
    }

    /// Call a value as a function.
    pub fn call(&mut self, callee: Value, args: Vec<Value>) -> Result<Value, RuntimeError> {
        // Push arguments onto stack
        for arg in &args {
            self.push(arg.clone())?;
        }

        match callee {
            Value::Closure(closure) => {
                self.call_closure(closure, args.len() as u8)?;
                self.execute()
            }
            Value::Native(native) => {
                let start = self.stack.len() - args.len();
                let result = (native.func)(&mut self.stack[start..])
                    .map_err(RuntimeError::Custom)?;
                self.stack.truncate(start);
                Ok(result)
            }
            _ => Err(RuntimeError::NotCallable(callee.type_name().to_string())),
        }
    }

    /// Get the output buffer.
    pub fn output(&self) -> &[String] {
        &self.output
    }

    /// Clear the output buffer.
    pub fn clear_output(&mut self) {
        self.output.clear();
    }

    // ---
    // Stack operations
    // ---

    fn push(&mut self, value: Value) -> Result<(), RuntimeError> {
        if self.stack.len() >= self.max_stack_size {
            return Err(RuntimeError::Custom("stack overflow".to_string()));
        }
        self.stack.push(value);
        Ok(())
    }

    fn pop(&mut self) -> Result<Value, RuntimeError> {
        self.stack.pop().ok_or(RuntimeError::StackUnderflow)
    }

    fn peek(&self, distance: usize) -> Result<&Value, RuntimeError> {
        let idx = self.stack.len().checked_sub(1 + distance);
        idx.and_then(|i| self.stack.get(i))
            .ok_or(RuntimeError::StackUnderflow)
    }

    // ---
    // Frame operations
    // ---

    fn current_frame(&self) -> &CallFrame {
        self.frames.last().expect("No call frame")
    }

    fn current_frame_mut(&mut self) -> &mut CallFrame {
        self.frames.last_mut().expect("No call frame")
    }

    fn read_byte(&mut self) -> u8 {
        let frame = self.current_frame_mut();
        let byte = frame.chunk().code[frame.ip];
        frame.ip += 1;
        byte
    }

    fn read_u16(&mut self) -> u16 {
        let frame = self.current_frame_mut();
        let chunk = &frame.closure.proto.chunk;
        // Little-endian: low byte first, high byte second
        let value = (chunk.code[frame.ip] as u16) | ((chunk.code[frame.ip + 1] as u16) << 8);
        frame.ip += 2;
        value
    }

    fn read_constant(&mut self, index: usize) -> &Constant {
        &self.current_frame().closure.proto.chunk.constants[index]
    }

    // ---
    // Call operations
    // ---

    fn call_closure(&mut self, closure: Rc<Closure>, arg_count: u8) -> Result<(), RuntimeError> {
        let min_arity = closure.proto.min_arity;
        let max_arity = closure.proto.arity;

        if arg_count < min_arity || arg_count > max_arity {
            return Err(RuntimeError::ArityMismatch {
                expected: if min_arity == max_arity { max_arity } else { min_arity },
                got: arg_count,
            });
        }

        // Fill in default values for missing arguments
        let missing_args = max_arity - arg_count;
        if missing_args > 0 {
            // defaults[0] is for first param with default
            // We need the last `missing_args` defaults
            let defaults = &closure.proto.defaults;
            let defaults_to_use = defaults.len().saturating_sub(missing_args as usize);
            for i in defaults_to_use..defaults.len() {
                let const_idx = defaults[i] as usize;
                let value = self.constant_to_value_from_proto(&closure.proto, const_idx)?;
                self.push(value)?;
            }
        }

        if self.frames.len() >= self.max_call_depth {
            return Err(RuntimeError::Custom("call stack overflow".to_string()));
        }

        let stack_base = self.stack.len() - max_arity as usize;
        self.frames.push(CallFrame {
            closure,
            ip: 0,
            stack_base,
        });

        Ok(())
    }

    fn constant_to_value_from_proto(&self, proto: &FunctionProto, idx: usize) -> Result<Value, RuntimeError> {
        let constant = &proto.chunk.constants[idx];
        match constant {
            Constant::Int(n) => Ok(Value::Int(*n)),
            Constant::Float(n) => Ok(Value::Float(*n)),
            Constant::String(s) => Ok(Value::String(Rc::new(s.clone()))),
            _ => Err(RuntimeError::Custom("Invalid default value type".to_string())),
        }
    }

    // ---
    // Main execution loop
    // ---

    fn execute(&mut self) -> Result<Value, RuntimeError> {
        self.execute_with_target_frame_count(None)
    }

    fn execute_with_target_frame_count(&mut self, target_frame_count: Option<usize>) -> Result<Value, RuntimeError> {
        loop {
            let opcode_byte = self.read_byte();
            let opcode = OpCode::try_from(opcode_byte)
                .map_err(|_| RuntimeError::InvalidOpcode(opcode_byte))?;

            match opcode {
                // CONSTANTS
                OpCode::Const => {
                    let idx = self.read_u16() as usize;
                    let value = self.constant_to_value(idx)?;
                    self.push(value)?;
                }
                OpCode::ConstSmall => {
                    let idx = self.read_byte() as usize;
                    let value = self.constant_to_value(idx)?;
                    self.push(value)?;
                }
                OpCode::Unit => {
                    self.push(Value::Unit)?;
                }
                OpCode::True => {
                    self.push(Value::Bool(true))?;
                }
                OpCode::False => {
                    self.push(Value::Bool(false))?;
                }

                // STACK
                OpCode::Dup => {
                    let value = self.peek(0)?.clone();
                    self.push(value)?;
                }
                OpCode::Pop => {
                    self.pop()?;
                }
                OpCode::PopN => {
                    let n = self.read_byte() as usize;
                    for _ in 0..n {
                        self.pop()?;
                    }
                }
                OpCode::Swap => {
                    let len = self.stack.len();
                    if len >= 2 {
                        self.stack.swap(len - 1, len - 2);
                    }
                }

                // LOCALS
                OpCode::LoadLocal => {
                    let slot = self.read_u16() as usize;
                    let base = self.current_frame().stack_base;
                    let value = self.stack[base + slot].clone();
                    self.push(value)?;
                }
                OpCode::LoadLocalSmall => {
                    let slot = self.read_byte() as usize;
                    let base = self.current_frame().stack_base;
                    let value = self.stack[base + slot].clone();
                    self.push(value)?;
                }
                OpCode::StoreLocal => {
                    let slot = self.read_u16() as usize;
                    let value = self.peek(0)?.clone();
                    let base = self.current_frame().stack_base;
                    self.stack[base + slot] = value;
                }
                OpCode::StoreLocalSmall => {
                    let slot = self.read_byte() as usize;
                    let value = self.peek(0)?.clone();
                    let base = self.current_frame().stack_base;
                    self.stack[base + slot] = value;
                }

                // GLOBALS
                OpCode::LoadGlobal => {
                    let idx = self.read_u16() as usize;
                    let name = match self.read_constant(idx) {
                        Constant::String(s) => s.clone(),
                        _ => return Err(RuntimeError::Custom("Invalid global name".to_string())),
                    };
                    let value = self
                        .globals
                        .get(&name)
                        .cloned()
                        .ok_or_else(|| RuntimeError::UndefinedVariable(name))?;
                    self.push(value)?;
                }
                OpCode::StoreGlobal => {
                    let idx = self.read_u16() as usize;
                    let name = match self.read_constant(idx) {
                        Constant::String(s) => s.clone(),
                        _ => return Err(RuntimeError::Custom("Invalid global name".to_string())),
                    };
                    let value = self.pop()?;
                    self.globals.insert(name, value);
                }

                // UPVALUES
                OpCode::LoadUpvalue => {
                    let idx = self.read_byte() as usize;
                    let upvalue = self.current_frame().closure.upvalues[idx].clone();
                    let value = match &*upvalue.borrow() {
                        Upvalue::Open(slot) => self.stack[*slot].clone(),
                        Upvalue::Closed(v) => v.clone(),
                    };
                    self.push(value)?;
                }
                OpCode::StoreUpvalue => {
                    let idx = self.read_byte() as usize;
                    let value = self.peek(0)?.clone();
                    let upvalue = self.current_frame().closure.upvalues[idx].clone();
                    match &mut *upvalue.borrow_mut() {
                        Upvalue::Open(slot) => self.stack[*slot] = value,
                        Upvalue::Closed(v) => *v = value,
                    };
                }
                OpCode::CloseUpvalue => {
                    let stack_top = self.stack.len() - 1;
                    self.close_upvalues(stack_top);
                    self.pop()?;
                }

                // ARITHMETIC
                OpCode::Add => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = match (&a, &b) {
                        (Value::Int(a), Value::Int(b)) => Value::Int(a + b),
                        (Value::Float(a), Value::Float(b)) => Value::Float(a + b),
                        (Value::Int(a), Value::Float(b)) => Value::Float(*a as f64 + b),
                        (Value::Float(a), Value::Int(b)) => Value::Float(a + *b as f64),
                        (Value::String(a), Value::String(b)) => {
                            Value::String(Rc::new(format!("{}{}", a, b)))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "numbers or strings",
                                got: format!("{} and {}", a.type_name(), b.type_name()),
                            })
                        }
                    };
                    self.push(result)?;
                }
                OpCode::Sub => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = self.binary_num_op(&a, &b, |a, b| a - b, |a, b| a - b)?;
                    self.push(result)?;
                }
                OpCode::Mul => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = self.binary_num_op(&a, &b, |a, b| a * b, |a, b| a * b)?;
                    self.push(result)?;
                }
                OpCode::Div => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    // Check for division by zero
                    match &b {
                        Value::Int(0) => return Err(RuntimeError::DivisionByZero),
                        Value::Float(f) if *f == 0.0 => return Err(RuntimeError::DivisionByZero),
                        _ => {}
                    }
                    let result = self.binary_num_op(&a, &b, |a, b| a / b, |a, b| a / b)?;
                    self.push(result)?;
                }
                OpCode::Rem => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    match &b {
                        Value::Int(0) => return Err(RuntimeError::DivisionByZero),
                        _ => {}
                    }
                    let result = self.binary_num_op(&a, &b, |a, b| a % b, |a, b| a % b)?;
                    self.push(result)?;
                }
                OpCode::Neg => {
                    let a = self.pop()?;
                    let result = match a {
                        Value::Int(n) => Value::Int(-n),
                        Value::Float(n) => Value::Float(-n),
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "number",
                                got: a.type_name().to_string(),
                            })
                        }
                    };
                    self.push(result)?;
                }
                OpCode::IAdd => {
                    // Integer add (optimized)
                    let b = self.pop()?;
                    let a = self.pop()?;
                    match (&a, &b) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Int(a + b))?,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "int",
                                got: format!("{} and {}", a.type_name(), b.type_name()),
                            })
                        }
                    }
                }
                OpCode::ISub => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    match (&a, &b) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Int(a - b))?,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "int",
                                got: format!("{} and {}", a.type_name(), b.type_name()),
                            })
                        }
                    }
                }
                OpCode::FAdd => {
                    // Float add (optimized)
                    let b = self.pop()?;
                    let a = self.pop()?;
                    match (&a, &b) {
                        (Value::Float(a), Value::Float(b)) => self.push(Value::Float(a + b))?,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "float",
                                got: format!("{} and {}", a.type_name(), b.type_name()),
                            })
                        }
                    }
                }
                OpCode::FSub => {
                    // Float subtract (optimized)
                    let b = self.pop()?;
                    let a = self.pop()?;
                    match (&a, &b) {
                        (Value::Float(a), Value::Float(b)) => self.push(Value::Float(a - b))?,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "float",
                                got: format!("{} and {}", a.type_name(), b.type_name()),
                            })
                        }
                    }
                }
                OpCode::FMul => {
                    // Float multiply (optimized)
                    let b = self.pop()?;
                    let a = self.pop()?;
                    match (&a, &b) {
                        (Value::Float(a), Value::Float(b)) => self.push(Value::Float(a * b))?,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "float",
                                got: format!("{} and {}", a.type_name(), b.type_name()),
                            })
                        }
                    }
                }
                OpCode::FDiv => {
                    // Float divide (optimized)
                    let b = self.pop()?;
                    let a = self.pop()?;
                    match (&a, &b) {
                        (Value::Float(a), Value::Float(b)) => self.push(Value::Float(a / b))?,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "float",
                                got: format!("{} and {}", a.type_name(), b.type_name()),
                            })
                        }
                    }
                }
                OpCode::SConcat => {
                    // String concatenation (optimized)
                    let b = self.pop()?;
                    let a = self.pop()?;
                    match (&a, &b) {
                        (Value::String(a), Value::String(b)) => {
                            let mut result = (**a).to_string();
                            result.push_str(b);
                            self.push(Value::String(Rc::new(result)))?
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "string",
                                got: format!("{} and {}", a.type_name(), b.type_name()),
                            })
                        }
                    }
                }

                // BITWISE
                OpCode::BitAnd => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    match (&a, &b) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Int(a & b))?,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "int",
                                got: format!("{} and {}", a.type_name(), b.type_name()),
                            })
                        }
                    }
                }
                OpCode::BitOr => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    match (&a, &b) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Int(a | b))?,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "int",
                                got: format!("{} and {}", a.type_name(), b.type_name()),
                            })
                        }
                    }
                }
                OpCode::BitXor => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    match (&a, &b) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Int(a ^ b))?,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "int",
                                got: format!("{} and {}", a.type_name(), b.type_name()),
                            })
                        }
                    }
                }
                OpCode::BitNot => {
                    let a = self.pop()?;
                    match a {
                        Value::Int(n) => self.push(Value::Int(!n))?,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "int",
                                got: a.type_name().to_string(),
                            })
                        }
                    }
                }
                OpCode::Shl => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    match (&a, &b) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Int(a << b))?,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "int",
                                got: format!("{} and {}", a.type_name(), b.type_name()),
                            })
                        }
                    }
                }
                OpCode::Shr => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    match (&a, &b) {
                        (Value::Int(a), Value::Int(b)) => self.push(Value::Int(a >> b))?,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "int",
                                got: format!("{} and {}", a.type_name(), b.type_name()),
                            })
                        }
                    }
                }

                // COMPARISON
                OpCode::Eq => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(Value::Bool(a == b))?;
                }
                OpCode::Ne => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    self.push(Value::Bool(a != b))?;
                }
                OpCode::Lt => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = self.compare(&a, &b)?;
                    self.push(Value::Bool(result < 0))?;
                }
                OpCode::Le => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = self.compare(&a, &b)?;
                    self.push(Value::Bool(result <= 0))?;
                }
                OpCode::Gt => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = self.compare(&a, &b)?;
                    self.push(Value::Bool(result > 0))?;
                }
                OpCode::Ge => {
                    let b = self.pop()?;
                    let a = self.pop()?;
                    let result = self.compare(&a, &b)?;
                    self.push(Value::Bool(result >= 0))?;
                }
                OpCode::Not => {
                    let a = self.pop()?;
                    self.push(Value::Bool(!a.is_truthy()))?;
                }

                // CONTROL FLOW
                OpCode::Jump => {
                    let offset = self.read_u16() as usize;
                    self.current_frame_mut().ip += offset;
                }
                OpCode::JumpIfFalse => {
                    let offset = self.read_u16() as usize;
                    let condition = self.pop()?;
                    if !condition.is_truthy() {
                        self.current_frame_mut().ip += offset;
                    }
                }
                OpCode::JumpIfFalseNoPop => {
                    let offset = self.read_u16() as usize;
                    let condition = self.peek(0)?;
                    if !condition.is_truthy() {
                        self.current_frame_mut().ip += offset;
                    }
                }
                OpCode::JumpIfTrueNoPop => {
                    let offset = self.read_u16() as usize;
                    let condition = self.peek(0)?;
                    if condition.is_truthy() {
                        self.current_frame_mut().ip += offset;
                    }
                }
                OpCode::Loop => {
                    let offset = self.read_u16() as usize;
                    self.current_frame_mut().ip -= offset;
                }

                // CALLS
                OpCode::Call => {
                    let arg_count = self.read_byte();
                    let callee_idx = self.stack.len() - 1 - arg_count as usize;
                    let callee = self.stack[callee_idx].clone();

                    match callee {
                        Value::Closure(closure) => {
                            // Remove callee from stack and adjust
                            self.stack.remove(callee_idx);
                            self.call_closure(closure, arg_count)?;
                        }
                        Value::Native(native) => {
                            if arg_count != native.arity {
                                return Err(RuntimeError::ArityMismatch {
                                    expected: native.arity,
                                    got: arg_count,
                                });
                            }
                            let start = self.stack.len() - arg_count as usize;
                            let result = (native.func)(&mut self.stack[start..])
                                .map_err(RuntimeError::Custom)?;
                            // Pop args and callee
                            self.stack.truncate(callee_idx);
                            self.push(result)?;
                        }
                        Value::VMNative(id) => {
                            let (func, arity) = self.vm_natives[id as usize];
                            if arg_count != arity {
                                return Err(RuntimeError::ArityMismatch {
                                    expected: arity,
                                    got: arg_count,
                                });
                            }
                            // Collect arguments
                            let start = self.stack.len() - arg_count as usize;
                            let args: Vec<Value> = self.stack[start..].to_vec();
                            // Pop args and callee
                            self.stack.truncate(callee_idx);
                            // Call the VM-native function
                            let result = func(self, &args)?;
                            self.push(result)?;
                        }
                        _ => {
                            return Err(RuntimeError::NotCallable(callee.type_name().to_string()))
                        }
                    }
                }
                OpCode::TailCall => {
                    let arg_count = self.read_byte();
                    let callee_idx = self.stack.len() - 1 - arg_count as usize;
                    let callee = self.stack[callee_idx].clone();

                    match callee {
                        Value::Closure(closure) => {
                            if arg_count != closure.proto.arity {
                                return Err(RuntimeError::ArityMismatch {
                                    expected: closure.proto.arity,
                                    got: arg_count,
                                });
                            }

                            // Reuse current frame
                            let frame = self.current_frame_mut();
                            let old_base = frame.stack_base;

                            // Copy arguments to frame base
                            let new_args_start = self.stack.len() - arg_count as usize;
                            for i in 0..arg_count as usize {
                                self.stack[old_base + i] = self.stack[new_args_start + i].clone();
                            }

                            // Truncate stack
                            self.stack.truncate(old_base + arg_count as usize);

                            // Update frame
                            let frame = self.current_frame_mut();
                            frame.closure = closure;
                            frame.ip = 0;
                        }
                        _ => {
                            return Err(RuntimeError::NotCallable(callee.type_name().to_string()))
                        }
                    }
                }
                OpCode::Return => {
                    let result = self.pop()?;
                    let frame = self.frames.pop().unwrap();

                    // Close upvalues
                    self.close_upvalues(frame.stack_base);

                    // Pop locals
                    self.stack.truncate(frame.stack_base);

                    // Check if we've reached the target frame count (for nested calls)
                    if let Some(target) = target_frame_count {
                        if self.frames.len() <= target {
                            return Ok(result);
                        }
                    } else if self.frames.is_empty() {
                        return Ok(result);
                    }

                    self.push(result)?;
                }

                OpCode::CallMethod => {
                    let method_name_idx = self.read_u16() as usize;
                    let arg_count = self.read_byte();

                    let method_name = match self.read_constant(method_name_idx) {
                        Constant::String(s) => s.clone(),
                        _ => return Err(RuntimeError::InternalError("Invalid method name constant".to_string())),
                    };

                    // Get receiver (first arg, at bottom of args on stack)
                    let receiver_idx = self.stack.len() - arg_count as usize;
                    let receiver = &self.stack[receiver_idx];

                    // Get the type name from the receiver
                    let type_name = match receiver {
                        Value::Struct(s) => s.name.clone(),
                        Value::Enum(e) => e.enum_name.clone(),
                        _ => receiver.type_name().to_string(),
                    };

                    // Look up the method
                    let closure = self.methods
                        .get(&type_name)
                        .and_then(|methods| methods.get(&method_name))
                        .cloned()
                        .ok_or_else(|| RuntimeError::UndefinedMethod {
                            method: method_name.clone(),
                            type_name: type_name.clone()
                        })?;

                    // Check arity
                    if closure.proto.arity != arg_count {
                        return Err(RuntimeError::ArityMismatch {
                            expected: closure.proto.arity,
                            got: arg_count,
                        });
                    }

                    // Call the method (receiver is already on stack as first arg)
                    self.call_closure(closure, arg_count)?;
                }

                OpCode::CallStatic => {
                    let type_name_idx = self.read_u16() as usize;
                    let method_name_idx = self.read_u16() as usize;
                    let arg_count = self.read_byte();

                    let type_name = match self.read_constant(type_name_idx) {
                        Constant::String(s) => s.clone(),
                        _ => return Err(RuntimeError::InternalError("Invalid type name constant".to_string())),
                    };

                    let method_name = match self.read_constant(method_name_idx) {
                        Constant::String(s) => s.clone(),
                        _ => return Err(RuntimeError::InternalError("Invalid method name constant".to_string())),
                    };

                    // Look up the static method
                    let closure = self.methods
                        .get(&type_name)
                        .and_then(|methods| methods.get(&method_name))
                        .cloned()
                        .ok_or_else(|| RuntimeError::UndefinedMethod {
                            method: method_name.clone(),
                            type_name: type_name.clone()
                        })?;

                    // Check arity
                    if closure.proto.arity != arg_count {
                        return Err(RuntimeError::ArityMismatch {
                            expected: closure.proto.arity,
                            got: arg_count,
                        });
                    }

                    // Call the static method (no receiver)
                    self.call_closure(closure, arg_count)?;
                }

                OpCode::DefineMethod => {
                    let type_name_idx = self.read_u16() as usize;
                    let method_name_idx = self.read_u16() as usize;

                    let type_name = match self.read_constant(type_name_idx) {
                        Constant::String(s) => s.clone(),
                        _ => return Err(RuntimeError::InternalError("Invalid type name constant".to_string())),
                    };

                    let method_name = match self.read_constant(method_name_idx) {
                        Constant::String(s) => s.clone(),
                        _ => return Err(RuntimeError::InternalError("Invalid method name constant".to_string())),
                    };

                    // Pop the closure from the stack
                    let closure = match self.pop()? {
                        Value::Closure(c) => c,
                        v => return Err(RuntimeError::TypeError {
                            expected: "closure",
                            got: v.type_name().to_string()
                        }),
                    };

                    // Register the method
                    self.methods
                        .entry(type_name)
                        .or_insert_with(HashMap::new)
                        .insert(method_name, closure);
                }

                // CLOSURES
                OpCode::Closure => {
                    let idx = self.read_u16() as usize;
                    let upvalue_count = self.read_byte() as usize;

                    let proto = match self.read_constant(idx) {
                        Constant::Function(f) => Rc::new((**f).clone()),
                        _ => {
                            return Err(RuntimeError::Custom(
                                "Invalid closure constant".to_string(),
                            ))
                        }
                    };

                    let mut upvalues = Vec::with_capacity(upvalue_count);
                    for _ in 0..upvalue_count {
                        let is_local = self.read_byte() != 0;
                        let index = self.read_byte() as usize;

                        let upvalue = if is_local {
                            let slot = self.current_frame().stack_base + index;
                            self.capture_upvalue(slot)
                        } else {
                            self.current_frame().closure.upvalues[index].clone()
                        };
                        upvalues.push(upvalue);
                    }

                    let closure = Closure { proto, upvalues };
                    self.push(Value::Closure(Rc::new(closure)))?;
                }

                // DATA STRUCTURES
                OpCode::Tuple => {
                    let count = self.read_byte() as usize;
                    let mut elements = Vec::with_capacity(count);
                    for _ in 0..count {
                        elements.push(self.pop()?);
                    }
                    elements.reverse();
                    self.push(Value::Tuple(Rc::new(elements)))?;
                }
                OpCode::Array => {
                    let count = self.read_u16() as usize;
                    let mut elements = Vec::with_capacity(count);
                    for _ in 0..count {
                        elements.push(self.pop()?);
                    }
                    elements.reverse();
                    self.push(Value::Array(Rc::new(RefCell::new(elements))))?;
                }
                OpCode::Struct => {
                    let struct_info_idx = self.read_u16();
                    let field_count = self.read_byte() as usize;

                    // Get struct info from constants
                    let (name, field_names) = match self.read_constant(struct_info_idx as usize) {
                        Constant::StructInfo { name, fields } => (name.clone(), fields.clone()),
                        _ => return Err(RuntimeError::InternalError(
                            "Expected StructInfo constant".to_string()
                        )),
                    };

                    // Pop field values (in reverse order since they're on stack)
                    let mut values = Vec::with_capacity(field_count);
                    for _ in 0..field_count {
                        values.push(self.pop()?);
                    }
                    values.reverse();

                    let instance = StructInstance::new(name, field_names, values);
                    self.push(Value::Struct(Rc::new(instance)))?;
                }
                OpCode::Enum => {
                    let enum_info_idx = self.read_u16();
                    let variant_idx = self.read_byte();
                    let field_count = self.read_byte() as usize;

                    // Get enum info from constants
                    let (enum_name, variant_name) = match self.read_constant(enum_info_idx as usize) {
                        Constant::EnumInfo { name, variants } => {
                            let variant = &variants[variant_idx as usize];
                            (name.clone(), variant.0.clone())
                        }
                        _ => return Err(RuntimeError::InternalError(
                            "Expected EnumInfo constant".to_string()
                        )),
                    };

                    // Pop field values (in reverse order since they're on stack)
                    let mut fields = Vec::with_capacity(field_count);
                    for _ in 0..field_count {
                        fields.push(self.pop()?);
                    }
                    fields.reverse();

                    let instance = EnumInstance {
                        enum_name,
                        variant: variant_name,
                        fields,
                    };
                    self.push(Value::Enum(Rc::new(instance)))?;
                }
                OpCode::Range => {
                    let inclusive = self.read_byte() != 0;
                    let end = match self.pop()? {
                        Value::Int(n) => n,
                        v => return Err(RuntimeError::TypeError {
                            expected: "int",
                            got: v.type_name().to_string(),
                        }),
                    };
                    let start = match self.pop()? {
                        Value::Int(n) => n,
                        v => return Err(RuntimeError::TypeError {
                            expected: "int",
                            got: v.type_name().to_string(),
                        }),
                    };
                    self.push(Value::Range(start, end, inclusive))?;
                }
                OpCode::GetField => {
                    let index = self.read_byte() as usize;
                    let value = self.pop()?;
                    let field = match value {
                        Value::Tuple(t) => t
                            .get(index)
                            .cloned()
                            .ok_or_else(|| RuntimeError::IndexOutOfBounds {
                                index: index as i64,
                                length: t.len(),
                            })?,
                        Value::Array(arr) => {
                            let arr = arr.borrow();
                            arr.get(index)
                                .cloned()
                                .ok_or_else(|| RuntimeError::IndexOutOfBounds {
                                    index: index as i64,
                                    length: arr.len(),
                                })?
                        }
                        Value::Enum(e) => e
                            .fields
                            .get(index)
                            .cloned()
                            .ok_or_else(|| RuntimeError::IndexOutOfBounds {
                                index: index as i64,
                                length: e.fields.len(),
                            })?,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "tuple, array, or enum",
                                got: value.type_name().to_string(),
                            })
                        }
                    };
                    self.push(field)?;
                }
                OpCode::SetField => {
                    let index = self.read_byte() as usize;
                    let value = self.pop()?;
                    let target = self.pop()?;
                    match target {
                        Value::Array(arr) => {
                            let mut arr = arr.borrow_mut();
                            if index >= arr.len() {
                                return Err(RuntimeError::IndexOutOfBounds {
                                    index: index as i64,
                                    length: arr.len(),
                                });
                            }
                            arr[index] = value;
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "array",
                                got: target.type_name().to_string(),
                            })
                        }
                    }
                    self.push(Value::Unit)?;
                }
                OpCode::GetFieldNamed => {
                    // Call site ID for inline caching (IP before reading operands)
                    let call_site = CallSiteId::from_offset(self.current_frame().ip.saturating_sub(1));
                    let field_name_idx = self.read_u16();
                    let field_name = match self.read_constant(field_name_idx as usize) {
                        Constant::String(s) => s.clone(),
                        _ => return Err(RuntimeError::InternalError(
                            "Expected string constant for field name".to_string()
                        )),
                    };
                    let value = self.pop()?;
                    let field = match value {
                        Value::Struct(s) => {
                            let type_id = TypeId::from_name(&s.name);

                            // Try cache first
                            if let Some(cached_idx) = self.inline_cache.lookup_field(call_site, type_id) {
                                // Cache hit - use indexed access
                                s.get_field_idx(cached_idx).ok_or_else(|| {
                                    RuntimeError::UndefinedField {
                                        field: field_name.clone(),
                                        type_name: s.name.clone(),
                                    }
                                })?
                            } else {
                                // Cache miss - look up index and update cache
                                if let Some(&idx) = s.field_indices.get(&field_name) {
                                    self.inline_cache.update_field(call_site, type_id, idx);
                                    s.get_field_idx(idx).ok_or_else(|| {
                                        RuntimeError::UndefinedField {
                                            field: field_name.clone(),
                                            type_name: s.name.clone(),
                                        }
                                    })?
                                } else {
                                    // Field not in ordered fields, try dynamic
                                    s.get_field(&field_name).ok_or_else(|| {
                                        RuntimeError::UndefinedField {
                                            field: field_name.clone(),
                                            type_name: s.name.clone(),
                                        }
                                    })?
                                }
                            }
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "struct",
                                got: value.type_name().to_string(),
                            })
                        }
                    };
                    self.push(field)?;
                }
                OpCode::SetFieldNamed => {
                    // Call site ID for inline caching
                    let call_site = CallSiteId::from_offset(self.current_frame().ip.saturating_sub(1));
                    let field_name_idx = self.read_u16();
                    let field_name = match self.read_constant(field_name_idx as usize) {
                        Constant::String(s) => s.clone(),
                        _ => return Err(RuntimeError::InternalError(
                            "Expected string constant for field name".to_string()
                        )),
                    };
                    let target = self.pop()?;
                    let value = self.pop()?;
                    match &target {
                        Value::Struct(s) => {
                            let type_id = TypeId::from_name(&s.name);

                            // Try cache first
                            if let Some(cached_idx) = self.inline_cache.lookup_field(call_site, type_id) {
                                // Cache hit - use indexed access
                                s.set_field_idx(cached_idx, value.clone());
                            } else {
                                // Cache miss - look up index and update cache
                                if let Some(&idx) = s.field_indices.get(&field_name) {
                                    self.inline_cache.update_field(call_site, type_id, idx);
                                    s.set_field_idx(idx, value.clone());
                                } else if !s.set_field(&field_name, value.clone()) {
                                    return Err(RuntimeError::UndefinedField {
                                        field: field_name,
                                        type_name: s.name.clone(),
                                    });
                                }
                            }
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "struct",
                                got: target.type_name().to_string(),
                            })
                        }
                    }
                    // Push the assigned value back (like StoreLocal leaves value on stack)
                    self.push(value)?;
                }
                OpCode::Index => {
                    let index = self.pop()?;
                    let target = self.pop()?;
                    let result = match (&target, &index) {
                        (Value::Array(arr), Value::Int(i)) => {
                            let arr = arr.borrow();
                            let idx = if *i < 0 {
                                (arr.len() as i64 + i) as usize
                            } else {
                                *i as usize
                            };
                            arr.get(idx)
                                .cloned()
                                .ok_or_else(|| RuntimeError::IndexOutOfBounds {
                                    index: *i,
                                    length: arr.len(),
                                })?
                        }
                        (Value::Tuple(t), Value::Int(i)) => {
                            let idx = if *i < 0 {
                                (t.len() as i64 + i) as usize
                            } else {
                                *i as usize
                            };
                            t.get(idx)
                                .cloned()
                                .ok_or_else(|| RuntimeError::IndexOutOfBounds {
                                    index: *i,
                                    length: t.len(),
                                })?
                        }
                        (Value::String(s), Value::Int(i)) => {
                            let idx = if *i < 0 {
                                (s.len() as i64 + i) as usize
                            } else {
                                *i as usize
                            };
                            s.chars()
                                .nth(idx)
                                .map(|c| Value::Int(c as i64))
                                .ok_or_else(|| RuntimeError::IndexOutOfBounds {
                                    index: *i,
                                    length: s.len(),
                                })?
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "indexable type with int index",
                                got: format!("{}[{}]", target.type_name(), index.type_name()),
                            })
                        }
                    };
                    self.push(result)?;
                }
                OpCode::IndexSet => {
                    let value = self.pop()?;
                    let index = self.pop()?;
                    let target = self.pop()?;
                    match (&target, &index) {
                        (Value::Array(arr), Value::Int(i)) => {
                            let mut arr = arr.borrow_mut();
                            let idx = if *i < 0 {
                                (arr.len() as i64 + i) as usize
                            } else {
                                *i as usize
                            };
                            if idx >= arr.len() {
                                return Err(RuntimeError::IndexOutOfBounds {
                                    index: *i,
                                    length: arr.len(),
                                });
                            }
                            arr[idx] = value;
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "array[int]",
                                got: format!("{}[{}]", target.type_name(), index.type_name()),
                            })
                        }
                    }
                    self.push(Value::Unit)?;
                }
                OpCode::Len => {
                    let value = self.pop()?;
                    let len = match &value {
                        Value::String(s) => s.len() as i64,
                        Value::Array(arr) => arr.borrow().len() as i64,
                        Value::Tuple(t) => t.len() as i64,
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "string, array, or tuple",
                                got: value.type_name().to_string(),
                            })
                        }
                    };
                    self.push(Value::Int(len))?;
                }
                OpCode::Slice => {
                    let end = self.pop()?;
                    let start = self.pop()?;
                    let target = self.pop()?;
                    match (&target, &start, &end) {
                        (Value::Array(arr), Value::Int(s), Value::Int(e)) => {
                            let arr = arr.borrow();
                            let start = *s as usize;
                            let end = (*e as usize).min(arr.len());
                            let slice: Vec<Value> = arr[start..end].to_vec();
                            self.push(Value::Array(Rc::new(RefCell::new(slice))))?;
                        }
                        (Value::String(string), Value::Int(s), Value::Int(e)) => {
                            let start = *s as usize;
                            let end = (*e as usize).min(string.len());
                            let slice: String = string.chars().skip(start).take(end - start).collect();
                            self.push(Value::String(Rc::new(slice)))?;
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "sliceable type",
                                got: target.type_name().to_string(),
                            })
                        }
                    }
                }

                // PATTERN MATCHING
                OpCode::TryUnwrap => {
                    let value = self.pop()?;
                    match &value {
                        Value::Enum(e) => {
                            // Check for Some/Ok variants (unwrap to inner value)
                            // Works for any Option-like or Result-like enum
                            if e.variant == "Some" || e.variant == "Ok" {
                                // Unwrap Some(x) or Ok(x) -> push x
                                if let Some(inner) = e.fields.first() {
                                    self.push(inner.clone())?;
                                } else {
                                    self.push(Value::Unit)?;
                                }
                            } else if e.variant == "None" || e.variant == "Err" {
                                // None or Err -> return from function with the original value
                                let frame = self.frames.pop().unwrap();
                                self.close_upvalues(frame.stack_base);
                                self.stack.truncate(frame.stack_base);
                                if self.frames.is_empty() {
                                    return Ok(value);
                                }
                                self.push(value)?;
                            } else {
                                // Unknown variant - just push value back
                                self.push(value)?;
                            }
                        }
                        _ => {
                            // Not an enum - just push back (may be a type error)
                            self.push(value)?;
                        }
                    }
                }

                OpCode::MatchEnum => {
                    let enum_name_idx = self.read_u16() as usize;
                    let variant_name_idx = self.read_u16() as usize;

                    let enum_name = match self.read_constant(enum_name_idx) {
                        Constant::String(s) => s.clone(),
                        _ => return Err(RuntimeError::InternalError("Invalid enum name constant".to_string())),
                    };

                    let variant_name = match self.read_constant(variant_name_idx) {
                        Constant::String(s) => s.clone(),
                        _ => return Err(RuntimeError::InternalError("Invalid variant name constant".to_string())),
                    };

                    let scrutinee = self.pop()?;

                    let matches = match &scrutinee {
                        Value::Enum(e) => {
                            // If enum_name is empty, match any enum with this variant
                            let enum_matches = enum_name.is_empty() || e.enum_name == enum_name;
                            let variant_matches = e.variant == variant_name;
                            enum_matches && variant_matches
                        }
                        _ => false,
                    };

                    self.push(Value::Bool(matches))?;
                }

                // ITERATION
                OpCode::Iter => {
                    let value = self.pop()?;
                    let iter: Box<dyn SquamIterator> = match value {
                        Value::Array(arr) => Box::new(ArrayIterator::new(arr)),
                        Value::Tuple(t) => {
                            // Convert tuple to array for iteration
                            let arr = Rc::new(RefCell::new((*t).clone()));
                            Box::new(ArrayIterator::new(arr))
                        }
                        Value::Range(start, end, inclusive) => {
                            Box::new(RangeIterator::new(start, end, inclusive))
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "iterable",
                                got: value.type_name().to_string(),
                            })
                        }
                    };
                    self.push(Value::Iterator(Rc::new(RefCell::new(iter))))?;
                }
                OpCode::IterNext => {
                    let iter = self.pop()?;
                    match iter {
                        Value::Iterator(it) => {
                            let next = it.borrow_mut().next();
                            match next {
                                Some(v) => {
                                    // Push value first, then true on top for JumpIfFalse to check
                                    self.push(v)?;
                                    self.push(Value::Bool(true))?;
                                }
                                None => {
                                    self.push(Value::Bool(false))?;
                                }
                            }
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "iterator",
                                got: iter.type_name().to_string(),
                            })
                        }
                    }
                }

                // REFERENCES
                OpCode::MakeRef => {
                    let slot = self.read_u16() as usize;
                    let mutable = self.read_byte() != 0;
                    let base = self.current_frame().stack_base;
                    // Create a LocalRef that points to the absolute stack index
                    let abs_slot = base + slot;
                    self.push(Value::LocalRef(abs_slot, mutable))?;
                }

                OpCode::Deref => {
                    let r = self.pop()?;
                    match r {
                        Value::LocalRef(slot, _) => {
                            let value = self.stack[slot].clone();
                            self.push(value)?;
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "reference",
                                got: r.type_name().to_string(),
                            })
                        }
                    }
                }

                OpCode::DerefStore => {
                    let value = self.pop()?;
                    let r = self.pop()?;
                    match r {
                        Value::LocalRef(slot, mutable) => {
                            if !mutable {
                                return Err(RuntimeError::Custom(
                                    "cannot assign through immutable reference".to_string()
                                ));
                            }
                            self.stack[slot] = value;
                            self.push(Value::Unit)?;
                        }
                        _ => {
                            return Err(RuntimeError::TypeError {
                                expected: "mutable reference",
                                got: r.type_name().to_string(),
                            })
                        }
                    }
                }

                // NATIVE/FFI
                OpCode::NativeCall => {
                    let _idx = self.read_u16();
                    self.push(Value::Unit)?;
                }

                // DEBUG
                OpCode::Print => {
                    let value = self.pop()?;
                    let output = format!("{}", value);
                    println!("{}", output);
                    self.output.push(output);
                    self.push(Value::Unit)?;
                }
                OpCode::Panic => {
                    let value = self.pop()?;
                    return Err(RuntimeError::Panic(format!("{}", value)));
                }
                OpCode::Assert => {
                    let value = self.pop()?;
                    if !value.is_truthy() {
                        return Err(RuntimeError::AssertionFailed);
                    }
                    self.push(Value::Unit)?;
                }

                OpCode::Nop => {}
                OpCode::Halt => {
                    return Ok(self.pop().unwrap_or(Value::Unit));
                }
            }
        }
    }

    // ---
    // Helper methods
    // ---

    fn constant_to_value(&self, idx: usize) -> Result<Value, RuntimeError> {
        let constant = &self.current_frame().closure.proto.chunk.constants[idx];
        Ok(match constant {
            Constant::Int(n) => Value::Int(*n),
            Constant::Float(n) => Value::Float(*n),
            Constant::String(s) => Value::String(Rc::new(s.clone())),
            Constant::Function(f) => {
                let closure = Closure {
                    proto: Rc::new((**f).clone()),
                    upvalues: Vec::new(),
                };
                Value::Closure(Rc::new(closure))
            }
            // These are metadata constants, not runtime values
            Constant::StructInfo { .. } | Constant::EnumInfo { .. } => {
                return Err(RuntimeError::InternalError(
                    "Cannot load type info as value".to_string()
                ));
            }
        })
    }

    fn binary_num_op<F, G>(
        &self,
        a: &Value,
        b: &Value,
        int_op: F,
        float_op: G,
    ) -> Result<Value, RuntimeError>
    where
        F: Fn(i64, i64) -> i64,
        G: Fn(f64, f64) -> f64,
    {
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(int_op(*a, *b))),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(float_op(*a, *b))),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(float_op(*a as f64, *b))),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(float_op(*a, *b as f64))),
            _ => Err(RuntimeError::TypeError {
                expected: "numbers",
                got: format!("{} and {}", a.type_name(), b.type_name()),
            }),
        }
    }

    fn compare(&self, a: &Value, b: &Value) -> Result<i8, RuntimeError> {
        match (a, b) {
            (Value::Int(a), Value::Int(b)) => Ok(a.cmp(b) as i8),
            (Value::Float(a), Value::Float(b)) => {
                Ok(a.partial_cmp(b).map(|o| o as i8).unwrap_or(0))
            }
            (Value::Int(a), Value::Float(b)) => {
                let a = *a as f64;
                Ok(a.partial_cmp(b).map(|o| o as i8).unwrap_or(0))
            }
            (Value::Float(a), Value::Int(b)) => {
                let b = *b as f64;
                Ok(a.partial_cmp(&b).map(|o| o as i8).unwrap_or(0))
            }
            (Value::String(a), Value::String(b)) => Ok(a.cmp(b) as i8),
            _ => Err(RuntimeError::TypeError {
                expected: "comparable types",
                got: format!("{} and {}", a.type_name(), b.type_name()),
            }),
        }
    }

    fn capture_upvalue(&mut self, stack_slot: usize) -> Rc<RefCell<Upvalue>> {
        // Check if we already have this upvalue open
        for uv in &self.open_upvalues {
            if let Upvalue::Open(slot) = &*uv.borrow() {
                if *slot == stack_slot {
                    return uv.clone();
                }
            }
        }

        // Create new open upvalue
        let upvalue = Rc::new(RefCell::new(Upvalue::Open(stack_slot)));
        self.open_upvalues.push(upvalue.clone());
        upvalue
    }

    fn close_upvalues(&mut self, from_slot: usize) {
        let mut i = 0;
        while i < self.open_upvalues.len() {
            let should_close = {
                let uv = self.open_upvalues[i].borrow();
                if let Upvalue::Open(slot) = &*uv {
                    *slot >= from_slot
                } else {
                    false
                }
            };

            if should_close {
                let uv = self.open_upvalues.remove(i);
                let value = {
                    let inner = uv.borrow();
                    if let Upvalue::Open(slot) = &*inner {
                        self.stack[*slot].clone()
                    } else {
                        continue;
                    }
                };
                *uv.borrow_mut() = Upvalue::Closed(value);
            } else {
                i += 1;
            }
        }
    }
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use squam_compiler::Compiler;
    use squam_parser::Parser;

    fn run_code(source: &str) -> Result<Value, RuntimeError> {
        let mut parser = Parser::new(source, 0);
        let module = parser.parse_module();
        assert!(
            parser.errors().is_empty(),
            "Parse errors: {:?}",
            parser.errors()
        );

        let mut compiler = Compiler::new();
        let proto = compiler.compile_module(&module).unwrap();

        let mut vm = VM::new();
        vm.run(&proto)
    }

    #[test]
    fn test_simple_return() {
        // Module returns unit by default
        let result = run_code("").unwrap();
        assert_eq!(result, Value::Unit);
    }

    #[test]
    fn test_arithmetic() {
        let result = run_code("fn main() -> int { 1 + 2 * 3 }").unwrap();
        // Note: main is defined but the module itself returns unit
        // The actual computation happens inside main
        assert_eq!(result, Value::Unit);
    }

    #[test]
    fn test_call_main() {
        // We need to call main to get the result
        let source = "fn main() -> int { 1 + 2 }";
        let mut parser = Parser::new(source, 0);
        let module = parser.parse_module();

        let mut compiler = Compiler::new();
        let proto = compiler.compile_module(&module).unwrap();

        let mut vm = VM::new();
        vm.run(&proto).unwrap();

        // Now call main
        let main = vm.globals.get("main").cloned().unwrap();
        let result = vm.call(main, vec![]).unwrap();
        assert_eq!(result, Value::Int(3));
    }
}