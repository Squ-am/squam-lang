/// Bytecode opcodes.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum OpCode {
    // STACK OPERATIONS (0x00-0x0F)
    /// Push a constant from the constant pool [index: u16]
    Const = 0x00,
    /// Push a small constant [index: u8]
    ConstSmall = 0x01,
    /// Push unit value
    Unit = 0x02,
    /// Push true
    True = 0x03,
    /// Push false
    False = 0x04,
    /// Duplicate top of stack
    Dup = 0x05,
    /// Pop top of stack
    Pop = 0x06,
    /// Pop N values [count: u8]
    PopN = 0x07,
    /// Swap top two values
    Swap = 0x08,

    // LOCAL VARIABLES (0x10-0x1F)
    /// Load local variable [slot: u16]
    LoadLocal = 0x10,
    /// Load local (small index) [slot: u8]
    LoadLocalSmall = 0x11,
    /// Store to local variable [slot: u16]
    StoreLocal = 0x12,
    /// Store to local (small index) [slot: u8]
    StoreLocalSmall = 0x13,
    /// Load global variable [index: u16]
    LoadGlobal = 0x14,
    /// Store to global variable [index: u16]
    StoreGlobal = 0x15,
    /// Load upvalue (closure capture) [index: u8]
    LoadUpvalue = 0x16,
    /// Store to upvalue [index: u8]
    StoreUpvalue = 0x17,
    /// Close upvalue at stack position
    CloseUpvalue = 0x18,

    // ARITHMETIC (0x20-0x2F)
    /// Add two values
    Add = 0x20,
    /// Subtract
    Sub = 0x21,
    /// Multiply
    Mul = 0x22,
    /// Divide
    Div = 0x23,
    /// Remainder
    Rem = 0x24,
    /// Negate
    Neg = 0x25,
    /// Integer add (no overflow check)
    IAdd = 0x26,
    /// Integer subtract
    ISub = 0x27,

    // BITWISE (0x28-0x2F)
    /// Bitwise and
    BitAnd = 0x28,
    /// Bitwise or
    BitOr = 0x29,
    /// Bitwise xor
    BitXor = 0x2A,
    /// Bitwise not
    BitNot = 0x2B,
    /// Shift left
    Shl = 0x2C,
    /// Shift right
    Shr = 0x2D,

    // COMPARISON (0x30-0x3F)
    /// Equal
    Eq = 0x30,
    /// Not equal
    Ne = 0x31,
    /// Less than
    Lt = 0x32,
    /// Less than or equal
    Le = 0x33,
    /// Greater than
    Gt = 0x34,
    /// Greater than or equal
    Ge = 0x35,
    /// Logical not
    Not = 0x38,

    // CONTROL FLOW (0x40-0x4F)
    /// Unconditional jump [offset: i16]
    Jump = 0x40,
    /// Jump if top is false (pops) [offset: i16]
    JumpIfFalse = 0x41,
    /// Jump if top is true (pops) [offset: i16]
    JumpIfTrue = 0x42,
    /// Jump if false, don't pop [offset: i16]
    JumpIfFalseNoPop = 0x43,
    /// Jump if true, don't pop [offset: i16]
    JumpIfTrueNoPop = 0x44,
    /// Loop back [offset: u16]
    Loop = 0x45,

    // FUNCTIONS (0x50-0x5F)
    /// Call function [arg_count: u8]
    Call = 0x50,
    /// Tail call [arg_count: u8]
    TailCall = 0x51,
    /// Return from function
    Return = 0x52,
    /// Call method [method_name_idx: u16, arg_count: u8]
    /// Stack: receiver, args... -> pops all, pushes result
    CallMethod = 0x54,
    /// Define method [type_name_idx: u16, method_name_idx: u16]
    /// Stack: closure -> pops closure and registers as method
    DefineMethod = 0x55,
    /// Call static method [type_name_idx: u16, method_name_idx: u16, arg_count: u8]
    /// Stack: args... -> pops args, pushes result
    CallStatic = 0x56,
    /// Create closure [proto_index: u16, upvalue_count: u8]
    Closure = 0x58,

    // DATA STRUCTURES (0x60-0x6F)
    /// Create tuple [count: u8]
    Tuple = 0x60,
    /// Create array [count: u16]
    Array = 0x61,
    /// Create struct [struct_info_idx: u16, field_count: u8]
    /// Stack: field values in declaration order, struct_info constant has field names
    Struct = 0x62,
    /// Create enum variant [enum_info_idx: u16, variant_idx: u8, field_count: u8]
    /// Stack: field values (field_count items), pops them and creates enum instance
    Enum = 0x63,
    /// Create range [inclusive: u8] (0 = exclusive, 1 = inclusive)
    Range = 0x64,

    // FIELD ACCESS (0x68-0x6F)
    /// Get field by index [index: u8] (for tuples)
    GetField = 0x68,
    /// Set field by index [index: u8] (for tuples)
    SetField = 0x69,
    /// Get named field [field_name_idx: u16] (for structs)
    GetFieldNamed = 0x6A,
    /// Set named field [field_name_idx: u16] (for structs)
    SetFieldNamed = 0x6B,

    // ARRAY/SLICE (0x70-0x7F)
    /// Index into array/slice
    Index = 0x70,
    /// Set array/slice element
    IndexSet = 0x71,
    /// Get length
    Len = 0x72,
    /// Slice [start, end on stack]
    Slice = 0x73,

    // PATTERN MATCHING (0x80-0x8F)
    /// Match enum variant [enum_name_idx: u16, variant_name_idx: u16]
    /// Stack: scrutinee -> pops scrutinee, pushes bool (true if matches)
    MatchEnum = 0x80,
    /// Try unwrap - for ? operator
    /// Stack: [Option/Result] -> if Some/Ok, unwraps to value; if None/Err, returns from function
    TryUnwrap = 0x81,

    // ITERATION (0x90-0x9F)
    /// Create iterator from value
    Iter = 0x90,
    /// Get next from iterator (pushes Option)
    IterNext = 0x91,

    // REFERENCES (0xA0-0xAF)
    /// Create reference to local variable [slot: u16, mutable: u8]
    /// Stack: -> pushes Ref value
    MakeRef = 0xA0,
    /// Dereference (read through reference)
    /// Stack: ref -> value
    Deref = 0xA1,
    /// Dereference and store (write through reference)
    /// Stack: ref, value -> ()
    DerefStore = 0xA2,

    // NATIVE/FFI (0xF0-0xF7)
    /// Call native function [index: u16]
    NativeCall = 0xF0,

    // DEBUG (0xF8-0xFE)
    /// Print top of stack (debug)
    Print = 0xF8,
    /// Panic with message
    Panic = 0xF9,
    /// Assertion
    Assert = 0xFA,

    /// No operation
    Nop = 0xFE,
    /// Halt execution
    Halt = 0xFF,
}

impl OpCode {
    /// Get the size of an instruction (including operands) in bytes.
    pub fn instruction_size(self) -> usize {
        match self {
            // No operands
            OpCode::Unit
            | OpCode::True
            | OpCode::False
            | OpCode::Dup
            | OpCode::Pop
            | OpCode::Swap
            | OpCode::Add
            | OpCode::Sub
            | OpCode::Mul
            | OpCode::Div
            | OpCode::Rem
            | OpCode::Neg
            | OpCode::IAdd
            | OpCode::ISub
            | OpCode::BitAnd
            | OpCode::BitOr
            | OpCode::BitXor
            | OpCode::BitNot
            | OpCode::Shl
            | OpCode::Shr
            | OpCode::Eq
            | OpCode::Ne
            | OpCode::Lt
            | OpCode::Le
            | OpCode::Gt
            | OpCode::Ge
            | OpCode::Not
            | OpCode::Return
            | OpCode::CloseUpvalue
            | OpCode::Index
            | OpCode::IndexSet
            | OpCode::Len
            | OpCode::Slice
            | OpCode::TryUnwrap
            | OpCode::Iter
            | OpCode::IterNext
            | OpCode::Deref
            | OpCode::DerefStore
            | OpCode::Print
            | OpCode::Panic
            | OpCode::Assert
            | OpCode::Nop
            | OpCode::Halt => 1,

            // 1-byte operand
            OpCode::ConstSmall
            | OpCode::PopN
            | OpCode::LoadLocalSmall
            | OpCode::StoreLocalSmall
            | OpCode::LoadUpvalue
            | OpCode::StoreUpvalue
            | OpCode::Call
            | OpCode::TailCall
            | OpCode::Tuple
            | OpCode::Range
            | OpCode::GetField
            | OpCode::SetField => 2,

            // 2-byte operand
            OpCode::Const
            | OpCode::LoadLocal
            | OpCode::StoreLocal
            | OpCode::LoadGlobal
            | OpCode::StoreGlobal
            | OpCode::Jump
            | OpCode::JumpIfFalse
            | OpCode::JumpIfTrue
            | OpCode::JumpIfFalseNoPop
            | OpCode::JumpIfTrueNoPop
            | OpCode::Loop
            | OpCode::Array
            | OpCode::NativeCall
            | OpCode::GetFieldNamed
            | OpCode::SetFieldNamed => 3,

            // 3-byte operand (u16 + u8)
            OpCode::Closure | OpCode::Struct | OpCode::CallMethod | OpCode::MakeRef => 4,
            // 4-byte operand (u16 + u16 or u16 + u8 + u8)
            OpCode::Enum | OpCode::DefineMethod | OpCode::MatchEnum => 5,
            // 5-byte operand (u16 + u16 + u8)
            OpCode::CallStatic => 6,
        }
    }

    /// Get a human-readable name for the opcode.
    pub fn name(self) -> &'static str {
        match self {
            OpCode::Const => "CONST",
            OpCode::ConstSmall => "CONST_SMALL",
            OpCode::Unit => "UNIT",
            OpCode::True => "TRUE",
            OpCode::False => "FALSE",
            OpCode::Dup => "DUP",
            OpCode::Pop => "POP",
            OpCode::PopN => "POP_N",
            OpCode::Swap => "SWAP",
            OpCode::LoadLocal => "LOAD_LOCAL",
            OpCode::LoadLocalSmall => "LOAD_LOCAL_SMALL",
            OpCode::StoreLocal => "STORE_LOCAL",
            OpCode::StoreLocalSmall => "STORE_LOCAL_SMALL",
            OpCode::LoadGlobal => "LOAD_GLOBAL",
            OpCode::StoreGlobal => "STORE_GLOBAL",
            OpCode::LoadUpvalue => "LOAD_UPVALUE",
            OpCode::StoreUpvalue => "STORE_UPVALUE",
            OpCode::CloseUpvalue => "CLOSE_UPVALUE",
            OpCode::Add => "ADD",
            OpCode::Sub => "SUB",
            OpCode::Mul => "MUL",
            OpCode::Div => "DIV",
            OpCode::Rem => "REM",
            OpCode::Neg => "NEG",
            OpCode::IAdd => "IADD",
            OpCode::ISub => "ISUB",
            OpCode::BitAnd => "BIT_AND",
            OpCode::BitOr => "BIT_OR",
            OpCode::BitXor => "BIT_XOR",
            OpCode::BitNot => "BIT_NOT",
            OpCode::Shl => "SHL",
            OpCode::Shr => "SHR",
            OpCode::Eq => "EQ",
            OpCode::Ne => "NE",
            OpCode::Lt => "LT",
            OpCode::Le => "LE",
            OpCode::Gt => "GT",
            OpCode::Ge => "GE",
            OpCode::Not => "NOT",
            OpCode::Jump => "JUMP",
            OpCode::JumpIfFalse => "JUMP_IF_FALSE",
            OpCode::JumpIfTrue => "JUMP_IF_TRUE",
            OpCode::JumpIfFalseNoPop => "JUMP_IF_FALSE_NO_POP",
            OpCode::JumpIfTrueNoPop => "JUMP_IF_TRUE_NO_POP",
            OpCode::Loop => "LOOP",
            OpCode::Call => "CALL",
            OpCode::TailCall => "TAIL_CALL",
            OpCode::Return => "RETURN",
            OpCode::CallMethod => "CALL_METHOD",
            OpCode::DefineMethod => "DEFINE_METHOD",
            OpCode::CallStatic => "CALL_STATIC",
            OpCode::Closure => "CLOSURE",
            OpCode::Tuple => "TUPLE",
            OpCode::Array => "ARRAY",
            OpCode::Struct => "STRUCT",
            OpCode::Enum => "ENUM",
            OpCode::Range => "RANGE",
            OpCode::GetField => "GET_FIELD",
            OpCode::SetField => "SET_FIELD",
            OpCode::GetFieldNamed => "GET_FIELD_NAMED",
            OpCode::SetFieldNamed => "SET_FIELD_NAMED",
            OpCode::Index => "INDEX",
            OpCode::IndexSet => "INDEX_SET",
            OpCode::Len => "LEN",
            OpCode::Slice => "SLICE",
            OpCode::MatchEnum => "MATCH_ENUM",
            OpCode::TryUnwrap => "TRY_UNWRAP",
            OpCode::Iter => "ITER",
            OpCode::IterNext => "ITER_NEXT",
            OpCode::MakeRef => "MAKE_REF",
            OpCode::Deref => "DEREF",
            OpCode::DerefStore => "DEREF_STORE",
            OpCode::NativeCall => "NATIVE_CALL",
            OpCode::Print => "PRINT",
            OpCode::Panic => "PANIC",
            OpCode::Assert => "ASSERT",
            OpCode::Nop => "NOP",
            OpCode::Halt => "HALT",
        }
    }
}

impl TryFrom<u8> for OpCode {
    type Error = ();

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0x00 => Ok(OpCode::Const),
            0x01 => Ok(OpCode::ConstSmall),
            0x02 => Ok(OpCode::Unit),
            0x03 => Ok(OpCode::True),
            0x04 => Ok(OpCode::False),
            0x05 => Ok(OpCode::Dup),
            0x06 => Ok(OpCode::Pop),
            0x07 => Ok(OpCode::PopN),
            0x08 => Ok(OpCode::Swap),
            0x10 => Ok(OpCode::LoadLocal),
            0x11 => Ok(OpCode::LoadLocalSmall),
            0x12 => Ok(OpCode::StoreLocal),
            0x13 => Ok(OpCode::StoreLocalSmall),
            0x14 => Ok(OpCode::LoadGlobal),
            0x15 => Ok(OpCode::StoreGlobal),
            0x16 => Ok(OpCode::LoadUpvalue),
            0x17 => Ok(OpCode::StoreUpvalue),
            0x18 => Ok(OpCode::CloseUpvalue),
            0x20 => Ok(OpCode::Add),
            0x21 => Ok(OpCode::Sub),
            0x22 => Ok(OpCode::Mul),
            0x23 => Ok(OpCode::Div),
            0x24 => Ok(OpCode::Rem),
            0x25 => Ok(OpCode::Neg),
            0x26 => Ok(OpCode::IAdd),
            0x27 => Ok(OpCode::ISub),
            0x28 => Ok(OpCode::BitAnd),
            0x29 => Ok(OpCode::BitOr),
            0x2A => Ok(OpCode::BitXor),
            0x2B => Ok(OpCode::BitNot),
            0x2C => Ok(OpCode::Shl),
            0x2D => Ok(OpCode::Shr),
            0x30 => Ok(OpCode::Eq),
            0x31 => Ok(OpCode::Ne),
            0x32 => Ok(OpCode::Lt),
            0x33 => Ok(OpCode::Le),
            0x34 => Ok(OpCode::Gt),
            0x35 => Ok(OpCode::Ge),
            0x38 => Ok(OpCode::Not),
            0x40 => Ok(OpCode::Jump),
            0x41 => Ok(OpCode::JumpIfFalse),
            0x42 => Ok(OpCode::JumpIfTrue),
            0x43 => Ok(OpCode::JumpIfFalseNoPop),
            0x44 => Ok(OpCode::JumpIfTrueNoPop),
            0x45 => Ok(OpCode::Loop),
            0x50 => Ok(OpCode::Call),
            0x51 => Ok(OpCode::TailCall),
            0x52 => Ok(OpCode::Return),
            0x54 => Ok(OpCode::CallMethod),
            0x55 => Ok(OpCode::DefineMethod),
            0x56 => Ok(OpCode::CallStatic),
            0x58 => Ok(OpCode::Closure),
            0x60 => Ok(OpCode::Tuple),
            0x61 => Ok(OpCode::Array),
            0x62 => Ok(OpCode::Struct),
            0x63 => Ok(OpCode::Enum),
            0x64 => Ok(OpCode::Range),
            0x68 => Ok(OpCode::GetField),
            0x69 => Ok(OpCode::SetField),
            0x6A => Ok(OpCode::GetFieldNamed),
            0x6B => Ok(OpCode::SetFieldNamed),
            0x70 => Ok(OpCode::Index),
            0x71 => Ok(OpCode::IndexSet),
            0x72 => Ok(OpCode::Len),
            0x73 => Ok(OpCode::Slice),
            0x80 => Ok(OpCode::MatchEnum),
            0x81 => Ok(OpCode::TryUnwrap),
            0x90 => Ok(OpCode::Iter),
            0x91 => Ok(OpCode::IterNext),
            0xA0 => Ok(OpCode::MakeRef),
            0xA1 => Ok(OpCode::Deref),
            0xA2 => Ok(OpCode::DerefStore),
            0xF0 => Ok(OpCode::NativeCall),
            0xF8 => Ok(OpCode::Print),
            0xF9 => Ok(OpCode::Panic),
            0xFA => Ok(OpCode::Assert),
            0xFE => Ok(OpCode::Nop),
            0xFF => Ok(OpCode::Halt),
            _ => Err(()),
        }
    }
}

/// A constant value in the constant pool.
#[derive(Debug, Clone, PartialEq)]
pub enum Constant {
    /// Integer constant
    Int(i64),
    /// Floating-point constant
    Float(f64),
    /// String constant
    String(String),
    /// Function prototype
    Function(Box<FunctionProto>),
    /// Struct type info (name, field names in order)
    StructInfo { name: String, fields: Vec<String> },
    /// Enum type info (name, variant names in declaration order)
    EnumInfo { name: String, variants: Vec<(String, usize)> }, // (variant_name, field_count)
}

/// Information about an upvalue capture.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct UpvalueInfo {
    /// Index in the enclosing scope
    pub index: u8,
    /// Whether this captures a local (true) or another upvalue (false)
    pub is_local: bool,
}

/// A function prototype (compiled function).
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionProto {
    /// Function name (for debugging)
    pub name: Option<String>,
    /// Number of parameters (total, including ones with defaults)
    pub arity: u8,
    /// Minimum number of required arguments (params without defaults)
    pub min_arity: u8,
    /// The bytecode chunk
    pub chunk: Chunk,
    /// Upvalue information for closure creation
    pub upvalues: Vec<UpvalueInfo>,
    /// Default value indices into chunk.constants for optional parameters
    /// defaults[0] is for the first parameter with a default, etc.
    pub defaults: Vec<u16>,
}

/// A bytecode chunk (compiled code).
#[derive(Debug, Clone, Default, PartialEq)]
pub struct Chunk {
    /// The bytecode
    pub code: Vec<u8>,
    /// Constant pool
    pub constants: Vec<Constant>,
    /// Line number information: (bytecode_offset, line_number)
    pub lines: Vec<(u32, u32)>,
}

impl Chunk {
    /// Create a new empty chunk.
    pub fn new() -> Self {
        Self::default()
    }

    /// Write a byte to the chunk.
    pub fn write(&mut self, byte: u8, line: u32) {
        self.code.push(byte);
        // Compress line info: only store when line changes
        if self.lines.is_empty() || self.lines.last().unwrap().1 != line {
            self.lines.push((self.code.len() as u32 - 1, line));
        }
    }

    /// Write an opcode to the chunk.
    pub fn write_op(&mut self, op: OpCode, line: u32) {
        self.write(op as u8, line);
    }

    /// Write a u16 value (little-endian).
    pub fn write_u16(&mut self, value: u16, line: u32) {
        self.write((value & 0xFF) as u8, line);
        self.write((value >> 8) as u8, line);
    }

    /// Write an i16 value (little-endian).
    pub fn write_i16(&mut self, value: i16, line: u32) {
        self.write_u16(value as u16, line);
    }

    /// Add a constant and return its index.
    pub fn add_constant(&mut self, constant: Constant) -> u16 {
        // Check for existing constant
        for (i, c) in self.constants.iter().enumerate() {
            if c == &constant {
                return i as u16;
            }
        }
        self.constants.push(constant);
        (self.constants.len() - 1) as u16
    }

    /// Get the current code length.
    pub fn len(&self) -> usize {
        self.code.len()
    }

    /// Check if the chunk is empty.
    pub fn is_empty(&self) -> bool {
        self.code.is_empty()
    }

    /// Get the line number for a bytecode offset.
    pub fn get_line(&self, offset: usize) -> u32 {
        let offset = offset as u32;
        for &(code_offset, line) in self.lines.iter().rev() {
            if code_offset <= offset {
                return line;
            }
        }
        0
    }

    /// Patch a u16 at the given offset.
    pub fn patch_u16(&mut self, offset: usize, value: u16) {
        self.code[offset] = (value & 0xFF) as u8;
        self.code[offset + 1] = (value >> 8) as u8;
    }

    /// Read a u16 at the given offset.
    pub fn read_u16(&self, offset: usize) -> u16 {
        self.code[offset] as u16 | ((self.code[offset + 1] as u16) << 8)
    }

    /// Disassemble the chunk.
    pub fn disassemble(&self, name: &str) -> String {
        let mut output = format!("== {} ==\n", name);
        let mut offset = 0;

        while offset < self.code.len() {
            output.push_str(&self.disassemble_instruction(offset));
            output.push('\n');
            offset += self.instruction_size(offset);
        }

        output
    }

    /// Disassemble a single instruction.
    pub fn disassemble_instruction(&self, offset: usize) -> String {
        let op = OpCode::try_from(self.code[offset]).unwrap_or(OpCode::Nop);
        let line = self.get_line(offset);

        let line_str = if offset > 0 && self.get_line(offset - 1) == line {
            "   |".to_string()
        } else {
            format!("{:4}", line)
        };

        match op {
            OpCode::Const => {
                let idx = self.read_u16(offset + 1);
                let constant = &self.constants[idx as usize];
                format!("{:04} {} {:16} {} ; {:?}", offset, line_str, op.name(), idx, constant)
            }
            OpCode::ConstSmall => {
                let idx = self.code[offset + 1];
                let constant = &self.constants[idx as usize];
                format!("{:04} {} {:16} {} ; {:?}", offset, line_str, op.name(), idx, constant)
            }
            OpCode::LoadLocal | OpCode::StoreLocal | OpCode::LoadGlobal | OpCode::StoreGlobal => {
                let idx = self.read_u16(offset + 1);
                format!("{:04} {} {:16} {}", offset, line_str, op.name(), idx)
            }
            OpCode::LoadLocalSmall | OpCode::StoreLocalSmall | OpCode::LoadUpvalue | OpCode::StoreUpvalue => {
                let idx = self.code[offset + 1];
                format!("{:04} {} {:16} {}", offset, line_str, op.name(), idx)
            }
            OpCode::Jump | OpCode::JumpIfFalse | OpCode::JumpIfTrue | OpCode::JumpIfFalseNoPop | OpCode::JumpIfTrueNoPop => {
                let jump = self.read_u16(offset + 1) as i16;
                let target = (offset as i32 + 3 + jump as i32) as usize;
                format!("{:04} {} {:16} {} -> {}", offset, line_str, op.name(), jump, target)
            }
            OpCode::Loop => {
                let jump = self.read_u16(offset + 1);
                let target = offset + 3 - jump as usize;
                format!("{:04} {} {:16} {} -> {}", offset, line_str, op.name(), jump, target)
            }
            OpCode::Call | OpCode::TailCall => {
                let argc = self.code[offset + 1];
                format!("{:04} {} {:16} {} args", offset, line_str, op.name(), argc)
            }
            OpCode::Closure => {
                let idx = self.read_u16(offset + 1);
                let upvalues = self.code[offset + 3];
                format!("{:04} {} {:16} fn={} upvalues={}", offset, line_str, op.name(), idx, upvalues)
            }
            OpCode::Tuple | OpCode::GetField | OpCode::SetField => {
                let count = self.code[offset + 1];
                format!("{:04} {} {:16} {}", offset, line_str, op.name(), count)
            }
            OpCode::Range => {
                let inclusive = self.code[offset + 1];
                format!("{:04} {} {:16} {}", offset, line_str, op.name(), if inclusive != 0 { "inclusive" } else { "exclusive" })
            }
            OpCode::Array => {
                let count = self.read_u16(offset + 1);
                format!("{:04} {} {:16} {}", offset, line_str, op.name(), count)
            }
            OpCode::PopN => {
                let count = self.code[offset + 1];
                format!("{:04} {} {:16} {}", offset, line_str, op.name(), count)
            }
            OpCode::NativeCall => {
                let idx = self.read_u16(offset + 1);
                format!("{:04} {} {:16} {}", offset, line_str, op.name(), idx)
            }
            OpCode::GetFieldNamed | OpCode::SetFieldNamed => {
                let idx = self.read_u16(offset + 1);
                let field_name = match &self.constants.get(idx as usize) {
                    Some(Constant::String(s)) => s.as_str(),
                    _ => "?",
                };
                format!("{:04} {} {:16} {} ({})", offset, line_str, op.name(), idx, field_name)
            }
            OpCode::Struct => {
                let idx = self.read_u16(offset + 1);
                let count = self.code[offset + 3];
                let struct_name = match &self.constants.get(idx as usize) {
                    Some(Constant::StructInfo { name, .. }) => name.as_str(),
                    _ => "?",
                };
                format!("{:04} {} {:16} {} ({}) fields={}", offset, line_str, op.name(), idx, struct_name, count)
            }
            _ => format!("{:04} {} {:16}", offset, line_str, op.name()),
        }
    }

    /// Get the size of an instruction at the given offset.
    fn instruction_size(&self, offset: usize) -> usize {
        let op = OpCode::try_from(self.code[offset]).unwrap_or(OpCode::Nop);
        op.instruction_size()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_chunk_write() {
        let mut chunk = Chunk::new();
        chunk.write_op(OpCode::Unit, 1);
        chunk.write_op(OpCode::Return, 1);
        assert_eq!(chunk.code, vec![OpCode::Unit as u8, OpCode::Return as u8]);
    }

    #[test]
    fn test_constant_dedup() {
        let mut chunk = Chunk::new();
        let idx1 = chunk.add_constant(Constant::Int(42));
        let idx2 = chunk.add_constant(Constant::Int(42));
        assert_eq!(idx1, idx2);
    }

    #[test]
    fn test_disassemble() {
        let mut chunk = Chunk::new();
        chunk.add_constant(Constant::Int(42));
        chunk.write_op(OpCode::ConstSmall, 1);
        chunk.write(0, 1);
        chunk.write_op(OpCode::Return, 1);

        let output = chunk.disassemble("test");
        assert!(output.contains("CONST_SMALL"));
        assert!(output.contains("RETURN"));
    }
}