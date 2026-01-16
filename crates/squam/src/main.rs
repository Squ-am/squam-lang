use std::fs;
use std::path::PathBuf;
use std::process::ExitCode;

use ariadne::{Color, Label, Report, ReportKind, Source};
use clap::{Parser, Subcommand};
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;

use squam_compiler::{CompileError, Compiler};
use squam_parser::Parser as SquamParser;
use squam_stdlib::register_stdlib;
use squam_types::{TypeChecker, TypeError, TypeId};
use squam_vm::{RuntimeError, Value, VM};

/// Squam - A Rust-like embedded scripting language
#[derive(Parser)]
#[command(name = "squam")]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Run a Squam source file
    Run {
        /// Path to the Squam source file
        file: PathBuf,

        /// Arguments to pass to the program
        #[arg(trailing_var_arg = true)]
        args: Vec<String>,
    },

    /// Start an interactive REPL
    Repl,

    /// Check a file for errors without running
    Check {
        /// Path to the Squam source file
        file: PathBuf,
    },

    /// Disassemble a file to show bytecode
    Disasm {
        /// Path to the Squam source file
        file: PathBuf,
    },
}

/// Register stdlib function signatures with the type checker.
/// Uses the `any` type for dynamically-typed functions.
fn register_stdlib_types(checker: &mut TypeChecker) {
    let any = TypeId::ANY;
    let unit = TypeId::UNIT;
    let bool_ty = TypeId::BOOL;
    let i64_ty = TypeId::I64;
    let f64_ty = TypeId::F64;
    let string = TypeId::STRING;

    // I/O functions
    checker.register_extern_function("print", vec![any], unit);
    checker.register_extern_function("println", vec![any], unit);
    checker.register_extern_function("eprint", vec![any], unit);
    checker.register_extern_function("eprintln", vec![any], unit);
    checker.register_extern_function("debug", vec![any], string);
    checker.register_extern_function("debug_print", vec![any], unit);
    checker.register_extern_function("dbg", vec![any], any);
    checker.register_extern_function("read_line", vec![], string);
    checker.register_extern_function("input", vec![string], string);
    checker.register_extern_function("format", vec![string, any], string);

    // Assertions
    checker.register_extern_function("assert", vec![bool_ty], unit);
    checker.register_extern_function("assert_eq", vec![any, any], unit);
    checker.register_extern_function("panic", vec![string], TypeId::NEVER);
    checker.register_extern_function("unreachable", vec![], TypeId::NEVER);
    checker.register_extern_function("todo", vec![], TypeId::NEVER);

    // File I/O
    checker.register_extern_function("read_file", vec![string], string);
    checker.register_extern_function("write_file", vec![string, string], unit);
    checker.register_extern_function("append_file", vec![string, string], unit);
    checker.register_extern_function("file_exists", vec![string], bool_ty);
    checker.register_extern_function("read_lines", vec![string], any); // returns array

    // Environment
    checker.register_extern_function("env_var", vec![string], string);
    checker.register_extern_function("env_var_or", vec![string, string], string);
    checker.register_extern_function("args", vec![], any); // returns array

    // Math functions
    checker.register_extern_function("abs", vec![i64_ty], i64_ty);
    checker.register_extern_function("min", vec![i64_ty, i64_ty], i64_ty);
    checker.register_extern_function("max", vec![i64_ty, i64_ty], i64_ty);
    checker.register_extern_function("clamp", vec![i64_ty, i64_ty, i64_ty], i64_ty);
    checker.register_extern_function("sqrt", vec![f64_ty], f64_ty);
    checker.register_extern_function("pow", vec![f64_ty, f64_ty], f64_ty);
    checker.register_extern_function("floor", vec![f64_ty], f64_ty);
    checker.register_extern_function("ceil", vec![f64_ty], f64_ty);
    checker.register_extern_function("round", vec![f64_ty], f64_ty);
    checker.register_extern_function("sin", vec![f64_ty], f64_ty);
    checker.register_extern_function("cos", vec![f64_ty], f64_ty);
    checker.register_extern_function("tan", vec![f64_ty], f64_ty);
    checker.register_extern_function("log", vec![f64_ty], f64_ty);
    checker.register_extern_function("log10", vec![f64_ty], f64_ty);
    checker.register_extern_function("exp", vec![f64_ty], f64_ty);
    checker.register_extern_function("random", vec![], f64_ty);
    checker.register_extern_function("random_int", vec![i64_ty, i64_ty], i64_ty);

    // String functions
    checker.register_extern_function("str_len", vec![string], i64_ty);
    checker.register_extern_function("str_concat", vec![string, string], string);
    checker.register_extern_function("str_slice", vec![string, i64_ty, i64_ty], string);
    checker.register_extern_function("str_contains", vec![string, string], bool_ty);
    checker.register_extern_function("str_starts_with", vec![string, string], bool_ty);
    checker.register_extern_function("str_ends_with", vec![string, string], bool_ty);
    checker.register_extern_function("str_index_of", vec![string, string], i64_ty);
    checker.register_extern_function("str_replace", vec![string, string, string], string);
    checker.register_extern_function("str_to_upper", vec![string], string);
    checker.register_extern_function("str_to_lower", vec![string], string);
    checker.register_extern_function("str_trim", vec![string], string);
    checker.register_extern_function("str_split", vec![string, string], any); // returns array
    checker.register_extern_function("str_chars", vec![string], any); // returns array
    checker.register_extern_function("str_repeat", vec![string, i64_ty], string);
    checker.register_extern_function("str_reverse", vec![string], string);
    checker.register_extern_function("str_pad_left", vec![string, i64_ty, string], string);
    checker.register_extern_function("str_pad_right", vec![string, i64_ty, string], string);
    checker.register_extern_function("char_at", vec![string, i64_ty], string);
    checker.register_extern_function("parse_int", vec![string], any); // returns Option
    checker.register_extern_function("parse_float", vec![string], any); // returns Option
    checker.register_extern_function("to_string", vec![any], string);

    // Array functions
    checker.register_extern_function("arr_len", vec![any], i64_ty);
    checker.register_extern_function("arr_push", vec![any, any], unit);
    checker.register_extern_function("arr_pop", vec![any], any);
    checker.register_extern_function("arr_get", vec![any, i64_ty], any);
    checker.register_extern_function("arr_set", vec![any, i64_ty, any], unit);
    checker.register_extern_function("arr_slice", vec![any, i64_ty, i64_ty], any);
    checker.register_extern_function("arr_concat", vec![any, any], any);
    checker.register_extern_function("arr_contains", vec![any, any], bool_ty);
    checker.register_extern_function("arr_index_of", vec![any, any], i64_ty);
    checker.register_extern_function("arr_reverse", vec![any], any);
    checker.register_extern_function("arr_sort", vec![any], any);
    checker.register_extern_function("arr_join", vec![any, string], string);
    checker.register_extern_function("arr_clone", vec![any], any);
    checker.register_extern_function("arr_clear", vec![any], unit);
    checker.register_extern_function("arr_is_empty", vec![any], bool_ty);
    checker.register_extern_function("arr_first", vec![any], any);
    checker.register_extern_function("arr_last", vec![any], any);
    checker.register_extern_function("range", vec![i64_ty, i64_ty], any);
    checker.register_extern_function("range_inclusive", vec![i64_ty, i64_ty], any);

    // Option/Result functions
    checker.register_extern_function("Some", vec![any], any);
    checker.register_extern_function("None", vec![], any);
    checker.register_extern_function("Ok", vec![any], any);
    checker.register_extern_function("Err", vec![any], any);
    checker.register_extern_function("is_some", vec![any], bool_ty);
    checker.register_extern_function("is_none", vec![any], bool_ty);
    checker.register_extern_function("is_ok", vec![any], bool_ty);
    checker.register_extern_function("is_err", vec![any], bool_ty);
    checker.register_extern_function("unwrap", vec![any], any);
    checker.register_extern_function("unwrap_or", vec![any, any], any);
    checker.register_extern_function("unwrap_ok", vec![any], any);
    checker.register_extern_function("unwrap_err", vec![any], any);

    // HashMap functions
    checker.register_extern_function("hashmap_new", vec![], any);
    checker.register_extern_function("hashmap_insert", vec![any, any, any], unit);
    checker.register_extern_function("hashmap_get", vec![any, any], any);
    checker.register_extern_function("hashmap_remove", vec![any, any], any);
    checker.register_extern_function("hashmap_contains", vec![any, any], bool_ty);
    checker.register_extern_function("hashmap_len", vec![any], i64_ty);
    checker.register_extern_function("hashmap_keys", vec![any], any);
    checker.register_extern_function("hashmap_values", vec![any], any);
    checker.register_extern_function("hashmap_clear", vec![any], unit);

    // HashSet functions
    checker.register_extern_function("hashset_new", vec![], any);
    checker.register_extern_function("hashset_insert", vec![any, any], bool_ty);
    checker.register_extern_function("hashset_remove", vec![any, any], bool_ty);
    checker.register_extern_function("hashset_contains", vec![any, any], bool_ty);
    checker.register_extern_function("hashset_len", vec![any], i64_ty);
    checker.register_extern_function("hashset_clear", vec![any], unit);

    // Iterator functions
    checker.register_extern_function("iter", vec![any], any);
    checker.register_extern_function("iter_next", vec![any], any);
    checker.register_extern_function("iter_collect", vec![any], any);
    checker.register_extern_function("iter_map", vec![any, any], any);
    checker.register_extern_function("iter_filter", vec![any, any], any);
    checker.register_extern_function("iter_take", vec![any, i64_ty], any);
    checker.register_extern_function("iter_skip", vec![any, i64_ty], any);
    checker.register_extern_function("iter_enumerate", vec![any], any);
    checker.register_extern_function("iter_zip", vec![any, any], any);
    checker.register_extern_function("iter_chain", vec![any, any], any);
    checker.register_extern_function("iter_fold", vec![any, any, any], any);
    checker.register_extern_function("iter_reduce", vec![any, any], any);
    checker.register_extern_function("iter_sum", vec![any], any);
    checker.register_extern_function("iter_count", vec![any], i64_ty);
    checker.register_extern_function("iter_any", vec![any, any], bool_ty);
    checker.register_extern_function("iter_all", vec![any, any], bool_ty);
    checker.register_extern_function("iter_find", vec![any, any], any);
    checker.register_extern_function("iter_position", vec![any, any], any);

    // Box functions
    checker.register_extern_function("box_new", vec![any], any);
    checker.register_extern_function("box_get", vec![any], any);
    checker.register_extern_function("box_set", vec![any, any], unit);

    // TCP Networking
    checker.register_extern_function("tcp_connect", vec![string, i64_ty], any);
    checker.register_extern_function("tcp_connect_timeout", vec![string, i64_ty, i64_ty], any);
    checker.register_extern_function("tcp_listen", vec![string, i64_ty], any);
    checker.register_extern_function("tcp_accept", vec![any], any);
    checker.register_extern_function("tcp_read", vec![any, i64_ty], string);
    checker.register_extern_function("tcp_read_line", vec![any], string);
    checker.register_extern_function("tcp_write", vec![any, string], i64_ty);
    checker.register_extern_function("tcp_write_line", vec![any, string], i64_ty);
    checker.register_extern_function("tcp_flush", vec![any], unit);
    checker.register_extern_function("tcp_close", vec![any], unit);
    checker.register_extern_function("tcp_set_timeout", vec![any, i64_ty, i64_ty], unit);
    checker.register_extern_function("tcp_peer_addr", vec![any], string);
    checker.register_extern_function("tcp_local_addr", vec![any], string);

    // Utility
    checker.register_extern_function("clone_value", vec![any], any);
    checker.register_extern_function("equals", vec![any, any], bool_ty);
    checker.register_extern_function("type_of", vec![any], string);
    checker.register_extern_function("sleep", vec![i64_ty], unit);
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Run { file, args: _ }) => run_file(&file),
        Some(Commands::Repl) => run_repl(),
        Some(Commands::Check { file }) => check_file(&file),
        Some(Commands::Disasm { file }) => disasm_file(&file),
        None => run_repl(), // Default to REPL if no command given
    }
}

/// Run a Squam source file
fn run_file(path: &PathBuf) -> ExitCode {
    let source = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", path.display(), e);
            return ExitCode::FAILURE;
        }
    };

    let filename = path.to_string_lossy().to_string();

    // Parse
    let mut parser = SquamParser::new(&source, 0);
    let module = parser.parse_module();

    if !parser.errors().is_empty() {
        report_parse_errors(&filename, &source, parser.errors());
        return ExitCode::FAILURE;
    }

    // Type check
    let mut checker = TypeChecker::new();
    register_stdlib_types(&mut checker);
    checker.check_module(&module);
    let type_errors = checker.take_errors();
    if !type_errors.is_empty() {
        report_type_errors(&filename, &source, &type_errors);
        return ExitCode::FAILURE;
    }

    // Get type annotations for the compiler (needed for generics monomorphization)
    let annotations = checker.take_annotations();

    // Compile
    let mut compiler = Compiler::new();
    compiler.set_type_annotations(annotations);
    let proto = match compiler.compile_module(&module) {
        Ok(p) => p,
        Err(e) => {
            report_compile_error(&filename, &source, &e);
            return ExitCode::FAILURE;
        }
    };

    // Run
    let mut vm = VM::new();
    register_stdlib(&mut vm);

    if let Err(e) = vm.run(&proto) {
        report_runtime_error(&filename, &e);
        return ExitCode::FAILURE;
    }

    // Call main if it exists
    if let Some(main) = vm.globals.get("main").cloned() {
        match vm.call(main, vec![]) {
            Ok(Value::Unit) => {}
            Ok(result) => println!("{}", result),
            Err(e) => {
                report_runtime_error(&filename, &e);
                return ExitCode::FAILURE;
            }
        }
    }

    ExitCode::SUCCESS
}

/// Check a file for errors without running
fn check_file(path: &PathBuf) -> ExitCode {
    let source = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", path.display(), e);
            return ExitCode::FAILURE;
        }
    };

    let filename = path.to_string_lossy().to_string();

    // Parse
    let mut parser = SquamParser::new(&source, 0);
    let module = parser.parse_module();

    if !parser.errors().is_empty() {
        report_parse_errors(&filename, &source, parser.errors());
        return ExitCode::FAILURE;
    }

    // Type check
    let mut checker = TypeChecker::new();
    register_stdlib_types(&mut checker);
    checker.check_module(&module);
    let type_errors = checker.take_errors();
    if !type_errors.is_empty() {
        report_type_errors(&filename, &source, &type_errors);
        return ExitCode::FAILURE;
    }

    // Get type annotations for the compiler (needed for generics monomorphization)
    let annotations = checker.take_annotations();

    // Compile (checks for compile-time errors)
    let mut compiler = Compiler::new();
    compiler.set_type_annotations(annotations);
    if let Err(e) = compiler.compile_module(&module) {
        report_compile_error(&filename, &source, &e);
        return ExitCode::FAILURE;
    }

    println!("No errors found in '{}'", path.display());
    ExitCode::SUCCESS
}

/// Disassemble a file to show bytecode
fn disasm_file(path: &PathBuf) -> ExitCode {
    let source = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Error reading file '{}': {}", path.display(), e);
            return ExitCode::FAILURE;
        }
    };

    let filename = path.to_string_lossy().to_string();

    // Parse
    let mut parser = SquamParser::new(&source, 0);
    let module = parser.parse_module();

    if !parser.errors().is_empty() {
        report_parse_errors(&filename, &source, parser.errors());
        return ExitCode::FAILURE;
    }

    // Type check
    let mut checker = TypeChecker::new();
    register_stdlib_types(&mut checker);
    checker.check_module(&module);
    let type_errors = checker.take_errors();
    if !type_errors.is_empty() {
        report_type_errors(&filename, &source, &type_errors);
        return ExitCode::FAILURE;
    }

    // Get type annotations for the compiler (needed for generics monomorphization)
    let annotations = checker.take_annotations();

    // Compile
    let mut compiler = Compiler::new();
    compiler.set_type_annotations(annotations);
    let proto = match compiler.compile_module(&module) {
        Ok(p) => p,
        Err(e) => {
            report_compile_error(&filename, &source, &e);
            return ExitCode::FAILURE;
        }
    };

    // Print disassembly
    println!("=== Module ===");
    println!("{}", proto.chunk.disassemble("module"));

    // Print function disassemblies
    for constant in &proto.chunk.constants {
        if let squam_compiler::Constant::Function(func) = constant {
            println!(
                "\n=== Function: {} ===",
                func.name.as_deref().unwrap_or("<anonymous>")
            );
            println!(
                "{}",
                func.chunk
                    .disassemble(func.name.as_deref().unwrap_or("<anonymous>"))
            );
        }
    }

    ExitCode::SUCCESS
}

/// Run the interactive REPL
fn run_repl() -> ExitCode {
    println!("Squam {} - Interactive REPL", env!("CARGO_PKG_VERSION"));
    println!("Type 'exit' or press Ctrl+D to quit, 'help' for commands.\n");

    let mut rl = match DefaultEditor::new() {
        Ok(rl) => rl,
        Err(e) => {
            eprintln!("Error initializing REPL: {}", e);
            return ExitCode::FAILURE;
        }
    };

    // Load history
    let history_path = dirs_history_path();
    if let Some(ref path) = history_path {
        let _ = rl.load_history(path);
    }

    let mut vm = VM::new();
    register_stdlib(&mut vm);

    loop {
        let readline = rl.readline(">>> ");
        match readline {
            Ok(line) => {
                let line = line.trim();

                if line.is_empty() {
                    continue;
                }

                let _ = rl.add_history_entry(line);

                // Handle special commands
                match line {
                    "exit" | "quit" => break,
                    "help" => {
                        print_repl_help();
                        continue;
                    }
                    "clear" => {
                        print!("\x1B[2J\x1B[1;1H"); // Clear screen
                        continue;
                    }
                    "reset" => {
                        vm = VM::new();
                        register_stdlib(&mut vm);
                        println!("VM state reset.");
                        continue;
                    }
                    _ => {}
                }

                // Try to evaluate as expression or statement
                eval_repl_input(&mut vm, line);
            }
            Err(ReadlineError::Interrupted) => {
                println!("^C");
                continue;
            }
            Err(ReadlineError::Eof) => {
                println!("Goodbye!");
                break;
            }
            Err(err) => {
                eprintln!("Error: {:?}", err);
                break;
            }
        }
    }

    // Save history
    if let Some(ref path) = history_path {
        let _ = rl.save_history(path);
    }

    ExitCode::SUCCESS
}

/// Evaluate input in the REPL
fn eval_repl_input(vm: &mut VM, input: &str) {
    // Wrap the input - if it looks like an expression, wrap it in a function to get the result
    let source = if is_likely_expression(input) {
        format!("fn __repl__() {{ {} }}", input)
    } else {
        input.to_string()
    };

    // Parse
    let mut parser = SquamParser::new(&source, 0);
    let module = parser.parse_module();

    if !parser.errors().is_empty() {
        for error in parser.errors() {
            eprintln!("Parse error: {}", error);
        }
        return;
    }

    // Type check
    let mut checker = TypeChecker::new();
    register_stdlib_types(&mut checker);
    checker.check_module(&module);
    let type_errors = checker.take_errors();
    if !type_errors.is_empty() {
        for error in &type_errors {
            eprintln!("Type error: {}", error);
        }
        return;
    }

    // Get type annotations for the compiler (needed for generics monomorphization)
    let annotations = checker.take_annotations();

    // Compile
    let mut compiler = Compiler::new();
    compiler.set_type_annotations(annotations);
    let proto = match compiler.compile_module(&module) {
        Ok(p) => p,
        Err(e) => {
            eprintln!("Compile error: {}", e);
            return;
        }
    };

    // Run
    if let Err(e) = vm.run(&proto) {
        eprintln!("Runtime error: {}", e);
        return;
    }

    // If we wrapped it as __repl__, call it and print the result
    if is_likely_expression(input) {
        if let Some(repl_fn) = vm.globals.remove("__repl__") {
            match vm.call(repl_fn, vec![]) {
                Ok(Value::Unit) => {} // Don't print unit
                Ok(result) => println!("{:?}", result),
                Err(e) => eprintln!("Runtime error: {}", e),
            }
        }
    }
}

/// Check if input looks like an expression (vs a statement/definition)
fn is_likely_expression(input: &str) -> bool {
    let input = input.trim();

    // If it starts with these keywords, it's likely a definition/statement
    if input.starts_with("fn ")
        || input.starts_with("let ")
        || input.starts_with("const ")
        || input.starts_with("struct ")
        || input.starts_with("enum ")
        || input.starts_with("type ")
        || input.starts_with("impl ")
        || input.starts_with("trait ")
        || input.starts_with("use ")
        || input.starts_with("mod ")
    {
        return false;
    }

    // Otherwise, treat it as an expression
    true
}

/// Get path for REPL history file
fn dirs_history_path() -> Option<PathBuf> {
    dirs::data_local_dir().map(|mut p| {
        p.push("squam");
        let _ = std::fs::create_dir_all(&p);
        p.push("history.txt");
        p
    })
}

/// Print REPL help
fn print_repl_help() {
    println!("Squam REPL Commands:");
    println!("  help     - Show this help message");
    println!("  exit     - Exit the REPL (also: quit, Ctrl+D)");
    println!("  clear    - Clear the screen");
    println!("  reset    - Reset VM state");
    println!();
    println!("Examples:");
    println!("  >>> 1 + 2 * 3");
    println!("  7");
    println!("  >>> let x = 42");
    println!("  >>> fn square(n: int) -> int {{ n * n }}");
    println!("  >>> square(5)");
    println!("  25");
}

// ---
// Error reporting with ariadne
// ---

fn report_parse_errors(filename: &str, source: &str, errors: &[squam_parser::ParseError]) {
    for error in errors {
        let span = error.span();
        let start = span.start as usize;
        let end = span.end as usize;

        Report::<(&str, std::ops::Range<usize>)>::build(ReportKind::Error, filename, start)
            .with_message("Parse error")
            .with_label(
                Label::new((filename, start..end))
                    .with_message(format!("{}", error))
                    .with_color(Color::Red),
            )
            .finish()
            .print((filename, Source::from(source)))
            .unwrap();
    }
}

fn report_compile_error(filename: &str, source: &str, error: &CompileError) {
    // For now, we don't have span info in compile errors, so just print the message
    Report::<(&str, std::ops::Range<usize>)>::build(ReportKind::Error, filename, 0)
        .with_message(format!("Compile error: {}", error))
        .finish()
        .print((filename, Source::from(source)))
        .unwrap();
}

fn report_runtime_error(filename: &str, error: &RuntimeError) {
    eprintln!("\x1b[1;31mRuntime error\x1b[0m in {}: {}", filename, error);
}

fn report_type_errors(filename: &str, source: &str, errors: &[TypeError]) {
    for error in errors {
        let span = error.span();
        let start = span.start as usize;
        let end = span.end as usize;

        Report::<(&str, std::ops::Range<usize>)>::build(ReportKind::Error, filename, start)
            .with_message("Type error")
            .with_label(
                Label::new((filename, start..end))
                    .with_message(format!("{}", error))
                    .with_color(Color::Red),
            )
            .finish()
            .print((filename, Source::from(source)))
            .unwrap();
    }
}
