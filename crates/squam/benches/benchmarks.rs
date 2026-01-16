use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion};
use squam_compiler::Compiler;
use squam_lexer::Lexer;
use squam_parser::Parser;
use squam_vm::VM;

// ---
// Lexer Benchmarks
// ---

fn bench_lexer(c: &mut Criterion) {
    let source = r#"
        fn fibonacci(n: i64) -> i64 {
            if n <= 1 {
                n
            } else {
                fibonacci(n - 1) + fibonacci(n - 2)
            }
        }

        fn main() {
            let result = fibonacci(20);
            println(result);
        }
    "#;

    c.bench_function("lexer/fibonacci", |b| {
        b.iter(|| {
            let lexer = Lexer::new(black_box(source), 0);
            let tokens: Vec<_> = lexer.collect();
            black_box(tokens)
        })
    });

    // Benchmark with varying source sizes
    let mut group = c.benchmark_group("lexer/size");
    for size in [10, 100, 1000] {
        let large_source = "let x = 1 + 2 * 3 - 4 / 5;\n".repeat(size);
        group.bench_with_input(
            BenchmarkId::from_parameter(size),
            &large_source,
            |b, src| {
                b.iter(|| {
                    let lexer = Lexer::new(black_box(src), 0);
                    let tokens: Vec<_> = lexer.collect();
                    black_box(tokens)
                })
            },
        );
    }
    group.finish();
}

// ---
// Parser Benchmarks
// ---

fn bench_parser(c: &mut Criterion) {
    let source = r#"
        fn fibonacci(n: i64) -> i64 {
            if n <= 1 {
                n
            } else {
                fibonacci(n - 1) + fibonacci(n - 2)
            }
        }

        fn main() {
            let result = fibonacci(20);
            println(result);
        }
    "#;

    c.bench_function("parser/fibonacci", |b| {
        b.iter(|| {
            let mut parser = Parser::new(black_box(source), 0);
            let module = parser.parse_module();
            black_box(module)
        })
    });

    // Complex expression parsing
    let expr_source = "1 + 2 * 3 - 4 / 5 + 6 * (7 + 8) - 9";
    c.bench_function("parser/complex_expr", |b| {
        b.iter(|| {
            let mut parser = Parser::new(black_box(expr_source), 0);
            let expr = parser.parse_expression();
            black_box(expr)
        })
    });
}

// ---
// Compiler Benchmarks
// ---

fn bench_compiler(c: &mut Criterion) {
    let source = r#"
        fn fibonacci(n: i64) -> i64 {
            if n <= 1 {
                n
            } else {
                fibonacci(n - 1) + fibonacci(n - 2)
            }
        }

        fn main() {
            let result = fibonacci(20);
            println(result);
        }
    "#;

    c.bench_function("compiler/fibonacci", |b| {
        let mut parser = Parser::new(source, 0);
        let module = parser.parse_module();

        b.iter(|| {
            let mut compiler = Compiler::new();
            let proto = compiler.compile_module(black_box(&module));
            black_box(proto)
        })
    });

    // Benchmark with closures
    let closure_source = r#"
        fn make_adder(x: i64) -> fn(i64) -> i64 {
            |y| x + y
        }

        fn main() {
            let add5 = make_adder(5);
            add5(10)
        }
    "#;

    c.bench_function("compiler/closures", |b| {
        let mut parser = Parser::new(closure_source, 0);
        let module = parser.parse_module();

        b.iter(|| {
            let mut compiler = Compiler::new();
            let proto = compiler.compile_module(black_box(&module));
            black_box(proto)
        })
    });
}

// ---
// VM Benchmarks
// ---

fn bench_vm(c: &mut Criterion) {
    // Fibonacci benchmark
    let fib_source = r#"
        fn fibonacci(n: i64) -> i64 {
            if n <= 1 {
                n
            } else {
                fibonacci(n - 1) + fibonacci(n - 2)
            }
        }

        fn main() {
            fibonacci(15)
        }
    "#;

    let mut parser = Parser::new(fib_source, 0);
    let module = parser.parse_module();
    let mut compiler = Compiler::new();
    let fib_proto = compiler.compile_module(&module).unwrap();

    c.bench_function("vm/fibonacci_15", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            let result = vm.run(black_box(&fib_proto));
            black_box(result)
        })
    });

    // Loop benchmark
    let loop_source = r#"
        fn main() {
            let mut sum = 0;
            let mut i = 0;
            while i < 1000 {
                sum = sum + i;
                i = i + 1;
            }
            sum
        }
    "#;

    let mut parser = Parser::new(loop_source, 0);
    let module = parser.parse_module();
    let mut compiler = Compiler::new();
    let loop_proto = compiler.compile_module(&module).unwrap();

    c.bench_function("vm/loop_1000", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            let result = vm.run(black_box(&loop_proto));
            black_box(result)
        })
    });

    // Arithmetic benchmark
    let arith_source = r#"
        fn main() {
            let mut result = 0;
            let mut i = 0;
            while i < 100 {
                result = result + i * 2 - i / 2;
                i = i + 1;
            }
            result
        }
    "#;

    let mut parser = Parser::new(arith_source, 0);
    let module = parser.parse_module();
    let mut compiler = Compiler::new();
    let arith_proto = compiler.compile_module(&module).unwrap();

    c.bench_function("vm/arithmetic_100", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            let result = vm.run(black_box(&arith_proto));
            black_box(result)
        })
    });

    // Function call overhead benchmark
    let call_source = r#"
        fn add(a: i64, b: i64) -> i64 {
            a + b
        }

        fn main() {
            let mut sum = 0;
            let mut i = 0;
            while i < 100 {
                sum = add(sum, i);
                i = i + 1;
            }
            sum
        }
    "#;

    let mut parser = Parser::new(call_source, 0);
    let module = parser.parse_module();
    let mut compiler = Compiler::new();
    let call_proto = compiler.compile_module(&module).unwrap();

    c.bench_function("vm/function_calls_100", |b| {
        b.iter(|| {
            let mut vm = VM::new();
            let result = vm.run(black_box(&call_proto));
            black_box(result)
        })
    });
}

// ---
// End-to-End Benchmarks
// ---

fn bench_e2e(c: &mut Criterion) {
    let source = r#"
        fn fibonacci(n: i64) -> i64 {
            if n <= 1 {
                n
            } else {
                fibonacci(n - 1) + fibonacci(n - 2)
            }
        }

        fn main() {
            fibonacci(10)
        }
    "#;

    c.bench_function("e2e/fibonacci_10", |b| {
        b.iter(|| {
            // Lex
            let _lexer = Lexer::new(black_box(source), 0);

            // Parse
            let mut parser = Parser::new(source, 0);
            let module = parser.parse_module();

            // Compile
            let mut compiler = Compiler::new();
            let proto = compiler.compile_module(&module).unwrap();

            // Execute
            let mut vm = VM::new();
            let result = vm.run(&proto);
            black_box(result)
        })
    });
}

// ---
// Optimizer Benchmarks
// ---

fn bench_optimizer(c: &mut Criterion) {
    use squam_compiler::{ConstantFolder, DeadCodeEliminator, PeepholeOptimizer};
    use squam_lexer::Span;
    use squam_parser::{BinaryOp, Expr, ExprKind, Literal};

    // Constant folding benchmark
    c.bench_function("optimizer/constant_fold", |b| {
        // Create a nested constant expression: ((1 + 2) * (3 + 4)) + ((5 + 6) * (7 + 8))
        let span = Span::new(0, 0, 0);
        let make_int = |n: i64| Expr {
            kind: ExprKind::Literal(Literal::Int(n)),
            span,
        };
        let make_add = |left: Expr, right: Expr| Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Add,
                left: Box::new(left),
                right: Box::new(right),
            },
            span,
        };
        let make_mul = |left: Expr, right: Expr| Expr {
            kind: ExprKind::Binary {
                op: BinaryOp::Mul,
                left: Box::new(left),
                right: Box::new(right),
            },
            span,
        };

        let expr = make_add(
            make_mul(
                make_add(make_int(1), make_int(2)),
                make_add(make_int(3), make_int(4)),
            ),
            make_mul(
                make_add(make_int(5), make_int(6)),
                make_add(make_int(7), make_int(8)),
            ),
        );

        b.iter(|| {
            let result = ConstantFolder::fold(black_box(&expr));
            black_box(result)
        })
    });

    // Peephole optimizer benchmark
    c.bench_function("optimizer/peephole", |b| {
        // Create bytecode with optimization opportunities
        let source = r#"
            fn main() {
                let x = 1 + 2;
                let y = x + 3;
                y
            }
        "#;

        let mut parser = Parser::new(source, 0);
        let module = parser.parse_module();
        let mut compiler = Compiler::new();
        let proto = compiler.compile_module(&module).unwrap();

        b.iter(|| {
            let mut chunk = proto.chunk.clone();
            let stats = PeepholeOptimizer::optimize(black_box(&mut chunk));
            black_box((chunk, stats))
        })
    });

    // Dead code elimination benchmark
    c.bench_function("optimizer/dead_code", |b| {
        let source = r#"
            fn main() {
                let x = 1;
                if true {
                    x + 1
                } else {
                    x + 2
                }
            }
        "#;

        let mut parser = Parser::new(source, 0);
        let module = parser.parse_module();
        let mut compiler = Compiler::new();
        let proto = compiler.compile_module(&module).unwrap();

        b.iter(|| {
            let mut chunk = proto.chunk.clone();
            let stats = DeadCodeEliminator::eliminate(black_box(&mut chunk));
            black_box((chunk, stats))
        })
    });
}

// ---
// Inline Cache Benchmarks
// ---

fn bench_inline_cache(c: &mut Criterion) {
    use squam_vm::{CallSiteId, InlineCacheManager, TypeId};

    c.bench_function("inline_cache/lookup_hit", |b| {
        let mut manager = InlineCacheManager::new();
        let site = CallSiteId::from_offset(100);
        let type_id = TypeId::from_name("Point");
        manager.update_method(site, type_id, 42);

        b.iter(|| {
            let result = manager.lookup_method(black_box(site), black_box(type_id));
            black_box(result)
        })
    });

    c.bench_function("inline_cache/lookup_miss", |b| {
        let mut manager = InlineCacheManager::new();
        let site = CallSiteId::from_offset(100);
        let type_id = TypeId::from_name("Point");
        let other_type = TypeId::from_name("Rectangle");
        manager.update_method(site, type_id, 42);

        b.iter(|| {
            let result = manager.lookup_method(black_box(site), black_box(other_type));
            black_box(result)
        })
    });

    c.bench_function("inline_cache/update", |b| {
        let site = CallSiteId::from_offset(100);
        let type_id = TypeId::from_name("Point");

        b.iter(|| {
            let mut manager = InlineCacheManager::new();
            manager.update_method(black_box(site), black_box(type_id), 42);
            black_box(manager)
        })
    });
}

// ---
// GC Benchmarks
// ---

fn bench_gc(c: &mut Criterion) {
    use squam_vm::{GcHeap, Trace, Tracer};

    #[derive(Debug)]
    struct TestValue(i64);

    impl Trace for TestValue {
        fn trace(&self, _tracer: &mut Tracer) {}
    }

    c.bench_function("gc/alloc", |b| {
        b.iter(|| {
            let heap = GcHeap::new();
            for i in 0..100 {
                let _gc = heap.alloc(TestValue(i));
            }
            black_box(heap)
        })
    });

    c.bench_function("gc/alloc_and_collect", |b| {
        b.iter(|| {
            let heap = GcHeap::new();
            for i in 0..100 {
                let _gc = heap.alloc(TestValue(i));
            }
            heap.collect(&[]);
            black_box(heap)
        })
    });
}

criterion_group!(
    benches,
    bench_lexer,
    bench_parser,
    bench_compiler,
    bench_vm,
    bench_e2e,
    bench_optimizer,
    bench_inline_cache,
    bench_gc,
);

criterion_main!(benches);
