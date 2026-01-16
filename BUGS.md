# Squam Language - Identified Issues

This document lists potential bugs and issues discovered during a comprehensive code review.

---

## 1. GC Never Actually Runs

**Status:** Confirmed
**Severity:** Medium
**Location:** `crates/squam-vm/src/vm.rs:133`

### Description

The `collect_garbage()` method is defined but never called anywhere in the codebase. The GC heap (`GcHeap`) has infrastructure for mark-sweep collection, but garbage collection is never triggered.

```rust
// Defined at vm.rs:133
pub fn collect_garbage(&mut self) {
    // ... collects garbage
    self.gc_heap.collect(&roots);
}
```

### Impact

If `MemoryMode::MarkSweep` is used (currently it's not - defaults to `RefCounting`), memory would never be reclaimed. With the default `RefCounting` mode, this is not currently a problem, but the GC infrastructure is dead code.

### Suggested Fix

Either:
- Remove the unused GC infrastructure, or
- Call `collect_garbage()` periodically during execution (e.g., after N allocations or when heap threshold is exceeded)

---

## 2. Range Iterator Overflow

**Status:** Confirmed
**Severity:** Low
**Location:** `crates/squam-vm/src/value.rs:201`

### Description

The `RangeIterator::next()` method increments `current` with `self.current += 1`, which will overflow (panic in debug, wrap in release) when iterating an inclusive range ending at `i64::MAX`.

```rust
// value.rs:192-206
impl SquamIterator for RangeIterator {
    fn next(&mut self) -> Option<Value> {
        let in_range = if self.inclusive {
            self.current <= self.end  // true when current == i64::MAX
        } else {
            self.current < self.end
        };
        if in_range {
            let val = self.current;
            self.current += 1;  // OVERFLOW when val == i64::MAX
            Some(Value::Int(val))
        }
        // ...
    }
}
```

### Impact

Code like `for i in 0..=i64::MAX { ... }` will panic or behave unexpectedly at the boundary.

### Suggested Fix

```rust
fn next(&mut self) -> Option<Value> {
    let in_range = if self.inclusive {
        self.current <= self.end
    } else {
        self.current < self.end
    };
    if in_range {
        let val = self.current;
        self.current = self.current.saturating_add(1);
        Some(Value::Int(val))
    } else {
        None
    }
}
```

---

## 3. Float NaN Sorting Produces Non-Deterministic Order

**Status:** Confirmed
**Severity:** Low
**Location:** `crates/squam-stdlib/src/vec.rs:10`

### Description

The `value_cmp` function uses `partial_cmp(...).unwrap_or(Ordering::Equal)` for floats, which means `NaN` values compare as equal to everything. This can produce inconsistent sort results.

```rust
// vec.rs:7-19
fn value_cmp(a: &Value, b: &Value, descending: bool) -> Ordering {
    let ord = match (a, b) {
        (Value::Int(a), Value::Int(b)) => a.cmp(b),
        (Value::Float(a), Value::Float(b)) => a.partial_cmp(b).unwrap_or(Ordering::Equal),
        // ...
    };
    // ...
}
```

### Impact

Arrays containing `NaN` may sort differently across runs or produce logically inconsistent results.

### Suggested Fix

Either:
- Document that NaN behavior is undefined for sorting, or
- Use `total_cmp()` (Rust 1.62+) which defines a total order including NaN:

```rust
(Value::Float(a), Value::Float(b)) => a.total_cmp(b),
```

---

## 4. Integer Overflow Inconsistency Between Optimizer and VM

**Status:** Confirmed
**Severity:** Low
**Location:**
- `crates/squam-compiler/src/optimizer.rs:92-94`
- `crates/squam-vm/src/vm.rs:592`

### Description

The optimizer uses wrapping arithmetic for constant folding, but the VM uses standard arithmetic that panics on overflow in debug builds.

```rust
// optimizer.rs:92-94 - uses wrapping
BinaryOp::Add => Some(Literal::Int(a.wrapping_add(*b))),
BinaryOp::Sub => Some(Literal::Int(a.wrapping_sub(*b))),
BinaryOp::Mul => Some(Literal::Int(a.wrapping_mul(*b))),

// vm.rs:592 - uses standard (panics in debug)
(Value::Int(a), Value::Int(b)) => Value::Int(a + b),
```

### Impact

- `let x = 9223372036854775807 + 1` at compile time yields `-9223372036854775808` (wrapped)
- The same expression at runtime panics in debug builds

This creates inconsistent semantics between compile-time and runtime evaluation.

### Suggested Fix

Choose one approach and apply consistently:
- **Wrapping (current optimizer behavior):** Use `wrapping_add` in VM
- **Checked (safer):** Return error on overflow in both places
- **Panic (current VM debug behavior):** Remove wrapping from optimizer

---

## Non-Issues (Investigated and Cleared)

### set_field Return Value
The return value of `StructInstance::set_field()` IS properly checked at `vm.rs:1455`. No bug here.

### Constant Pool Overflow
This was a bug (always-false comparison) but was fixed in a previous cleanup commit.

---

## Notes

- **LocalRef Stack Indices:** These store absolute stack positions. The compile-time borrow checker prevents escaping references, but runtime validation is minimal. Worth monitoring but not currently exploitable.

- **Array Index Overflow:** Negative index calculations use `as usize` which wraps, but results are bounds-checked via `.get()` returning `Option`, so this is safe.
