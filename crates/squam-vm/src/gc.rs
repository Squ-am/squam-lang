use std::cell::{Cell, RefCell};
use std::collections::HashMap;
use std::fmt;
use std::ops::Deref;
use std::ptr::NonNull;

// ---
// GC Configuration
// ---

/// Configuration for the garbage collector.
#[derive(Debug, Clone)]
pub struct GcConfig {
    /// Initial heap size before first collection (in bytes, approximate)
    pub initial_threshold: usize,
    /// Growth factor for heap threshold after collection
    pub growth_factor: f64,
    /// Minimum heap threshold
    pub min_threshold: usize,
    /// Enable stress testing (collect on every allocation)
    pub stress_test: bool,
}

impl Default for GcConfig {
    fn default() -> Self {
        Self {
            initial_threshold: 1024 * 1024, // 1 MB
            growth_factor: 2.0,
            min_threshold: 1024 * 64, // 64 KB
            stress_test: false,
        }
    }
}

// ---
// GC Object Header
// ---

/// Header for GC-managed objects.
#[derive(Debug)]
struct GcHeader {
    /// Mark bit for tri-color marking
    marked: Cell<bool>,
    /// Size of this allocation (for memory tracking)
    size: usize,
    /// Pointer to next object in the allocation list
    next: Cell<Option<NonNull<GcBox<dyn Trace>>>>,
}

/// A GC-managed box containing a value.
#[repr(C)]
struct GcBox<T: Trace + ?Sized> {
    header: GcHeader,
    value: T,
}

impl<T: Trace> GcBox<T> {
    fn new(value: T, size: usize) -> Self {
        Self {
            header: GcHeader {
                marked: Cell::new(false),
                size,
                next: Cell::new(None),
            },
            value,
        }
    }
}

// ---
// Gc Smart Pointer
// ---

/// A garbage-collected pointer to a value of type T.
///
/// This is similar to `Rc<T>` but uses tracing garbage collection
/// instead of reference counting.
pub struct Gc<T: Trace + ?Sized> {
    ptr: NonNull<GcBox<T>>,
}

impl<T: Trace + ?Sized> Gc<T> {
    /// Get a reference to the inner value.
    #[inline]
    pub fn as_ref(&self) -> &T {
        unsafe { &self.ptr.as_ref().value }
    }

    /// Check if this Gc points to the same allocation as another.
    #[inline]
    pub fn ptr_eq(this: &Gc<T>, other: &Gc<T>) -> bool {
        std::ptr::addr_eq(this.ptr.as_ptr(), other.ptr.as_ptr())
    }
}

impl<T: Trace + ?Sized> Clone for Gc<T> {
    fn clone(&self) -> Self {
        Gc { ptr: self.ptr }
    }
}

impl<T: Trace + ?Sized> Copy for Gc<T> {}

impl<T: Trace + ?Sized> Deref for Gc<T> {
    type Target = T;

    fn deref(&self) -> &T {
        self.as_ref()
    }
}

impl<T: Trace + ?Sized + fmt::Debug> fmt::Debug for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self.as_ref(), f)
    }
}

impl<T: Trace + ?Sized + fmt::Display> fmt::Display for Gc<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_ref(), f)
    }
}

impl<T: Trace + PartialEq> PartialEq for Gc<T> {
    fn eq(&self, other: &Self) -> bool {
        **self == **other
    }
}

impl<T: Trace + Eq> Eq for Gc<T> {}

impl<T: Trace + std::hash::Hash> std::hash::Hash for Gc<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        (**self).hash(state)
    }
}

// ---
// GcRefCell - Interior mutability for GC values
// ---

/// A garbage-collected reference cell for interior mutability.
pub struct GcRefCell<T: Trace> {
    inner: RefCell<T>,
}

impl<T: Trace> GcRefCell<T> {
    /// Create a new GcRefCell.
    pub fn new(value: T) -> Self {
        Self {
            inner: RefCell::new(value),
        }
    }

    /// Borrow the inner value immutably.
    pub fn borrow(&self) -> std::cell::Ref<'_, T> {
        self.inner.borrow()
    }

    /// Borrow the inner value mutably.
    pub fn borrow_mut(&self) -> std::cell::RefMut<'_, T> {
        self.inner.borrow_mut()
    }
}

impl<T: Trace + fmt::Debug> fmt::Debug for GcRefCell<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.inner.try_borrow() {
            Ok(v) => fmt::Debug::fmt(&*v, f),
            Err(_) => write!(f, "<borrowed>"),
        }
    }
}

// ---
// Trace Trait
// ---

/// Trait for types that can be traced by the garbage collector.
///
/// Implementors must mark all Gc pointers they contain.
pub trait Trace {
    /// Trace all Gc pointers contained in this value.
    fn trace(&self, tracer: &mut Tracer);
}

/// Tracer used during the mark phase.
pub struct Tracer<'a> {
    #[allow(dead_code)]
    heap: &'a GcHeap,
}

impl<'a> Tracer<'a> {
    /// Mark a Gc pointer as reachable.
    pub fn mark<T: Trace + ?Sized>(&mut self, gc: &Gc<T>) {
        unsafe {
            let header = &gc.ptr.as_ref().header;
            if !header.marked.get() {
                header.marked.set(true);
                // Recursively trace the value
                gc.ptr.as_ref().value.trace(self);
            }
        }
    }
}

// ---
// Trace implementations for primitive types
// ---

impl Trace for () {
    fn trace(&self, _tracer: &mut Tracer) {}
}

impl Trace for bool {
    fn trace(&self, _tracer: &mut Tracer) {}
}

impl Trace for i64 {
    fn trace(&self, _tracer: &mut Tracer) {}
}

impl Trace for f64 {
    fn trace(&self, _tracer: &mut Tracer) {}
}

impl Trace for String {
    fn trace(&self, _tracer: &mut Tracer) {}
}

impl<T: Trace> Trace for Vec<T> {
    fn trace(&self, tracer: &mut Tracer) {
        for item in self {
            item.trace(tracer);
        }
    }
}

impl<T: Trace> Trace for Option<T> {
    fn trace(&self, tracer: &mut Tracer) {
        if let Some(v) = self {
            v.trace(tracer);
        }
    }
}

impl<T: Trace> Trace for RefCell<T> {
    fn trace(&self, tracer: &mut Tracer) {
        self.borrow().trace(tracer);
    }
}

impl<T: Trace> Trace for GcRefCell<T> {
    fn trace(&self, tracer: &mut Tracer) {
        self.borrow().trace(tracer);
    }
}

impl<K: Trace, V: Trace> Trace for HashMap<K, V> {
    fn trace(&self, tracer: &mut Tracer) {
        for (k, v) in self {
            k.trace(tracer);
            v.trace(tracer);
        }
    }
}

impl<T: Trace + ?Sized> Trace for Gc<T> {
    fn trace(&self, tracer: &mut Tracer) {
        tracer.mark(self);
    }
}

// ---
// GC Heap
// ---

/// The garbage-collected heap.
pub struct GcHeap {
    /// Head of the linked list of all allocations
    objects: Cell<Option<NonNull<GcBox<dyn Trace>>>>,
    /// Total bytes allocated
    bytes_allocated: Cell<usize>,
    /// Threshold for triggering collection
    threshold: Cell<usize>,
    /// Configuration
    config: GcConfig,
    /// Statistics
    stats: RefCell<GcStats>,
}

/// GC statistics.
#[derive(Debug, Default, Clone)]
pub struct GcStats {
    /// Total number of allocations
    pub total_allocations: usize,
    /// Total bytes allocated (cumulative)
    pub total_bytes_allocated: usize,
    /// Number of collections performed
    pub collections: usize,
    /// Total bytes freed
    pub total_bytes_freed: usize,
    /// Current live objects
    pub live_objects: usize,
    /// Current live bytes
    pub live_bytes: usize,
}

impl GcHeap {
    /// Create a new GC heap with default configuration.
    pub fn new() -> Self {
        Self::with_config(GcConfig::default())
    }

    /// Create a new GC heap with custom configuration.
    pub fn with_config(config: GcConfig) -> Self {
        Self {
            objects: Cell::new(None),
            bytes_allocated: Cell::new(0),
            threshold: Cell::new(config.initial_threshold),
            config,
            stats: RefCell::new(GcStats::default()),
        }
    }

    /// Allocate a new GC-managed value.
    pub fn alloc<T: Trace + 'static>(&self, value: T) -> Gc<T> {
        let size = std::mem::size_of::<GcBox<T>>();

        // Check if we should collect
        if self.config.stress_test || self.bytes_allocated.get() + size > self.threshold.get() {
            // Note: Collection requires roots, which we don't have here.
            // In practice, collection is triggered by the VM which has access to roots.
        }

        // Allocate the box
        let boxed = Box::new(GcBox::new(value, size));
        let ptr = unsafe { NonNull::new_unchecked(Box::into_raw(boxed)) };

        // Add to allocation list
        unsafe {
            let gc_box = ptr.as_ref();
            gc_box.header.next.set(self.objects.get());
        }
        self.objects.set(Some(ptr as NonNull<GcBox<dyn Trace>>));

        // Update stats
        self.bytes_allocated.set(self.bytes_allocated.get() + size);
        {
            let mut stats = self.stats.borrow_mut();
            stats.total_allocations += 1;
            stats.total_bytes_allocated += size;
            stats.live_objects += 1;
            stats.live_bytes += size;
        }

        Gc { ptr }
    }

    /// Perform garbage collection with the given roots.
    pub fn collect(&self, roots: &[&dyn Trace]) {
        // Mark phase
        self.mark(roots);

        // Sweep phase
        self.sweep();

        // Update threshold
        let new_threshold = ((self.bytes_allocated.get() as f64) * self.config.growth_factor) as usize;
        self.threshold
            .set(new_threshold.max(self.config.min_threshold));

        self.stats.borrow_mut().collections += 1;
    }

    /// Mark all reachable objects starting from roots.
    fn mark(&self, roots: &[&dyn Trace]) {
        let mut tracer = Tracer { heap: self };
        for root in roots {
            root.trace(&mut tracer);
        }
    }

    /// Sweep unmarked objects.
    fn sweep(&self) {
        let mut prev: Option<NonNull<GcBox<dyn Trace>>> = None;
        let mut current = self.objects.get();
        let mut freed_bytes = 0usize;
        let mut freed_objects = 0usize;

        while let Some(ptr) = current {
            unsafe {
                let gc_box = ptr.as_ref();
                let next = gc_box.header.next.get();

                if gc_box.header.marked.get() {
                    // Object is live, clear mark for next cycle
                    gc_box.header.marked.set(false);
                    prev = current;
                    current = next;
                } else {
                    // Object is garbage, free it
                    let size = gc_box.header.size;
                    freed_bytes += size;
                    freed_objects += 1;

                    // Update linked list
                    if let Some(prev_ptr) = prev {
                        prev_ptr.as_ref().header.next.set(next);
                    } else {
                        self.objects.set(next);
                    }

                    // Free the memory
                    drop(Box::from_raw(ptr.as_ptr()));
                    current = next;
                }
            }
        }

        // Update stats
        self.bytes_allocated
            .set(self.bytes_allocated.get().saturating_sub(freed_bytes));
        {
            let mut stats = self.stats.borrow_mut();
            stats.total_bytes_freed += freed_bytes;
            stats.live_objects = stats.live_objects.saturating_sub(freed_objects);
            stats.live_bytes = stats.live_bytes.saturating_sub(freed_bytes);
        }
    }

    /// Get current statistics.
    pub fn stats(&self) -> GcStats {
        self.stats.borrow().clone()
    }

    /// Check if collection should be triggered.
    pub fn should_collect(&self) -> bool {
        self.config.stress_test || self.bytes_allocated.get() > self.threshold.get()
    }

    /// Get current bytes allocated.
    pub fn bytes_allocated(&self) -> usize {
        self.bytes_allocated.get()
    }
}

impl Default for GcHeap {
    fn default() -> Self {
        Self::new()
    }
}

impl Drop for GcHeap {
    fn drop(&mut self) {
        // Free all remaining objects
        let mut current = self.objects.get();
        while let Some(ptr) = current {
            unsafe {
                let gc_box = ptr.as_ref();
                let next = gc_box.header.next.get();
                drop(Box::from_raw(ptr.as_ptr()));
                current = next;
            }
        }
    }
}

// ---
// Tests
// ---

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_gc_alloc() {
        let heap = GcHeap::new();
        let s = heap.alloc(String::from("hello"));
        assert_eq!(&*s, "hello");
    }

    #[test]
    fn test_gc_alloc_multiple() {
        let heap = GcHeap::new();
        let a = heap.alloc(42i64);
        let b = heap.alloc(String::from("world"));
        let c = heap.alloc(vec![1i64, 2, 3]);

        assert_eq!(*a, 42);
        assert_eq!(&*b, "world");
        assert_eq!(&*c, &vec![1i64, 2, 3]);
    }

    #[test]
    fn test_gc_collect_unreachable() {
        let heap = GcHeap::with_config(GcConfig {
            stress_test: false,
            ..Default::default()
        });

        // Allocate some objects
        let _a = heap.alloc(String::from("keep me"));
        let _b = heap.alloc(String::from("garbage"));

        let initial_objects = heap.stats().live_objects;
        assert_eq!(initial_objects, 2);

        // Collect with only 'a' as root - 'b' should be freed
        heap.collect(&[&_a as &dyn Trace]);

        let final_objects = heap.stats().live_objects;
        assert_eq!(final_objects, 1);
    }

    #[test]
    fn test_gc_stats() {
        let heap = GcHeap::new();
        let _a = heap.alloc(42i64);
        let _b = heap.alloc(String::from("hello"));

        let stats = heap.stats();
        assert_eq!(stats.total_allocations, 2);
        assert_eq!(stats.live_objects, 2);
    }

    #[derive(Debug)]
    struct Node {
        value: i64,
        next: Option<Gc<RefCell<Node>>>,
    }

    impl Trace for Node {
        fn trace(&self, tracer: &mut Tracer) {
            if let Some(ref next) = self.next {
                tracer.mark(next);
            }
        }
    }

    #[test]
    fn test_gc_linked_list() {
        let heap = GcHeap::new();

        let node3 = heap.alloc(RefCell::new(Node {
            value: 3,
            next: None,
        }));
        let node2 = heap.alloc(RefCell::new(Node {
            value: 2,
            next: Some(node3),
        }));
        let node1 = heap.alloc(RefCell::new(Node {
            value: 1,
            next: Some(node2),
        }));

        // All three should be reachable from node1
        heap.collect(&[&node1 as &dyn Trace]);
        assert_eq!(heap.stats().live_objects, 3);

        // Only node3 reachable
        heap.collect(&[&node3 as &dyn Trace]);
        assert_eq!(heap.stats().live_objects, 1);
    }
}