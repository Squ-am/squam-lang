use std::cell::RefCell;
use std::collections::HashMap;
use std::time::{Duration, Instant};

// ---
// Memory Mode
// ---

/// The memory management mode.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum MemoryMode {
    /// Reference counting (default) - uses Rust's Rc
    #[default]
    RefCounting,
    /// Mark-sweep garbage collection
    MarkSweep,
}

// ---
// Allocation Tracker
// ---

/// Tracks memory allocations for profiling.
#[derive(Debug, Clone)]
pub struct AllocationInfo {
    /// Size of the allocation in bytes
    pub size: usize,
    /// Type name of the allocated value
    pub type_name: &'static str,
    /// Allocation timestamp (if tracking is enabled)
    pub allocated_at: Option<Instant>,
}

/// Memory profiler for tracking allocations.
pub struct MemoryProfiler {
    /// Total bytes currently allocated
    bytes_allocated: usize,
    /// Peak memory usage
    peak_bytes: usize,
    /// Number of allocations
    allocation_count: usize,
    /// Number of deallocations
    deallocation_count: usize,
    /// Allocation history (if detailed tracking is enabled)
    allocations: HashMap<usize, AllocationInfo>,
    /// Next allocation ID
    next_id: usize,
    /// Whether detailed tracking is enabled
    detailed_tracking: bool,
    /// GC collection times
    gc_times: Vec<Duration>,
}

impl MemoryProfiler {
    /// Create a new memory profiler.
    pub fn new() -> Self {
        Self {
            bytes_allocated: 0,
            peak_bytes: 0,
            allocation_count: 0,
            deallocation_count: 0,
            allocations: HashMap::new(),
            next_id: 1,
            detailed_tracking: false,
            gc_times: Vec::new(),
        }
    }

    /// Enable or disable detailed allocation tracking.
    pub fn set_detailed_tracking(&mut self, enabled: bool) {
        self.detailed_tracking = enabled;
        if !enabled {
            self.allocations.clear();
        }
    }

    /// Record an allocation.
    pub fn record_alloc(&mut self, size: usize, type_name: &'static str) -> usize {
        self.bytes_allocated += size;
        self.allocation_count += 1;
        if self.bytes_allocated > self.peak_bytes {
            self.peak_bytes = self.bytes_allocated;
        }

        let id = self.next_id;
        self.next_id += 1;

        if self.detailed_tracking {
            self.allocations.insert(
                id,
                AllocationInfo {
                    size,
                    type_name,
                    allocated_at: Some(Instant::now()),
                },
            );
        }

        id
    }

    /// Record a deallocation.
    pub fn record_dealloc(&mut self, id: usize, size: usize) {
        self.bytes_allocated = self.bytes_allocated.saturating_sub(size);
        self.deallocation_count += 1;

        if self.detailed_tracking {
            self.allocations.remove(&id);
        }
    }

    /// Record a GC collection time.
    pub fn record_gc_time(&mut self, duration: Duration) {
        self.gc_times.push(duration);
    }

    /// Get current bytes allocated.
    pub fn bytes_allocated(&self) -> usize {
        self.bytes_allocated
    }

    /// Get peak memory usage.
    pub fn peak_bytes(&self) -> usize {
        self.peak_bytes
    }

    /// Get total allocation count.
    pub fn allocation_count(&self) -> usize {
        self.allocation_count
    }

    /// Get total deallocation count.
    pub fn deallocation_count(&self) -> usize {
        self.deallocation_count
    }

    /// Get current live allocation count.
    pub fn live_allocations(&self) -> usize {
        self.allocation_count
            .saturating_sub(self.deallocation_count)
    }

    /// Get memory statistics snapshot.
    pub fn stats(&self) -> MemoryStats {
        MemoryStats {
            bytes_allocated: self.bytes_allocated,
            peak_bytes: self.peak_bytes,
            allocation_count: self.allocation_count,
            deallocation_count: self.deallocation_count,
            live_allocations: self.live_allocations(),
            gc_collection_count: self.gc_times.len(),
            total_gc_time: self.gc_times.iter().sum(),
            avg_gc_time: if self.gc_times.is_empty() {
                Duration::ZERO
            } else {
                self.gc_times.iter().sum::<Duration>() / self.gc_times.len() as u32
            },
        }
    }

    /// Get detailed allocation information (if tracking is enabled).
    pub fn allocations(&self) -> &HashMap<usize, AllocationInfo> {
        &self.allocations
    }

    /// Generate a memory report.
    pub fn report(&self) -> String {
        let stats = self.stats();
        let mut report = String::new();

        report.push_str("=== Memory Profile ===\n\n");
        report.push_str(&format!(
            "Current Usage:     {} bytes ({:.2} KB)\n",
            stats.bytes_allocated,
            stats.bytes_allocated as f64 / 1024.0
        ));
        report.push_str(&format!(
            "Peak Usage:        {} bytes ({:.2} KB)\n",
            stats.peak_bytes,
            stats.peak_bytes as f64 / 1024.0
        ));
        report.push_str(&format!("Live Allocations:  {}\n", stats.live_allocations));
        report.push_str(&format!("Total Allocations: {}\n", stats.allocation_count));
        report.push_str(&format!(
            "Total Deallocations: {}\n",
            stats.deallocation_count
        ));

        if stats.gc_collection_count > 0 {
            report.push_str("\n=== GC Statistics ===\n");
            report.push_str(&format!(
                "Collections:       {}\n",
                stats.gc_collection_count
            ));
            report.push_str(&format!("Total GC Time:     {:?}\n", stats.total_gc_time));
            report.push_str(&format!("Avg GC Time:       {:?}\n", stats.avg_gc_time));
        }

        if self.detailed_tracking && !self.allocations.is_empty() {
            report.push_str("\n=== Live Allocations by Type ===\n");

            // Group allocations by type
            let mut by_type: HashMap<&str, (usize, usize)> = HashMap::new();
            for info in self.allocations.values() {
                let entry = by_type.entry(info.type_name).or_insert((0, 0));
                entry.0 += 1;
                entry.1 += info.size;
            }

            let mut types: Vec<_> = by_type.into_iter().collect();
            types.sort_by(|a, b| b.1 .1.cmp(&a.1 .1)); // Sort by size descending

            for (type_name, (count, size)) in types {
                report.push_str(&format!(
                    "  {}: {} allocations, {} bytes\n",
                    type_name, count, size
                ));
            }
        }

        report
    }

    /// Reset all statistics.
    pub fn reset(&mut self) {
        self.bytes_allocated = 0;
        self.peak_bytes = 0;
        self.allocation_count = 0;
        self.deallocation_count = 0;
        self.allocations.clear();
        self.gc_times.clear();
    }
}

impl Default for MemoryProfiler {
    fn default() -> Self {
        Self::new()
    }
}

/// Memory statistics snapshot.
#[derive(Debug, Clone)]
pub struct MemoryStats {
    /// Current bytes allocated
    pub bytes_allocated: usize,
    /// Peak memory usage
    pub peak_bytes: usize,
    /// Total number of allocations
    pub allocation_count: usize,
    /// Total number of deallocations
    pub deallocation_count: usize,
    /// Current live allocations
    pub live_allocations: usize,
    /// Number of GC collections
    pub gc_collection_count: usize,
    /// Total time spent in GC
    pub total_gc_time: Duration,
    /// Average GC collection time
    pub avg_gc_time: Duration,
}

// ---
// Memory Manager
// ---

/// Unified memory manager for the VM.
pub struct MemoryManager {
    /// Current memory mode
    mode: MemoryMode,
    /// Memory profiler
    profiler: RefCell<MemoryProfiler>,
    /// Whether profiling is enabled
    profiling_enabled: bool,
}

impl MemoryManager {
    /// Create a new memory manager.
    pub fn new() -> Self {
        Self {
            mode: MemoryMode::default(),
            profiler: RefCell::new(MemoryProfiler::new()),
            profiling_enabled: false,
        }
    }

    /// Create a memory manager with a specific mode.
    pub fn with_mode(mode: MemoryMode) -> Self {
        Self {
            mode,
            profiler: RefCell::new(MemoryProfiler::new()),
            profiling_enabled: false,
        }
    }

    /// Get the current memory mode.
    pub fn mode(&self) -> MemoryMode {
        self.mode
    }

    /// Set the memory mode.
    pub fn set_mode(&mut self, mode: MemoryMode) {
        self.mode = mode;
    }

    /// Enable or disable profiling.
    pub fn set_profiling(&mut self, enabled: bool) {
        self.profiling_enabled = enabled;
    }

    /// Enable or disable detailed allocation tracking.
    pub fn set_detailed_tracking(&mut self, enabled: bool) {
        self.profiler.borrow_mut().set_detailed_tracking(enabled);
    }

    /// Record an allocation (if profiling is enabled).
    pub fn record_alloc(&self, size: usize, type_name: &'static str) -> usize {
        if self.profiling_enabled {
            self.profiler.borrow_mut().record_alloc(size, type_name)
        } else {
            0
        }
    }

    /// Record a deallocation (if profiling is enabled).
    pub fn record_dealloc(&self, id: usize, size: usize) {
        if self.profiling_enabled {
            self.profiler.borrow_mut().record_dealloc(id, size);
        }
    }

    /// Record GC time (if profiling is enabled).
    pub fn record_gc_time(&self, duration: Duration) {
        if self.profiling_enabled {
            self.profiler.borrow_mut().record_gc_time(duration);
        }
    }

    /// Get memory statistics.
    pub fn stats(&self) -> MemoryStats {
        self.profiler.borrow().stats()
    }

    /// Generate a memory report.
    pub fn report(&self) -> String {
        self.profiler.borrow().report()
    }

    /// Reset profiler statistics.
    pub fn reset_stats(&self) {
        self.profiler.borrow_mut().reset();
    }
}

impl Default for MemoryManager {
    fn default() -> Self {
        Self::new()
    }
}

// ---
// Tests
// ---

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_profiler_basic() {
        let mut profiler = MemoryProfiler::new();

        let id1 = profiler.record_alloc(100, "String");
        let id2 = profiler.record_alloc(200, "Array");

        assert_eq!(profiler.bytes_allocated(), 300);
        assert_eq!(profiler.allocation_count(), 2);
        assert_eq!(profiler.live_allocations(), 2);

        profiler.record_dealloc(id1, 100);
        assert_eq!(profiler.bytes_allocated(), 200);
        assert_eq!(profiler.live_allocations(), 1);

        profiler.record_dealloc(id2, 200);
        assert_eq!(profiler.bytes_allocated(), 0);
        assert_eq!(profiler.live_allocations(), 0);
    }

    #[test]
    fn test_profiler_peak() {
        let mut profiler = MemoryProfiler::new();

        let id1 = profiler.record_alloc(100, "A");
        let _id2 = profiler.record_alloc(200, "B");
        profiler.record_dealloc(id1, 100);
        let _id3 = profiler.record_alloc(50, "C");

        assert_eq!(profiler.peak_bytes(), 300); // 100 + 200 was peak
        assert_eq!(profiler.bytes_allocated(), 250); // 200 + 50 current
    }

    #[test]
    fn test_detailed_tracking() {
        let mut profiler = MemoryProfiler::new();
        profiler.set_detailed_tracking(true);

        let id1 = profiler.record_alloc(100, "String");
        let _id2 = profiler.record_alloc(200, "Array");

        assert_eq!(profiler.allocations().len(), 2);

        profiler.record_dealloc(id1, 100);
        assert_eq!(profiler.allocations().len(), 1);
    }

    #[test]
    fn test_memory_manager() {
        let mut manager = MemoryManager::new();
        manager.set_profiling(true);

        manager.record_alloc(100, "Test");
        assert_eq!(manager.stats().bytes_allocated, 100);
    }

    #[test]
    fn test_gc_time_tracking() {
        let mut profiler = MemoryProfiler::new();

        profiler.record_gc_time(Duration::from_millis(10));
        profiler.record_gc_time(Duration::from_millis(20));

        let stats = profiler.stats();
        assert_eq!(stats.gc_collection_count, 2);
        assert_eq!(stats.total_gc_time, Duration::from_millis(30));
    }
}
