use std::collections::HashMap;
use std::sync::Arc;

// ---
// Cache Entry Types
// ---

/// A cached slot index for a specific type.
#[derive(Debug, Clone)]
pub struct CacheEntry {
    /// The type ID this cache entry is valid for
    pub type_id: TypeId,
    /// The cached slot/offset
    pub slot: usize,
    /// Hit count for statistics
    pub hits: u64,
}

/// Type identifier for cache invalidation.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(u64);

impl TypeId {
    /// Create a new type ID from a type name.
    pub fn from_name(name: &str) -> Self {
        // Simple hash-based type ID
        let mut hash: u64 = 0;
        for byte in name.bytes() {
            hash = hash.wrapping_mul(31).wrapping_add(byte as u64);
        }
        TypeId(hash)
    }

    /// Get the internal ID value.
    pub fn as_u64(&self) -> u64 {
        self.0
    }
}

// ---
// Inline Cache
// ---

/// An inline cache for a single call site.
#[derive(Debug, Clone, Default)]
pub enum InlineCache {
    /// Uninitialized cache
    #[default]
    Empty,
    /// Monomorphic cache (single type seen)
    Mono(CacheEntry),
    /// Polymorphic cache (multiple types, up to 4)
    Poly(Vec<CacheEntry>),
    /// Megamorphic (too many types, fall back to full lookup)
    Mega,
}

impl InlineCache {
    /// Maximum number of entries in polymorphic cache.
    const POLY_LIMIT: usize = 4;

    /// Create a new empty cache.
    pub fn new() -> Self {
        InlineCache::Empty
    }

    /// Look up a cached slot for a type.
    pub fn lookup(&mut self, type_id: TypeId) -> Option<usize> {
        match self {
            InlineCache::Empty => None,
            InlineCache::Mono(entry) => {
                if entry.type_id == type_id {
                    entry.hits += 1;
                    Some(entry.slot)
                } else {
                    None
                }
            }
            InlineCache::Poly(entries) => {
                for entry in entries.iter_mut() {
                    if entry.type_id == type_id {
                        entry.hits += 1;
                        return Some(entry.slot);
                    }
                }
                None
            }
            InlineCache::Mega => None,
        }
    }

    /// Update the cache with a new type -> slot mapping.
    pub fn update(&mut self, type_id: TypeId, slot: usize) {
        match self {
            InlineCache::Empty => {
                *self = InlineCache::Mono(CacheEntry {
                    type_id,
                    slot,
                    hits: 0,
                });
            }
            InlineCache::Mono(entry) => {
                if entry.type_id != type_id {
                    // Promote to polymorphic
                    let old_entry = entry.clone();
                    *self = InlineCache::Poly(vec![
                        old_entry,
                        CacheEntry {
                            type_id,
                            slot,
                            hits: 0,
                        },
                    ]);
                }
            }
            InlineCache::Poly(entries) => {
                // Check if type already exists
                for entry in entries.iter() {
                    if entry.type_id == type_id {
                        return;
                    }
                }

                if entries.len() < Self::POLY_LIMIT {
                    entries.push(CacheEntry {
                        type_id,
                        slot,
                        hits: 0,
                    });
                } else {
                    // Too many types, go megamorphic
                    *self = InlineCache::Mega;
                }
            }
            InlineCache::Mega => {
                // Already megamorphic, nothing to do
            }
        }
    }

    /// Get cache statistics.
    pub fn stats(&self) -> CacheStats {
        match self {
            InlineCache::Empty => CacheStats {
                state: CacheState::Empty,
                total_hits: 0,
                entry_count: 0,
            },
            InlineCache::Mono(entry) => CacheStats {
                state: CacheState::Monomorphic,
                total_hits: entry.hits,
                entry_count: 1,
            },
            InlineCache::Poly(entries) => CacheStats {
                state: CacheState::Polymorphic,
                total_hits: entries.iter().map(|e| e.hits).sum(),
                entry_count: entries.len(),
            },
            InlineCache::Mega => CacheStats {
                state: CacheState::Megamorphic,
                total_hits: 0,
                entry_count: 0,
            },
        }
    }

    /// Clear the cache.
    pub fn clear(&mut self) {
        *self = InlineCache::Empty;
    }
}

// ---
// Global Cache Manager
// ---

/// Manages inline caches for all call sites.
pub struct InlineCacheManager {
    /// Method dispatch caches by call site ID
    method_caches: HashMap<CallSiteId, InlineCache>,
    /// Field access caches by call site ID
    field_caches: HashMap<CallSiteId, InlineCache>,
    /// Global variable caches by name
    global_caches: HashMap<Arc<str>, usize>,
    /// Whether caching is enabled
    enabled: bool,
    /// Statistics
    stats: GlobalCacheStats,
}

/// Identifier for a call site (bytecode offset).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct CallSiteId(u32);

impl CallSiteId {
    /// Create a new call site ID from a bytecode offset.
    pub fn from_offset(offset: usize) -> Self {
        CallSiteId(offset as u32)
    }
}

/// Statistics for a single cache.
#[derive(Debug, Clone)]
pub struct CacheStats {
    /// Current cache state
    pub state: CacheState,
    /// Total number of cache hits
    pub total_hits: u64,
    /// Number of cached entries
    pub entry_count: usize,
}

/// The state of an inline cache.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CacheState {
    Empty,
    Monomorphic,
    Polymorphic,
    Megamorphic,
}

/// Global inline cache statistics.
#[derive(Debug, Clone, Default)]
pub struct GlobalCacheStats {
    /// Total cache lookups
    pub lookups: u64,
    /// Total cache hits
    pub hits: u64,
    /// Total cache misses
    pub misses: u64,
    /// Number of monomorphic caches
    pub mono_count: usize,
    /// Number of polymorphic caches
    pub poly_count: usize,
    /// Number of megamorphic caches
    pub mega_count: usize,
}

impl InlineCacheManager {
    /// Create a new cache manager.
    pub fn new() -> Self {
        Self {
            method_caches: HashMap::new(),
            field_caches: HashMap::new(),
            global_caches: HashMap::new(),
            enabled: true,
            stats: GlobalCacheStats::default(),
        }
    }

    /// Enable or disable caching.
    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
        if !enabled {
            self.clear_all();
        }
    }

    /// Check if caching is enabled.
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Look up a method cache.
    pub fn lookup_method(&mut self, site: CallSiteId, type_id: TypeId) -> Option<usize> {
        if !self.enabled {
            return None;
        }

        self.stats.lookups += 1;
        let cache = self.method_caches.entry(site).or_default();
        match cache.lookup(type_id) {
            Some(slot) => {
                self.stats.hits += 1;
                Some(slot)
            }
            None => {
                self.stats.misses += 1;
                None
            }
        }
    }

    /// Update a method cache.
    pub fn update_method(&mut self, site: CallSiteId, type_id: TypeId, slot: usize) {
        if !self.enabled {
            return;
        }

        let cache = self.method_caches.entry(site).or_default();
        cache.update(type_id, slot);
    }

    /// Look up a field cache.
    pub fn lookup_field(&mut self, site: CallSiteId, type_id: TypeId) -> Option<usize> {
        if !self.enabled {
            return None;
        }

        self.stats.lookups += 1;
        let cache = self.field_caches.entry(site).or_default();
        match cache.lookup(type_id) {
            Some(slot) => {
                self.stats.hits += 1;
                Some(slot)
            }
            None => {
                self.stats.misses += 1;
                None
            }
        }
    }

    /// Update a field cache.
    pub fn update_field(&mut self, site: CallSiteId, type_id: TypeId, slot: usize) {
        if !self.enabled {
            return;
        }

        let cache = self.field_caches.entry(site).or_default();
        cache.update(type_id, slot);
    }

    /// Look up a global variable cache.
    pub fn lookup_global(&mut self, name: &str) -> Option<usize> {
        if !self.enabled {
            return None;
        }

        self.stats.lookups += 1;
        match self.global_caches.get(name) {
            Some(&slot) => {
                self.stats.hits += 1;
                Some(slot)
            }
            None => {
                self.stats.misses += 1;
                None
            }
        }
    }

    /// Update a global variable cache.
    pub fn update_global(&mut self, name: Arc<str>, slot: usize) {
        if !self.enabled {
            return;
        }

        self.global_caches.insert(name, slot);
    }

    /// Clear all caches.
    pub fn clear_all(&mut self) {
        self.method_caches.clear();
        self.field_caches.clear();
        self.global_caches.clear();
    }

    /// Get global statistics.
    pub fn stats(&self) -> GlobalCacheStats {
        let mut stats = self.stats.clone();

        // Count cache states
        for cache in self.method_caches.values() {
            match cache {
                InlineCache::Empty => {}
                InlineCache::Mono(_) => stats.mono_count += 1,
                InlineCache::Poly(_) => stats.poly_count += 1,
                InlineCache::Mega => stats.mega_count += 1,
            }
        }

        for cache in self.field_caches.values() {
            match cache {
                InlineCache::Empty => {}
                InlineCache::Mono(_) => stats.mono_count += 1,
                InlineCache::Poly(_) => stats.poly_count += 1,
                InlineCache::Mega => stats.mega_count += 1,
            }
        }

        stats
    }

    /// Get cache hit ratio.
    pub fn hit_ratio(&self) -> f64 {
        if self.stats.lookups == 0 {
            0.0
        } else {
            self.stats.hits as f64 / self.stats.lookups as f64
        }
    }

    /// Generate a cache report.
    pub fn report(&self) -> String {
        let stats = self.stats();
        let mut report = String::new();

        report.push_str("=== Inline Cache Report ===\n\n");
        report.push_str(&format!("Enabled: {}\n", self.enabled));
        report.push_str(&format!("Hit Ratio: {:.2}%\n", self.hit_ratio() * 100.0));
        report.push_str(&format!("Total Lookups: {}\n", stats.lookups));
        report.push_str(&format!("Total Hits: {}\n", stats.hits));
        report.push_str(&format!("Total Misses: {}\n", stats.misses));
        report.push_str("\nCache States:\n");
        report.push_str(&format!("  Monomorphic: {}\n", stats.mono_count));
        report.push_str(&format!("  Polymorphic: {}\n", stats.poly_count));
        report.push_str(&format!("  Megamorphic: {}\n", stats.mega_count));
        report.push_str(&format!(
            "\nGlobal Variable Caches: {}\n",
            self.global_caches.len()
        ));

        report
    }
}

impl Default for InlineCacheManager {
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
    fn test_type_id() {
        let id1 = TypeId::from_name("String");
        let id2 = TypeId::from_name("String");
        let id3 = TypeId::from_name("Array");

        assert_eq!(id1, id2);
        assert_ne!(id1, id3);
    }

    #[test]
    fn test_inline_cache_mono() {
        let mut cache = InlineCache::new();
        let type_id = TypeId::from_name("Point");

        // First lookup should miss
        assert!(cache.lookup(type_id).is_none());

        // Update cache
        cache.update(type_id, 42);

        // Second lookup should hit
        assert_eq!(cache.lookup(type_id), Some(42));

        let stats = cache.stats();
        assert_eq!(stats.state, CacheState::Monomorphic);
        assert_eq!(stats.entry_count, 1);
    }

    #[test]
    fn test_inline_cache_poly() {
        let mut cache = InlineCache::new();
        let type1 = TypeId::from_name("Point2D");
        let type2 = TypeId::from_name("Point3D");

        cache.update(type1, 10);
        cache.update(type2, 20);

        assert_eq!(cache.lookup(type1), Some(10));
        assert_eq!(cache.lookup(type2), Some(20));

        let stats = cache.stats();
        assert_eq!(stats.state, CacheState::Polymorphic);
        assert_eq!(stats.entry_count, 2);
    }

    #[test]
    fn test_inline_cache_mega() {
        let mut cache = InlineCache::new();

        // Add more than POLY_LIMIT types
        for i in 0..10 {
            let type_id = TypeId::from_name(&format!("Type{}", i));
            cache.update(type_id, i);
        }

        let stats = cache.stats();
        assert_eq!(stats.state, CacheState::Megamorphic);
    }

    #[test]
    fn test_cache_manager() {
        let mut manager = InlineCacheManager::new();
        let site = CallSiteId::from_offset(100);
        let type_id = TypeId::from_name("MyClass");

        // First lookup should miss
        assert!(manager.lookup_method(site, type_id).is_none());

        // Update cache
        manager.update_method(site, type_id, 5);

        // Second lookup should hit
        assert_eq!(manager.lookup_method(site, type_id), Some(5));

        // Check stats
        let stats = manager.stats();
        assert_eq!(stats.lookups, 2);
        assert_eq!(stats.hits, 1);
        assert_eq!(stats.misses, 1);
    }

    #[test]
    fn test_global_cache() {
        let mut manager = InlineCacheManager::new();

        // First lookup should miss
        assert!(manager.lookup_global("my_global").is_none());

        // Update cache
        manager.update_global("my_global".into(), 42);

        // Second lookup should hit
        assert_eq!(manager.lookup_global("my_global"), Some(42));
    }

    #[test]
    fn test_cache_disabled() {
        let mut manager = InlineCacheManager::new();
        manager.set_enabled(false);

        let site = CallSiteId::from_offset(100);
        let type_id = TypeId::from_name("MyClass");

        // Updates should be ignored
        manager.update_method(site, type_id, 5);

        // Lookups should always miss
        assert!(manager.lookup_method(site, type_id).is_none());
    }
}
