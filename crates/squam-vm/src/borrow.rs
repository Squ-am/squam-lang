use std::cell::Cell;
use std::collections::HashMap;
use std::fmt;

// ---
// Borrow State
// ---

/// The state of borrows for a value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BorrowState {
    /// No active borrows
    #[default]
    Unused,
    /// One or more immutable borrows
    Reading(u32),
    /// One mutable borrow
    Writing,
}

impl BorrowState {
    /// Check if a read borrow can be acquired.
    pub fn can_read(&self) -> bool {
        matches!(self, BorrowState::Unused | BorrowState::Reading(_))
    }

    /// Check if a write borrow can be acquired.
    pub fn can_write(&self) -> bool {
        matches!(self, BorrowState::Unused)
    }

    /// Acquire a read borrow.
    pub fn acquire_read(&mut self) -> Result<(), BorrowError> {
        match self {
            BorrowState::Unused => {
                *self = BorrowState::Reading(1);
                Ok(())
            }
            BorrowState::Reading(n) => {
                *self = BorrowState::Reading(*n + 1);
                Ok(())
            }
            BorrowState::Writing => Err(BorrowError::AlreadyMutablyBorrowed),
        }
    }

    /// Release a read borrow.
    pub fn release_read(&mut self) -> Result<(), BorrowError> {
        match self {
            BorrowState::Reading(1) => {
                *self = BorrowState::Unused;
                Ok(())
            }
            BorrowState::Reading(n) if *n > 1 => {
                *self = BorrowState::Reading(*n - 1);
                Ok(())
            }
            _ => Err(BorrowError::NotBorrowed),
        }
    }

    /// Acquire a write borrow.
    pub fn acquire_write(&mut self) -> Result<(), BorrowError> {
        match self {
            BorrowState::Unused => {
                *self = BorrowState::Writing;
                Ok(())
            }
            BorrowState::Reading(_) => Err(BorrowError::AlreadyImmutablyBorrowed),
            BorrowState::Writing => Err(BorrowError::AlreadyMutablyBorrowed),
        }
    }

    /// Release a write borrow.
    pub fn release_write(&mut self) -> Result<(), BorrowError> {
        match self {
            BorrowState::Writing => {
                *self = BorrowState::Unused;
                Ok(())
            }
            _ => Err(BorrowError::NotBorrowed),
        }
    }
}

// ---
// Borrow Error
// ---

/// Errors that can occur during borrow checking.
#[derive(Debug, Clone, thiserror::Error)]
pub enum BorrowError {
    #[error("cannot borrow as immutable because it is already mutably borrowed")]
    AlreadyMutablyBorrowed,

    #[error("cannot borrow as mutable because it is already immutably borrowed")]
    AlreadyImmutablyBorrowed,

    #[error("cannot release borrow: not currently borrowed")]
    NotBorrowed,

    #[error("use after move: value has been moved")]
    UseAfterMove,

    #[error("double free: value has already been freed")]
    DoubleFree,
}

// ---
// Borrow Tracker
// ---

/// Unique identifier for a tracked value.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ValueId(u64);

impl ValueId {
    /// Create a new unique value ID.
    fn new(id: u64) -> Self {
        Self(id)
    }
}

impl fmt::Display for ValueId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ValueId({})", self.0)
    }
}

/// Tracks borrows and ownership at runtime.
pub struct BorrowTracker {
    /// Borrow states for tracked values
    states: HashMap<ValueId, BorrowState>,
    /// Counter for generating unique IDs
    next_id: Cell<u64>,
    /// Whether borrow checking is enabled
    enabled: bool,
    /// Moved values (use after move detection)
    moved: HashMap<ValueId, bool>,
}

impl BorrowTracker {
    /// Create a new borrow tracker.
    pub fn new() -> Self {
        Self {
            states: HashMap::new(),
            next_id: Cell::new(1),
            enabled: true,
            moved: HashMap::new(),
        }
    }

    /// Enable or disable borrow checking.
    pub fn set_enabled(&mut self, enabled: bool) {
        self.enabled = enabled;
    }

    /// Check if borrow checking is enabled.
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    /// Allocate a new value ID.
    pub fn alloc(&mut self) -> ValueId {
        let id = self.next_id.get();
        self.next_id.set(id + 1);
        let value_id = ValueId::new(id);
        self.states.insert(value_id, BorrowState::Unused);
        value_id
    }

    /// Free a value ID.
    pub fn free(&mut self, id: ValueId) -> Result<(), BorrowError> {
        if !self.enabled {
            return Ok(());
        }

        if self.moved.get(&id) == Some(&true) {
            return Err(BorrowError::DoubleFree);
        }

        match self.states.get(&id) {
            Some(BorrowState::Unused) => {
                self.states.remove(&id);
                Ok(())
            }
            Some(BorrowState::Reading(_)) => Err(BorrowError::AlreadyImmutablyBorrowed),
            Some(BorrowState::Writing) => Err(BorrowError::AlreadyMutablyBorrowed),
            None => Err(BorrowError::DoubleFree),
        }
    }

    /// Mark a value as moved.
    pub fn mark_moved(&mut self, id: ValueId) {
        if self.enabled {
            self.moved.insert(id, true);
        }
    }

    /// Check if a value has been moved.
    pub fn is_moved(&self, id: ValueId) -> bool {
        self.moved.get(&id) == Some(&true)
    }

    /// Try to acquire a read borrow.
    pub fn borrow(&mut self, id: ValueId) -> Result<(), BorrowError> {
        if !self.enabled {
            return Ok(());
        }

        if self.is_moved(id) {
            return Err(BorrowError::UseAfterMove);
        }

        if let Some(state) = self.states.get_mut(&id) {
            state.acquire_read()
        } else {
            Err(BorrowError::DoubleFree)
        }
    }

    /// Release a read borrow.
    pub fn unborrow(&mut self, id: ValueId) -> Result<(), BorrowError> {
        if !self.enabled {
            return Ok(());
        }

        if let Some(state) = self.states.get_mut(&id) {
            state.release_read()
        } else {
            Err(BorrowError::NotBorrowed)
        }
    }

    /// Try to acquire a write borrow.
    pub fn borrow_mut(&mut self, id: ValueId) -> Result<(), BorrowError> {
        if !self.enabled {
            return Ok(());
        }

        if self.is_moved(id) {
            return Err(BorrowError::UseAfterMove);
        }

        if let Some(state) = self.states.get_mut(&id) {
            state.acquire_write()
        } else {
            Err(BorrowError::DoubleFree)
        }
    }

    /// Release a write borrow.
    pub fn unborrow_mut(&mut self, id: ValueId) -> Result<(), BorrowError> {
        if !self.enabled {
            return Ok(());
        }

        if let Some(state) = self.states.get_mut(&id) {
            state.release_write()
        } else {
            Err(BorrowError::NotBorrowed)
        }
    }

    /// Get the current borrow state for a value.
    pub fn state(&self, id: ValueId) -> Option<BorrowState> {
        self.states.get(&id).copied()
    }

    /// Get statistics about tracked values.
    pub fn stats(&self) -> BorrowStats {
        let mut reading = 0;
        let mut writing = 0;
        let mut unused = 0;

        for state in self.states.values() {
            match state {
                BorrowState::Unused => unused += 1,
                BorrowState::Reading(_) => reading += 1,
                BorrowState::Writing => writing += 1,
            }
        }

        BorrowStats {
            total_tracked: self.states.len(),
            reading,
            writing,
            unused,
            moved: self.moved.values().filter(|&&v| v).count(),
        }
    }
}

impl Default for BorrowTracker {
    fn default() -> Self {
        Self::new()
    }
}

/// Statistics about borrow tracking.
#[derive(Debug, Clone)]
pub struct BorrowStats {
    /// Total number of tracked values
    pub total_tracked: usize,
    /// Number of values with active read borrows
    pub reading: usize,
    /// Number of values with active write borrows
    pub writing: usize,
    /// Number of values with no active borrows
    pub unused: usize,
    /// Number of moved values
    pub moved: usize,
}

// ---
// Tests
// ---

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_borrow_state_read() {
        let mut state = BorrowState::Unused;
        assert!(state.can_read());
        assert!(state.can_write());

        state.acquire_read().unwrap();
        assert_eq!(state, BorrowState::Reading(1));
        assert!(state.can_read());
        assert!(!state.can_write());

        state.acquire_read().unwrap();
        assert_eq!(state, BorrowState::Reading(2));

        state.release_read().unwrap();
        assert_eq!(state, BorrowState::Reading(1));

        state.release_read().unwrap();
        assert_eq!(state, BorrowState::Unused);
    }

    #[test]
    fn test_borrow_state_write() {
        let mut state = BorrowState::Unused;
        state.acquire_write().unwrap();
        assert_eq!(state, BorrowState::Writing);
        assert!(!state.can_read());
        assert!(!state.can_write());

        assert!(state.acquire_read().is_err());
        assert!(state.acquire_write().is_err());

        state.release_write().unwrap();
        assert_eq!(state, BorrowState::Unused);
    }

    #[test]
    fn test_tracker_basic() {
        let mut tracker = BorrowTracker::new();
        let id = tracker.alloc();

        tracker.borrow(id).unwrap();
        tracker.borrow(id).unwrap();
        tracker.unborrow(id).unwrap();
        tracker.unborrow(id).unwrap();

        tracker.borrow_mut(id).unwrap();
        tracker.unborrow_mut(id).unwrap();

        tracker.free(id).unwrap();
    }

    #[test]
    fn test_tracker_conflict() {
        let mut tracker = BorrowTracker::new();
        let id = tracker.alloc();

        tracker.borrow(id).unwrap();
        assert!(tracker.borrow_mut(id).is_err());
        tracker.unborrow(id).unwrap();

        tracker.borrow_mut(id).unwrap();
        assert!(tracker.borrow(id).is_err());
        tracker.unborrow_mut(id).unwrap();
    }

    #[test]
    fn test_tracker_move() {
        let mut tracker = BorrowTracker::new();
        let id = tracker.alloc();

        tracker.mark_moved(id);
        assert!(tracker.is_moved(id));
        assert!(tracker.borrow(id).is_err());
        assert!(tracker.borrow_mut(id).is_err());
    }

    #[test]
    fn test_tracker_disabled() {
        let mut tracker = BorrowTracker::new();
        tracker.set_enabled(false);
        let id = tracker.alloc();

        // Everything should succeed when disabled
        tracker.borrow(id).unwrap();
        tracker.borrow_mut(id).unwrap();
        tracker.free(id).unwrap();
    }
}
