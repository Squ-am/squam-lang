use std::cell::RefCell;
use std::collections::{BinaryHeap, HashMap, VecDeque};
use std::rc::Rc;
use std::time::Instant;

use crate::value::{Closure, FutureState, Value};

/// Unique identifier for a task.
pub type TaskId = u64;

/// Unique identifier for a timer.
pub type TimerId = u64;

/// The result of polling a future.
#[derive(Debug, Clone)]
pub enum Poll<T> {
    /// The future completed with this value.
    Ready(T),
    /// The future is not ready yet.
    Pending,
}

/// A suspended task waiting to be resumed.
#[derive(Clone)]
pub struct Task {
    /// Unique task identifier.
    pub id: TaskId,
    /// Saved value stack.
    pub stack: Vec<Value>,
    /// Saved call frames (closure, ip, stack_base).
    pub frames: Vec<(Rc<Closure>, usize, usize)>,
    /// The future this task is waiting on (if any).
    pub waiting_on: Option<Rc<RefCell<FutureState>>>,
    /// Result value when the task completes.
    pub result: Option<Value>,
}

/// A timer entry in the timer heap.
#[derive(Clone, Eq, PartialEq)]
struct TimerEntry {
    deadline: Instant,
    task_id: TaskId,
    timer_id: TimerId,
}

impl Ord for TimerEntry {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        // Reverse ordering for min-heap (earliest deadline first)
        other.deadline.cmp(&self.deadline)
            .then_with(|| other.timer_id.cmp(&self.timer_id))
    }
}

impl PartialOrd for TimerEntry {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

/// The async executor/event loop.
pub struct Executor {
    /// All tasks by ID.
    tasks: HashMap<TaskId, Task>,
    /// Queue of tasks ready to run.
    ready_queue: VecDeque<TaskId>,
    /// Timer heap (min-heap by deadline).
    timers: BinaryHeap<TimerEntry>,
    /// Next task ID to assign.
    next_task_id: TaskId,
    /// Next timer ID to assign.
    next_timer_id: TimerId,
    /// The currently running task (if any).
    current_task: Option<TaskId>,
    /// Tasks waiting on other tasks to complete (target_task_id -> [waiting_task_ids]).
    task_waiters: HashMap<TaskId, Vec<TaskId>>,
    /// Completed task results (for tasks that have waiters).
    completed_results: HashMap<TaskId, Value>,
}

impl Default for Executor {
    fn default() -> Self {
        Self::new()
    }
}

impl Executor {
    /// Create a new executor.
    pub fn new() -> Self {
        Self {
            tasks: HashMap::new(),
            ready_queue: VecDeque::new(),
            timers: BinaryHeap::new(),
            next_task_id: 1,
            next_timer_id: 1,
            current_task: None,
            task_waiters: HashMap::new(),
            completed_results: HashMap::new(),
        }
    }

    /// Spawn a new task from a future value.
    /// Returns the task ID.
    pub fn spawn(&mut self, future: Value) -> TaskId {
        let id = self.next_task_id;
        self.next_task_id += 1;

        let task = Task {
            id,
            stack: vec![future],
            frames: Vec::new(),
            waiting_on: None,
            result: None,
        };

        self.tasks.insert(id, task);
        self.ready_queue.push_back(id);
        id
    }

    /// Spawn a task and return it (for block_on which needs to track a specific task).
    pub fn spawn_task(&mut self, future: Value) -> TaskId {
        self.spawn(future)
    }

    /// Register a timer that will wake the given task at the deadline.
    pub fn register_timer(&mut self, task_id: TaskId, deadline: Instant) -> TimerId {
        let timer_id = self.next_timer_id;
        self.next_timer_id += 1;

        self.timers.push(TimerEntry {
            deadline,
            task_id,
            timer_id,
        });

        timer_id
    }

    /// Get the current task ID (if executing).
    pub fn current_task_id(&self) -> Option<TaskId> {
        self.current_task
    }

    /// Mark a task as ready to run.
    pub fn wake(&mut self, task_id: TaskId) {
        if self.tasks.contains_key(&task_id) && !self.ready_queue.contains(&task_id) {
            self.ready_queue.push_back(task_id);
        }
    }

    /// Process any timers that have expired and wake their tasks.
    pub fn process_timers(&mut self) {
        let now = Instant::now();

        while let Some(entry) = self.timers.peek() {
            if entry.deadline <= now {
                let entry = self.timers.pop().unwrap();
                // Mark the timer future as ready
                if let Some(task) = self.tasks.get_mut(&entry.task_id) {
                    if let Some(future_state) = &task.waiting_on {
                        let mut state = future_state.borrow_mut();
                        if matches!(&*state, FutureState::Timer { .. }) {
                            *state = FutureState::Ready(Value::Unit);
                        }
                    }
                    // Note: Don't clear waiting_on here - run_task needs it to push the result
                }
                self.wake(entry.task_id);
            } else {
                break;
            }
        }
    }

    /// Get the next deadline (for sleeping until next timer).
    pub fn next_deadline(&self) -> Option<Instant> {
        self.timers.peek().map(|e| e.deadline)
    }

    /// Check if there are any tasks (ready or waiting).
    pub fn has_tasks(&self) -> bool {
        !self.tasks.is_empty()
    }

    /// Check if there are ready tasks.
    pub fn has_ready_tasks(&self) -> bool {
        !self.ready_queue.is_empty()
    }

    /// Pop the next ready task to run.
    pub fn pop_ready_task(&mut self) -> Option<TaskId> {
        self.ready_queue.pop_front()
    }

    /// Get a task by ID.
    pub fn get_task(&self, id: TaskId) -> Option<&Task> {
        self.tasks.get(&id)
    }

    /// Get a mutable task by ID.
    pub fn get_task_mut(&mut self, id: TaskId) -> Option<&mut Task> {
        self.tasks.get_mut(&id)
    }

    /// Set the current running task.
    pub fn set_current_task(&mut self, id: Option<TaskId>) {
        self.current_task = id;
    }

    /// Remove a completed task.
    pub fn remove_task(&mut self, id: TaskId) -> Option<Task> {
        self.tasks.remove(&id)
    }

    /// Check if a specific task is complete.
    pub fn is_task_complete(&self, id: TaskId) -> bool {
        self.tasks.get(&id).map(|t| t.result.is_some()).unwrap_or(true)
    }

    /// Get the result of a completed task.
    pub fn get_task_result(&self, id: TaskId) -> Option<Value> {
        self.tasks.get(&id).and_then(|t| t.result.clone())
    }

    /// Mark a task as complete with a result.
    /// Also wakes up any tasks waiting on this task's completion.
    pub fn complete_task(&mut self, id: TaskId, result: Value) {
        if let Some(task) = self.tasks.get_mut(&id) {
            task.result = Some(result.clone());
        }

        // Store result for any waiters
        if self.task_waiters.contains_key(&id) {
            self.completed_results.insert(id, result);
        }

        // Wake up any tasks waiting on this task
        if let Some(waiters) = self.task_waiters.remove(&id) {
            for waiter_id in waiters {
                self.wake(waiter_id);
            }
        }
    }

    /// Register a task as waiting on another task's completion.
    pub fn register_task_waiter(&mut self, target_task_id: TaskId, waiter_task_id: TaskId) {
        self.task_waiters
            .entry(target_task_id)
            .or_default()
            .push(waiter_task_id);
    }

    /// Get the result of a completed task (for join handles).
    pub fn get_completed_result(&mut self, task_id: TaskId) -> Option<Value> {
        // First check completed_results (for waited tasks)
        if let Some(result) = self.completed_results.remove(&task_id) {
            return Some(result);
        }
        // Fall back to task result
        self.tasks.get(&task_id).and_then(|t| t.result.clone())
    }

    /// Check if a task has completed (either in tasks or completed_results).
    pub fn has_task_completed(&self, task_id: TaskId) -> bool {
        self.completed_results.contains_key(&task_id)
            || self.tasks.get(&task_id).map(|t| t.result.is_some()).unwrap_or(false)
    }

    /// Suspend the current task, waiting on a future.
    pub fn suspend_current_task(
        &mut self,
        stack: Vec<Value>,
        frames: Vec<(Rc<Closure>, usize, usize)>,
        waiting_on: Rc<RefCell<FutureState>>,
    ) {
        if let Some(task_id) = self.current_task {
            if let Some(task) = self.tasks.get_mut(&task_id) {
                task.stack = stack;
                task.frames = frames;
                task.waiting_on = Some(waiting_on);
            }
        }
    }

    /// Get the number of pending tasks.
    pub fn pending_count(&self) -> usize {
        self.tasks.len()
    }

    /// Get the number of ready tasks.
    pub fn ready_count(&self) -> usize {
        self.ready_queue.len()
    }

    /// Get the number of pending timers.
    pub fn timer_count(&self) -> usize {
        self.timers.len()
    }
}
