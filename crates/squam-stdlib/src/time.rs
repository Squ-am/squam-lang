use squam_vm::{Value, VM};
use std::rc::Rc;
use std::thread;
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

pub fn register(vm: &mut VM) {
    // time_now() -> int (unix timestamp in seconds)
    vm.define_native("time_now", 0, |_args| {
        match SystemTime::now().duration_since(UNIX_EPOCH) {
            Ok(d) => Ok(Value::Int(d.as_secs() as i64)),
            Err(_) => Ok(Value::Int(0)),
        }
    });

    // time_now_ms() -> int (unix timestamp in milliseconds)
    vm.define_native("time_now_ms", 0, |_args| {
        match SystemTime::now().duration_since(UNIX_EPOCH) {
            Ok(d) => Ok(Value::Int(d.as_millis() as i64)),
            Err(_) => Ok(Value::Int(0)),
        }
    });

    // time_now_ns() -> int (unix timestamp in nanoseconds)
    vm.define_native("time_now_ns", 0, |_args| {
        match SystemTime::now().duration_since(UNIX_EPOCH) {
            Ok(d) => Ok(Value::Int(d.as_nanos() as i64)),
            Err(_) => Ok(Value::Int(0)),
        }
    });

    // sleep(ms: int) -> ()
    vm.define_native("sleep", 1, |args| match &args[0] {
        Value::Int(ms) => {
            if *ms > 0 {
                thread::sleep(Duration::from_millis(*ms as u64));
            }
            Ok(Value::Unit)
        }
        _ => Err("sleep: expected int (milliseconds)".to_string()),
    });

    // sleep_secs(secs: float) -> ()
    vm.define_native("sleep_secs", 1, |args| match &args[0] {
        Value::Float(secs) => {
            if *secs > 0.0 {
                thread::sleep(Duration::from_secs_f64(*secs));
            }
            Ok(Value::Unit)
        }
        Value::Int(secs) => {
            if *secs > 0 {
                thread::sleep(Duration::from_secs(*secs as u64));
            }
            Ok(Value::Unit)
        }
        _ => Err("sleep_secs: expected number".to_string()),
    });

    // Instant-based timing for performance measurement
    lazy_static::lazy_static! {
        static ref TIMER_START: std::sync::Mutex<Option<Instant>> = std::sync::Mutex::new(None);
    }

    // timer_start() -> ()
    vm.define_native("timer_start", 0, |_args| {
        *TIMER_START.lock().unwrap() = Some(Instant::now());
        Ok(Value::Unit)
    });

    // timer_elapsed_ms() -> int
    vm.define_native("timer_elapsed_ms", 0, |_args| {
        match *TIMER_START.lock().unwrap() {
            Some(start) => Ok(Value::Int(start.elapsed().as_millis() as i64)),
            None => Err("timer_elapsed_ms: timer not started".to_string()),
        }
    });

    // timer_elapsed_us() -> int (microseconds)
    vm.define_native("timer_elapsed_us", 0, |_args| {
        match *TIMER_START.lock().unwrap() {
            Some(start) => Ok(Value::Int(start.elapsed().as_micros() as i64)),
            None => Err("timer_elapsed_us: timer not started".to_string()),
        }
    });

    // timer_elapsed_secs() -> float
    vm.define_native("timer_elapsed_secs", 0, |_args| {
        match *TIMER_START.lock().unwrap() {
            Some(start) => Ok(Value::Float(start.elapsed().as_secs_f64())),
            None => Err("timer_elapsed_secs: timer not started".to_string()),
        }
    });

    // Date/time formatting (basic)
    // format_timestamp(unix_secs: int, format: string) -> string
    // Format: %Y=year, %m=month, %d=day, %H=hour, %M=minute, %S=second
    vm.define_native("format_timestamp", 2, |args| {
        match (&args[0], &args[1]) {
            (Value::Int(ts), Value::String(fmt)) => {
                // Convert unix timestamp to components (simplified, UTC only)
                let secs = *ts;

                // Days since epoch
                let days = secs / 86400;
                let time_of_day = secs % 86400;

                let hours = time_of_day / 3600;
                let minutes = (time_of_day % 3600) / 60;
                let seconds = time_of_day % 60;

                // Calculate year/month/day from days since epoch (1970-01-01)
                let (year, month, day) = days_to_ymd(days);

                let result = fmt
                    .replace("%Y", &format!("{:04}", year))
                    .replace("%m", &format!("{:02}", month))
                    .replace("%d", &format!("{:02}", day))
                    .replace("%H", &format!("{:02}", hours))
                    .replace("%M", &format!("{:02}", minutes))
                    .replace("%S", &format!("{:02}", seconds));

                Ok(Value::String(Rc::new(result)))
            }
            _ => Err("format_timestamp: expected (int, string)".to_string()),
        }
    });
}

// Helper to convert days since epoch to year/month/day
fn days_to_ymd(days: i64) -> (i64, i64, i64) {
    let mut remaining = days;
    let mut year = 1970;

    loop {
        let days_in_year = if is_leap_year(year) { 366 } else { 365 };
        if remaining < days_in_year {
            break;
        }
        remaining -= days_in_year;
        year += 1;
    }

    let days_in_months = if is_leap_year(year) {
        [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    } else {
        [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    };

    let mut month = 1;
    for days_in_month in days_in_months.iter() {
        if remaining < *days_in_month {
            break;
        }
        remaining -= days_in_month;
        month += 1;
    }

    (year, month, remaining + 1)
}

fn is_leap_year(year: i64) -> bool {
    (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)
}
