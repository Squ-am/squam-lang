/// A span represents a range in the source code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span {
    /// Start byte offset (inclusive)
    pub start: u32,
    /// End byte offset (exclusive)
    pub end: u32,
    /// File identifier for multi-file support
    pub file_id: u16,
}

impl Span {
    /// Create a new span from byte offsets.
    pub fn new(start: usize, end: usize, file_id: u16) -> Self {
        Self {
            start: start as u32,
            end: end as u32,
            file_id,
        }
    }

    /// Create a dummy span for generated code or tests.
    pub fn dummy() -> Self {
        Self::default()
    }

    /// Merge two spans into one that covers both.
    pub fn merge(self, other: Span) -> Span {
        debug_assert_eq!(
            self.file_id, other.file_id,
            "Cannot merge spans from different files"
        );
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            file_id: self.file_id,
        }
    }

    /// Get the length of this span in bytes.
    pub fn len(&self) -> usize {
        (self.end - self.start) as usize
    }

    /// Check if this span is empty.
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Create a zero-width span at the start of this span.
    pub fn shrink_to_start(&self) -> Span {
        Span {
            start: self.start,
            end: self.start,
            file_id: self.file_id,
        }
    }

    /// Create a zero-width span at the end of this span.
    pub fn shrink_to_end(&self) -> Span {
        Span {
            start: self.end,
            end: self.end,
            file_id: self.file_id,
        }
    }
}

/// A value with an associated span.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    pub fn new(node: T, span: Span) -> Self {
        Self { node, span }
    }

    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned {
            node: f(self.node),
            span: self.span,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_merge() {
        let a = Span::new(0, 5, 0);
        let b = Span::new(3, 10, 0);
        let merged = a.merge(b);
        assert_eq!(merged.start, 0);
        assert_eq!(merged.end, 10);
    }

    #[test]
    fn test_span_len() {
        let span = Span::new(5, 15, 0);
        assert_eq!(span.len(), 10);
    }
}
