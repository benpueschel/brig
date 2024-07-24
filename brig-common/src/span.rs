#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl From<(usize, usize)> for Span {
    fn from((start, end): (usize, usize)) -> Self {
        Self { start, end }
    }
}

impl From<Span> for (usize, usize) {
    fn from(span: Span) -> Self {
        (span.start, span.end)
    }
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
    pub fn with_len(start: usize, len: usize) -> Self {
        Self {
            start,
            end: start + len,
        }
    }
    /// Composes two spans into a single span that fully contains both.
    ///
    /// # Examples
    /// ```rust
    /// # use brig_common::Span;
    /// let a = Span::new(0, 5);
    /// let b = Span::new(3, 8);
    /// let c = Span::compose(a, b);
    /// assert_eq!(c, Span::new(0, 8));
    /// ```
    /// - `a` and `b` partially overlap, where `a` starts before `b` and `b` ends after `a`.
    ///
    /// ```rust
    /// # use brig_common::Span;
    /// let a = Span::new(3, 8);
    /// let b = Span::new(0, 5);
    /// let c = Span::compose(a, b);
    /// assert_eq!(c, Span::new(0, 8));
    /// ```
    /// - `a` and `b` partially overlap, where `a` starts *after* `b` and `b` ends *before* `a`.
    ///
    /// ```rust
    /// # use brig_common::Span;
    /// let a = Span::new(3, 5);
    /// let b = Span::new(0, 8);
    /// let c = Span::compose(a, b);
    /// assert_eq!(c, Span::new(0, 8));
    /// ```
    /// - `a` is fully contained within `b`.
    pub fn compose(a: Span, b: Span) -> Span {
        Self {
            start: a.start.min(b.start),
            end: b.end.max(a.end),
        }
    }
    pub fn single(pos: usize) -> Self {
        Self::new(pos, pos + 1)
    }
    pub fn len(&self) -> usize {
        self.end - self.start
    }
    pub fn is_empty(&self) -> bool {
        self.start == self.end
    }
}
