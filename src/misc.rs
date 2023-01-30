/// Shall be replaced by `pub type Never = !;` when
/// [feature(never_type)](https://github.com/rust-lang/rust/issues/35121) is stabilized
#[derive(Debug, Clone, Copy)]
pub enum Never {}

#[derive(Debug, Clone, Copy)]
pub enum Loc<T> {
    Pos(T),
    Span(T, T),
}

impl From<pest::Span<'_>> for Loc<usize> {
    fn from(value: pest::Span) -> Self {
        Loc::Span(value.start(), value.end())
    }
}

impl From<(usize, usize)> for Loc<(usize, usize)> {
    fn from(value: (usize, usize)) -> Self {
        Loc::Pos(value)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct GlobalLoc {
    pub pos: Loc<usize>,
    pub line_column: Loc<(usize, usize)>,
}

impl GlobalLoc {
    pub fn new(pos: Loc<usize>, line_column: Loc<(usize, usize)>) -> Self {
        Self { pos, line_column }
    }
}
