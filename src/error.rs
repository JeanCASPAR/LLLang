use pest::error::{InputLocation, LineColLocation};

use std::num::ParseIntError;

use crate::parser::Rule;

#[derive(Debug, Clone, Copy)]
pub enum Loc<T> {
    Pos(T),
    Span(T, T),
}

impl<'a> From<pest::Span<'a>> for Loc<usize> {
    fn from(value: pest::Span) -> Self {
        Loc::Span(value.start(), value.end())
    }
}

impl<'a> From<(usize, usize)> for Loc<(usize, usize)> {
    fn from(value: (usize, usize)) -> Self {
        Loc::Pos(value)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct GlobalLoc {
    pos: Loc<usize>,
    line_column: Loc<(usize, usize)>,
}

impl GlobalLoc {
    pub fn new(pos: Loc<usize>, line_column: Loc<(usize, usize)>) -> Self {
        Self { pos, line_column }
    }
}

#[derive(Debug)]
pub struct Error {
    error_type: ErrorType,
    loc: GlobalLoc,
}

#[derive(Debug)]
pub enum ErrorType {
    PestError(pest::error::Error<Rule>),
    ParseError(ParseError),
}

impl ErrorType {
    pub fn report(self, pos: Loc<usize>, line_column: Loc<(usize, usize)>) -> Error {
        Error {
            error_type: self,
            loc: GlobalLoc { pos, line_column },
        }
    }
}

impl From<ParseError> for ErrorType {
    fn from(value: ParseError) -> Self {
        ErrorType::ParseError(value)
    }
}

impl From<pest::error::Error<Rule>> for Error {
    fn from(value: pest::error::Error<Rule>) -> Self {
        let line_column = match value.line_col {
            LineColLocation::Pos(n) => Loc::Pos(n),
            LineColLocation::Span(a, b) => Loc::Span(a, b),
        };
        let pos = match value.location {
            InputLocation::Pos(n) => Loc::Pos(n),
            InputLocation::Span((a, b)) => Loc::Span(a, b),
        };
        Error {
            error_type: ErrorType::PestError(value),
            loc: GlobalLoc { pos, line_column },
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    ParseInt(ParseIntError),
    ExpectedRule(Rule, Rule),
    TooManyChild(Rule),
    NotEnoughChild(Rule),
}

impl From<ParseIntError> for ParseError {
    fn from(value: ParseIntError) -> Self {
        ParseError::ParseInt(value)
    }
}
