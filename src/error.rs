use pest::error::{InputLocation, LineColLocation};

use std::num::ParseIntError;

use crate::{
    ast::*,
    misc::*,
    parser::{ParserStage, Rule},
    type_check::TypeCheckStage,
};

#[derive(Debug)]
pub struct Error {
    pub error_type: ErrorType,
    pub loc: GlobalLoc,
}

#[derive(Debug)]
pub enum ErrorType {
    Pest(pest::error::Error<Rule>),
    Parse(ParseError),
    Type(TypeError),
}

impl ErrorType {
    pub fn report(self, pos: Loc<usize>, line_column: Loc<(usize, usize)>) -> Error {
        Error {
            error_type: self,
            loc: GlobalLoc { pos, line_column },
        }
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
            error_type: ErrorType::Pest(value),
            loc: GlobalLoc { pos, line_column },
        }
    }
}

impl From<ParseError> for ErrorType {
    fn from(value: ParseError) -> Self {
        ErrorType::Parse(value)
    }
}

impl From<TypeError> for ErrorType {
    fn from(value: TypeError) -> Self {
        ErrorType::Type(value)
    }
}

#[derive(Debug)]
pub enum ParseError {
    ParseInt(ParseIntError),
    /// Expected [first rule], found [second rule]
    ExpectedRule(Rule, Rule),
    TooManyChild(Rule),
    NotEnoughChild(Rule),
}

impl From<ParseIntError> for ParseError {
    fn from(value: ParseIntError) -> Self {
        ParseError::ParseInt(value)
    }
}

#[derive(Debug)]
pub enum TypeError {
    TypeVarWithParam(usize),
    /// Type [ident] was expected of arity [second usize], not [third usize]
    ExpectedArity(usize, usize, usize),
    UnknownType(usize),
    NonCompatibleType(Pattern<ParserStage>, Type<TypeCheckStage>),
    NonInfalliblePattern(Pattern<ParserStage>, Type<TypeCheckStage>),
    /// First binding and pos
    MultipleBindingPattern(usize, GlobalLoc),
}
