use std::error::Error as StdError;
use std::fmt::{self, Display, Formatter};
use std::fs;
use std::io::ErrorKind as IoErrorKind;
use std::path::Path;
use std::result::Result as StdResult;

use brig_common::span::Span;
use brig_common::Position;

pub type Result<T> = StdResult<T, Error>;

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorKind {
    /// Unexpected end of file
    UnexpectedEof,
    /// Expected a type
    ExpectedType,
    /// Type mismatch - expected, found
    TypeMismatch((String, Span), (String, Span)),
    /// Identifier is invalid (e.g. using a reserved keyword or starting with a number)
    InvalidIdentifier(String),
    /// Invalid number
    InvalidNumber(String),
    /// Unexpected symbol
    UnexpectedSymbol(char),
    /// Unexpected token
    UnexpectedToken(String),
    /// Unexpected token, expected one of the provided
    ExpectedToken(String, Vec<String>),
    /// Resource not found, such as a file
    ResourceNotFound(String),
    /// Other error
    Other(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

impl Error {
    pub fn new(kind: ErrorKind, span: Span) -> Self {
        Self { kind, span }
    }
    pub fn unexpected_eof(pos: usize) -> Self {
        Self::new(ErrorKind::UnexpectedEof, Span::new(pos, pos))
    }
    pub fn invalid_identifier(identifier: impl ToString, span: Span) -> Self {
        Self::new(ErrorKind::InvalidIdentifier(identifier.to_string()), span)
    }
    pub fn invalid_number(number: impl ToString, span: Span) -> Self {
        Self::new(ErrorKind::InvalidNumber(number.to_string()), span)
    }
    pub fn unexpected_symbol(symbol: char, pos: usize) -> Self {
        Self::new(ErrorKind::UnexpectedSymbol(symbol), Span::single(pos))
    }
    pub fn unexpected_token(token: impl ToString, span: Span) -> Self {
        Self::new(ErrorKind::UnexpectedToken(token.to_string()), span)
    }
    pub fn expected_type(span: Span) -> Self {
        Self::new(ErrorKind::ExpectedType, span)
    }
    pub fn type_mismatch(expected: (impl ToString, Span), found: (impl ToString, Span)) -> Self {
        let expected = (expected.0.to_string(), expected.1);
        let found = (found.0.to_string(), found.1);
        Self::new(ErrorKind::TypeMismatch(expected, found), Span::default())
    }
    pub fn expected_token(token: impl ToString, expected: Vec<String>, span: Span) -> Self {
        Self::new(ErrorKind::ExpectedToken(token.to_string(), expected), span)
    }
    pub fn resource_not_found(path: impl ToString, span: Span) -> Self {
        Self::new(ErrorKind::ResourceNotFound(path.to_string()), span)
    }
    pub fn other(message: impl ToString, span: Span) -> Self {
        Self::new(ErrorKind::Other(message.to_string()), span)
    }

    pub fn get_lines(&self, source: &str) -> (Position, Position) {
        let mut start = Position::new(0, 0);
        let mut end = Position::new(0, 0);
        let mut current_line = 0;
        for (i, c) in source.char_indices() {
            if i == self.span.start {
                start = Position::new(current_line, i);
            }
            if i == self.span.end {
                end = Position::new(current_line, i);
                break;
            }
            if c == '\n' {
                current_line += 1;
            }
        }
        (start, end)
    }

    pub fn span(&self) -> Span {
        self.span
    }
    pub fn kind(&self) -> &ErrorKind {
        &self.kind
    }
    pub fn with_kind(self, kind: ErrorKind) -> Self {
        Self { kind, ..self }
    }
    pub fn with_span(self, span: Span) -> Self {
        Self { span, ..self }
    }
}

impl Default for Error {
    fn default() -> Self {
        Self {
            kind: ErrorKind::Other(String::new()),
            span: Default::default(),
        }
    }
}

impl StdError for Error {}
impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Error")
    }
}

pub fn open_file<P>(path: P) -> Result<String>
where
    P: AsRef<Path> + Clone,
{
    let result = fs::read_to_string(path.clone());
    match result {
        Ok(x) => Ok(x),
        Err(e) => {
            let kind = match e.kind() {
                IoErrorKind::NotFound => {
                    ErrorKind::ResourceNotFound(format!("{}", path.as_ref().display()))
                }
                IoErrorKind::UnexpectedEof => ErrorKind::UnexpectedEof,
                _ => ErrorKind::Other(format!("{}", e)),
            };
            Err(Error {
                kind,
                span: Span::default(),
            })
        }
    }
}
