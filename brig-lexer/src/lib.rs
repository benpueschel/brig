use std::fmt::Formatter;
use std::fmt::{self, Display};

use brig_common::sym::Symbol;
use brig_common::Span;
use brig_diagnostic::{Error, Result};

#[cfg(test)]
mod test;

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn single(kind: TokenKind, pos: usize) -> Self {
        Self {
            kind,
            span: Span::single(pos),
        }
    }
    pub fn with_len(kind: TokenKind, pos: usize, len: usize) -> Self {
        Self {
            kind,
            span: Span::with_len(pos, len),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    Struct,
    Return,
    Extern,
    Fn,
    Let,
    If,
    Else,
    True,
    False,
    Integer(usize),
    Identifier(Symbol),
    Bang,
    Equal,
    Star,
    Percent,
    Slash,
    Plus,
    Minus,
    LeftCaret,
    RightCaret,
    ParenOpen,
    ParenClose,
    BracketOpen,
    BracketClose,
    BraceOpen,
    BraceClose,
    Comma,
    Semicolon,
    Colon,
    EOF,
    Invalid,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        match self {
            TokenKind::Integer(value) => write!(f, "int_lit({})", value),
            TokenKind::Identifier(name) => write!(f, "ident({})", *name.as_str()),
            x => write!(f, "{}", x.to_str()),
        }
    }
}

impl TokenKind {
    pub const fn to_str(&self) -> &str {
        match self {
            TokenKind::Struct => "struct",
            TokenKind::Extern => "extern",
            TokenKind::Return => "return",
            TokenKind::Fn => "fn",
            TokenKind::Let => "let",
            TokenKind::If => "if",
            TokenKind::Else => "else",
            TokenKind::True => "true",
            TokenKind::False => "false",
            TokenKind::Integer(_) => "int_lit",
            TokenKind::Identifier(_) => "ident",
            TokenKind::Bang => "!",
            TokenKind::Equal => "=",
            TokenKind::Star => "*",
            TokenKind::Percent => "%",
            TokenKind::Slash => "/",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::LeftCaret => "<",
            TokenKind::RightCaret => ">",
            TokenKind::ParenOpen => "(",
            TokenKind::ParenClose => ")",
            TokenKind::BracketOpen => "[",
            TokenKind::BracketClose => "]",
            TokenKind::BraceOpen => "{",
            TokenKind::BraceClose => "}",
            TokenKind::Comma => ",",
            TokenKind::Semicolon => ";",
            TokenKind::Colon => ":",
            TokenKind::EOF => "EOF",
            TokenKind::Invalid => "ERROR",
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Lexer {
    input: String,
    current_pos: usize,
}

impl Lexer {
    pub fn new(input: String) -> Self {
        Self {
            input,
            current_pos: 0,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>> {
        let mut tokens = Vec::new();
        loop {
            let token = self.next_token()?;
            if token.kind == TokenKind::EOF {
                break;
            }
            tokens.push(token);
        }
        Ok(tokens)
    }

    pub fn peek(&self) -> Result<Token> {
        let whitespaces = self.count_whitespaces();
        let i = self.current_pos + whitespaces;
        let next = match self.input[i..].chars().next() {
            None => {
                return Ok(Token {
                    kind: TokenKind::EOF,
                    span: Span::new(i, i),
                })
            }
            Some(x) => x,
        };

        match next {
            ',' => Ok(Token::single(TokenKind::Comma, i)),
            ';' => Ok(Token::single(TokenKind::Semicolon, i)),
            ':' => Ok(Token::single(TokenKind::Colon, i)),
            '!' => Ok(Token::single(TokenKind::Bang, i)),
            '=' => Ok(Token::single(TokenKind::Equal, i)),
            '(' => Ok(Token::single(TokenKind::ParenOpen, i)),
            ')' => Ok(Token::single(TokenKind::ParenClose, i)),
            '[' => Ok(Token::single(TokenKind::BracketOpen, i)),
            ']' => Ok(Token::single(TokenKind::BracketClose, i)),
            '{' => Ok(Token::single(TokenKind::BraceOpen, i)),
            '}' => Ok(Token::single(TokenKind::BraceClose, i)),
            '*' => Ok(Token::single(TokenKind::Star, i)),
            '%' => Ok(Token::single(TokenKind::Percent, i)),
            '/' => Ok(Token::single(TokenKind::Slash, i)),
            '+' => Ok(Token::single(TokenKind::Plus, i)),
            '-' => Ok(Token::single(TokenKind::Minus, i)),
            '<' => Ok(Token::single(TokenKind::LeftCaret, i)),
            '>' => Ok(Token::single(TokenKind::RightCaret, i)),

            c if c.is_ascii_digit() => self.tokenize_number(i),
            c @ '_' | c if c.is_alphabetic() => self.tokenize_ident(i),

            x => Err(Error::unexpected_symbol(x, i)),
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        let token = self.peek()?;

        self.current_pos = token.span.end;
        Ok(token)
    }

    pub fn tokenize_ident(&self, pos: usize) -> Result<Token> {
        // identifiers need to start with an alphabetic ascii char
        let data = &self.input[pos..];
        let mut illegal = false;
        match data.chars().next() {
            Some(x) if !x.is_alphabetic() => illegal = true,
            None => return Err(Error::unexpected_eof(pos)),
            _ => {}
        };

        let mut i = 0;
        let mut skip_keyword_val = false;
        for c in data.chars() {
            if c == '_' {
                // NOTE: no keyword contains '_', so we can skip validation
                skip_keyword_val = true;
                i += 1;
                continue;
            }
            if !c.is_alphanumeric() {
                break;
            }
            i += 1;
        }

        if illegal {
            return Err(Error::invalid_identifier(data, Span::with_len(pos, i)));
        }

        let identifier = &data[0..i];
        if skip_keyword_val {
            return Ok(Token {
                kind: TokenKind::Identifier(Symbol::intern(identifier)),
                span: Span::with_len(pos, i),
            });
        }

        Ok(Token {
            kind: match identifier {
                "struct" => TokenKind::Struct,
                "return" => TokenKind::Return,
                "let" => TokenKind::Let,
                "extern" => TokenKind::Extern,
                "fn" => TokenKind::Fn,
                "if" => TokenKind::If,
                "else" => TokenKind::Else,
                "true" => TokenKind::True,
                "false" => TokenKind::False,
                _ => TokenKind::Identifier(Symbol::intern(identifier)),
            },
            span: Span::with_len(pos, identifier.len()),
        })
    }

    fn tokenize_number(&self, start: usize) -> Result<Token> {
        let data = &self.input[start..];
        let mut illegal = false;

        match data.chars().next() {
            Some(x) if !x.is_numeric() => illegal = true,
            None => return Err(Error::unexpected_eof(start)),
            _ => {}
        };

        let mut i = 0;
        for c in data.chars() {
            // TODO: support _ and . in numbers
            if !c.is_numeric() {
                break;
            }
            i += 1;
        }

        if illegal {
            return Err(Error::invalid_number(data, Span::with_len(start, i)));
        }

        let decimal = &data[0..i];

        match decimal.parse() {
            Ok(value) => Ok(Token {
                kind: TokenKind::Integer(value),
                span: Span::with_len(start, i),
            }),
            Err(_) => Err(Error::invalid_number(decimal, Span::with_len(start, i))),
        }
    }

    fn count_whitespaces(&self) -> usize {
        let mut i = 0;
        for c in self.input[self.current_pos..].chars() {
            if !c.is_whitespace() {
                break;
            }
            i += 1;
        }
        i
    }
}
