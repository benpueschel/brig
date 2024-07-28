use brig_ast::*;
use brig_common::Span;
use brig_diagnostic::{Error, Result};
use brig_lexer::{Lexer, Token, TokenKind};
use brig_macros::verify_token;

mod decl;
mod expr;
mod stmt;

pub fn expected_ident(token: &Token) -> Error {
    Error::expected_token(
        token.kind.to_string(),
        vec!["identifier".to_string()],
        token.span,
    )
}

pub fn ident_from_token(token: Token) -> Result<Identifier> {
    match token.kind {
        TokenKind::Identifier(ident) => Ok(Identifier {
            name: ident,
            span: token.span,
        }),
        _ => Err(expected_ident(&token)),
    }
}

pub struct Parser {
    lexer: Lexer,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        Self { lexer }
    }

    pub fn peek(&self) -> Result<Token> {
        self.lexer.peek()
    }
    pub fn eat(&mut self) -> Result<Token> {
        self.lexer.next_token()
    }
    pub fn eat_if<F, R>(&mut self, mut f: F) -> Result<R>
    where
        F: FnMut(&Token) -> Result<R>,
    {
        let token = self.peek()?;
        let result = f(&token);
        if result.is_ok() {
            self.eat()?;
        }
        result
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        let mut program = Program::default();
        while self.peek()?.kind != TokenKind::EOF {
            program.declarations.push(self.parse_declaration()?);
        }
        Ok(program)
    }

    pub fn parse_block(&mut self) -> Result<Block> {
        verify_token!(self.eat()?, TokenKind::BraceOpen);

        let mut statements = Vec::new();
        while self.peek()?.kind != TokenKind::BraceClose {
            statements.push(self.parse_statement()?);
        }

        verify_token!(self.eat()?, TokenKind::BraceClose);

        Ok(Block {
            span: Span::compose(
                statements.first().map(|s| s.span()).unwrap_or_default(),
                statements.last().map(|s| s.span()).unwrap_or_default(),
            ),
            statements,
        })
    }

    pub fn parse_type(&mut self) -> Result<Ty> {
        if !matches!(self.peek()?.kind, TokenKind::Colon) {
            return Ok(Ty {
                kind: TyKind::Unspecified,
                span: Span::with_len(self.peek()?.span.start, 0),
            });
        }

        self.eat()?;
        let token = self.peek()?;
        match &token.kind {
            TokenKind::Identifier(_) => {
                let ident = ident_from_token(self.eat()?)?;
                let span = ident.span;
                let kind = match ident.name.as_str() {
                    "u32" => TyKind::Literal(LiteralType::U32),
                    _ => TyKind::UserDefined(ident),
                };
                Ok(Ty { kind, span })
            }
            _ => Err(expected_ident(&token)),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::*;
    #[test]
    pub fn parse_empty_type() {
        let input = "";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let ty = parser.parse_type().expect("Failed to parse type");
        assert_eq!(
            ty,
            Ty {
                kind: TyKind::Unspecified,
                span: Span::new(0, 0),
            }
        );
    }

    #[test]
    pub fn parse_u32_type() {
        let input = ": u32";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let ty = parser.parse_type().expect("Failed to parse type");
        assert_eq!(
            ty,
            Ty {
                kind: TyKind::Literal(LiteralType::U32),
                span: Span::new(2, 5),
            }
        );
    }

    #[test]
    pub fn parse_user_type() {
        let input = ": MyType";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let ty = parser.parse_type().expect("Failed to parse type");
        assert_eq!(
            ty,
            Ty {
                kind: TyKind::UserDefined(Identifier {
                    name: "MyType".to_string(),
                    span: Span::new(2, 8),
                }),
                span: Span::new(2, 8),
            }
        );
    }
}
