use crate::*;

impl Parser {
    pub fn parse_statement(&mut self) -> Result<Stmt> {
        let token = self.peek()?;
        match token.kind {
            // TODO: if statement
            TokenKind::Let => self.parse_variable_declaration(),
            TokenKind::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_expression_statement(&mut self) -> Result<Stmt> {
        let expr = self.parse_expression()?;
        match self.peek()?.kind {
            TokenKind::Semicolon => {
                // TODO: include semicolon in span
                self.eat()?;
                Ok(Stmt::Semi(expr))
            }
            _ => Ok(Stmt::Expr(expr)),
        }
    }

    pub fn parse_variable_declaration(&mut self) -> Result<Stmt> {
        // let [name] <: [type]> = [expr];
        let token = self.eat()?;
        let start = token.span;

        verify_token!(token, TokenKind::Let);
        let name = ident_from_token(self.eat()?)?;
        let ty = self.parse_type()?;
        let token = self.eat()?;
        if let TokenKind::Semicolon = token.kind {
            return Ok(Stmt::LetDecl(LetDecl {
                name,
                ty,
                expr: None,
                span: Span::compose(start, token.span),
            }));
        }

        let expr = self.parse_expression()?;
        verify_token!(self.eat()?, TokenKind::Semicolon);
        let span = Span::compose(start, expr.span());
        Ok(Stmt::LetDecl(LetDecl {
            name,
            ty,
            expr: Some(expr),
            span,
        }))
    }

    pub fn parse_return_statement(&mut self) -> Result<Stmt> {
        // return [expr];
        let token = self.eat()?;
        let start = token.span;

        verify_token!(token, TokenKind::Return);

        if let TokenKind::Semicolon = self.peek()?.kind {
            let span = Span::compose(start, self.peek()?.span);
            return Ok(Stmt::Return(ReturnStmt {
                expr: Expr::Lit(Lit {
                    value: LitVal::Unit,
                    ty: LitTy::Unit,
                    span,
                }),
                span,
            }));
        }

        let expr = self.parse_expression()?;
        verify_token!(self.eat()?, TokenKind::Semicolon);

        let span = Span::compose(start, expr.span());
        Ok(Stmt::Return(ReturnStmt { expr, span }))
    }
}

#[cfg(test)]
mod test {
    use brig_common::sym::Symbol;

    use crate::*;

    #[test]
    pub fn parse_literal_expression() {
        let input = "return var;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let statement = parser.parse_statement().expect("Failed to parse statement");
        assert_eq!(
            statement,
            Stmt::Return(ReturnStmt {
                expr: Expr::Ident(Ident {
                    name: Symbol::intern("var"),
                    span: Span::new(7, 10),
                }),
                span: Span::new(0, 10),
            })
        );
    }

    #[test]
    pub fn parse_usize_assignment() {
        let input = "let var: usize = 5;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let statement = parser.parse_statement().expect("Failed to parse statement");
        assert_eq!(
            statement,
            Stmt::LetDecl(LetDecl {
                name: Ident {
                    name: Symbol::intern("var"),
                    span: Span::new(4, 7),
                },
                ty: Ty {
                    kind: TyKind::Lit(LitTy::Uint(UintTy::Usize)),
                    span: Span::new(9, 14),
                },
                expr: Some(Expr::Lit(Lit {
                    value: LitVal::Int(IntLit { value: 5 }),
                    ty: LitTy::Unresolved,
                    span: Span::new(17, 18),
                })),
                span: Span::new(0, 18),
            })
        );
    }

    #[test]
    pub fn parse_assignment() {
        let input = "let var = 5;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let statement = parser.parse_statement().expect("Failed to parse statement");
        assert_eq!(
            statement,
            Stmt::LetDecl(LetDecl {
                name: Ident {
                    name: Symbol::intern("var"),
                    span: Span::new(4, 7),
                },
                ty: Ty {
                    kind: TyKind::Unspecified,
                    span: Span::new(8, 8),
                },
                expr: Some(Expr::Lit(Lit {
                    value: LitVal::Int(IntLit { value: 5 }),
                    ty: LitTy::Unresolved,
                    span: Span::new(10, 11),
                })),
                span: Span::new(0, 11),
            })
        );
    }

    #[test]
    pub fn parse_return_statement() {
        let input = "return 5;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let statement = parser.parse_statement().expect("Failed to parse statement");
        assert_eq!(
            statement,
            Stmt::Return(ReturnStmt {
                expr: Expr::Lit(Lit {
                    value: LitVal::Int(IntLit { value: 5 }),
                    ty: LitTy::Unresolved,
                    span: Span::new(7, 8),
                }),
                span: Span::new(0, 8),
            })
        );
    }
}
