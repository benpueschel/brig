use thin_vec::thin_vec;

use crate::*;

impl Parser {
    pub fn parse_struct_init_expression(&mut self) -> Result<Expr> {
        let mut span = self.peek()?.span;
        verify_token!(self.eat()?, TokenKind::Struct);

        let name = ident_from_token(self.eat()?)?;

        verify_token!(self.eat()?, TokenKind::BraceOpen);

        // Parse struct fields
        let mut fields = thin_vec![];
        while self.peek()?.kind != TokenKind::BraceClose {
            let name = ident_from_token(self.eat()?)?;
            verify_token!(self.eat()?, TokenKind::Colon);
            let expr = self.parse_expression()?;

            fields.push(FieldInit { name, expr });

            match self.peek()?.kind {
                TokenKind::Comma => _ = self.eat()?,
                TokenKind::BraceClose => break,
                x => {
                    return Err(Error::expected_token(
                        x,
                        vec![",".to_string()],
                        self.peek()?.span,
                    ))
                }
            }
        }

        span = Span::compose(span, self.peek()?.span);
        verify_token!(self.eat()?, TokenKind::BraceClose);

        let init = StructInit {
            fields,
            span,
            name,
            def: None,
            ty: None,
        };

        Ok(Expr::AdtInit(AdtInit::Struct(init)))
    }
}

#[cfg(test)]
mod test {
    use crate::*;
    use brig_common::sym;
    use brig_lexer::Lexer;
    use thin_vec::thin_vec;

    #[test]
    fn parse_struct_init() {
        let input = "struct Foo { x: 42, y: 43 }";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let expr = parser
            .parse_expression()
            .expect("failed to parse expression");

        assert_eq!(expr.span(), Span::with_len(0, 27));
        assert_eq!(
            expr,
            Expr::AdtInit(AdtInit::Struct(StructInit {
                name: Ident {
                    name: sym!("Foo"),
                    span: Span::with_len(7, 3)
                },
                fields: thin_vec![
                    FieldInit {
                        name: Ident {
                            name: sym!("x"),
                            span: Span::with_len(13, 1)
                        },
                        expr: Expr::Lit(Lit {
                            value: LitVal::Int(IntLit { value: 42 }),
                            ty: LitTy::Unresolved,
                            span: Span::with_len(16, 2)
                        })
                    },
                    FieldInit {
                        name: Ident {
                            name: sym!("y"),
                            span: Span::with_len(20, 1)
                        },
                        expr: Expr::Lit(Lit {
                            value: LitVal::Int(IntLit { value: 43 }),
                            ty: LitTy::Unresolved,
                            span: Span::with_len(23, 2)
                        })
                    }
                ],
                span: Span::with_len(0, 27),
                def: None,
                ty: None
            }))
        )
    }
}
