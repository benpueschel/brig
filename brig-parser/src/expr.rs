use crate::*;

type ExprFn = fn(&mut Parser) -> Result<Expression>;

impl Parser {
    pub fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_assignment_expression()
    }

    fn parse_binary_expression(
        &mut self,
        next: ExprFn,
        pred: fn(this: &Self) -> Result<bool>,
    ) -> Result<Expression> {
        let mut left = next(self)?;
        while pred(self)? {
            let op = self.parse_binary_operator()?;
            let right = self.parse_expression()?;
            let span = Span::compose(left.span(), right.span());
            left = Expression::Binary(BinaryExpression {
                lhs: Box::new(left),
                rhs: Box::new(right),
                ty_kind: None,
                op,
                span,
            });
        }
        Ok(left)
    }
    pub fn parse_assignment_expression(&mut self) -> Result<Expression> {
        self.parse_binary_expression(Parser::parse_comparison_expression, |this| {
            let x = this.peek()?.kind;
            Ok(x == TokenKind::Equal)
        })
    }
    pub fn parse_comparison_expression(&mut self) -> Result<Expression> {
        self.parse_binary_expression(Parser::parse_add_expression, |this| {
            let x = this.peek()?.kind;
            Ok(x == TokenKind::LeftCaret || x == TokenKind::RightCaret)
        })
    }
    pub fn parse_add_expression(&mut self) -> Result<Expression> {
        self.parse_binary_expression(Parser::parse_mult_expression, |this| {
            let x = this.peek()?.kind;
            Ok(x == TokenKind::Plus || x == TokenKind::Minus)
        })
    }

    pub fn parse_mult_expression(&mut self) -> Result<Expression> {
        self.parse_binary_expression(Parser::parse_call_expression, |this| {
            let x = this.peek()?.kind;
            Ok(x == TokenKind::Star || x == TokenKind::Slash)
        })
    }

    pub fn parse_call_expression(&mut self) -> Result<Expression> {
        // TODO: for now, we don't support namespacing or structs with methods or fancy stuff
        // like that. We'd need to parse all that into a PathExpression, then parse the call
        // on that path.
        // For now, we'll just parse a single identifier as the callee.

        let token = self.peek()?;
        let span = token.span;

        let callee = ident_from_token(token);
        if callee.is_err() {
            // This doesn't look like a call expression, so just parse the primary expression
            // and return it.
            return self.parse_primary_expression();
        }
        let callee = callee.unwrap();
        let _ = self.eat()?; // eat the identifier

        if self.peek()?.kind != TokenKind::ParenOpen {
            // This isn't a call expression, so just return the callee as an identifier.
            return Ok(Expression::Identifier(callee));
        }
        let args = self.parse_punctuated_list(TokenKind::Comma, Parser::parse_expression)?;
        let span = Span::compose(span, args.span);

        let fn_ty = None;
        Ok(Expression::Call(CallExpression {
            callee,
            args,
            fn_ty,
            span,
        }))
    }

    pub fn parse_primary_expression(&mut self) -> Result<Expression> {
        // TODO: probably refactor this to be more betterer.
        match self.peek()?.kind {
            TokenKind::BraceOpen => {
                let block = self.parse_block()?;
                return Ok(Expression::Block(block));
            }
            TokenKind::ParenOpen => return self.parse_paren_expression(),
            _ => {}
        }

        let token = self.eat()?;
        match token.kind {
            TokenKind::Identifier(_) => Ok(Expression::Identifier(ident_from_token(token)?)),
            TokenKind::Integer(value) => Ok(Expression::Literal(Literal {
                value: LiteralValue::Int(IntLit { value }),
                ty: LiteralType::Unresolved,
                span: token.span,
            })),
            x => Err(Error::expected_token(
                x.to_string(),
                vec![
                    "ident".to_string(),
                    TokenKind::Integer(0).to_str().to_string(),
                    TokenKind::BraceOpen.to_str().to_string(),
                    TokenKind::ParenOpen.to_str().to_string(),
                ],
                token.span,
            )),
        }
    }

    pub fn parse_paren_expression(&mut self) -> Result<Expression> {
        verify_token!(self.eat()?, TokenKind::ParenOpen);
        let expr = self.parse_expression()?;
        verify_token!(self.eat()?, TokenKind::ParenClose);
        Ok(expr)
    }

    pub fn parse_binary_operator(&mut self) -> Result<BinaryOperator> {
        let token = self.eat()?;

        // TODO: parse a second operator (call parse_binary_operator again, then match it's
        // operator with the first operator)
        match token.kind {
            TokenKind::Star => Ok(BinaryOperator::Multiply),
            TokenKind::Slash => Ok(BinaryOperator::Divide),
            TokenKind::Plus => Ok(BinaryOperator::Add),
            TokenKind::Minus => Ok(BinaryOperator::Subtract),
            TokenKind::Equal => Ok(BinaryOperator::Assign),
            TokenKind::LeftCaret => Ok(BinaryOperator::LessThan),
            TokenKind::RightCaret => Ok(BinaryOperator::GreaterThan),

            x => Err(Error::expected_token(
                x.to_string(),
                vec![
                    TokenKind::Star.to_str().to_string(),
                    TokenKind::Slash.to_str().to_string(),
                    TokenKind::Plus.to_str().to_string(),
                    TokenKind::Minus.to_str().to_string(),
                    TokenKind::Equal.to_str().to_string(),
                ],
                token.span,
            )),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::*;

    fn test_base(input: &str, expected: Expression) {
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let expr = parser
            .parse_expression()
            .expect("Failed to parse expression");

        assert_eq!(expr, expected);
    }

    #[test]
    pub fn parse_assignment_expr() {
        let input = "x = y";
        test_base(
            input,
            Expression::Binary(BinaryExpression {
                lhs: Box::new(Expression::Identifier(Identifier {
                    name: "x".to_string(),
                    span: Span::new(0, 1),
                })),
                rhs: Box::new(Expression::Identifier(Identifier {
                    name: "y".to_string(),
                    span: Span::new(4, 5),
                })),
                op: BinaryOperator::Assign,
                span: Span::new(0, 5),
                ty_kind: None,
            }),
        );
    }

    #[test]
    pub fn parse_comparison_expr() {
        let input = "x < 10";
        test_base(
            input,
            Expression::Binary(BinaryExpression {
                lhs: Box::new(Expression::Identifier(Identifier {
                    name: "x".to_string(),
                    span: Span::new(0, 1),
                })),
                rhs: Box::new(Expression::Literal(Literal {
                    value: LiteralValue::Int(IntLit { value: 10 }),
                    ty: LiteralType::Unresolved,
                    span: Span::new(4, 6),
                })),
                op: BinaryOperator::LessThan,
                span: Span::new(0, 6),
                ty_kind: None,
            }),
        );
    }

    #[test]
    pub fn parse_add_expr() {
        let input = "x + 5";
        test_base(
            input,
            Expression::Binary(BinaryExpression {
                lhs: Box::new(Expression::Identifier(Identifier {
                    name: "x".to_string(),
                    span: Span::new(0, 1),
                })),
                rhs: Box::new(Expression::Literal(Literal {
                    value: LiteralValue::Int(IntLit { value: 5 }),
                    ty: LiteralType::Unresolved,
                    span: Span::new(4, 5),
                })),
                op: BinaryOperator::Add,
                span: Span::new(0, 5),
                ty_kind: None,
            }),
        );
    }

    #[test]
    pub fn parse_mult_expr() {
        let input = "10 * 283";
        test_base(
            input,
            Expression::Binary(BinaryExpression {
                lhs: Box::new(Expression::Literal(Literal {
                    value: LiteralValue::Int(IntLit { value: 10 }),
                    ty: LiteralType::Unresolved,
                    span: Span::new(0, 2),
                })),
                rhs: Box::new(Expression::Literal(Literal {
                    value: LiteralValue::Int(IntLit { value: 283 }),
                    ty: LiteralType::Unresolved,
                    span: Span::new(5, 8),
                })),
                op: BinaryOperator::Multiply,
                span: Span::new(0, 8),
                ty_kind: None,
            }),
        );
    }

    #[test]
    pub fn parse_primary_expr() {
        let input = "x";
        test_base(
            input,
            Expression::Identifier(Identifier {
                name: "x".to_string(),
                span: Span::new(0, 1),
            }),
        );
    }

    #[test]
    pub fn parse_paren_expr() {
        let input = "(x + 5) * 3";
        test_base(
            input,
            Expression::Binary(BinaryExpression {
                lhs: Box::new(Expression::Binary(BinaryExpression {
                    lhs: Box::new(Expression::Identifier(Identifier {
                        name: "x".to_string(),
                        span: Span::new(1, 2),
                    })),
                    rhs: Box::new(Expression::Literal(Literal {
                        value: LiteralValue::Int(IntLit { value: 5 }),
                        ty: LiteralType::Unresolved,
                        span: Span::new(5, 6),
                    })),
                    op: BinaryOperator::Add,
                    span: Span::new(1, 6),
                    ty_kind: None,
                })),
                rhs: Box::new(Expression::Literal(Literal {
                    value: LiteralValue::Int(IntLit { value: 3 }),
                    ty: LiteralType::Unresolved,
                    span: Span::new(10, 11),
                })),
                op: BinaryOperator::Multiply,
                ty_kind: None,
                span: Span::new(1, 11),
            }),
        );
    }

    #[test]
    pub fn parse_assignment_binary_expr() {
        let input = "x = 5 + 10";
        test_base(
            input,
            Expression::Binary(BinaryExpression {
                lhs: Box::new(Expression::Identifier(Identifier {
                    name: "x".to_string(),
                    span: Span::new(0, 1),
                })),
                rhs: Box::new(Expression::Binary(BinaryExpression {
                    lhs: Box::new(Expression::Literal(Literal {
                        value: LiteralValue::Int(IntLit { value: 5 }),
                        ty: LiteralType::Unresolved,
                        span: Span::new(4, 5),
                    })),
                    rhs: Box::new(Expression::Literal(Literal {
                        value: LiteralValue::Int(IntLit { value: 10 }),
                        ty: LiteralType::Unresolved,
                        span: Span::new(8, 10),
                    })),
                    op: BinaryOperator::Add,
                    span: Span::new(4, 10),
                    ty_kind: None,
                })),
                op: BinaryOperator::Assign,
                span: Span::new(0, 10),
                ty_kind: None,
            }),
        );
    }

    #[test]
    pub fn parse_mixed_expr() {
        let input = "x + 5 * 10";
        test_base(
            input,
            Expression::Binary(BinaryExpression {
                lhs: Box::new(Expression::Identifier(Identifier {
                    name: "x".to_string(),
                    span: Span::new(0, 1),
                })),
                rhs: Box::new(Expression::Binary(BinaryExpression {
                    lhs: Box::new(Expression::Literal(Literal {
                        value: LiteralValue::Int(IntLit { value: 5 }),
                        ty: LiteralType::Unresolved,
                        span: Span::new(4, 5),
                    })),
                    rhs: Box::new(Expression::Literal(Literal {
                        value: LiteralValue::Int(IntLit { value: 10 }),
                        ty: LiteralType::Unresolved,
                        span: Span::new(8, 10),
                    })),
                    op: BinaryOperator::Multiply,
                    span: Span::new(4, 10),
                    ty_kind: None,
                })),
                op: BinaryOperator::Add,
                span: Span::new(0, 10),
                ty_kind: None,
            }),
        );
    }
}
