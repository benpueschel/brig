use crate::*;

type ExprFn = fn(&mut Parser) -> Result<Expression>;

impl Parser {
    pub fn parse_expression(&mut self) -> Result<Expression> {
        self.parse_add_expression()
    }

    fn parse_binary_expression(
        &mut self,
        next: ExprFn,
        pred: fn(TokenKind) -> bool,
    ) -> Result<Expression> {
        let mut left = next(self)?;
        while pred(self.peek()?.kind) {
            let op = self.parse_binary_operator()?;
            let right = self.parse_expression()?;
            let span = Span::compose(left.span(), right.span());
            left = Expression::Binary(BinaryExpression {
                lhs: Box::new(left),
                rhs: Box::new(right),
                op,
                span,
            });
        }
        Ok(left)
    }
    pub fn parse_add_expression(&mut self) -> Result<Expression> {
        self.parse_binary_expression(Parser::parse_mult_expression, |x| {
            x == TokenKind::Plus || x == TokenKind::Minus
        })
    }

    pub fn parse_mult_expression(&mut self) -> Result<Expression> {
        self.parse_binary_expression(Parser::parse_primary_expression, |x| {
            x == TokenKind::Star || x == TokenKind::Slash
        })
    }

    pub fn parse_primary_expression(&mut self) -> Result<Expression> {
        let token = self.eat()?;
        match token.kind {
            TokenKind::ParenOpen => self.parse_paren_expression(),
            TokenKind::Identifier(_) => Ok(Expression::Identifier(ident_from_token(token)?)),
            TokenKind::Integer(value) => Ok(Expression::Literal(Literal {
                // TODO: don't default to U32
                value: LiteralValue::U32(value as u32),
                ty: LiteralType::U32,
                span: token.span,
            })),
            x => Err(Error::expected_token(
                x.to_string(),
                vec![
                    "ident".to_string(),
                    TokenKind::Integer(0).to_str().to_string(),
                ],
                token.span,
            )),
        }
    }

    pub fn parse_paren_expression(&mut self) -> Result<Expression> {
        // TODO: verify that the token is actually a ParenOpen
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
    #[test]
    pub fn parse_add_expr() {
        let input = "x + 5";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let expr = parser
            .parse_expression()
            .expect("Failed to parse expression");

        assert_eq!(
            expr,
            Expression::Binary(BinaryExpression {
                lhs: Box::new(Expression::Identifier(Identifier {
                    name: "x".to_string(),
                    span: Span::new(0, 1),
                })),
                rhs: Box::new(Expression::Literal(Literal {
                    value: LiteralValue::U32(5),
                    ty: LiteralType::U32,
                    span: Span::new(4, 5),
                })),
                op: BinaryOperator::Add,
                span: Span::new(0, 5),
            })
        );
    }

    #[test]
    pub fn parse_mult_expr() {
        let input = "10 * 283";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let expr = parser
            .parse_expression()
            .expect("Failed to parse expression");

        assert_eq!(
            expr,
            Expression::Binary(BinaryExpression {
                lhs: Box::new(Expression::Literal(Literal {
                    value: LiteralValue::U32(10),
                    ty: LiteralType::U32,
                    span: Span::new(0, 2),
                })),
                rhs: Box::new(Expression::Literal(Literal {
                    value: LiteralValue::U32(283),
                    ty: LiteralType::U32,
                    span: Span::new(5, 8),
                })),
                op: BinaryOperator::Multiply,
                span: Span::new(0, 8),
            })
        );
    }

    #[test]
    pub fn parse_primary_expr() {
        let input = "x";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let expr = parser
            .parse_expression()
            .expect("Failed to parse expression");

        assert_eq!(
            expr,
            Expression::Identifier(Identifier {
                name: "x".to_string(),
                span: Span::new(0, 1),
            })
        );
    }

    #[test]
    pub fn parse_paren_expr() {
        let input = "(x + 5)";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let expr = parser
            .parse_expression()
            .expect("Failed to parse expression");

        assert_eq!(
            expr,
            Expression::Binary(BinaryExpression {
                lhs: Box::new(Expression::Identifier(Identifier {
                    name: "x".to_string(),
                    span: Span::new(1, 2),
                })),
                rhs: Box::new(Expression::Literal(Literal {
                    value: LiteralValue::U32(5),
                    ty: LiteralType::U32,
                    span: Span::new(5, 6),
                })),
                op: BinaryOperator::Add,
                span: Span::new(1, 6),
            })
        );
    }

    #[test]
    pub fn parse_mixed_expr() {
        let input = "x + 5 * 10";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let expr = parser
            .parse_expression()
            .expect("Failed to parse expression");

        assert_eq!(
            expr,
            Expression::Binary(BinaryExpression {
                lhs: Box::new(Expression::Identifier(Identifier {
                    name: "x".to_string(),
                    span: Span::new(0, 1),
                })),
                rhs: Box::new(Expression::Binary(BinaryExpression {
                    lhs: Box::new(Expression::Literal(Literal {
                        value: LiteralValue::U32(5),
                        ty: LiteralType::U32,
                        span: Span::new(4, 5),
                    })),
                    rhs: Box::new(Expression::Literal(Literal {
                        value: LiteralValue::U32(10),
                        ty: LiteralType::U32,
                        span: Span::new(8, 10),
                    })),
                    op: BinaryOperator::Multiply,
                    span: Span::new(4, 10),
                })),
                op: BinaryOperator::Add,
                span: Span::new(0, 10),
            })
        );
    }
}
