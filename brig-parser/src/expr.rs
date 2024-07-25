use crate::*;

impl Parser {
    pub fn parse_expression(&mut self) -> Result<Expression> {
        println!("parse_expression");
        self.parse_mult_expression()
    }

    pub fn parse_mult_expression(&mut self) -> Result<Expression> {
        println!("parse_mult_expression");
        let mut left = self.parse_primary_expression()?;
        let mut peek = self.peek()?;
        while peek.kind == TokenKind::Star || peek.kind == TokenKind::Slash {
            println!("  {:?}", peek);
            let op = self.parse_binary_operator()?;

            let right = self.parse_expression()?;
            let span = Span::compose(left.span(), right.span());
            left = Expression::Binary(BinaryExpression {
                lhs: Box::new(left),
                rhs: Box::new(right),
                op,
                span,
            });
            peek = self.peek()?;
        }
        Ok(left)
    }

    pub fn parse_primary_expression(&mut self) -> Result<Expression> {
        let token = self.eat()?;
        println!("parse_primary_expression: {:?}", token);
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
