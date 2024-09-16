use crate::*;

type ExprFn = fn(&mut Parser) -> Result<Expr>;

impl Parser {
    pub fn parse_expression(&mut self) -> Result<Expr> {
        match self.peek()?.kind {
            TokenKind::If => self.parse_if_expression(),
            _ => self.parse_assignment_expression(),
        }
    }

    fn parse_if_expression(&mut self) -> Result<Expr> {
        let token = self.eat()?;
        let start = token.span;

        verify_token!(token, TokenKind::If);
        let cond = self.parse_expression()?;
        let then_block = self.parse_block()?;
        let mut span = Span::compose(start, then_block.span());

        let else_block = if self.peek()?.kind == TokenKind::Else {
            self.eat()?;
            let else_block = self.parse_block()?;
            span = Span::compose(span, else_block.span());
            Some(else_block)
        } else {
            None
        };

        Ok(Expr::If(IfExpr {
            cond: Box::new(cond),
            then_block,
            else_block,
            span,
        }))
    }

    fn parse_binary_expression(
        &mut self,
        next: ExprFn,
        pred: fn(this: &Self) -> Result<bool>,
    ) -> Result<Expr> {
        let mut left = next(self)?;
        while pred(self)? {
            let op = self.parse_binary_operator()?;
            let right = self.parse_expression()?;
            let span = Span::compose(left.span(), right.span());
            left = Expr::Bin(BinExpr {
                lhs: Box::new(left),
                rhs: Box::new(right),
                ty_kind: None,
                op,
                span,
            });
        }
        Ok(left)
    }
    pub fn parse_assignment_expression(&mut self) -> Result<Expr> {
        let mut left = self.parse_comparison_expression()?;
        while self.peek()?.kind == TokenKind::Equal {
            verify_token!(self.eat()?, TokenKind::Equal);
            let right = self.parse_expression()?;
            left = Expr::Assign(Box::new(left), Box::new(right));
        }
        Ok(left)
    }
    pub fn parse_comparison_expression(&mut self) -> Result<Expr> {
        self.parse_binary_expression(Parser::parse_add_expression, |this| {
            let x = this.peek()?.kind;
            Ok(x == TokenKind::LeftCaret || x == TokenKind::RightCaret)
        })
    }
    pub fn parse_add_expression(&mut self) -> Result<Expr> {
        self.parse_binary_expression(Parser::parse_mult_expression, |this| {
            let x = this.peek()?.kind;
            Ok(x == TokenKind::Plus || x == TokenKind::Minus)
        })
    }

    pub fn parse_mult_expression(&mut self) -> Result<Expr> {
        self.parse_binary_expression(Parser::parse_call_expression, |this| {
            let x = this.peek()?.kind;
            Ok(x == TokenKind::Star || x == TokenKind::Slash)
        })
    }

    pub fn parse_call_expression(&mut self) -> Result<Expr> {
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
            return Ok(Expr::Ident(callee));
        }
        let args = self.parse_punctuated_list(TokenKind::Comma, Parser::parse_expression)?;
        let span = Span::compose(span, args.span);

        let fn_ty = None;
        Ok(Expr::Call(CallExpr {
            callee,
            args,
            fn_ty,
            span,
        }))
    }

    pub fn parse_primary_expression(&mut self) -> Result<Expr> {
        // TODO: probably refactor this to be more betterer.
        match self.peek()?.kind {
            TokenKind::BraceOpen => {
                let block = self.parse_block()?;
                return Ok(Expr::Block(block));
            }
            TokenKind::ParenOpen => return self.parse_paren_expression(),
            _ => {}
        }

        let token = self.eat()?;
        match token.kind {
            TokenKind::Identifier(_) => Ok(Expr::Ident(ident_from_token(token)?)),
            TokenKind::Integer(value) => Ok(Expr::Lit(Lit {
                value: LitVal::Int(IntLit { value }),
                ty: LitTy::Unresolved,
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

    pub fn parse_paren_expression(&mut self) -> Result<Expr> {
        verify_token!(self.eat()?, TokenKind::ParenOpen);
        let expr = self.parse_expression()?;
        verify_token!(self.eat()?, TokenKind::ParenClose);
        Ok(expr)
    }

    pub fn parse_binary_operator(&mut self) -> Result<BinOp> {
        let token = self.eat()?;

        if token.kind == TokenKind::Equal {
            let token = self.eat()?;
            return match token.kind {
                TokenKind::Equal => Ok(BinOp::Eq),
                x => Err(Error::expected_token(
                    x.to_string(),
                    vec![TokenKind::Equal.to_str().to_string()],
                    token.span,
                )),
            };
        }

        let next = self.peek()?;
        use TokenKind::*;
        match (token.kind, next.kind) {
            (Plus, _) => Ok(BinOp::Add),
            (Minus, _) => Ok(BinOp::Sub),
            (Star, _) => Ok(BinOp::Mul),
            (Slash, _) => Ok(BinOp::Div),
            (LeftCaret, Equal) => {
                self.eat()?;
                Ok(BinOp::Lte)
            }
            (RightCaret, Equal) => {
                self.eat()?;
                Ok(BinOp::Gte)
            }
            (LeftCaret, _) => Ok(BinOp::Lt),
            (RightCaret, _) => Ok(BinOp::Gt),

            (x, _) => Err(Error::expected_token(
                x.to_string(),
                vec![
                    TokenKind::Star.to_str().to_string(),
                    TokenKind::Slash.to_str().to_string(),
                    TokenKind::Plus.to_str().to_string(),
                    TokenKind::Minus.to_str().to_string(),
                    TokenKind::Equal.to_str().to_string(),
                    TokenKind::LeftCaret.to_str().to_string(),
                    TokenKind::RightCaret.to_str().to_string(),
                    TokenKind::Equal.to_str().to_string(),
                ],
                token.span,
            )),
        }
    }
}

#[cfg(test)]
mod test {
    use brig_common::sym::Symbol;

    use crate::*;

    fn test_base(input: &str, expected: Expr) {
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
            Expr::Assign(
                Box::new(Expr::Ident(Ident {
                    name: Symbol::intern("x"),
                    span: Span::new(0, 1),
                })),
                Box::new(Expr::Ident(Ident {
                    name: Symbol::intern("y"),
                    span: Span::new(4, 5),
                })),
            ),
        );
    }

    #[test]
    pub fn test_gte() {
        let input = "x >= 10";
        test_base(
            input,
            Expr::Bin(BinExpr {
                lhs: Box::new(Expr::Ident(Ident {
                    name: Symbol::intern("x"),
                    span: Span::new(0, 1),
                })),
                rhs: Box::new(Expr::Lit(Lit {
                    value: LitVal::Int(IntLit { value: 10 }),
                    ty: LitTy::Unresolved,
                    span: Span::new(5, 7),
                })),
                op: BinOp::Gte,
                span: Span::new(0, 7),
                ty_kind: None,
            }),
        );
    }

    #[test]
    pub fn parse_comparison_expr() {
        let input = "x < 10";
        test_base(
            input,
            Expr::Bin(BinExpr {
                lhs: Box::new(Expr::Ident(Ident {
                    name: Symbol::intern("x"),
                    span: Span::new(0, 1),
                })),
                rhs: Box::new(Expr::Lit(Lit {
                    value: LitVal::Int(IntLit { value: 10 }),
                    ty: LitTy::Unresolved,
                    span: Span::new(4, 6),
                })),
                op: BinOp::Lt,
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
            Expr::Bin(BinExpr {
                lhs: Box::new(Expr::Ident(Ident {
                    name: Symbol::intern("x"),
                    span: Span::new(0, 1),
                })),
                rhs: Box::new(Expr::Lit(Lit {
                    value: LitVal::Int(IntLit { value: 5 }),
                    ty: LitTy::Unresolved,
                    span: Span::new(4, 5),
                })),
                op: BinOp::Add,
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
            Expr::Bin(BinExpr {
                lhs: Box::new(Expr::Lit(Lit {
                    value: LitVal::Int(IntLit { value: 10 }),
                    ty: LitTy::Unresolved,
                    span: Span::new(0, 2),
                })),
                rhs: Box::new(Expr::Lit(Lit {
                    value: LitVal::Int(IntLit { value: 283 }),
                    ty: LitTy::Unresolved,
                    span: Span::new(5, 8),
                })),
                op: BinOp::Mul,
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
            Expr::Ident(Ident {
                name: Symbol::intern("x"),
                span: Span::new(0, 1),
            }),
        );
    }

    #[test]
    pub fn parse_paren_expr() {
        let input = "(x + 5) * 3";
        test_base(
            input,
            Expr::Bin(BinExpr {
                lhs: Box::new(Expr::Bin(BinExpr {
                    lhs: Box::new(Expr::Ident(Ident {
                        name: Symbol::intern("x"),
                        span: Span::new(1, 2),
                    })),
                    rhs: Box::new(Expr::Lit(Lit {
                        value: LitVal::Int(IntLit { value: 5 }),
                        ty: LitTy::Unresolved,
                        span: Span::new(5, 6),
                    })),
                    op: BinOp::Add,
                    span: Span::new(1, 6),
                    ty_kind: None,
                })),
                rhs: Box::new(Expr::Lit(Lit {
                    value: LitVal::Int(IntLit { value: 3 }),
                    ty: LitTy::Unresolved,
                    span: Span::new(10, 11),
                })),
                op: BinOp::Mul,
                ty_kind: None,
                span: Span::new(1, 11),
            }),
        );
    }

    #[test]
    pub fn parse_if_expr() {
        let input = "if x { 5 } else { 10 }";
        test_base(
            input,
            Expr::If(IfExpr {
                cond: Box::new(Expr::Ident(Ident {
                    name: Symbol::intern("x"),
                    span: Span::new(3, 4),
                })),
                then_block: Block {
                    stmts: vec![Stmt::Expr(Expr::Lit(Lit {
                        value: LitVal::Int(IntLit { value: 5 }),
                        ty: LitTy::Unresolved,
                        span: Span::new(7, 8),
                    }))],
                    span: Span::new(5, 8),
                },
                else_block: Some(Block {
                    stmts: vec![Stmt::Expr(Expr::Lit(Lit {
                        value: LitVal::Int(IntLit { value: 10 }),
                        ty: LitTy::Unresolved,
                        span: Span::new(18, 20),
                    }))],
                    span: Span::new(16, 20),
                }),
                span: Span::new(0, 20),
            }),
        );
    }

    #[test]
    pub fn parse_assignment_binary_expr() {
        let input = "x = 5 + 10";
        test_base(
            input,
            Expr::Assign(
                Box::new(Expr::Ident(Ident {
                    name: Symbol::intern("x"),
                    span: Span::new(0, 1),
                })),
                Box::new(Expr::Bin(BinExpr {
                    lhs: Box::new(Expr::Lit(Lit {
                        value: LitVal::Int(IntLit { value: 5 }),
                        ty: LitTy::Unresolved,
                        span: Span::new(4, 5),
                    })),
                    rhs: Box::new(Expr::Lit(Lit {
                        value: LitVal::Int(IntLit { value: 10 }),
                        ty: LitTy::Unresolved,
                        span: Span::new(8, 10),
                    })),
                    op: BinOp::Add,
                    span: Span::new(4, 10),
                    ty_kind: None,
                })),
            ),
        );
    }

    #[test]
    pub fn parse_mixed_expr() {
        let input = "x + 5 * 10";
        test_base(
            input,
            Expr::Bin(BinExpr {
                lhs: Box::new(Expr::Ident(Ident {
                    name: Symbol::intern("x"),
                    span: Span::new(0, 1),
                })),
                rhs: Box::new(Expr::Bin(BinExpr {
                    lhs: Box::new(Expr::Lit(Lit {
                        value: LitVal::Int(IntLit { value: 5 }),
                        ty: LitTy::Unresolved,
                        span: Span::new(4, 5),
                    })),
                    rhs: Box::new(Expr::Lit(Lit {
                        value: LitVal::Int(IntLit { value: 10 }),
                        ty: LitTy::Unresolved,
                        span: Span::new(8, 10),
                    })),
                    op: BinOp::Mul,
                    span: Span::new(4, 10),
                    ty_kind: None,
                })),
                op: BinOp::Add,
                span: Span::new(0, 10),
                ty_kind: None,
            }),
        );
    }
}
