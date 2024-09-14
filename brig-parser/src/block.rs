use crate::*;

impl Parser {
    /// Parse a single block of code, containing a series of statements.
    pub fn parse_block(&mut self) -> Result<Block> {
        let span = self.peek()?.span;
        verify_token!(self.eat()?, TokenKind::BraceOpen);

        let mut statements = Vec::new();
        while self.peek()?.kind != TokenKind::BraceClose {
            statements.push(self.parse_statement()?);
        }

        verify_token!(self.eat()?, TokenKind::BraceClose);

        let span = Span::compose(
            span,
            statements.last().map(|s| s.span()).unwrap_or_default(),
        );

        Ok(Block { span, statements })
    }
}

#[cfg(test)]
mod test {
    use brig_ast::{
        AstNode, BinaryExpression, BinaryOperator, Block, Expression, Identifier, IntLit, Literal,
        LiteralType, LiteralValue, Statement, Ty, TyKind, UintType, VariableDeclaration,
    };
    use brig_common::{sym::Symbol, Span};
    use brig_lexer::Lexer;

    use crate::Parser;

    #[test]
    fn parse_let_block() {
        let input = "let x:usize={42};";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let stmt = parser.parse_statement().expect("failed to parse statement");

        assert_eq!(stmt.span(), Span::with_len(0, 15));
        assert_eq!(
            stmt,
            Statement::VariableDeclaration(VariableDeclaration {
                name: Identifier {
                    name: Symbol::intern("x"),
                    span: Span::with_len(4, 1),
                },
                ty: Ty {
                    kind: TyKind::Literal(LiteralType::Uint(UintType::Usize)),
                    size: 8,
                    span: Span::with_len(6, 5),
                },
                expr: Some(Expression::Block(Block {
                    statements: vec![Statement::Expr(Expression::Literal(Literal {
                        value: LiteralValue::Int(IntLit { value: 42 }),
                        ty: LiteralType::Unresolved,
                        span: Span::with_len(13, 2),
                    }))],
                    span: Span::with_len(12, 3),
                })),
                span: Span::with_len(0, 15),
            })
        );
    }

    #[test]
    fn parse_block_leading_expr() {
        let input = "{let x:u32=42;x+1}";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let block = parser.parse_block().expect("failed to parse block");

        assert_eq!(block.statements.len(), 2);
        assert_eq!(block.span, Span::with_len(0, 17));
        assert_eq!(
            block,
            Block {
                statements: vec![
                    Statement::VariableDeclaration(VariableDeclaration {
                        name: Identifier {
                            name: Symbol::intern("x"),
                            span: Span::with_len(5, 1),
                        },
                        ty: Ty {
                            kind: TyKind::Literal(LiteralType::Uint(UintType::U32)),
                            size: 4,
                            span: Span::with_len(7, 3),
                        },
                        expr: Some(Expression::Literal(Literal {
                            value: LiteralValue::Int(IntLit { value: 42 }),
                            ty: LiteralType::Unresolved,
                            span: Span::with_len(11, 2),
                        })),
                        span: Span::with_len(1, 12),
                    }),
                    Statement::Expr(Expression::Binary(BinaryExpression {
                        lhs: Box::new(Expression::Identifier(Identifier {
                            name: Symbol::intern("x"),
                            span: Span::with_len(14, 1),
                        })),
                        rhs: Box::new(Expression::Literal(Literal {
                            value: LiteralValue::Int(IntLit { value: 1 }),
                            ty: LiteralType::Unresolved,
                            span: Span::with_len(16, 1),
                        })),
                        op: BinaryOperator::Add,
                        span: Span::with_len(14, 3),
                        ty_kind: None,
                    })),
                ],
                span: Span::with_len(0, 17),
            }
        );
    }

    #[test]
    fn parse_block() {
        let input = "{let x:u32=42;x+1;}";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let block = parser.parse_block().expect("failed to parse block");

        assert_eq!(block.statements.len(), 2);
        assert_eq!(block.span, Span::with_len(0, 17));
        assert_eq!(
            block,
            Block {
                statements: vec![
                    Statement::VariableDeclaration(VariableDeclaration {
                        name: Identifier {
                            name: Symbol::intern("x"),
                            span: Span::with_len(5, 1),
                        },
                        ty: Ty {
                            kind: TyKind::Literal(LiteralType::Uint(UintType::U32)),
                            size: 4,
                            span: Span::with_len(7, 3),
                        },
                        expr: Some(Expression::Literal(Literal {
                            value: LiteralValue::Int(IntLit { value: 42 }),
                            ty: LiteralType::Unresolved,
                            span: Span::with_len(11, 2),
                        })),
                        span: Span::with_len(1, 12),
                    }),
                    Statement::Semi(Expression::Binary(BinaryExpression {
                        lhs: Box::new(Expression::Identifier(Identifier {
                            name: Symbol::intern("x"),
                            span: Span::with_len(14, 1),
                        })),
                        rhs: Box::new(Expression::Literal(Literal {
                            value: LiteralValue::Int(IntLit { value: 1 }),
                            ty: LiteralType::Unresolved,
                            span: Span::with_len(16, 1),
                        })),
                        op: BinaryOperator::Add,
                        span: Span::with_len(14, 3),
                        ty_kind: None,
                    })),
                ],
                span: Span::with_len(0, 17),
            }
        );
    }
}
