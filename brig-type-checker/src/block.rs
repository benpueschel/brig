use brig_ast::{AstNode, Block, LiteralType, Statement, Ty, TyKind};
use brig_diagnostic::Result;

use crate::TypeChecker;

impl TypeChecker {
    pub fn check_block(&mut self, block: &mut Block, ty: Option<&Ty>) -> Result<Ty> {
        self.push_scope();
        for stmt in &mut block.statements {
            self.check_statement(stmt, None)?;
        }
        let ty = if let Some(stmt) = block.statements.last_mut() {
            match stmt {
                // FIXME: This is wrong. The return statement should be checked in a separate
                // function walking an entire function body (or - better yet - in the IR?)
                Statement::Return(r) => self.check_expression(&mut r.expr, ty)?,
                Statement::Expr(e) => self.check_expression(e, ty)?,
                _ => Ty {
                    kind: TyKind::Literal(LiteralType::Unit),
                    size: 0,
                    span: stmt.span(),
                },
            }
        } else {
            Ty {
                kind: TyKind::Literal(LiteralType::Unit),
                size: 0,
                span: block.span(),
            }
        };
        self.pop_scope();
        Ok(ty)
    }
}

#[cfg(test)]
mod test {
    use crate::*;
    use brig_ast::*;
    use brig_common::Span;

    #[test]
    fn check_multiple_statement_block_return() {
        // let a: usize = { let b: usize = 42; 12 + 24 };
        let mut stmt = Statement::VariableDeclaration(VariableDeclaration {
            name: Identifier {
                name: "a".to_string(),
                span: Span::with_len(4, 1),
            },
            ty: Ty {
                kind: TyKind::Literal(LiteralType::Uint(UintType::Usize)),
                size: 8,
                span: Span::with_len(7, 5),
            },
            expr: Some(Expression::Block(Block {
                statements: vec![
                    Statement::VariableDeclaration(VariableDeclaration {
                        name: Identifier {
                            name: "b".to_string(),
                            span: Span::with_len(14, 1),
                        },
                        ty: Ty {
                            kind: TyKind::Literal(LiteralType::Uint(UintType::Usize)),
                            size: 8,
                            span: Span::with_len(17, 5),
                        },
                        expr: Some(Expression::Literal(Literal {
                            value: LiteralValue::Int(IntLit { value: 42 }),
                            ty: LiteralType::Unresolved,
                            span: Span::with_len(25, 2),
                        })),
                        span: Span::with_len(10, 17),
                    }),
                    Statement::Expr(Expression::Binary(BinaryExpression {
                        lhs: Box::new(Expression::Literal(Literal {
                            value: LiteralValue::Int(IntLit { value: 12 }),
                            ty: LiteralType::Unresolved,
                            span: Span::with_len(30, 2),
                        })),
                        rhs: Box::new(Expression::Literal(Literal {
                            value: LiteralValue::Int(IntLit { value: 24 }),
                            ty: LiteralType::Unresolved,
                            span: Span::with_len(35, 2),
                        })),
                        op: BinaryOperator::Add,
                        span: Span::with_len(32, 3),
                        ty_kind: None,
                    })),
                ],
                span: Span::with_len(12, 27),
            })),
            span: Span::with_len(0, 39),
        });
        let mut tc = TypeChecker::default();
        tc.check_statement(&mut stmt, None)
            .expect("type check failed");
    }

    #[test]
    fn check_block_return() {
        // let a: usize = { 42 };
        let mut stmt = Statement::VariableDeclaration(VariableDeclaration {
            name: Identifier {
                name: "a".to_string(),
                span: Span::with_len(4, 1),
            },
            ty: Ty {
                kind: TyKind::Literal(LiteralType::Uint(UintType::Usize)),
                size: 8,
                span: Span::with_len(7, 5),
            },
            expr: Some(Expression::Block(Block {
                statements: vec![Statement::Expr(Expression::Literal(Literal {
                    value: LiteralValue::Int(IntLit { value: 42 }),
                    ty: LiteralType::Unresolved,
                    span: Span::with_len(17, 2),
                }))],
                span: Span::with_len(14, 5),
            })),
            span: Span::with_len(0, 19),
        });
        let mut tc = TypeChecker::default();
        tc.check_statement(&mut stmt, None)
            .expect("type check failed");
    }

    #[test]
    fn test_check_block() {
        // { return 42; } -> the block should return a u32
        let mut block = Block {
            statements: vec![Statement::Return(ReturnStatement {
                expr: Expression::Literal(Literal {
                    value: LiteralValue::Int(IntLit { value: 42 }),
                    ty: LiteralType::Unresolved,
                    span: Span::with_len(9, 2),
                }),
                span: Span::with_len(2, 10),
            })],
            span: Span::with_len(0, 13),
        };
        let mut tc = TypeChecker::default();
        tc.check_block(&mut block, None).expect("type check failed");

        assert_eq!(
            block,
            Block {
                statements: vec![Statement::Return(ReturnStatement {
                    expr: Expression::Literal(Literal {
                        value: LiteralValue::Int(IntLit { value: 42 }),
                        ty: LiteralType::Uint(UintType::U32),
                        span: Span::with_len(9, 2),
                    }),
                    span: Span::with_len(2, 10),
                })],
                span: Span::with_len(0, 13),
            }
        );
    }
}
