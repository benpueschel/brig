use brig_ast::{AstNode, Block, LitTy, Stmt, Ty, TyKind};
use brig_diagnostic::Result;

use crate::TypeChecker;

impl TypeChecker {
    pub fn check_block(&mut self, block: &mut Block, ty: Option<&Ty>) -> Result<Ty> {
        self.push_scope();
        for stmt in &mut block.stmts {
            self.check_statement(stmt, None)?;
        }
        let ty = if let Some(stmt) = block.stmts.last_mut() {
            match stmt {
                // FIXME: This is wrong. The return statement should be checked in a separate
                // function walking an entire function body (or - better yet - in the IR?)
                Stmt::Return(r) => self.check_expression(&mut r.expr, ty)?,
                Stmt::Expr(e) => self.check_expression(e, ty)?,
                _ => Ty {
                    kind: TyKind::Lit(LitTy::Unit),
                    span: stmt.span(),
                },
            }
        } else {
            Ty {
                kind: TyKind::Lit(LitTy::Unit),
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
        let mut stmt = Stmt::LetDecl(LetDecl {
            name: Ident {
                name: Symbol::intern("a"),
                span: Span::with_len(4, 1),
            },
            ty: Ty {
                kind: TyKind::Lit(LitTy::Uint(UintTy::Usize)),
                span: Span::with_len(7, 5),
            },
            expr: Some(Expr::Block(Block {
                stmts: vec![
                    Stmt::LetDecl(LetDecl {
                        name: Ident {
                            name: Symbol::intern("b"),
                            span: Span::with_len(14, 1),
                        },
                        ty: Ty {
                            kind: TyKind::Lit(LitTy::Uint(UintTy::Usize)),
                            span: Span::with_len(17, 5),
                        },
                        expr: Some(Expr::Lit(Lit {
                            value: LitVal::Int(IntLit { value: 42 }),
                            ty: LitTy::Unresolved,
                            span: Span::with_len(25, 2),
                        })),
                        span: Span::with_len(10, 17),
                    }),
                    Stmt::Expr(Expr::Bin(BinExpr {
                        lhs: Box::new(Expr::Lit(Lit {
                            value: LitVal::Int(IntLit { value: 12 }),
                            ty: LitTy::Unresolved,
                            span: Span::with_len(30, 2),
                        })),
                        rhs: Box::new(Expr::Lit(Lit {
                            value: LitVal::Int(IntLit { value: 24 }),
                            ty: LitTy::Unresolved,
                            span: Span::with_len(35, 2),
                        })),
                        op: BinOp::Add,
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
        let mut stmt = Stmt::LetDecl(LetDecl {
            name: Ident {
                name: Symbol::intern("a"),
                span: Span::with_len(4, 1),
            },
            ty: Ty {
                kind: TyKind::Lit(LitTy::Uint(UintTy::Usize)),
                span: Span::with_len(7, 5),
            },
            expr: Some(Expr::Block(Block {
                stmts: vec![Stmt::Expr(Expr::Lit(Lit {
                    value: LitVal::Int(IntLit { value: 42 }),
                    ty: LitTy::Unresolved,
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
            stmts: vec![Stmt::Return(ReturnStmt {
                expr: Expr::Lit(Lit {
                    value: LitVal::Int(IntLit { value: 42 }),
                    ty: LitTy::Unresolved,
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
                stmts: vec![Stmt::Return(ReturnStmt {
                    expr: Expr::Lit(Lit {
                        value: LitVal::Int(IntLit { value: 42 }),
                        ty: LitTy::Uint(UintTy::U32),
                        span: Span::with_len(9, 2),
                    }),
                    span: Span::with_len(2, 10),
                })],
                span: Span::with_len(0, 13),
            }
        );
    }
}
