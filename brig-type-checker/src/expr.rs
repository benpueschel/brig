use brig_ast::{BinaryExpression, Expression, Ty, TyKind};
use brig_common::Span;
use brig_diagnostic::{Error, Result};

use crate::TypeChecker;

impl TypeChecker {
    pub fn check_expression(&self, expr: &mut Expression) -> Result<Ty> {
        match expr {
            Expression::Binary(ref mut e) => self.check_binary_expression(e),
            Expression::Literal(ref lit) => Ok(Ty {
                kind: TyKind::Literal(lit.ty),
                span: lit.span,
            }),
            Expression::Identifier(ref ident) => Ok(Ty {
                kind: TyKind::UserDefined(ident.clone()),
                span: ident.span,
            }),
        }
    }

    pub fn check_binary_expression(&self, expr: &mut BinaryExpression) -> Result<Ty> {
        let lhs = self.check_expression(&mut expr.lhs)?;
        let rhs = self.check_expression(&mut expr.rhs)?;
        let span = Span::compose(lhs.span, rhs.span);
        if lhs.kind != rhs.kind {
            return Err(Error::type_mismatch(
                (lhs.kind, lhs.span),
                (rhs.kind, rhs.span),
            ));
        }
        expr.ty_kind = Some(lhs.kind.clone());
        Ok(Ty {
            kind: lhs.kind,
            span,
        })
    }
}

#[cfg(test)]
mod test {
    use crate::*;
    use brig_ast::*;
    use brig_common::Span;

    #[test]
    fn check_usize_declaration() {
        // let x: usize = 42;
        let mut decl = VariableDeclaration {
            name: Identifier {
                name: "x".to_string(),
                span: Span::new(4, 5),
            },
            ty: Ty {
                kind: TyKind::Literal(LiteralType::Usize),
                span: Span::new(7, 12),
            },
            expr: Some(Expression::Literal(Literal {
                value: LiteralValue::Usize(42),
                ty: LiteralType::Usize,
                span: Span::new(15, 17),
            })),
            span: Span::new(0, 17),
        };
        let tc = TypeChecker;
        tc.check_variable_declaration(&mut decl)
            .expect("type check failed");

        assert_eq!(decl.ty.kind, TyKind::Literal(LiteralType::Usize));
        assert_eq!(
            decl.expr,
            Some(Expression::Literal(Literal {
                value: LiteralValue::Usize(42),
                ty: LiteralType::Usize,
                span: Span::new(15, 17),
            }))
        );
    }

    #[test]
    fn check_u32_declaration() {
        // let x: u32 = 42;
        let mut decl = VariableDeclaration {
            name: Identifier {
                name: "x".to_string(),
                span: Span::new(4, 5),
            },
            ty: Ty {
                kind: TyKind::Literal(LiteralType::U32),
                span: Span::new(7, 9),
            },
            expr: Some(Expression::Literal(Literal {
                value: LiteralValue::U32(42),
                ty: LiteralType::U32,
                span: Span::new(13, 15),
            })),
            span: Span::new(0, 15),
        };
        let tc = TypeChecker;
        tc.check_variable_declaration(&mut decl)
            .expect("type check failed");

        assert_eq!(decl.ty.kind, TyKind::Literal(LiteralType::U32));
        assert_eq!(
            decl.expr,
            Some(Expression::Literal(Literal {
                value: LiteralValue::U32(42),
                ty: LiteralType::U32,
                span: Span::new(13, 15),
            }))
        );
    }

    #[test]
    fn check_simple_binary_expression_types() {
        // 1 + 2
        let mut expr = BinaryExpression {
            lhs: Box::new(Expression::Literal(Literal {
                value: LiteralValue::U32(1),
                ty: LiteralType::U32,
                span: Span::new(0, 1),
            })),
            rhs: Box::new(Expression::Literal(Literal {
                value: LiteralValue::U32(2),
                ty: LiteralType::U32,
                span: Span::new(4, 5),
            })),
            ty_kind: None,
            op: BinaryOperator::Add,
            span: Span::new(0, 5),
        };

        let ty = TypeChecker.check_binary_expression(&mut expr).unwrap();
        assert_eq!(ty.kind, TyKind::Literal(LiteralType::U32));
        assert_eq!(expr.ty_kind, Some(TyKind::Literal(LiteralType::U32)));
    }
    #[test]
    fn check_binary_expression_types() {
        // 1 + 2 * 3
        let mut expr = BinaryExpression {
            lhs: Box::new(Expression::Literal(Literal {
                value: LiteralValue::U32(1),
                ty: LiteralType::U32,
                span: Span::new(0, 1),
            })),
            rhs: Box::new(Expression::Binary(BinaryExpression {
                lhs: Box::new(Expression::Literal(Literal {
                    value: LiteralValue::U32(2),
                    ty: LiteralType::U32,
                    span: Span::new(4, 5),
                })),
                rhs: Box::new(Expression::Literal(Literal {
                    value: LiteralValue::U32(3),
                    ty: LiteralType::U32,
                    span: Span::new(8, 9),
                })),
                ty_kind: None,
                op: BinaryOperator::Multiply,
                span: Span::new(4, 9),
            })),
            ty_kind: None,
            op: BinaryOperator::Add,
            span: Span::new(0, 9),
        };

        let ty = TypeChecker.check_binary_expression(&mut expr).unwrap();
        assert_eq!(ty.kind, TyKind::Literal(LiteralType::U32));
        assert_eq!(expr.ty_kind, Some(TyKind::Literal(LiteralType::U32)));
    }
}
