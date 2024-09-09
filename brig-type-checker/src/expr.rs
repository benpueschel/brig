use brig_ast::{
    BinaryExpression, Expression, Literal, LiteralType, LiteralValue, Ty, TyKind, UintType,
};
use brig_common::Span;
use brig_diagnostic::{Error, Result};

use crate::TypeChecker;

impl TypeChecker {
    /// NOTE: The `ty` field is used to coerce literals (e.g. `42` to `u32`).
    /// That's not great, but I don't care right now.
    pub fn check_expression(&self, expr: &mut Expression, ty: Option<&Ty>) -> Result<Ty> {
        match expr {
            Expression::Binary(ref mut e) => self.check_binary_expression(e, ty),
            Expression::Literal(ref mut lit) => self.check_literal(lit, ty),
            Expression::Identifier(ref ident) => {
                let kind = self.get_symbol(&ident.name).ok_or(Error::other(
                    format!("Could not find type for '{}'", &ident.name),
                    ident.span,
                ))?;
                if let Some(ty) = ty {
                    if kind != ty.kind {
                        return Err(Error::type_mismatch(
                            (ty.kind.clone(), ty.span),
                            (kind.clone(), ident.span),
                        ));
                    }
                }
                Ok(Ty {
                    kind,
                    span: ident.span,
                })
            }
        }
    }

    fn lit_is_compatible_with(&self, lit: &Literal, ty: &LiteralType) -> bool {
        match &lit.value {
            LiteralValue::Int(int) => {
                match ty {
                    // TODO: check if int is negative, which is not allowed for uints
                    LiteralType::Uint(uint) => match uint {
                        UintType::U32 => int.value <= u32::MAX as usize,
                        UintType::Usize => true, // any uint can be coerced to usize
                    },
                    // TODO: impl signed ints
                    _ => false,
                }
            }
        }
    }

    fn lit_default(&self, lit: &mut Literal) -> Result<Ty> {
        match &lit.value {
            LiteralValue::Int(int) => {
                // TODO: check if int is negative, which would make it a signed int

                let mut ty = LiteralType::Uint(UintType::U32);
                if int.value > u32::MAX as usize {
                    ty = LiteralType::Uint(UintType::Usize);
                }
                lit.ty = ty;
                Ok(Ty {
                    kind: TyKind::Literal(ty),
                    span: lit.span,
                })
            }
        }
    }

    pub fn check_literal(&self, lit: &mut Literal, ty: Option<&Ty>) -> Result<Ty> {
        if let Some(ty) = ty {
            match &ty.kind {
                TyKind::Literal(lit_ty) => {
                    if self.lit_is_compatible_with(lit, lit_ty) {
                        let new_ty = Ty {
                            kind: ty.kind.clone(),
                            span: lit.span,
                        };
                        lit.ty = *lit_ty;
                        Ok(new_ty)
                    } else {
                        Err(Error::type_mismatch(
                            (ty.kind.to_string(), ty.span),
                            (lit_ty.to_string(), lit.span),
                        ))
                    }
                }
                TyKind::UserDefined(u_ty) => {
                    // TODO: check some stuff about u_ty
                    Err(Error::type_mismatch(
                        (ty.kind.to_string(), ty.span),
                        (u_ty.name.to_string(), u_ty.span),
                    ))
                }
                TyKind::Unspecified => self.lit_default(lit),
            }
        } else {
            self.lit_default(lit)
        }
    }

    pub fn check_binary_expression(
        &self,
        expr: &mut BinaryExpression,
        ty: Option<&Ty>,
    ) -> Result<Ty> {
        let lhs = self.check_expression(&mut expr.lhs, ty)?;
        let rhs = self.check_expression(&mut expr.rhs, ty)?;
        let span = Span::compose(lhs.span, rhs.span);
        if lhs.kind != rhs.kind {
            eprintln!("lhs: {:#?}, rhs: {:#?}", lhs, rhs);
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
                kind: TyKind::Literal(LiteralType::Uint(UintType::Usize)),
                span: Span::new(7, 12),
            },
            expr: Some(Expression::Literal(Literal {
                value: LiteralValue::Int(IntLit { value: 42 }),
                ty: LiteralType::Unresolved,
                span: Span::new(15, 17),
            })),
            span: Span::new(0, 17),
        };
        let mut tc = TypeChecker::default();
        // TODO: refactor this. I don't want to manually push a scope here, but because we're only
        // parsing a single statement, that's kinda the only way to do it right now.
        tc.push_scope();
        tc.check_variable_declaration(&mut decl)
            .expect("type check failed");

        assert_eq!(
            decl.ty.kind,
            TyKind::Literal(LiteralType::Uint(UintType::Usize))
        );
        assert_eq!(
            decl.expr,
            Some(Expression::Literal(Literal {
                value: LiteralValue::Int(IntLit { value: 42 }),
                ty: LiteralType::Uint(UintType::Usize),
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
                kind: TyKind::Literal(LiteralType::Uint(UintType::U32)),
                span: Span::new(7, 9),
            },
            expr: Some(Expression::Literal(Literal {
                value: LiteralValue::Int(IntLit { value: 42 }),
                ty: LiteralType::Unresolved,
                span: Span::new(13, 15),
            })),
            span: Span::new(0, 15),
        };
        let mut tc = TypeChecker::default();
        // TODO: refactor this. I don't want to manually push a scope here, but because we're only
        // parsing a single statement, that's kinda the only way to do it right now.
        tc.push_scope();
        tc.check_variable_declaration(&mut decl)
            .expect("type check failed");

        assert_eq!(
            decl.ty.kind,
            TyKind::Literal(LiteralType::Uint(UintType::U32))
        );
        assert_eq!(
            decl.expr,
            Some(Expression::Literal(Literal {
                value: LiteralValue::Int(IntLit { value: 42 }),
                ty: LiteralType::Uint(UintType::U32),
                span: Span::new(13, 15),
            }))
        );
    }

    #[test]
    fn check_simple_binary_expression_types() {
        // 1 + 2
        let mut expr = BinaryExpression {
            lhs: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Int(IntLit { value: 1 }),
                ty: LiteralType::Unresolved,
                span: Span::new(0, 1),
            })),
            rhs: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Int(IntLit { value: 2 }),
                ty: LiteralType::Unresolved,
                span: Span::new(4, 5),
            })),
            ty_kind: None,
            op: BinaryOperator::Add,
            span: Span::new(0, 5),
        };

        let ty = TypeChecker::default()
            .check_binary_expression(&mut expr, None)
            .unwrap();
        assert_eq!(ty.kind, TyKind::Literal(LiteralType::Uint(UintType::U32)));
        assert_eq!(
            expr.ty_kind,
            Some(TyKind::Literal(LiteralType::Uint(UintType::U32)))
        );
    }
    #[test]
    fn check_binary_expression_types() {
        // 1 + 2 * 3
        let mut expr = BinaryExpression {
            lhs: Box::new(Expression::Literal(Literal {
                value: LiteralValue::Int(IntLit { value: 1 }),
                ty: LiteralType::Unresolved,
                span: Span::new(0, 1),
            })),
            rhs: Box::new(Expression::Binary(BinaryExpression {
                lhs: Box::new(Expression::Literal(Literal {
                    value: LiteralValue::Int(IntLit { value: 2 }),
                    ty: LiteralType::Unresolved,
                    span: Span::new(4, 5),
                })),
                rhs: Box::new(Expression::Literal(Literal {
                    value: LiteralValue::Int(IntLit { value: 3 }),
                    ty: LiteralType::Unresolved,
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

        let ty = TypeChecker::default()
            .check_binary_expression(&mut expr, None)
            .unwrap();
        assert_eq!(ty.kind, TyKind::Literal(LiteralType::Uint(UintType::U32)));
        assert_eq!(
            expr.ty_kind,
            Some(TyKind::Literal(LiteralType::Uint(UintType::U32)))
        );
    }
}
