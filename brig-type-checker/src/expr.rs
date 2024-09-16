use brig_ast::{AstNode, BinExpr, CallExpr, Expr, IfExpr, Lit, LitTy, LitVal, Ty, TyKind, UintTy};
use brig_common::Span;
use brig_diagnostic::{Error, Result};

use crate::TypeChecker;

impl TypeChecker {
    /// NOTE: The `ty` field is used to coerce literals (e.g. `42` to `u32`).
    /// That's not great, but I don't care right now.
    pub fn check_expression(&mut self, expr: &mut Expr, ty: Option<&Ty>) -> Result<Ty> {
        match expr {
            Expr::Bin(ref mut e) => self.check_binary_expression(e, ty),
            Expr::Assign(ref mut lhs, ref mut rhs) => {
                let lhs_ty = self.check_expression(lhs, None)?;
                let rhs_ty = self.check_expression(rhs, Some(&lhs_ty))?;
                if lhs_ty.kind != rhs_ty.kind {
                    return Err(Error::type_mismatch(
                        (lhs_ty.kind, lhs_ty.span),
                        (rhs_ty.kind, rhs_ty.span),
                    ));
                }
                Ok(Ty {
                    kind: TyKind::Lit(LitTy::Unit),
                    size: 0,
                    span: expr.span(),
                })
            }
            Expr::Lit(ref mut lit) => self.check_literal(lit, ty),
            Expr::Call(ref mut call) => self.check_call_expression(call, ty),
            Expr::If(if_expr) => self.check_if_expression(if_expr, ty),
            Expr::Block(block) => self.check_block(block, ty),
            Expr::Ident(ref ident) => {
                let ident_ty = self.get_symbol(ident.name).ok_or(Error::other(
                    format!("Could not find type for '{}'", &ident.name),
                    ident.span,
                ))?;
                if let Some(ty) = ty {
                    if ident_ty.kind != ty.kind {
                        return Err(Error::type_mismatch(
                            (ty.kind.clone(), ty.span),
                            (ident_ty.kind.clone(), ident.span),
                        ));
                    }
                }
                Ok(Ty {
                    kind: ident_ty.kind.clone(),
                    span: ident.span,
                    size: ident_ty.size,
                })
            }
        }
    }

    fn lit_is_compatible_with(&self, lit: &Lit, ty: &LitTy) -> bool {
        match &lit.value {
            LitVal::Int(int) => {
                match ty {
                    // TODO: check if int is negative, which is not allowed for uints
                    LitTy::Uint(uint) => match uint {
                        UintTy::U32 => int.value <= u32::MAX as usize,
                        UintTy::Usize => true, // any uint can be coerced to usize
                    },
                    // TODO: impl signed ints
                    _ => false,
                }
            }
            LitVal::Unit => matches!(ty, LitTy::Unit),
        }
    }

    fn lit_default(&self, lit: &mut Lit) -> Result<Ty> {
        match &lit.value {
            LitVal::Unit => {
                lit.ty = LitTy::Unit;
                Ok(Ty {
                    kind: TyKind::Lit(LitTy::Unit),
                    span: lit.span,
                    size: 0,
                })
            }
            LitVal::Int(int) => {
                // TODO: check if int is negative, which would make it a signed int

                let (mut size, mut ty) = (4, LitTy::Uint(UintTy::U32));
                if int.value > u32::MAX as usize {
                    size = size_of::<usize>();
                    ty = LitTy::Uint(UintTy::Usize);
                }
                lit.ty = ty;
                Ok(Ty {
                    kind: TyKind::Lit(ty),
                    span: lit.span,
                    size,
                })
            }
        }
    }

    pub fn check_literal(&self, lit: &mut Lit, ty: Option<&Ty>) -> Result<Ty> {
        if let Some(ty) = ty {
            match &ty.kind {
                TyKind::Lit(lit_ty) => {
                    if self.lit_is_compatible_with(lit, lit_ty) {
                        let new_ty = Ty {
                            kind: ty.kind.clone(),
                            size: ty.size,
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
                TyKind::Fn(fn_ty) => Err(Error::type_mismatch(
                    (ty.kind.to_string(), ty.span),
                    (fn_ty.name.to_string(), lit.span),
                )),
                TyKind::Unspecified => self.lit_default(lit),
            }
        } else {
            self.lit_default(lit)
        }
    }

    pub fn check_if_expression(&mut self, if_expr: &mut IfExpr, ty: Option<&Ty>) -> Result<Ty> {
        // TODO: check if the condition is a bool (we don't have that ty yet)
        let _cond_ty = self.check_expression(&mut if_expr.cond, None)?;

        let then_ty = self.check_block(&mut if_expr.then_block, ty)?;
        if let Some(ref mut else_block) = if_expr.else_block {
            let else_ty = self.check_block(else_block, ty)?;
            if then_ty.kind != else_ty.kind {
                return Err(Error::type_mismatch(
                    (then_ty.kind, then_ty.span),
                    (else_ty.kind, else_ty.span),
                ));
            }
        }

        Ok(then_ty)
    }

    pub fn check_call_expression(&mut self, call: &mut CallExpr, _ty: Option<&Ty>) -> Result<Ty> {
        let call_def = self.get_symbol(call.callee.name).ok_or(Error::other(
            format!("Could not find function '{}'", &call.callee),
            call.span,
        ))?;

        let fn_ty = match &call_def.kind {
            TyKind::Fn(fn_ty) => fn_ty.clone(),
            _ => {
                return Err(Error::other(
                    format!("'{}' is not a function", &call.callee),
                    call.span,
                ))
            }
        };

        // Check if the number of arguments match the expected number
        if call.args.len() != fn_ty.args.len() {
            return Err(Error::other(
                format!(
                    "Expected {} arguments, got {}",
                    fn_ty.args.len(),
                    call.args.len()
                ),
                call.span,
            ));
        }
        // Check if the arguments match the expected types
        for (arg, ty) in call.args.iter_mut().zip(fn_ty.args.iter()) {
            self.check_expression(arg, Some(ty))?;
        }

        call.fn_ty = Some(fn_ty.clone());

        // Call expressions have the type of the return type of the function
        Ok(*fn_ty.ret)
    }

    pub fn check_binary_expression(&mut self, expr: &mut BinExpr, ty: Option<&Ty>) -> Result<Ty> {
        let lhs = self.check_expression(&mut expr.lhs, ty)?;
        let rhs = self.check_expression(&mut expr.rhs, Some(&lhs))?;
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
            size: lhs.size,
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
        let mut decl = LetDecl {
            name: Ident {
                name: Symbol::intern("x"),
                span: Span::new(4, 5),
            },
            ty: Ty {
                kind: TyKind::Lit(LitTy::Uint(UintTy::Usize)),
                size: 8,
                span: Span::new(7, 12),
            },
            expr: Some(Expr::Lit(Lit {
                value: LitVal::Int(IntLit { value: 42 }),
                ty: LitTy::Unresolved,
                span: Span::new(15, 17),
            })),
            span: Span::new(0, 17),
        };
        let mut tc = TypeChecker::default();
        tc.check_variable_declaration(&mut decl)
            .expect("type check failed");

        assert_eq!(decl.ty.kind, TyKind::Lit(LitTy::Uint(UintTy::Usize)));
        assert_eq!(
            decl.expr,
            Some(Expr::Lit(Lit {
                value: LitVal::Int(IntLit { value: 42 }),
                ty: LitTy::Uint(UintTy::Usize),
                span: Span::new(15, 17),
            }))
        );
    }

    #[test]
    fn check_u32_declaration() {
        // let x: u32 = 42;
        let mut decl = LetDecl {
            name: Ident {
                name: Symbol::intern("x"),
                span: Span::new(4, 5),
            },
            ty: Ty {
                kind: TyKind::Lit(LitTy::Uint(UintTy::U32)),
                size: 4,
                span: Span::new(7, 9),
            },
            expr: Some(Expr::Lit(Lit {
                value: LitVal::Int(IntLit { value: 42 }),
                ty: LitTy::Unresolved,
                span: Span::new(13, 15),
            })),
            span: Span::new(0, 15),
        };
        let mut tc = TypeChecker::default();
        tc.check_variable_declaration(&mut decl)
            .expect("type check failed");

        assert_eq!(decl.ty.kind, TyKind::Lit(LitTy::Uint(UintTy::U32)));
        assert_eq!(
            decl.expr,
            Some(Expr::Lit(Lit {
                value: LitVal::Int(IntLit { value: 42 }),
                ty: LitTy::Uint(UintTy::U32),
                span: Span::new(13, 15),
            }))
        );
    }

    #[test]
    fn check_simple_binary_expression_types() {
        // 1 + 2
        let mut expr = BinExpr {
            lhs: Box::new(Expr::Lit(Lit {
                value: LitVal::Int(IntLit { value: 1 }),
                ty: LitTy::Unresolved,
                span: Span::new(0, 1),
            })),
            rhs: Box::new(Expr::Lit(Lit {
                value: LitVal::Int(IntLit { value: 2 }),
                ty: LitTy::Unresolved,
                span: Span::new(4, 5),
            })),
            ty_kind: None,
            op: BinOp::Add,
            span: Span::new(0, 5),
        };

        let ty = TypeChecker::default()
            .check_binary_expression(&mut expr, None)
            .unwrap();
        assert_eq!(ty.kind, TyKind::Lit(LitTy::Uint(UintTy::U32)));
        assert_eq!(expr.ty_kind, Some(TyKind::Lit(LitTy::Uint(UintTy::U32))));
    }
    #[test]
    fn check_binary_expression_types() {
        // 1 + 2 * 3
        let mut expr = BinExpr {
            lhs: Box::new(Expr::Lit(Lit {
                value: LitVal::Int(IntLit { value: 1 }),
                ty: LitTy::Unresolved,
                span: Span::new(0, 1),
            })),
            rhs: Box::new(Expr::Bin(BinExpr {
                lhs: Box::new(Expr::Lit(Lit {
                    value: LitVal::Int(IntLit { value: 2 }),
                    ty: LitTy::Unresolved,
                    span: Span::new(4, 5),
                })),
                rhs: Box::new(Expr::Lit(Lit {
                    value: LitVal::Int(IntLit { value: 3 }),
                    ty: LitTy::Unresolved,
                    span: Span::new(8, 9),
                })),
                ty_kind: None,
                op: BinOp::Mul,
                span: Span::new(4, 9),
            })),
            ty_kind: None,
            op: BinOp::Add,
            span: Span::new(0, 9),
        };

        let ty = TypeChecker::default()
            .check_binary_expression(&mut expr, None)
            .unwrap();
        assert_eq!(ty.kind, TyKind::Lit(LitTy::Uint(UintTy::U32)));
        assert_eq!(expr.ty_kind, Some(TyKind::Lit(LitTy::Uint(UintTy::U32))));
    }
}
