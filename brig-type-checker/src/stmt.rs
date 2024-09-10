use brig_ast::{Statement, VariableDeclaration};
use brig_diagnostic::{Error, Result};

use crate::TypeChecker;

impl TypeChecker {
    pub fn check_statement(&mut self, stmt: &mut Statement) -> Result<()> {
        match stmt {
            // Statement::Expression => map the returned Ty to ()
            Statement::Expression(ref mut e) => self.check_expression(e, None).map(|_| ()),
            Statement::VariableDeclaration(ref mut decl) => self.check_variable_declaration(decl),
            Statement::Return(ref mut ret) => {
                self.check_expression(&mut ret.expr, None).map(|_| ())
            }
        }
    }
    pub fn check_variable_declaration(&mut self, decl: &mut VariableDeclaration) -> Result<()> {
        if let Some(ref mut expr) = decl.expr {
            let ty = self.check_expression(expr, Some(&decl.ty))?;
            if ty.kind != decl.ty.kind {
                return Err(Error::type_mismatch(
                    (ty.kind, ty.span),
                    (decl.ty.kind.clone(), decl.span),
                ));
            }
            self.add_symbol(decl.name.name.clone(), ty);
        }
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::*;
    use brig_ast::*;
    use brig_common::Span;

    #[test]
    fn check_variable_declaration() {
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
                span: Span::new(13, 14),
            })),
            span: Span::new(0, 15),
        };
        let mut tc = TypeChecker::default();
        tc.check_variable_declaration(&mut decl)
            .expect("type check failed");

        assert_eq!(
            decl.ty.kind,
            TyKind::Literal(LiteralType::Uint(UintType::U32))
        );
    }
}
