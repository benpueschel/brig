use brig_ast::{Statement, VariableDeclaration};
use brig_diagnostic::{Error, Result};

use crate::TypeChecker;

impl TypeChecker {
    pub fn check_statement(&self, stmt: &mut Statement) -> Result<()> {
        match stmt {
            // Statement::Expression => map the returned Ty to ()
            Statement::Expression(ref mut e) => self.check_expression(e).map(|_| ()),
            Statement::VariableDeclaration(ref mut decl) => self.check_variable_declaration(decl),
        }
    }
    pub fn check_variable_declaration(&self, decl: &mut VariableDeclaration) -> Result<()> {
        if let Some(ref mut expr) = decl.expr {
            let ty = self.check_expression(expr)?;
            if ty.kind != decl.ty.kind {
                return Err(Error::type_mismatch(
                    (ty.kind, ty.span),
                    (decl.ty.kind.clone(), decl.span),
                ));
            }
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
        // let x: i32 = 42;
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
                span: Span::new(13, 14),
            })),
            span: Span::new(0, 15),
        };
        let tc = TypeChecker;
        tc.check_variable_declaration(&mut decl)
            .expect("type check failed");

        assert_eq!(decl.ty.kind, TyKind::Literal(LiteralType::U32));
    }
}
