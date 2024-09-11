use brig_ast::VariableDeclaration;
use brig_diagnostic::{Error, Result};

use crate::{Lvalue, Scope, Statement, StatementKind, Var, VAR_UNINITIALIZED};

impl crate::Ir {
    pub(crate) fn traverse_statement(
        &mut self,
        statement: brig_ast::Statement,
        scope: Scope,
    ) -> Result<Vec<Statement>> {
        match statement {
            brig_ast::Statement::VariableDeclaration(decl) => self.traverse_let_decl(decl, scope),
            brig_ast::Statement::Expression(expr) => self.traverse_expr_stmt(expr, scope),
            brig_ast::Statement::Return(ret) => {
                Err(Error::other("return statement is a branch", ret.span))
            }
        }
    }

    pub(crate) fn traverse_let_decl(
        &mut self,
        data: VariableDeclaration,
        scope: Scope,
    ) -> Result<Vec<Statement>> {
        let scope_data = self.scope_data_mut(scope);
        let var = Var {
            ident: data.name.clone(),
            id: VAR_UNINITIALIZED,
            size: data.ty.size,
        };
        scope_data.var_decls.push(crate::VarDecl {
            var: var.clone(),
            scope,
        });
        if let Some(expr) = data.expr {
            let (right, mut stmts) = self.traverse_rvalue(expr, scope)?;
            stmts.push(Statement {
                kind: StatementKind::Assign(Lvalue::Variable(var), right),
                span: data.span,
            });
            return Ok(stmts);
        }

        Ok(Vec::new())
    }
}
