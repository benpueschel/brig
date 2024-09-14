use brig_ast::{AstNode, BinaryOperator, Expression, ReturnStatement, VariableDeclaration};
use brig_diagnostic::{Error, Result};

use crate::{resolve, Lvalue, Operand, Scope, StatementKind, Terminator, Var, VarDecl};

type Res = Result<Option<Operand>>;
impl crate::Ir {
    pub fn traverse_expr_stmt(&mut self, expr: Expression, scope: Scope) -> Res {
        match expr {
            brig_ast::Expression::Call(call) => self.traverse_call_expression(call, scope),
            brig_ast::Expression::Block(block) => Ok(self.traverse_block(block, scope)?.1),
            brig_ast::Expression::Binary(expr) => {
                // TODO: move into a separate assignment statement instead of an expression
                if let BinaryOperator::Assign = expr.op {
                    let left = self.traverse_lvalue(*expr.lhs, scope)?;
                    let right = self.traverse_rvalue(*expr.rhs, scope)?.ok_or_else(|| {
                        Error::other("right-hand side of assignment is not an rvalue", expr.span)
                    })?;
                    self.current_block_mut().statements.push(crate::Statement {
                        kind: StatementKind::Assign(left, right),
                        span: expr.span,
                    });
                }
                Ok(None)
            }
            brig_ast::Expression::Literal(lit) => self.traverse_literal(lit),
            brig_ast::Expression::Identifier(ident) => self.traverse_identifier(ident, scope),
        }
    }

    pub fn traverse_var_decl(&mut self, data: VariableDeclaration, scope: Scope) -> Res {
        let decls = &mut self.scope_data_mut(scope).var_decls;
        let id = resolve::make_var_id(scope, decls.len());

        let var = Var {
            ident: data.name,
            size: data.ty.size,
            id,
        };

        decls.push(VarDecl {
            var: var.clone(),
            scope,
        });

        if data.expr.is_none() {
            return Ok(None);
        }
        let expr = data.expr.unwrap();
        let span = expr.span();

        let operand = self
            .traverse_rvalue(expr, scope)?
            .expect("Expected an operand");

        self.current_block_mut().statements.push(crate::Statement {
            kind: StatementKind::Assign(Lvalue::Variable(var), operand.clone()),
            span,
        });

        Ok(Some(operand))
    }

    pub fn traverse_return(&mut self, stmt: ReturnStatement, parent_scope: Scope) -> Res {
        let operand = self
            .traverse_expr(stmt.expr.clone(), parent_scope)?
            .expect("Expected an operand");

        self.current_block_mut().terminator = Some(Terminator {
            kind: crate::TerminatorKind::Return {
                expr: operand.clone(),
            },
            span: stmt.span,
            scope: parent_scope,
        });

        Ok(Some(operand))
    }
}
