use brig_ast::{AstNode, Expr, LetDecl, ReturnStmt};
use brig_diagnostic::{Error, Result};

use crate::{resolve, Lvalue, Operand, Scope, Statement, StatementKind, Terminator, Var, VarDecl};

type Res = Result<Option<Operand>>;
impl crate::Ir {
    pub fn traverse_expr_stmt(&mut self, expr: Expr, scope: Scope) -> Res {
        let span = expr.span();
        match expr {
            Expr::Call(call) => {
                let call = self.traverse_call_expression(call, scope)?;
                self.current_block_mut().statements.push(Statement {
                    kind: StatementKind::FunctionCall(call),
                    span,
                });
                Ok(None)
            }
            Expr::Block(block) => Ok(self.traverse_block(block, scope, None)?.1),
            Expr::If(if_expr) => self.traverse_if_expression(if_expr, scope),
            Expr::Bin(expr) => self.traverse_binary(expr, scope),
            Expr::Assign(lhs, rhs) => {
                let lhs = self.traverse_lvalue(*lhs, scope)?;
                let rhs = self.traverse_expr(*rhs, scope)?.ok_or_else(|| {
                    Error::other("right-hand side of assignment is not an rvalue", span)
                })?;
                self.current_block_mut().statements.push(Statement {
                    kind: StatementKind::Assign(lhs, rhs),
                    span,
                });
                Ok(None)
            }
            Expr::Lit(lit) => self.traverse_literal(lit),
            Expr::Ident(ident) => self.traverse_identifier(ident, scope),
            Expr::AdtInit(adt) => self.traverse_adt_init(adt, scope),
        }
    }

    pub fn traverse_var_decl(&mut self, data: LetDecl, scope: Scope) -> Res {
        let decls = &mut self.scope_data_mut(scope).var_decls;
        let id = resolve::make_var_id(scope, decls.len());
        let ty = brig_ty::resolve::parse_ast_ty(&data.ty)?;

        let var = Var {
            ident: data.name,
            id,
            ty,
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
            .traverse_expr(expr, scope)?
            .expect("Expected an operand");

        self.current_block_mut().statements.push(crate::Statement {
            kind: StatementKind::Assign(Lvalue::Variable(var), operand.clone()),
            span,
        });

        Ok(Some(operand))
    }

    pub fn traverse_return(&mut self, stmt: ReturnStmt, parent_scope: Scope) -> Res {
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
