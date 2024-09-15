use brig_ast::{AstNode, BinOp, Expr, LetDecl, ReturnStmt};
use brig_diagnostic::{Error, Result};

use crate::{
    resolve, Lvalue, Operand, OperandKind, Scope, Statement, StatementKind, Terminator, Var,
    VarDecl,
};

type Res = Result<Option<Operand>>;
impl crate::Ir {
    pub fn traverse_expr_stmt(&mut self, expr: Expr, scope: Scope) -> Res {
        match expr {
            brig_ast::Expr::Call(call) => {
                let call = self.traverse_call_expression(call, scope)?;
                let size = call.ty.ret.size;
                let temp = Lvalue::Temp(self.alloc_temp(size, scope));

                self.current_block_mut().statements.push(Statement {
                    span: call.span,
                    kind: StatementKind::Assign(
                        temp.clone(),
                        Operand {
                            kind: OperandKind::FunctionCall(call),
                            size,
                        },
                    ),
                });

                Ok(Some(Operand {
                    kind: OperandKind::Consume(temp),
                    size,
                }))
            }
            brig_ast::Expr::Block(block) => Ok(self.traverse_block(block, scope, None)?.1),
            brig_ast::Expr::If(if_expr) => self.traverse_if_expression(if_expr, scope),
            brig_ast::Expr::Bin(expr) => {
                // TODO: move into a separate assignment statement instead of an expression
                if let BinOp::Assign = expr.op {
                    let left = self.traverse_lvalue(*expr.lhs, scope)?;
                    let right = self.traverse_expr(*expr.rhs, scope)?.ok_or_else(|| {
                        Error::other("right-hand side of assignment is not an rvalue", expr.span)
                    })?;
                    self.current_block_mut().statements.push(crate::Statement {
                        kind: StatementKind::Assign(left, right),
                        span: expr.span,
                    });
                }
                Ok(None)
            }
            brig_ast::Expr::Lit(lit) => self.traverse_literal(lit),
            brig_ast::Expr::Ident(ident) => self.traverse_identifier(ident, scope),
        }
    }

    pub fn traverse_var_decl(&mut self, data: LetDecl, scope: Scope) -> Res {
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
