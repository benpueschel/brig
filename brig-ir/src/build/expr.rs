use brig_ast::{AstNode, BinaryExpression, BinaryOperator, CallExpression, Expression, Ty, TyKind};
use brig_diagnostic::{Error, Result};

use crate::{
    FunctionCall, Lvalue, Operand, Rvalue, Scope, Statement, StatementKind, TempDecl, TempVal, Var,
    VAR_UNINITIALIZED,
};

impl crate::Ir {
    pub(crate) fn traverse_expr_stmt(
        &mut self,
        expr: brig_ast::Expression,
        scope: Scope,
    ) -> Result<Vec<Statement>> {
        match expr {
            brig_ast::Expression::Call(call) => {
                let (_op, stmts) = self.traverse_call_expression(call, scope)?;
                Ok(stmts)
            }
            brig_ast::Expression::Literal(lit) => Err(Error::other(
                format!("literal '{:?}' is not a statement", lit.value),
                lit.span,
            )),
            brig_ast::Expression::Binary(expr) => {
                if let BinaryOperator::Assign = expr.op {
                    let left = self.traverse_lvalue(*expr.lhs, scope)?;
                    let (right, mut stmts) = self.traverse_rvalue(*expr.rhs, scope)?;
                    stmts.push(Statement {
                        kind: StatementKind::Assign(left, right),
                        span: expr.span,
                    });
                    return Ok(stmts);
                }
                Ok(vec![])
            }
            brig_ast::Expression::Identifier(ident) => Err(Error::other(
                format!("identifier '{}' is not a statement", ident.name),
                ident.span,
            )),
        }
    }

    pub(crate) fn traverse_expr(
        &mut self,
        expr: brig_ast::Expression,
        scope: Scope,
    ) -> Result<(Operand, Vec<Statement>)> {
        match expr {
            brig_ast::Expression::Call(call) => self.traverse_call_expression(call, scope),
            brig_ast::Expression::Literal(_) => todo!("literal expression"),
            brig_ast::Expression::Binary(expr) => self.traverse_binary_expr(expr, scope),
            brig_ast::Expression::Identifier(ident) => Ok((
                Operand::Consume(Lvalue::Variable(Var {
                    name: ident.name,
                    id: VAR_UNINITIALIZED,
                    // TODO: get the type from the symbol table (but that's in brig-type-checker)
                    ty: Ty {
                        kind: TyKind::Unspecified,
                        span: ident.span,
                    },
                    span: ident.span,
                })),
                vec![],
            )),
        }
    }

    pub(crate) fn traverse_binary_expr_as_rvalue(
        &mut self,
        expr: BinaryExpression,
        scope: Scope,
    ) -> Result<(Rvalue, Vec<Statement>)> {
        if let BinaryOperator::Assign = expr.op {
            return Err(Error::other(
                "assignment operator '=' is not an rvalue expression",
                expr.span,
            ));
        }

        let (left, mut stmts) = self.traverse_rvalue(*expr.lhs, scope)?;
        let (right, right_stmts) = self.traverse_rvalue(*expr.rhs, scope)?;
        stmts.extend(right_stmts);

        Ok((Rvalue::BinaryExpr(expr.op.into(), left, right), stmts))
    }

    pub(crate) fn traverse_binary_expr(
        &mut self,
        expr: BinaryExpression,
        scope: Scope,
    ) -> Result<(Operand, Vec<Statement>)> {
        if let BinaryOperator::Assign = expr.op {
            return Err(Error::other(
                "assignment operator '=' is not an rvalue expression",
                expr.span,
            ));
        }
        let (left, mut stmts) = self.traverse_rvalue(*expr.lhs, scope)?;
        let (right, right_stmts) = self.traverse_rvalue(*expr.rhs, scope)?;
        stmts.extend(right_stmts);
        let temp = Lvalue::Temp(self.alloc_temp(scope));
        stmts.push(Statement {
            span: expr.span,
            kind: StatementKind::Assign(temp.clone(), left),
        });
        stmts.push(Statement {
            span: expr.span,
            kind: StatementKind::Modify(temp.clone(), expr.op.into(), right),
        });
        Ok((Operand::Consume(temp), stmts))
    }

    pub(crate) fn traverse_lvalue(&mut self, expr: Expression, _scope: Scope) -> Result<Lvalue> {
        match expr {
            brig_ast::Expression::Identifier(ident) => Ok(Lvalue::Variable(Var {
                name: ident.name,
                id: VAR_UNINITIALIZED,
                // TODO: get the type from the symbol table (but that's in brig-type-checker)
                ty: Ty {
                    kind: TyKind::Unspecified,
                    span: ident.span,
                },
                span: ident.span,
            })),
            x => Err(Error::other(
                format!("expression '{:?}' is not an lvalue", x),
                x.span(),
            )),
        }
    }

    pub(crate) fn traverse_rvalue(
        &mut self,
        expr: Expression,
        scope: Scope,
    ) -> Result<(Operand, Vec<Statement>)> {
        match expr {
            Expression::Binary(expr) => self.traverse_binary_expr(expr, scope),
            Expression::Literal(lit) => match lit.value {
                brig_ast::LiteralValue::Int(val) => Ok((Operand::IntegerLit(val.value), vec![])),
            },
            Expression::Identifier(ident) => Ok((
                Operand::Consume(Lvalue::Variable(Var {
                    name: ident.name,
                    id: VAR_UNINITIALIZED,
                    // TODO: get the type from the symbol table (but that's in brig-type-checker)
                    ty: Ty {
                        kind: TyKind::Unspecified,
                        span: ident.span,
                    },
                    span: ident.span,
                })),
                vec![],
            )),
            Expression::Call(call) => self.traverse_call_expression(call, scope),
        }
    }

    pub(crate) fn traverse_call_expression(
        &mut self,
        call: CallExpression,
        scope: Scope,
    ) -> Result<(Operand, Vec<Statement>)> {
        let fn_ty = match call.fn_ty {
            Some(ty) => ty,
            None => return Err(Error::other("function type not found", call.span)),
        };

        let mut stmts = vec![];
        let mut args = vec![];
        for arg in call.args {
            let (arg, arg_stmts) = self.traverse_rvalue(arg, scope)?;
            stmts.extend(arg_stmts);
            args.push(arg);
        }

        let func = FunctionCall {
            name: call.callee.name,
            ty: fn_ty,
            args,
            span: call.span,
        };
        let temp = Lvalue::Temp(self.alloc_temp(scope));
        stmts.push(Statement {
            span: call.span,
            kind: StatementKind::Assign(temp.clone(), Operand::FunctionCall(func)),
        });
        // TODO: figure out what to do with the return value
        Ok((Operand::Consume(temp), stmts))
    }

    fn alloc_temp(&mut self, scope: Scope) -> TempVal {
        let data = self.scope_data_mut(scope);
        data.temp_decls.push(TempDecl { scope });
        TempVal(data.temp_decls.len() - 1)
    }
}
