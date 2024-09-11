use brig_ast::{AstNode, BinaryExpression, BinaryOperator, CallExpression, Expression};
use brig_diagnostic::{Error, Result};

use crate::{
    resolve, FunctionCall, Lvalue, Operand, OperandKind, Rvalue, Scope, Statement, StatementKind,
    TempDecl, TempVal,
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
            brig_ast::Expression::Identifier(ident) => {
                let var = resolve::resolve_var(self, ident.clone(), scope).ok_or_else(|| {
                    Error::other(format!("variable '{}' not found", ident.name), ident.span)
                })?;
                Ok((
                    Operand {
                        size: var.size,
                        kind: OperandKind::Consume(Lvalue::Variable(var)),
                    },
                    vec![],
                ))
            }
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
        let size = left.size.max(right.size);

        stmts.extend(right_stmts);
        let temp = Lvalue::Temp(self.alloc_temp(size, scope));
        stmts.push(Statement {
            span: expr.span,
            kind: StatementKind::Assign(temp.clone(), left),
        });
        stmts.push(Statement {
            span: expr.span,
            kind: StatementKind::Modify(temp.clone(), expr.op.into(), right),
        });
        Ok((
            Operand {
                size,
                kind: OperandKind::Consume(temp),
            },
            stmts,
        ))
    }

    pub(crate) fn traverse_lvalue(&mut self, expr: Expression, scope: Scope) -> Result<Lvalue> {
        match expr {
            brig_ast::Expression::Identifier(ident) => {
                let var = resolve::resolve_var(self, ident.clone(), scope).ok_or_else(|| {
                    Error::other(format!("variable '{}' not found", ident.name), ident.span)
                })?;
                Ok(Lvalue::Variable(var))
            }
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
                brig_ast::LiteralValue::Int(val) => Ok((
                    Operand {
                        size: 0,
                        kind: OperandKind::IntegerLit(val.value),
                    },
                    vec![],
                )),
                brig_ast::LiteralValue::Unit => Ok((
                    Operand {
                        size: 0,
                        kind: OperandKind::Unit,
                    },
                    vec![],
                )),
            },
            Expression::Identifier(ident) => {
                let var = resolve::resolve_var(self, ident.clone(), scope).ok_or_else(|| {
                    Error::other(format!("variable '{}' not found", ident.name), ident.span)
                })?;
                Ok((
                    Operand {
                        size: var.size,
                        kind: OperandKind::Consume(Lvalue::Variable(var)),
                    },
                    vec![],
                ))
            }
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

        let temp = Lvalue::Temp(self.alloc_temp(fn_ty.ret.size, scope));
        let size = fn_ty.ret.size;
        let func = FunctionCall {
            name: call.callee.name,
            ty: fn_ty,
            args,
            span: call.span,
        };
        stmts.push(Statement {
            span: call.span,
            kind: StatementKind::Assign(
                temp.clone(),
                Operand {
                    kind: OperandKind::FunctionCall(func),
                    size,
                },
            ),
        });
        Ok((
            Operand {
                kind: OperandKind::Consume(temp),
                size,
            },
            stmts,
        ))
    }

    fn alloc_temp(&mut self, size: usize, scope: Scope) -> TempVal {
        let data = self.scope_data_mut(scope);
        data.temp_decls.push(TempDecl { scope });
        TempVal {
            size,
            index: data.temp_decls.len() - 1,
        }
    }
}
