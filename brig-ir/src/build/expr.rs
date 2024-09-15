use brig_ast::{AstNode, BinExpr, BinOp, CallExpr, Expr};
use brig_diagnostic::{Error, Result};

use crate::{
    resolve, FunctionCall, Lvalue, Operand, OperandKind, Scope, Statement, StatementKind, TempDecl,
    TempVal,
};

type Res = Result<Option<Operand>>;

impl crate::Ir {
    pub fn traverse_expr(&mut self, expr: Expr, scope: Scope) -> Res {
        match expr {
            Expr::Call(c) => self.traverse_call_expression(c, scope),
            Expr::Lit(lit) => self.traverse_literal(lit),
            Expr::Bin(expr) => self.traverse_binary(expr, scope),
            Expr::Ident(ident) => self.traverse_identifier(ident, scope),
            Expr::Block(block) => Ok(self.traverse_block(block, scope)?.1),
        }
    }

    pub fn traverse_binary(&mut self, expr: BinExpr, scope: Scope) -> Res {
        if let BinOp::Assign = expr.op {
            return Err(Error::other(
                "assignment operator '=' is not an rvalue expression",
                expr.span,
            ));
        }

        let left = self.traverse_rvalue(*expr.lhs, scope)?.ok_or_else(|| {
            Error::other(
                "left-hand side of binary expression is not an rvalue",
                expr.span,
            )
        })?;
        let right = self.traverse_rvalue(*expr.rhs, scope)?.ok_or_else(|| {
            Error::other(
                "right-hand side of binary expression is not an rvalue",
                expr.span,
            )
        })?;
        let size = left.size.max(right.size);

        let temp = Lvalue::Temp(self.alloc_temp(size, scope));
        self.current_block_mut().statements.push(Statement {
            span: expr.span,
            kind: StatementKind::Assign(temp.clone(), left),
        });
        self.current_block_mut().statements.push(Statement {
            span: expr.span,
            kind: StatementKind::Modify(temp.clone(), expr.op.into(), right),
        });
        Ok(Some(Operand {
            size,
            kind: OperandKind::Consume(temp),
        }))
    }

    pub fn traverse_lvalue(&mut self, expr: Expr, scope: Scope) -> Result<Lvalue> {
        match expr {
            brig_ast::Expr::Ident(ident) => {
                let var = resolve::resolve_var(self, ident.clone(), scope).ok_or_else(|| {
                    Error::other(
                        format!("variable '{}' not found", *ident.name.as_str()),
                        ident.span,
                    )
                })?;
                Ok(Lvalue::Variable(var))
            }
            x => Err(Error::other(
                format!("expression '{:?}' is not an lvalue", x),
                x.span(),
            )),
        }
    }

    pub fn traverse_rvalue(&mut self, expr: Expr, scope: Scope) -> Res {
        match expr {
            Expr::Bin(expr) => self.traverse_binary(expr, scope),
            Expr::Call(call) => self.traverse_call_expression(call, scope),
            Expr::Block(block) => self.traverse_block(block, scope).map(|x| x.1),
            Expr::Lit(lit) => self.traverse_literal(lit),
            Expr::Ident(ident) => self.traverse_identifier(ident, scope),
        }
    }

    pub fn traverse_literal(&mut self, lit: brig_ast::Lit) -> Res {
        match lit.value {
            brig_ast::LitVal::Int(val) => Ok(Some(Operand {
                size: lit.ty.size(),
                kind: OperandKind::IntegerLit(val.value),
            })),
            brig_ast::LitVal::Unit => Ok(Some(Operand {
                size: 0,
                kind: OperandKind::Unit,
            })),
        }
    }

    pub fn traverse_identifier(&mut self, ident: brig_ast::Ident, scope: Scope) -> Res {
        let var = resolve::resolve_var(self, ident.clone(), scope).ok_or_else(|| {
            Error::other(
                format!("variable '{}' not found", *ident.name.as_str()),
                ident.span,
            )
        })?;
        Ok(Some(Operand {
            size: var.size,
            kind: OperandKind::Consume(Lvalue::Variable(var)),
        }))
    }

    pub fn traverse_call_expression(&mut self, call: CallExpr, scope: Scope) -> Res {
        let fn_ty = match call.fn_ty {
            Some(ty) => ty,
            None => return Err(Error::other("function type not found", call.span)),
        };

        let mut args = vec![];
        for arg in call.args {
            let span = arg.span();
            args.push(
                self.traverse_rvalue(arg, scope)?
                    .ok_or_else(|| Error::other("argument is not an rvalue", span))?,
            );
        }

        let size = fn_ty.ret.size;
        let temp = Lvalue::Temp(self.alloc_temp(size, scope));
        let func = FunctionCall {
            name: call.callee.name,
            ty: fn_ty,
            span: call.span,
            args,
        };

        self.current_block_mut().statements.push(Statement {
            span: call.span,
            kind: StatementKind::Assign(
                temp.clone(),
                Operand {
                    kind: OperandKind::FunctionCall(func),
                    size,
                },
            ),
        });

        Ok(Some(Operand {
            kind: OperandKind::Consume(temp),
            size,
        }))
    }

    fn alloc_temp(&mut self, size: usize, scope: Scope) -> TempVal {
        let data = self.scope_data_mut(scope);
        data.temp_decls.push(TempDecl { scope });
        TempVal {
            size,
            index: data.temp_decls.len() as u64 - 1,
        }
    }
}
