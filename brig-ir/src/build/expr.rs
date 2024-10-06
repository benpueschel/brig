use brig_ast::{AstNode, BinExpr, CallExpr, Expr, IfExpr};
use brig_common::Span;
use brig_diagnostic::{Error, Result};
use brig_ty::{Adt, TyKind};

use crate::{
    resolve, FunctionCall, Lvalue, Operand, OperandKind, Rvalue, Scope, Statement, StatementKind,
    TempDecl, TempVal, Terminator, TerminatorKind,
};

type Res = Result<Option<Operand>>;

impl crate::Ir {
    pub fn traverse_binary_as_rvalue(&mut self, expr: BinExpr, scope: Scope) -> Result<Rvalue> {
        let left = self.traverse_expr(*expr.lhs, scope)?.ok_or_else(|| {
            Error::other(
                "left-hand side of binary expression is not an rvalue",
                expr.span,
            )
        })?;
        let right = self.traverse_expr(*expr.rhs, scope)?.ok_or_else(|| {
            Error::other(
                "right-hand side of binary expression is not an rvalue",
                expr.span,
            )
        })?;
        Ok(Rvalue::BinaryExpr(expr.op.into(), left, right))
    }

    pub fn traverse_binary(&mut self, expr: BinExpr, scope: Scope) -> Res {
        let left = self.traverse_expr(*expr.lhs, scope)?.ok_or_else(|| {
            Error::other(
                "left-hand side of binary expression is not an rvalue",
                expr.span,
            )
        })?;
        let right = self.traverse_expr(*expr.rhs, scope)?.ok_or_else(|| {
            Error::other(
                "right-hand side of binary expression is not an rvalue",
                expr.span,
            )
        })?;
        debug_assert_eq!(left.ty, right.ty);
        let ty = left.ty;

        let temp = Lvalue::Temp(self.alloc_temp(ty, scope));
        self.current_block_mut().statements.push(Statement {
            span: expr.span,
            kind: StatementKind::Assign(temp.clone(), left),
        });
        self.current_block_mut().statements.push(Statement {
            span: expr.span,
            kind: StatementKind::Modify(temp.clone(), expr.op.into(), right),
        });
        Ok(Some(Operand {
            kind: OperandKind::Consume(temp),
            ty,
        }))
    }

    pub fn traverse_lvalue(&mut self, expr: Expr, scope: Scope) -> Result<Lvalue> {
        match expr {
            brig_ast::Expr::Ident(ident) => {
                let var = resolve::resolve_var(self, ident, scope).ok_or_else(|| {
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

    pub fn traverse_rvalue(&mut self, expr: Expr, scope: Scope) -> Result<Rvalue> {
        let span = expr.span();
        match expr {
            Expr::Call(call) => Ok(Rvalue::Call(self.traverse_call_expression(call, scope)?)),
            Expr::AdtInit(adt) => Ok(self
                .traverse_adt_init(adt, scope)?
                .ok_or_else(|| Error::other("adt init expression did not return an operand", span))?
                .into()),
            Expr::Lit(lit) => match lit.value {
                brig_ast::LitVal::Int(val) => Ok(Rvalue::IntegerLit(
                    brig_ty::resolve::parse_lit_ast_ty(&lit.ty)?,
                    val.value,
                )),
                brig_ast::LitVal::Bool(val) => Ok(Rvalue::IntegerLit(
                    brig_ty::resolve::parse_lit_ast_ty(&lit.ty)?,
                    val as usize,
                )),
                brig_ast::LitVal::Unit => Ok(Rvalue::Unit),
            },
            Expr::Bin(expr) => self.traverse_binary_as_rvalue(expr, scope),
            Expr::Assign(lhs, rhs) => {
                let lhs = self.traverse_lvalue(*lhs, scope)?;
                let rhs = self.traverse_expr(*rhs, scope)?.ok_or_else(|| {
                    Error::other("right-hand side of assignment is not an rvalue", span)
                })?;
                self.current_block_mut().statements.push(Statement {
                    kind: StatementKind::Assign(lhs, rhs),
                    span,
                });
                Ok(Rvalue::Unit)
            }
            Expr::Ident(ident) => {
                let var = resolve::resolve_var(self, ident, scope).ok_or_else(|| {
                    Error::other(
                        format!("variable '{}' not found", *ident.name.as_str()),
                        ident.span,
                    )
                })?;
                Ok(Rvalue::Variable(var))
            }
            Expr::Block(block) => {
                let (_block, op) = self.traverse_block(block, scope, None)?;
                Ok(op.map_or(Rvalue::Unit, |op| op.into()))
            }
            Expr::If(if_expr) => self.traverse_rvalue(*if_expr.cond, scope),
        }
    }

    pub fn traverse_expr(&mut self, expr: Expr, scope: Scope) -> Res {
        let span = expr.span();
        match expr {
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
            Expr::Call(call) => {
                let ty = call
                    .fn_ty
                    .as_ref()
                    .ok_or_else(|| Error::other("function type not resolved", call.span))?;

                let ty = brig_ty::resolve::parse_ast_ty(&ty.ret)?;
                let call = self.traverse_call_expression(call, scope)?;
                let temp = Lvalue::Temp(self.alloc_temp(ty, scope));

                self.current_block_mut().statements.push(Statement {
                    span: call.span,
                    kind: StatementKind::Assign(
                        temp.clone(),
                        Operand {
                            kind: OperandKind::FunctionCall(call),
                            ty,
                        },
                    ),
                });

                Ok(Some(Operand {
                    kind: OperandKind::Consume(temp),
                    ty,
                }))
            }
            Expr::Block(block) => self.traverse_block(block, scope, None).map(|x| x.1),
            Expr::If(if_expr) => self.traverse_if_expression(if_expr, scope),
            Expr::Lit(lit) => self.traverse_literal(lit),
            Expr::Ident(ident) => self.traverse_identifier(ident, scope),
            Expr::AdtInit(adt) => self.traverse_adt_init(adt, scope),
        }
    }

    pub fn traverse_literal(&mut self, lit: brig_ast::Lit) -> Res {
        let ty = brig_ty::resolve::parse_lit_ast_ty(&lit.ty)?;
        match lit.value {
            brig_ast::LitVal::Int(val) => Ok(Some(Operand {
                kind: OperandKind::IntegerLit(ty, val.value),
                ty,
            })),
            brig_ast::LitVal::Bool(val) => Ok(Some(Operand {
                kind: OperandKind::IntegerLit(ty, val as usize),
                ty,
            })),
            brig_ast::LitVal::Unit => Ok(Some(Operand {
                kind: OperandKind::Unit,
                ty,
            })),
        }
    }

    pub fn traverse_identifier(&mut self, ident: brig_ast::Ident, scope: Scope) -> Res {
        let var = resolve::resolve_var(self, ident, scope).ok_or_else(|| {
            Error::other(
                format!("variable '{}' not found", *ident.name.as_str()),
                ident.span,
            )
        })?;
        Ok(Some(Operand {
            ty: var.ty,
            kind: OperandKind::Consume(Lvalue::Variable(var)),
        }))
    }

    pub fn traverse_if_expression(&mut self, if_expr: IfExpr, scope: Scope) -> Res {
        let cond = self.traverse_rvalue(*if_expr.cond, scope)?;

        let current = self.current_block_id();
        let target = self.alloc_empty_basic_block(scope);

        let mut op = None;
        let (then_block, then_op) =
            self.traverse_block(if_expr.then_block, scope, Some(current))?;
        if let Some(then_op) = &then_op {
            op = Some(self.alloc_temp(then_op.ty, scope));
            let op = Lvalue::Temp(op.unwrap());
            self.current_block_mut().statements.push(Statement {
                span: if_expr.span,
                kind: StatementKind::Assign(op, then_op.clone()),
            });
        }

        // Terminator for the last block of the then branch, point to the target block
        if self.current_block().terminator.is_none() {
            self.current_block_mut().terminator = Some(Terminator {
                kind: TerminatorKind::Goto { target },
                span: if_expr.span,
                scope,
            });
        }

        let else_block = match if_expr.else_block {
            Some(else_block) => {
                let (else_block, else_op) =
                    self.traverse_block(else_block, scope, Some(current))?;

                match (else_op, op) {
                    (Some(else_op), Some(op)) => {
                        self.current_block_mut().statements.push(Statement {
                            span: if_expr.span,
                            kind: StatementKind::Assign(Lvalue::Temp(op), else_op),
                        });
                    }
                    (None, None) => {}
                    (None, Some(_)) => {
                        panic!("else block did not return an operand, but then block did.")
                    }
                    (Some(_), None) => {
                        panic!("else block returned an operand, but then block did not.")
                    }
                }

                // Terminator for the last block of the else branch, point to the target block
                if self.current_block().terminator.is_none() {
                    self.current_block_mut().terminator = Some(Terminator {
                        kind: TerminatorKind::Goto { target },
                        span: if_expr.span,
                        scope,
                    });
                }
                else_block
            }
            None => target,
        };
        self.basic_block_data_mut(current).terminator = Some(Terminator {
            kind: TerminatorKind::If {
                condition: cond,
                targets: (then_block, else_block),
            },
            span: if_expr.span,
            scope,
        });

        // NOTE: this is a hack - I'd need to create the target block after both branches,
        // but each branch needs to point to the target block. So the target block is essentially
        // empty and only contains a goto to the next block.
        let next = self.alloc_empty_basic_block(scope);
        self.basic_block_data_mut(target).terminator = Some(Terminator {
            kind: TerminatorKind::Goto { target: next },
            span: if_expr.span,
            scope,
        });

        Ok(op.map(|op| Operand {
            ty: op.ty,
            kind: OperandKind::Consume(Lvalue::Temp(op)),
        }))
    }

    pub fn traverse_call_expression(
        &mut self,
        call: CallExpr,
        scope: Scope,
    ) -> Result<FunctionCall> {
        let fn_ty = match call.fn_ty {
            Some(ty) => ty,
            None => return Err(Error::other("function type not found", call.span)),
        };

        let mut args = vec![];
        for arg in call.args {
            let span = arg.span();
            args.push(
                self.traverse_expr(arg, scope)?
                    .ok_or_else(|| Error::other("argument is not an rvalue", span))?,
            );
        }

        let ty = brig_ty::resolve::parse_fn_ast_ty(&fn_ty)?;

        Ok(FunctionCall {
            name: call.callee.name,
            span: call.span,
            args,
            ty,
        })
    }

    pub fn assign_copy(
        &mut self,
        lhs: &Lvalue,
        rhs: Operand,
        span: Span,
        scope: Scope,
    ) -> Result<Operand> {
        debug_assert_eq!(
            lhs.ty(),
            rhs.ty,
            "Type mismatch: expected {:?}, got {:?}",
            lhs.ty(),
            rhs.ty
        );

        if let OperandKind::Consume(lvalue) = &rhs.kind {
            Ok(match lvalue {
                Lvalue::Variable(var) => {
                    let temp = self.alloc_temp(var.ty, scope);
                    self.current_block_mut().statements.push(Statement {
                        span,
                        kind: StatementKind::Assign(Lvalue::Temp(temp), rhs.clone()),
                    });
                    Operand {
                        ty: var.ty,
                        kind: OperandKind::Consume(Lvalue::Temp(temp)),
                    }
                }
                Lvalue::FieldAccess(var, field) => {
                    let ty = { var.ty.lock().kind.clone() };
                    let ty = match ty {
                        TyKind::Adt(Adt::Struct(s)) => s
                            .fields
                            .iter()
                            .find_map(|f| if f.name == *field { Some(f.ty) } else { None })
                            .ok_or_else(|| {
                                Error::other(format!("field '{}' not found on struct", field), span)
                            })?,
                        _ => return Err(Error::other("field access on non-struct", span)),
                    };
                    let temp = self.alloc_temp(ty, scope);
                    self.current_block_mut().statements.push(Statement {
                        span,
                        kind: StatementKind::Assign(Lvalue::Temp(temp), rhs.clone()),
                    });
                    Operand {
                        ty: var.ty,
                        kind: OperandKind::Consume(Lvalue::Temp(temp)),
                    }
                }
                Lvalue::Temp(_) => rhs,
            })
        } else {
            Ok(rhs)
        }
    }

    pub fn alloc_temp(&mut self, ty: brig_ty::TyIdx, scope: Scope) -> TempVal {
        let data = self.scope_data_mut(scope);
        data.temp_decls.push(TempDecl { scope });
        TempVal {
            ty,
            index: data.temp_decls.len() as u64 - 1,
        }
    }
}
