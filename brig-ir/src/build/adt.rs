use brig_ast::{AdtInit, AstNode, StructInit};
use brig_diagnostic::{Error, Result};

use crate::{
    resolve, Ir, Lvalue, Operand, OperandKind, Scope, Statement, StatementKind, Var, VarDecl,
};

impl Ir {
    pub fn traverse_adt_init(&mut self, init: AdtInit, scope: Scope) -> Result<Option<Operand>> {
        match init {
            AdtInit::Struct(s) => self.traverse_struct_init(s, scope),
        }
    }

    pub fn traverse_struct_init(
        &mut self,
        init: StructInit,
        scope: Scope,
    ) -> Result<Option<Operand>> {
        let decls = &mut self.scope_data_mut(scope).var_decls;
        let id = resolve::make_var_id(scope, decls.len());
        let ty = brig_ty::resolve::parse_ast_ty(&init.ty.ok_or_else(|| {
            Error::other("could not find type for struct initialization", init.span)
        })?)?;

        let var = Var {
            ident: init.name,
            id,
            ty,
        };

        decls.push(VarDecl {
            var: var.clone(),
            scope,
        });
        self.scope_data_mut(scope).var_decls.push(VarDecl {
            var: var.clone(),
            scope,
        });

        for field in init.fields {
            let span = field.span();
            let lhs = Lvalue::FieldAccess(var.clone(), field.name.name);
            let op = self
                .traverse_expr(field.expr, scope)?
                .ok_or_else(|| Error::other("field expression is not an rvalue", span))?;
            let op = self.assign_copy(&lhs, op, span, scope)?;

            let stmts = &mut self.current_block_mut().statements;
            stmts.push(Statement {
                kind: StatementKind::Assign(lhs, op),
                span,
            });
        }

        Ok(Some(Operand {
            ty: var.ty,
            kind: OperandKind::Consume(Lvalue::Variable(var)),
        }))
    }
}
