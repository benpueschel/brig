use brig_ast::ReturnStatement;
use brig_diagnostic::Result;

use crate::{BasicBlock, Scope, Statement, Terminator};

pub struct Branch {
    pub terminator: Terminator,
    pub condition_stmts: Vec<Statement>,
    pub leaves: Vec<BasicBlock>,
}

impl crate::Ir {
    #[allow(clippy::match_single_binding)]
    pub fn is_branching(&self, stmt: &brig_ast::Statement) -> bool {
        match stmt {
            brig_ast::Statement::Expression(expr) => match expr {
                // TODO: If expression is a comparison, return true
                // TODO: loop expression is a branch
                _ => false,
            },
            brig_ast::Statement::Return(_) => true,
            _ => false,
        }
    }

    pub fn traverse_branch(&mut self, stmt: &brig_ast::Statement, scope: Scope) -> Result<Branch> {
        match stmt {
            brig_ast::Statement::Return(ret) => self.traverse_return(ret, scope),
            x => panic!("Unexpected statement (expected a branch): {:?}", x),
        }
    }

    pub fn traverse_return(&mut self, stmt: &ReturnStatement, scope: Scope) -> Result<Branch> {
        let (expr, stmts) = self.traverse_expr(stmt.expr.clone(), scope)?;

        Ok(Branch {
            terminator: Terminator {
                kind: crate::TerminatorKind::Return { expr },
                span: stmt.span,
                scope,
            },
            condition_stmts: stmts,
            leaves: vec![],
        })
    }
}
