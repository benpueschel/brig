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
            // TODO: return statemment is a branch
            _ => false,
        }
    }

    pub fn traverse_branch(&mut self, _stmt: &brig_ast::Statement, _scope: Scope) -> Branch {
        todo!("we don't even have branching yet lol")
    }
}
