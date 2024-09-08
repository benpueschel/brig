//! The `brig_type_checker` crate is responsible for type checking the AST produced by the
//! `brig_parser` crate.
//!

use brig_ast::Program;
use brig_diagnostic::Result;

mod block;
mod decl;
mod expr;
mod stmt;

pub struct TypeChecker;

impl TypeChecker {
    pub fn check_program(&self, program: &mut Program) -> Result<()> {
        for decl in &mut program.declarations {
            self.check_declaration(decl)?;
        }
        Ok(())
    }
}
