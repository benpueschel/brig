use brig_ast::{Declaration, DeclarationKind, FunctionDeclaration};
use brig_diagnostic::Result;

use crate::TypeChecker;

impl TypeChecker {
    pub fn check_declaration(&mut self, decl: &mut Declaration) -> Result<()> {
        match decl.kind {
            DeclarationKind::Function(ref mut f) => self.check_function_declaration(f),
        }
    }

    pub fn check_function_declaration(&mut self, decl: &mut FunctionDeclaration) -> Result<()> {
        self.push_scope();

        // TODO: check parameters, check return type and maybe check if the return type matches the
        // last expression in the block.
        self.check_block(&mut decl.body)?;

        self.pop_scope();
        Ok(())
    }
}