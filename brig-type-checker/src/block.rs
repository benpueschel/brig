use brig_ast::Block;
use brig_diagnostic::Result;

use crate::TypeChecker;

impl TypeChecker {
    pub fn check_block(&mut self, block: &mut Block) -> Result<()> {
        self.push_scope();
        for stmt in &mut block.statements {
            self.check_statement(stmt)?;
        }
        self.pop_scope();
        Ok(())
    }
}
