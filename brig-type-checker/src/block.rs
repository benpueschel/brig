use brig_ast::Block;
use brig_diagnostic::Result;

use crate::TypeChecker;

impl TypeChecker {
    pub fn check_block(&self, block: &mut Block) -> Result<()> {
        for stmt in &mut block.statements {
            self.check_statement(stmt)?;
        }
        Ok(())
    }
}
