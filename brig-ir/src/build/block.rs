use brig_ast::Block;
use brig_diagnostic::Result;

use crate::{BasicBlock, Operand, Scope, ScopeData, Terminator, TerminatorKind};

pub type BlockRes = Result<(BasicBlock, Option<Operand>)>;
impl crate::Ir {
    pub fn traverse_block(
        &mut self,
        block: Block,
        parent_scope: Scope,
        from: Option<BasicBlock>,
    ) -> BlockRes {
        let scope = self.alloc_scope(ScopeData {
            temp_decls: vec![],
            var_decls: vec![],
            parent: Some(parent_scope),
            span: block.span,
        });

        let from = from.unwrap_or(self.current_block_id());
        let first = self.alloc_empty_basic_block(scope);
        if self.basic_block_data(from).terminator.is_none() {
            self.basic_block_data_mut(from).terminator = Some(Terminator {
                kind: TerminatorKind::Goto { target: first },
                span: block.span,
                scope,
            });
        }
        let mut operand = None;

        for stmt in block.stmts {
            operand = match stmt {
                brig_ast::Stmt::Expr(e) => self.traverse_expr(e, scope)?,
                brig_ast::Stmt::Semi(e) => self.traverse_expr_stmt(e, scope)?,
                brig_ast::Stmt::LetDecl(d) => self.traverse_var_decl(d, scope)?,
                brig_ast::Stmt::None => operand,
                brig_ast::Stmt::Return(e) => self.traverse_return(e, scope)?,
            }
        }
        Ok((first, operand))
    }
}
