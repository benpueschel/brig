use brig_ast::Block;
use brig_diagnostic::Result;

use crate::{BasicBlock, Operand, Scope, ScopeData, Terminator, TerminatorKind};

pub type BlockRes = Result<(BasicBlock, Option<Operand>)>;
impl crate::Ir {
    pub fn traverse_block(&mut self, block: Block, parent_scope: Scope) -> BlockRes {
        let scope = self.alloc_scope(ScopeData {
            temp_decls: vec![],
            var_decls: vec![],
            parent: Some(parent_scope),
            span: block.span,
        });

        let first = self.alloc_empty_basic_block(scope);
        let mut operand = None;

        for stmt in block.statements {
            operand = match stmt {
                brig_ast::Statement::Expr(e) => self.traverse_expr(e, scope)?,
                brig_ast::Statement::Semi(e) => self.traverse_expr_stmt(e, scope)?,
                brig_ast::Statement::VariableDeclaration(d) => self.traverse_var_decl(d, scope)?,
                brig_ast::Statement::None => operand,
                brig_ast::Statement::Return(e) => self.traverse_return(e, scope)?,
            }
        }

        if self.current_block_id() != first {
            self.basic_block_data_mut(first).terminator = Some(Terminator {
                span: block.span,
                kind: TerminatorKind::Goto {
                    target: self.current_block_id(),
                },
                scope,
            });
        }

        Ok((first, operand))
    }
}
