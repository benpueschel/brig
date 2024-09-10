use brig_ast::Block;
use brig_common::Span;
use brig_diagnostic::Result;

use crate::{BasicBlock, BasicBlockData, Scope, ScopeData, Terminator, TerminatorKind};

use super::BasicBlockBuilder;

impl crate::Ir {
    pub fn traverse_block(
        &mut self,
        block: Block,
        parent_scope: Option<Scope>,
    ) -> Result<(BasicBlock, BasicBlock)> {
        let scope = self.alloc_scope(ScopeData {
            temp_decls: vec![],
            var_decls: vec![],
            parent: parent_scope,
            span: block.span,
        });
        let mut statements = vec![];
        let mut blocks = vec![];

        for stmt in block.statements {
            if self.is_branching(&stmt) {
                let branch = self.traverse_branch(&stmt, scope)?;
                statements.extend(branch.condition_stmts);

                let block = self.alloc_basic_block(BasicBlockData {
                    statements,
                    scope,
                    terminator: Some(branch.terminator),
                });

                if let Some(last_block) = blocks.last() {
                    self.terminate_leaves(last_block, block, scope);
                }

                blocks.push(BasicBlockBuilder {
                    block,
                    leaves: branch.leaves,
                });
                statements = Vec::new();
            } else {
                statements.extend(self.traverse_statement(stmt, scope)?);
            }
        }

        let last_block = self.alloc_basic_block(BasicBlockData {
            statements,
            terminator: None,
            scope,
        });

        if let Some(block) = blocks.last() {
            self.terminate_leaves(block, last_block, scope);
        }

        let first_block = blocks.first().map(|b| b.block).unwrap_or(last_block);
        Ok((first_block, last_block))
    }

    fn terminate_leaves(&mut self, block: &BasicBlockBuilder, target: BasicBlock, scope: Scope) {
        for leaf in &block.leaves {
            let term = &mut self.basic_block_data_mut(*leaf).terminator;
            if term.is_none() {
                *term = Some(Terminator {
                    kind: TerminatorKind::Goto { target },
                    scope,
                    span: Span::default(),
                });
            }
        }
    }
}
