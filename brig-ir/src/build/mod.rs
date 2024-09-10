use brig_ast::FunctionDeclaration;
use brig_diagnostic::Result;

use crate::{
    BasicBlock, BasicBlockData, Scope, ScopeData, Terminator, TerminatorKind, Var, VarDecl,
    IR_END_BLOCK, IR_START_BLOCK, VAR_UNINITIALIZED,
};

pub mod block;
pub mod branch;
pub mod expr;
pub mod stmt;

pub struct BasicBlockBuilder {
    pub block: BasicBlock,
    pub leaves: Vec<BasicBlock>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct IrBuilder {}

impl IrBuilder {
    pub fn new() -> Self {
        Self {}
    }

    pub fn build(data: FunctionDeclaration) -> Result<crate::Ir> {
        let mut ir = crate::Ir {
            basic_blocks: vec![],
            scopes: vec![],
            fn_name: data.name.name,
            span: data.span,
        };

        assert_eq!(ir.alloc_empty_basic_block(Scope(0)), IR_START_BLOCK);
        assert_eq!(ir.alloc_empty_basic_block(Scope(0)), IR_END_BLOCK);

        let scope = ir.alloc_scope(ScopeData {
            temp_decls: vec![],
            var_decls: vec![],
            parent: None,
            span: data.span,
        });

        for param in &data.parameters {
            let data = ir.scope_data_mut(scope);
            let decl = VarDecl {
                var: Var {
                    name: param.ident.name.clone(),
                    id: VAR_UNINITIALIZED,
                    ty: param.ty.clone(),
                    span: param.span,
                },
                scope,
            };
            data.var_decls.push(decl);
        }

        let (first_node, last_node) = ir.traverse_block(data.body, Some(scope))?;

        ir.basic_block_data_mut(IR_START_BLOCK).terminator = Some(Terminator {
            kind: TerminatorKind::Goto { target: first_node },
            span: data.span,
            scope,
        });

        ir.basic_block_data_mut(last_node).terminator = Some(Terminator {
            kind: TerminatorKind::Goto {
                target: IR_END_BLOCK,
            },
            span: data.span,
            scope,
        });
        Ok(ir)
    }
}

impl crate::Ir {
    pub fn alloc_basic_block(&mut self, data: BasicBlockData) -> BasicBlock {
        let index = self.basic_blocks.len();
        self.basic_blocks.push(data);
        BasicBlock(index)
    }
    pub fn alloc_empty_basic_block(&mut self, scope: Scope) -> BasicBlock {
        self.alloc_basic_block(BasicBlockData {
            statements: vec![],
            terminator: None,
            scope,
        })
    }
    pub fn alloc_scope(&mut self, data: ScopeData) -> Scope {
        self.scopes.push(data);
        Scope(self.scopes.len() - 1)
    }
}
