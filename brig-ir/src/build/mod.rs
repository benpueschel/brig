use brig_ast::FunctionDeclaration;
use brig_diagnostic::Result;

use crate::{
    BasicBlock, BasicBlockData, Scope, ScopeData, Terminator, TerminatorKind, Var, VarDecl,
    IR_END_BLOCK, IR_GLOBAL_SCOPE, IR_START_BLOCK,
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
            fn_params: vec![],
            span: data.span,
        };

        assert_eq!(ir.alloc_empty_basic_block(Scope(0)), IR_START_BLOCK);
        assert_eq!(ir.alloc_empty_basic_block(Scope(0)), IR_END_BLOCK);

        let global_scope = ir.alloc_scope(ScopeData {
            temp_decls: vec![],
            var_decls: vec![],
            parent: None,
            span: data.span,
        });
        assert_eq!(global_scope, IR_GLOBAL_SCOPE);
        let fn_scope = ir.alloc_scope(ScopeData {
            temp_decls: vec![],
            var_decls: vec![],
            parent: Some(global_scope),
            span: data.span,
        });

        for (i, param) in data.parameters.iter().enumerate() {
            let var_id = crate::resolve::make_var_id(fn_scope, i);
            ir.scope_data_mut(fn_scope).var_decls.push(VarDecl {
                scope: fn_scope,
                var: Var {
                    name: param.ident.name.clone(),
                    ty: param.ty.clone(),
                    span: param.span,
                    id: var_id as usize, // FIXME: ugly, u64 rules
                },
            });
            ir.fn_params.push(var_id);
        }

        let (first_node, last_node) = ir.traverse_block(data.body, Some(fn_scope))?;

        ir.basic_block_data_mut(IR_START_BLOCK).terminator = Some(Terminator {
            kind: TerminatorKind::Goto { target: first_node },
            span: data.span,
            scope: fn_scope,
        });

        ir.basic_block_data_mut(last_node).terminator = Some(Terminator {
            kind: TerminatorKind::Goto {
                target: IR_END_BLOCK,
            },
            span: data.span,
            scope: fn_scope,
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
