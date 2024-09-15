use brig_ast::{DeclModKind, FnDecl};
use brig_diagnostic::Result;

use crate::{
    BasicBlock, BasicBlockData, Ir, Scope, ScopeData, Terminator, TerminatorKind, Var, VarDecl,
    IR_END_BLOCK, IR_GLOBAL_SCOPE, IR_START_BLOCK,
};

mod block;
mod expr;
mod stmt;

pub struct IrBuilder {
    decl: FnDecl,
}

impl IrBuilder {
    pub fn new(decl: FnDecl) -> Self {
        Self { decl }
    }

    pub fn build(self) -> Result<Ir> {
        let mut ir = crate::Ir {
            basic_blocks: vec![],
            scopes: vec![],
            fn_name: self.decl.name.name,
            is_extern: self
                .decl
                .modifiers
                .iter()
                .any(|m| matches!(m.kind, DeclModKind::Extern)),
            fn_params: vec![],
            span: self.decl.span,
        };

        // TODO: when we have a proper symbol table, we can omit this and throw an error instead.
        // This is just a temporary solution to allow extern fn declarations without a body.
        if self.decl.body.is_none() {
            return Ok(ir);
        }
        let body = self.decl.body.unwrap();

        assert_eq!(ir.alloc_empty_basic_block(Scope(0)), IR_START_BLOCK);
        assert_eq!(ir.alloc_empty_basic_block(Scope(0)), IR_END_BLOCK);

        let global_scope = ir.alloc_scope(ScopeData {
            temp_decls: vec![],
            var_decls: vec![],
            parent: None,
            span: self.decl.span,
        });
        assert_eq!(global_scope, IR_GLOBAL_SCOPE);
        let fn_scope = ir.alloc_scope(ScopeData {
            temp_decls: vec![],
            var_decls: vec![],
            parent: Some(global_scope),
            span: self.decl.span,
        });

        for (i, param) in self.decl.parameters.iter().enumerate() {
            let var_id = crate::resolve::make_var_id(fn_scope, i);
            ir.scope_data_mut(fn_scope).var_decls.push(VarDecl {
                scope: fn_scope,
                var: Var {
                    ident: param.ident.clone(),
                    size: param.ty.size,
                    id: var_id,
                },
            });
            ir.fn_params.push(var_id);
        }

        let (first_block, _) = ir.traverse_block(body, fn_scope)?;

        assert_ne!(ir.basic_blocks.len(), 0);

        ir.basic_block_data_mut(IR_START_BLOCK).terminator = Some(Terminator {
            kind: TerminatorKind::Goto {
                target: first_block,
            },
            span: self.decl.span,
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

    pub fn current_block_id(&self) -> BasicBlock {
        BasicBlock(self.basic_blocks.len() - 1)
    }

    pub fn current_block(&self) -> &BasicBlockData {
        if let Some(last) = self.basic_blocks.last() {
            return last;
        }
        panic!("Ir has no basic blocks. Did you forget to add IR_START_BLOCK and IR_END_BLOCK?");
    }

    pub fn current_block_mut(&mut self) -> &mut BasicBlockData {
        if let Some(last) = self.basic_blocks.last_mut() {
            return last;
        }
        panic!("Ir has no basic blocks. Did you forget to add IR_START_BLOCK and IR_END_BLOCK?");
    }
}
