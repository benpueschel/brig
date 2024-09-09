use brig_ast::{Declaration, DeclarationKind, FnTy, FunctionDeclaration, Ty, TyKind};
use brig_diagnostic::Result;

use crate::TypeChecker;

impl TypeChecker {
    pub fn check_declaration(&mut self, decl: &mut Declaration) -> Result<()> {
        // TODO: resolve global declarations
        match decl.kind {
            DeclarationKind::Function(ref mut f) => self.check_function_declaration(f),
        }
    }

    pub fn check_function_declaration(&mut self, decl: &mut FunctionDeclaration) -> Result<()> {
        let kind = TyKind::Function(FnTy {
            name: decl.name.name.clone(),
            span: decl.span,
            args: decl.parameters.iter().map(|p| p.ty.clone()).collect(),
            ret: Box::new(decl.return_ty.clone()),
        });
        let ty = Ty {
            kind,
            span: decl.span,
        };
        self.add_symbol(decl.name.name.clone(), ty);

        self.push_scope();

        for param in &decl.parameters {
            self.add_symbol(param.ident.name.clone(), param.ty.clone());
        }

        // TODO: check parameters, check return type and maybe check if the return type matches the
        // last expression in the block.
        self.check_block(&mut decl.body)?;

        self.pop_scope();
        Ok(())
    }
}
