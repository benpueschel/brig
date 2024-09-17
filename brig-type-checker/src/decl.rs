use brig_ast::{Decl, DeclKind, FnDecl, FnTy, Ty, TyKind};
use brig_diagnostic::Result;

use crate::TypeChecker;

impl TypeChecker {
    pub fn check_declaration(&mut self, decl: &mut Decl) -> Result<()> {
        // TODO: resolve global declarations
        match decl.kind {
            DeclKind::Fn(ref mut f) => self.check_function_declaration(f),
        }
    }

    pub fn check_function_declaration(&mut self, decl: &mut FnDecl) -> Result<()> {
        if decl.return_ty.kind == TyKind::Unspecified {
            decl.return_ty.kind = TyKind::Lit(brig_ast::LitTy::Unit);
        }

        let kind = TyKind::Fn(FnTy {
            name: decl.name.name,
            span: decl.span,
            args: decl.parameters.iter().map(|p| p.ty.clone()).collect(),
            ret: Box::new(decl.return_ty.clone()),
        });
        let ty = Ty {
            kind,
            span: decl.span,
        };
        self.add_symbol(decl.name.name, ty);

        self.push_scope();

        for param in &decl.parameters {
            self.add_symbol(param.ident.name, param.ty.clone());
        }

        // TODO: check parameters, check return type and maybe check if the return type matches the
        // last expression in the block.
        if let Some(ref mut block) = decl.body {
            self.check_block(block, Some(&decl.return_ty))?;
        }

        self.pop_scope();
        Ok(())
    }
}
