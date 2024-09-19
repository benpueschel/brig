use brig_ast::{Decl, DeclKind, FnDecl, FnTy, StructDecl, Ty, TyKind};
use brig_diagnostic::Result;

use crate::TypeChecker;

impl TypeChecker {
    pub fn check_declaration(&mut self, decl: &mut Decl) -> Result<()> {
        // TODO: resolve global declarations
        match decl.kind {
            DeclKind::Fn(ref mut f) => self.check_function_declaration(f),
            DeclKind::Struct(ref mut s) => self.check_struct_declaration(s),
        }
    }

    /// TODO: check for cyclic dependencies
    pub fn check_struct_declaration(&mut self, decl: &mut StructDecl) -> Result<()> {
        for field in &decl.fields {
            self.add_symbol(field.name.name, field.ty.clone());
        }

        Ok(())
    }

    pub fn check_function_declaration(&mut self, decl: &mut FnDecl) -> Result<()> {
        if decl.return_ty.kind == TyKind::Unspecified {
            decl.return_ty.kind = TyKind::Lit(brig_ast::LitTy::Unit);
        }

        let kind = TyKind::Fn(FnTy {
            name: decl.name.name,
            span: decl.span,
            args: decl
                .parameters
                .iter()
                .map(|p| brig_ast::Field {
                    name: p.ident.clone(),
                    ty: p.ty.clone(),
                })
                .collect(),
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
