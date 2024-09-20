use brig_ast::{Adt, Decl, DeclKind, Field, FnDecl, FnTy, Struct, StructDecl, Ty, TyKind};
use brig_diagnostic::Result;
use thin_vec::thin_vec;

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
        let mut fields = thin_vec![];
        for field in &mut decl.fields {
            self.check_ty(&mut field.ty)?;
            self.add_symbol(field.name.name, field.ty.clone());
            fields.push(Field {
                name: field.name,
                ty: field.ty.clone(),
            });
        }
        self.add_symbol(
            decl.name.name,
            Ty {
                kind: TyKind::Adt(Adt::Struct(Struct {
                    name: decl.name.name,
                    fields,
                })),
                span: decl.span,
            },
        );
        Ok(())
    }

    pub fn check_function_declaration(&mut self, decl: &mut FnDecl) -> Result<()> {
        if decl.return_ty.kind == TyKind::Unspecified {
            decl.return_ty.kind = TyKind::Lit(brig_ast::LitTy::Unit);
        }

        self.check_ty(&mut decl.return_ty)?;

        let kind = TyKind::Fn(FnTy {
            name: decl.name.name,
            span: decl.span,
            args: decl
                .parameters
                .iter()
                .map(|p| Field {
                    name: p.ident,
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
