use brig_ast::{Adt, AdtInit, AstNode, StructInit, Ty, TyKind};
use brig_diagnostic::{Error, Result};

use crate::TypeChecker;

impl TypeChecker {
    pub fn check_adt_init(&mut self, e: &mut AdtInit, ty: Option<&Ty>) -> Result<Ty> {
        match e {
            AdtInit::Struct(ref mut s) => self.check_struct_init(s, ty),
        }
    }

    pub fn check_struct_init(&mut self, s: &mut StructInit, ty: Option<&Ty>) -> Result<Ty> {
        let s_ty = self.get_symbol(s.name.name).ok_or_else(|| {
            Error::other(format!("Could not find type for '{}'", s.name), s.name.span)
        })?;
        s.ty = Some(s_ty.clone());

        if let Some(ty) = ty {
            if s_ty.kind != ty.kind {
                return Err(Error::type_mismatch(
                    (ty.kind.clone(), ty.span),
                    (s_ty.kind.clone(), s.span),
                ));
            }
        }

        let def = match s_ty.kind {
            TyKind::Adt(Adt::Struct(def)) => {
                s.def = Some(def.clone());
                def
            }
            x => return Err(Error::other(format!("'{}' is not a struct", x), s_ty.span))?,
        };

        for field in &def.fields {
            let init = s
                .fields
                .iter_mut()
                .find(|f| f.name == field.name)
                .ok_or_else(|| {
                    Error::other(format!("Missing field '{}'", field.name), field.span())
                })?;

            let init_ty = self.check_expression(&mut init.expr, Some(&field.ty))?;
            if init_ty.kind != field.ty.kind {
                return Err(Error::type_mismatch(
                    (field.ty.kind.clone(), field.ty.span),
                    (init_ty.kind, init_ty.span),
                ));
            }
        }

        for field in &s.fields {
            if !def.fields.iter().any(|f| f.name == field.name) {
                return Err(Error::other(
                    format!("Unknown field '{}' on struct '{}'", field.name, s.name),
                    field.span(),
                ));
            }
        }

        Ok(Ty {
            kind: TyKind::Adt(Adt::Struct(def)),
            span: s.span,
        })
    }
}
