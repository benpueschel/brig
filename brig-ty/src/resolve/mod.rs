use brig_common::Span;
use brig_diagnostic::{Error, Result};
use thin_vec::ThinVec;

use crate::{Adt, FnTy, Struct, Ty, TyIdx, TyKind, UintTy};

/// Parse a [brig_ast::Ty] into a [brig_ty::Ty](crate::Ty) and return a [`brig_ty::TyIdx`].
///
/// As of now, any user-defined types are assumed to be already declared and resolved.
/// This usually means that user-defined types should only be referenced after they have been
/// declared:
/// ```
/// // This is fine:
/// type Foo = u32;
/// let x: Foo = 42;
///
/// // This is not:
/// let x: Bar = 42;
/// type Bar = u32;
/// ```
pub fn parse_ast_ty(ty: &brig_ast::Ty) -> Result<TyIdx> {
    let kind = match &ty.kind {
        brig_ast::TyKind::Lit(lit) => return parse_lit_ast_ty(lit),
        brig_ast::TyKind::Adt(adt) => return parse_adt_ast_ty(adt),
        brig_ast::TyKind::Fn(fn_ty) => {
            let fn_ty = parse_fn_ast_ty(fn_ty)?;
            TyKind::Fn(fn_ty)
        }
        brig_ast::TyKind::Ident(ident) => return Err(Error::other("unresolved type", ident.span)),
        brig_ast::TyKind::Unspecified => return Err(Error::other("unspecified type", ty.span)),
    };
    Ok(crate::add_ty(Ty { kind }))
}

pub fn parse_adt_ast_ty(adt: &brig_ast::Adt) -> Result<TyIdx> {
    match adt {
        brig_ast::Adt::Struct(s) => parse_struct_ast_ty(s),
    }
}

pub fn parse_struct_ast_ty(s: &brig_ast::Struct) -> Result<TyIdx> {
    let fields = s
        .fields
        .iter()
        .map(|f| {
            let ty = parse_ast_ty(&f.ty)?;
            Ok(crate::Field {
                name: f.name.name,
                ty,
            })
        })
        .collect::<Result<ThinVec<_>>>()?;
    Ok(crate::add_ty(Ty {
        kind: TyKind::Adt(Adt::Struct(Struct {
            name: s.name,
            fields,
        })),
    }))
}

pub fn parse_lit_ast_ty(ty: &brig_ast::LitTy) -> Result<TyIdx> {
    let kind = match ty {
        brig_ast::LitTy::Uint(uint) => match uint {
            brig_ast::UintTy::U32 => TyKind::Uint(UintTy::U32),
            brig_ast::UintTy::Usize => TyKind::Uint(UintTy::Usize),
        },
        brig_ast::LitTy::Bool => TyKind::Bool,
        brig_ast::LitTy::Unit => TyKind::Unit,
        brig_ast::LitTy::Unresolved => {
            return Err(Error::other("unresolved literal type", Span::default()))
        }
    };
    Ok(crate::add_ty(Ty { kind }))
}

pub fn parse_fn_ast_ty(ty: &brig_ast::FnTy) -> Result<FnTy> {
    Ok(crate::FnTy {
        name: ty.name,
        args: ty
            .args
            .iter()
            .map(|x| parse_ast_ty(&x.ty))
            .collect::<Result<ThinVec<_>>>()?,
        ret: parse_ast_ty(&ty.ret)?,
    })
}
