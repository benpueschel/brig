use brig_common::Span;
use brig_diagnostic::{Error, Result};
use thin_vec::ThinVec;

use crate::{FnTy, Ty, TyIdx, TyKind, UintTy};

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
        brig_ast::TyKind::Adt(_) => todo!("Adt type"),
        brig_ast::TyKind::Fn(fn_ty) => TyKind::Fn(FnTy {
            name: fn_ty.name,
            args: fn_ty
                .args
                .iter()
                .map(|x| parse_ast_ty(&x.ty))
                .collect::<Result<ThinVec<_>>>()?,
            ret: parse_ast_ty(&fn_ty.ret)?,
        }),
        brig_ast::TyKind::Ident(ident) => return Err(Error::other("unresolved type", ident.span)),
        brig_ast::TyKind::Unspecified => return Err(Error::other("unspecified type", ty.span)),
    };
    Ok(crate::add_ty(Ty { kind }))
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
