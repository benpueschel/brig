use brig_ast::Program;
use brig_diagnostic::{Error, Result};

use crate::{get_ty, FnTy, Ty, TyKind, UintTy};

pub fn populate_global_decls(program: &Program) -> Result<()> {
    for decl in &program.declarations {
        match &decl.kind {
            brig_ast::DeclarationKind::Function(func) => {
                let mut args = Vec::with_capacity(func.parameters.len());
                for param in &func.parameters {
                    let ty = parse_ast_ty(&param.ty)?;
                    args.push(ty);
                }

                let ret = parse_ast_ty(&func.return_ty)?;
                let kind = TyKind::Function(FnTy {
                    name: func.name.name.clone(),
                    args,
                    ret: Box::new(ret),
                });
                let ty = Ty { kind };
                crate::add_ty(&func.name.name, ty);
            }
        }
    }
    Ok(())
}

/// Parse a [brig_ast::Ty] into a [brig_ty::Ty](crate::Ty).
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
pub fn parse_ast_ty(ty: &brig_ast::Ty) -> Result<Ty> {
    let kind = match &ty.kind {
        brig_ast::TyKind::Literal(lit) => match lit {
            brig_ast::LiteralType::Uint(uint) => match uint {
                brig_ast::UintType::U32 => TyKind::Uint(UintTy::U32),
                brig_ast::UintType::Usize => TyKind::Uint(UintTy::Usize),
            },
            brig_ast::LiteralType::Unit => TyKind::Unit,
            brig_ast::LiteralType::Unresolved => {
                return Err(Error::other("unresolved literal type", ty.span))
            }
        },
        brig_ast::TyKind::UserDefined(ident) => {
            let name = &ident.name;
            match get_ty(name) {
                Some(ty) => ty.kind,
                None => return Err(Error::other(format!("unknown type: {name}"), ident.span)),
            }
        }
        brig_ast::TyKind::Unspecified => todo!("Unspecified type"),
    };

    Ok(Ty { kind })
}