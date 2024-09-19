//! The `brig_type_checker` crate is responsible for type checking the AST produced by the
//! `brig_parser` crate.
//!

use std::collections::HashMap;

use brig_ast::{Adt, Program, Ty, TyKind};
use brig_common::sym::Symbol;
use brig_diagnostic::{Error, Result};

mod adt;
mod block;
mod decl;
mod expr;
mod stmt;

#[derive(Debug)]
pub struct TypeChecker {
    symbols: Vec<HashMap<Symbol, Ty>>,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self {
            // Start with the global scope
            symbols: vec![HashMap::new()],
        }
    }

    pub fn check_program(&mut self, program: &mut Program) -> Result<()> {
        for decl in &mut program.declarations {
            self.check_declaration(decl)?;
        }
        Ok(())
    }

    pub fn push_scope(&mut self) {
        self.symbols.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) -> Option<HashMap<Symbol, Ty>> {
        self.symbols.pop()
    }

    pub fn add_symbol(&mut self, name: Symbol, ty: Ty) {
        let symbols = self.symbols.last_mut().expect("No scope pushed");
        symbols.insert(name, ty);
    }

    pub fn get_symbol(&self, name: Symbol) -> Option<Ty> {
        for scope in self.symbols.iter().rev() {
            let symbol = scope.get(&name).cloned();
            if symbol.is_some() {
                return symbol;
            }
        }
        None
    }

    fn check_ty(&self, ty: &mut Ty) -> Result<()> {
        match &mut ty.kind {
            TyKind::Lit(_) => Ok(()),
            TyKind::Adt(adt) => match adt {
                Adt::Struct(s) => {
                    for field in &mut s.fields {
                        self.check_ty(&mut field.ty)?;
                    }
                    Ok(())
                }
            },
            TyKind::Fn(fn_ty) => {
                for arg in &mut fn_ty.args {
                    self.check_ty(&mut arg.ty)?;
                }
                self.check_ty(&mut fn_ty.ret)?;
                Ok(())
            }
            TyKind::Ident(path) => {
                // TODO: make get_symbol take in a path instead of an interned string
                if let Some(resolved_ty) = self.get_symbol(*path.segments.last().unwrap()) {
                    // don't overwrite the span, we need that for diagnostics
                    ty.kind = resolved_ty.kind;
                    Ok(())
                } else {
                    Err(Error::other(
                        format!("Could not resolve type '{}'", path),
                        ty.span,
                    ))
                }
            }
            TyKind::Unspecified => todo!(),
        }
    }
}

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}
