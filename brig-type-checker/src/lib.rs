//! The `brig_type_checker` crate is responsible for type checking the AST produced by the
//! `brig_parser` crate.
//!

use std::collections::HashMap;

use brig_ast::{Program, TyKind};
use brig_diagnostic::Result;

mod block;
mod decl;
mod expr;
mod stmt;

#[derive(Debug, Default)]
pub struct TypeChecker {
    symbols: Vec<HashMap<String, TyKind>>,
}

impl TypeChecker {
    pub fn check_program(&mut self, program: &mut Program) -> Result<()> {
        for decl in &mut program.declarations {
            self.check_declaration(decl)?;
        }
        Ok(())
    }

    pub fn push_scope(&mut self) {
        self.symbols.push(HashMap::new());
    }

    pub fn pop_scope(&mut self) -> Option<HashMap<String, TyKind>> {
        self.symbols.pop()
    }

    /// TODO: This is shit.
    pub fn add_symbol(&mut self, name: String, ty: TyKind) {
        let symbols = self.symbols.last_mut().expect("No scope pushed");
        symbols.insert(name, ty);
    }

    /// TODO: This is shit.
    /// We aren't tracking branches - as long as an ident was ever valid, this will just get that ident
    /// even if it isn't valid anymore. Ok for now tho
    pub fn get_symbol(&self, name: &str) -> Option<TyKind> {
        for scope in self.symbols.iter().rev() {
            let symbol = scope.get(name).cloned();
            if symbol.is_some() {
                return symbol;
            }
        }
        None
    }
}
