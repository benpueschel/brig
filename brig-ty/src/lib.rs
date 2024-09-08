use std::{
    collections::HashMap,
    sync::{LazyLock, Mutex},
};

pub mod resolve;

// TODO: This is a temporary solution to store type declarations.
// Make all this global, and pass around references to the actual types once they are resolved.
// That also solves the endless cloning of every tiny little thing in the compiler.
static DECLS: LazyLock<Mutex<HashMap<String, Ty>>> = LazyLock::new(|| Mutex::new(HashMap::new()));

pub fn add_ty(name: &str, ty: Ty) {
    DECLS
        .lock()
        .expect("could not unlock decls")
        .insert(name.to_string(), ty);
}

pub fn get_ty(name: &str) -> Option<Ty> {
    DECLS
        .lock()
        .expect("could not unlock decls")
        .get(name)
        .cloned()
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ty {
    pub kind: TyKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyKind {
    Int(IntTy),
    Uint(UintTy),
    Unit,
    UserDefined(UserDefinedTy),
    Function(FnTy),
    Unresolved,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UintTy {
    Usize,
    U32,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntTy {}

#[derive(Debug, Clone, PartialEq)]
pub struct FnTy {
    pub name: String,
    pub args: Vec<Ty>,
    pub ret: Box<Ty>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct UserDefinedTy {
    pub name: String,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: String,
    pub ty: Ty,
}
