use std::collections::HashMap;

use lazy_static::lazy_static;

lazy_static! {
    static ref DECLS: HashMap<String, Ty> = HashMap::new();
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ty {
    pub kind: TyKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyKind {
    Int(IntTy),
    Uint(UIntTy),
    Unit,
    UserDefined(UserDefinedTy),
    Unresolved,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UIntTy {
    Usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntTy {}

#[derive(Debug, Clone, PartialEq)]
pub struct UserDefinedTy {
    pub name: String,
}
