use std::{
    fmt::Display,
    ops::Deref,
    sync::{LazyLock, Mutex, MutexGuard},
};

use brig_common::sym::Symbol;
use thin_vec::ThinVec;

pub mod resolve;

// TODO: This is a temporary solution to store type declarations.
// Make all this global, and pass around references to the actual types once they are resolved.
// That also solves the endless cloning of every tiny little thing in the compiler.
static TYPES: LazyLock<Mutex<Vec<Ty>>> = LazyLock::new(|| Mutex::new(Vec::new()));

pub fn add_ty(ty: Ty) -> TyIdx {
    let mut types = TYPES.lock().expect("failed to lock global type table");
    if let Some(idx) = types.iter().position(|t| *t == ty) {
        return TyIdx(idx);
    }
    types.push(ty);
    TyIdx(types.len() - 1)
}

pub struct TyHandle<'ctx> {
    idx: TyIdx,
    types: MutexGuard<'ctx, Vec<Ty>>,
}

impl Deref for TyHandle<'_> {
    type Target = Ty;

    fn deref(&self) -> &Self::Target {
        &self.types[self.idx.0]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ty {
    pub kind: TyKind,
}

impl Ty {
    pub fn size(&self) -> usize {
        self.kind.size()
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TyIdx(pub usize);

impl TyIdx {
    /// Get the type from the global type table.
    pub fn get<'ctx>(&self) -> TyHandle<'ctx> {
        let types = TYPES.lock().expect("failed to lock global type table");
        TyHandle { idx: *self, types }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyKind {
    Int(IntTy),
    Uint(UintTy),
    Bool,
    Unit,
    Adt(Adt),
    Fn(FnTy),
    Unresolved,
}

impl Display for TyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TyKind::Int(int) => write!(f, "{}", int),
            TyKind::Uint(uint) => write!(f, "{}", uint),
            TyKind::Bool => write!(f, "bool"),
            TyKind::Unit => write!(f, "()"),
            TyKind::Adt(adt) => write!(f, "{}", adt),
            TyKind::Fn(fn_ty) => write!(f, "{}", fn_ty),
            TyKind::Unresolved => write!(f, "unresolved"),
        }
    }
}

impl TyKind {
    #[inline]
    pub fn size(&self) -> usize {
        match self {
            TyKind::Int(int) => int.size(),
            TyKind::Uint(uint) => uint.size(),
            TyKind::Bool => 1,
            TyKind::Unit => 0,
            TyKind::Adt(adt) => adt.size(),
            TyKind::Fn(fn_ty) => fn_ty.size(),
            TyKind::Unresolved => usize::MAX,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UintTy {
    Usize,
    U32,
}

impl Display for UintTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            UintTy::Usize => write!(f, "usize"),
            UintTy::U32 => write!(f, "u32"),
        }
    }
}

impl UintTy {
    #[inline]
    pub fn size(&self) -> usize {
        match self {
            UintTy::Usize => std::mem::size_of::<usize>(),
            UintTy::U32 => 4,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IntTy {}

impl Display for IntTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "INT")
    }
}

impl IntTy {
    #[inline]
    pub fn size(&self) -> usize {
        usize::MAX
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnTy {
    pub name: Symbol,
    pub args: ThinVec<TyIdx>,
    pub ret: TyIdx,
}

impl Display for FnTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}(", self.name)?;
        for (i, arg) in self.args.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", *arg.get())?;
        }
        write!(f, "): {}", *self.ret.get())
    }
}

impl FnTy {
    #[inline]
    pub fn size(&self) -> usize {
        self.ret.get().size()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Adt {
    Struct(Struct),
}

impl Display for Adt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Adt::Struct(s) => write!(f, "{}", s),
        }
    }
}

impl Adt {
    #[inline]
    pub fn size(&self) -> usize {
        match self {
            Adt::Struct(s) => s.size(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: Symbol,
    pub fields: ThinVec<Field>,
}

impl Display for Struct {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl Struct {
    #[inline]
    pub fn size(&self) -> usize {
        self.fields.iter().map(|field| field.size()).sum()
    }

    #[inline]
    pub fn field_offset(&self, field: &Field) -> usize {
        let mut offset = 0;
        for f in &self.fields {
            if f == field {
                return offset;
            }
            offset += f.size();
        }
        panic!("field not found in struct");
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Symbol,
    pub ty: TyIdx,
}

impl Field {
    #[inline]
    pub fn size(&self) -> usize {
        self.ty.get().size()
    }
}
