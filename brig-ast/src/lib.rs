use std::fmt::{self, Display};

use brig_common::{sym::Symbol, Path, Span};
use thin_vec::ThinVec;

pub trait AstNode {
    fn span(&self) -> Span;
}

/// The main entry point for the parser.
/// A program is a list of top-level declarations in a file.
#[derive(Default, Debug, Clone, PartialEq)]
pub struct Program {
    pub declarations: ThinVec<Decl>,
    pub span: Span,
}

impl AstNode for Program {
    fn span(&self) -> Span {
        self.span
    }
}

/// A top-level declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct Decl {
    /// The kind of declaration.
    pub kind: DeclKind,
}

impl AstNode for Decl {
    fn span(&self) -> Span {
        self.kind.span()
    }
}

/// A top-level declaration kind.
#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Fn(FnDecl),
    Struct(StructDecl),
}

impl AstNode for DeclKind {
    fn span(&self) -> Span {
        match self {
            DeclKind::Fn(f) => f.span,
            DeclKind::Struct(s) => s.span,
        }
    }
}

/// A function declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct FnDecl {
    /// The name of the function.
    pub name: Ident,
    /// The modifiers of the declaration (e.g. `extern`).
    pub modifiers: ThinVec<DeclMod>,
    /// The parameters of the function.
    pub parameters: Punctuated<Param>,
    /// The return type of the function.
    pub return_ty: Ty,
    /// The body of the function.
    /// If this is `None`, the function is extern.
    pub body: Option<Block>,
    /// The span of the function declaration.
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDecl {
    pub name: Ident,
    pub fields: ThinVec<Field>,
    pub span: Span,
}

/// A list of punctuated elements.
#[derive(Debug, Clone, PartialEq)]
pub struct Punctuated<T> {
    /// The elements of the punctuated list.
    pub elements: ThinVec<T>,
    /// The span of the punctuated list.
    pub span: Span,
}

impl<T> Punctuated<T> {
    pub fn new(elements: ThinVec<T>, span: Span) -> Self {
        Self { elements, span }
    }
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.elements.iter()
    }
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut T> {
        self.elements.iter_mut()
    }
    pub fn len(&self) -> usize {
        self.elements.len()
    }
    pub fn is_empty(&self) -> bool {
        self.elements.is_empty()
    }
}

impl<'a, T> IntoIterator for &'a Punctuated<T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.elements.iter()
    }
}

impl<T> IntoIterator for Punctuated<T> {
    type Item = T;
    type IntoIter = thin_vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.elements.into_iter()
    }
}

impl AstNode for FnDecl {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclMod {
    /// The kind of function modifier.
    pub kind: DeclModKind,
    /// The span of the function modifier.
    pub span: Span,
}

impl AstNode for DeclMod {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclModKind {
    /// An `extern` function. This makes the function globally available to the linker.
    /// Extern functions are used to interface with other languages or libraries.
    Extern,
}

/// A block of statements.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    /// The statements in the block.
    pub stmts: ThinVec<Stmt>,
    /// The span of the block.
    pub span: Span,
}

impl AstNode for Block {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Expr(Expr),
    Semi(Expr),
    LetDecl(LetDecl),
    Return(ReturnStmt),
    None,
}

impl AstNode for Stmt {
    fn span(&self) -> Span {
        match self {
            Stmt::Expr(e) => e.span(),
            Stmt::Semi(e) => e.span(),
            Stmt::LetDecl(v) => v.span(),
            Stmt::Return(e) => e.span(),
            Stmt::None => Span::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStmt {
    /// The expression to return.
    pub expr: Expr,
    /// The span of the return statement.
    pub span: Span,
}

impl AstNode for ReturnStmt {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LetDecl {
    /// The name of the variable.
    pub name: Ident,
    /// The type of the variable.
    pub ty: Ty,
    /// The expression that initializes the variable.
    pub expr: Option<Expr>,
    /// The span of the variable declaration.
    pub span: Span,
}

impl AstNode for LetDecl {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Call(CallExpr),
    Lit(Lit),
    Bin(BinExpr),
    Assign(Box<Expr>, Box<Expr>),
    Ident(Ident),
    Block(Block),
    If(IfExpr),
}

impl AstNode for Expr {
    fn span(&self) -> Span {
        match self {
            Expr::Lit(l) => l.span,
            Expr::Bin(b) => b.span,
            Expr::Assign(lhs, rhs) => Span::compose(lhs.span(), rhs.span()),
            Expr::Ident(v) => v.span,
            Expr::Call(c) => c.span,
            Expr::Block(b) => b.span,
            Expr::If(i) => i.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IfExpr {
    pub cond: Box<Expr>,
    pub then_block: Block,
    // TODO: else if
    pub else_block: Option<Block>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpr {
    pub callee: Ident,
    pub args: Punctuated<Expr>,
    pub fn_ty: Option<FnTy>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinExpr {
    pub lhs: Box<Expr>,
    pub rhs: Box<Expr>,
    pub op: BinOp,
    pub span: Span,
    pub ty_kind: Option<TyKind>,
}

impl AstNode for BinExpr {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Gt,
    Lte,
    Gte,
}

impl BinOp {
    /// Whether the binary operator is a comparison operator.
    /// Comparision operators are `==`, `<`, `>`, `<=`, and `>=`.
    pub fn is_comparison(&self) -> bool {
        matches!(
            self,
            BinOp::Eq | BinOp::Lt | BinOp::Gt | BinOp::Lte | BinOp::Gte | BinOp::Ne
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Lit {
    /// The value of the literal.
    pub value: LitVal,
    /// The type of the literal.
    pub ty: LitTy,
    /// The span of the literal.
    pub span: Span,
}

impl AstNode for Lit {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LitVal {
    Int(IntLit),
    Bool(bool),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntLit {
    /// The value of the integer literal.
    pub value: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Param {
    /// The name of the parameter.
    pub ident: Ident,
    /// The type of the parameter.
    pub ty: Ty,
    /// The span of the parameter.
    pub span: Span,
}

impl AstNode for Param {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    /// The name of the identifier.
    pub name: Symbol,
    /// The span of the identifier.
    pub span: Span,
}

impl Display for Ident {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self.name.as_str())
    }
}

impl AstNode for Ident {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ty {
    pub kind: TyKind,
    pub span: Span,
}

impl Ty {
    pub fn size(&self) -> usize {
        match &self.kind {
            TyKind::Lit(l) => l.size(),
            TyKind::Fn(f) => f.ret.size(),
            TyKind::Adt(_) => usize::MAX,
            TyKind::Ident(_) => usize::MAX,
            TyKind::Unspecified => usize::MAX,
        }
    }
}

impl AstNode for Ty {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyKind {
    /// A literal type, like a u32.
    Lit(LitTy),
    /// An algebraic data type.
    Adt(Adt),
    /// A function type.
    Fn(FnTy),
    Ident(Path),
    /// A type the user didn't specify.
    Unspecified,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Adt {
    Struct(Struct),
}

impl AstNode for Adt {
    fn span(&self) -> Span {
        match self {
            Adt::Struct(s) => s.fields[0].ty.span(),
        }
    }
}

impl Display for Adt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Adt::Struct(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Struct {
    pub name: Symbol,
    pub fields: ThinVec<Field>,
}

impl Display for Struct {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self.name.as_str())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnTy {
    pub name: Symbol,
    pub args: ThinVec<Field>,
    pub ret: Box<Ty>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Ident,
    pub ty: Ty,
}

impl Display for Field {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.ty.kind)
    }
}

impl Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyKind::Fn(func) => {
                write!(f, "fn {}(", *func.name.as_str())?;
                for (i, arg) in func.args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, "): {}", func.ret.kind)
            }
            TyKind::Lit(l) => write!(f, "{}", l),
            TyKind::Adt(path) => write!(f, "{}", path),
            TyKind::Ident(path) => write!(f, "{}", path),
            TyKind::Unspecified => write!(f, "unspecified"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LitTy {
    /// Various unsigned integer types, like `u32`.
    Uint(UintTy),
    /// The unit type. Equivalent to `void` in C-like languages.
    Unit,
    /// The boolean type.
    Bool,
    /// A type that has not been resolved yet.
    Unresolved,
}

impl LitTy {
    pub fn size(&self) -> usize {
        match self {
            LitTy::Uint(uint) => uint.size(),
            LitTy::Bool => 1,
            LitTy::Unit => 0,
            LitTy::Unresolved => usize::MAX,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UintTy {
    /// A 32-bit unsigned integer.
    U32,
    /// An unsigned integer the size of a pointer for the target architecture.
    Usize,
}

impl UintTy {
    pub fn size(&self) -> usize {
        match self {
            UintTy::U32 => 4,
            UintTy::Usize => std::mem::size_of::<usize>(),
        }
    }
}

impl Display for UintTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UintTy::U32 => write!(f, "u32"),
            UintTy::Usize => write!(f, "usize"),
        }
    }
}

impl Display for LitTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LitTy::Uint(uint) => write!(f, "{}", uint),
            LitTy::Bool => write!(f, "bool"),
            LitTy::Unit => write!(f, "()"),
            LitTy::Unresolved => write!(f, "unresolved"),
        }
    }
}
