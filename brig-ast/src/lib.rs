use std::fmt::{self, Display};

use brig_common::{sym::Symbol, Span};

pub trait AstNode {
    fn span(&self) -> Span;
}

/// The main entry point for the parser.
/// A program is a list of top-level declarations in a file.
#[derive(Default, Debug, Clone, PartialEq)]
pub struct Program {
    pub declarations: Vec<Declaration>,
    pub span: Span,
}

impl AstNode for Program {
    fn span(&self) -> Span {
        self.span
    }
}

/// A top-level declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    /// The kind of declaration.
    pub kind: DeclarationKind,
}

impl AstNode for Declaration {
    fn span(&self) -> Span {
        self.kind.span()
    }
}

/// A top-level declaration kind.
#[derive(Debug, Clone, PartialEq)]
pub enum DeclarationKind {
    Function(FunctionDeclaration),
}

impl AstNode for DeclarationKind {
    fn span(&self) -> Span {
        match self {
            DeclarationKind::Function(f) => f.span,
        }
    }
}

/// A function declaration.
#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDeclaration {
    /// The name of the function.
    pub name: Identifier,
    /// The modifiers of the declaration (e.g. `extern`).
    pub modifiers: Vec<DeclarationModifier>,
    /// The parameters of the function.
    pub parameters: Punctuated<Parameter>,
    /// The return type of the function.
    pub return_ty: Ty,
    /// The body of the function.
    /// If this is `None`, the function is extern.
    pub body: Option<Block>,
    /// The span of the function declaration.
    pub span: Span,
}

/// A list of punctuated elements.
#[derive(Debug, Clone, PartialEq)]
pub struct Punctuated<T> {
    /// The elements of the punctuated list.
    pub elements: Vec<T>,
    /// The span of the punctuated list.
    pub span: Span,
}

impl<T> Punctuated<T> {
    pub fn new(elements: Vec<T>, span: Span) -> Self {
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
    type IntoIter = std::vec::IntoIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        self.elements.into_iter()
    }
}

impl AstNode for FunctionDeclaration {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct DeclarationModifier {
    /// The kind of function modifier.
    pub kind: DeclarationModifierKind,
    /// The span of the function modifier.
    pub span: Span,
}

impl AstNode for DeclarationModifier {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclarationModifierKind {
    /// An `extern` function. This makes the function globally available to the linker.
    /// Extern functions are used to interface with other languages or libraries.
    Extern,
}

/// A block of statements.
#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    /// The statements in the block.
    pub statements: Vec<Statement>,
    /// The span of the block.
    pub span: Span,
}

impl AstNode for Block {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Expr(Expression),
    Semi(Expression),
    VariableDeclaration(VariableDeclaration),
    Return(ReturnStatement),
    None,
}

impl AstNode for Statement {
    fn span(&self) -> Span {
        match self {
            Statement::Expr(e) => e.span(),
            Statement::Semi(e) => e.span(),
            Statement::VariableDeclaration(v) => v.span(),
            Statement::Return(e) => e.span(),
            Statement::None => Span::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ReturnStatement {
    /// The expression to return.
    pub expr: Expression,
    /// The span of the return statement.
    pub span: Span,
}

impl AstNode for ReturnStatement {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariableDeclaration {
    /// The name of the variable.
    pub name: Identifier,
    /// The type of the variable.
    pub ty: Ty,
    /// The expression that initializes the variable.
    pub expr: Option<Expression>,
    /// The span of the variable declaration.
    pub span: Span,
}

impl AstNode for VariableDeclaration {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Call(CallExpression),
    Literal(Literal),
    Binary(BinaryExpression),
    Identifier(Identifier),
    Block(Block),
}

impl AstNode for Expression {
    fn span(&self) -> Span {
        match self {
            Expression::Literal(l) => l.span,
            Expression::Binary(b) => b.span,
            Expression::Identifier(v) => v.span,
            Expression::Call(c) => c.span,
            Expression::Block(b) => b.span,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CallExpression {
    pub callee: Identifier,
    pub args: Punctuated<Expression>,
    pub fn_ty: Option<FnTy>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryExpression {
    pub lhs: Box<Expression>,
    pub rhs: Box<Expression>,
    pub op: BinaryOperator,
    pub span: Span,
    pub ty_kind: Option<TyKind>,
}

impl AstNode for BinaryExpression {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Assign,
    LessThan,
    GreaterThan,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Literal {
    /// The value of the literal.
    pub value: LiteralValue,
    /// The type of the literal.
    pub ty: LiteralType,
    /// The span of the literal.
    pub span: Span,
}

impl AstNode for Literal {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Int(IntLit),
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntLit {
    /// The value of the integer literal.
    pub value: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    /// The name of the parameter.
    pub ident: Identifier,
    /// The type of the parameter.
    pub ty: Ty,
    /// The span of the parameter.
    pub span: Span,
}

impl AstNode for Parameter {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Identifier {
    /// The name of the identifier.
    pub name: Symbol,
    /// The span of the identifier.
    pub span: Span,
}

impl Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self.name.as_str())
    }
}

impl AstNode for Identifier {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ty {
    pub kind: TyKind,
    pub size: usize,
    pub span: Span,
}

impl AstNode for Ty {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TyKind {
    /// A literal type, like a u32.
    Literal(LiteralType),
    /// A user-defined type.
    UserDefined(Identifier),
    /// A function type.
    Function(FnTy),
    /// A type the user didn't specify.
    Unspecified,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FnTy {
    pub name: Symbol,
    pub args: Vec<Ty>,
    pub ret: Box<Ty>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field {
    pub name: Symbol,
    pub ty: Ty,
}

impl Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyKind::Function(func) => {
                write!(f, "fn {}(", *func.name.as_str())?;
                for (i, arg) in func.args.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg.kind)?;
                }
                write!(f, "): {}", func.ret.kind)
            }
            TyKind::Literal(l) => write!(f, "{}", l),
            TyKind::UserDefined(ident) => write!(f, "{}", *ident.name.as_str()),
            TyKind::Unspecified => write!(f, "unspecified"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LiteralType {
    /// Various unsigned integer types, like `u32`.
    Uint(UintType),
    /// The unit type. Equivalent to `void` in C-like languages.
    Unit,
    /// A type that has not been resolved yet.
    Unresolved,
}

impl LiteralType {
    pub fn size(&self) -> usize {
        match self {
            LiteralType::Uint(UintType::U32) => 4,
            LiteralType::Uint(UintType::Usize) => std::mem::size_of::<usize>(),
            LiteralType::Unit => 0,
            LiteralType::Unresolved => usize::MAX,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UintType {
    /// A 32-bit unsigned integer.
    U32,
    /// An unsigned integer the size of a pointer for the target architecture.
    Usize,
}

impl Display for UintType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UintType::U32 => write!(f, "u32"),
            UintType::Usize => write!(f, "usize"),
        }
    }
}

impl Display for LiteralType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralType::Uint(uint) => write!(f, "{}", uint),
            LiteralType::Unit => write!(f, "()"),
            LiteralType::Unresolved => write!(f, "unresolved"),
        }
    }
}
