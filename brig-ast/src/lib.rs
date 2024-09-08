use std::fmt::{self, Display};

use brig_common::Span;

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
    /// The modifiers of the declaration (e.g. `extern`).
    pub modifiers: Vec<DeclarationModifier>,
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
    /// The parameters of the function.
    pub parameters: Vec<Parameter>,
    /// The return type of the function.
    pub return_ty: Ty,
    /// The body of the function.
    pub body: Block,
    /// The span of the function declaration.
    pub span: Span,
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
    Expression(Expression),
    VariableDeclaration(VariableDeclaration),
}

impl AstNode for Statement {
    fn span(&self) -> Span {
        match self {
            Statement::Expression(e) => e.span(),
            Statement::VariableDeclaration(v) => v.span(),
        }
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
    Literal(Literal),
    Binary(BinaryExpression),
    Identifier(Identifier),
}

impl AstNode for Expression {
    fn span(&self) -> Span {
        match self {
            Expression::Literal(l) => l.span,
            Expression::Binary(b) => b.span,
            Expression::Identifier(v) => v.span,
        }
    }
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
    /// A 32-bit unsigned integer.
    U32(u32),
    /// An unsigned integer the size of a pointer for the target architecture.
    Usize(usize),
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
    pub name: String,
    /// The span of the identifier.
    pub span: Span,
}

impl AstNode for Identifier {
    fn span(&self) -> Span {
        self.span
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ty {
    pub kind: TyKind,
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
    /// A type the user didn't specify.
    Unspecified,
}

impl Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TyKind::Literal(l) => write!(f, "{}", l),
            TyKind::UserDefined(ident) => write!(f, "{}", ident.name),
            TyKind::Unspecified => write!(f, "unspecified"),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LiteralType {
    /// A 32-bit unsigned integer.
    U32,
    /// An unsigned integer the size of a pointer for the target architecture.
    Usize,
    /// The unit type. Equivalent to `void` in C-like languages.
    Unit,
}

impl Display for LiteralType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            LiteralType::U32 => write!(f, "u32"),
            LiteralType::Usize => write!(f, "usize"),
            LiteralType::Unit => write!(f, "()"),
        }
    }
}
