//! # brig-ir
//!
//! This crate is responsible for generating the intermediate representation (IR) of a program.
//!
//! # Example
//!
//! Suppose we have the following program:
//! ```orca
//! fn main(): usize {
//!     let x: usize = 1;
//!     let y: usize = 2;
//!     let z: usize = x + y;
//!     print(z);
//!     return z;
//! }
//! ```
//! The resulting IR would be generated on a very desugared version of the program:
//! ```orca
//! fn main(): usize {
//!     let x: usize;
//!     x = 1;
//!     let y: usize;
//!     y = 2;
//!     let z: usize;
//!     z = x + y;
//!     print(z);
//!     return z;
//! ```
//!
//! # Basic Blocks
//!
//! The IR contains A list of [BasicBlocks](BasicBlock) that represent the control flow of the
//! program. A basic block is a sequence of instructions that are executed sequentially, until a
//! branch instruction is found.
//!
//! Every basic block is terminated by a [Terminator]. The terminator denotes the branch type:
//!

use std::ops::{Index, IndexMut};

use brig_ast::{BinOp, Ident};
use brig_common::{sym::Symbol, Span};

pub mod build;
pub mod debug;
pub mod resolve;

pub const IR_START_BLOCK: BasicBlock = BasicBlock(0);
pub const IR_END_BLOCK: BasicBlock = BasicBlock(1);

pub const IR_GLOBAL_SCOPE: Scope = Scope(0);
pub const IR_FN_SCOPE: Scope = Scope(1);

#[derive(Debug, Clone, PartialEq)]
pub struct Ir {
    pub basic_blocks: Vec<BasicBlockData>,
    pub scopes: Vec<ScopeData>,
    pub fn_name: Symbol,
    pub is_extern: bool,
    pub fn_params: Vec<u64>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ScopeData {
    pub span: Span,
    pub parent: Option<Scope>,
    pub var_decls: Vec<VarDecl>,
    pub temp_decls: Vec<TempDecl>,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Scope(usize);
impl Scope {
    pub fn index(&self) -> usize {
        self.0
    }
}
impl Index<Scope> for Vec<ScopeData> {
    type Output = ScopeData;
    fn index(&self, index: Scope) -> &ScopeData {
        &self[index.index()]
    }
}
impl IndexMut<Scope> for Vec<ScopeData> {
    fn index_mut(&mut self, index: Scope) -> &mut ScopeData {
        &mut self[index.index()]
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct BasicBlock(usize);
impl BasicBlock {
    pub fn index(&self) -> usize {
        self.0
    }
}
impl Index<BasicBlock> for Vec<BasicBlockData> {
    type Output = BasicBlockData;
    fn index(&self, index: BasicBlock) -> &BasicBlockData {
        &self[index.index()]
    }
}
impl IndexMut<BasicBlock> for Vec<BasicBlockData> {
    fn index_mut(&mut self, index: BasicBlock) -> &mut BasicBlockData {
        &mut self[index.index()]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BasicBlockData {
    pub statements: Vec<Statement>,
    pub terminator: Option<Terminator>,
    pub scope: Scope,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Terminator {
    pub span: Span,
    pub scope: Scope,
    pub kind: TerminatorKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TerminatorKind {
    Return {
        expr: Operand,
    },
    Goto {
        target: BasicBlock,
    },
    If {
        condition: Rvalue,
        targets: (BasicBlock, BasicBlock),
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Statement {
    pub span: Span,
    pub kind: StatementKind,
}

#[derive(Debug, Clone, PartialEq)]
pub enum StatementKind {
    Assign(Lvalue, Operand),
    Modify(Lvalue, ExprOperator, Operand),
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub name: Symbol,
    pub ty: brig_ast::FnTy,
    pub args: Vec<Operand>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Lvalue {
    Variable(Var),
    Temp(TempVal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Rvalue {
    IntegerLit(usize),
    Variable(Var),
    Temp(TempVal),
    BinaryExpr(ExprOperator, Operand, Operand),
    Call(FunctionCall),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprOperator {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Gt,
    Lt,
    Gte,
    Lte,
}

impl From<BinOp> for ExprOperator {
    fn from(value: BinOp) -> Self {
        match value {
            BinOp::Add => ExprOperator::Add,
            BinOp::Subtract => ExprOperator::Sub,
            BinOp::Multiply => ExprOperator::Mul,
            BinOp::Divide => ExprOperator::Div,
            BinOp::Assign => panic!("assign operator is not a valid expression operator"),
            BinOp::LessThan => ExprOperator::Lt,
            BinOp::GreaterThan => ExprOperator::Gt,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Operand {
    pub kind: OperandKind,
    pub size: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OperandKind {
    Consume(Lvalue),
    IntegerLit(usize),
    FunctionCall(FunctionCall),
    Unit,
}

impl From<OperandKind> for Rvalue {
    fn from(operand: OperandKind) -> Rvalue {
        match operand {
            OperandKind::Consume(lvalue) => lvalue.into(),
            OperandKind::IntegerLit(value) => Rvalue::IntegerLit(value),
            OperandKind::FunctionCall(call) => Rvalue::Call(call),
            OperandKind::Unit => panic!("unit operand is not a valid rvalue"),
        }
    }
}

impl From<Lvalue> for Rvalue {
    fn from(lvalue: Lvalue) -> Rvalue {
        match lvalue {
            Lvalue::Variable(var) => Rvalue::Variable(var),
            Lvalue::Temp(temp) => Rvalue::Temp(temp),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TempVal {
    index: u64,
    size: usize,
}
impl TempVal {
    pub fn value(&self) -> u64 {
        self.index
    }
    pub fn size(&self) -> usize {
        self.size
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TempDecl {
    pub scope: Scope,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Var {
    // TODO: hide behind a debug flag - we exclusively use the id from this point on
    pub ident: Ident,
    pub id: u64,
    pub size: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VarDecl {
    pub var: Var,
    pub scope: Scope,
}

impl Ir {
    pub fn basic_block_data(&self, block: BasicBlock) -> &BasicBlockData {
        &self.basic_blocks[block.index()]
    }
    pub fn basic_block_data_mut(&mut self, block: BasicBlock) -> &mut BasicBlockData {
        &mut self.basic_blocks[block.index()]
    }
    pub fn scope_data(&self, scope: Scope) -> &ScopeData {
        &self.scopes[scope.index()]
    }
    pub fn scope_data_mut(&mut self, scope: Scope) -> &mut ScopeData {
        &mut self.scopes[scope.index()]
    }
    pub fn find_declaration_mut(&mut self, id: u64) -> &mut VarDecl {
        // let id = scope_index.0 << 32 | i;
        let scope_index = id >> 32;
        let var_index = id & 0xFFFF_FFFF;

        let scope = &mut self.scopes[scope_index as usize];
        &mut scope.var_decls[var_index as usize]
    }
    pub fn find_declaration(&self, id: u64) -> &VarDecl {
        // let id = scope_index.0 << 32 | i;
        let scope_index = id >> 32;
        let var_index = id & 0xFFFF_FFFF;

        let scope = &self.scopes[scope_index as usize];
        &scope.var_decls[var_index as usize]
    }
}
