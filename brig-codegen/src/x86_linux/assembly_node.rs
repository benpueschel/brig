use std::fmt::Debug;

use brig_common::sym::Symbol;

use super::scratch::{self, ScratchRegisters};

#[derive(Debug, Clone, PartialEq)]
pub enum Instruction {
    AssemblyDirective,
    LabelDeclaration,
    Mov,
    Cmp,
    Add,
    Sub,
    IMul,
    IDiv,
    And,
    Or,
    Xor,
    Set(JumpCondition),
    Push,
    Pop,
    Ret,
    Call,
    Jmp(JumpCondition),
}

#[derive(Debug, Clone, PartialEq)]
pub enum JumpCondition {
    Less,
    Greater,
    LessOrEqual,
    GreaterOrEqual,
    Equal,
    NotEqual,
    None,
}

#[derive(Clone, PartialEq)]
pub enum Expression {
    IntegerLiteral(usize),
    Register(scratch::Register),
    Memory(Symbol),
    Label(Symbol),
    None,
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::IntegerLiteral(x) => write!(f, "IntegerLiteral({})", x),
            Expression::Register(reg) => write!(
                f,
                "Register({}: {})",
                reg,
                ScratchRegisters::get_name(*reg, 8)
            ),
            Expression::Memory(x) => write!(f, "Memory({})", x),
            Expression::Label(x) => write!(f, "Label({})", x),
            Expression::None => write!(f, "None"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssemblyNode {
    pub instruction: Instruction,
    pub left: Expression,
    pub right: Expression,
    pub size: usize,
}

impl AssemblyNode {
    pub fn clone_result(&self) -> Expression {
        self.right.clone()
    }
}
