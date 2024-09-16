use std::fmt::Display;

use crate::{
    BasicBlock, ExprOperator, Ir, Lvalue, Operand, OperandKind, Rvalue, Statement, Terminator,
    TerminatorKind, IR_END_BLOCK, IR_START_BLOCK,
};

impl Display for Ir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let params = self
            .fn_params
            .iter()
            .map(|p| {
                let var = &self.find_declaration(*p).var;
                format!("{}: TODO TYPE ({} bytes)", var.ident, var.size)
            })
            .collect::<Vec<_>>();

        writeln!(f, "fn {}({}) {{", *self.fn_name.as_str(), params.join(", "))?;

        self.scopes.iter().enumerate().try_for_each(|(i, s)| {
            let indent = "    ".repeat(i);
            if i != 0 {
                writeln!(f, "{indent}scope {} {{", i)?;
            }
            s.var_decls.iter().try_for_each(|d| {
                writeln!(
                    f,
                    "{indent}    let {}: TODO TYPE ({} bytes)",
                    d.var.ident, d.var.size
                )
            })?;

            s.temp_decls
                .iter()
                .enumerate()
                .try_for_each(|(i, _)| writeln!(f, "{indent}    let t{}: TODO TYPE", i))?;

            Ok(())
        })?;

        self.scopes
            .iter()
            .enumerate()
            .rev()
            .try_for_each(|(i, _)| {
                let indent = "    ".repeat(i);
                if i != 0 {
                    writeln!(f, "{indent}}}")?;
                }
                Ok(())
            })?;

        for (index, block) in self.basic_blocks.iter().enumerate() {
            let bb = BasicBlock(index);

            writeln!(f, "    {bb}(scope {}): {{", block.scope.0)?;
            for stmt in &block.statements {
                writeln!(f, "        {};", stmt)?;
            }

            if let Some(terminator) = &block.terminator {
                writeln!(f, "        {};", terminator)?;
            }

            writeln!(f, "    }}")?;
        }

        writeln!(f, "}}")
    }
}

impl Display for Terminator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for BasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match *self {
            IR_START_BLOCK => write!(f, "START"),
            IR_END_BLOCK => write!(f, "END"),
            _ => write!(f, "bb{}", self.0),
        }
    }
}

impl Display for TerminatorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TerminatorKind::Return { expr } => write!(f, "return {}", expr),
            TerminatorKind::Goto { target } => write!(f, "goto {}", target),
            TerminatorKind::If { condition, targets } => write!(
                f,
                "[if {}: goto {}; else goto {}]",
                condition, targets.0, targets.1,
            ),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            crate::StatementKind::Assign(lhs, rhs) => {
                write!(f, "{} = {}", lhs, rhs)
            }
            crate::StatementKind::Modify(lhs, op, rhs) => {
                write!(f, "{} = {} {} {}", lhs, lhs, op, rhs)
            }
        }
    }
}

impl Display for Rvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Rvalue::IntegerLit(int) => write!(f, "{}", int),
            Rvalue::Unit => write!(f, "()"),
            Rvalue::Variable(var) => write!(f, "{}", var.ident),
            Rvalue::Temp(temp) => write!(f, "t{}", temp.index),
            Rvalue::BinaryExpr(op, lhs, rhs) => write!(f, "{} {} {}", lhs, op, rhs),
            Rvalue::Call(call) => write!(
                f,
                "{}({})",
                *call.name.as_str(),
                call.args
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

impl Display for Lvalue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Lvalue::Variable(var) => write!(f, "{}", var.ident),
            Lvalue::Temp(temp) => write!(f, "t{}", temp.index),
        }
    }
}

impl Display for ExprOperator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprOperator::Add => write!(f, "+"),
            ExprOperator::Sub => write!(f, "-"),
            ExprOperator::Mul => write!(f, "*"),
            ExprOperator::Div => write!(f, "/"),
            ExprOperator::Eq => write!(f, "=="),
            ExprOperator::Gt => write!(f, ">"),
            ExprOperator::Lt => write!(f, "<"),
            ExprOperator::Gte => write!(f, ">="),
            ExprOperator::Lte => write!(f, "<="),
        }
    }
}

impl Display for Operand {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for OperandKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OperandKind::Consume(lvalue) => write!(f, "{}", lvalue),
            OperandKind::IntegerLit(int) => write!(f, "{}", int),
            OperandKind::FunctionCall(call) => write!(
                f,
                "{}({})",
                call.name,
                call.args
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            OperandKind::Unit => write!(f, "()"),
        }
    }
}
