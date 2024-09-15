use brig_ir::{
    BasicBlock, ExprOperator, Ir, Lvalue, Operand, OperandKind, Rvalue, Terminator, TerminatorKind,
};

use super::{
    assembly_node::{AssemblyNode, Expression, Instruction, JumpCondition},
    scratch, X86Linux,
};

impl X86Linux {
    pub(super) fn process_terminator(&mut self, graph: &mut Ir, terminator: Terminator) {
        match &terminator.kind {
            TerminatorKind::Return { expr } => self.process_return(expr),
            TerminatorKind::Goto { target } => self.process_basic_block(graph, *target),
            TerminatorKind::If { condition, targets } => self.process_if(graph, condition, targets),
        }
    }

    fn process_return(&mut self, expr: &Operand) {
        let (size, operand) = self.process_operand(expr);
        if operand != Expression::None {
            self.nodes.push(AssemblyNode {
                instruction: Instruction::Mov,
                size,
                left: operand,
                right: Expression::Register(scratch::RAX),
            });
        }
        self.nodes.push(AssemblyNode {
            instruction: Instruction::Jmp(JumpCondition::None),
            size: 0,
            left: self.finished_label.clone(),
            right: Expression::None,
        });
    }

    fn process_if(
        &mut self,
        graph: &mut Ir,
        condition: &Rvalue,
        targets: &(BasicBlock, BasicBlock),
    ) {
        let then_label = self.label_alloc();
        let done_label = self.label_alloc();
        match condition {
            Rvalue::Call(_call) => todo!("process call in if condition"),
            Rvalue::Temp(temp) => {
                let (size, operand) = self.process_operand(&Operand {
                    kind: OperandKind::Consume(Lvalue::Temp(*temp)),
                    size: temp.size(),
                });
                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Cmp,
                    size,
                    left: Expression::IntegerLiteral(0),
                    right: operand,
                });
                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Jmp(JumpCondition::Equal),
                    size: 0,
                    left: then_label.clone(),
                    right: Expression::None,
                });
            }
            Rvalue::Variable(var) => {
                let (size, operand) = self.process_operand(&Operand {
                    kind: OperandKind::Consume(Lvalue::Variable(var.clone())),
                    size: var.size,
                });
                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Cmp,
                    size,
                    left: Expression::IntegerLiteral(0),
                    right: operand,
                });
                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Jmp(JumpCondition::Equal),
                    size: 0,
                    left: then_label.clone(),
                    right: Expression::None,
                });
            }
            Rvalue::IntegerLit(lit) => {
                let (size, operand) = self.process_operand(&Operand {
                    kind: OperandKind::IntegerLit(*lit),
                    size: 0,
                });
                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Cmp,
                    size,
                    left: Expression::IntegerLiteral(0),
                    right: operand,
                });
                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Jmp(JumpCondition::Equal),
                    size: 0,
                    left: then_label.clone(),
                    right: Expression::None,
                });
            }
            Rvalue::Unit => panic!("unit rvalue in if condition"),
            Rvalue::BinaryExpr(op, lhs, rhs) => {
                if let Some(condition) = self.get_jump_condition(op) {
                    let (size, left) = self.process_operand(rhs);
                    let (rsize, right) = self.process_operand(lhs);
                    let size = size.max(rsize);

                    let right = match right {
                        Expression::Register(x) => Expression::Register(x),
                        x => {
                            self.nodes.push(AssemblyNode {
                                instruction: Instruction::Mov,
                                size,
                                left: x,
                                right: Expression::Register(scratch::RAX),
                            });
                            Expression::Register(scratch::RAX)
                        }
                    };
                    self.nodes.push(AssemblyNode {
                        instruction: Instruction::Cmp,
                        size,
                        left,
                        right,
                    });
                    self.nodes.push(AssemblyNode {
                        instruction: Instruction::Jmp(condition),
                        size: 0,
                        left: then_label.clone(),
                        right: Expression::None,
                    });
                } else {
                    let (size, expr) =
                        self.process_binary_expr(op.clone(), rhs.clone(), lhs.clone());
                    self.nodes.push(AssemblyNode {
                        instruction: Instruction::Cmp,
                        size,
                        left: Expression::IntegerLiteral(0),
                        right: expr,
                    });
                    self.nodes.push(AssemblyNode {
                        instruction: Instruction::Jmp(JumpCondition::Equal),
                        size: 0,
                        left: then_label.clone(),
                        right: Expression::None,
                    });
                }
            }
        }

        self.process_basic_block(graph, targets.1);

        self.nodes.push(AssemblyNode {
            instruction: Instruction::Jmp(JumpCondition::None),
            size: 0,
            left: done_label.clone(),
            right: Expression::None,
        });
        self.nodes.push(AssemblyNode {
            instruction: Instruction::LabelDeclaration,
            size: 0,
            left: then_label,
            right: Expression::None,
        });

        self.process_basic_block(graph, targets.0);

        self.nodes.push(AssemblyNode {
            instruction: Instruction::LabelDeclaration,
            size: 0,
            left: done_label,
            right: Expression::None,
        });
    }

    fn get_jump_condition(&self, operator: &ExprOperator) -> Option<JumpCondition> {
        match operator {
            ExprOperator::Eq => Some(JumpCondition::Equal),
            ExprOperator::Gt => Some(JumpCondition::Greater),
            ExprOperator::Gte => Some(JumpCondition::GreaterOrEqual),
            ExprOperator::Lt => Some(JumpCondition::Less),
            ExprOperator::Lte => Some(JumpCondition::LessOrEqual),
            _ => None,
        }
    }
}
