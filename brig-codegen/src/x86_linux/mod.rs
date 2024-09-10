use assembly_node::{AssemblyNode, Expression, Instruction, JumpCondition};
use brig_diagnostic::Result;
use brig_ir::{
    BasicBlock, ExprOperator, Ir, Lvalue, Operand, Statement, StatementKind, TempVal, Var,
    IR_START_BLOCK,
};
use scratch::{Register, RegisterGraph, RegisterNode, ScratchRegisters};

use super::CodeGenerator;

pub mod assembly_node;
pub mod codegen;
pub mod scratch;
pub mod terminator;

type GraphNodeIndex = usize;

static FN_CALL_REGISTERS: [Register; 6] = [
    scratch::RDI,
    scratch::RSI,
    scratch::RDX,
    scratch::RCX,
    scratch::R8,
    scratch::R9,
];

#[derive(Debug, Clone, PartialEq)]
pub struct Graph {
    nodes: Vec<GraphNode>,
    pub start_node: Option<GraphNodeIndex>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct GraphNode {
    nodes: Vec<AssemblyNode>,
    register: Option<Register>,
    branches: Vec<GraphNodeIndex>,
}

pub struct X86Linux {
    fn_params: Vec<u64>,
    registers: ScratchRegisters,
    register_graph: RegisterGraph,
    pub nodes: Vec<AssemblyNode>,
    finished_label: Expression,

    label_counter: usize,
}

impl CodeGenerator for X86Linux {
    fn new() -> Self {
        X86Linux {
            registers: ScratchRegisters::new(),
            nodes: vec![],
            finished_label: Expression::None,
            label_counter: 0,
            register_graph: RegisterGraph::new(),
            fn_params: vec![],
        }
    }
    fn process_graph(&mut self, mut graph: Ir) -> Result<()> {
        let label_counter = self.label_counter;
        *self = Self::new();
        self.label_counter = label_counter;

        if graph.is_extern {
            self.nodes.push(AssemblyNode {
                instruction: Instruction::AssemblyDirective,
                size: 0,
                left: Expression::Label(format!(".globl {}", graph.fn_name)),
                right: Expression::None,
            });
        }

        // if the graph has no basic blocks, it's an external function declaration
        // (e.g. `extern fn foo(): u32;`), so we don't need to generate any code, just
        // declare the function globally
        if graph.basic_blocks.is_empty() {
            return Ok(());
        }

        self.finished_label = self.label_alloc();

        self.register_graph.build_graph(&graph);
        self.fn_params = graph.fn_params.clone();

        self.nodes.push(AssemblyNode {
            instruction: Instruction::LabelDeclaration,
            size: 0,
            left: Expression::Label(graph.fn_name.clone()),
            right: Expression::None,
        });

        self.setup_stack_frame();

        self.process_basic_block(&mut graph, IR_START_BLOCK);

        self.nodes.push(AssemblyNode {
            instruction: Instruction::LabelDeclaration,
            size: 0,
            left: self.finished_label.clone(),
            right: Expression::None,
        });

        self.nodes.push(AssemblyNode {
            instruction: Instruction::Pop,
            size: 8,
            left: Expression::Register(scratch::RBP),
            right: Expression::None,
        });
        self.nodes.push(AssemblyNode {
            instruction: Instruction::Ret,
            size: 8,
            left: Expression::None,
            right: Expression::None,
        });
        Ok(())
    }
}

impl X86Linux {
    fn process_basic_block(&mut self, graph: &mut Ir, block: BasicBlock) {
        let block = graph.basic_block_data_mut(block);
        for statement in &block.statements {
            self.process_statement(statement);
        }
        if block.terminator.is_none() {
            return;
        }
        let terminator = block.terminator.clone().unwrap();
        self.process_terminator(graph, terminator);
    }

    fn process_statement(&mut self, statement: &Statement) {
        match &statement.kind {
            StatementKind::Assign(lhs, rhs) => {
                let right = self.process_lvalue(lhs);
                let left = self.process_operand(rhs);
                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Mov,
                    size: 8, // TODO: dynamic size
                    left,
                    right,
                });
            }
            StatementKind::Modify(lhs, op, rhs) => {
                let right = self.process_lvalue(lhs);
                let left = self.process_operand(rhs);
                let instruction = self.get_instruction(op);
                self.nodes.push(AssemblyNode {
                    instruction,
                    size: 8, // TODO: dynamic size
                    left,
                    right,
                });
            }
        }
    }

    fn process_lvalue(&mut self, lvalue: &Lvalue) -> Expression {
        match lvalue {
            Lvalue::Variable(var) => self.process_variable(var.clone()),
            Lvalue::Temp(temp) => self.process_temp(*temp),
        }
    }

    fn process_variable(&mut self, var: Var) -> Expression {
        for (i, param) in self.fn_params.iter().enumerate() {
            if *param == var.id as u64 {
                if i < FN_CALL_REGISTERS.len() {
                    return Expression::Register(FN_CALL_REGISTERS[i]);
                } else {
                    todo!("bleed into stack");
                }
            }
        }
        self.process_register_node(var.into())
    }

    fn process_temp(&mut self, temp: TempVal) -> Expression {
        self.process_register_node(temp.into())
    }

    fn process_register_node(&mut self, node: RegisterNode) -> Expression {
        let node_data = self.register_graph.node_data(node).unwrap_or_else(|| {
            panic!("Node {:?} not found in register graph", node);
        });
        match node_data.location {
            scratch::ScratchLocation::Register(register) => Expression::Register(register),
            scratch::ScratchLocation::Stack(offset) => Expression::Memory(format!(
                "{}({})",
                offset,
                ScratchRegisters::get_name(scratch::RBP, 8)
            )),
            scratch::ScratchLocation::Unassigned => panic!("Unassigned register"),
        }
    }

    fn process_operand(&mut self, operand: &Operand) -> Expression {
        match operand {
            Operand::Consume(lvalue) => self.process_lvalue(lvalue),
            Operand::IntegerLit(x) => Expression::IntegerLiteral(*x),
            Operand::Unit => Expression::None,
            Operand::FunctionCall(call) => {
                // process a function call based on the system v abi

                for (i, arg) in call.args.iter().enumerate() {
                    let expr = self.process_operand(arg);

                    let dest = if i < FN_CALL_REGISTERS.len() {
                        Expression::Register(FN_CALL_REGISTERS[i])
                    } else {
                        todo!("bleed into stack")
                    };

                    self.nodes.push(AssemblyNode {
                        instruction: Instruction::Mov,
                        size: 8, // TODO: dynamic size
                        left: expr,
                        right: dest,
                    });
                }

                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Call,
                    size: 0,
                    left: Expression::Label(call.name.clone()),
                    right: Expression::None,
                });

                // TODO: get return type and size, check how that changes the abi
                Expression::Register(scratch::RAX)
            }
        }
    }

    fn process_binary_expr(&mut self, op: ExprOperator, lhs: Operand, rhs: Operand) -> Expression {
        match op {
            ExprOperator::Eq => self.process_comparison(JumpCondition::Equal, lhs, rhs),
            ExprOperator::Gt => self.process_comparison(JumpCondition::Greater, lhs, rhs),
            ExprOperator::Gte => self.process_comparison(JumpCondition::GreaterOrEqual, lhs, rhs),
            ExprOperator::Lt => self.process_comparison(JumpCondition::Less, lhs, rhs),
            ExprOperator::Lte => self.process_comparison(JumpCondition::LessOrEqual, lhs, rhs),
            ExprOperator::Add => self.process_instruction(Instruction::Add, lhs, rhs),
            ExprOperator::Sub => self.process_instruction(Instruction::Sub, lhs, rhs),
            ExprOperator::Mul => self.process_instruction(Instruction::IMul, lhs, rhs),
            ExprOperator::Div => self.process_instruction(Instruction::IDiv, lhs, rhs),
        }
    }

    fn process_instruction(
        &mut self,
        instr: Instruction,
        lhs: Operand,
        rhs: Operand,
    ) -> Expression {
        let lhs = self.process_operand(&lhs);
        let rhs = self.process_operand(&rhs);
        self.nodes.push(AssemblyNode {
            instruction: instr,
            size: 8, // TODO: dynamic size
            left: lhs,
            right: rhs.clone(),
        });
        rhs
    }

    fn process_comparison(
        &mut self,
        condition: JumpCondition,
        lhs: Operand,
        rhs: Operand,
    ) -> Expression {
        let lhs = self.process_operand(&lhs);
        let rhs = self.process_operand(&rhs);
        self.nodes.push(AssemblyNode {
            instruction: Instruction::Cmp,
            size: 8, // TODO: dynamic size
            left: lhs,
            right: rhs.clone(),
        });
        self.nodes.push(AssemblyNode {
            instruction: Instruction::Set(condition),
            size: 1,
            left: rhs.clone(),
            right: Expression::None,
        });
        rhs
    }

    fn setup_stack_frame(&mut self) {
        self.nodes.push(AssemblyNode {
            instruction: Instruction::Push,
            size: 8,
            left: Expression::Register(scratch::RBP),
            right: Expression::None,
        });
        self.nodes.push(AssemblyNode {
            instruction: Instruction::Mov,
            size: 8,
            left: Expression::Register(scratch::RSP),
            right: Expression::Register(scratch::RBP),
        });
    }

    fn label_alloc(&mut self) -> Expression {
        let label = format!(".L{}", self.label_counter);
        self.label_counter += 1;
        Expression::Label(label)
    }

    fn get_instruction(&self, op: &ExprOperator) -> Instruction {
        match op {
            ExprOperator::Add => Instruction::Add,
            ExprOperator::Sub => Instruction::Sub,
            ExprOperator::Mul => Instruction::IMul,
            ExprOperator::Div => Instruction::IDiv,
            ExprOperator::Eq => Instruction::Cmp,
            ExprOperator::Gt => Instruction::Cmp,
            ExprOperator::Gte => Instruction::Cmp,
            ExprOperator::Lt => Instruction::Cmp,
            ExprOperator::Lte => Instruction::Cmp,
        }
    }
}
