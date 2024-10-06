//! Let me interject for a moment.
//!
//! What you guys are referring to as Linux, is in fact, GNU/Linux, or as I've recently taken to
//! calling it, GNU plus Linux. Linux is not an operating system unto itself, but rather another
//! free component of a fully functioning GNU system made useful by the GNU corelibs, shell
//! utilities and vital system components comprising a full OS as defined by POSIX.
//! Many computer users run a modified version of the GNU system every day, without realizing it.
//! Through a peculiar turn of events, the version of GNU which is widely used today is often
//! called "Linux", and many of its users are not aware that it is basically the GNU system,
//! developed by the GNU Project.
//! There really is a Linux, and these people are using it, but it is just a part of the system
//! they use.
//! Linux is the kernel: the program in the system that allocates the machine's resources to the
//! other programs that you run. The kernel is an essential part of an operating system,
//! but useless by itself; it can only function in the context of a complete operating system.
//!
//! Linux is normally used in combination with the GNU operating system:
//! the whole system is basically GNU with Linux added, or GNU/Linux.
//! All the so-called "Linux" distributions are really distributions of GNU/Linux.
//! Thank you for taking your time to cooperate with with me, your friendly GNU+Linux neighbor,
//!
//! Richard Stallman.
//!
use std::collections::HashMap;

use assembly_node::{AssemblyNode, Expression, Instruction, JumpCondition};
use brig_common::sym::Symbol;
use brig_diagnostic::Result;
use brig_ir::{
    BasicBlock, ExprOperator, FunctionCall, Ir, Lvalue, Operand, OperandKind, Statement,
    StatementKind, TempVal, Var, IR_START_BLOCK,
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
    pub labels: HashMap<BasicBlock, Expression>,
    finished_label: Expression,

    label_counter: usize,
}

impl CodeGenerator for X86Linux {
    fn new() -> Self {
        X86Linux {
            registers: ScratchRegisters::new(),
            nodes: vec![],
            finished_label: Expression::None,
            labels: HashMap::new(),
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
                left: Expression::Label(Symbol::intern(&format!(".globl {}", graph.fn_name))),
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
            left: Expression::Label(graph.fn_name),
            right: Expression::None,
        });

        self.setup_stack_frame();

        let stack_size = self.register_graph.stack_offset.unsigned_abs() as usize;
        // pad stack_size to 16 bytes
        let stack_size = (stack_size + 15) & !15;

        if stack_size > 0 {
            self.nodes.push(AssemblyNode {
                instruction: Instruction::Sub,
                size: 8,
                left: Expression::IntegerLiteral(stack_size),
                right: Expression::Register(scratch::RSP),
            });
        }

        for (i, param) in graph.fn_params.iter().enumerate() {
            let var = graph.find_declaration(*param).var.clone();
            let size = var.ty.lock().size();
            let expr = self.process_variable(var);
            if i < FN_CALL_REGISTERS.len() {
                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Mov,
                    left: Expression::Register(FN_CALL_REGISTERS[i]),
                    right: expr,
                    size,
                });
            }
        }

        self.process_basic_block(&mut graph, IR_START_BLOCK);

        self.nodes.push(AssemblyNode {
            instruction: Instruction::LabelDeclaration,
            size: 0,
            left: self.finished_label.clone(),
            right: Expression::None,
        });

        self.nodes.push(AssemblyNode {
            instruction: Instruction::Add,
            size: 8,
            left: Expression::IntegerLiteral(stack_size),
            right: Expression::Register(scratch::RSP),
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
            StatementKind::FunctionCall(call) => {
                // Call the function, ignore the return value
                self.process_function_call(call);
            }
            StatementKind::Assign(lhs, rhs) => {
                let right = self.process_lvalue(lhs);
                let left = self.process_operand(rhs);
                debug_assert_eq!(lhs.ty(), rhs.ty);
                let ty = rhs.ty;

                let size = { ty.lock().size() };
                if size <= 8 {
                    self.nodes.push(AssemblyNode {
                        instruction: Instruction::Mov,
                        left,
                        right,
                        size,
                    });
                    return;
                }
                let size = size as i64;
                let stack_offset = self.register_graph.stack_offset;

                let mut offset = 0;
                while offset < size {
                    let instr_size = 8.min(size - offset).unsigned_abs() as usize;
                    let left = Expression::StackOffset(stack_offset + offset);
                    let right = Expression::StackOffset(stack_offset + offset);
                    self.nodes.push(AssemblyNode {
                        instruction: Instruction::Mov,
                        left,
                        right,
                        size: instr_size,
                    });
                    // move 8 bytes at a time, or the remaining bytes if less than 8
                    offset += instr_size as i64;
                }
            }
            StatementKind::Modify(lhs, op, rhs) => {
                let right = self.process_lvalue(lhs);
                let left = self.process_operand(rhs);
                debug_assert_eq!(lhs.ty(), rhs.ty);

                let instruction = self.get_instruction(op);
                self.nodes.push(AssemblyNode {
                    instruction,
                    size: rhs.ty.lock().size(),
                    left,
                    right,
                });
            }
        }
    }

    fn process_lvalue(&mut self, lvalue: &Lvalue) -> Expression {
        match lvalue {
            Lvalue::FieldAccess(var, field) => self.process_field_access(var, *field),
            Lvalue::Variable(var) => self.process_variable(var.clone()),
            Lvalue::Temp(temp) => self.process_temp(*temp),
        }
    }

    fn process_variable(&mut self, var: Var) -> Expression {
        self.process_register_node(var.into())
    }

    fn process_temp(&mut self, temp: TempVal) -> Expression {
        self.process_register_node(temp.into())
    }

    fn process_function_call(&mut self, call: &FunctionCall) -> Expression {
        // process a function call based on the system v abi

        let mut stack_offset = 0;
        for (i, arg) in call.args.iter().enumerate().rev() {
            let expr = self.process_operand(arg);

            if i < FN_CALL_REGISTERS.len() {
                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Mov,
                    size: arg.ty.lock().size(),
                    left: expr,
                    right: Expression::Register(FN_CALL_REGISTERS[i]),
                });
            } else {
                self.nodes.push(AssemblyNode {
                    instruction: Instruction::Push,
                    size: arg.ty.lock().size(),
                    left: expr,
                    right: Expression::None,
                });
                stack_offset += 8;
            }
        }
        self.nodes.push(AssemblyNode {
            instruction: Instruction::Call,
            size: 0,
            left: Expression::Label(call.name),
            right: Expression::None,
        });

        if stack_offset > 0 {
            self.nodes.push(AssemblyNode {
                instruction: Instruction::Add,
                size: 8,
                left: Expression::IntegerLiteral(stack_offset),
                right: Expression::Register(scratch::RSP),
            });
        }

        // TODO: get return type and size, check how that changes the abi
        let size = { call.ty.ret.lock().size() };

        match size {
            0..=8 => Expression::Register(scratch::RAX),
            // TODO: 9..=16 => make a struct with two registers
            size => todo!("Unsupported return size: {}", size),
        }
    }

    fn process_register_node(&mut self, node: RegisterNode) -> Expression {
        let node_data = self.register_graph.node_data(node).unwrap_or_else(|| {
            panic!("Node {:?} not found in register graph", node);
        });
        match node_data.location {
            scratch::ScratchLocation::Register(register) => Expression::Register(register),
            scratch::ScratchLocation::Stack(offset) => Expression::StackOffset(offset),
            scratch::ScratchLocation::Unassigned => panic!("Unassigned register"),
        }
    }

    fn process_operand(&mut self, operand: &Operand) -> Expression {
        match &operand.kind {
            OperandKind::Consume(lvalue) => self.process_lvalue(lvalue),
            OperandKind::FunctionCall(call) => self.process_function_call(call),
            OperandKind::IntegerLit(_ty, x) => Expression::IntegerLiteral(*x),
            OperandKind::Unit => Expression::None,
        }
    }

    fn process_field_access(&mut self, var: &Var, field: Symbol) -> Expression {
        let (offset, _field_ty) = var.get_field_properties(field);
        let node = self
            .register_graph
            .node_data(var.clone().into())
            .unwrap_or_else(|| {
                panic!("Node {} not found in register graph", var);
            });
        match node.location {
            scratch::ScratchLocation::Register(reg) => panic!(
                "Field access on variable {}, which is in register {}",
                var, reg
            ),
            scratch::ScratchLocation::Stack(stack_offset) => {
                Expression::StackOffset(stack_offset + offset as i64)
            }
            scratch::ScratchLocation::Unassigned => panic!("Unassigned variable {}", var),
        }
    }

    fn process_binary_expr(
        &mut self,
        op: ExprOperator,
        lhs: Operand,
        rhs: Operand,
    ) -> (usize, Expression) {
        match op {
            ExprOperator::Eq => self.process_comparison(JumpCondition::Equal, lhs, rhs),
            ExprOperator::Ne => self.process_comparison(JumpCondition::NotEqual, lhs, rhs),
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
    ) -> (usize, Expression) {
        let left = self.process_operand(&lhs);
        let right = self.process_operand(&rhs);
        debug_assert_eq!(lhs.ty, rhs.ty);
        let size = lhs.ty.lock().size();

        self.nodes.push(AssemblyNode {
            instruction: instr,
            size,
            left,
            right: right.clone(),
        });
        (size, right)
    }

    fn process_comparison(
        &mut self,
        condition: JumpCondition,
        lhs: Operand,
        rhs: Operand,
    ) -> (usize, Expression) {
        let left = self.process_operand(&lhs);
        let right = self.process_operand(&rhs);
        debug_assert_eq!(lhs.ty, rhs.ty);
        let size = lhs.ty.lock().size();

        self.nodes.push(AssemblyNode {
            instruction: Instruction::Cmp,
            size,
            left,
            right: right.clone(),
        });
        self.nodes.push(AssemblyNode {
            instruction: Instruction::Set(condition),
            size,
            left: right.clone(),
            right: Expression::None,
        });
        (size, right)
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
        let label = Symbol::intern(&format!(".L{}", self.label_counter));
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
            ExprOperator::Ne => Instruction::Cmp,
            ExprOperator::Gt => Instruction::Cmp,
            ExprOperator::Gte => Instruction::Cmp,
            ExprOperator::Lt => Instruction::Cmp,
            ExprOperator::Lte => Instruction::Cmp,
        }
    }
}
