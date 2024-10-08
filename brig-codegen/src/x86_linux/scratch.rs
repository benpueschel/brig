use std::collections::HashMap;

use brig_ir::{
    BasicBlock, Ir, Lvalue, Operand, OperandKind, Rvalue, StatementKind, TempVal, TerminatorKind,
    Var, IR_START_BLOCK,
};

use super::FN_CALL_REGISTERS;

pub const REGISTER_SIZE: Register = 7;
pub const ALL_REGISTERS_ALLOCATED: Register = 2 * (1 << (REGISTER_SIZE - 1)) - 1;

#[allow(clippy::identity_op)]
pub const RAX: Register = 1 << (REGISTER_SIZE + 0);
pub const RCX: Register = 1 << (REGISTER_SIZE + 1);
pub const RDX: Register = 1 << (REGISTER_SIZE + 2);
pub const RDI: Register = 1 << (REGISTER_SIZE + 3);
pub const RSI: Register = 1 << (REGISTER_SIZE + 4);
pub const R8: Register = 1 << (REGISTER_SIZE + 5);
pub const R9: Register = 1 << (REGISTER_SIZE + 6);
pub const RBP: Register = 1 << (REGISTER_SIZE + 7);
pub const RSP: Register = 1 << (REGISTER_SIZE + 8);

pub type Register = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct RegisterNode {
    pub value: u64,
    pub size: usize,
}

impl RegisterNode {
    pub fn new(value: u64, size: usize) -> Self {
        RegisterNode { value, size }
    }
}
impl From<Var> for RegisterNode {
    fn from(var: Var) -> Self {
        RegisterNode::new(var.id, var.ty.get().size())
    }
}
impl From<TempVal> for RegisterNode {
    fn from(temp: TempVal) -> Self {
        RegisterNode::new(temp.value(), temp.size())
    }
}

pub struct RegisterGraph {
    nodes: HashMap<RegisterNode, RegisterNodeData>,
    scratch: ScratchRegisters,
    pub stack_offset: i64,
}

pub struct RegisterNodeData {
    pub edges: Vec<RegisterNode>,
    pub location: ScratchLocation,
}

pub enum ScratchLocation {
    Register(Register),
    Stack(i64),
    /// This should not be used outside of the register allocator.
    /// It indicates a node that has not been processed yet. If you encounter this outside of the
    /// register allocator, it is a bug.
    Unassigned,
}

impl Default for RegisterGraph {
    fn default() -> Self {
        RegisterGraph {
            nodes: HashMap::new(),
            scratch: ScratchRegisters::new(),
            stack_offset: 0,
        }
    }
}

impl RegisterGraph {
    #[allow(unused)]
    pub fn new() -> Self {
        RegisterGraph::default()
    }
    pub fn build_graph(&mut self, ir: &Ir) {
        let mut stack_offset = 16; // When a function gets called, the return address is pushed to
                                   // the stack, so we start at 16. (first variable is from 9 to 16)
        for (i, param) in ir.fn_params.iter().enumerate() {
            let var = ir.find_declaration(*param).var.clone();
            if i < FN_CALL_REGISTERS.len() {
                self.process_lvalue(&Lvalue::Variable(var));
            } else {
                self.nodes.insert(
                    var.into(),
                    RegisterNodeData {
                        edges: Vec::new(),
                        location: ScratchLocation::Stack(stack_offset),
                    },
                );
                stack_offset += 8; // NOTE: I think that parameters pushed to the stack are always 8
                                   // bytes long. Idk tho
            }
        }
        self.traverse_graph(ir, IR_START_BLOCK);
    }

    fn traverse_graph(&mut self, ir: &Ir, block: BasicBlock) {
        let block = ir.basic_block_data(block);
        for i in 0..block.statements.len() {
            let stmt = &block.statements[i];
            match &stmt.kind {
                StatementKind::FunctionCall(call) => {
                    for arg in &call.args {
                        self.process_operand(arg);
                    }
                }
                StatementKind::Assign(lhs, operand) => {
                    self.process_lvalue(lhs);
                    self.process_operand(operand);
                }
                StatementKind::Modify(lhs, _, operand) => {
                    self.process_lvalue(lhs);
                    self.process_operand(operand);
                }
            }
        }
        if let Some(terminator) = &block.terminator {
            match &terminator.kind {
                TerminatorKind::Goto { target } => {
                    self.traverse_graph(ir, *target);
                }
                TerminatorKind::If { condition, targets } => {
                    self.process_rvalue(condition);
                    self.traverse_graph(ir, targets.0);
                    self.traverse_graph(ir, targets.1);
                }
                TerminatorKind::Return { expr } => {
                    self.process_operand(expr);
                }
            }
        }
    }

    fn process_lvalue(&mut self, lvalue: &Lvalue) {
        match lvalue {
            Lvalue::Variable(var) => {
                if self.nodes.contains_key(&var.clone().into()) {
                    return;
                }
                self.stack_offset -= var.ty.get().size() as i64;
                self.nodes.insert(
                    var.clone().into(),
                    RegisterNodeData {
                        edges: Vec::new(),
                        location: ScratchLocation::Stack(self.stack_offset),
                    },
                );
            }
            Lvalue::Temp(temp) => {
                let _node = self.get_node_or_insert((*temp).into());
            }
        };
    }

    fn process_rvalue(&mut self, rvalue: &Rvalue) {
        match rvalue {
            Rvalue::Variable(var) => {
                if self.nodes.contains_key(&var.clone().into()) {
                    return;
                }
                self.stack_offset -= var.ty.get().size() as i64;
                self.nodes.insert(
                    var.clone().into(),
                    RegisterNodeData {
                        edges: Vec::new(),
                        location: ScratchLocation::Stack(self.stack_offset),
                    },
                );
            }
            Rvalue::BinaryExpr(_, lhs, rhs) => {
                self.process_operand(lhs);
                self.process_operand(rhs);
            }
            Rvalue::IntegerLit(_, _) => {}
            Rvalue::Unit => {}
            Rvalue::Call(call) => {
                for arg in &call.args {
                    self.process_operand(arg);
                }
            }
            Rvalue::Temp(temp) => {
                let _node = self.get_node_or_insert((*temp).into());
            }
        };
    }

    fn process_operand(&mut self, operand: &Operand) {
        match &operand.kind {
            OperandKind::Consume(lvalue) => self.process_lvalue(lvalue),
            OperandKind::Unit => {}
            OperandKind::IntegerLit(_, _) => {}
            OperandKind::FunctionCall(_) => {}
        }
    }

    fn get_node_or_insert(&mut self, node: RegisterNode) -> &mut RegisterNodeData {
        self.nodes.entry(node).or_insert_with(|| {
            let location = self
                .scratch
                .allocate()
                .map(ScratchLocation::Register)
                .unwrap_or_else(|| {
                    self.stack_offset -= node.size as i64;
                    ScratchLocation::Stack(self.stack_offset)
                });
            RegisterNodeData {
                edges: Vec::new(),
                location,
            }
        })
    }

    pub fn node_data(&self, node: RegisterNode) -> Option<&RegisterNodeData> {
        self.nodes.get(&node)
    }
    pub fn node_data_mut(&mut self, node: RegisterNode) -> Option<&mut RegisterNodeData> {
        self.nodes.get_mut(&node)
    }
}

#[derive(Default)]
pub struct ScratchRegisters {
    registers: Register,
}

impl ScratchRegisters {
    pub fn new() -> Self {
        ScratchRegisters::default()
    }

    pub fn allocate(&mut self) -> Option<Register> {
        if self.registers ^ ALL_REGISTERS_ALLOCATED == 0 {
            return None;
        }
        let mut current_reg = 1 << 0;
        loop {
            if self.registers & current_reg == 0 {
                self.registers |= current_reg;
                return Some(current_reg);
            }
            current_reg <<= 1;
        }
    }

    pub fn free(&mut self, reg: Register) {
        self.registers ^= reg;
    }

    pub fn get_name(reg: Register, size: usize) -> &'static str {
        match size {
            8 => match reg {
                1 => "%rbx",
                2 => "%r10",
                4 => "%r11",
                8 => "%r12",
                16 => "%r13",
                32 => "%r14",
                64 => "%r15",
                RAX => "%rax",
                RCX => "%rcx",
                RDX => "%rdx",
                RDI => "%rdi",
                RSI => "%rsi",
                R8 => "%r8",
                R9 => "%r9",
                RBP => "%rbp",
                RSP => "%rsp",
                x => panic!("{} is not a valid register", x),
            },
            4 => match reg {
                1 => "%ebx",
                2 => "%r10d",
                4 => "%r11d",
                8 => "%r12d",
                16 => "%r13d",
                32 => "%r14d",
                64 => "%r15d",
                RAX => "%eax",
                RCX => "%ecx",
                RDX => "%edx",
                RDI => "%edi",
                RSI => "%esi",
                R8 => "%r8d",
                R9 => "%r9d",
                RBP => "%ebp",
                RSP => "%esp",
                x => panic!("{} is not a valid register", x),
            },
            2 => match reg {
                1 => "%bx",
                2 => "%r10w",
                4 => "%r11w",
                8 => "%r12w",
                16 => "%r13w",
                32 => "%r14w",
                64 => "%r15w",
                RAX => "%ax",
                RCX => "%cx",
                RDX => "%dx",
                RDI => "%di",
                RSI => "%si",
                R8 => "%r8w",
                R9 => "%r9w",
                RBP => "%bp",
                RSP => "%sp",
                x => panic!("{} is not a valid register", x),
            },
            1 => match reg {
                1 => "%bl",
                2 => "%r10l",
                4 => "%r11l",
                8 => "%r12l",
                16 => "%r13l",
                32 => "%r14l",
                64 => "%r15l",
                RAX => "%al",
                RCX => "%cl",
                RDX => "%dl",
                RDI => "%dil",
                RSI => "%sil",
                R8 => "%r8l",
                R9 => "%r9l",
                RBP => "%bpl",
                RSP => "%spl",
                x => panic!("{} is not a valid register", x),
            },
            _ => panic!("register size {} is not supported.", size),
        }
    }
}
