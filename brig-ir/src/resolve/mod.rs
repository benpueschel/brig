//! This module is responsible for resoving types and symbols in the IR

use brig_ast::Ident;

use crate::{
    BasicBlock, Ir, Lvalue, Operand, OperandKind, Rvalue, Scope, Statement, StatementKind,
    TerminatorKind, Var, IR_START_BLOCK,
};

pub fn resolve_symbols(mut ir: Ir) -> Ir {
    resolve_symbols_mut(&mut ir);
    ir
}

pub fn resolve_symbols_mut(ir: &mut Ir) {
    if ir.basic_blocks.is_empty() {
        return;
    }
    resolve_symbols_in_block(ir, IR_START_BLOCK);
}
fn resolve_symbols_in_block(ir: &mut Ir, block: BasicBlock) {
    // NOTE: this is safe because we're not modifying the IR, just the symbol IDs
    let block = unsafe { (*(ir as *mut Ir)).basic_block_data_mut(block) };
    for statement in &mut block.statements {
        resolve_symbols_in_statement(ir, statement, block.scope);
    }
    if let Some(terminator) = &mut block.terminator {
        match &mut terminator.kind {
            TerminatorKind::Goto { target } => resolve_symbols_in_block(ir, *target),
            TerminatorKind::If { condition, targets } => {
                resolve_symbols_in_rvalue(ir, condition, block.scope);
                resolve_symbols_in_block(ir, targets.0);
                resolve_symbols_in_block(ir, targets.1);
            }
            TerminatorKind::Return { expr } => {
                resolve_symbols_in_operand(ir, expr, block.scope);
            }
        }
    }
}
fn resolve_symbols_in_statement(ir: &mut Ir, statement: &mut Statement, scope: Scope) {
    match &mut statement.kind {
        StatementKind::Assign(lvalue, operand) => {
            resolve_symbols_in_lvalue(ir, lvalue, scope);
            resolve_symbols_in_operand(ir, operand, scope);
        }
        StatementKind::Modify(lvalue, _, operand) => {
            resolve_symbols_in_lvalue(ir, lvalue, scope);
            resolve_symbols_in_operand(ir, operand, scope);
        }
    }
}
fn resolve_symbols_in_rvalue(ir: &mut Ir, rvalue: &mut Rvalue, scope: Scope) {
    match rvalue {
        Rvalue::Variable(var) => resolve_symbol(ir, var, scope),
        Rvalue::IntegerLit(_) => {}
        Rvalue::Temp(_) => {}
        Rvalue::BinaryExpr(_, lhs, rhs) => {
            resolve_symbols_in_operand(ir, lhs, scope);
            resolve_symbols_in_operand(ir, rhs, scope);
        }
        Rvalue::Call(call) => {
            for arg in &mut call.args {
                resolve_symbols_in_operand(ir, arg, scope);
            }
        }
        Rvalue::Unit => {}
    }
}
fn resolve_symbols_in_lvalue(ir: &mut Ir, lvalue: &mut Lvalue, scope: Scope) {
    match lvalue {
        Lvalue::Variable(var) => {
            resolve_symbol(ir, var, scope);
        }
        Lvalue::Temp(_) => {}
    }
}
fn resolve_symbols_in_operand(ir: &mut Ir, operand: &mut Operand, scope: Scope) {
    match &mut operand.kind {
        OperandKind::Consume(lvalue) => {
            resolve_symbols_in_lvalue(ir, lvalue, scope);
        }
        OperandKind::Unit => {}
        OperandKind::IntegerLit(_) => {}
        OperandKind::FunctionCall(call) => {
            for arg in &mut call.args {
                resolve_symbols_in_operand(ir, arg, scope);
            }
        }
    }
}

pub(crate) fn make_var_id(scope_index: Scope, var_index: usize) -> u64 {
    (scope_index.0 as u64) << 32 | var_index as u64
}

pub(crate) fn resolve_var(ir: &mut Ir, ident: Ident, scope_index: Scope) -> Option<Var> {
    let scope = ir.scope_data_mut(scope_index);
    let mut var = Var {
        id: 0,
        size: 0,
        ident: ident.clone(),
    };

    for i in 0..scope.var_decls.len() {
        let var_decl = &mut scope.var_decls[i].var;
        if var_decl.ident.name == ident.name {
            var.id = var_decl.id;
            var.size = var_decl.size;
            return Some(var);
        }
    }
    // if we didn't find the symbol in this scope, try the parent scope
    if let Some(parent) = scope.parent {
        return resolve_var(ir, ident, parent);
    }
    None
}

pub(crate) fn resolve_symbol(ir: &mut Ir, var: &mut Var, scope_index: Scope) {
    if let Some(v) = resolve_var(ir, var.ident.clone(), scope_index) {
        *var = v;
        return;
    }
    panic!("Symbol not found: {}", var.ident);
}
