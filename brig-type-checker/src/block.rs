use brig_ast::{Block, Ty};
use brig_diagnostic::Result;

use crate::TypeChecker;

impl TypeChecker {
    pub fn check_block(&mut self, block: &mut Block, ty: Option<&Ty>) -> Result<()> {
        self.push_scope();
        for stmt in &mut block.statements {
            self.check_statement(stmt, ty)?;
        }
        // TODO: make blocks return values
        // (example: let a = { 42 }; -> a should return 42)
        self.pop_scope();
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use crate::*;
    use brig_ast::*;
    use brig_common::Span;

    #[test]
    fn test_check_block() {
        // { return 42; } -> the block should return a u32
        let mut block = Block {
            statements: vec![Statement::Return(ReturnStatement {
                expr: Expression::Literal(Literal {
                    value: LiteralValue::Int(IntLit { value: 42 }),
                    ty: LiteralType::Unresolved,
                    span: Span::with_len(9, 2),
                }),
                span: Span::with_len(2, 10),
            })],
            span: Span::with_len(0, 13),
        };
        let mut tc = TypeChecker::default();
        tc.check_block(&mut block, None).expect("type check failed");

        assert_eq!(
            block,
            Block {
                statements: vec![Statement::Return(ReturnStatement {
                    expr: Expression::Literal(Literal {
                        value: LiteralValue::Int(IntLit { value: 42 }),
                        ty: LiteralType::Uint(UintType::U32),
                        span: Span::with_len(9, 2),
                    }),
                    span: Span::with_len(2, 10),
                })],
                span: Span::with_len(0, 13),
            }
        );
    }
}
