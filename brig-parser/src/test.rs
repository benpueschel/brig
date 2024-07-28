use crate::*;

#[test]
pub fn parse_empty_type() {
    let input = "";
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let ty = parser.parse_type().expect("Failed to parse type");
    assert_eq!(
        ty,
        Ty {
            kind: TyKind::Unspecified,
            span: Span::new(0, 0),
        }
    );
}

#[test]
pub fn parse_u32_type() {
    let input = ": u32";
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let ty = parser.parse_type().expect("Failed to parse type");
    assert_eq!(
        ty,
        Ty {
            kind: TyKind::Literal(LiteralType::U32),
            span: Span::new(2, 5),
        }
    );
}

#[test]
pub fn parse_user_type() {
    let input = ": MyType";
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let ty = parser.parse_type().expect("Failed to parse type");
    assert_eq!(
        ty,
        Ty {
            kind: TyKind::UserDefined(Identifier {
                name: "MyType".to_string(),
                span: Span::new(2, 8),
            }),
            span: Span::new(2, 8),
        }
    );
}

#[test]
pub fn parse_add_expr() {
    let input = "x + 5";
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let expr = parser
        .parse_expression()
        .expect("Failed to parse expression");

    assert_eq!(
        expr,
        Expression::Binary(BinaryExpression {
            lhs: Box::new(Expression::Identifier(Identifier {
                name: "x".to_string(),
                span: Span::new(0, 1),
            })),
            rhs: Box::new(Expression::Literal(Literal {
                value: LiteralValue::U32(5),
                ty: LiteralType::U32,
                span: Span::new(4, 5),
            })),
            op: BinaryOperator::Add,
            span: Span::new(0, 5),
        })
    );
}

#[test]
pub fn parse_mult_expr() {
    let input = "10 * 283";
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let expr = parser
        .parse_expression()
        .expect("Failed to parse expression");

    assert_eq!(
        expr,
        Expression::Binary(BinaryExpression {
            lhs: Box::new(Expression::Literal(Literal {
                value: LiteralValue::U32(10),
                ty: LiteralType::U32,
                span: Span::new(0, 2),
            })),
            rhs: Box::new(Expression::Literal(Literal {
                value: LiteralValue::U32(283),
                ty: LiteralType::U32,
                span: Span::new(5, 8),
            })),
            op: BinaryOperator::Multiply,
            span: Span::new(0, 8),
        })
    );
}

#[test]
pub fn parse_assignment() {
    let input = "let var = 5;";
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let statement = parser.parse_statement().expect("Failed to parse statement");
    assert_eq!(
        statement,
        Statement::VariableDeclaration(VariableDeclaration {
            name: Identifier {
                name: "var".to_string(),
                span: Span::new(4, 7),
            },
            ty: Ty {
                kind: TyKind::Unspecified,
                span: Span::new(8, 8),
            },
            expr: Some(Expression::Literal(Literal {
                value: LiteralValue::U32(5),
                ty: LiteralType::U32,
                span: Span::new(10, 11),
            })),
            span: Span::new(0, 11),
        })
    );
}

#[test]
pub fn parse_function() {
    let input = "fn test() {
        let x = 5;
    }";
    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let function = parser
        .parse_function_declaration()
        .expect("Failed to parse function");

    assert_eq!(function.span(), Span::new(0, 29));
    assert_eq!(
        function,
        FunctionDeclaration {
            name: Identifier {
                name: "test".to_string(),
                span: Span::new(3, 7),
            },
            parameters: vec![],
            return_ty: Ty {
                kind: TyKind::Unspecified,
                span: Span::new(10, 10),
            },
            body: Block {
                statements: vec![Statement::VariableDeclaration(VariableDeclaration {
                    name: Identifier {
                        name: "x".to_string(),
                        span: Span::new(24, 25),
                    },
                    ty: Ty {
                        kind: TyKind::Unspecified,
                        span: Span::new(26, 26),
                    },
                    expr: Some(Expression::Literal(Literal {
                        value: LiteralValue::U32(5),
                        ty: LiteralType::U32,
                        span: Span::new(28, 29),
                    })),
                    span: Span::new(20, 29),
                })],
                span: Span::new(20, 29),
            },
            span: Span::new(0, 29),
        }
    );
}
