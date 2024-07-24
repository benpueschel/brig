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
                span: Span::new(11, 11),
            })),
            span: Span::new(4, 11),
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
                span: Span::new(8, 8),
            },
            body: Block {
                statements: vec![Statement::VariableDeclaration(VariableDeclaration {
                    name: Identifier {
                        name: "x".to_string(),
                        span: Span::new(15, 16),
                    },
                    ty: Ty {
                        kind: TyKind::Unspecified,
                        span: Span::new(18, 18),
                    },
                    expr: Some(Expression::Literal(Literal {
                        value: LiteralValue::U32(5),
                        ty: LiteralType::U32,
                        span: Span::new(21, 21),
                    })),
                    span: Span::new(15, 21),
                })],
                span: Span::new(12, 28),
            },
            span: Span::new(0, 28),
        }
    );
}
