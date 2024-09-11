use crate::*;

impl Parser {
    pub fn parse_declaration(&mut self) -> Result<Declaration> {
        let modifiers = self.parse_declaration_modifiers()?;
        let token = self.peek()?;
        let kind = match token.kind {
            TokenKind::Fn => DeclarationKind::Function(self.parse_function_declaration(modifiers)?),
            x => return Err(Error::expected_token(x, vec!["fn".to_string()], token.span)),
        };
        Ok(Declaration { kind })
    }

    pub fn parse_declaration_modifiers(&mut self) -> Result<Vec<DeclarationModifier>> {
        static MODIFIERS: [TokenKind; 1] = [TokenKind::Extern];
        let mut modifiers = Vec::new();

        while MODIFIERS.contains(&self.peek()?.kind) {
            let Token { kind, span } = self.eat()?;
            let kind = match kind {
                TokenKind::Extern => DeclarationModifierKind::Extern,
                _ => unreachable!("token kind in MODIFIERS is not handled in match statement"),
            };
            modifiers.push(DeclarationModifier { kind, span });
        }
        Ok(modifiers)
    }

    pub fn parse_function_declaration(
        &mut self,
        modifiers: Vec<DeclarationModifier>,
    ) -> Result<FunctionDeclaration> {
        // <modifiers> fn [name] ( <params> ): <return_type> { <body> }
        let start = modifiers
            .first()
            .map(|m| Ok(m.span))
            .unwrap_or_else(|| Ok(self.peek()?.span))?;

        verify_token!(self.eat()?, TokenKind::Fn);

        // Parse function name
        let name = ident_from_token(self.eat()?)?;

        // Verify that the next token is a ParenOpen
        verify_token!(self.eat()?, TokenKind::ParenOpen);

        // Parse parameters
        let mut parameters: Vec<Parameter> = Vec::new();
        while self.peek()?.kind != TokenKind::ParenClose {
            let ident = ident_from_token(self.eat()?)?;

            // function parameters must always have a type
            let ty = self.parse_type()?;
            if ty.kind == TyKind::Unspecified {
                return Err(Error::expected_type(ident.span));
            }

            let span = Span::compose(ident.span, ty.span);
            parameters.push(Parameter { ident, ty, span });

            if self.peek()?.kind == TokenKind::Comma {
                self.eat()?;
            }
        }

        // Verify that the next token is a ParenClose
        verify_token!(self.eat()?, TokenKind::ParenClose);

        // Parse return type
        let return_ty = self.parse_type()?;

        // Parse function body
        let body;
        let span;
        match self.peek()?.kind {
            TokenKind::Semicolon => {
                span = Span::compose(start, self.eat()?.span);
                body = None;
            }
            _ => {
                let block = self.parse_block()?;
                span = Span::compose(start, block.span);
                body = Some(block);
            }
        };
        Ok(FunctionDeclaration {
            name,
            modifiers,
            parameters,
            return_ty,
            body,
            span,
        })
    }
}

#[cfg(test)]
mod test {
    use crate::*;

    #[test]
    pub fn parse_extern_function() {
        let input = "extern fn test(a: usize): u32;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let declaration = parser
            .parse_declaration()
            .expect("Failed to parse declaration");

        assert_eq!(declaration.span(), Span::new(0, 30));
        assert_eq!(
            declaration,
            Declaration {
                kind: DeclarationKind::Function(FunctionDeclaration {
                    name: Identifier {
                        name: "test".to_string(),
                        span: Span::new(10, 14),
                    },
                    parameters: vec![Parameter {
                        ident: Identifier {
                            name: "a".to_string(),
                            span: Span::new(15, 16),
                        },
                        ty: Ty {
                            kind: TyKind::Literal(LiteralType::Uint(UintType::Usize)),
                            span: Span::new(18, 23),
                            size: 8,
                        },
                        span: Span::new(15, 23),
                    }],
                    return_ty: Ty {
                        kind: TyKind::Literal(LiteralType::Uint(UintType::U32)),
                        span: Span::new(26, 29),
                        size: 4,
                    },
                    body: None,
                    span: Span::new(0, 30),
                    modifiers: vec![DeclarationModifier {
                        kind: DeclarationModifierKind::Extern,
                        span: Span::new(0, 6),
                    }],
                }),
            }
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
            .parse_function_declaration(vec![])
            .expect("Failed to parse function");

        assert_eq!(function.span(), Span::new(0, 29));
        assert_eq!(
            function,
            FunctionDeclaration {
                modifiers: vec![],
                name: Identifier {
                    name: "test".to_string(),
                    span: Span::new(3, 7),
                },
                parameters: vec![],
                return_ty: Ty {
                    kind: TyKind::Unspecified,
                    span: Span::new(10, 10),
                    size: 0,
                },
                body: Some(Block {
                    statements: vec![Statement::VariableDeclaration(VariableDeclaration {
                        name: Identifier {
                            name: "x".to_string(),
                            span: Span::new(24, 25),
                        },
                        ty: Ty {
                            kind: TyKind::Unspecified,
                            size: 0,
                            span: Span::new(26, 26),
                        },
                        expr: Some(Expression::Literal(Literal {
                            value: LiteralValue::Int(IntLit { value: 5 }),
                            ty: LiteralType::Unresolved,
                            span: Span::new(28, 29),
                        })),
                        span: Span::new(20, 29),
                    })],
                    span: Span::new(20, 29),
                }),
                span: Span::new(0, 29),
            }
        );
    }
}
