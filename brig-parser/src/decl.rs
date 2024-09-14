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

        let parameters = self.parse_punctuated_list(TokenKind::Comma, |parser: &mut Self| {
            let ident = ident_from_token(parser.eat()?)?;

            // function parameters must always have a type
            let ty = parser.parse_type()?;
            if ty.kind == TyKind::Unspecified {
                return Err(Error::expected_type(ident.span));
            }

            let span = Span::compose(ident.span, ty.span);
            Ok(Parameter { ident, ty, span })
        })?;

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
    use brig_common::sym::Symbol;

    use crate::*;

    #[test]
    pub fn parse_extern_function() {
        let input = "extern fn test(a: usize, b: u32): u32;";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let declaration = parser
            .parse_declaration()
            .expect("Failed to parse declaration");

        assert_eq!(declaration.span(), Span::new(0, 38));
        assert_eq!(
            declaration,
            Declaration {
                kind: DeclarationKind::Function(FunctionDeclaration {
                    name: Identifier {
                        name: Symbol::intern("test"),
                        span: Span::new(10, 14),
                    },
                    parameters: Punctuated {
                        elements: vec![
                            Parameter {
                                ident: Identifier {
                                    name: Symbol::intern("a"),
                                    span: Span::new(15, 16),
                                },
                                ty: Ty {
                                    kind: TyKind::Literal(LiteralType::Uint(UintType::Usize)),
                                    span: Span::new(18, 23),
                                    size: 8,
                                },
                                span: Span::new(15, 23),
                            },
                            Parameter {
                                ident: Identifier {
                                    name: Symbol::intern("b"),
                                    span: Span::new(25, 26),
                                },
                                ty: Ty {
                                    kind: TyKind::Literal(LiteralType::Uint(UintType::U32)),
                                    span: Span::new(28, 31),
                                    size: 4,
                                },
                                span: Span::new(25, 31),
                            }
                        ],
                        span: Span::new(15, 26),
                    },
                    return_ty: Ty {
                        kind: TyKind::Literal(LiteralType::Uint(UintType::U32)),
                        span: Span::new(34, 37),
                        size: 4,
                    },
                    body: None,
                    span: Span::new(0, 38),
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

        assert_eq!(function.span(), Span::new(0, 33));
        assert_eq!(
            function,
            FunctionDeclaration {
                name: Identifier {
                    name: Symbol::intern("test"),
                    span: Span::new(3, 7)
                },
                modifiers: vec![],
                parameters: Punctuated {
                    elements: vec![],
                    span: Span::new(8, 9)
                },
                return_ty: Ty {
                    kind: TyKind::Unspecified,
                    size: 0,
                    span: Span::new(10, 10)
                },
                body: Some(Block {
                    statements: vec![Statement::VariableDeclaration(VariableDeclaration {
                        name: Identifier {
                            name: Symbol::intern("x"),
                            span: Span::new(28, 29)
                        },
                        ty: Ty {
                            kind: TyKind::Unspecified,
                            size: 0,
                            span: Span::new(30, 30)
                        },
                        expr: Some(Expression::Literal(Literal {
                            value: LiteralValue::Int(IntLit { value: 5 }),
                            ty: LiteralType::Unresolved,
                            span: Span::new(32, 33)
                        })),
                        span: Span::new(24, 33)
                    })],
                    span: Span::new(10, 33)
                }),
                span: Span::new(0, 33)
            }
        );
    }
}
