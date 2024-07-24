use crate::*;

impl Parser {
    pub fn parse_declaration(&mut self) -> Result<Declaration> {
        let modifiers = self.parse_declaration_modifiers()?;
        let token = self.peek()?;
        let kind = match token.kind {
            TokenKind::Fn => DeclarationKind::Function(self.parse_function_declaration()?),
            x => return Err(Error::expected_token(x, vec!["fn".to_string()], token.span)),
        };
        Ok(Declaration { kind, modifiers })
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

    pub fn parse_function_declaration(&mut self) -> Result<FunctionDeclaration> {
        // fn [name] ( <params> ): <return_type> { <body> }
        let start = self.peek()?.span;
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
        }

        // Verify that the next token is a ParenClose
        verify_token!(self.eat()?, TokenKind::ParenClose);

        // Parse return type
        let return_ty = self.parse_type()?;

        // Parse function body
        let body = self.parse_block()?;
        let span = Span::compose(start, body.span);
        Ok(FunctionDeclaration {
            name,
            parameters,
            return_ty,
            body,
            span,
        })
    }
}
