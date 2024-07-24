use crate::*;

impl Parser {
    pub fn parse_statement(&mut self) -> Result<Statement> {
        let token = self.peek()?;
        match token.kind {
            // TODO: return statement
            // TODO: if statement
            TokenKind::Let => Ok(Statement::VariableDeclaration(
                self.parse_variable_declaration()?,
            )),
            _ => Ok(Statement::Expression(self.parse_expression()?)),
        }
    }

    pub fn parse_variable_declaration(&mut self) -> Result<VariableDeclaration> {
        // let [name] <: [type]> = [expr];
        let token = self.eat()?;
        let start = token.span;

        verify_token!(token, TokenKind::Let);
        let name = ident_from_token(self.eat()?)?;
        let ty = self.parse_type()?;
        let token = self.eat()?;
        if let TokenKind::Semicolon = token.kind {
            return Ok(VariableDeclaration {
                name,
                ty,
                expr: None,
                span: Span::compose(start, token.span),
            });
        }

        let expr = self.parse_expression()?;
        let span = Span::compose(start, expr.span());
        Ok(VariableDeclaration {
            name,
            ty,
            expr: Some(expr),
            span,
        })
    }

}
