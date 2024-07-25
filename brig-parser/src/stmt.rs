use crate::*;

impl Parser {
    pub fn parse_statement(&mut self) -> Result<Statement> {
        let token = self.peek()?;
        match token.kind {
            // TODO: return statement
            // TODO: if statement
            TokenKind::Let => self.parse_variable_declaration(),
            _ => self.parse_expression_statement(),
        }
    }

    pub fn parse_expression_statement(&mut self) -> Result<Statement> {
        println!("parse_expression_statement");
        let expr = self.parse_expression()?;
        verify_token!(self.eat()?, TokenKind::Semicolon);
        Ok(Statement::Expression(expr))
    }

    pub fn parse_variable_declaration(&mut self) -> Result<Statement> {
        // let [name] <: [type]> = [expr];
        let token = self.eat()?;
        let start = token.span;

        verify_token!(token, TokenKind::Let);
        let name = ident_from_token(self.eat()?)?;
        let ty = self.parse_type()?;
        let token = self.eat()?;
        if let TokenKind::Semicolon = token.kind {
            return Ok(Statement::VariableDeclaration(VariableDeclaration {
                name,
                ty,
                expr: None,
                span: Span::compose(start, token.span),
            }));
        }

        let expr = self.parse_expression()?;
        verify_token!(self.eat()?, TokenKind::Semicolon);
        let span = Span::compose(start, expr.span());
        Ok(Statement::VariableDeclaration(VariableDeclaration {
            name,
            ty,
            expr: Some(expr),
            span,
        }))
    }
}
