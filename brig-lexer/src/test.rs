use crate::*;

fn test_lexer(input: &str, expected: Vec<Token>) {
    let mut lexer = Lexer::new(input.to_string());
    let result = lexer.tokenize().expect("failed to tokenize");
    let full_span = Span::compose(
        expected.first().map_or(Span::new(0, 0), |t| t.span),
        expected.last().map_or(Span::new(0, 0), |t| t.span),
    );
    assert_eq!(result, expected);
    assert_eq!(&input[full_span.start..full_span.end], input);
}

#[test]
fn test_whitespaces() {
    let program = "fn  \t\n  main() \t\n  {  \t\n  }";
    test_lexer(
        program,
        vec![
            Token::with_len(TokenKind::Fn, 0, 2),
            Token::with_len(TokenKind::Identifier("main".to_string()), 8, 4),
            Token::with_len(TokenKind::ParenOpen, 12, 1),
            Token::with_len(TokenKind::ParenClose, 13, 1),
            Token::with_len(TokenKind::BraceOpen, 19, 1),
            Token::with_len(TokenKind::BraceClose, 26, 1),
        ],
    );
}

#[test]
fn test_invalid_number() {
    let program = "1abc";
    let mut lexer = Lexer::new(program.to_string());
    let error = lexer.tokenize().expect_err("expected error");
    assert_eq!(error, Error::invalid_number("1abc", Span::with_len(0, 4)));
}

#[test]
fn test_empty_program() {
    let program = "";
    test_lexer(program, vec![]);
}

#[test]
fn test_single_token() {
    let program = "fn";
    test_lexer(program, vec![Token::with_len(TokenKind::Fn, 0, 2)]);
}

#[test]
fn test_unexpected_symbol() {
    let program = "fn main() { let a = 1; return a + 3; } $";
    let mut lexer = Lexer::new(program.to_string());
    let error = lexer.tokenize().expect_err("expected error");
    assert_eq!(error, Error::unexpected_symbol('$', 39));
}

#[test]
fn test_program() {
    let program = "fn main() { let a = 1; return a + 3; }";
    test_lexer(
        program,
        vec![
            Token::with_len(TokenKind::Fn, 0, 2),
            Token::with_len(TokenKind::Identifier("main".to_string()), 3, 4),
            Token::with_len(TokenKind::ParenOpen, 7, 1),
            Token::with_len(TokenKind::ParenClose, 8, 1),
            Token::with_len(TokenKind::BraceOpen, 10, 1),
            Token::with_len(TokenKind::Let, 12, 3),
            Token::with_len(TokenKind::Identifier("a".to_string()), 16, 1),
            Token::with_len(TokenKind::Equal, 18, 1),
            Token::with_len(TokenKind::Integer(1), 20, 1),
            Token::with_len(TokenKind::Semicolon, 21, 1),
            Token::with_len(TokenKind::Return, 23, 6),
            Token::with_len(TokenKind::Identifier("a".to_string()), 30, 1),
            Token::with_len(TokenKind::Plus, 32, 1),
            Token::with_len(TokenKind::Integer(3), 34, 1),
            Token::with_len(TokenKind::Semicolon, 35, 1),
            Token::with_len(TokenKind::BraceClose, 37, 1),
        ],
    );
}
