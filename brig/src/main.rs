use ariadne::Source;
use brig_lexer::Lexer;
use brig_parser::Parser;
use brig_type_checker::TypeChecker;

fn main() {
    let input = r#"
fn main() {
    let x: usize = 10;
    x = 15 + 10;
}
"#;

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let mut program = parser
        .parse_program()
        .map_err(|err| report_error(err, input))
        .unwrap();
    TypeChecker::default()
        .check_program(&mut program)
        .map_err(|err| report_error(err, input))
        .unwrap();

    println!("{:#?}", program);
}

fn report_error(err: brig_diagnostic::Error, input: &str) -> ! {
    err.report()
        .print(Source::from(input))
        .expect("Failed to print");
    std::process::exit(1);
}
