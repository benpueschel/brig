use ariadne::Source;
use brig_ast::DeclarationKind;
use brig_ir::build::IrBuilder;
use brig_lexer::Lexer;
use brig_parser::Parser;
use brig_type_checker::TypeChecker;

fn main() {
    let input = r#"
fn foo(x: usize): usize {
    x = x - 5;
}

fn main() {
    let x: usize = 10;
    x = 15 + foo(x - 5);
}
"#;

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let mut program = parser
        .parse_program()
        .map_err(|err| report_error(err, input))
        .unwrap();
    let mut tc = TypeChecker::default();
    tc.check_program(&mut program)
        .map_err(|err| report_error(err, input))
        .unwrap();

    for decl in &program.declarations {
        if let DeclarationKind::Function(func) = &decl.kind {
            let ir = IrBuilder::build(func.clone()).expect("Failed to build IR");
            println!("\n\n{}\n\n", func.name.name);
            println!("{}", ir);
        }
    }

    // println!("{:#?}", program);
}

fn report_error(err: brig_diagnostic::Error, input: &str) -> ! {
    err.report()
        .print(Source::from(input))
        .expect("Failed to print");
    std::process::exit(1);
}
