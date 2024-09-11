use ariadne::Source;
use brig_ast::DeclarationKind;
use brig_codegen::CodeGenerator;
use brig_ir::build::IrBuilder;
use brig_lexer::Lexer;
use brig_parser::Parser;
use brig_type_checker::TypeChecker;

fn main() {
    let input = r#"
extern fn bar(x: usize): usize;

extern fn ayo(a: usize, b: usize, c: usize, d: usize, e: usize, f: usize, g: usize, h: usize): usize {
    return a + b + c + d + e + f + g + h;
}

extern fn foo(x: usize): usize {
    return bar(x - 5);
}
"#;

    let _ = r#"
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

    // TODO: ask user for target
    let mut codegen = brig_codegen::x86_linux::X86Linux::new();

    for decl in &program.declarations {
        // TODO: other kinds of declarations
        #[allow(irrefutable_let_patterns)]
        if let DeclarationKind::Function(func) = &decl.kind {
            let mut ir = IrBuilder::build(func.clone())
                .map_err(|err| report_error(err, input))
                .unwrap();
            brig_ir::resolve::resolve_symbols_mut(&mut ir);

            codegen
                .process_graph(ir)
                .map_err(|err| report_error(err, input))
                .unwrap();

            let asm = brig_codegen::x86_linux::codegen::generate_code(&codegen.nodes);
            println!("{}", asm);
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
