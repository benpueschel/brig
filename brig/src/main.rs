use ariadne::Source;
use brig_ast::DeclarationKind;
use brig_codegen::CodeGenerator;
use brig_ir::build::IrBuilder;
use brig_lexer::Lexer;
use brig_parser::Parser;
use brig_type_checker::TypeChecker;
use clap::Parser as _;

pub mod args;

fn main() {
    let args = args::Args::parse();

    if args.input.is_empty() {
        eprintln!("No input files");
        std::process::exit(1);
    }

    // TODO: multiple input files
    let input = std::fs::read_to_string(&args.input[0]).expect("Failed to read file");

    let lexer = Lexer::new(input.to_string());
    let mut parser = Parser::new(lexer);
    let mut program = parser
        .parse_program()
        .map_err(|err| report_error(err, &input))
        .unwrap();
    let mut tc = TypeChecker::default();
    tc.check_program(&mut program)
        .map_err(|err| report_error(err, &input))
        .unwrap();

    let mut codegen;
    if let Some(target) = args.target {
        codegen = match target {
            args::Target::LinuxX86 => brig_codegen::x86_linux::X86Linux::new(),
        };
    } else {
        #[cfg(target_os = "linux")]
        {
            #[cfg(target_arch = "x86_64")]
            {
                codegen = brig_codegen::x86_linux::X86Linux::new();
            }
        }
    }

    let mut output = String::new();

    for decl in program.declarations {
        // TODO: other kinds of declarations
        #[allow(irrefutable_let_patterns)]
        if let DeclarationKind::Function(func) = decl.kind {
            let mut ir = IrBuilder::new(func)
                .build()
                .map_err(|err| report_error(err, &input))
                .unwrap();
            brig_ir::resolve::resolve_symbols_mut(&mut ir);

            if args.print_ir {
                output.push_str(&format!("{}\n", ir));
            } else {
                codegen
                    .process_graph(ir)
                    .map_err(|err| report_error(err, &input))
                    .unwrap();
                let asm = brig_codegen::x86_linux::codegen::generate_code(&codegen.nodes);
                output.push_str(&asm);
                output.push('\n');
            }
        }
    }

    if let Some(output_path) = &args.output {
        std::fs::write(output_path, output).expect("Failed to write file");
    } else {
        println!("{}", output);
    }
}

fn report_error(err: brig_diagnostic::Error, input: &str) -> ! {
    err.report()
        .print(Source::from(input))
        .expect("Failed to print");
    std::process::exit(1);
}
