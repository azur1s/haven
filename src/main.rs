use std::io::Write;
use clap::Parser;

mod args;
mod ast;
mod parse;
mod intrinsics;
mod typecheck;
mod mil;
mod llvm;

fn offset_to_line_col(src: &str, offset: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;

    // Clamp to end of source so out-of-range offsets are still printable.
    let clamped = offset.min(src.len());

    for (i, ch) in src.char_indices() {
        if i >= clamped {
            break;
        }

        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    (line, col)
}

const RUNTIME_ARCHIVE: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/libruntime.a"));

fn main() {
    let args = args::Args::parse();

    let file_path = args.input.to_str().expect("Invalid input file path");
    let src = std::fs::read_to_string(&args.input).expect("Failed to read file");

    let (tokens, errors) = parse::lex(&file_path, &src);

    let parse_errors = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = parse::parse(file_path.to_string(), src.len(), tokens);

        if let Some(ast) = ast.filter(|_| errors.len() + parse_errs.len() == 0) {
            let (typecheck_errs, node_types) = typecheck::typecheck_program(&ast);

            if !typecheck_errs.is_empty() {
                typecheck_errs.into_iter()
                    .for_each(|typecheck::Error { msg, span, .. }| {
                        let (start_line, start_col) = offset_to_line_col(&src, span.start);

                        eprintln!(
                            "Typecheck error in {}:{}:{}: {}",
                            span.file,
                            start_line,
                            start_col,
                            msg,
                        );
                    });
            } else {
                let mil = mil::lower(&ast, node_types);
                let llvm_ir = llvm::emit(mil);

                let llvm_ir_output_path = args.output.with_extension("ll");

                std::fs::write(&llvm_ir_output_path, llvm_ir)
                    .expect("Failed to write LLVM IR to file");

                if args.emit_ir {
                    std::process::exit(0);
                }

                // Dump the embedded runtime archive into a temporary file
                let mut temp_runtime = tempfile::NamedTempFile::new().expect("Failed to create temp file");
                temp_runtime.write_all(RUNTIME_ARCHIVE).expect("Failed to write runtime archive to temp file");

                if args.emit_optimized_ir {
                    let optimized_ir_output_path = args.output.with_extension("opt.ll");

                    let status = std::process::Command::new(&args.compiler)
                        .arg(&llvm_ir_output_path)
                        // .arg(temp_runtime.path())
                        .arg("-S")
                        .arg("-emit-llvm")
                        .args(&args.compiler_flags.split_whitespace().collect::<Vec<_>>())
                        .arg("-o")
                        .arg(&optimized_ir_output_path)
                        .status()
                        .expect("Failed to execute compiler for optimized IR");

                    if !status.success() {
                        eprintln!("Compiler exited with non-zero status when generating optimized IR: {}", status);
                        std::process::exit(1);
                    }
                }

                let status = if args.shared {
                    println!("Compiling as a shared library...");
                    std::process::Command::new(&args.compiler)
                        .arg(&llvm_ir_output_path)
                        .arg(temp_runtime.path())
                        .args(&args.compiler_flags.split_whitespace().collect::<Vec<_>>())
                        .arg("-shared")
                        .arg("-o")
                        .arg(&args.output)
                        .status()
                        .expect("Failed to execute compiler for shared library")
                } else if args.static_lib {
                    todo!("Implement static library compilation");
                } else {
                    println!("Compiling as an executable...");
                    std::process::Command::new(&args.compiler)
                        .arg(&llvm_ir_output_path)
                        .arg(temp_runtime.path())
                        .args(&args.compiler_flags.split_whitespace().collect::<Vec<_>>())
                        .arg("-o")
                        .arg(&args.output)
                        .status()
                        .expect("Failed to execute compiler for executable")
                };

                if !status.success() {
                    eprintln!("Compiler exited with non-zero status: {}", status);
                    std::process::exit(1);
                }

                if !args.keep_ir {
                    std::fs::remove_file(llvm_ir_output_path).expect("Failed to remove LLVM IR file");
                }
            }
        }

        parse_errs
    } else {
        vec![]
    };

    errors.into_iter()
        .map(|e| e.map_token(|c| c.to_string()))
        .chain(parse_errors.into_iter()
            .map(|tk| tk.map_token(|t| format!("{:?}", t))))
        .for_each(|e| {
            let (start_line, start_col) = offset_to_line_col(&src, e.span().start);

            eprintln!(
                "Error in {}:{}:{}: {}",
                e.span().file,
                start_line,
                start_col,
                e.reason(),
            );
        });
}
