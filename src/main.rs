use std::io::Write;
use clap::Parser;

mod args;
mod ast;
mod parse;
mod intrinsics;
mod typecheck;
mod safecheck;
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

/// Prelude source, embedded at compile time. Parsed and injected ahead of every
/// user program (see `main`). Backed by the C runtime in `crt/rt.c`.
const PRELUDE_SRC: &str = include_str!("../crt/prelude.ixc");

/// The declared name of a top-level item, used to let user definitions
/// override (shadow) prelude ones.
fn toplevel_name<'a>(item: &ast::TopLevel<'a>) -> &'a str {
    match &item.value {
        ast::TopLevelNode::Function { name, .. }
        | ast::TopLevelNode::Extern { name, .. }
        | ast::TopLevelNode::Struct { name, .. } => *name,
    }
}

fn main() {
    let args = args::Args::parse();

    let file_path = args.input.to_str().expect("Invalid input file path");
    let src = std::fs::read_to_string(&args.input).expect("Failed to read file");

    // lex the prelude up front, its tokens (and thus the AST borrowing them)
    // must outlive the user AST so the two can be merged. a failure here is a
    // compiler bug
    let (prelude_tokens, prelude_lex_errs) = parse::lex("<prelude>", PRELUDE_SRC);
    let prelude_tokens = prelude_tokens.expect("prelude failed to lex (compiler bug)");
    assert!(prelude_lex_errs.is_empty(), "prelude lex errors (compiler bug): {:?}", prelude_lex_errs);

    let (tokens, errors) = parse::lex(&file_path, &src);

    let parse_errors = if let Some(tokens) = &tokens {
        let (ast, parse_errs) = parse::parse(file_path.to_string(), src.len(), tokens);

        // inject the prelude, prepend its declarations to the user program.
        // a user definition may not collide with a prelude name (no implicit
        // shadowing), every collision is a hard error. can be skipped under
        // --no-prelude for freestanding builds
        // this is temporary, when there's better module support, the prelude
        // could be a module that can be imported, and there shouldn't be this
        // problem
        let ast = if args.no_prelude {
            ast
        } else {
            ast.map(|user_ast| {
                let (prelude_ast, prelude_errs) =
                    parse::parse("<prelude>".to_string(), PRELUDE_SRC.len(), &prelude_tokens);
                let prelude_ast = prelude_ast.expect("prelude failed to parse (compiler bug)");
                assert!(prelude_errs.is_empty(), "prelude parse errors (compiler bug): {:?}", prelude_errs);

                let prelude_names: std::collections::HashSet<&str> =
                    prelude_ast.iter().map(toplevel_name).collect();

                let collisions: Vec<(&str, ast::Span)> = user_ast.iter()
                    .filter(|item| prelude_names.contains(toplevel_name(item)))
                    .map(|item| (toplevel_name(item), item.span.clone()))
                    .collect();
                if !collisions.is_empty() {
                    for (name, span) in collisions {
                        let (line, col) = offset_to_line_col(&src, span.start);
                        eprintln!(
                            "Error in {}:{}:{}: '{}' collides with a prelude definition; \
                             rename it, or compile with --no-prelude to drop the prelude",
                            span.file, line, col, name,
                        );
                    }
                    std::process::exit(1);
                }

                let mut merged = prelude_ast;
                merged.extend(user_ast);
                merged
            })
        };

        if let Some(ast) = ast.filter(|_| errors.len() + parse_errs.len() == 0) {
            let mut cx = typecheck::Context::new();
            let typecheck_errs = typecheck::typecheck_program(&mut cx, &ast);

            // check if there is no main function when compiling an executable
            if !args.shared && !args.static_lib {
                let main_fn = ast.iter().find_map(|item| {
                    if let ast::TopLevelNode::Function { name, .. } = item.value {
                        if name == "main" {
                            Some(())
                        } else {
                            None
                        }
                    } else {
                        None
                    }
                });

                if main_fn.is_none() {
                    eprintln!("Error: No 'main' function exported");
                    eprintln!("If you intended to compile a shared/static library, use the --shared or --static-lib flag.");
                    eprintln!("Otherwise, add a 'main' function to your program and an @export attribute to it.");
                    std::process::exit(1);
                }
            }

            if !typecheck_errs.is_empty() {
                typecheck_errs.into_iter()
                    .for_each(|ast::Error { msg, span, .. }| {
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
                safecheck::alloc_check_program(&ast).unwrap_or_else(|errs| {
                    for err in errs {
                        let (start_line, start_col) = offset_to_line_col(&src, err.span.start);

                        eprintln!(
                            "Check error in {}:{}:{}: {}",
                            err.span.file,
                            start_line,
                            start_col,
                            err.msg,
                        );
                    }

                    std::process::exit(1);
                });

                let mil = mil::lower(&ast, &cx);
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

                if args.emit_asm {
                    let asm_output_path = args.output.with_extension("s");

                    let status = std::process::Command::new(&args.compiler)
                        .arg(&llvm_ir_output_path)
                        .arg("-S")
                        .args(&args.compiler_flags.split_whitespace().collect::<Vec<_>>())
                        .arg("-o")
                        .arg(&asm_output_path)
                        .status()
                        .expect("Failed to execute compiler for assembly");

                    if !status.success() {
                        eprintln!("Compiler exited with non-zero status when generating assembly: {}", status);
                        std::process::exit(1);
                    }

                    std::process::exit(0);
                }

                let status = if args.shared {
                    println!("Compiling as a shared library...");

                    let shared_output_path = if cfg!(target_os = "windows") {
                        args.output.with_extension("dll")
                    } else if cfg!(target_os = "macos") {
                        args.output.with_extension("dylib")
                    } else {
                        // Default to .so for linux & other platforms
                        args.output.with_extension("so")
                    };

                    std::process::Command::new(&args.compiler)
                        .arg(&llvm_ir_output_path)
                        .arg(temp_runtime.path())
                        .args(&args.compiler_flags.split_whitespace().collect::<Vec<_>>())
                        .arg("-shared")
                        .arg("-o")
                        .arg(&shared_output_path)
                        .status()
                        .expect("Failed to execute compiler for shared library")
                } else if args.static_lib {
                    todo!("Implement static library compilation");
                } else {
                    println!("Compiling as an executable...");

                    let output = if cfg!(target_os = "windows") {
                        args.output.with_extension("exe")
                    } else {
                        args.output
                    };

                    std::process::Command::new(&args.compiler)
                        .arg(&llvm_ir_output_path)
                        .arg(temp_runtime.path())
                        .args(&args.compiler_flags.split_whitespace().collect::<Vec<_>>())
                        .arg("-o")
                        .arg(&output)
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
