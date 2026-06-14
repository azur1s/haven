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


fn main() {
    let file_path = std::env::args().nth(1).expect("Please provide a file path");
    let src = std::fs::read_to_string(&file_path).expect("Failed to read file");

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
                // ast.iter().for_each(|node| println!("{}", node.value));
                let mil = mil::lower(&ast, node_types);
                // println!("{}", mil);
                let llvm_ir = llvm::emit(mil);
                // println!("{}", llvm_ir);
                // output to out.ll
                std::fs::write("out.ll", llvm_ir).expect("Failed to write LLVM IR to file");
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
