use std::{rc::Rc, u64};
use chumsky::{
    input::MappedInput,
    pratt::*,
    prelude::*,
};
use crate::ast::*;

/// Leak a String to get a `&'static str` (coerces to the source lifetime). used
/// for the few synthetic identifiers the parser has to mint, e.g. the joined
/// `qualifier::symbol` of a qualified ref. compiler is single-shot so leaking a
/// bounded number of names is fine.
// TODO: the rest of the pipeline now threads am arena for exactly this kind of
// synthetic name (see module.rs / mono.rs). this leak predates that and should
// probably use the arena too instead of leaking for the whole process.
fn leak(s: String) -> &'static str {
    Box::leak(s.into_boxed_str())
}

fn lexer<'a> (
    // Rc should be cheaper to clone per token
    file_path: Rc<str>,
)
-> impl Parser<
    'a,
    &'a str,
    Vec<Metadata<Token<'a>>>,
    extra::Err<Rich<'a, char>>,
> {
    macro_rules! try_parse_int {
        ($ty:ty, $val:expr, $span:expr) => {
            $val.parse::<$ty>()
                .map_err(|_| Rich::custom($span,
                    format!("failed to parse literal: {} does not fit in {}", $val, stringify!($ty))))
        };
    }

    // defaults ints and floats to 32 bits and use suffixes to disambiguate
    let float = text::int::<_, extra::Err<Rich<'a, char>>>(10)
        .then(just('.').then(text::digits(10)))
        .to_slice()
        .from_str::<f64>()
        .unwrapped()
        .then(just('f').or(just('F'))
            .ignore_then(text::int(10).from_str::<u32>().unwrapped())
            .or_not())
        .try_map(|(f, width), span| {
            match width {
                Some(32) => Ok(Token::Float32(f as f32)),
                Some(64) => Ok(Token::Float64(f)),
                None => Ok(Token::Float32(f as f32)),
                Some(other) => Err(Rich::custom(span, format!("invalid float literal suffix 'f{other}'"))),
            }
        });

    let int = text::int(10)
        .to_slice()
        .then(
            just('i').or(just('I')).or(just('u')).or(just('U'))
                .map(|c| c.to_ascii_lowercase())
                .then(text::int(10).from_str::<u32>().unwrapped())
                .or_not()
        ).try_map(|(n, suffix): (&str, _), span| {
            match suffix {
                Some(('i', 32)) => try_parse_int!(i32, n, span).map(Token::Int32),
                Some(('i', 64)) => try_parse_int!(i64, n, span).map(Token::Int64),
                Some(('u', 32)) => try_parse_int!(u32, n, span).map(Token::Uint32),
                Some(('u', 64)) => try_parse_int!(u64, n, span).map(Token::Uint64),
                None => try_parse_int!(i32, n, span).map(Token::Int32),
                Some((other, width)) => Err(Rich::custom(span, format!("invalid integer literal suffix '{other}{width}'"))),
            }
        });

    // String literal: keep the raw inner text (escapes are resolved later in
    // MIL lowering). A backslash escapes the next char so that `\"` and `\\`
    // don't prematurely terminate the literal.
    let str_char = choice((
        just('\\').then(any()).ignored(), // escape pair, e.g. \n \" \\
        none_of("\\\"").ignored(),        // any ordinary char
    ));
    let str_ = str_char
        .repeated()
        .to_slice()
        .delimited_by(just('"'), just('"'))
        .map(Token::Str);

    let ident = text::ascii::ident().map(|ident: &str| match ident {
        "true"     => Token::Bool(true),
        "false"    => Token::Bool(false),
        "let"      => Token::Let,
        "if"       => Token::If,
        "else"     => Token::Else,
        "return"   => Token::Return,
        "while"    => Token::While,
        "break"    => Token::Break,
        "continue" => Token::Continue,
        "proc"     => Token::Proc,
        "extern"   => Token::Extern,
        "const"    => Token::Const,
        "struct"   => Token::Struct,
        "import"   => Token::Import,
        _ => Token::Var(ident),
    });

    // chumsky have a built-in arity limit for choice (26, A-Z) so split these up
    let math = choice((
        just('|').then(just('|')).to(Token::BinaryOp(BinaryOp::Or)),
        just('&').then(just('&')).to(Token::BinaryOp(BinaryOp::And)),
        just('<').then(just('=')).to(Token::BinaryOp(BinaryOp::Le)),
        just('>').then(just('=')).to(Token::BinaryOp(BinaryOp::Ge)),
        just('=').then(just('=')).to(Token::BinaryOp(BinaryOp::Eq)),
        just('!').then(just('=')).to(Token::BinaryOp(BinaryOp::Ne)),

        just('^').to(Token::BinaryOp(BinaryOp::Xor)),
        just('+').to(Token::BinaryOp(BinaryOp::Add)),
        just('-').to(Token::BinaryOp(BinaryOp::Sub)), // Map to unary neg in parsing
        just('*').to(Token::BinaryOp(BinaryOp::Mul)), // This too
        just('/').to(Token::BinaryOp(BinaryOp::Div)),
        just('%').to(Token::BinaryOp(BinaryOp::Mod)),
        just('<').to(Token::BinaryOp(BinaryOp::Lt)),
        just('>').to(Token::BinaryOp(BinaryOp::Gt)),

        just('!').to(Token::UnaryOp(UnaryOp::Not)),
        just('&').to(Token::UnaryOp(UnaryOp::AddrOf)),
    ));

    let delim = choice((
        just('(').to(Token::LParen),
        just(')').to(Token::RParen),
        just('{').to(Token::LBrace),
        just('}').to(Token::RBrace),
        just('[').to(Token::LBracket),
        just(']').to(Token::RBracket),

        just('.').to(Token::Dot),
        just(',').to(Token::Comma),
        just(';').to(Token::Semicolon),
        just(':').then(just(':')).to(Token::ColonColon), // before `:`
        just(':').to(Token::Colon),
        just('=').to(Token::Assign),
        just('@').to(Token::At),
    ));

    let token = float
        .or(int)
        .or(str_)
        .or(ident)
        .or(math)
        .or(delim);

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded();

    token
        .map_with(move |tok, e| {
            let input_span: SimpleSpan = e.span();

            Metadata::new(
                tok,
                Span::new(
                    file_path.to_string(),
                    input_span.start,
                    input_span.end,
                ),
            )
        })
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), end()))
        .repeated()
        .collect()
}

pub fn lex<'a>(file_path: &'a str, source: &'a str) -> (
    Option<Vec<Metadata<Token<'a>>>>,
    Vec<chumsky::error::Rich<'a, char, Span>>
) {
    let (tks, errs) = lexer(std::rc::Rc::from(file_path))
        .parse(source)
        .into_output_errors();

    (tks, errs.into_iter()
        .map(|e| {
            e.map_span(|simple_span| {
                Span::new(
                    file_path.to_string(),
                    simple_span.start,
                    simple_span.end
                )
            })
        })
        .collect())
}

fn parse_expr<'tks, 'src: 'tks>()
-> impl Parser<
    'tks,
    MappedInput<'tks, Token<'src>, Span, &'tks [Metadata<Token<'src>>]>,
    Expr<'src>,
    extra::Err<Rich<'tks, Token<'src>, Span>>,
> {
    recursive(|expr| {
        let var = select_ref! { Token::Var(ident) => ident };

        macro_rules! una {
            // Separate $from and $op because some operators (like '-') can be
            // both unary and binary
            ($from:expr, $op:expr, $precedence:expr) => {
                prefix($precedence, just($from), |_, v, e|
                    Metadata::new(
                        ExprNode::Unary {
                            op: $op,
                            operand: Box::new(v),
                        },
                        e.span(),
                    )
                )
            };
        }

        macro_rules! bin {
            ($op:expr, $precedence:expr) => {
                infix(left($precedence), just(Token::BinaryOp($op)), |x, _, y, e|
                    Metadata::new(
                        ExprNode::Binary {
                            op: $op,
                            left: Box::new(x),
                            right: Box::new(y),
                        },
                        e.span(),
                    )
                )
            };
        }

        choice((
            select_ref! {
                Token::Bool(b)    => ExprNode::Bool(*b),
                Token::Int32(i)   => ExprNode::Int32(*i),
                Token::Int64(i)   => ExprNode::Int64(*i),
                Token::Uint32(u)  => ExprNode::Uint32(*u),
                Token::Uint64(u)  => ExprNode::Uint64(*u),
                Token::Float32(f) => ExprNode::Float32(*f),
                Token::Float64(f) => ExprNode::Float64(*f),
                Token::Str(s)     => ExprNode::Str(*s),
            },

            // Struct init, with optional type params: S or S<T1, T2>{f1: v1, f2: v2}
            var.map(|s| *s)
                // .then(
                //     parse_type()
                //         .separated_by(just(Token::Comma))
                //         .allow_trailing()
                //         .collect::<Vec<_>>()
                //         .delimited_by(
                //             just(Token::BinaryOp(BinaryOp::Lt)),
                //             just(Token::BinaryOp(BinaryOp::Gt))
                //         )
                //         .or_not()
                // )
                .then(
                    var.map(|s| *s)
                        .then_ignore(just(Token::Colon))
                        .then(expr.clone())
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .delimited_by(just(Token::LBrace), just(Token::RBrace))
                )
                .map(|(name, fields)| ExprNode::Struct {
                    name,
                    fields,
                }),

            // a variable, or a module-qualified ref `qualifier::symbol`
            // (e.g. `math::sinf`). the qualified form is one `Var` holding the
            // joined `"qualifier::symbol"` string; the module resolver splits and
            // rewrites it before any later stage. the `::symbol` is optional and
            // only taken when followed by an ident, so a turbofish `::<...>` is
            // left for the call postfix.
            // TODO: only one `::` segment — `a::b::c` leaves `::c` dangling.
            var.then(just(Token::ColonColon).ignore_then(var).or_not())
                .map(|(a, b)| match b {
                    Some(sym) => ExprNode::Var(leak(format!("{}::{}", a, sym))),
                    None => ExprNode::Var(*a),
                }),
            expr.clone()
                .separated_by(just(Token::Comma))
                .allow_leading()
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBracket), just(Token::RBracket))
                .map(|inner| ExprNode::Slice(inner))
        ))

        .map_with(|node, e| {
            Metadata::new(
                node,
                e.span(),
            )
        })
        .boxed()
        .or(expr.clone().delimited_by(just(Token::LParen), just(Token::RParen)))

        .pratt((
            postfix(
                200,
                just(Token::Dot).ignore_then(var),
                |base, field: &&str, e| {
                    Metadata::new(
                        ExprNode::Access {
                            base: Box::new(base),
                            field: *field,
                        },
                        e.span(),
                    )
                }
            ),

            // calls, with an optional turbofish `::<T, N>` before the args.
            // `::` disambiguates from the `<`/`>` comparison operators.
            postfix(
                300,
                just(Token::ColonColon)
                    .ignore_then(
                        choice((
                            select! { Token::Int32(n) => GenericArg::Const(n as i64) },
                            parse_type().map(GenericArg::Type),
                        ))
                        .separated_by(just(Token::Comma))
                        .allow_trailing()
                        .collect::<Vec<_>>()
                        .delimited_by(
                            just(Token::BinaryOp(BinaryOp::Lt)),
                            just(Token::BinaryOp(BinaryOp::Gt))),
                    )
                    .or_not()
                    .map(|t| t.unwrap_or_default())
                    .then(
                        expr.clone()
                            .separated_by(just(Token::Comma))
                            .allow_trailing()
                            .collect::<Vec<_>>()
                            .delimited_by(just(Token::LParen), just(Token::RParen)),
                    )
                    .boxed(),
                |func, (type_args, args), e|
                Metadata::new(
                    ExprNode::Call {
                        func: Box::new(func),
                        type_args,
                        args,
                    },
                    e.span(),
                ),
            ),

            // Index
            postfix(
                290,
                expr.clone().delimited_by(just(Token::LBracket), just(Token::RBracket)),
                |slice, index, e| Metadata::new(
                    ExprNode::Index {
                        slice: Box::new(slice),
                        index: Box::new(index),
                    },
                    e.span(),
                ),
            ),

            una!(Token::BinaryOp(BinaryOp::Sub), UnaryOp::Neg, 190),
            una!(Token::UnaryOp(UnaryOp::Not), UnaryOp::Not, 190),
            una!(Token::BinaryOp(BinaryOp::Mul), UnaryOp::Deref, 190),
            una!(Token::UnaryOp(UnaryOp::AddrOf), UnaryOp::AddrOf, 190),

            bin!(BinaryOp::Mul, 180),
            bin!(BinaryOp::Div, 180),

            bin!(BinaryOp::Add, 170),
            bin!(BinaryOp::Sub, 170),

            bin!(BinaryOp::Lt,  160),
            bin!(BinaryOp::Gt,  160),
            bin!(BinaryOp::Le,  160),
            bin!(BinaryOp::Ge,  160),

            bin!(BinaryOp::Eq,  150),
            bin!(BinaryOp::Ne,  150),

            bin!(BinaryOp::Xor, 140),
            bin!(BinaryOp::And, 130),
            bin!(BinaryOp::Or,  120),
        ))
    })
}

// TODO: no fn-type production yet (fn-as-value), and no user generic-type
// production either, only the hardcoded `simd<T,N>` handles `name<...>`. so a
// generic type in type position (`x: List<T>`, nested turbofish `f::<Vec<i32>>`)
// won't parse. mangle_ty in mono.rs already collapses fn types to "fn", so once
// this lands watch the mangler collision noted there.
fn parse_type<'tks, 'src: 'tks>()
-> impl Parser<
    'tks,
    MappedInput<'tks, Token<'src>, Span, &'tks [Metadata<Token<'src>>]>,
    Type<'src>,
    extra::Err<Rich<'tks, Token<'src>, Span>>,
> {
    recursive(|ty| {
        let var = select_ref! { Token::Var(ident) => ident };

        choice((
            // [T; N]
            just(Token::LBracket)
                .ignore_then(ty.clone())
                .then_ignore(just(Token::Semicolon))
                .then(select! { Token::Int32(x) => x })
                .then_ignore(just(Token::RBracket))
                .try_map(|(inner, size), span| {
                    if size > 0 {
                        Ok(Type::Array(Box::new(inner), size as usize))
                    } else {
                        Err(Rich::custom(span, format!("invalid array size parameter: {size} (must be greater than 0)")))
                    }
                }),
            // [T]
            just(Token::LBracket)
                .ignore_then(ty.clone())
                .then_ignore(just(Token::RBracket))
                .map(|inner| Type::Slice(Box::new(inner))),
            // T or simd<T, N>
            var.then(ty.clone()
                .then_ignore(just(Token::Comma))
                .then(select! { Token::Int32(x) => x })
                .delimited_by(
                    just(Token::BinaryOp(BinaryOp::Lt)),
                    just(Token::BinaryOp(BinaryOp::Gt)))
                .or_not())
            .try_map(|(name, size), span| {
                if let None = size {
                    Ok(match *name {
                        "void" => Type::Void,
                        "bool" => Type::Bool,
                        "i32"  => Type::Int32,
                        "i64"  => Type::Int64,
                        "u32"  => Type::Uint32,
                        "u64"  => Type::Uint64,
                        "f32"  => Type::Float32,
                        "f64"  => Type::Float64,
                        "str"  => Type::Str,
                        other  => Type::Struct(other),
                    })
                } else {
                    // probably could reuse this when we have generic types
                    // hope we reach that point lmao
                    let (ty, size) = size.unwrap();
                    match *name {
                        "simd" if size > 0 && size <= 64 => Ok(Type::Simd(Box::new(ty), size as usize)),
                        "simd" => Err(Rich::custom(span, format!("invalid SIMD size parameter: {size} (must be between 1 and 64)"))),
                        other => Err(Rich::custom(span, format!("unexpected type '{other}' with size parameter"))),
                    }
                }
            })
        ))
        .boxed()
        .pratt((
            // [T]
            // prefix(1, just(Token::LBracket).then(just(Token::RBracket)), |_, t, _| Type::Slice(Box::new(t))),
            // *T
            prefix(1, just(Token::BinaryOp(BinaryOp::Mul)), |_, t, _| Type::Pointer(Box::new(t))),
        ))
        .labelled("type")
    })
}

fn parse_stmt<'tks, 'src: 'tks>()
-> impl Parser<
    'tks,
    MappedInput<'tks, Token<'src>, Span, &'tks [Metadata<Token<'src>>]>,
    Stmt<'src>,
    extra::Err<Rich<'tks, Token<'src>, Span>>,
> {
    recursive(|stmt| {
        let var = select_ref! { Token::Var(ident) => ident };

        let single_stmt_or_block = stmt.clone()
            .repeated()
            .collect::<Vec<_>>()
            .delimited_by(just(Token::LBrace), just(Token::RBrace))
            .map_with(|node, e| Metadata::new(
                StmtNode::Block(node),
                e.span(),
            ))
            .or(stmt.clone());

        let declare = just(Token::Let)
            .ignore_then(var)
            .then_ignore(just(Token::Colon))
            .then(parse_type())
            .then_ignore(just(Token::Assign))
            .then(parse_expr())
            .then_ignore(just(Token::Semicolon))
            .map(|((name, ty), value)| StmtNode::Declare {
                name,
                ty,
                value,
            });

        let assign_or_expr = parse_expr()
            .then(just(Token::Assign)
                .ignore_then(parse_expr())
                .or_not())
            .then_ignore(just(Token::Semicolon))
            .map(|(left, value)| match value {
                Some(value) => StmtNode::Assign { left, value },
                None => StmtNode::Expr(left),
            });

        let if_ = just(Token::If)
            .ignore_then(parse_expr()
                .delimited_by(just(Token::LParen), just(Token::RParen)))
            .then(single_stmt_or_block.clone())
            .map(|(condition, then_branch)| StmtNode::If {
                condition,
                then_branch: Box::new(then_branch),
                else_branch: None,
            });

        let if_else = just(Token::If)
            .ignore_then(parse_expr()
                .delimited_by(just(Token::LParen), just(Token::RParen)))
            .then(single_stmt_or_block.clone())
            .then(just(Token::Else)
                .ignore_then(single_stmt_or_block.clone())
            )
            .map(|((condition, then_branch), else_branch)| StmtNode::If {
                condition,
                then_branch: Box::new(then_branch),
                else_branch: Some(Box::new(else_branch)),
            });

        let while_ = just(Token::While)
            .ignore_then(parse_expr()
                .delimited_by(just(Token::LParen), just(Token::RParen)))
            .then(single_stmt_or_block.clone())
            .map(|(condition, body)| StmtNode::While {
                condition,
                body: Box::new(body),
            });

        let contbreak = choice((
            just(Token::Continue).to(StmtNode::Continue),
            just(Token::Break).to(StmtNode::Break),
        )).then_ignore(just(Token::Semicolon));

        let return_ = just(Token::Return)
            .ignore_then(parse_expr())
            .then_ignore(just(Token::Semicolon))
            .map(StmtNode::Return);

        declare
            .or(assign_or_expr)
            .or(if_else)
            .or(if_)
            .or(while_)
            .or(contbreak)
            .or(return_)
            .map_with(|node, e| {
                Metadata::new(
                    node,
                    e.span(),
                )
            })
            .boxed()
    })
}

fn parse_attribute<'tks, 'src: 'tks>()
-> impl Parser<
    'tks,
    MappedInput<'tks, Token<'src>, Span, &'tks [Metadata<Token<'src>>]>,
    Attribute<'src>,
    extra::Err<Rich<'tks, Token<'src>, Span>>,
> {
    just(Token::At)
        .ignore_then(select_ref! { Token::Var(ident) => ident })
        .then(
            just(Token::LParen)
                .ignore_then(select_ref! {
                    Token::Var(s) => s.to_string(),
                    Token::Bool(b) => if *b { "true" } else { "false" }.to_string(),
                    Token::Int32(i) => i.to_string(),
                    // Token::Str(s) => s.to_string()
                })
                .then_ignore(just(Token::RParen))
                .or_not()
        )
        .map(|(name, value)| AttributeNode::new(name, value))
        .map_with(|attr, e| Metadata::new(attr, e.span()))
        .boxed()
}

fn parse_toplevel<'tks, 'src: 'tks>()
-> impl Parser<
    'tks,
    MappedInput<'tks, Token<'src>, Span, &'tks [Metadata<Token<'src>>]>,
    TopLevel<'src>,
    extra::Err<Rich<'tks, Token<'src>, Span>>,
> {
    let var = select_ref! { Token::Var(ident) => ident };

    // a single generic parameter: either `const N: u32` or a bare type param `T`
    let generic_param = choice((
        just(Token::Const)
            .ignore_then(var.map(|s| *s))
            .then_ignore(just(Token::Colon))
            .then(parse_type())
            .map(|(name, ty)| GenericParam::Const(name, ty)),
        var.map(|s| GenericParam::Type(*s)),
    ));

    // optional `<T, const N: u32, ...>` list following a function name
    let generics = generic_param
        .separated_by(just(Token::Comma))
        .allow_trailing()
        .collect::<Vec<_>>()
        .delimited_by(
            just(Token::BinaryOp(BinaryOp::Lt)),
            just(Token::BinaryOp(BinaryOp::Gt)))
        .or_not()
        .map(|g| g.unwrap_or_default())
        .boxed();

    let function = parse_attribute()
        .repeated().collect::<Vec<_>>()
        .then_ignore(just(Token::Proc))
        .then(var)
        .then(generics.clone())
        .then(
            var.map(|s| *s)
                .then_ignore(just(Token::Colon))
                .then(parse_type())
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen))
        )
        .then(parse_type().or_not().map(|t| t.unwrap_or(Type::Void)))
        .then(
            parse_stmt()
                // .separated_by(just(Token::Semicolon))
                // .allow_leading()
                // .allow_trailing()
                .repeated()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
        )
        .map(|(((((attributes, name), generics), params), return_type), body)| TopLevelNode::Function {
            name,
            attributes,
            generics,
            params,
            return_type,
            body,
        });

    let extern_ = parse_attribute()
        .repeated().collect::<Vec<_>>()
        .then_ignore(just(Token::Extern))
        .then(var)
        .then(generics.clone())
        .then(
            var.map(|s| *s)
                .then_ignore(just(Token::Colon))
                .then(parse_type())
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LParen), just(Token::RParen))
        )
        .then(parse_type().or_not().map(|t| t.unwrap_or(Type::Void)))
        .then_ignore(just(Token::Semicolon))
        .map(|((((attributes, name), generics), params), return_type)| TopLevelNode::Extern {
            name,
            attributes,
            generics,
            params,
            return_type,
        });

    let struct_ = parse_attribute()
        .repeated().collect::<Vec<_>>()
        .then_ignore(just(Token::Struct))
        .then(var)
        .then(
            var.map(|s| *s)
                .then_ignore(just(Token::Colon))
                .then(parse_type())
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
        )
        .map(|((attributes, name), fields)| TopLevelNode::Struct {
            name,
            attributes,
            fields,
        });

    choice((
        function,
        extern_,
        struct_,
    ))
        .map_with(|node, e| {
            Metadata::new(
                node,
                e.span(),
            )
        })
        .boxed()
}

fn parse_import<'tks, 'src: 'tks>()
-> impl Parser<
    'tks,
    MappedInput<'tks, Token<'src>, Span, &'tks [Metadata<Token<'src>>]>,
    Import<'src>,
    extra::Err<Rich<'tks, Token<'src>, Span>>,
> {
    let var = select_ref! { Token::Var(ident) => *ident };

    // `import seg/seg/...` optionally followed by `{ sym, sym, ... }`. the path
    // separator reuses the `/` (division) token; unambiguous here since an import
    // never contains an expression.
    // TODO: path segments are `var` only, so a segment that lexes to a keyword
    // (`import std/const`) won't parse. and `{}` is `.at_least(1)`, so an empty
    // selective import is a hard error rather than a no-op.
    just(Token::Import)
        .ignore_then(
            var.separated_by(just(Token::BinaryOp(BinaryOp::Div)))
                .at_least(1)
                .collect::<Vec<_>>()
        )
        .then(
            var.separated_by(just(Token::Comma))
                .allow_trailing()
                .at_least(1)
                .collect::<Vec<_>>()
                .delimited_by(just(Token::LBrace), just(Token::RBrace))
                .or_not()
        )
        .map_with(|(path, symbols), e| Import { span: e.span(), path, symbols })
        .boxed()
}

/// One item at file scope: an `import` or a real top-level definition. parsed
/// from the same stream and partitioned by `parse`.
enum FileItem<'a> {
    Import(Import<'a>),
    Item(TopLevel<'a>),
}

pub fn parse<'a>(file_path: String, len: usize, tokens: &'a [Metadata<Token<'a>>]) -> (
    Option<(Vec<Import<'a>>, Vec<TopLevel<'a>>)>,
    Vec<chumsky::error::Rich<'a, Token<'a>, Span>>,
) {
    let (out, errs) = choice((
            parse_import().map(FileItem::Import),
            parse_toplevel().map(FileItem::Item),
        ))
        .repeated()
        .collect::<Vec<_>>()
        .parse(
            tokens
            .map(Span::new(file_path, len, len),
                |Metadata { value: t, span: s, .. }| (t, s),
            ))
        .into_output_errors();

    let split = out.map(|items| {
        let mut imports = Vec::new();
        let mut tops = Vec::new();
        for item in items {
            match item {
                FileItem::Import(i) => imports.push(i),
                FileItem::Item(t) => tops.push(t),
            }
        }
        (imports, tops)
    });

    (split, errs)
}
