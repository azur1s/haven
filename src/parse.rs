use std::rc::Rc;
use chumsky::{
    input::MappedInput,
    pratt::*,
    prelude::*,
};
use crate::ast::*;

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
    let float = text::int(10)
        .then(just('.').then(text::digits(10)))
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::Float32);

    let int = text::int(10)
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::Int32);

    // let str_ = just('"')
    //     .ignore_then(none_of('"').repeated().to_slice())
    //     .then_ignore(just('"'))
    //     .map(Token::Str);

    let ident = text::ascii::ident().map(|ident: &str| match ident {
        "true"   => Token::Bool(true),
        "false"  => Token::Bool(false),
        "let"    => Token::Let,
        "if"     => Token::If,
        "else"   => Token::Else,
        "while"  => Token::While,
        "return" => Token::Return,
        "proc"   => Token::Proc,
        "extern" => Token::Extern,
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
        just(':').to(Token::Colon),
        just('=').to(Token::Assign),
        just('@').to(Token::At),
    ));

    let token = float
        .or(int)
        // .or(str_)
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
                Token::Float32(f) => ExprNode::Float32(*f),
                // Token::Str(s)     => ExprNode::Str(*s),
            },

            // Struct init, with optional type params: S or S<T1, T2>{f1: v1, f2: v2}
            // var.map(|s| *s)
            //     .then(
            //         parse_type()
            //             .separated_by(just(Token::Comma))
            //             .allow_trailing()
            //             .collect::<Vec<_>>()
            //             .delimited_by(
            //                 just(Token::BinaryOp(BinaryOp::Lt)),
            //                 just(Token::BinaryOp(BinaryOp::Gt))
            //             )
            //             .or_not()
            //     )
            //     .then(
            //         var.map(|s| *s)
            //             .then_ignore(just(Token::Colon))
            //             .then(expr.clone())
            //             .separated_by(just(Token::Comma))
            //             .allow_trailing()
            //             .collect::<Vec<_>>()
            //             .delimited_by(just(Token::LBrace), just(Token::RBrace))
            //     )
            //     .map(|((name, params), fields)| ExprNode::StructInit {
            //         name,
            //         params: params.unwrap_or_else(|| vec![]),
            //         fields,
            //     }),

            var.map(|s| ExprNode::Var(*s)),
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
            // postfix(
            //     200,
            //     just(Token::Dot).ignore_then(var),
            //     |base, field: &&str, e| {
            //         Metadata::new(
            //             ExprNode::Access {
            //                 base: Box::new(base),
            //                 field: *field,
            //             },
            //             e.span(),
            //         )
            //     }
            // ),

            // Calls
            postfix(
                300,
                expr.clone()
                    .separated_by(just(Token::Comma))
                    .allow_trailing()
                    .collect::<Vec<_>>()
                    .delimited_by(just(Token::LParen), just(Token::RParen)),
                |func, args, e|
                Metadata::new(
                    ExprNode::Call {
                        func: Box::new(func),
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

fn parse_type<'tks, 'src: 'tks>()
-> impl Parser<
    'tks,
    MappedInput<'tks, Token<'src>, Span, &'tks [Metadata<Token<'src>>]>,
    Type<'src>,
    extra::Err<Rich<'tks, Token<'src>, Span>>,
> {
    // recursive(|ty| {
        let var = select_ref! { Token::Var(ident) => ident };

        var.map(|s| match *s {
            "void" => Type::Void,
            "bool" => Type::Bool,
            "i32"  => Type::Int32,
            "f32"  => Type::Float32,
            // "str"  => Type::Str,
            other  => Type::Defined(other),
        })
        .boxed()
        .pratt((
            prefix(1, just(Token::LBracket).then(just(Token::RBracket)), |_, t, _| Type::Slice(Box::new(t))),
            prefix(1, just(Token::BinaryOp(BinaryOp::Mul)), |_, t, _| Type::Pointer(Box::new(t))),
        ))
        .labelled("type")
    // })
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

        let return_ = just(Token::Return)
            .ignore_then(parse_expr())
            .then_ignore(just(Token::Semicolon))
            .map(StmtNode::Return);

        declare
            .or(assign_or_expr)
            .or(if_else)
            .or(if_)
            .or(while_)
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
        .map(|(name, value)| AttributeNode {
            name,
            value,
        })
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

    let function = parse_attribute()
        .repeated().collect::<Vec<_>>()
        .then_ignore(just(Token::Proc))
        .then(var)
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
        .map(|((((attributes, name), params), return_type), body)| TopLevelNode::Function {
            name,
            attributes,
            params,
            return_type,
            body,
        });

    let extern_ = parse_attribute()
        .repeated().collect::<Vec<_>>()
        .then_ignore(just(Token::Extern))
        .then(var)
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
        .map(|(((attributes, name), params), return_type)| TopLevelNode::Extern {
            name,
            attributes,
            params,
            return_type,
        });

    function
        .or(extern_)
        .map_with(|node, e| {
            Metadata::new(
                node,
                e.span(),
            )
        })
        .boxed()
}

pub fn parse<'a>(file_path: String, len: usize, tokens: &'a Vec<Metadata<Token<'a>>>) -> (
    Option<Vec<TopLevel<'a>>>,
    Vec<chumsky::error::Rich<'a, Token<'a>, Span>>,
) {
    parse_toplevel()
        .repeated()
        .collect::<Vec<_>>()
        .parse(
            tokens
            .as_slice()
            .map(Span::new(file_path, len, len),
                |Metadata { value: t, span: s, .. }| (t, s),
            ))
        .into_output_errors()
}
