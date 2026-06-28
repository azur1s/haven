use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub struct Span {
    pub file: String,
    pub start: usize,
    pub end: usize,
}

impl Span {
    pub fn new(file: String, start: usize, end: usize) -> Self {
        Self { file, start, end }
    }
}

impl chumsky::span::Span for Span {
    type Context = String;
    type Offset = usize;

    fn context(&self) -> Self::Context { self.file.clone() }
    fn new(context: Self::Context, range: std::ops::Range<Self::Offset>) -> Self {
        Self {
            file: context,
            start: range.start,
            end: range.end,
        }
    }
    fn start(&self) -> usize { self.start }
    fn end(&self) -> usize { self.end }
}

#[derive(Clone, Debug)]
pub struct Error {
    pub msg: String,
    pub span: Span,
}

impl Error {
    pub fn new(span: Span, msg: String) -> Self {
        Self { span, msg }
    }
}

#[derive(Clone, Debug)]
pub struct Metadata<T> {
    pub span: Span,
    pub id: usize,
    pub value: T,
}

static GLOBAL_ID_COUNTER: std::sync::atomic::AtomicUsize =
    std::sync::atomic::AtomicUsize::new(0);

impl<T> Metadata<T> {
    pub fn new(value: T, span: Span) -> Self {
        let id = GLOBAL_ID_COUNTER.fetch_add(1, std::sync::atomic::Ordering::SeqCst);
        Self { value, id, span }
    }
}

/// Extension trait to add a convenient method for creating metadata from a value and span.
// pub trait MetadataExt<T> {
//     fn make_metadata(self, span: Span) -> Metadata<T>;
// }

// impl<T> MetadataExt<T> for T {
//     fn make_metadata(self, span: Span) -> Metadata<T> {
//         Metadata::new(self, span)
//     }
// }

#[derive(Clone, Debug, PartialEq)]
pub enum Token<'a> {
    Bool(bool),
    Int32(i32), Int64(i64),
    Uint32(u32), Uint64(u64),
    Float32(f32), Float64(f64),
    Var(&'a str),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),

    LParen, RParen,
    LBrace, RBrace,
    LBracket, RBracket,

    Dot, Comma, Semicolon,
    Colon, Assign, At,

    Let, If, Else, Return,
    While, Break, Continue,
    Proc, Extern
}

impl Display for Token<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Bool(b)      => write!(f, "{}", b),
            Token::Int32(n)     => write!(f, "{}i32", n),
            Token::Int64(n)     => write!(f, "{}i64", n),
            Token::Uint32(n)    => write!(f, "{}u32", n),
            Token::Uint64(n)    => write!(f, "{}u64", n),
            Token::Float32(n)   => write!(f, "{}f32", n),
            Token::Float64(n)   => write!(f, "{}f64", n),
            Token::Var(s)       => write!(f, "{}", s),
            Token::BinaryOp(op) => write!(f, "{}", op),
            Token::UnaryOp(op)  => write!(f, "{}", op),
            Token::LParen       => write!(f, "("),
            Token::RParen       => write!(f, ")"),
            Token::LBrace       => write!(f, "{{"),
            Token::RBrace       => write!(f, "}}"),
            Token::LBracket     => write!(f, "["),
            Token::RBracket     => write!(f, "]"),
            Token::Dot          => write!(f, "."),
            Token::Comma        => write!(f, ","),
            Token::Semicolon    => write!(f, ";"),
            Token::Colon        => write!(f, ":"),
            Token::Assign       => write!(f, "="),
            Token::At           => write!(f, "@"),
            Token::Let          => write!(f, "let"),
            Token::If           => write!(f, "if"),
            Token::Else         => write!(f, "else"),
            Token::Return       => write!(f, "return"),
            Token::While        => write!(f, "while"),
            Token::Break        => write!(f, "break"),
            Token::Continue     => write!(f, "continue"),
            Token::Proc         => write!(f, "proc"),
            Token::Extern       => write!(f, "extern"),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOp { Neg, Not, Deref, AddrOf, }

impl Display for UnaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            UnaryOp::Neg    => "-",
            UnaryOp::Not    => "!",
            UnaryOp::Deref  => "*",
            UnaryOp::AddrOf => "&",
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod,
    Eq, Ne, Lt, Gt, Le, Ge,
    And, Or, Xor,
}

impl Display for BinaryOp {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Eq  => "==",
            BinaryOp::Ne  => "!=",
            BinaryOp::Lt  => "<",
            BinaryOp::Gt  => ">",
            BinaryOp::Le  => "<=",
            BinaryOp::Ge  => ">=",
            BinaryOp::And => "&&",
            BinaryOp::Or  => "||",
            BinaryOp::Xor => "^",
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type<'a> {
    Void, Bool,
    Int32, Int64,
    Uint32, Uint64,
    Float32, Float64,
    Function {
        params: Vec<Type<'a>>,
        return_type: Box<Type<'a>>,
    },
    Pointer(Box<Self>),
    /// Fixed-size array type, e.g., `[T; N]`
    Array(Box<Self>, usize),
    /// Slice type, e.g., `[T]`
    Slice(Box<Self>),
    Simd(Box<Self>, usize),
    Defined(&'a str),
}

impl<'a> Type<'a> {
    pub fn is_numeric(&self) -> bool {
        matches!(self,
            Type::Int32 | Type::Int64
            | Type::Uint32 | Type::Uint64
            | Type::Float32 | Type::Float64)
    }

    pub fn is_integer(&self) -> bool {
        matches!(self, Type::Int32 | Type::Int64 | Type::Uint32 | Type::Uint64)
    }

    pub fn is_numeric_or_numeric_simd(&self) -> bool {
        self.is_numeric() || matches!(self, Type::Simd(inner, _) if inner.is_numeric())
    }
}

impl<'a> Display for Type<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Type::*;
        match self {
            Void => write!(f, "void"),
            Bool => write!(f, "bool"),
            Uint32 => write!(f, "u32"), Uint64 => write!(f, "u64"),
            Int32 => write!(f, "i32"), Int64 => write!(f, "i64"),
            Float32 => write!(f, "f32"), Float64 => write!(f, "f64"),
            Array(inner, size) => write!(f, "[{}; {}]", inner, size),
            Function { params, return_type } => {
                let params_str = params.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "fn({}) -> {}", params_str, return_type)
            },
            Pointer(inner) => write!(f, "*{}", inner),
            Slice(inner) => write!(f, "{}[]", inner),
            Simd(inner, size) => write!(f, "simd[{}, {}]", inner, size),
            Defined(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExprNode<'a> {
    Bool(bool),
    Int32(i32), Int64(i64),
    Uint32(u32), Uint64(u64),
    Float32(f32), Float64(f64),
    Var(&'a str),
    Slice(Vec<Expr<'a>>),
    Index {
        slice: Box<Expr<'a>>,
        index: Box<Expr<'a>>,
    },
    Unary {
        op: UnaryOp,
        operand: Box<Expr<'a>>,
    },
    Binary {
        op: BinaryOp,
        left: Box<Expr<'a>>,
        right: Box<Expr<'a>>,
    },
    Call {
        func: Box<Expr<'a>>,
        args: Vec<Expr<'a>>,
    },
}

impl<'a> Display for ExprNode<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprNode::Bool(val) => write!(f, "{}", val),
            ExprNode::Int32(val) => write!(f, "{}i32", val),
            ExprNode::Int64(val) => write!(f, "{}i64", val),
            ExprNode::Uint32(val) => write!(f, "{}u32", val),
            ExprNode::Uint64(val) => write!(f, "{}u64", val),
            ExprNode::Float32(val) => write!(f, "{}f32", val),
            ExprNode::Float64(val) => write!(f, "{}f64", val),
            ExprNode::Var(name) => write!(f, "{}", name),
            ExprNode::Slice(elements) => {
                let elements_str = elements.iter()
                    .map(|e| e.value.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "[{}]", elements_str)
            },
            ExprNode::Index { slice, index } => write!(f, "{}[{}]", slice.value, index.value),
            ExprNode::Unary { op, operand } => write!(f, "({}{})", op, operand.value),
            ExprNode::Binary { op, left, right } => write!(f, "({} {} {})", left.value, op, right.value),
            ExprNode::Call { func, args } => {
                let args_str = args.iter()
                    .map(|arg| arg.value.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{}({})", func.value, args_str)
            }
        }
    }
}

pub type Expr<'a> = Metadata<ExprNode<'a>>;

#[derive(Clone, Debug)]
pub enum StmtNode<'a> {
    Expr(Expr<'a>),
    Block(Vec<Stmt<'a>>),

    Declare {
        name: &'a str,
        ty: Type<'a>,
        value: Expr<'a>,
    },
    Assign {
        left: Expr<'a>,
        value: Expr<'a>,
    },
    If {
        condition: Expr<'a>,
        then_branch: Box<Stmt<'a>>,
        else_branch: Option<Box<Stmt<'a>>>,
    },
    While {
        condition: Expr<'a>,
        body: Box<Stmt<'a>>,
    },

    // TODO add label? (e.g. `continue 'label;`)
    Continue,
    Break,
    Return(Expr<'a>),
}

impl<'a> Display for StmtNode<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            StmtNode::Expr(expr) => write!(f, "{}", expr.value),
            StmtNode::Block(stmts) => {
                let stmts_str = stmts.iter().map(|stmt| format!("    {}\n", stmt.value)).collect::<String>();
                write!(f, "{{\n{}}}", stmts_str)
            },
            StmtNode::Declare { name, ty, value } => write!(f, "let {}: {} = {}", name, ty, value.value),
            StmtNode::Assign { left, value } => write!(f, "{} = {}", left.value, value.value),
            StmtNode::If { condition, then_branch, else_branch } => {
                let else_str = if let Some(else_branch) = else_branch {
                    format!(" else {}", else_branch.value)
                } else {
                    String::new()
                };
                write!(f, "if ({}) {}{}", condition.value, then_branch.value, else_str)
            },
            StmtNode::While { condition, body } => write!(f, "while ({}) {}", condition.value, body.value),

            StmtNode::Continue => write!(f, "continue"),
            StmtNode::Break => write!(f, "break"),
            StmtNode::Return(expr) => write!(f, "return {}", expr.value),
        }
    }
}

pub type Stmt<'a> = Metadata<StmtNode<'a>>;

#[derive(Clone, Debug)]
pub struct AttributeNode<'a> {
    pub name: &'a str,
    pub value: Option<String>,
}

impl<'a> AttributeNode<'a> {
    pub fn new(name: &'a str, value: Option<String>) -> Self {
        Self { name, value }
    }

    pub fn is_true(&self, name: &'a str) -> bool {
        self.name == name && self.value.is_some() && self.value.as_deref() == Some("true")
    }

    pub fn is_false(&self, name: &'a str) -> bool {
        self.name == name && self.value.is_some() && self.value.as_deref() == Some("false")
    }
}

impl<'a> Display for AttributeNode<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if let Some(value) = &self.value {
            write!(f, "@{}({})", self.name, value)
        } else {
            write!(f, "@{}", self.name)
        }
    }
}

pub type Attribute<'a> = Metadata<AttributeNode<'a>>;

#[derive(Clone, Debug)]
pub enum TopLevelNode<'a> {
    Function {
        name: &'a str,
        attributes: Vec<Attribute<'a>>,
        params: Vec<(&'a str, Type<'a>)>,
        return_type: Type<'a>,
        body: Vec<Stmt<'a>>,
    },
    Extern {
        name: &'a str,
        attributes: Vec<Attribute<'a>>,
        params: Vec<(&'a str, Type<'a>)>,
        return_type: Type<'a>,
    },
}

impl<'a> Display for TopLevelNode<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TopLevelNode::Function { name, attributes, params, return_type, body } => {
                let attrs_str = if attributes.is_empty() {
                    String::new()
                } else {
                    attributes.iter().map(|attr| attr.value.to_string()).collect::<Vec<_>>().join("\n") + "\n"
                };
                let params_str = params.iter().map(|(name, ty)| format!("{}: {}", name, ty)).collect::<Vec<_>>().join(", ");
                let body_str = body.iter().map(|stmt| format!("    {}\n", stmt.value)).collect::<String>();

                write!(f, "{}proc {}({}) {} {{\n{}}}", attrs_str, name, params_str, return_type, body_str)
            },
            TopLevelNode::Extern { name, attributes, params, return_type } => {
                let attrs_str = if attributes.is_empty() {
                    String::new()
                } else {
                    attributes.iter().map(|attr| attr.value.to_string()).collect::<Vec<_>>().join("\n") + "\n"
                };
                let params_str = params.iter().map(|(name, ty)| format!("{}: {}", name, ty)).collect::<Vec<_>>().join(", ");

                write!(f, "{}extern {}({}) {};", attrs_str, name, params_str, return_type)
            },
        }
    }
}

pub type TopLevel<'a> = Metadata<TopLevelNode<'a>>;