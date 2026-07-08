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

/// A resolved value binding: which specific declaration a `Var` use refers to.
/// Produced by name resolution (in the typechecker) and consumed by MIL lowering
/// to key each variable's storage slot. Locals are identified by their `Declare`
/// statement's node id — globally unique, so two same-named locals in different
/// scopes (shadowing) never collide. Params are identified by name, which is
/// unique within a single function's parameter list.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Binding<'a> {
    Local(usize),
    Param(&'a str),
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
    Str(&'a str),
    Var(&'a str),
    BinaryOp(BinaryOp),
    UnaryOp(UnaryOp),

    LParen, RParen,
    LBrace, RBrace,
    LBracket, RBracket,

    Dot, Comma, Semicolon,
    Colon, ColonColon, Assign, At,

    Let, If, Else, Return,
    While, Break, Continue,
    Proc, Extern, Const, Struct,
    Import,
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
            Token::Str(s)       => write!(f, "\"{:?}\"", s),
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
            Token::ColonColon   => write!(f, "::"),
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
            Token::Const        => write!(f, "const"),
            Token::Struct       => write!(f, "struct"),
            Token::Import       => write!(f, "import"),
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

/// A compile-time constant appearing in a type position — the `N` in `[T; N]` or
/// `simd<T, N>`. Either a concrete literal or an unresolved const generic
/// parameter. Like [`Type::Param`], a `Param` is abstract and must never survive
/// to codegen; monomorphization substitutes it away. Use [`ConstVal::expect_lit`]
/// at code-gen sites to assert that.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum ConstVal<'a> {
    Lit(usize),
    /// An unresolved const generic parameter, e.g. the `N` in `[T; N]`. Produced
    /// by the parser; monomorphization substitutes it with a `Lit`.
    Param(&'a str),
}

impl<'a> ConstVal<'a> {
    /// The concrete literal value. Panics if a const param survived past
    /// monomorphization — mirrors the `Type::Param` "must not reach codegen"
    /// contract, so a bug surfaces loudly rather than miscompiling.
    pub fn expect_lit(&self) -> usize {
        match self {
            ConstVal::Lit(n) => *n,
            ConstVal::Param(name) => panic!("const param '{name}' survived to codegen"),
        }
    }
}

impl<'a> Display for ConstVal<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ConstVal::Lit(n) => write!(f, "{}", n),
            ConstVal::Param(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Type<'a> {
    Void, Bool,
    Int8, Int32, Int64,
    Uint8, Uint32, Uint64,
    Float32, Float64,
    Function {
        params: Vec<Type<'a>>,
        return_type: Box<Type<'a>>,
    },
    Pointer(Box<Self>),
    /// Fixed-size array type, e.g., `[T; N]`
    Array(Box<Self>, ConstVal<'a>),
    /// Slice type, e.g., `[T]`
    Slice(Box<Self>),
    Simd(Box<Self>, ConstVal<'a>),
    /// Static string slice (like `&'static str` in Rust)
    Str,
    Struct(&'a str),
    /// A generic type parameter, e.g. `T`.
    /// Produced during typechecking by resolving a `Struct(name)` where the name
    /// matches a type param in scope. It is abstract and must never survive to
    /// codegen stage.
    Param(&'a str),
}

impl<'a> Type<'a> {
    pub fn is_numeric(&self) -> bool {
        matches!(self,
            Type::Int8 | Type::Int32 | Type::Int64
            | Type::Uint8 | Type::Uint32 | Type::Uint64
            | Type::Float32 | Type::Float64)
    }

    pub fn is_integer(&self) -> bool {
        matches!(self,
            Type::Int8 | Type::Int32 | Type::Int64
            | Type::Uint8 | Type::Uint32 | Type::Uint64)
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
            Uint8 => write!(f, "u8"), Uint32 => write!(f, "u32"), Uint64 => write!(f, "u64"),
            Int8 => write!(f, "i8"), Int32 => write!(f, "i32"), Int64 => write!(f, "i64"),
            Float32 => write!(f, "f32"), Float64 => write!(f, "f64"),
            Function { params, return_type } => {
                let params_str = params.iter().map(|p| p.to_string()).collect::<Vec<_>>().join(", ");
                write!(f, "proc({}) {}", params_str, return_type)
            },
            Pointer(inner) => write!(f, "*{}", inner),
            Array(inner, size) => write!(f, "[{}; {}]", inner, size),
            Slice(inner) => write!(f, "[{}]", inner),
            Simd(inner, size) => write!(f, "simd[{}, {}]", inner, size),
            Str => write!(f, "str"),
            Struct(name) => write!(f, "{}", name),
            Param(name) => write!(f, "{}", name),
        }
    }
}

#[derive(Clone, Debug)]
pub enum ExprNode<'a> {
    Bool(bool),
    Int32(i32), Int64(i64),
    Uint32(u32), Uint64(u64),
    Float32(f32), Float64(f64),
    /// String literal `"..."`. Holds the raw source text between the quotes.
    /// escape sequences are resolved later, during MIL lowering, e.g.
    /// `\n` ([\, n]) becomes a single byte 0x0A
    Str(&'a str),
    Var(&'a str),
    Slice(Vec<Expr<'a>>),

    Struct {
        name: &'a str,
        fields: Vec<(&'a str, Expr<'a>)>,
    },
    Access {
        base: Box<Expr<'a>>,
        field: &'a str,
    },

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
        /// Turbofish type/const arguments, e.g. `::<f32, 4>`. Empty for ordinary
        /// calls.
        type_args: Vec<GenericArg<'a>>,
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
            ExprNode::Str(s) => write!(f, "{:?}", s),
            ExprNode::Var(name) => write!(f, "{}", name),

            ExprNode::Slice(elements) => {
                let elements_str = elements.iter()
                    .map(|e| e.value.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "[{}]", elements_str)
            },
            ExprNode::Access { base, field } => write!(f, "{}.{}", base.value, field),

            ExprNode::Struct { name, fields } => {
                let fields_str = fields.iter()
                    .map(|(field_name, field_value)| format!("{}: {}", field_name, field_value.value))
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{} {{ {} }}", name, fields_str)
            },

            ExprNode::Index { slice, index } => write!(f, "{}[{}]", slice.value, index.value),
            ExprNode::Unary { op, operand } => write!(f, "({}{})", op, operand.value),
            ExprNode::Binary { op, left, right } => write!(f, "({} {} {})", left.value, op, right.value),
            ExprNode::Call { func, type_args, args } => {
                let turbofish = if type_args.is_empty() {
                    String::new()
                } else {
                    format!("::<{}>", type_args.iter().map(|t| t.to_string()).collect::<Vec<_>>().join(", "))
                };
                let args_str = args.iter()
                    .map(|arg| arg.value.to_string())
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "{}{}({})", func.value, turbofish, args_str)
            }
        }
    }
}

pub type Expr<'a> = Metadata<ExprNode<'a>>;

/// A generic parameter declared in a function's generic list, e.g. the `T` and
/// `const N: u32` in `proc foo<T, const N: u32>(...)`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum GenericParam<'a> {
    /// A type parameter, e.g. `T`.
    Type(&'a str),
    /// A compile-time constant parameter, e.g. `const N: u32`.
    Const(&'a str, Type<'a>),
}

impl<'a> Display for GenericParam<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            GenericParam::Type(name) => write!(f, "{}", name),
            GenericParam::Const(name, ty) => write!(f, "const {}: {}", name, ty),
        }
    }
}

/// A generic argument supplied at a call site via turbofish, e.g. the `f32` and
/// `4` in `simd_splat::<f32, 4>(v)`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum GenericArg<'a> {
    /// A type argument, e.g. `f32`, `[i32; 4]`, `*T`.
    Type(Type<'a>),
    /// A compile-time constant argument (literal only for now), e.g. `4`.
    Const(i64),
}

impl<'a> Display for GenericArg<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            GenericArg::Type(ty) => write!(f, "{}", ty),
            GenericArg::Const(n) => write!(f, "{}", n),
        }
    }
}

/// Renders a generic list as `<T, const N: u32>`, or the empty string when there
/// are no params.
fn fmt_generics(generics: &[GenericParam<'_>]) -> String {
    if generics.is_empty() {
        String::new()
    } else {
        format!("<{}>", generics.iter().map(|g| g.to_string()).collect::<Vec<_>>().join(", "))
    }
}

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
        generics: Vec<GenericParam<'a>>,
        params: Vec<(&'a str, Type<'a>)>,
        return_type: Type<'a>,
        body: Vec<Stmt<'a>>,
    },
    Extern {
        name: &'a str,
        attributes: Vec<Attribute<'a>>,
        generics: Vec<GenericParam<'a>>,
        params: Vec<(&'a str, Type<'a>)>,
        return_type: Type<'a>,
    },

    Struct {
        name: &'a str,
        attributes: Vec<Attribute<'a>>,
        fields: Vec<(&'a str, Type<'a>)>,
    },

    /// A module-level constant, e.g. `const SR: f32 = 48000.0;`. The initializer
    /// must be a compile-time constant (literal or const struct literal); it is
    /// emitted as an LLVM `constant` global. `@export` gives it external linkage
    /// so a host can look the symbol up (see the CLAP `clap_entry` use case).
    Global {
        name: &'a str,
        attributes: Vec<Attribute<'a>>,
        ty: Type<'a>,
        value: Expr<'a>,
    },
}

impl<'a> Display for TopLevelNode<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TopLevelNode::Function { name, attributes, generics, params, return_type, body } => {
                let attrs_str = if attributes.is_empty() {
                    String::new()
                } else {
                    attributes.iter().map(|attr| attr.value.to_string()).collect::<Vec<_>>().join("\n") + "\n"
                };
                let generics_str = fmt_generics(generics);
                let params_str = params.iter().map(|(name, ty)| format!("{}: {}", name, ty)).collect::<Vec<_>>().join(", ");
                let body_str = body.iter().map(|stmt| format!("    {}\n", stmt.value)).collect::<String>();

                write!(f, "{}proc {}{}({}) {} {{\n{}}}", attrs_str, name, generics_str, params_str, return_type, body_str)
            },
            TopLevelNode::Extern { name, attributes, generics, params, return_type } => {
                let attrs_str = if attributes.is_empty() {
                    String::new()
                } else {
                    attributes.iter().map(|attr| attr.value.to_string()).collect::<Vec<_>>().join("\n") + "\n"
                };
                let generics_str = fmt_generics(generics);
                let params_str = params.iter().map(|(name, ty)| format!("{}: {}", name, ty)).collect::<Vec<_>>().join(", ");

                write!(f, "{}extern {}{}({}) {};", attrs_str, name, generics_str, params_str, return_type)
            },
            TopLevelNode::Struct { name, attributes, fields } => {
                let attrs_str = if attributes.is_empty() {
                    String::new()
                } else {
                    attributes.iter().map(|attr| attr.value.to_string()).collect::<Vec<_>>().join("\n") + "\n"
                };
                let fields_str = fields.iter().map(|(name, ty)| format!("    {}: {},\n", name, ty)).collect::<String>();

                write!(f, "{}struct {} {{\n{}}}", attrs_str, name, fields_str)
            },
            TopLevelNode::Global { name, attributes, ty, value } => {
                let attrs_str = if attributes.is_empty() {
                    String::new()
                } else {
                    attributes.iter().map(|attr| attr.value.to_string()).collect::<Vec<_>>().join("\n") + "\n"
                };

                write!(f, "{}const {}: {} = {};", attrs_str, name, ty, value.value)
            },
        }
    }
}

pub type TopLevel<'a> = Metadata<TopLevelNode<'a>>;

/// a module import, e.g. `import std/math` or `import std/math { sinf, cosf }`
///
/// kept out of `TopLevelNode` so the later stages (typecheck, mono, mil, ...)
/// never see imports: the module resolver eats every import, mangles + merges
/// the referenced modules, and hands those stages one flat program of concrete
/// items with no imports left
#[derive(Clone, Debug)]
pub struct Import<'a> {
    pub span: Span,
    /// path segments as written, e.g. `["std", "math"]` or `["utils", "foo"]`
    pub path: Vec<&'a str>,
    /// `None` = whole-module import (`import std/math`): every public symbol
    /// visible unqualified. `Some(list)` = selective (`import std/math { sinf }`):
    /// only those symbols, and only qualified under the last path segment
    /// (`math::sinf`)
    pub symbols: Option<Vec<&'a str>>,
}