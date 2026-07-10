use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::front::ast::*;
use crate::intrinsics::Intrinsic;

// Unique SSA temps that will never be reassigned
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Register(pub usize);

impl Display for Register {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "%t{}", self.0)
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Reg(Register),
    Const(Const),
}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Reg(r) => write!(f, "{}", r),
            Value::Const(c) => write!(f, "{}", c),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Const {
    Undef, // for uninitialized values (e.g. insertvalue with undef)
    Bool(bool),
    Int8(i8),
    Int32(i32),
    Int64(i64),
    Uint8(u8),
    Uint32(u32),
    Uint64(u64),
    Float32(f32),
    Float64(f64),
    /// Address of a module-level string blob, emitted as `@.str.{0}`.
    /// The index refers into `Module::strings`.
    GlobalStr(usize),
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Const::Undef => write!(f, "undef"),
            Const::Bool(b) => write!(f, "{}", b),
            Const::Int8(n) => write!(f, "{}", n),
            Const::Int32(n) => write!(f, "{}", n),
            Const::Int64(n) => write!(f, "{}", n),
            Const::Uint8(n) => write!(f, "{}", n),
            Const::Uint32(n) => write!(f, "{}", n),
            Const::Uint64(n) => write!(f, "{}", n),
            Const::Float32(n)=> write!(f, "{:?}", n),
            Const::Float64(n)=> write!(f, "{:?}", n),
            Const::GlobalStr(n) => write!(f, "@.str.{}", n),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct BlockId(pub usize);

impl Display for BlockId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "b{}", self.0)
    }
}

/// The target of a call: either a statically-known function symbol (`@name`) or a
/// function-pointer value computed at runtime (`%reg`).
#[derive(Clone, Debug)]
pub enum Callee<'a> {
    Direct(&'a str),
    Indirect(Value),
}

#[derive(Clone, Debug)]
pub enum Inst<'a> {
    Comment(String),

    // %dst = <unary op> %val
    Unary { dst: Register, op: UnaryOp, val: Value, ty: Type<'a> },
    // %dst = <binary op> %lhs, %rhs
    Binary { dst: Register, op: BinaryOp, ty: Type<'a>, lhs: Value, rhs: Value },

    // %dst = call callee(args...)
    Call {
        dst: Option<Register>, // None if return type is void
        callee: Callee<'a>,
        args: Vec<(Value, Type<'a>)>,
        return_type: Type<'a>,
        // when the callee returns a struct, the result is passed back through
        // this caller-allocated slot (sret) instead of a return value, and the
        // call itself returns void. Some((slot, struct_name))
        sret: Option<(Register, &'a str)>,
    },

    // %dst = getelemptr %slice, %index (for slice indexing)
    // using Register here because slice will be { ptr, len } fat pointer struct
    Index { dst: Register, slice: Register, index: Value, element_ty: Type<'a> },
    // %dst = insertvalue {{ ptr, i32 }} %elem, ty %val, %index
    InsertValue { dst: Register, elem: Value, ty: Type<'a>, val: Value, index: usize },
    // %dst = extractvalue %val, index (for extracting from fat pointer struct, e.g. data pointer or length)
    ExtractValue { dst: Register, val: Value, index: usize },
    // %dst = getelementptr %Name, ptr %base, i32 0, i32 <field_index>
    FieldPtr { dst: Register, struct_name: &'a str, base: Register, field_index: usize },
    // %dst = ptr to a module-level global @name (a zero-offset gep, so %dst == @name)
    GlobalPtr { dst: Register, name: &'a str },

    // %dst = alloca ty (for mutable locals, if needed)
    Alloca { dst: Register, ty: Type<'a>, align: Option<usize> },
    // store ty %val, ptr %ptr
    Store { ptr: Register, val: Value, ty: Type<'a>, align: Option<usize> },
    // %dst = load ty %ptr
    Load { dst: Register, ptr: Register, ty: Type<'a>, align: Option<usize> },

    // since Type doesn't encode length for slices and I'm too lazy to change it
    // %dst = alloca [length x ty]
    AllocaArray { dst: Register, ty: Type<'a>, length: usize },
    // %dst = getelemptr [length X ty] %array, %index
    IndexArray { dst: Register, ty: Type<'a>, length: usize, array: Register, index: usize },
    // %dst = alloca %struct_ty, align N
    AllocaStruct { dst: Register, name: &'a str, align: Option<usize> },

    // Intrinsic/SIMD related instructions
    // %dst = ptrtoint of `getelementptr ty, ptr null, i32 1` -> size of `ty` in bytes (u64)
    Sizeof { dst: Register, ty: Type<'a> },
    Extend { dst: Register, val: Value, from_ty: Type<'a>, to_ty: Type<'a> },
    // %v0 = insertelement ty(simd) undef, %value, 0
    Splat { dst: Register, val: Value, ty: Type<'a>, size: usize },
    // %dst = shufflevector ty(simd) %v0, ty(simd) %v1, <size x i32> <mask>
    Shuffle { dst: Register, value_size: usize, v0: Value, v1: Value, ty: Type<'a>, size: usize, mask: Vec<usize> },
}

#[derive(Clone, Debug)]
pub enum Terminator<'a> {
    // return %val / return void
    Return(Option<(Value, Type<'a>)>),

    // br label %block
    Jump(BlockId),

    // br i1 %cond, label %then, label %else
    Branch {
        cond: Register,
        then_block: BlockId,
        else_block: BlockId,
    },

    // marks a provably unreachable point (e.g. the merge block after an if/else
    // whose branches both return). only emitted where the typechecker has already
    // proven control can't actually reach it.
    Unreachable,
}

impl <'a> Display for Terminator<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Terminator::Return(Some((val, ty))) => write!(f, "return {} : {}", val, ty),
            Terminator::Return(None) => write!(f, "return"),
            Terminator::Jump(block) => write!(f, "br label {}", block),
            Terminator::Branch { cond, then_block, else_block } => {
                write!(f, "br i1 {}, label {}, label {}", cond, then_block, else_block)
            }
            Terminator::Unreachable => write!(f, "unreachable"),
        }
    }
}

#[derive(Clone, Debug)]
pub struct BasicBlock<'a> {
    pub id: BlockId,
    pub instructions: Vec<Inst<'a>>,
    pub terminator: Option<Terminator<'a>>,
}

#[derive(Clone, Debug)]
pub struct ExternDecl<'a> {
    pub name: &'a str,
    pub attributes: Vec<Attribute<'a>>,
    pub params: Vec<Type<'a>>,
    pub return_type: Type<'a>,
}

#[derive(Clone, Debug)]
pub struct Function<'a> {
    pub name: &'a str,
    pub attributes: Vec<Attribute<'a>>,
    pub params: Vec<(Register, Type<'a>)>,
    pub return_type: Type<'a>,
    pub blocks: Vec<BasicBlock<'a>>, // blocks[0] is always the entry
    // for struct-returning functions: the hidden sret out-pointer parameter
    // (its register and the struct name). The function returns void in LLVM
    pub sret: Option<(Register, &'a str)>,
}

/// The constant value initializing a module-level global. A restricted subset of
/// expressions that map onto an LLVM constant aggregate — no code runs to produce
/// it.
#[derive(Clone, Debug)]
pub enum ConstInit<'a> {
    Scalar(Const),
    /// A constant struct aggregate: one `(field type, field constant)` per field,
    /// in declaration order. Nested structs recurse.
    Struct(Vec<(Type<'a>, ConstInit<'a>)>),
    /// The address of a top-level function, emitted as `@name` — a link-time
    /// constant `ptr`. Used for function-pointer fields (e.g. CLAP's `clap_entry`).
    FnAddr(&'a str),
}

#[derive(Clone, Debug)]
pub struct Global<'a> {
    pub name: &'a str,
    /// `@export`: give the symbol external (dllexport) linkage so a host can look
    /// it up, instead of the default `internal`.
    pub export: bool,
    pub ty: Type<'a>,
    pub init: ConstInit<'a>,
}

#[derive(Clone, Debug)]
pub struct Module<'a> {
    pub functions: Vec<Function<'a>>,
    pub externs: Vec<ExternDecl<'a>>,
    /// Struct definitions, in declaration order: name -> ordered (field name, field type)
    pub structs: Vec<(&'a str, Vec<(&'a str, Type<'a>)>)>,
    /// Module-level constants, emitted as LLVM `constant` globals.
    pub globals: Vec<Global<'a>>,
    /// Interned, escape-resolved string-literal blobs. Index `i` is emitted as
    /// the global `@.str.{i}` and referenced by `Const::GlobalStr(i)`.
    pub strings: Vec<Vec<u8>>,
}

#[derive(Clone, Debug)]
pub struct LoopTargets {
    pub continue_block: BlockId, // where to jump for `continue`
    pub break_block: BlockId,    // where to jump for `break`
}

#[derive(Clone, Debug)]
pub struct LowerCtx<'a> {
    pub reg_counter: usize,
    pub block_counter: usize,
    pub current_block: BlockId,
    pub blocks: Vec<BasicBlock<'a>>,
    pub loop_stack: Vec<LoopTargets>,

    /// Storage slot per param/local, keyed by resolved binding identity (not by
    /// name) so shadowed same-named locals get distinct slots. Cleared per fn.
    pub env: HashMap<Binding<'a>, (Register, Type<'a>)>,
    /// Module-level globals in scope: final emitted name -> type. Referenced as
    /// bare vars, distinct from `env` (locals/params), which shadow these.
    pub globals: HashMap<&'a str, Type<'a>>,
    pub structs: HashMap<&'a str, Vec<(&'a str, Type<'a>)>>, // from typecheck
    pub node_types: HashMap<usize, Type<'a>>, // from typecheck
    /// Name resolution from typecheck: `Var` node id -> its param/local binding.
    /// Absent for globals/functions, which resolve via `globals` / direct calls.
    pub resolved: HashMap<usize, Binding<'a>>, // from typecheck
    pub current_return_type: Type<'a>,        // return type of the function being lowered
    pub sret_param: Option<Register>,         // out-pointer slot, if the current fn returns a struct
    /// When set, the next struct/array literal lowered fills this pre-allocated
    /// slot instead of allocating a fresh one. Used to hoist a loop-local's slot
    /// to the entry block (see `collect_locals`) so it isn't re-alloca'd every
    /// iteration. The literal lowering `take()`s it, so it applies to exactly the
    /// outermost literal and never leaks into nested field/element literals.
    pub store_target: Option<Register>,
    pub strings: Vec<Vec<u8>>,                // interned string-literal blobs (-> @.str.N)
}

impl<'a> LowerCtx<'a> {
    pub fn fresh_reg(&mut self) -> Register {
        let r = Register(self.reg_counter);
        self.reg_counter += 1;
        r
    }

    pub fn fresh_block(&mut self) -> BlockId {
        let b = BlockId(self.block_counter);
        self.block_counter += 1;
        self.blocks.push(BasicBlock {
            id: b,
            instructions: vec![],
            terminator: None,
        });
        b
    }

    /// Intern a string literal's raw source text, resolving escape sequences,
    /// and return `(index_into_strings, byte_length)`. Identical blobs are
    /// deduplicated so repeated literals share one global.
    pub fn intern_string(&mut self, raw: &str) -> (usize, usize) {
        let bytes = resolve_escapes(raw);
        let len = bytes.len();
        if let Some(i) = self.strings.iter().position(|b| *b == bytes) {
            return (i, len);
        }
        let idx = self.strings.len();
        self.strings.push(bytes);
        (idx, len)
    }

    pub fn emit(&mut self, inst: Inst<'a>) {
        let block = self.blocks.iter_mut().find(|b| b.id == self.current_block).unwrap();
        block.instructions.push(inst);
    }

    pub fn terminate(&mut self, term: Terminator<'a>) {
        let block = self.blocks.iter_mut().find(|b| b.id == self.current_block).unwrap();
        block.terminator = Some(term);
    }
}

/// Resolve the escape sequences in a string literal's raw source text into the
/// actual bytes. Recognizes `\n \t \r \0 \\ \"`; an unknown escape `\x` keeps
/// the char `x` verbatim. The lexer guarantees a backslash is always followed
/// by at least one char, so a trailing lone backslash cannot occur.
fn resolve_escapes(raw: &str) -> Vec<u8> {
    let mut out = Vec::with_capacity(raw.len());
    let mut chars = raw.chars();
    while let Some(c) = chars.next() {
        if c != '\\' {
            let mut buf = [0u8; 4];
            out.extend_from_slice(c.encode_utf8(&mut buf).as_bytes());
            continue;
        }
        match chars.next() {
            Some('n') => out.push(b'\n'),
            Some('t') => out.push(b'\t'),
            Some('r') => out.push(b'\r'),
            Some('0') => out.push(0),
            Some('\\') => out.push(b'\\'),
            Some('"') => out.push(b'"'),
            Some(other) => {
                let mut buf = [0u8; 4];
                out.extend_from_slice(other.encode_utf8(&mut buf).as_bytes());
            }
            None => {} // should be unreachable per lexer invariant
        }
    }
    out
}

/// Materialize an array->slice coercion: build a { ptr, i32 N } fat pointer from
/// an array value. Returns the original value unchanged if no coercion applies.
fn coerce<'a>(cx: &mut LowerCtx<'a>, val: Value, from_ty: &Type<'a>, to_ty: &Type<'a>) -> Value {
    match (from_ty, to_ty) {
        (Type::Array(inner, len), Type::Slice(_)) => {
            let fat0 = cx.fresh_reg();
            cx.emit(Inst::InsertValue {
                dst: fat0,
                elem: Value::Const(Const::Undef),
                ty: Type::Pointer(Box::new(*inner.clone())),
                val,
                index: 0,
            });
            let fat1 = cx.fresh_reg();
            cx.emit(Inst::InsertValue {
                dst: fat1,
                elem: Value::Reg(fat0),
                ty: Type::Int32,
                val: Value::Const(Const::Int32(len.expect_lit() as i32)),
                index: 1,
            });
            Value::Reg(fat1)
        }
        _ => val,
    }
}

/// Pulls a concrete type from a turbofish type argument. Type params are skipped
/// in lowering (generic functions aren't monomorphized), so these are concrete.
fn ta_type<'a>(type_args: &[GenericArg<'a>], i: usize) -> Type<'a> {
    match &type_args[i] {
        GenericArg::Type(t) => t.clone(),
        _ => unreachable!("expected a type argument at position {i}"),
    }
}

/// Pulls a const from a turbofish const argument.
fn ta_const(type_args: &[GenericArg<'_>], i: usize) -> usize {
    match &type_args[i] {
        GenericArg::Const(cv) => cv.expect_lit(),
        _ => unreachable!("expected a const argument at position {i}"),
    }
}

fn lower_intrinsic<'a>(
    cx: &mut LowerCtx<'a>,
    intrinsic: Intrinsic,
    type_args: &[GenericArg<'a>],
    args: &[Expr<'a>],
) -> Value {
    match intrinsic {
        Intrinsic::Len => {
            let slice_val = lower_expr(cx, &args[0]);
            let dst = cx.fresh_reg();
            cx.emit(Inst::ExtractValue { dst, val: slice_val, index: 1 });
            Value::Reg(dst)
        }
        Intrinsic::NumericalCast => {
            // numerical_cast::<T>(value)
            let val = lower_expr(cx, &args[0]);
            let from_ty = cx.node_types[&args[0].id].clone();
            let to_ty = ta_type(type_args, 0);
            let dst = cx.fresh_reg();
            cx.emit(Inst::Extend { dst, val, from_ty, to_ty });
            Value::Reg(dst)
        }
        Intrinsic::Sizeof => {
            // sizeof::<T>(); the type is taken directly from the turbofish.
            let ty = ta_type(type_args, 0);
            let dst = cx.fresh_reg();
            cx.emit(Inst::Sizeof { dst, ty });
            Value::Reg(dst)
        }
        Intrinsic::PtrCast => {
            // ptr_cast::<*T>(p) is a no-op under opaque pointers: the value is
            // already a `ptr`, only its static pointee type changes. Pass it
            // through; the result's type is tracked in node_types by typecheck.
            lower_expr(cx, &args[0])
        }
        Intrinsic::SimdSplat => {
            let ty = ta_type(type_args, 0);
            let size = ta_const(type_args, 1);
            let value_val = lower_expr(cx, &args[0]);

            let v0 = cx.fresh_reg();
            cx.emit(Inst::Splat { dst: v0, val: value_val, ty: ty.clone(), size });
            let dst = cx.fresh_reg();
            cx.emit(Inst::Shuffle {
                dst,
                value_size: size,
                v0: Value::Reg(v0),
                v1: Value::Const(Const::Undef),
                ty, size,
                mask: vec![0; size],
            });
            Value::Reg(dst)
        }
        Intrinsic::SimdLoad => {
            // simd_load::<T, N>(slice, offset) -> simd[T, N]
            let ty = ta_type(type_args, 0);
            let size = ta_const(type_args, 1);
            let slice_val = lower_expr(cx, &args[0]);
            let offset_val = lower_expr(cx, &args[1]);

            cx.emit(Inst::Comment(format!("simd_load")));
            // extract the data pointer from the fat pointer struct, or use directly if it's already a pointer
            let data_ptr = match cx.node_types[&args[0].id] {
                Type::Slice(_) => {
                    let extracted = cx.fresh_reg();
                    cx.emit(Inst::ExtractValue { dst: extracted, val: slice_val.clone(), index: 0 });
                    extracted
                }
                Type::Pointer(_) | Type::Array(_, _) => {
                    // already a raw pointer, use it directly
                    match slice_val {
                        Value::Reg(r) => r,
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            };
            // get the element pointer with the offset
            let elem_ptr = cx.fresh_reg();
            cx.emit(Inst::Index { dst: elem_ptr, slice: data_ptr, index: offset_val, element_ty: ty.clone() });
            // load the SIMD vector from the element pointer
            let dst = cx.fresh_reg();
            cx.emit(Inst::Load { dst, ptr: elem_ptr, ty: Type::Simd(Box::new(ty), ConstVal::Lit(size)), align: None });
            Value::Reg(dst)
        }
        Intrinsic::SimdStore => {
            // simd_store::<T, N>(slice, offset, value) -> ()
            let ty = ta_type(type_args, 0);
            let size = ta_const(type_args, 1);
            let slice_val = lower_expr(cx, &args[0]);
            let offset_val = lower_expr(cx, &args[1]);
            let value_val = lower_expr(cx, &args[2]);

            // like simd_load
            let data_ptr = match cx.node_types[&args[0].id] {
                Type::Slice(_) => {
                    let extracted = cx.fresh_reg();
                    cx.emit(Inst::ExtractValue { dst: extracted, val: slice_val.clone(), index: 0 });
                    extracted
                }
                Type::Pointer(_) | Type::Array(_, _) => {
                    match slice_val {
                        Value::Reg(r) => r,
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            };
            // get the element pointer with the offset
            let elem_ptr = cx.fresh_reg();
            cx.emit(Inst::Comment(format!("simd_store")));
            cx.emit(Inst::Index { dst: elem_ptr, slice: data_ptr, index: offset_val, element_ty: ty.clone() });
            // store the SIMD vector to the element pointer
            cx.emit(Inst::Store { ptr: elem_ptr, val: value_val, ty: Type::Simd(Box::new(ty), ConstVal::Lit(size)), align: None });
            Value::Const(Const::Undef) // placeholder since void return
        }
        Intrinsic::SimdConcat => {
            let ty = ta_type(type_args, 0);
            let size = ta_const(type_args, 1);
            let value1_val = lower_expr(cx, &args[0]);
            let value2_val = lower_expr(cx, &args[1]);
            let dst = cx.fresh_reg();
            cx.emit(Inst::Comment(format!("simd_concat({}, {}, ..., ...)", ty, size)));
            cx.emit(Inst::Shuffle {
                dst,
                value_size: match &cx.node_types[&args[0].id] {
                    Type::Simd(_, s) => s.expect_lit(),
                    _ => unreachable!(),
                },
                v0: value1_val,
                v1: value2_val,
                ty,
                size,
                mask: (0..size).collect(),
            });
            Value::Reg(dst)
        }
        Intrinsic::SimdLow
        | Intrinsic::SimdHigh => {
            // simd_low::<T, N>(value) -> simd[T, N]
            let ty = ta_type(type_args, 0);
            let size = ta_const(type_args, 1);
            let value_val = lower_expr(cx, &args[0]);

            let dst = cx.fresh_reg();
            cx.emit(Inst::Comment(format!("{}({}, {}, ...)", intrinsic, ty, size)));
            cx.emit(Inst::Shuffle {
                dst,
                value_size: match &cx.node_types[&args[0].id] {
                    Type::Simd(_, s) => s.expect_lit(),
                    _ => unreachable!(),
                },
                v0: value_val,
                v1: Value::Const(Const::Undef),
                ty,
                size,
                mask: match intrinsic {
                    Intrinsic::SimdLow => (0..size).collect(),
                    Intrinsic::SimdHigh => (size..size*2).collect(),
                    _ => unreachable!(),
                },
            });
            Value::Reg(dst)
        }
    }
}

fn lower_expr<'a>(cx: &mut LowerCtx<'a>, expr: &Expr<'a>) -> Value {
    match &expr.value {
        ExprNode::Bool(b)    => Value::Const(Const::Bool(*b)),
        ExprNode::Int8(n)    => Value::Const(Const::Int8(*n)),
        ExprNode::Int32(n)   => Value::Const(Const::Int32(*n)),
        ExprNode::Int64(n)   => Value::Const(Const::Int64(*n)),
        ExprNode::Uint8(n)   => Value::Const(Const::Uint8(*n)),
        ExprNode::Uint32(n)  => Value::Const(Const::Uint32(*n)),
        ExprNode::Uint64(n)  => Value::Const(Const::Uint64(*n)),
        ExprNode::Float32(n) => Value::Const(Const::Float32(*n)),
        ExprNode::Float64(n) => Value::Const(Const::Float64(*n)),

        // a string literal is a { ptr, i32 } fat pointer where data field is
        // the address of a read-only global blob and where length is the byte
        // count like how slices are, so len()/extractvalue (should) work uniformly
        ExprNode::Str(s) => {
            let (idx, len) = cx.intern_string(s);
            let fat0 = cx.fresh_reg();
            cx.emit(Inst::InsertValue {
                dst: fat0,
                elem: Value::Const(Const::Undef),
                ty: Type::Pointer(Box::new(Type::Int32)), // emits as `ptr`
                val: Value::Const(Const::GlobalStr(idx)),
                index: 0,
            });
            let fat1 = cx.fresh_reg();
            cx.emit(Inst::InsertValue {
                dst: fat1,
                elem: Value::Reg(fat0),
                ty: Type::Int32,
                val: Value::Const(Const::Int32(len as i32)),
                index: 1,
            });
            Value::Reg(fat1)
        }

        ExprNode::Var(name) => {
            // locals/params (env) shadow module-level globals. resolution picked
            // the exact binding for this use, so shadowing is already decided.
            if let Some((reg, ty)) = cx.resolved.get(&expr.id).and_then(|b| cx.env.get(b)).cloned() {
                match ty {
                    // fixed arrays are stored in env as the alloca register itself, not a pointer
                    // to one, so loading would yield the array value — we want the pointer
                    Type::Array(_, _) => Value::Reg(reg),
                    Type::Struct(_) => Value::Reg(reg),
                    _ => {
                        let dst = cx.fresh_reg();
                        cx.emit(Inst::Load { dst, ptr: reg, ty, align: None });
                        Value::Reg(dst)
                    }
                }
            } else if let Some(ty) = cx.globals.get(name).cloned() {
                // reference to a module-level global: its symbol @name is already
                // a pointer to the constant. Materialize that address, then treat
                // it like an env entry — hand back the pointer for aggregates,
                // load the value for scalars.
                let addr = cx.fresh_reg();
                cx.emit(Inst::GlobalPtr { dst: addr, name });
                match ty {
                    Type::Array(_, _) | Type::Struct(_) => Value::Reg(addr),
                    _ => {
                        let dst = cx.fresh_reg();
                        cx.emit(Inst::Load { dst, ptr: addr, ty, align: None });
                        Value::Reg(dst)
                    }
                }
            } else if matches!(cx.node_types.get(&expr.id), Some(Type::Function { .. })) {
                // a bare top-level function used as a value: its symbol @name is a
                // function pointer. Materialize the address (reusing GlobalPtr).
                let dst = cx.fresh_reg();
                cx.emit(Inst::GlobalPtr { dst, name });
                Value::Reg(dst)
            } else {
                panic!("unknown variable '{name}' in MIL lowering");
            }
        }

        ExprNode::Struct { name, fields } => {
            // reuse a hoisted entry-block slot if the caller provided one;
            // otherwise this literal owns a fresh slot. take() so nested field
            // literals don't inherit the target.
            let dst = match cx.store_target.take() {
                Some(slot) => slot,
                None => {
                    let dst = cx.fresh_reg();
                    cx.emit(Inst::AllocaStruct { dst, name, align: None });
                    dst
                }
            };

            for (i, (_field_name, field_expr)) in fields.iter().enumerate() {
                let field_ty = cx.structs[name][i].1.clone();
                let field_val = lower_expr(cx, field_expr);
                let field_ptr = cx.fresh_reg();

                cx.emit(Inst::FieldPtr {
                    dst: field_ptr,
                    struct_name: name,
                    base: dst,
                    field_index: i,
                });
                match field_ty {
                    // a nested struct field is inlined storage, so copy the
                    // source struct's contents into it rather than storing a
                    // pointer (field_val is the source struct's address)
                    Type::Struct(inner) => {
                        let src = match field_val {
                            Value::Reg(r) => r,
                            _ => unreachable!(),
                        };
                        copy_struct(cx, inner, src, field_ptr);
                    }
                    _ => {
                        cx.emit(Inst::Store {
                            ptr: field_ptr,
                            val: field_val,
                            ty: field_ty,
                            align: None,
                        });
                    }
                }
            }
            Value::Reg(dst)
        }
        ExprNode::Access { base, field } => {
            let base_val = lower_expr(cx, base);
            let base_ty = cx.node_types[&base.id].clone();
            // matches the typechecker's one-level auto-deref for `ptr.field`
            let struct_name = match base_ty {
                Type::Struct(name) => name,
                Type::Pointer(inner) => match *inner {
                    Type::Struct(name) => name,
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            let field_index = cx.structs[&struct_name]
                .iter()
                .position(|(fname, _)| fname == field)
                .unwrap();
            let field_ty = cx.structs[&struct_name][field_index].1.clone();

            let field_ptr = cx.fresh_reg();
            cx.emit(Inst::FieldPtr {
                dst: field_ptr,
                struct_name: &struct_name,
                base: match base_val {
                    Value::Reg(r) => r,
                    _ => unreachable!(),
                },
                field_index,
            });
            match field_ty {
                // a struct-typed field is inlined: its value is its address,
                // so hand back the field pointer instead of loading it
                Type::Struct(_) => Value::Reg(field_ptr),
                _ => {
                    let dst = cx.fresh_reg();
                    cx.emit(Inst::Load { dst, ptr: field_ptr, ty: field_ty, align: None });
                    Value::Reg(dst)
                }
            }
        }

        // use fat pointer struct for slices
        ExprNode::Slice(elements) if elements.len() == 0 => {
            todo!()
        },
        ExprNode::Slice(elements) => {
            let (ty, is_fixed) = match cx.node_types[&expr.id].clone() {
                Type::Slice(inner) => (*inner, false),
                Type::Array(inner, _) => (*inner, true),
                _ => unreachable!(),
            };

            // %arr_reg = alloca [N x T]
            // a fixed array bound to a local may reuse a hoisted entry-block slot
            // (see `collect_locals`); the fat-pointer case always allocs backing
            // storage fresh and never carries a store_target.
            let arr_reg = match (is_fixed, cx.store_target.take()) {
                (true, Some(slot)) => slot,
                _ => {
                    let arr_reg = cx.fresh_reg();
                    cx.emit(Inst::AllocaArray {
                        dst: arr_reg,
                        ty: ty.clone(),
                        length: elements.len(),
                    });
                    arr_reg
                }
            };

            // %ptr = gep
            for (index, element) in elements.iter().enumerate() {
                let ptr = cx.fresh_reg();
                cx.emit(Inst::IndexArray {
                    dst: ptr,
                    ty: ty.clone(),
                    length: elements.len(),
                    array: arr_reg,
                    index,
                });

                let elem_val = lower_expr(cx, element);
                cx.emit(Inst::Store { ptr, val: elem_val, ty: ty.clone(), align: None });
            }

            if is_fixed {
                // the alloca register is the value
                Value::Reg(arr_reg)
            } else {
                // construct fat pointer struct { ptr, len }
                // %fat_ptr0 = insertvalue { ptr, i32 } undef, ptr %arr_reg, 0
                // %fat_ptr1 = insertvalue { ptr, i32 } %fat_ptr0, i32 N, 1
                let fat_ptr0 = cx.fresh_reg();
                cx.emit(Inst::InsertValue {
                    dst: fat_ptr0,
                    elem: Value::Const(Const::Undef),
                    ty: Type::Pointer(Box::new(ty.clone())),
                    val: Value::Reg(arr_reg),
                    index: 0,
                });
                let fat_ptr1 = cx.fresh_reg();
                cx.emit(Inst::InsertValue {
                    dst: fat_ptr1,
                    elem: Value::Reg(fat_ptr0),
                    ty: Type::Int32,
                    val: Value::Const(Const::Int32(elements.len() as i32)),
                    index: 1,
                });

                Value::Reg(fat_ptr1)
            }
        }

        // *ptr (where ptr is *T or T[]) = load from ptr, so dst = load ptr
        ExprNode::Unary { op: UnaryOp::Deref, operand } => {
            let ptr_val = lower_expr(cx, operand);
            let ptr_reg = match ptr_val {
                Value::Reg(r) => r,
                _ => unreachable!(),
            };
            let ty = match cx.node_types[&operand.id].clone() {
                Type::Pointer(inner) | Type::Slice(inner) => *inner,
                _ => unreachable!(),
            };
            // dereferencing to a struct keeps it in memory
            // the pointer already points at the struct's storage, so it is the
            // struct value
            if matches!(ty, Type::Struct(_)) {
                return Value::Reg(ptr_reg);
            }
            let dst = cx.fresh_reg();
            cx.emit(Inst::Load { dst, ptr: ptr_reg, ty, align: None });
            Value::Reg(dst)
        }

        ExprNode::Unary { op: UnaryOp::AddrOf, operand } => {
            let ptr = lower_lvalue(cx, operand); // get the address of the operand
            Value::Reg(ptr)
        }

        ExprNode::Unary { op, operand } => {
            let val = lower_expr(cx, operand);
            let ty = cx.node_types[&operand.id].clone();
            let dst = cx.fresh_reg();
            cx.emit(Inst::Unary { dst, op: *op, val, ty });
            Value::Reg(dst)
        }

        // short-circuiting && and ||
        ExprNode::Binary { op: op @ (BinaryOp::And | BinaryOp::Or), left, right } => {
            let lhs = lower_expr(cx, left);
            // branch needs a Register, but lhs might be a bare Const(Bool) (e.g. `true && foo()`)
            let lhs_reg = match lhs {
                Value::Reg(r) => r,
                v => {
                    let r = cx.fresh_reg();
                    cx.emit(Inst::Alloca { dst: r, ty: Type::Bool, align: None });
                    cx.emit(Inst::Store { ptr: r, val: v, ty: Type::Bool, align: None });
                    let s = cx.fresh_reg();
                    cx.emit(Inst::Load { dst: s, ptr: r, ty: Type::Bool, align: None });
                    s
                }
            };

            let result_ptr = cx.fresh_reg();
            cx.emit(Inst::Alloca { dst: result_ptr, ty: Type::Bool, align: None });

            let rhs_block           = cx.fresh_block();
            let short_circuit_block = cx.fresh_block();
            let merge_block         = cx.fresh_block();

            // && : lhs false -> skip rhs, result = false
            // || : lhs true  -> skip rhs, result = true
            cx.terminate(match op {
                BinaryOp::And => Terminator::Branch { cond: lhs_reg, then_block: rhs_block, else_block: short_circuit_block },
                BinaryOp::Or  => Terminator::Branch { cond: lhs_reg, then_block: short_circuit_block, else_block: rhs_block },
                _ => unreachable!(),
            });

            cx.current_block = short_circuit_block;
            cx.emit(Inst::Comment(format!("{} short-circuit", op)));
            cx.emit(Inst::Store {
                ptr: result_ptr,
                val: Value::Const(Const::Bool(matches!(op, BinaryOp::Or))),
                ty: Type::Bool, align: None,
            });
            cx.terminate(Terminator::Jump(merge_block));

            cx.current_block = rhs_block;
            let rhs = lower_expr(cx, right);
            cx.emit(Inst::Store { ptr: result_ptr, val: rhs, ty: Type::Bool, align: None });
            cx.terminate(Terminator::Jump(merge_block));

            cx.current_block = merge_block;
            let dst = cx.fresh_reg();
            cx.emit(Inst::Load { dst, ptr: result_ptr, ty: Type::Bool, align: None });
            Value::Reg(dst)
        }

        ExprNode::Binary { op, left, right } => {
            // And and Or doesn't reach here but we keep it just in case
            // (for non short-circuiting versions)
            let lhs = lower_expr(cx, left);
            let rhs = lower_expr(cx, right);
            let ty = cx.node_types[&left.id].clone();
            let dst = cx.fresh_reg();
            cx.emit(Inst::Binary { dst, op: *op, lhs, rhs, ty });
            Value::Reg(dst)
        }

        ExprNode::Call { func, type_args, args }
            if matches!(&func.value, ExprNode::Var(name)
                if Intrinsic::lookup(name).is_some()) => {
            let ExprNode::Var(name) = &func.value else { unreachable!() };
            let intrinsic = Intrinsic::lookup(name).unwrap();
            lower_intrinsic(cx, intrinsic, type_args, args)
        }

        ExprNode::Call { func, args, .. } => {
            cx.emit(Inst::Comment(format!("call {}(...)", func.value)));
            // a bare name that isn't a local/param/global is a top-level function
            // -> direct call. Anything else (a local holding a fn pointer, a struct
            // field, etc.) is lowered to a `ptr` value and called indirectly.
            let callee = match &func.value {
                ExprNode::Var(name)
                    if !cx.resolved.contains_key(&func.id) && !cx.globals.contains_key(name) =>
                    Callee::Direct(*name),
                _ => Callee::Indirect(lower_expr(cx, func)),
            };
            // grab the callee's parameter types so we can apply array->slice coercion
            let param_tys: Vec<Type<'a>> = match cx.node_types.get(&func.id).cloned() {
                Some(Type::Function { params, .. }) => params,
                _ => vec![],
            };
            let lowered_args = args.iter().enumerate().map(|(i, arg)| {
                let arg_ty = cx.node_types[&arg.id].clone();
                let val = lower_expr(cx, arg);
                let param_ty = param_tys.get(i).cloned()
                    .unwrap_or_else(|| arg_ty.clone());
                let coerced_val = coerce(cx, val, &arg_ty, &param_ty);
                (coerced_val, param_ty)
            }).collect();

            let return_type = cx.node_types[&expr.id].clone();
            let struct_ret = if let Type::Struct(s) = &return_type { Some(*s) } else { None };
            if let Some(sname) = struct_ret {
                // if sret, allocate the result slot here and hand the callee a
                // pointer to it. The call returns void & the slot is the value
                let slot = cx.fresh_reg();
                cx.emit(Inst::AllocaStruct { dst: slot, name: sname, align: None });
                cx.emit(Inst::Call {
                    dst: None,
                    callee,
                    args: lowered_args,
                    return_type: Type::Void,
                    sret: Some((slot, sname)),
                });
                Value::Reg(slot)
            } else if return_type == Type::Void {
                cx.emit(Inst::Call { dst: None, callee, args: lowered_args, return_type, sret: None });
                Value::Const(Const::Bool(false)) // placeholder
            } else {
                let dst = cx.fresh_reg();
                cx.emit(Inst::Call { dst: Some(dst), callee, args: lowered_args, return_type, sret: None });
                Value::Reg(dst)
            }
        }

        ExprNode::Index { slice, index } => {
            let slice_val = lower_expr(cx, slice);
            let index_val = lower_expr(cx, index);
            let element_ty = match &cx.node_types[&slice.id] {
                Type::Slice(inner) => *inner.clone(),
                Type::Pointer(inner) => *inner.clone(),
                Type::Array(inner, _) => *inner.clone(),
                _ => unreachable!(),
            };

            let data_ptr = match cx.node_types[&slice.id] {
                Type::Slice(_) => {
                    let extracted = cx.fresh_reg();
                    cx.emit(Inst::ExtractValue { dst: extracted, val: slice_val.clone(), index: 0 });
                    extracted
                }
                Type::Pointer(_) | Type::Array(_, _) => {
                    // for fixed arrays the Var lowering already gave us the alloca pointer
                    match slice_val {
                        Value::Reg(r) => r,
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            };
            let elem_ptr = cx.fresh_reg();
            cx.emit(Inst::Index { dst: elem_ptr, slice: data_ptr, index: index_val, element_ty: element_ty.clone() });

            let dst = cx.fresh_reg();
            cx.emit(Inst::Load { dst, ptr: elem_ptr, ty: element_ty, align: None });

            Value::Reg(dst)
        }
    }
}

fn lower_lvalue<'a>(cx: &mut LowerCtx<'a>, expr: &Expr<'a>) -> Register {
    match &expr.value {
        ExprNode::Var(_) => cx.env[&cx.resolved[&expr.id]].0,

        ExprNode::Access { base, field } => {
            let base_val = lower_expr(cx, base);
            let base_ty = cx.node_types[&base.id].clone();
            // matches the typechecker's one-level auto-deref for `ptr.field`
            let struct_name = match base_ty {
                Type::Struct(name) => name,
                Type::Pointer(inner) => match *inner {
                    Type::Struct(name) => name,
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            let field_index = cx.structs[&struct_name]
                .iter()
                .position(|(fname, _)| fname == field)
                .unwrap();

            let field_ptr = cx.fresh_reg();
            cx.emit(Inst::FieldPtr {
                dst: field_ptr,
                struct_name: &struct_name,
                base: match base_val {
                    Value::Reg(r) => r,
                    _ => unreachable!(),
                },
                field_index,
            });
            field_ptr
        }

        // evaluate ptr as a value, register is the address
        ExprNode::Unary { op: UnaryOp::Deref, operand } => {
            match lower_expr(cx, operand) {
                Value::Reg(r) => r,
                _ => unreachable!(),
            }
        }

        ExprNode::Index { slice, index } => {
            let slice_val = lower_expr(cx, slice);
            let index_val = lower_expr(cx, index);
            let element_ty = match cx.node_types[&slice.id].clone() {
                Type::Slice(inner) => *inner,
                Type::Pointer(inner) => *inner,
                Type::Array(inner, _) => *inner,
                _ => unreachable!(),
            };

            // same extraction as rvalue
            let data_ptr = match cx.node_types[&slice.id] {
                Type::Slice(_) => {
                    let extracted = cx.fresh_reg();
                    cx.emit(Inst::ExtractValue { dst: extracted, val: slice_val.clone(), index: 0 });
                    extracted
                }
                Type::Pointer(_) | Type::Array(_, _) => {
                    match slice_val {
                        Value::Reg(r) => r,
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            };
            // return the address of the element instead of loading
            let dst = cx.fresh_reg();
            cx.emit(Inst::Index { dst, slice: data_ptr, index: index_val, element_ty });
            dst
        }

        e => panic!("invalid lvalue: {}", e),
    }
}

// Field-by-field copy from one struct pointer to another. Both `src` and `dst`
// must be pointers to a struct of the given name. Recurses on nested struct
// fields so the whole tree is deep-copied (value semantics).
// TODO: switch to an `llvm.memcpy` intrinsic for large structs instead of
// emitting a load/store per scalar field.
fn copy_struct<'a>(cx: &mut LowerCtx<'a>, struct_name: &'a str, src: Register, dst: Register) {
    let fields = cx.structs[struct_name].clone();
    for (i, (_fname, fty)) in fields.iter().enumerate() {
        let src_field = cx.fresh_reg();
        let dst_field = cx.fresh_reg();
        cx.emit(Inst::FieldPtr { dst: src_field, struct_name, base: src, field_index: i });
        cx.emit(Inst::FieldPtr { dst: dst_field, struct_name, base: dst, field_index: i });
        match fty {
            // for nested struct, both field pointers point at inlined sub-struct
            // storage, so recurse to deep-copy it
            Type::Struct(inner_name) => {
                copy_struct(cx, inner_name, src_field, dst_field);
            }
            // everything else (scalars, arrays, slices, simd) is a single
            // value/aggregate that an LLVM load/store copies directly
            _ => {
                let loaded = cx.fresh_reg();
                cx.emit(Inst::Load { dst: loaded, ptr: src_field, ty: fty.clone(), align: None });
                cx.emit(Inst::Store { ptr: dst_field, val: Value::Reg(loaded), ty: fty.clone(), align: None });
            }
        }
    }
}

fn lower_stmt<'a>(cx: &mut LowerCtx<'a>, stmt: &Stmt<'a>) {
    match &stmt.value {
        StmtNode::Expr(expr) => {
            lower_expr(cx, expr); // side effects only, discard result
        }

        StmtNode::Block(stmts) => {
            for stmt in stmts {
                lower_stmt(cx, stmt);
            }
        }

        StmtNode::Declare { ty, value, .. } => {
            // this local's binding identity is the Declare stmt's node id, matching
            // what name resolution recorded for every use of it.
            let binding = Binding::Local(stmt.id);
            // A struct/array literal local has a slot hoisted to the entry block
            // (collect_locals pre-allocated it into env); point the literal at that
            // slot so it fills it in place instead of alloca'ing at the literal
            // site — which, inside a loop, would grow the stack every iteration.
            if matches!(ty, Type::Array(_, _) | Type::Struct(_))
                && matches!(value.value, ExprNode::Struct { .. } | ExprNode::Slice(_))
            {
                let (slot, _) = cx.env[&binding]; // pre-allocated in the entry block
                cx.store_target = Some(slot);
                lower_expr(cx, value); // fills `slot`, consuming store_target
                return;
            }
            let val = lower_expr(cx, value);
            let value_ty = cx.node_types[&value.id].clone();
            match ty {
                Type::Array(_, _) => {
                    // a non-literal array value (e.g. a var) is already backed by a
                    // slot; adopt its register directly.
                    let arr_reg = match val {
                        Value::Reg(r) => r,
                        _ => unreachable!(),
                    };
                    cx.env.insert(binding, (arr_reg, ty.clone()));
                }
                Type::Struct(struct_name) => {
                    let src_reg = match val {
                        Value::Reg(r) => r,
                        _ => unreachable!(),
                    };
                    // a struct-returning call writes into a fresh sret slot nobody
                    // else aliases, so adopt it. anything else (Var, field access,
                    // etc.) references someone else's storage and needs a copy for
                    // value semantics. (Struct literals are handled above.)
                    if matches!(value.value, ExprNode::Call { .. }) {
                        cx.env.insert(binding, (src_reg, ty.clone()));
                    } else {
                        let dst_reg = cx.fresh_reg();
                        cx.emit(Inst::AllocaStruct { dst: dst_reg, name: struct_name, align: None });
                        copy_struct(cx, struct_name, src_reg, dst_reg);
                        cx.env.insert(binding, (dst_reg, ty.clone()));
                    }
                }
                _ => {
                    let val = coerce(cx, val, &value_ty, ty);
                    let (ptr, _) = cx.env[&binding]; // already alloca'd
                    cx.emit(Inst::Store { ptr, val, ty: ty.clone(), align: None });
                }
            }
        }

        StmtNode::Assign { left, value } => {
            let ptr = lower_lvalue(cx, left);  // see below
            let val = lower_expr(cx, value);
            let value_ty = cx.node_types[&value.id].clone();
            let ty = cx.node_types[&left.id].clone();
            let val = coerce(cx, val, &value_ty, &ty);
            cx.emit(Inst::Store { ptr, val, ty, align: None });
        }

        StmtNode::If { condition, then_branch, else_branch } => {
            cx.emit(Inst::Comment(format!("if {}", condition.value)));

            let cond_val = lower_expr(cx, condition);
            let cond_reg = match cond_val {
                Value::Reg(r) => r,
                _ => unreachable!(),
            };

            let then_block = cx.fresh_block();
            let else_block = cx.fresh_block();
            let merge_block = cx.fresh_block();

            cx.terminate(Terminator::Branch { cond: cond_reg, then_block, else_block });

            // then branch
            cx.current_block = then_block;
            cx.emit(Inst::Comment("if-then".to_string()));
            lower_stmt(cx, then_branch);
            if cx.blocks.iter().find(|b| b.id == then_block).unwrap().terminator.is_none() {
                cx.terminate(Terminator::Jump(merge_block));
            }

            // else branch
            cx.current_block = else_block;
            if let Some(else_branch) = else_branch {
                cx.emit(Inst::Comment("if-else".to_string()));
                lower_stmt(cx, else_branch);
            }
            if cx.blocks.iter().find(|b| b.id == else_block).unwrap().terminator.is_none() {
                cx.terminate(Terminator::Jump(merge_block));
            }

            cx.current_block = merge_block;
            cx.emit(Inst::Comment("if-merge".to_string()));
        }

        StmtNode::While { condition, body } => {
            let cond_block  = cx.fresh_block();
            let body_block  = cx.fresh_block();
            let merge_block = cx.fresh_block();

            cx.terminate(Terminator::Jump(cond_block));

            cx.current_block = cond_block;
            cx.emit(Inst::Comment(format!("while {}", condition.value)));
            let cond_val = lower_expr(cx, condition);
            let cond_reg = match cond_val {
                Value::Reg(r) => r,
                // if simple expression (e.g. while(true)) then we have to get
                // the value into a register first to branch on it
                v => {
                    let r = cx.fresh_reg();
                    cx.emit(Inst::Alloca { dst: r, ty: Type::Bool, align: None });
                    cx.emit(Inst::Store { ptr: r, val: v, ty: Type::Bool, align: None });
                    let s = cx.fresh_reg();
                    cx.emit(Inst::Load { dst: s, ptr: r, ty: Type::Bool, align: None });
                    s
                }
            };
            cx.terminate(Terminator::Branch {
                cond: cond_reg,
                then_block: body_block,
                else_block: merge_block,
            });

            cx.current_block = body_block;
            cx.emit(Inst::Comment("while body".to_string()));

            cx.loop_stack.push(LoopTargets {
                continue_block: cond_block,
                break_block: merge_block,
            });
            lower_stmt(cx, body);
            cx.loop_stack.pop();

            // add a jump to the merge block if the body doesn't already terminate
            // (e.g. via break/continue/return inside the last statement of the body)
            if cx.blocks.iter().find(|b| b.id == cx.current_block).unwrap().terminator.is_none() {
                cx.terminate(Terminator::Jump(cond_block));
            }

            cx.current_block = merge_block;
        }

        StmtNode::Continue => {
            cx.emit(Inst::Comment("continue".to_string()));
            cx.terminate(Terminator::Jump(cx.loop_stack.last().unwrap().continue_block));
        }
        StmtNode::Break => {
            cx.emit(Inst::Comment("break".to_string()));
            cx.terminate(Terminator::Jump(cx.loop_stack.last().unwrap().break_block));
        }

        StmtNode::Return(expr) => {
            let val = lower_expr(cx, expr);
            let value_ty = cx.node_types[&expr.id].clone();
            let ret_ty = cx.current_return_type.clone();
            let struct_ret = if let Type::Struct(s) = &ret_ty { Some(*s) } else { None };
            if let Some(name) = struct_ret {
                // copy the struct into the caller-provided sret slot, ret void
                let src = match val {
                    Value::Reg(r) => r,
                    _ => unreachable!(),
                };
                let dst = cx.sret_param.expect("struct-returning function has no sret slot");
                copy_struct(cx, name, src, dst);
                cx.terminate(Terminator::Return(None));
            } else {
                let val = coerce(cx, val, &value_ty, &ret_ty);
                cx.terminate(Terminator::Return(Some((val, ret_ty))));
            }
        }
    }
}

/// Recursively collect all local variable declarations in the function body,
/// including nested ones in blocks and branches.
/// This is for emitting all Alloca instructions upfront in the entry block
fn collect_locals<'a>(stmt: &Stmt<'a>, out: &mut Vec<(usize, &'a str, Type<'a>)>) {
    match &stmt.value {
        StmtNode::Declare { name, ty, value } => {
            // key by the Declare stmt's node id (its binding identity)
            match ty {
                // A struct/array *literal* used to alloca at its own site, which
                // sits inside any enclosing loop body and grows the stack every
                // iteration. Hoist one slot to the entry block; the Declare arm
                // fills it in place via store_target. Non-literal struct/array
                // declares (a struct-returning call, or a var copy) still adopt
                // or copy in lower_stmt and are not pre-allocated here.
                Type::Array(_, _) | Type::Struct(_) => {
                    if matches!(value.value, ExprNode::Struct { .. } | ExprNode::Slice(_)) {
                        out.push((stmt.id, name, ty.clone()));
                    }
                }
                _ => out.push((stmt.id, name, ty.clone())),
            }
        }
        StmtNode::Block(stmts) => {
            for s in stmts { collect_locals(s, out); }
        }
        StmtNode::If { then_branch, else_branch, .. } => {
            collect_locals(then_branch, out);
            if let Some(e) = else_branch { collect_locals(e, out); }
        }
        StmtNode::While { body, .. } => {
            collect_locals(body, out);
        }
        // rest should have no declarations
        _ => {}
    }
}

/// Lower a global's initializer expression to a constant. Scalar literals
/// (optionally negated) and struct literals of constants are supported; the
/// typechecker has already rejected anything else, so the unreachable arms
/// indicate a compiler bug. `structs` supplies field types for struct literals.
fn lower_const_init<'a>(
    structs: &HashMap<&'a str, Vec<(&'a str, Type<'a>)>>,
    expr: &Expr<'a>,
) -> ConstInit<'a> {
    match &expr.value {
        ExprNode::Bool(b)    => ConstInit::Scalar(Const::Bool(*b)),
        ExprNode::Int8(n)    => ConstInit::Scalar(Const::Int8(*n)),
        ExprNode::Int32(n)   => ConstInit::Scalar(Const::Int32(*n)),
        ExprNode::Int64(n)   => ConstInit::Scalar(Const::Int64(*n)),
        ExprNode::Uint8(n)   => ConstInit::Scalar(Const::Uint8(*n)),
        ExprNode::Uint32(n)  => ConstInit::Scalar(Const::Uint32(*n)),
        ExprNode::Uint64(n)  => ConstInit::Scalar(Const::Uint64(*n)),
        ExprNode::Float32(f) => ConstInit::Scalar(Const::Float32(*f)),
        ExprNode::Float64(f) => ConstInit::Scalar(Const::Float64(*f)),
        ExprNode::Unary { op: UnaryOp::Neg, operand } => match &operand.value {
            ExprNode::Int8(n)    => ConstInit::Scalar(Const::Int8(-*n)),
            ExprNode::Int32(n)   => ConstInit::Scalar(Const::Int32(-*n)),
            ExprNode::Int64(n)   => ConstInit::Scalar(Const::Int64(-*n)),
            ExprNode::Uint8(n)   => ConstInit::Scalar(Const::Uint8(n.wrapping_neg())),
            ExprNode::Uint32(n)  => ConstInit::Scalar(Const::Uint32(n.wrapping_neg())),
            ExprNode::Uint64(n)  => ConstInit::Scalar(Const::Uint64(n.wrapping_neg())),
            ExprNode::Float32(f) => ConstInit::Scalar(Const::Float32(-*f)),
            ExprNode::Float64(f) => ConstInit::Scalar(Const::Float64(-*f)),
            other => unreachable!("non-constant global initializer reached lowering: -{}", other),
        },
        // struct literal: pair each field's declared type with its constant.
        // typecheck guarantees the fields match the definition in order.
        ExprNode::Struct { name, fields } => {
            let defs = &structs[name];
            let inits = defs.iter().zip(fields.iter())
                .map(|((_, fty), (_, fexpr))| (fty.clone(), lower_const_init(structs, fexpr)))
                .collect();
            ConstInit::Struct(inits)
        }
        // a bare name in a const initializer is a function (typecheck ensured it);
        // its address @name is the constant.
        ExprNode::Var(name) => ConstInit::FnAddr(name),
        other => unreachable!("non-constant global initializer reached lowering: {}", other),
    }
}

fn lower_function<'a>(cx: &mut LowerCtx<'a>, func: &TopLevel<'a>)
-> (Vec<(Register, Type<'a>)>, Option<(Register, &'a str)>) {
    match &func.value {
        TopLevelNode::Function { attributes, params, return_type, body, .. } => {
            let entry = cx.fresh_block();
            cx.current_block = entry;
            cx.current_return_type = return_type.clone();

            // struct returns are lowered with an sret out-pointer
            // allocate its register up front so `return` can copy into it
            let sret = if let Type::Struct(s) = return_type {
                let r = cx.fresh_reg();
                cx.sret_param = Some(r);
                Some((r, *s))
            } else {
                cx.sret_param = None;
                None
            };

            let mut param_regs = Vec::new();
            for (name, ty) in params {
                if attributes.iter().any(|a| a.value.name == "export")
                && matches!(ty, Type::Slice(_)) {
                    // split the fat pointer into 2 parameters for the data pointer and length
                    // e.g. foo(buf: T[]) -> @foo(*T %buf_ptr, i32 %buf_len)
                    let ptr_reg = cx.fresh_reg();
                    let len_reg = cx.fresh_reg();

                    let inner_ty = match ty {
                        Type::Slice(inner) => *inner.clone(),
                        _ => unreachable!(),
                    };

                    let fat0 = cx.fresh_reg();
                    cx.emit(Inst::Comment(format!("params {}: {} (splitted)", name, ty)));
                    cx.emit(Inst::InsertValue {
                        dst: fat0,
                        elem: Value::Const(Const::Undef),
                        ty: Type::Pointer(Box::new(inner_ty.clone())),
                        val: Value::Reg(ptr_reg),
                        index: 0,
                    });
                    let fat1 = cx.fresh_reg();
                    cx.emit(Inst::InsertValue {
                        dst: fat1,
                        elem: Value::Reg(fat0),
                        ty: Type::Int32,
                        val: Value::Reg(len_reg),
                        index: 1,
                    });

                    // store the reconstructed fat pointer into the alloca'd local
                    let param_ptr = cx.fresh_reg();
                    cx.emit(Inst::Alloca { dst: param_ptr, ty: ty.clone(), align: None });
                    cx.emit(Inst::Store { ptr: param_ptr, val: Value::Reg(fat1), ty: ty.clone(), align: None });
                    cx.env.insert(Binding::Param(name), (param_ptr, ty.clone()));

                    // push BOTH as incoming params
                    param_regs.push((ptr_reg, Type::Pointer(Box::new(inner_ty.clone()))));
                    param_regs.push((len_reg, Type::Int32));
                } else if let Type::Struct(struct_name) = ty {
                    // pass-by-value: caller still hands us a `ptr` to its
                    // struct, but we copy into our own local slot on entry so
                    // mutations don't leak back. For pass-by-reference the
                    // user writes `*Stereo` explicitly — that falls through
                    // to the `_` arm and stays a plain pointer.
                    let param_reg = cx.fresh_reg();
                    cx.emit(Inst::Comment(format!("params {}: {} (by value, copied on entry)", name, ty)));
                    param_regs.push((param_reg, ty.clone()));

                    let local_reg = cx.fresh_reg();
                    cx.emit(Inst::AllocaStruct { dst: local_reg, name: struct_name, align: None });
                    copy_struct(cx, struct_name, param_reg, local_reg);
                    cx.env.insert(Binding::Param(name), (local_reg, ty.clone()));
                } else {
                    // actual register for the parameter value
                    // e.g. @foo(T %param_regN, ...)
                    // and then use param_ptr so we can treat it like normal mutable locals
                    let param_reg = cx.fresh_reg();
                    let param_ptr = cx.fresh_reg();
                    cx.emit(Inst::Comment(format!("params {}: {}", name, ty)));
                    cx.emit(Inst::Alloca { dst: param_ptr, ty: ty.clone(), align: None });
                    cx.emit(Inst::Store { ptr: param_ptr, val: Value::Reg(param_reg), ty: ty.clone(), align: None });

                    param_regs.push((param_reg, ty.clone()));
                    cx.env.insert(Binding::Param(name), (param_ptr, ty.clone()));
                }
            }

            // collect all local variable declarations in the function body and emit Allocas for them.
            // each local is keyed by its Declare's node id, so shadowed same-named
            // locals get distinct slots (uses are routed by name resolution).
            let mut locals = Vec::new();
            for stmt in body {
                collect_locals(stmt, &mut locals);
            }
            for (decl_id, name, ty) in locals {
                let local_reg = cx.fresh_reg();
                cx.emit(Inst::Comment(format!("local {}: {}", name, ty)));
                match &ty {
                    // struct/array literal locals are hoisted here (collect_locals)
                    // so the slot is allocated once, not per loop iteration.
                    Type::Struct(struct_name) => {
                        cx.emit(Inst::AllocaStruct { dst: local_reg, name: struct_name, align: None });
                    }
                    Type::Array(inner, length) => {
                        cx.emit(Inst::AllocaArray { dst: local_reg, ty: (**inner).clone(), length: length.expect_lit() });
                    }
                    _ => {
                        cx.emit(Inst::Alloca { dst: local_reg, ty: ty.clone(), align: None });
                    }
                }
                cx.env.insert(Binding::Local(decl_id), (local_reg, ty.clone()));
            }

            cx.emit(Inst::Comment("function body".to_string()));

            for stmt in body {
                lower_stmt(cx, stmt);
            }

            if cx.blocks.iter().find(|b| b.id == cx.current_block).unwrap().terminator.is_none() {
                if *return_type == Type::Void {
                    cx.terminate(Terminator::Return(None));
                } else {
                    // typecheck's all-paths-return analysis guarantees every path
                    // of a non-void function returns, so a block still open at the
                    // end of the body is provably dead (e.g. the merge block after
                    // an if/else whose branches both return). mark it unreachable.
                    cx.terminate(Terminator::Unreachable);
                }
            }

            (param_regs, sret)
        }
        TopLevelNode::Extern { .. } => (vec![], None),
        TopLevelNode::Struct { .. } => (vec![], None),
        TopLevelNode::Global { .. } => (vec![], None),
    }
}

pub fn lower<'a>(
    program: &[TopLevel<'a>],
    typecheck_context: &crate::mid::typecheck::Context<'a>,
) -> Module<'a> {
    let mut cx = LowerCtx {
        reg_counter: 0,
        block_counter: 0,
        current_block: BlockId(0),
        blocks: vec![],
        loop_stack: vec![],
        env: HashMap::new(),
        globals: HashMap::new(),
        structs: typecheck_context.structs.clone(),
        node_types: typecheck_context.node_types.clone(),
        resolved: typecheck_context.resolved.clone(),
        current_return_type: Type::Void,
        sret_param: None,
        store_target: None,
        strings: Vec::new(),
    };

    let mut functions = Vec::new();
    let mut externs = Vec::new();
    let mut structs = Vec::new();
    let mut globals = Vec::new();

    // register every global up front so a function can reference one declared
    // later in the file.
    for toplevel in program {
        if let TopLevelNode::Global { name, ty, .. } = &toplevel.value {
            cx.globals.insert(name, ty.clone());
        }
    }

    for toplevel in program {
        match &toplevel.value {
            TopLevelNode::Function { name, attributes, return_type, generics, .. } => {
                // generic functions are templates; without monomorphization there
                // is nothing to lower until they're instantiated at a call site
                if !generics.is_empty() {
                    continue;
                }
                let (param_regs, sret) = lower_function(&mut cx, toplevel);
                functions.push(Function {
                    name,
                    attributes: attributes.clone(),
                    params: param_regs,
                    return_type: return_type.clone(),
                    blocks: cx.blocks.clone(),
                    sret,
                });
                // reset for next function. env holds only this function's
                // params/locals; clearing keeps stale names from a prior function
                // from masquerading as in-scope (the call dispatch consults it).
                cx.reg_counter = 0;
                cx.block_counter = 0;
                cx.blocks.clear();
                cx.env.clear();
            }
            TopLevelNode::Extern { name, attributes, params, return_type, .. } => {
                externs.push(ExternDecl {
                    name,
                    attributes: attributes.clone(),
                    params: params.iter().map(|(_, ty)| ty.clone()).collect(),
                    return_type: return_type.clone(),
                });
            }
            TopLevelNode::Struct { name, fields, .. } => {
                structs.push((*name, fields.clone()));
            }
            TopLevelNode::Global { name, attributes, ty, value } => {
                globals.push(Global {
                    name,
                    export: attributes.iter().any(|a| a.value.name == "export"),
                    ty: ty.clone(),
                    init: lower_const_init(&cx.structs, value),
                });
            }
        }
    }

    Module { functions, externs, structs, globals, strings: cx.strings }
}