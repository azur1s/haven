use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use haven_common::ast::*;
use crate::intrinsics::Intrinsic;
use crate::typecheck::EnumDef;

/// A constant integer of the given integer `repr` type (an enum discriminant).
fn int_const(repr: &Type, val: i64) -> Const {
    match repr {
        Type::Int8   => Const::Int8(val as i8),
        Type::Int32  => Const::Int32(val as i32),
        Type::Int64  => Const::Int64(val),
        Type::Uint8  => Const::Uint8(val as u8),
        Type::Uint32 => Const::Uint32(val as u32),
        Type::Uint64 => Const::Uint64(val as u64),
        _ => unreachable!("non-integer enum repr {:?}", repr),
    }
}

/// If `name` is an `Enum::Variant` reference, the discriminant as a typed const.
fn enum_const<'a>(enums: &HashMap<&'a str, EnumDef<'a>>, name: &str) -> Option<Const> {
    let (ename, variant) = name.split_once("::")?;
    let def = enums.get(ename)?;
    Some(int_const(&def.repr, *def.variants.get(variant)?))
}

/// Rewrites a declared type's `Struct(name)` into `Type::Enum` for any declared
/// enum, recursing through compound types. Struct field types arrive already
/// resolved (via typecheck's `cx.structs`), but declared param/return/local
/// types are the raw parser types, so lowering resolves them here.
fn resolve_enum_ty<'a>(enums: &HashMap<&'a str, EnumDef<'a>>, ty: &Type<'a>) -> Type<'a> {
    match ty {
        Type::Struct { name, args } if args.is_empty() && enums.contains_key(name) =>
            Type::Enum { name, repr: Box::new(enums[name].repr.clone()), has_payload: enums[name].has_payload },
        Type::Pointer(i) => Type::Pointer(Box::new(resolve_enum_ty(enums, i))),
        Type::Array(i, n) => Type::Array(Box::new(resolve_enum_ty(enums, i)), n.clone()),
        Type::Slice(i)    => Type::Slice(Box::new(resolve_enum_ty(enums, i))),
        Type::Simd(i, n)  => Type::Simd(Box::new(resolve_enum_ty(enums, i)), n.clone()),
        Type::Function { params, return_type } => Type::Function {
            params: params.iter().map(|p| resolve_enum_ty(enums, p)).collect(),
            return_type: Box::new(resolve_enum_ty(enums, return_type)),
        },
        other => other.clone(),
    }
}

/// The synthetic struct backing an aggregate value passed/stored by pointer: a
/// real struct, or a data-carrying enum (whose `{ tag, payload }` aggregate is
/// registered as a struct of the same name). Every "this is an aggregate, route
/// it by pointer" site funnels through here so a data enum is never mistaken for
/// a scalar. A field-less enum returns `None` - it is a bare scalar.
fn aggregate_struct_name<'a>(ty: &Type<'a>) -> Option<&'a str> {
    match ty {
        Type::Struct { name, .. } => Some(name),
        Type::Enum { name, has_payload: true, .. } => Some(name),
        _ => None,
    }
}

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
    Null,
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
    /// Address of a module-level, NUL-terminated string blob, emitted as
    /// `@.str.{0}` - the raw `*const u8` value of a `str`. The index refers into
    /// `Module::strings`.
    GlobalStr(usize),
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Const::Null => write!(f, "null"),
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
    Index { dst: Register, slice: Register, index: Value, index_ty: Type<'a>, element_ty: Type<'a> },
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

    // switch <ty> %value, label %default [ <ty> <const>, label %block ... ]
    // used by `match`; the scrutinee is an integer/enum-discriminant value.
    Switch {
        value: Value,
        value_ty: Type<'a>,
        default: BlockId,
        cases: Vec<(Const, BlockId)>,
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
            Terminator::Switch { value, value_ty, default, cases } => {
                let arms = cases.iter().map(|(c, b)| format!("{} {}, label {}", value_ty, c, b))
                    .collect::<Vec<_>>().join(" ");
                write!(f, "switch {} {}, label {} [ {} ]", value_ty, value, default, arms)
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
/// expressions that map onto an LLVM constant aggregate - no code runs to produce
/// it.
#[derive(Clone, Debug)]
pub enum ConstInit<'a> {
    Scalar(Const),
    /// A constant struct aggregate: one `(field type, field constant)` per field,
    /// in declaration order. Nested structs recurse.
    Struct(Vec<(Type<'a>, ConstInit<'a>)>),
    /// A constant array aggregate: the element type plus one constant per element.
    Array(Type<'a>, Vec<ConstInit<'a>>),
    /// The address of a top-level function, emitted as `@name` - a link-time
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
    /// Declared enums (from typecheck): resolves an `Enum::Variant` reference to
    /// its discriminant constant during lowering.
    pub enums: HashMap<&'a str, EnumDef<'a>>,
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

    /// Intern a string literal's raw source text, resolving escape sequences and
    /// appending a NUL terminator, and return its index into `strings`. `str` is
    /// a raw `*const u8` C string, so the stored blob carries the trailing `\0`;
    /// callers get a bare pointer to it. Identical blobs are deduplicated so
    /// repeated literals share one global.
    pub fn intern_string(&mut self, raw: &str) -> usize {
        let mut bytes = resolve_escapes(raw);
        bytes.push(0); // NUL terminator, so the raw pointer is a valid C string
        if let Some(i) = self.strings.iter().position(|b| *b == bytes) {
            return i;
        }
        let idx = self.strings.len();
        self.strings.push(bytes);
        idx
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
        Intrinsic::Null => Value::Const(Const::Null),
        // Intrinsic::Len => {
        //     let arg_val = lower_expr(cx, &args[0]);
        //     if matches!(cx.node_types[&args[0].id], Type::Str) {
        //         // `str` is a raw NUL-terminated C string with no carried length,
        //         // so recover it at runtime with libc `strlen` (returns i64) and
        //         // narrow to the i32 that `len()` is typed as.
        //         let raw = cx.fresh_reg();
        //         cx.emit(Inst::Call {
        //             dst: Some(raw),
        //             callee: Callee::Direct("strlen"),
        //             args: vec![(arg_val, Type::Pointer(Box::new(Type::Uint8)))],
        //             return_type: Type::Uint64,
        //             sret: None,
        //         });
        //         let dst = cx.fresh_reg();
        //         cx.emit(Inst::Extend {
        //             dst,
        //             val: Value::Reg(raw),
        //             from_ty: Type::Uint64,
        //             to_ty: Type::Int32,
        //         });
        //         Value::Reg(dst)
        //     } else {
        //         // slices/arrays carry an explicit i32 length in field 1.
        //         let dst = cx.fresh_reg();
        //         cx.emit(Inst::ExtractValue { dst, val: arg_val, index: 1 });
        //         Value::Reg(dst)
        //     }
        // }
        Intrinsic::NumericalCast => {
            // numerical_cast::<T>(value). An enum on either side casts as its
            // integer discriminant repr, so unwrap it before selecting the cast.
            let unwrap_enum = |t: Type<'a>| match t {
                Type::Enum { repr, .. } => *repr,
                other => other,
            };
            let val = lower_expr(cx, &args[0]);
            let from_ty = unwrap_enum(cx.node_types[&args[0].id].clone());
            let to_ty = unwrap_enum(ta_type(type_args, 0));
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
            cx.emit(Inst::Index { dst: elem_ptr, slice: data_ptr, index: offset_val, index_ty: cx.node_types[&args[1].id].clone(), element_ty: ty.clone() });
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
            cx.emit(Inst::Index { dst: elem_ptr, slice: data_ptr, index: offset_val, index_ty: cx.node_types[&args[1].id].clone(), element_ty: ty.clone() });
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

        // a string literal is a raw `*const u8`: the bare address of a
        // read-only, NUL-terminated global blob. No length is carried; `len()`
        // recovers it with `strlen` (see lower_intrinsic).
        ExprNode::Str(s) => {
            let idx = cx.intern_string(s);
            Value::Const(Const::GlobalStr(idx))
        }

        ExprNode::Var(name) => {
            // locals/params (env) shadow module-level globals. resolution picked
            // the exact binding for this use, so shadowing is already decided.
            if let Some((reg, ty)) = cx.resolved.get(&expr.id).and_then(|b| cx.env.get(b)).cloned() {
                match ty {
                    // fixed arrays are stored in env as the alloca register itself, not a pointer
                    // to one, so loading would yield the array value - we want the pointer
                    Type::Array(_, _) => Value::Reg(reg),
                    // a struct or data-enum aggregate is held by pointer: hand it back.
                    _ if aggregate_struct_name(&ty).is_some() => Value::Reg(reg),
                    _ => {
                        let dst = cx.fresh_reg();
                        cx.emit(Inst::Load { dst, ptr: reg, ty, align: None });
                        Value::Reg(dst)
                    }
                }
            } else if let Some(ty) = cx.globals.get(name).cloned() {
                // reference to a module-level global: its symbol @name is already
                // a pointer to the constant. Materialize that address, then treat
                // it like an env entry - hand back the pointer for aggregates,
                // load the value for scalars.
                let addr = cx.fresh_reg();
                cx.emit(Inst::GlobalPtr { dst: addr, name });
                match ty {
                    Type::Array(_, _) => Value::Reg(addr),
                    _ if aggregate_struct_name(&ty).is_some() => Value::Reg(addr),
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
            } else if let Some(c) = enum_const(&cx.enums, name) {
                // a unit variant of a *data* enum is an aggregate (alloca + tag
                // store), even though its `Var` shape matches a field-less enum's
                // scalar discriminant. Field-less enums stay a bare const.
                let agg_enum = match cx.node_types.get(&expr.id) {
                    Some(Type::Enum { has_payload: true, name: ename, .. }) => Some(*ename),
                    _ => None,
                };
                match agg_enum {
                    Some(ename) => construct_data_variant(cx, ename, name, c, &[]),
                    None => Value::Const(c),
                }
            } else {
                panic!("unknown variable '{name}' in MIL lowering");
            }
        }

        ExprNode::Struct { name, fields, .. } => {
            // a struct-style data-enum constructor `Msg::Cc { id, val }` reuses the
            // struct-literal syntax but builds the aggregate in place. Detected by
            // the node's inferred aggregate-enum type; construction requires
            // declaration order (typecheck enforced), so the field exprs are already
            // in payload-struct order and pass positionally to the shared builder.
            if let Some(Type::Enum { has_payload: true, name: ename, .. }) =
                cx.node_types.get(&expr.id).cloned().as_ref()
            {
                let ename = *ename;
                let tag = enum_const(&cx.enums, name).expect("variant const validated in typecheck");
                let args: Vec<Expr<'a>> = fields.iter().map(|(_, e)| e.clone()).collect();
                return construct_data_variant(cx, ename, name, tag, &args);
            }

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
                    Type::Struct { name: inner, .. } => {
                        let src = match field_val {
                            Value::Reg(r) => r,
                            _ => unreachable!(),
                        };
                        copy_struct(cx, inner, src, field_ptr);
                    }
                    // an array field is likewise inlined aggregate storage:
                    // field_val is the source array's address, so copy the whole
                    // aggregate in (load+store) rather than storing the pointer as
                    // if it were the array value.
                    Type::Array(..) => {
                        let src = match field_val {
                            Value::Reg(r) => r,
                            _ => unreachable!(),
                        };
                        let loaded = cx.fresh_reg();
                        cx.emit(Inst::Load { dst: loaded, ptr: src, ty: field_ty.clone(), align: None });
                        cx.emit(Inst::Store { ptr: field_ptr, val: Value::Reg(loaded), ty: field_ty, align: None });
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
                Type::Struct { name, .. } => name,
                Type::Pointer(inner) => match *inner {
                    Type::Struct { name, .. } => name,
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
                // a struct- or array-typed field is inlined aggregate storage: its
                // value is its address (indexing/copying use the pointer), so hand
                // back the field pointer instead of loading it
                Type::Struct { .. } | Type::Array(..) => Value::Reg(field_ptr),
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
            if matches!(ty, Type::Struct { .. }) {
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
            // a data-enum constructor `Msg::Note(a, b)` is not a real call: build
            // the aggregate in place. (A field-less enum "call" is impossible -
            // typecheck yields a scalar-typed variant, never `has_payload: true`.)
            if let ExprNode::Var(name) = &func.value {
                if let Some(c) = enum_const(&cx.enums, name) {
                    if let Some(Type::Enum { has_payload: true, name: ename, .. }) =
                        cx.node_types.get(&expr.id).cloned().as_ref()
                    {
                        let ename = *ename;
                        return construct_data_variant(cx, ename, name, c, args);
                    }
                }
            }
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
            let struct_ret = aggregate_struct_name(&return_type);
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
            cx.emit(Inst::Index { dst: elem_ptr, slice: data_ptr, index: index_val, index_ty: cx.node_types[&index.id].clone(), element_ty: element_ty.clone() });

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
                Type::Struct { name, .. } => name,
                Type::Pointer(inner) => match *inner {
                    Type::Struct { name, .. } => name,
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
            cx.emit(Inst::Index { dst, slice: data_ptr, index: index_val, index_ty: cx.node_types[&index.id].clone(), element_ty });
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
            Type::Struct { name: inner_name, .. } => {
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

/// Construct a data-enum aggregate in place: alloca `%Enum` (or reuse a hoisted
/// slot), store the variant's discriminant into the tag (field 0), then store
/// each payload argument into the variant's payload struct at field 1. `tag` is
/// the variant's discriminant const; `variant_path` is the joined `Enum::Variant`.
/// Returns a pointer to the aggregate. Mirrors `ExprNode::Struct` lowering, so a
/// unit variant (`args` empty) is just alloca + tag store.
fn construct_data_variant<'a>(
    cx: &mut LowerCtx<'a>,
    enum_name: &'a str,
    variant_path: &'a str,
    tag: Const,
    args: &[Expr<'a>],
) -> Value {
    let base = match cx.store_target.take() {
        Some(slot) => slot,
        None => {
            let dst = cx.fresh_reg();
            cx.emit(Inst::AllocaStruct { dst, name: enum_name, align: None });
            dst
        }
    };
    // tag -> field 0
    let tag_ptr = cx.fresh_reg();
    cx.emit(Inst::FieldPtr { dst: tag_ptr, struct_name: enum_name, base, field_index: 0 });
    let repr = cx.enums[enum_name].repr.clone();
    cx.emit(Inst::Store { ptr: tag_ptr, val: Value::Const(tag), ty: repr, align: None });

    if !args.is_empty() {
        let variant = variant_path.split_once("::").unwrap().1;
        let pstruct = crate::typecheck::enum_payload_struct_name(enum_name, variant);
        let payload_base = cx.fresh_reg();
        cx.emit(Inst::FieldPtr { dst: payload_base, struct_name: enum_name, base, field_index: 1 });
        for (i, arg) in args.iter().enumerate() {
            let field_ty = cx.structs[pstruct][i].1.clone();
            let arg_ty = cx.node_types[&arg.id].clone();
            let field_val = lower_expr(cx, arg);
            let field_ptr = cx.fresh_reg();
            cx.emit(Inst::FieldPtr { dst: field_ptr, struct_name: pstruct, base: payload_base, field_index: i });
            match aggregate_struct_name(&field_ty) {
                // a struct- or data-enum-typed payload field is inlined storage:
                // deep-copy the source aggregate into it (field_val is its address).
                Some(inner) => {
                    let src = match field_val { Value::Reg(r) => r, _ => unreachable!() };
                    copy_struct(cx, inner, src, field_ptr);
                }
                None => match field_ty {
                    // an array field is inlined aggregate storage: copy it whole.
                    Type::Array(..) => {
                        let src = match field_val { Value::Reg(r) => r, _ => unreachable!() };
                        let loaded = cx.fresh_reg();
                        cx.emit(Inst::Load { dst: loaded, ptr: src, ty: field_ty.clone(), align: None });
                        cx.emit(Inst::Store { ptr: field_ptr, val: Value::Reg(loaded), ty: field_ty, align: None });
                    }
                    _ => {
                        let cv = coerce(cx, field_val, &arg_ty, &field_ty);
                        cx.emit(Inst::Store { ptr: field_ptr, val: cv, ty: field_ty, align: None });
                    }
                },
            }
        }
    }
    Value::Reg(base)
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
            // resolve enum names so an enum local takes the scalar path below.
            let ty = &resolve_enum_ty(&cx.enums, ty);
            // this local's binding identity is the Declare stmt's node id, matching
            // what name resolution recorded for every use of it.
            let binding = Binding::Local(stmt.id);
            // A struct/array literal local has a slot hoisted to the entry block
            // (collect_locals pre-allocated it into env); point the literal at that
            // slot so it fills it in place instead of alloca'ing at the literal
            // site - which, inside a loop, would grow the stack every iteration.
            if matches!(ty, Type::Array(_, _) | Type::Struct { .. })
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
                // a struct or data-enum aggregate: same value-semantics rules.
                _ if aggregate_struct_name(ty).is_some() => {
                    let struct_name = aggregate_struct_name(ty).unwrap();
                    let src_reg = match val {
                        Value::Reg(r) => r,
                        _ => unreachable!(),
                    };
                    // a call (incl. a data-enum constructor) writes into a fresh
                    // slot nobody else aliases, so adopt it. anything else (Var,
                    // field access, ...) references someone else's storage and
                    // needs a copy for value semantics. (Literals handled above.)
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
            // Check the *current* block, not `then_block`: if the branch body
            // contained nested control flow (a `while`/`if`), `current_block`
            // has advanced to that construct's merge block, and it - not the
            // already-terminated `then_block` - is what still needs a jump.
            if cx.blocks.iter().find(|b| b.id == cx.current_block).unwrap().terminator.is_none() {
                cx.terminate(Terminator::Jump(merge_block));
            }

            // else branch
            cx.current_block = else_block;
            if let Some(else_branch) = else_branch {
                cx.emit(Inst::Comment("if-else".to_string()));
                lower_stmt(cx, else_branch);
            }
            if cx.blocks.iter().find(|b| b.id == cx.current_block).unwrap().terminator.is_none() {
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

        StmtNode::Match { scrutinee, arms } => {
            cx.emit(Inst::Comment(format!("match {}", scrutinee.value)));
            let scrut_ty = cx.node_types[&scrutinee.id].clone();
            let scrut_val = lower_expr(cx, scrutinee);
            // A data enum lowers to a pointer to its aggregate: the switch runs on
            // the tag (field 0), and payload bindings read off field 1. A field-less
            // enum / integer lowers to the scalar value directly.
            let (switch_val, value_ty, agg): (Value, Type<'a>, Option<(&'a str, Register)>) =
                match &scrut_ty {
                    Type::Enum { repr, has_payload: true, name } => {
                        let ptr = match scrut_val { Value::Reg(r) => r, _ => unreachable!() };
                        let tag_ptr = cx.fresh_reg();
                        cx.emit(Inst::FieldPtr { dst: tag_ptr, struct_name: name, base: ptr, field_index: 0 });
                        let tag = cx.fresh_reg();
                        cx.emit(Inst::Load { dst: tag, ptr: tag_ptr, ty: (**repr).clone(), align: None });
                        (Value::Reg(tag), (**repr).clone(), Some((*name, ptr)))
                    }
                    Type::Enum { repr, .. } => (scrut_val, (**repr).clone(), None),
                    t => (scrut_val, t.clone(), None),
                };

            let merge_block = cx.fresh_block();
            let mut cases: Vec<(Const, BlockId)> = Vec::new();
            let mut wildcard_block: Option<BlockId> = None;
            // one block per arm; bodies (and any payload bindings) are lowered
            // after the switch is wired. Carry the pattern to bind inside the arm.
            let mut arm_blocks: Vec<(BlockId, &Stmt<'a>, &Pattern<'a>)> = Vec::new();
            for (pat, body) in arms {
                let block = cx.fresh_block();
                match &pat.value {
                    PatternNode::Wildcard => wildcard_block = Some(block),
                    PatternNode::Int(n) => cases.push((int_const(&value_ty, *n), block)),
                    PatternNode::Path(p) => {
                        let c = enum_const(&cx.enums, p).expect("enum pattern validated in typecheck");
                        cases.push((c, block));
                    }
                    PatternNode::Variant { path, .. } | PatternNode::StructVariant { path, .. } => {
                        let c = enum_const(&cx.enums, path).expect("enum pattern validated in typecheck");
                        cases.push((c, block));
                    }
                    PatternNode::Bind(_) => unreachable!("bare binding rejected in typecheck"),
                }
                arm_blocks.push((block, body, pat));
            }

            // the default target is the wildcard arm, or - for an exhaustive enum
            // with no `_` - a synthesized unreachable block.
            let synth_default = wildcard_block.is_none();
            let default = wildcard_block.unwrap_or_else(|| cx.fresh_block());

            cx.terminate(Terminator::Switch { value: switch_val, value_ty, default, cases });

            for (block, body, pat) in arm_blocks {
                cx.current_block = block;
                // bind a data-variant's payload fields as views into the aggregate
                // (a FieldPtr per bound field, no fresh storage), keyed by each
                // binding's node id so uses in the body resolve to it.
                if let (PatternNode::Variant { path, fields }, Some((ename, ptr))) = (&pat.value, agg) {
                    let variant = path.split_once("::").unwrap().1;
                    let pstruct = crate::typecheck::enum_payload_struct_name(ename, variant);
                    let payload_base = cx.fresh_reg();
                    cx.emit(Inst::FieldPtr { dst: payload_base, struct_name: ename, base: ptr, field_index: 1 });
                    for (i, fpat) in fields.iter().enumerate() {
                        if let PatternNode::Bind(_) = &fpat.value {
                            let fty = cx.structs[pstruct][i].1.clone();
                            let fp = cx.fresh_reg();
                            cx.emit(Inst::FieldPtr { dst: fp, struct_name: pstruct, base: payload_base, field_index: i });
                            cx.env.insert(Binding::Local(fpat.id), (fp, fty));
                        }
                    }
                }
                // struct-style destructure: bind by field name, so the FieldPtr uses
                // the payload struct's index for that name (order in the pattern is
                // irrelevant). A `_` field is skipped, a `Bind` becomes a view.
                if let (PatternNode::StructVariant { path, fields }, Some((ename, ptr))) = (&pat.value, agg) {
                    let variant = path.split_once("::").unwrap().1;
                    let pstruct = crate::typecheck::enum_payload_struct_name(ename, variant);
                    let payload_base = cx.fresh_reg();
                    cx.emit(Inst::FieldPtr { dst: payload_base, struct_name: ename, base: ptr, field_index: 1 });
                    for (fname, fpat) in fields {
                        if let PatternNode::Bind(_) = &fpat.value {
                            let idx = cx.structs[pstruct].iter().position(|(n, _)| n == fname).unwrap();
                            let fty = cx.structs[pstruct][idx].1.clone();
                            let fp = cx.fresh_reg();
                            cx.emit(Inst::FieldPtr { dst: fp, struct_name: pstruct, base: payload_base, field_index: idx });
                            cx.env.insert(Binding::Local(fpat.id), (fp, fty));
                        }
                    }
                }
                lower_stmt(cx, body);
                if cx.blocks.iter().find(|b| b.id == cx.current_block).unwrap().terminator.is_none() {
                    cx.terminate(Terminator::Jump(merge_block));
                }
            }
            if synth_default {
                cx.current_block = default;
                cx.terminate(Terminator::Unreachable);
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
            let struct_ret = aggregate_struct_name(&ret_ty);
            if let Some(name) = struct_ret {
                // copy the aggregate into the caller-provided sret slot, ret void
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
fn collect_locals<'a>(stmt: &Stmt<'a>, enums: &HashMap<&'a str, EnumDef<'a>>, out: &mut Vec<(usize, &'a str, Type<'a>)>) {
    match &stmt.value {
        StmtNode::Declare { name, ty, value } => {
            // resolve enum names so an enum local is pre-allocated as its scalar
            // repr (not skipped as a struct), matching the scalar Declare path.
            let ty = resolve_enum_ty(enums, ty);
            // key by the Declare stmt's node id (its binding identity)
            match ty {
                // A struct/array/data-enum *literal* used to alloca at its own
                // site, which sits inside any enclosing loop body and grows the
                // stack every iteration. Hoist one slot to the entry block; the
                // Declare arm fills it in place via store_target. Non-literal
                // aggregate declares (a call, incl. a data-enum constructor, or a
                // var copy) still adopt or copy in lower_stmt and are not
                // pre-allocated here.
                Type::Array(_, _) => {
                    if matches!(value.value, ExprNode::Struct { .. } | ExprNode::Slice(_)) {
                        out.push((stmt.id, name, ty));
                    }
                }
                _ if aggregate_struct_name(&ty).is_some() => {
                    if matches!(value.value, ExprNode::Struct { .. } | ExprNode::Slice(_)) {
                        out.push((stmt.id, name, ty));
                    }
                }
                _ => out.push((stmt.id, name, ty)),
            }
        }
        StmtNode::Block(stmts) => {
            for s in stmts { collect_locals(s, enums, out); }
        }
        StmtNode::If { then_branch, else_branch, .. } => {
            collect_locals(then_branch, enums, out);
            if let Some(e) = else_branch { collect_locals(e, enums, out); }
        }
        StmtNode::While { body, .. } => {
            collect_locals(body, enums, out);
        }
        StmtNode::Match { arms, .. } => {
            for (_pat, body) in arms { collect_locals(body, enums, out); }
        }
        // rest should have no declarations
        _ => {}
    }
}

/// Lower a global's initializer expression to a constant. Scalar literals
/// (optionally negated), struct/array literals of constants, and function names
/// are supported; the typechecker has already rejected anything else, so the
/// unreachable arms indicate a compiler bug. Struct/array field types are read
/// from `cx.structs` / `cx.node_types`.
fn lower_const_init<'a>(
    cx: &mut LowerCtx<'a>,
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
        ExprNode::Struct { name, fields, .. } => {
            let defs = cx.structs[name].clone();
            let mut inits = Vec::with_capacity(fields.len());
            for ((_, fty), (_, fexpr)) in defs.iter().zip(fields.iter()) {
                inits.push((fty.clone(), lower_const_init(cx, fexpr)));
            }
            ConstInit::Struct(inits)
        }
        // array literal: the element type comes from the inferred array type.
        ExprNode::Slice(elements) => {
            let elem_ty = match cx.node_types.get(&expr.id) {
                Some(Type::Array(inner, _)) => (**inner).clone(),
                other => unreachable!("array const initializer has non-array type: {other:?}"),
            };
            let mut inits = Vec::with_capacity(elements.len());
            for elem in elements {
                inits.push(lower_const_init(cx, elem));
            }
            ConstInit::Array(elem_ty, inits)
        }
        // an `Enum::Variant` in a const initializer is its discriminant constant.
        ExprNode::Var(name) if enum_const(&cx.enums, name).is_some() =>
            ConstInit::Scalar(enum_const(&cx.enums, name).unwrap()),
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
            // resolve enum names in the declared return type (raw parser type).
            let return_type = &resolve_enum_ty(&cx.enums, return_type);
            cx.current_return_type = return_type.clone();

            // struct / data-enum returns are lowered with an sret out-pointer;
            // allocate its register up front so `return` can copy into it.
            let sret = if let Some(name) = aggregate_struct_name(return_type) {
                let r = cx.fresh_reg();
                cx.sret_param = Some(r);
                Some((r, name))
            } else {
                cx.sret_param = None;
                None
            };

            let mut param_regs = Vec::new();
            for (name, ty) in params {
                // resolve enum names in the declared param type (raw parser type)
                // so an enum param takes the scalar path, not the by-value struct one.
                let ty = &resolve_enum_ty(&cx.enums, ty);
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
                } else if let Some(struct_name) = aggregate_struct_name(ty) {
                    // pass-by-value (struct or data enum): caller still hands us a
                    // `ptr` to its aggregate, but we copy into our own local slot on
                    // entry so mutations don't leak back. For pass-by-reference the
                    // user writes `*Stereo` explicitly - that falls through to the
                    // `_` arm and stays a plain pointer.
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
                collect_locals(stmt, &cx.enums, &mut locals);
            }
            for (decl_id, name, ty) in locals {
                let local_reg = cx.fresh_reg();
                cx.emit(Inst::Comment(format!("local {}: {}", name, ty)));
                match &ty {
                    // struct/array/data-enum literal locals are hoisted here
                    // (collect_locals) so the slot is allocated once, not per loop
                    // iteration.
                    Type::Array(inner, length) => {
                        cx.emit(Inst::AllocaArray { dst: local_reg, ty: (**inner).clone(), length: length.expect_lit() });
                    }
                    _ if aggregate_struct_name(&ty).is_some() => {
                        let struct_name = aggregate_struct_name(&ty).unwrap();
                        cx.emit(Inst::AllocaStruct { dst: local_reg, name: struct_name, align: None });
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
        TopLevelNode::Enum { .. } => (vec![], None),
    }
}

/// Whether `ty` is a bare reference to one of `type_params` (an unresolved
/// generic slot like `T`, which parses as `Type::Struct { name: "T", args: [] }`).
/// Used to erase a generic extern's type-param params into a variadic `...`.
fn is_generic_type(type_params: &[&str], ty: &Type<'_>) -> bool {
    matches!(ty, Type::Struct { name, args } if args.is_empty() && type_params.contains(name))
}

pub fn lower<'a>(
    program: &[TopLevel<'a>],
    typecheck_context: &crate::typecheck::Context<'a>,
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
        enums: typecheck_context.enums.clone(),
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
                    return_type: resolve_enum_ty(&cx.enums, return_type),
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
            TopLevelNode::Extern { name, attributes, generics, params, return_type, .. } => {
                // a generic extern's type-param slots (`arg: T`) have no concrete
                // layout, so drop them from the declaration and keep only the
                // concrete leading params. the call site still passes the concrete
                // args; the underlying C symbol (e.g. printf) accepts them.
                let type_params: Vec<&'a str> = generics.iter().filter_map(|g| match g {
                    GenericParam::Type(n) => Some(*n),
                    GenericParam::Const(_, _) => None,
                }).collect();
                let concrete_params = params.iter()
                    .filter(|(_, ty)| !is_generic_type(&type_params, ty))
                    .map(|(_, ty)| resolve_enum_ty(&cx.enums, ty))
                    .collect();
                externs.push(ExternDecl {
                    name,
                    attributes: attributes.clone(),
                    params: concrete_params,
                    return_type: resolve_enum_ty(&cx.enums, return_type),
                });
            }
            // generic struct templates carry `Param` fields and are never laid out
            // directly; monomorphization emits concrete instances. skip them here,
            // mirroring how generic functions are skipped above.
            TopLevelNode::Struct { generics, .. } if !generics.is_empty() => {}
            TopLevelNode::Struct { name, fields, .. } => {
                // resolve enum-typed fields to their integer repr; a bare enum name
                // otherwise emits an undefined `%EnumName` in the struct type.
                let fields = fields.iter()
                    .map(|(fname, fty)| (*fname, resolve_enum_ty(&cx.enums, fty)))
                    .collect();
                structs.push((*name, fields));
            }
            TopLevelNode::Global { name, attributes, ty, value, .. } => {
                let init = lower_const_init(&mut cx, value);
                globals.push(Global {
                    name,
                    export: attributes.iter().any(|a| a.value.name == "export"),
                    ty: ty.clone(),
                    init,
                });
            }
            // a field-less enum is erased to its integer repr and emits nothing. A
            // data enum emits its synthetic aggregate: the per-variant payload
            // structs (`Msg$Note`) followed by the tagged aggregate (`Msg`), both
            // built in typecheck's forward-decl pass and looked up here so the
            // backend renders them as ordinary `%Name = type { .. }` structs. Keys
            // carry `'a`, so fetch them via `get_key_value`.
            TopLevelNode::Enum { name, .. } => {
                if cx.enums[name].has_payload {
                    let variants: Vec<&'a str> = cx.enums[name].payloads.keys().copied().collect();
                    for v in variants {
                        let sname = crate::typecheck::enum_payload_struct_name(name, v);
                        if let Some((k, f)) = cx.structs.get_key_value(sname) {
                            structs.push((*k, f.clone()));
                        }
                    }
                    if let Some((k, f)) = cx.structs.get_key_value(*name) {
                        structs.push((*k, f.clone()));
                    }
                }
            }
        }
    }

    Module { functions, externs, structs, globals, strings: cx.strings }
}