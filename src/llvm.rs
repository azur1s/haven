use crate::ast::*;
use crate::mil::*;

fn emit_value(val: Value) -> String {
    match val {
        Value::Const(Const::Float32(f)) => format!("0x{:016X}", (f as f64).to_bits()),
        Value::Const(Const::Float64(f)) => format!("0x{:016X}", f.to_bits()),
        Value::Const(Const::Bool(b)) => if b { "1".to_string() } else { "0".to_string() },
        _ => format!("{}", val),
    }
}

fn emit_type(ty: &Type) -> String {
    use Type::*;

    match ty {
        Void => "void".to_string(),
        Bool => "i1".to_string(),
        Int8 => "i8".to_string(),
        Int32 => "i32".to_string(),
        Int64 => "i64".to_string(),
        // LLVM just use one width for unsigned integers and rely on the
        // instructions (or us) to interpret them correctly
        Uint8 => "i8".to_string(),
        Uint32 => "i32".to_string(),
        Uint64 => "i64".to_string(),
        Float32 => "float".to_string(),
        Float64 => "double".to_string(),
        Pointer(_) => "ptr".to_string(),

        Array(t, n) => format!("[{} x {}]", n.expect_lit(), emit_type(t)),
        Slice(_) => "{ ptr, i32 }".to_string(), // struct { ptr, len }
        // `str` is a { ptr, len } fat pointer as a slice
        Str => "{ ptr, i32 }".to_string(),
        // <size x element_type>
        Simd(ty, size) => format!("<{} x {}>", size.expect_lit(), emit_type(ty)),
        // a function pointer is a plain opaque pointer
        Function { .. } => "ptr".to_string(),
        // struct values are always referenced via pointer in our codegen
        // (see lower_function and lower_expr for ExprNode::Struct)
        // the named LLVM type `%Name` is only used by AllocaStruct and FieldPtr,
        // which emit it directly without going through emit_type
        Struct(_) => "ptr".to_string(),
        // generic functions are skipped during MIL lowering, so a type param
        // should never reach codegen.
        Param(name) => unreachable!("generic type parameter `{name}` survived to LLVM codegen"),
    }
}

/// Layout type for a field *inside* a struct definition. Differs from
/// `emit_type` only for structs because a struct value is normally a `ptr`
/// handle, but as a field it is inlined as the named type `%Name`.
fn emit_field_type(ty: &Type) -> String {
    match ty {
        Type::Struct(name) => format!("%{name}"),
        Type::Array(t, n) => format!("[{} x {}]", n.expect_lit(), emit_field_type(t)),
        Type::Simd(t, n) => format!("<{} x {}>", n.expect_lit(), emit_field_type(t)),
        _ => emit_type(ty),
    }
}

#[derive(Debug, Clone, Copy)]
enum FastMathFlags {
    None,
    Fast,
    Reassoc,
    NNaN,
    NInf,
    NSZ,
    Arcp,
    Contract,
}

impl FastMathFlags {
    fn from_str(s: &str) -> Option<Self> {
        match s {
            "fast"     => Some(FastMathFlags::Fast),
            "reassoc"  => Some(FastMathFlags::Reassoc),
            "nnan"     => Some(FastMathFlags::NNaN),
            "ninf"     => Some(FastMathFlags::NInf),
            "nsz"      => Some(FastMathFlags::NSZ),
            "arcp"     => Some(FastMathFlags::Arcp),
            "contract" => Some(FastMathFlags::Contract),
            _ => None,
        }
    }

    fn to_str(&self) -> &'static str {
        match self {
            FastMathFlags::None     => "",
            FastMathFlags::Fast     => "fast",
            FastMathFlags::Reassoc  => "reassoc",
            FastMathFlags::NNaN     => "nnan",
            FastMathFlags::NInf     => "ninf",
            FastMathFlags::NSZ      => "nsz",
            FastMathFlags::Arcp     => "arcp",
            FastMathFlags::Contract => "contract",
        }
    }
}

struct EmitCtx {
    buf: String,
    current_fast_math_flags: FastMathFlags,
}

impl EmitCtx {
    fn emit(&mut self, str: String) {
        self.buf.push_str(&str);
    }
}

macro_rules! emitln {
    ($cx:expr, $($arg:tt)*) => {{
        $cx.emit(format!($($arg)*));
        $cx.emit("\n".to_string())
    }};
}

fn emit_inst<'a>(cx: &mut EmitCtx, inst: Inst<'a>) {
    use Inst::*;

    match inst {
        Comment(s) => emitln!(cx, "    ; {}", s),
        Unary { dst, op: UnaryOp::Neg, val, ty: Type::Float32 } =>
            emitln!(cx, "    {dst} = fneg float {}", emit_value(val)),
        Unary { dst, op, val, ty } =>
            emitln!(cx, "    {dst} = {}, {}", match op {
                UnaryOp::Neg if ty == Type::Int32 => "sub i32 0",
                UnaryOp::Neg if ty == Type::Int64 => "sub i64 0",

                // UnaryOp::Neg if ty == Type::Float32 => "fneg float",
                UnaryOp::Neg => unreachable!(),
                UnaryOp::Not if ty == Type::Int32 => "xor i32 -1",
                UnaryOp::Not => "xor i1 1", // logical not
                // this should've be mapped to different instructions (mil.rs)
                UnaryOp::Deref | UnaryOp::AddrOf => unreachable!(),
            }, emit_value(val)),
        Binary { dst, op, lhs, rhs, ty } => {
            use BinaryOp::*;

            // extract SIMD inner type for instruction selection
            let inner = match &ty {
                Type::Simd(inner, _) => inner.as_ref(),
                _ => &ty,
            };

            let is_float = matches!(inner, Type::Float32 | Type::Float64);
            let is_signed_int = matches!(inner, Type::Int32 | Type::Int64);
            let is_unsigned_int = matches!(inner, Type::Uint32 | Type::Uint64);

            let flags_str = if *inner == Type::Float32 || *inner == Type::Float64 {
                format!(" {}", cx.current_fast_math_flags.to_str())
            } else {
                String::new()
            };

            emitln!(cx, "    {dst} = {}{flags_str} {} {}, {}", match op {
                Add if is_signed_int || is_unsigned_int => "add",
                Add if is_float => "fadd",
                Add => unreachable!("{}", ty),

                Sub if is_signed_int || is_unsigned_int => "sub",
                Sub if is_float => "fsub",
                Sub => unreachable!("{}", ty),

                Mul if is_signed_int || is_unsigned_int => "mul",
                Mul if is_float => "fmul",
                Mul => unreachable!("{}", ty),

                Div if is_signed_int => "sdiv",
                Div if is_unsigned_int => "udiv",
                Div if is_float => "fdiv",
                Div => unreachable!("{}", ty),

                Mod if is_signed_int => "srem",
                Mod if is_unsigned_int => "urem",
                Mod if is_float => "frem",
                Mod => unreachable!("{}", ty),

                Eq if is_float => "fcmp oeq",
                Ne if is_float => "fcmp one",
                Lt if is_float => "fcmp olt",
                Le if is_float => "fcmp ole",
                Gt if is_float => "fcmp ogt",
                Ge if is_float => "fcmp oge",
                Eq => "icmp eq",
                Ne => "icmp ne",

                Lt if is_signed_int => "icmp slt",
                Le if is_signed_int => "icmp sle",
                Gt if is_signed_int => "icmp sgt",
                Ge if is_signed_int => "icmp sge",

                Lt if is_unsigned_int => "icmp ult",
                Le if is_unsigned_int => "icmp ule",
                Gt if is_unsigned_int => "icmp ugt",
                Ge if is_unsigned_int => "icmp uge",

                Lt | Le | Gt | Ge => unreachable!("{}", ty),

                And => "and",
                Or => "or",
                Xor => "xor",
            }, emit_type(&ty), emit_value(lhs), emit_value(rhs));
        }
        Call { dst, callee, args, return_type, sret } => {
            // %result = call <return_type> <callee>(<arg_type> <arg_val>, ...)
            // where <callee> is either a function symbol @name or a fn-pointer %reg
            let callee_str = match callee {
                Callee::Direct(name) => format!("@{name}"),
                Callee::Indirect(val) => emit_value(val),
            };
            let args_str = args.into_iter()
                .map(|(val, ty)| format!("{} {}", emit_type(&ty), emit_value(val)))
                .collect::<Vec<_>>()
                .join(", ");
            match sret {
                // struct return: void call with the result slot as a leading sret arg
                Some((slot, name)) => {
                    let all_args = if args_str.is_empty() {
                        format!("ptr sret(%{name}) {slot}")
                    } else {
                        format!("ptr sret(%{name}) {slot}, {args_str}")
                    };
                    emitln!(cx, "    call void {callee_str}({all_args})");
                }
                None => {
                    let dst_prefix = dst.map(|dst| format!("{dst} = ")).unwrap_or_default();
                    emitln!(cx, "    {dst_prefix}call {} {callee_str}({args_str})", emit_type(&return_type));
                }
            }
        }
        Index { dst, slice, index, element_ty } =>
            // %result = getelementptr <PointeeTy>, ptr <BasePtr> {, <IdxTy> <Idx> }*
            emitln!(cx, "    {dst} = getelementptr {}, ptr {slice}, i32 {index}", emit_type(&element_ty)),
        InsertValue { dst, elem, ty, val, index } =>
            emitln!(cx, "    {dst} = insertvalue {{ ptr, i32 }} {elem}, {} {}, {index}", emit_type(&ty), emit_value(val)),
        ExtractValue { dst, val, index } => emitln!(cx, "    {dst} = extractvalue {{ ptr, i32 }} {}, {index}", emit_value(val)),

        Alloca { dst, ty, align } =>  emitln!(cx, "    {dst} = alloca {}, align {}", emit_type(&ty), align.unwrap_or(1)),
        Store { ptr, val, ty, align } => emitln!(cx, "    store {} {}, ptr {ptr}, align {}", emit_type(&ty), emit_value(val), align.unwrap_or(1)),
        Load { dst, ptr, ty, align } => emitln!(cx, "    {dst} = load {}, ptr {ptr}, align {}", emit_type(&ty), align.unwrap_or(1)),

        AllocaArray { dst, ty, length } => emitln!(cx, "    {dst} = alloca [{} x {}]", length, emit_type(&ty)),
        IndexArray { dst, ty, length, array, index } =>
            emitln!(cx, "    {dst} = getelementptr [{length} x {}], ptr {array}, i32 0, i32 {index}", emit_type(&ty)),

        AllocaStruct { dst, name, align } =>
            emitln!(cx, "    {dst} = alloca %{name}, align {}", align.unwrap_or(1)),
        FieldPtr { dst, struct_name, base, field_index } =>
            emitln!(cx, "    {dst} = getelementptr %{struct_name}, ptr {base}, i32 0, i32 {field_index}"),
        // a zero-offset gep off the global symbol yields its address as a `ptr`
        GlobalPtr { dst, name } =>
            emitln!(cx, "    {dst} = getelementptr i8, ptr @{name}, i64 0"),

        Sizeof { dst, ty } => {
            // classic LLVM sizeof: index one element past a null base, then
            // reinterpret the resulting address as an integer. The target data
            // layout decides the stride, so struct padding/alignment is exact.
            let elem = emit_field_type(&ty);
            emitln!(cx, "    {dst}.szp = getelementptr {elem}, ptr null, i32 1");
            emitln!(cx, "    {dst} = ptrtoint ptr {dst}.szp to i64");
        }
        Extend { dst, val, from_ty, to_ty } => {
            use Type::*;
            let value = emit_value(val);
            // die typkonvertierungstabelle
            match (from_ty, to_ty) {
                // same-type or identity casts (no-op)
                (Int32, Int32) | (Int32, Uint32) | (Uint32, Int32) | (Uint32, Uint32) => emitln!(cx, "    {dst} = bitcast i32 {} to i32", value),
                (Int64, Int64) | (Int64, Uint64) | (Uint64, Int64) | (Uint64, Uint64) => emitln!(cx, "    {dst} = bitcast i64 {} to i64", value),
                (Float32, Float32) => emitln!(cx, "    {dst} = bitcast float {} to float", value),
                (Float64, Float64) => emitln!(cx, "    {dst} = bitcast double {} to double", value),

                // downcasting
                (Int64, Int32) | (Int64, Uint32)   => emitln!(cx, "    {dst} = trunc i64 {} to i32", value),
                (Uint64, Int32) | (Uint64, Uint32) => emitln!(cx, "    {dst} = trunc i64 {} to i32", value),

                // upcasting/extension
                // signed source -> sign extension
                (Int32, Int64) | (Int32, Uint64) => emitln!(cx, "    {dst} = sext i32 {} to i64", value),
                // unsigned source -> zero extension
                (Uint32, Int64) | (Uint32, Uint64) => emitln!(cx, "    {dst} = zext i32 {} to i64", value),

                // float to float
                (Float32, Float64) => emitln!(cx, "    {dst} = fpext float {} to double", value),
                (Float64, Float32) => emitln!(cx, "    {dst} = fptrunc double {} to float", value),

                // integer to float
                (Int32, Float32) => emitln!(cx, "    {dst} = sitofp i32 {} to float", value),
                (Int32, Float64) => emitln!(cx, "    {dst} = sitofp i32 {} to double", value),
                (Int64, Float32) => emitln!(cx, "    {dst} = sitofp i64 {} to float", value),
                (Int64, Float64) => emitln!(cx, "    {dst} = sitofp i64 {} to double", value),

                (Uint32, Float32) => emitln!(cx, "    {dst} = uitofp i32 {} to float", value),
                (Uint32, Float64) => emitln!(cx, "    {dst} = uitofp i32 {} to double", value),
                (Uint64, Float32) => emitln!(cx, "    {dst} = uitofp i64 {} to float", value),
                (Uint64, Float64) => emitln!(cx, "    {dst} = uitofp i64 {} to double", value),

                // float to integer (saturating)
                // this is like Rust where it defaults to saturating casts (`@llvm.fptosi.sat` / `@llvm.fptoui.sat`)
                // to prevent UB if a float overflows the target integer type (i hope)
                (Float32, Int32)  => emitln!(cx, "    {dst} = call i32 @llvm.fptosi.sat.i32.f32(float {value})"),
                (Float32, Uint32) => emitln!(cx, "    {dst} = call i32 @llvm.fptoui.sat.i32.f32(float {value})"),
                (Float32, Int64)  => emitln!(cx, "    {dst} = call i64 @llvm.fptosi.sat.i64.f32(float {value})"),
                (Float32, Uint64) => emitln!(cx, "    {dst} = call i64 @llvm.fptoui.sat.i64.f32(float {value})"),

                (Float64, Int32)  => emitln!(cx, "    {dst} = call i32 @llvm.fptosi.sat.i32.f64(double {value})"),
                (Float64, Uint32) => emitln!(cx, "    {dst} = call i32 @llvm.fptoui.sat.i32.f64(double {value})"),
                (Float64, Int64)  => emitln!(cx, "    {dst} = call i64 @llvm.fptosi.sat.i64.f64(double {value})"),
                (Float64, Uint64) => emitln!(cx, "    {dst} = call i64 @llvm.fptoui.sat.i64.f64(double {value})"),

                (f, t) => unreachable!("unsupported type extension from {f:?} to {t:?}"),
            };
            // emitln!(cx, "    {dst} = {inst} {from_ty_str} {} to {to_ty_str}", emit_value(val));
        }
        Splat { dst, val, ty, size } => {
            let ty_str = emit_type(&ty);
            let simd_ty = format!("<{size} x {}>", ty_str);
            emitln!(cx, "    {dst} = insertelement {simd_ty} undef, {ty_str} {}, i32 0", emit_value(val));
        }
        Shuffle { dst, value_size, v0, v1, ty, size, mask } => {
            let ty_str = emit_type(&ty);
            let simd_ty = format!("<{} x {}>", value_size, ty_str);
            let mask = mask.into_iter().map(|i| format!("i32 {}", i)).collect::<Vec<_>>().join(", ");
            emitln!(cx, "    {dst} = shufflevector {simd_ty} {v0}, {simd_ty} {v1}, <{size} x i32> <{mask}>");
        }
    };
}

fn emit_terminator<'a>(cx: &mut EmitCtx, term: Terminator<'a>) {
    use Terminator::*;

    match term {
        Return(None) => emitln!(cx, "    ret void"),
        Return(Some((value, ty))) => emitln!(cx, "    ret {} {}", emit_type(&ty), emit_value(value)),
        Jump(label) => emitln!(cx, "    br label %{label}"),
        Branch { cond, then_block, else_block } =>
            emitln!(cx, "    br i1 {cond}, label %{then_block}, label %{else_block}"),
    }
}

fn emit_block<'a>(cx: &mut EmitCtx, block: BasicBlock<'a>) {
    emitln!(cx, "  {}:", block.id);
    block.instructions.into_iter().for_each(|inst| emit_inst(cx, inst));
    if let Some(terminator) = block.terminator {
        emit_terminator(cx, terminator);
    } else {
        panic!("block {} has no terminator", block.id);
    }
}

fn emit_function<'a>(cx: &mut EmitCtx, func: Function<'a>) {
    let mut export = false;

    let attrs = func.attributes.iter()
        .filter_map(|a| match (a.value.name, a.value.value.as_deref()) {
            ("inline", Some("always")) => Some(String::from("alwaysinline")),
            ("inline", Some("never"))  => Some(String::from("noinline")),
            ("inline", _) => panic!("invalid inline attribute value (got {:?})", a.value.value),

            ("export", None) => { export = true; None },
            ("export", _) => panic!("invalid export attribute value (got {:?})", a.value.value),

            ("fastmath", Some(flag)) => {
                cx.current_fast_math_flags = match FastMathFlags::from_str(flag) {
                    Some(f) => f,
                    None => panic!("invalid fastmath attribute value (got {:?})", a.value.value),
                };

                None
            }
            ("fastmath", _) => panic!("invalid fastmath attribute value (got {:?})", a.value.value),

            _ => None,
        }).collect::<Vec<_>>()
        .join(" ");
    let linkage = if export { "dso_local dllexport" } else { "internal" };
    let attrs_str = if attrs.is_empty() { String::new() } else { format!(" {attrs}") };

    let params_str = func.params.into_iter()
        .map(|(reg, ty)| format!("{} {reg}", emit_type(&ty)))
        .collect::<Vec<_>>()
        .join(", ");

    // struct-returning functions return void and take a hidden leading sret param
    let (ret_ty_str, params_str) = match &func.sret {
        Some((reg, name)) => {
            let sret_param = format!("ptr sret(%{name}) {reg}");
            let params = if params_str.is_empty() {
                sret_param
            } else {
                format!("{sret_param}, {params_str}")
            };
            ("void".to_string(), params)
        }
        None => (emit_type(&func.return_type), params_str),
    };
    emitln!(cx, "define {linkage} {ret_ty_str} @{}({params_str}){attrs_str} {{", func.name);
    func.blocks.into_iter().for_each(|block| emit_block(cx, block));
    emitln!(cx, "}}");
}

fn emit_extern<'a>(cx: &mut EmitCtx, ext: ExternDecl<'a>) {
    let attrs = ext.attributes.iter()
        .filter_map(|a| match (a.value.name, a.value.value.as_deref()) {
            ("inline", Some("always")) => Some("alwaysinline"),
            ("inline", Some("never"))  => Some("noinline"),
            _ => None,
        }).collect::<Vec<_>>()
        .join(" ");
    let attrs_str = if attrs.is_empty() { String::new() } else { format!(" {attrs}") };

    let params_str = ext.params.into_iter()
        .map(|ty| emit_type(&ty))
        .collect::<Vec<_>>()
        .join(", ");
    emitln!(cx, "declare {} @{}({params_str}){attrs_str}", emit_type(&ext.return_type), ext.name);
}

/// Emit a byte blob as the body of an LLVM `c"..."` string constant.
/// Printable ASCII passes through and everything else is emitted as a `\XX` hex escape.
fn emit_string_blob(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len());
    for &b in bytes {
        if b == b'"' || b == b'\\' || b < 0x20 || b > 0x7e {
            out.push_str(&format!("\\{:02X}", b));
        } else {
            out.push(b as char);
        }
    }
    out
}

/// Render a global's constant initializer as an LLVM constant expression (the
/// text after the type in `@g = constant <ty> <init>`).
fn emit_const_init(init: &ConstInit) -> String {
    match init {
        ConstInit::Scalar(c) => emit_value(Value::Const(c.clone())),
        // `{ <fty> <finit>, ... }` — the surrounding type (`%Name`) is emitted by
        // the caller, and each field carries its own inline type.
        ConstInit::Struct(fields) => {
            let body = fields.iter()
                .map(|(ty, init)| format!("{} {}", emit_field_type(ty), emit_const_init(init)))
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{ {body} }}")
        }
        // a function's address is written as the bare symbol
        ConstInit::FnAddr(name) => format!("@{name}"),
    }
}

fn emit_module<'a>(cx: &mut EmitCtx, module: Module<'a>) {
    // read-only global blobs backing string literals (@.str.N)
    for (i, bytes) in module.strings.iter().enumerate() {
        emitln!(cx, "@.str.{i} = private unnamed_addr constant [{} x i8] c\"{}\"",
            bytes.len(), emit_string_blob(bytes));
    }
    if !module.strings.is_empty() {
        emitln!(cx, "");
    }

    // typecheck rejected unknown field types so there can be no forward reference
    // to a not yet declared struct
    for (name, fields) in &module.structs {
        let body = fields.iter()
            .map(|(_, ty)| emit_field_type(ty))
            .collect::<Vec<_>>()
            .join(", ");
        emitln!(cx, "%{name} = type {{ {body} }}");
    }
    if !module.structs.is_empty() {
        emitln!(cx, "");
    }

    // module-level constants. `@export` gets external (dllexport) linkage so a
    // host can resolve the symbol; everything else stays `internal`.
    for g in &module.globals {
        let linkage = if g.export { "dso_local dllexport constant" } else { "internal constant" };
        emitln!(cx, "@{} = {linkage} {} {}",
            g.name, emit_field_type(&g.ty), emit_const_init(&g.init));
    }
    if !module.globals.is_empty() {
        emitln!(cx, "");
    }

    module.externs.into_iter().for_each(|ext| emit_extern(cx, ext));
    module.functions.into_iter().for_each(|func| emit_function(cx, func));
}

// I hope LLVM will optimize these away if it's not used
const PREPEND: &str = r#"
; casts (for numeric_cast)
declare i32 @llvm.fptosi.sat.i32.f32(float)
declare i32 @llvm.fptoui.sat.i32.f32(float)
declare i64 @llvm.fptosi.sat.i64.f32(float)
declare i64 @llvm.fptoui.sat.i64.f32(float)
declare i32 @llvm.fptosi.sat.i32.f64(double)
declare i32 @llvm.fptoui.sat.i32.f64(double)
declare i64 @llvm.fptosi.sat.i64.f64(double)
declare i64 @llvm.fptoui.sat.i64.f64(double)

"#;

pub fn emit<'a>(module: Module<'a>) -> String {
    let mut header = format!("; ModuleID = 'compiled_module'\n");
    header.push_str(PREPEND.trim_start());

    let mut cx = EmitCtx {
        buf: header,
        current_fast_math_flags: FastMathFlags::None,
    };
    emit_module(&mut cx, module);
    cx.buf
}