use crate::ast::*;
use crate::mil::*;

// fn emit_float32(val: f32) -> String {
//     let bits = val.to_bits();
//     format!("0x{:08X}", bits)
// }

fn emit_value(val: Value) -> String {
    match val {
        Value::Const(Const::Float32(f)) => format!("0x{:016X}", (f as f64).to_bits()),
        Value::Const(Const::Bool(b)) => if b { "1".to_string() } else { "0".to_string() },
        _ => format!("{}", val),
    }
}

fn emit_type(ty: Type) -> String {
    use Type::*;

    match ty {
        Void => "void".to_string(),
        Bool => "i1".to_string(),
        Int32 => "i32".to_string(),
        Float32 => "float".to_string(),
        Pointer(_) => "ptr".to_string(),

        Slice(_) => "{ ptr, i32 }".to_string(), // struct { ptr, len }
        Function { .. } => todo!("function pointer type"),
        Defined(name) => todo!("defined types ({name})"),
    }
}

struct EmitCtx {
    buf: String,
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
                // UnaryOp::Neg if ty == Type::Float32 => "fneg float",
                UnaryOp::Neg => unreachable!(),
                UnaryOp::Not if ty == Type::Int32 => "xor i32 -1",
                UnaryOp::Not => "xor i1 1", // logical not
                // this should've be mapped to different instructions (mil.rs)
                UnaryOp::Deref | UnaryOp::AddrOf => unreachable!(),
            }, emit_value(val)),
        Binary { dst, op, lhs, rhs, ty } => {
            use BinaryOp::*;
            emitln!(cx, "    {dst} = {} {} {}, {}", match op {
                Add if ty == Type::Int32   => "add",
                Add if ty == Type::Float32 => "fadd",
                Add => unreachable!(),
                Sub if ty == Type::Int32   => "sub",
                Sub if ty == Type::Float32 => "fsub",
                Sub => unreachable!(),
                Mul if ty == Type::Int32   => "mul",
                Mul if ty == Type::Float32 => "fmul",
                Mul => unreachable!(),
                Div if ty == Type::Int32   => "sdiv",
                Div if ty == Type::Float32 => "fdiv",
                Div => unreachable!(),
                Mod if ty == Type::Int32   => "srem",
                Mod if ty == Type::Float32 => "frem",
                Mod => unreachable!(),
                Eq if ty == Type::Float32 => "fcmp oeq",
                Ne if ty == Type::Float32 => "fcmp one",
                Lt if ty == Type::Float32 => "fcmp olt",
                Le if ty == Type::Float32 => "fcmp ole",
                Gt if ty == Type::Float32 => "fcmp ogt",
                Ge if ty == Type::Float32 => "fcmp oge",
                Eq => "icmp eq",
                Ne => "icmp ne",
                Lt => "icmp slt",
                Le => "icmp sle",
                Gt => "icmp sgt",
                Ge => "icmp sge",
                And => "and",
                Or => "or",
                Xor => "xor",
            }, emit_type(ty), emit_value(lhs), emit_value(rhs));
        }
        Call { dst, func, args, return_type } => {
            // %result = call <return_type> @<function_name>(<arg_type> <arg_val>, ...)
            let dst_prefix = dst.map(|dst| format!("{dst} = ")).unwrap_or_default();
            let args_str = args.into_iter()
                .map(|(val, ty)| format!("{} {}", emit_type(ty), emit_value(val)))
                .collect::<Vec<_>>()
                .join(", ");
            emitln!(cx, "    {dst_prefix}call {} @{func}({args_str})", emit_type(return_type));
        }
        Index { dst, slice, index, element_ty } =>
            // %result = getelementptr <PointeeTy>, ptr <BasePtr> {, <IdxTy> <Idx> }*
            emitln!(cx, "    {dst} = getelementptr {}, ptr {slice}, i32 {index}", emit_type(element_ty)),
        InsertValue { dst, elem, ty, val, index } =>
            emitln!(cx, "    {dst} = insertvalue {{ ptr, i32 }} {elem}, {} {}, {index}", emit_type(ty), emit_value(val)),
        ExtractValue { dst, val, index } => emitln!(cx, "    {dst} = extractvalue {{ ptr, i32 }} {}, {index}", emit_value(val)),

        Alloca { dst, ty } =>  emitln!(cx, "    {dst} = alloca {}", emit_type(ty)),
        Store { ptr, val, ty } => emitln!(cx, "    store {} {}, ptr {ptr}", emit_type(ty), emit_value(val)),
        Load { dst, ptr, ty } => emitln!(cx, "    {dst} = load {}, ptr {ptr}", emit_type(ty)),

        AllocaArray { dst, ty, length } => emitln!(cx, "    {dst} = alloca [{} x {}]", length, emit_type(ty)),
        IndexArray { dst, ty, length, array, index } =>
            emitln!(cx, "    {dst} = getelementptr [{length} x {}], ptr {array}, i32 0, i32 {index}", emit_type(ty)),
    };
}

fn emit_terminator<'a>(cx: &mut EmitCtx, term: Terminator<'a>) {
    use Terminator::*;

    match term {
        Return(None) => emitln!(cx, "    ret void"),
        Return(Some((value, ty))) => emitln!(cx, "    ret {} {value}", emit_type(ty)),
        Jump(label) => emitln!(cx, "    br label %{label}"),
        Branch { cond, then_block, else_block } =>
            emitln!(cx, "    br i1 {cond}, label %{then_block}, label %{else_block}"),
    }
}

fn emit_block<'a>(cx: &mut EmitCtx, block: BasicBlock<'a>) {
    emitln!(cx, "  {}:", block.id);
    block.instructions.into_iter().for_each(|inst| emit_inst(cx, inst));
    emit_terminator(cx, block.terminator);
}

fn emit_function<'a>(cx: &mut EmitCtx, func: Function<'a>) {
    let attrs = func.attributes.iter()
        .filter_map(|a| match (a.value.name, a.value.value.as_deref()) {
            ("inline", Some("always")) => Some("alwaysinline"),
            ("inline", Some("never"))  => Some("noinline"),
            _ => None,
        }).collect::<Vec<_>>()
        .join(" ");
    let attrs_str = if attrs.is_empty() { String::new() } else { format!(" {attrs}") };

    let params_str = func.params.into_iter()
        .map(|(reg, ty)| format!("{} {reg}", emit_type(ty)))
        .collect::<Vec<_>>()
        .join(", ");
    emitln!(cx, "define {} @{}({params_str}){attrs_str} {{", emit_type(func.return_type), func.name);
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
        .map(emit_type)
        .collect::<Vec<_>>()
        .join(", ");
    emitln!(cx, "declare {} @{}({params_str}){attrs_str}", emit_type(ext.return_type), ext.name);
}

fn emit_module<'a>(cx: &mut EmitCtx, module: Module<'a>) {
    module.externs.into_iter().for_each(|ext| emit_extern(cx, ext));
    module.functions.into_iter().for_each(|func| emit_function(cx, func));
}

pub fn emit<'a>(module: Module<'a>) -> String {
    let mut cx = EmitCtx { buf: String::new() };
    emit_module(&mut cx, module);
    cx.buf
}