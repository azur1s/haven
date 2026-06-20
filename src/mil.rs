use std::collections::HashMap;
use std::fmt::{Display, Formatter};

use crate::ast::*;
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
    Int32(i32),
    Int64(i64),
    Uint32(u32),
    Uint64(u64),
    Float32(f32),
    Float64(f64),
}

impl Display for Const {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Const::Undef => write!(f, "undef"),
            Const::Bool(b) => write!(f, "{}", b),
            Const::Int32(n) => write!(f, "{}", n),
            Const::Int64(n) => write!(f, "{}", n),
            Const::Uint32(n) => write!(f, "{}", n),
            Const::Uint64(n) => write!(f, "{}", n),
            Const::Float32(n)=> write!(f, "{:?}", n),
            Const::Float64(n)=> write!(f, "{:?}", n),
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

#[derive(Clone, Debug)]
pub enum Inst<'a> {
    Comment(String),

    // %dst = <unary op> %val
    Unary { dst: Register, op: UnaryOp, val: Value, ty: Type<'a> },
    // %dst = <binary op> %lhs, %rhs
    Binary { dst: Register, op: BinaryOp, ty: Type<'a>, lhs: Value, rhs: Value },

    // %dst = call func(args...)
    Call {
        dst: Option<Register>, // None if return type is void
        func: &'a str,
        args: Vec<(Value, Type<'a>)>,
        return_type: Type<'a>,
    },

    // %dst = getelemptr %slice, %index (for slice indexing)
    // using Register here because slice will be { ptr, len } fat pointer struct
    Index { dst: Register, slice: Register, index: Value, element_ty: Type<'a> },
    // %dst = insertvalue {{ ptr, i32 }} %elem, ty %val, %index
    InsertValue { dst: Register, elem: Value, ty: Type<'a>, val: Value, index: usize },
    // %dst = extractvalue %val, index (for extracting from fat pointer struct, e.g. data pointer or length)
    ExtractValue { dst: Register, val: Value, index: usize },

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

    // Intrinsic/SIMD related instructions
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
}

#[derive(Clone, Debug)]
pub struct Module<'a> {
    pub functions: Vec<Function<'a>>,
    pub externs: Vec<ExternDecl<'a>>,
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

    pub env: HashMap<&'a str, (Register, Type<'a>)>,
    pub node_types: HashMap<usize, Type<'a>>, // from typecheck
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

    pub fn emit(&mut self, inst: Inst<'a>) {
        let block = self.blocks.iter_mut().find(|b| b.id == self.current_block).unwrap();
        block.instructions.push(inst);
    }

    pub fn terminate(&mut self, term: Terminator<'a>) {
        let block = self.blocks.iter_mut().find(|b| b.id == self.current_block).unwrap();
        block.terminator = Some(term);
    }
}

fn lower_intrinsic<'a>(
    cx: &mut LowerCtx<'a>,
    intrinsic: Intrinsic,
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
            let val = lower_expr(cx, &args[0]);
            let from_ty = cx.node_types[&args[0].id].clone();
            let to_ty = match &args[1].value {
                ExprNode::Var("i32") => Type::Int32,
                ExprNode::Var("i64") => Type::Int64,
                ExprNode::Var("u32") => Type::Uint32,
                ExprNode::Var("u64") => Type::Uint64,
                ExprNode::Var("f32") => Type::Float32,
                ExprNode::Var("f64") => Type::Float64,
                _ => unreachable!(),
            };
            let dst = cx.fresh_reg();
            cx.emit(Inst::Extend { dst, val, from_ty, to_ty });
            Value::Reg(dst)
        }
        Intrinsic::SimdSplat => {
            let ty = match &args[0].value {
                ExprNode::Var(name) => match *name {
                    "i32" => Type::Int32,
                    "i64" => Type::Int64,
                    "u32" => Type::Uint32,
                    "u64" => Type::Uint64,
                    "f32" => Type::Float32,
                    "f64" => Type::Float64,
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            let size = match &args[1].value {
                ExprNode::Int32(n) => *n as usize,
                _ => unreachable!(),
            };
            let value_val = lower_expr(cx, &args[2]);

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
            // simd_load(T, N, slice, offset) -> T where T = simd[T, N]
            let ty = match &args[0].value {
                ExprNode::Var(name) => match *name {
                    "i32" => Type::Int32,
                    "i64" => Type::Int64,
                    "u32" => Type::Uint32,
                    "u64" => Type::Uint64,
                    "f32" => Type::Float32,
                    "f64" => Type::Float64,
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            let size = match &args[1].value {
                ExprNode::Int32(n) => *n as usize,
                _ => unreachable!(),
            };
            let slice_val = lower_expr(cx, &args[2]);
            let offset_val = lower_expr(cx, &args[3]);

            cx.emit(Inst::Comment(format!("simd_load")));
            // extract the data pointer from the fat pointer struct, or use directly if it's already a pointer
            let data_ptr = match cx.node_types[&args[2].id] {
                Type::Slice(_) => {
                    let extracted = cx.fresh_reg();
                    cx.emit(Inst::ExtractValue { dst: extracted, val: slice_val.clone(), index: 0 });
                    extracted
                }
                Type::Pointer(_) => {
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
            cx.emit(Inst::Load { dst, ptr: elem_ptr, ty: Type::Simd(Box::new(ty), size), align: None });
            Value::Reg(dst)
        }
        Intrinsic::SimdStore => {
            // simd_store(T, N, slice, offset, value) -> () where T = simd[T, N]
            let ty = match &args[0].value {
                ExprNode::Var(name) => match *name {
                    "i32" => Type::Int32,
                    "i64" => Type::Int64,
                    "u32" => Type::Uint32,
                    "u64" => Type::Uint64,
                    "f32" => Type::Float32,
                    "f64" => Type::Float64,
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            let size = match &args[1].value {
                ExprNode::Int32(n) => *n as usize,
                _ => unreachable!(),
            };
            let slice_val = lower_expr(cx, &args[2]);
            let offset_val = lower_expr(cx, &args[3]);
            let value_val = lower_expr(cx, &args[4]);

            // like simd_load
            let data_ptr = match cx.node_types[&args[2].id] {
                Type::Slice(_) => {
                    let extracted = cx.fresh_reg();
                    cx.emit(Inst::ExtractValue { dst: extracted, val: slice_val.clone(), index: 0 });
                    extracted
                }
                Type::Pointer(_) => {
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
            cx.emit(Inst::Store { ptr: elem_ptr, val: value_val, ty: Type::Simd(Box::new(ty), size), align: None });
            Value::Const(Const::Undef) // placeholder since void return
        }
        Intrinsic::SimdConcat => {
            let ty = match &args[0].value {
                ExprNode::Var(name) => match *name {
                    "i32" => Type::Int32,
                    "i64" => Type::Int64,
                    "u32" => Type::Uint32,
                    "u64" => Type::Uint64,
                    "f32" => Type::Float32,
                    "f64" => Type::Float64,
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            let size = match &args[1].value {
                ExprNode::Int32(n) => *n as usize,
                _ => unreachable!(),
            };
            let value1_val = lower_expr(cx, &args[2]);
            let value2_val = lower_expr(cx, &args[3]);
            let dst = cx.fresh_reg();
            cx.emit(Inst::Comment(format!("simd_concat({}, {}, ..., ...)", ty, size)));
            cx.emit(Inst::Shuffle {
                dst,
                value_size: match cx.node_types[&args[2].id] {
                    Type::Simd(_, s) => s,
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
            // simd_low(T, N, value) -> T where T = simd[T, N]
            let ty = match &args[0].value {
                ExprNode::Var(name) => match *name {
                    "i32" => Type::Int32,
                    "i64" => Type::Int64,
                    "u32" => Type::Uint32,
                    "u64" => Type::Uint64,
                    "f32" => Type::Float32,
                    "f64" => Type::Float64,
                    _ => unreachable!(),
                },
                _ => unreachable!(),
            };
            let size = match &args[1].value {
                ExprNode::Int32(n) => *n as usize,
                _ => unreachable!(),
            };
            let value_val = lower_expr(cx, &args[2]);

            let dst = cx.fresh_reg();
            cx.emit(Inst::Comment(format!("{}({}, {}, ...)", intrinsic, ty, size)));
            cx.emit(Inst::Shuffle {
                dst,
                value_size: match cx.node_types[&args[2].id] {
                    Type::Simd(_, s) => s,
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
        ExprNode::Int32(n)   => Value::Const(Const::Int32(*n)),
        ExprNode::Int64(n)   => Value::Const(Const::Int64(*n)),
        ExprNode::Uint32(n)  => Value::Const(Const::Uint32(*n)),
        ExprNode::Uint64(n)  => Value::Const(Const::Uint64(*n)),
        ExprNode::Float32(n) => Value::Const(Const::Float32(*n)),
        ExprNode::Float64(n) => Value::Const(Const::Float64(*n)),

        ExprNode::Var(name) => {
            let (reg, ty) = cx.env[name].clone();
            // match ty {
            //     Type::Pointer(_) => {
                    let dst = cx.fresh_reg();
                    cx.emit(Inst::Load { dst, ptr: reg, ty, align: None });
                    Value::Reg(dst)
            //     }
            //     _ => Value::Reg(reg),
            // }
        }

        // use fat pointer struct for slices
        ExprNode::Slice(elements) if elements.len() == 0 => {
            todo!()
        },
        ExprNode::Slice(elements) => {
            let ty = match cx.node_types[&expr.id].clone() {
                Type::Slice(inner) => *inner,
                _ => unreachable!(),
            };

            // %arr_reg = alloca [N x T]
            let arr_reg = cx.fresh_reg();
            cx.emit(Inst::AllocaArray {
                dst: arr_reg,
                ty: ty.clone(),
                length: elements.len(),
            });

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

        ExprNode::Binary { op, left, right } => {
            let lhs = lower_expr(cx, left);
            let rhs = lower_expr(cx, right);
            let ty = cx.node_types[&left.id].clone();
            let dst = cx.fresh_reg();
            cx.emit(Inst::Binary { dst, op: *op, lhs, rhs, ty });
            Value::Reg(dst)
        }

        ExprNode::Call { func, args }
            if matches!(&func.value, ExprNode::Var(name)
                if Intrinsic::lookup(name).is_some()) => {
            let ExprNode::Var(name) = &func.value else { unreachable!() };
            let intrinsic = Intrinsic::lookup(name).unwrap();
            lower_intrinsic(cx, intrinsic, args)
        }

        ExprNode::Call { func, args } => {
            cx.emit(Inst::Comment(format!("call {}(...)", func.value)));
            let name = match &func.value {
                ExprNode::Var(name) => name,
                _ => todo!("function pointers in calls"),
            };
            let lowered_args = args.iter().map(|arg| {
                let ty = cx.node_types[&arg.id].clone();
                (lower_expr(cx, arg), ty)
            }).collect();

            let return_type = cx.node_types[&expr.id].clone();
            if return_type == Type::Void {
                cx.emit(Inst::Call { dst: None, func: name, args: lowered_args, return_type });
                Value::Const(Const::Bool(false)) // placeholder
            } else {
                let dst = cx.fresh_reg();
                cx.emit(Inst::Call { dst: Some(dst), func: name, args: lowered_args, return_type });
                Value::Reg(dst)
            }
        }

        ExprNode::Index { slice, index } => {
            let slice_val = lower_expr(cx, slice);
            let index_val = lower_expr(cx, index);
            let element_ty = match &cx.node_types[&slice.id] {
                Type::Slice(inner) => *inner.clone(),
                Type::Pointer(inner) => *inner.clone(),
                _ => unreachable!(),
            };

            let data_ptr = match cx.node_types[&slice.id] {
                Type::Slice(_) => {
                    let extracted = cx.fresh_reg();
                    cx.emit(Inst::ExtractValue { dst: extracted, val: slice_val.clone(), index: 0 });
                    extracted
                }
                Type::Pointer(_) => {
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
        ExprNode::Var(name) => cx.env[name].0,

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
                _ => unreachable!(),
            };

            // same extraction as rvalue
            let data_ptr = match cx.node_types[&slice.id] {
                Type::Slice(_) => {
                    let extracted = cx.fresh_reg();
                    cx.emit(Inst::ExtractValue { dst: extracted, val: slice_val.clone(), index: 0 });
                    extracted
                }
                Type::Pointer(_) => {
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

        StmtNode::Declare { name, ty, value } => {
            let val = lower_expr(cx, value);
            let (ptr, _) = cx.env[name]; // already alloca'd
            cx.emit(Inst::Store { ptr, val, ty: ty.clone(), align: None });
        }

        StmtNode::Assign { left, value } => {
            let ptr = lower_lvalue(cx, left);  // see below
            let val = lower_expr(cx, value);
            let ty = cx.node_types[&left.id].clone();
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
            let ty = cx.node_types[&expr.id].clone();
            cx.terminate(Terminator::Return(Some((val, ty))));
        }
    }
}

/// Recursively collect all local variable declarations in the function body,
/// including nested ones in blocks and branches.
/// This is for emitting all Alloca instructions upfront in the entry block
fn collect_locals<'a>(stmt: &Stmt<'a>, out: &mut Vec<(&'a str, Type<'a>)>) {
    match &stmt.value {
        StmtNode::Declare { name, ty, .. } => {
            out.push((name, ty.clone()));
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

fn lower_function<'a>(cx: &mut LowerCtx<'a>, func: &TopLevel<'a>)
-> Vec<(Register, Type<'a>)> {
    match &func.value {
        TopLevelNode::Function { name, attributes, params, return_type, body, .. } => {
            let entry = cx.fresh_block();
            cx.current_block = entry;

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
                    cx.env.insert(name, (param_ptr, ty.clone()));

                    // push BOTH as incoming params
                    param_regs.push((ptr_reg, Type::Pointer(Box::new(inner_ty.clone()))));
                    param_regs.push((len_reg, Type::Int32));
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
                    cx.env.insert(name, (param_ptr, ty.clone()));
                }
            }

            // collect all local variable declarations in the function body and emit Allocas for them
            // TODO handle shadowing since this will collide on the same name
            let mut locals = Vec::new();
            for stmt in body {
                collect_locals(stmt, &mut locals);
            }
            for (name, ty) in locals {
                let local_reg = cx.fresh_reg();
                cx.emit(Inst::Comment(format!("local {}: {}", name, ty)));
                cx.emit(Inst::Alloca { dst: local_reg, ty: ty.clone(), align: None });
                cx.env.insert(name, (local_reg, ty.clone()));
            }

            cx.emit(Inst::Comment("function body".to_string()));

            for stmt in body {
                lower_stmt(cx, stmt);
            }

            if cx.blocks.iter().find(|b| b.id == cx.current_block).unwrap().terminator.is_none() {
                if *return_type == Type::Void {
                    cx.terminate(Terminator::Return(None));
                } else {
                    // should've been caught by typechecker, so if we reach here
                    // it's a compiler bug
                    panic!("non-void function {} missing return statement", name);
                }
            }

            param_regs
        }
        TopLevelNode::Extern { .. } => vec![],
    }
}

pub fn lower<'a>(program: &[TopLevel<'a>], node_types: HashMap<usize, Type<'a>>) -> Module<'a> {
    let mut cx = LowerCtx {
        reg_counter: 0,
        block_counter: 0,
        current_block: BlockId(0),
        blocks: vec![],
        loop_stack: vec![],
        env: HashMap::new(),
        node_types,
    };

    let mut functions = Vec::new();
    let mut externs = Vec::new();

    for toplevel in program {
        match &toplevel.value {
            TopLevelNode::Function { name, attributes, return_type, .. } => {
                let param_regs = lower_function(&mut cx, toplevel);
                functions.push(Function {
                    name,
                    attributes: attributes.clone(),
                    params: param_regs,
                    return_type: return_type.clone(),
                    blocks: cx.blocks.clone(),
                });
                // reset for next function
                cx.reg_counter = 0;
                cx.block_counter = 0;
                cx.blocks.clear();
            }
            TopLevelNode::Extern { name, attributes, params, return_type, .. } => {
                externs.push(ExternDecl {
                    name,
                    attributes: attributes.clone(),
                    params: params.iter().map(|(_, ty)| ty.clone()).collect(),
                    return_type: return_type.clone(),
                });
            }
        }
    }

    Module { functions, externs }
}