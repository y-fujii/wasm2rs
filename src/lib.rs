// (c) Yasuhiro Fujii <http://mimosa-pudica.net>, under MIT License.
pub use parity_wasm::elements::Error;
use parity_wasm::elements::{BlockType, Instruction, ValueType};
use std::fmt::Write;
use std::*;

macro_rules! write_line {
    ($dst: expr, $n_indent: expr, $fmt: expr) => {{
        write_indent($dst, $n_indent);
        $dst.push_str(concat!($fmt, ";\n"));
    }};
    ($dst: expr, $n_indent: expr, $fmt: expr, $($args: expr),*) => {{
        write_indent($dst, $n_indent);
        write!($dst, concat!($fmt, ";\n"), $($args),*).unwrap();
    }};
}

#[derive(Debug)]
struct SymbolMap {
    functions: collections::HashMap<u32, String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LabelType {
    Block,
    Loop,
    If,
    Function,
}

#[derive(Debug, Clone)]
struct Label {
    lid: usize,
    ty: LabelType,
    coarity: usize,
    sid: usize,
}

fn type_sig(ty: &parity_wasm::elements::Type) -> u64 {
    let ty = match ty {
        parity_wasm::elements::Type::Function(t) => t,
    };
    let mut sig = match ty.return_type() {
        Some(t) => 1 + t as u64,
        None => 0,
    };
    assert!(ty.params().len() <= 64 / 3 - 1);
    for t in ty.params() {
        sig = (sig << 3) + (1 + *t as u64);
    }
    sig
}

fn type_str(v: parity_wasm::elements::ValueType) -> &'static str {
    match v {
        ValueType::I32 => "i32",
        ValueType::I64 => "i64",
        ValueType::F32 => "f32",
        ValueType::F64 => "f64",
        ValueType::V128 => panic!(),
    }
}

fn resize_stack(sid: &mut usize, labels: &Vec<Label>, arity: usize, coarity: usize) {
    assert!(*sid >= labels.last().unwrap().sid + arity);
    *sid -= arity;
    *sid += coarity;
}

fn write_indent(dst: &mut String, n: usize) {
    for _ in 0..n {
        dst.push('\t');
    }
}

fn write_jump(dst: &mut String, sid: usize, labels: &Vec<Label>, n: u32) {
    let label = &labels[labels.len() - 1 - n as usize];
    // note: br instructions resize the stack.
    match label.ty {
        LabelType::Block | LabelType::If => {
            assert!(label.sid + label.coarity <= sid);
            write!(dst, "break 'l{}", label.lid).unwrap();
            if label.coarity == 1 {
                write!(dst, " s{}", sid - 1).unwrap();
            }
        }
        LabelType::Loop => {
            assert!(label.sid <= sid);
            write!(dst, "continue 'l{}", label.lid).unwrap();
        }
        LabelType::Function => {
            assert!(label.sid + label.coarity <= sid);
            dst.push_str("return");
            if label.coarity == 1 {
                write!(dst, " s{}", sid - 1).unwrap();
            }
        }
    }
}

fn write_label(
    dst: &mut String,
    sid: usize,
    lid: &mut usize,
    labels: &mut Vec<Label>,
    n_indent: &mut usize,
    block_ty: &BlockType,
    label_ty: LabelType,
) {
    write_indent(dst, *n_indent);
    let coarity = match block_ty {
        BlockType::NoResult => 0,
        BlockType::Value(value_ty) => {
            write!(dst, "let s{}: {} = ", sid, type_str(*value_ty)).unwrap();
            1
        }
    };
    write!(dst, "'l{}: loop {{\n", lid).unwrap();
    labels.push(Label {
        lid: *lid,
        ty: label_ty,
        coarity: coarity,
        sid: sid,
    });
    *lid += 1;
    *n_indent += 1;
}

fn skip_to_end<'a, T: Iterator<Item = &'a Instruction>>(
    dst: &mut String,
    sid: &mut usize,
    labels: &mut Vec<Label>,
    n_indent: &mut usize,
    inst_it: &mut T,
) {
    let mut depth = 0;
    while let Some(inst) = inst_it.next() {
        match inst {
            Instruction::Block(_) | Instruction::Loop(_) | Instruction::If(_) => {
                depth += 1;
            }
            Instruction::Else => {
                if depth > 0 {
                    continue;
                }
                let label = labels.last().unwrap();
                assert!(label.ty == LabelType::If);
                *sid = label.sid;
                write_indent(dst, *n_indent - 1);
                dst.push_str("} else {\n");
                break;
            }
            Instruction::End => {
                if depth > 0 {
                    depth -= 1;
                    continue;
                }
                let label = labels.pop().unwrap();
                *sid = label.sid + label.coarity;
                *n_indent -= 1;
                write_indent(dst, *n_indent);
                match label.ty {
                    LabelType::Block | LabelType::Loop => {
                        dst.push_str("};\n");
                    }
                    LabelType::If => {
                        dst.push_str("}; };\n");
                    }
                    LabelType::Function => {
                        dst.push_str("}\n");
                    }
                }
                break;
            }
            _ => (),
        }
    }
}

fn write_code<'a, T: Iterator<Item = &'a parity_wasm::elements::Instruction>>(
    dst: &mut String,
    mut inst_it: T,
    coarity: usize,
    module: &parity_wasm::elements::Module,
    symbols: &SymbolMap,
) {
    let types = module.type_section().unwrap().types();
    let funcs = module.function_section().unwrap().entries();

    let mut n_indent = 1;
    let mut sid = 0;
    let mut lid = 0;
    let mut labels = vec![Label {
        lid: !0,
        ty: LabelType::Function,
        coarity: coarity,
        sid: sid,
    }];
    while let Some(inst) = inst_it.next() {
        match inst {
            Instruction::Nop => {}
            Instruction::Drop => {
                resize_stack(&mut sid, &labels, 1, 0);
            }
            Instruction::GrowMemory(_) => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = grow(s{})", sid - 1);
            }
            Instruction::CurrentMemory(0) => {
                resize_stack(&mut sid, &labels, 0, 1);
                write_line!(dst, n_indent, "let s{0} = size()", sid - 1);
            }
            Instruction::Select => {
                resize_stack(&mut sid, &labels, 3, 1);
                write_line!(
                    dst,
                    n_indent,
                    "let s{1} = if s{} != 0i32 {{ s{} }} else {{ s{} }}",
                    sid + 1,
                    sid - 1,
                    sid
                );
            }
            Instruction::I32Const(n) => {
                resize_stack(&mut sid, &labels, 0, 1);
                write_line!(dst, n_indent, "let s{} = {}i32", sid - 1, n);
            }
            Instruction::I64Const(n) => {
                resize_stack(&mut sid, &labels, 0, 1);
                write_line!(dst, n_indent, "let s{} = {}i64", sid - 1, n);
            }
            Instruction::F32Const(n) => {
                resize_stack(&mut sid, &labels, 0, 1);
                write_line!(dst, n_indent, "let s{} = f32::from_bits({})", sid - 1, n);
            }
            Instruction::F64Const(n) => {
                resize_stack(&mut sid, &labels, 0, 1);
                write_line!(dst, n_indent, "let s{} = f64::from_bits({})", sid - 1, n);
            }
            Instruction::GetLocal(i) => {
                resize_stack(&mut sid, &labels, 0, 1);
                write_line!(dst, n_indent, "let s{} = v{}", sid - 1, i);
            }
            Instruction::SetLocal(i) => {
                resize_stack(&mut sid, &labels, 1, 0);
                write_line!(dst, n_indent, "v{} = s{}", i, sid);
            }
            Instruction::GetGlobal(i) => {
                resize_stack(&mut sid, &labels, 0, 1);
                write_line!(dst, n_indent, "let s{} = G{}.with(|e| e.get())", sid - 1, i);
            }
            Instruction::SetGlobal(i) => {
                resize_stack(&mut sid, &labels, 1, 0);
                write_line!(dst, n_indent, "G{}.with(|e| e.set(s{}))", i, sid);
            }
            Instruction::I32Load8S(_, ofs) => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{1} = load::<i8>({} + s{} as u32) as i32", ofs, sid - 1);
            }
            Instruction::I32Load8U(_, ofs) => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{1} = load::<u8>({} + s{} as u32) as i32", ofs, sid - 1);
            }
            Instruction::I32Load16S(_, ofs) => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{1} = load::<i16>({} + s{} as u32) as i32", ofs, sid - 1);
            }
            Instruction::I32Load16U(_, ofs) => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{1} = load::<u16>({} + s{} as u32) as i32", ofs, sid - 1);
            }
            Instruction::I32Load(_, ofs) => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{1} = load::<i32>({} + s{} as u32)", ofs, sid - 1);
            }
            Instruction::I64Load8S(_, ofs) => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{1} = load::<i8>({} + s{} as u32) as i64", ofs, sid - 1);
            }
            Instruction::I64Load8U(_, ofs) => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{1} = load::<u8>({} + s{} as u32) as i64", ofs, sid - 1);
            }
            Instruction::I64Load16S(_, ofs) => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{1} = load::<i16>({} + s{} as u32) as i64", ofs, sid - 1);
            }
            Instruction::I64Load16U(_, ofs) => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{1} = load::<u16>({} + s{} as u32) as i64", ofs, sid - 1);
            }
            Instruction::I64Load32S(_, ofs) => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{1} = load::<i32>({} + s{} as u32) as i64", ofs, sid - 1);
            }
            Instruction::I64Load32U(_, ofs) => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{1} = load::<u32>({} + s{} as u32) as i64", ofs, sid - 1);
            }
            Instruction::I64Load(_, ofs) => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{1} = load::<i64>({} + s{} as u32)", ofs, sid - 1);
            }
            Instruction::F32Load(_, ofs) => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{1} = load::<f32>({} + s{} as u32)", ofs, sid - 1);
            }
            Instruction::F64Load(_, ofs) => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{1} = load::<f64>({} + s{} as u32)", ofs, sid - 1);
            }
            Instruction::I32Store(_, ofs)
            | Instruction::I64Store(_, ofs)
            | Instruction::F32Store(_, ofs)
            | Instruction::F64Store(_, ofs) => {
                resize_stack(&mut sid, &labels, 2, 0);
                write_line!(dst, n_indent, "store({} + s{} as u32, s{})", ofs, sid, sid + 1);
            }
            Instruction::I32Store8(_, ofs) | Instruction::I64Store8(_, ofs) => {
                resize_stack(&mut sid, &labels, 2, 0);
                write_line!(dst, n_indent, "store({} + s{} as u32, s{} as i8)", ofs, sid, sid + 1);
            }
            Instruction::I32Store16(_, ofs) | Instruction::I64Store16(_, ofs) => {
                resize_stack(&mut sid, &labels, 2, 0);
                write_line!(dst, n_indent, "store({} + s{} as u32, s{} as i16)", ofs, sid, sid + 1);
            }
            Instruction::I64Store32(_, ofs) => {
                resize_stack(&mut sid, &labels, 2, 0);
                write_line!(dst, n_indent, "store({} + s{} as u32, s{} as i32)", ofs, sid, sid + 1);
            }
            Instruction::TeeLocal(i) => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "v{} = s{}", i, sid - 1);
            }
            Instruction::I32Eq | Instruction::I64Eq | Instruction::F32Eq | Instruction::F64Eq => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} == s{}) as i32", sid - 1, sid);
            }
            Instruction::I32Ne | Instruction::I64Ne | Instruction::F32Ne | Instruction::F64Ne => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} != s{}) as i32", sid - 1, sid);
            }
            Instruction::I32LtS | Instruction::I64LtS | Instruction::F32Lt | Instruction::F64Lt => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} < s{}) as i32", sid - 1, sid);
            }
            Instruction::I32LeS | Instruction::I64LeS | Instruction::F32Le | Instruction::F64Le => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} <= s{}) as i32", sid - 1, sid);
            }
            Instruction::I32GtS | Instruction::I64GtS | Instruction::F32Gt | Instruction::F64Gt => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} > s{}) as i32", sid - 1, sid);
            }
            Instruction::I32GeS | Instruction::I64GeS | Instruction::F32Ge | Instruction::F64Ge => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} >= s{}) as i32", sid - 1, sid);
            }
            Instruction::I32LtU => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = ((s{} as u32) < s{} as u32) as i32", sid - 1, sid);
            }
            Instruction::I64LtU => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = ((s{} as u64) < s{} as u64) as i32", sid - 1, sid);
            }
            Instruction::I32LeU => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} as u32 <= s{} as u32) as i32", sid - 1, sid);
            }
            Instruction::I64LeU => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} as u64 <= s{} as u64) as i32", sid - 1, sid);
            }
            Instruction::I32GtU => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} as u32 > s{} as u32) as i32", sid - 1, sid);
            }
            Instruction::I64GtU => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} as u64 > s{} as u64) as i32", sid - 1, sid);
            }
            Instruction::I32GeU => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} as u32 >= s{} as u32) as i32", sid - 1, sid);
            }
            Instruction::I64GeU => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} as u64 >= s{} as u64) as i32", sid - 1, sid);
            }
            Instruction::F32Neg | Instruction::F64Neg => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = -s{}", sid - 1);
            }
            Instruction::I32Add | Instruction::I64Add | Instruction::F32Add | Instruction::F64Add => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = s{} + s{}", sid - 1, sid);
            }
            Instruction::I32Sub | Instruction::I64Sub | Instruction::F32Sub | Instruction::F64Sub => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = s{} - s{}", sid - 1, sid);
            }
            Instruction::I32Mul | Instruction::I64Mul | Instruction::F32Mul | Instruction::F64Mul => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = s{} * s{}", sid - 1, sid);
            }
            Instruction::I32DivS | Instruction::I64DivS | Instruction::F32Div | Instruction::F64Div => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = s{} / s{}", sid - 1, sid);
            }
            Instruction::I32RemS | Instruction::I64RemS => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = s{}.wrapping_rem(s{})", sid - 1, sid);
            }
            Instruction::I32And | Instruction::I64And => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = s{} & s{}", sid - 1, sid);
            }
            Instruction::I32Or | Instruction::I64Or => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = s{} | s{}", sid - 1, sid);
            }
            Instruction::I32Xor | Instruction::I64Xor => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = s{} ^ s{}", sid - 1, sid);
            }
            Instruction::I32Shl | Instruction::I64Shl => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = s{} << s{}", sid - 1, sid);
            }
            Instruction::I32ShrS | Instruction::I64ShrS => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = s{} >> s{}", sid - 1, sid);
            }
            Instruction::I32Rotl | Instruction::I64Rotl => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = s{}.rotate_left(s{} as u32)", sid - 1, sid);
            }
            Instruction::I32Rotr | Instruction::I64Rotr => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = s{}.rotate_right(s{} as u32)", sid - 1, sid);
            }
            Instruction::I32Popcnt => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{}.count_ones() as i32", sid - 1);
            }
            Instruction::I64Popcnt => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{}.count_ones() as i64", sid - 1);
            }
            Instruction::I32DivU => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} as u32 / s{} as u32) as i32", sid - 1, sid);
            }
            Instruction::I64DivU => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} as u64 / s{} as u64) as i64", sid - 1, sid);
            }
            Instruction::I32RemU => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} as u32 % s{} as u32) as i32", sid - 1, sid);
            }
            Instruction::I64RemU => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} as u64 % s{} as u64) as i64", sid - 1, sid);
            }
            Instruction::I32ShrU => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} as u32 >> s{} as u32) as i32", sid - 1, sid);
            }
            Instruction::I64ShrU => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} as u64 >> s{} as u64) as i64", sid - 1, sid);
            }
            Instruction::I32WrapI64 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{} as i32", sid - 1);
            }
            Instruction::I64ExtendSI32 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{} as i64", sid - 1);
            }
            Instruction::I64ExtendUI32 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{} as u32 as i64", sid - 1);
            }
            Instruction::I32TruncSF32 | Instruction::I32TruncSF64 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{} as i32", sid - 1);
            }
            Instruction::I32TruncUF32 | Instruction::I32TruncUF64 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{} as u32 as i32", sid - 1);
            }
            Instruction::I64TruncSF32 | Instruction::I64TruncSF64 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{} as i64", sid - 1);
            }
            Instruction::I64TruncUF32 | Instruction::I64TruncUF64 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{} as u64 as i64", sid - 1);
            }
            Instruction::F32ConvertSI32 | Instruction::F32ConvertSI64 | Instruction::F32DemoteF64 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{} as f32", sid - 1);
            }
            Instruction::F32ConvertUI32 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{} as u32 as f32", sid - 1);
            }
            Instruction::F32ConvertUI64 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{} as u64 as f32", sid - 1);
            }
            Instruction::F64ConvertSI32 | Instruction::F64ConvertSI64 | Instruction::F64PromoteF32 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{} as f64", sid - 1);
            }
            Instruction::F64ConvertUI32 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{} as u32 as f64", sid - 1);
            }
            Instruction::F64ConvertUI64 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{} as u64 as f64", sid - 1);
            }
            Instruction::I32ReinterpretF32 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = f32::to_bits(s{}) as i32", sid - 1);
            }
            Instruction::F32ReinterpretI32 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = f32::from_bits(s{} as u32)", sid - 1);
            }
            Instruction::I64ReinterpretF64 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = f64::to_bits(s{}) as i64", sid - 1);
            }
            Instruction::F64ReinterpretI64 => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = f64::from_bits(s{} as u64)", sid - 1);
            }
            Instruction::I32Ctz => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{}.trailing_zeros() as i32", sid - 1);
            }
            Instruction::I64Ctz => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{}.trailing_zeros() as i64", sid - 1);
            }
            Instruction::I32Clz => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{}.leading_zeros() as i32", sid - 1);
            }
            Instruction::I64Clz => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{}.leading_zeros() as i64", sid - 1);
            }
            Instruction::I32Eqz => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} == 0i32) as i32", sid - 1);
            }
            Instruction::I64Eqz => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = (s{} == 0i64) as i32", sid - 1);
            }
            Instruction::F32Abs | Instruction::F64Abs => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{}.abs()", sid - 1);
            }
            Instruction::F32Ceil | Instruction::F64Ceil => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{}.ceil()", sid - 1);
            }
            Instruction::F32Floor | Instruction::F64Floor => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{}.floor()", sid - 1);
            }
            Instruction::F32Trunc | Instruction::F64Trunc => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{}.trunc()", sid - 1);
            }
            Instruction::F32Nearest | Instruction::F64Nearest => {
                resize_stack(&mut sid, &labels, 1, 1);
                // XXX
                write_line!(dst, n_indent, "let s{0} = s{}.round()", sid - 1);
            }
            Instruction::F32Sqrt | Instruction::F64Sqrt => {
                resize_stack(&mut sid, &labels, 1, 1);
                write_line!(dst, n_indent, "let s{0} = s{}.sqrt()", sid - 1);
            }
            Instruction::F32Min | Instruction::F64Min => {
                resize_stack(&mut sid, &labels, 2, 1);
                // XXX
                write_line!(dst, n_indent, "let s{0} = s{}.min(s{})", sid - 1, sid);
            }
            Instruction::F32Max | Instruction::F64Max => {
                resize_stack(&mut sid, &labels, 2, 1);
                // XXX
                write_line!(dst, n_indent, "let s{0} = s{}.max(s{})", sid - 1, sid);
            }
            Instruction::F32Copysign | Instruction::F64Copysign => {
                resize_stack(&mut sid, &labels, 2, 1);
                write_line!(dst, n_indent, "let s{0} = s{}.copysign(s{})", sid - 1, sid);
            }
            Instruction::Call(n) => {
                let ftyp = match &types[funcs[*n as usize].type_ref() as usize] {
                    parity_wasm::elements::Type::Function(t) => t,
                };
                resize_stack(&mut sid, &labels, ftyp.params().len(), 0);
                write_indent(dst, n_indent);
                if let Some(_) = ftyp.return_type() {
                    write!(dst, "let s{} = ", sid).unwrap();
                }
                match symbols.functions.get(n) {
                    Some(v) => write!(dst, "{}(", v).unwrap(),
                    None => write!(dst, "f{}(", n).unwrap(),
                }
                for i in sid..sid + ftyp.params().len() {
                    write!(dst, "s{},", i).unwrap();
                }
                dst.push_str(");\n");
                if let Some(_) = ftyp.return_type() {
                    resize_stack(&mut sid, &labels, 0, 1);
                }
            }
            Instruction::CallIndirect(n, _) => {
                let ftyp = match &types[*n as usize] {
                    parity_wasm::elements::Type::Function(t) => t,
                };
                resize_stack(&mut sid, &labels, ftyp.params().len() + 1, 0);
                write_indent(dst, n_indent);
                if let Some(_) = ftyp.return_type() {
                    write!(dst, "let s{} = ", sid).unwrap();
                }
                dst.push_str("{\n");
                write_indent(dst, n_indent + 1);
                write!(dst, "let (t, f) = TABLE.with(|t| t[s{} as usize]);\n", sid + ftyp.params().len()).unwrap();
                write_indent(dst, n_indent + 1);
                write!(dst, "assert!(t == {});\n", type_sig(&types[*n as usize])).unwrap();
                write_indent(dst, n_indent + 1);
                dst.push_str("let f = unsafe { core::mem::transmute::<usize, fn(");
                for param in ftyp.params() {
                    write!(dst, "{},", type_str(*param)).unwrap();
                }
                dst.push_str(")");
                if let Some(rtype) = ftyp.return_type() {
                    dst.push_str(" -> ");
                    dst.push_str(type_str(rtype));
                }
                dst.push_str(">(f) };\n");
                write_indent(dst, n_indent + 1);
                dst.push_str("f(");
                for i in sid..sid + ftyp.params().len() {
                    write!(dst, "s{},", i).unwrap();
                }
                dst.push_str(")\n");
                write_indent(dst, n_indent);
                dst.push_str("};\n");
                if let Some(_) = ftyp.return_type() {
                    resize_stack(&mut sid, &labels, 0, 1);
                }
            }
            Instruction::Unreachable => {
                write_line!(dst, n_indent, "unreachable!()");
                skip_to_end(dst, &mut sid, &mut labels, &mut n_indent, &mut inst_it);
            }
            Instruction::Block(ty) => {
                write_label(dst, sid, &mut lid, &mut labels, &mut n_indent, ty, LabelType::Block);
            }
            Instruction::Loop(ty) => {
                write_label(dst, sid, &mut lid, &mut labels, &mut n_indent, ty, LabelType::Loop);
            }
            Instruction::Br(n) => {
                write_indent(dst, n_indent);
                write_jump(dst, sid, &labels, *n);
                dst.push_str(";\n");
                skip_to_end(dst, &mut sid, &mut labels, &mut n_indent, &mut inst_it);
            }
            Instruction::BrIf(n) => {
                resize_stack(&mut sid, &labels, 1, 0);
                write_indent(dst, n_indent);
                write!(dst, "if s{} != 0i32 {{\n", sid).unwrap();
                write_indent(dst, n_indent + 1);
                write_jump(dst, sid, &labels, *n);
                dst.push_str(";\n");
                write_indent(dst, n_indent);
                dst.push_str("}\n");
            }
            Instruction::BrTable(data) => {
                resize_stack(&mut sid, &labels, 1, 0);
                write_indent(dst, n_indent);
                write!(dst, "match s{} {{\n", sid).unwrap();
                for (i, n) in data.table.iter().enumerate() {
                    write_indent(dst, n_indent + 1);
                    write!(dst, "{} => ", i).unwrap();
                    write_jump(dst, sid, &labels, *n);
                    dst.push_str(",\n");
                }
                write_indent(dst, n_indent + 1);
                dst.push_str("_ => ");
                write_jump(dst, sid, &labels, data.default);
                dst.push_str(",\n");
                write_indent(dst, n_indent);
                dst.push_str("}\n");
                skip_to_end(dst, &mut sid, &mut labels, &mut n_indent, &mut inst_it);
            }
            Instruction::Return => {
                write_indent(dst, n_indent);
                write_jump(dst, sid, &labels, labels.len() as u32 - 1);
                dst.push_str(";\n");
                skip_to_end(dst, &mut sid, &mut labels, &mut n_indent, &mut inst_it);
            }
            Instruction::If(block_ty) => {
                resize_stack(&mut sid, &labels, 1, 0);
                write_indent(dst, n_indent);
                let coarity = match block_ty {
                    BlockType::NoResult => 0,
                    BlockType::Value(value_ty) => {
                        write!(dst, "let s{}: {} = ", sid, type_str(*value_ty)).unwrap();
                        1
                    }
                };
                write!(dst, "'l{}: loop {{ break if s{} != 0i32 {{\n", lid, sid).unwrap();
                labels.push(Label {
                    lid: lid,
                    ty: LabelType::If,
                    coarity: coarity,
                    sid: sid,
                });
                lid += 1;
                n_indent += 1;
            }
            Instruction::Else => {
                let label = labels.last().unwrap();
                assert!(label.ty == LabelType::If);
                assert!(label.sid + label.coarity == sid);
                if label.coarity == 1 {
                    write_indent(dst, n_indent);
                    write!(dst, "s{}\n", sid - 1).unwrap();
                    sid = label.sid;
                }
                write_indent(dst, n_indent - 1);
                dst.push_str("} else {\n");
            }
            Instruction::End => {
                let label = labels.pop().unwrap();
                assert!(label.sid + label.coarity == sid);
                match label.ty {
                    LabelType::Block | LabelType::Loop => {
                        write_indent(dst, n_indent);
                        if label.coarity == 1 {
                            write!(dst, "break s{};\n", sid - 1).unwrap();
                        } else {
                            dst.push_str("break;\n");
                        }
                        n_indent -= 1;
                        write_indent(dst, n_indent);
                        dst.push_str("};\n");
                    }
                    LabelType::If => {
                        if label.coarity == 1 {
                            write_indent(dst, n_indent);
                            write!(dst, "s{}\n", sid - 1).unwrap();
                        }
                        n_indent -= 1;
                        write_indent(dst, n_indent);
                        dst.push_str("}; };\n");
                    }
                    LabelType::Function => {
                        if label.coarity == 1 {
                            write_indent(dst, n_indent);
                            write!(dst, "s{}\n", sid - 1).unwrap();
                        }
                        n_indent -= 1;
                        write_indent(dst, n_indent);
                        dst.push_str("}\n");
                    }
                }
            }
            inst => {
                panic!("{:?}", inst);
            }
        }
    }
}

fn read_symbol_map(module: &parity_wasm::elements::Module) -> SymbolMap {
    let mut symbols = SymbolMap {
        functions: collections::HashMap::new(),
    };

    if let Some(exports) = module.export_section() {
        for entry in exports.entries() {
            let field = format!("_{}", entry.field().replace(|c: char| !c.is_ascii_alphanumeric(), "_"));
            match entry.internal() {
                parity_wasm::elements::Internal::Function(i) => {
                    let _r = symbols.functions.insert(*i, field);
                    assert!(_r.is_none());
                }
                parity_wasm::elements::Internal::Table(_) => (),
                parity_wasm::elements::Internal::Memory(_) => (),
                parity_wasm::elements::Internal::Global(_) => (),
            }
        }
    }

    symbols
}

fn write_memory(dst: &mut String, n_indent: usize, module: &parity_wasm::elements::Module) -> bool {
    let size = match module.memory_section() {
        Some(memories) => match memories.entries() {
            [memory] => 65536 * memory.limits().initial() as usize,
            _ => panic!(),
        },
        None => return false,
    };
    write_indent(dst, n_indent);
    dst.push_str("pub static MEMORY: core::cell::UnsafeCell<Vec<u8>> = {\n");
    write_indent(dst, n_indent + 1);
    write!(dst, "let mut m = vec![0; {}];\n", size).unwrap();

    if let Some(data) = module.data_section() {
        for datum in data.entries() {
            assert!(datum.index() == 0);
            let offset = match datum.offset().as_ref().unwrap().code() {
                [Instruction::I32Const(n), Instruction::End] => *n as usize,
                [Instruction::I64Const(n), Instruction::End] => *n as usize,
                _ => panic!(),
            };
            write_indent(dst, n_indent + 1);
            write!(dst, "m[{}..{}].copy_from_slice(&[\n", offset, offset + datum.value().len()).unwrap();
            for chunk in datum.value().chunks(32) {
                write_indent(dst, n_indent + 2);
                for v in chunk {
                    write!(dst, "{},", v).unwrap();
                }
                dst.push_str("\n");
            }
            write_indent(dst, n_indent + 1);
            dst.push_str("]);\n");
        }
    }

    write_indent(dst, n_indent + 1);
    dst.push_str("core::cell::UnsafeCell::new(m)\n");
    write_indent(dst, n_indent);
    dst.push_str("};\n");

    true
}

fn write_table(dst: &mut String, n_indent: usize, module: &parity_wasm::elements::Module, symbols: &SymbolMap) {
    let size = match module.table_section() {
        Some(tables) => match tables.entries() {
            [table] => table.limits().initial() as usize,
            _ => panic!(),
        },
        None => return,
    };
    let types = module.type_section().unwrap().types();
    let funcs = module.function_section().unwrap().entries();
    let mut content = vec![0; size];

    if let Some(elems) = module.elements_section() {
        for elem in elems.entries() {
            assert!(elem.index() == 0);
            let offset = match elem.offset().as_ref().unwrap().code() {
                [Instruction::I32Const(n), Instruction::End] => *n as usize,
                [Instruction::I64Const(n), Instruction::End] => *n as usize,
                _ => panic!(),
            };
            content[offset..offset + elem.members().len()].copy_from_slice(elem.members());
        }
    }

    write_indent(dst, n_indent);
    write!(dst, "static TABLE: [(i32, usize); {}] = [\n", size).unwrap();
    for i in content.iter() {
        let sig = type_sig(&types[funcs[*i as usize].type_ref() as usize]);
        write_indent(dst, n_indent + 1);
        match symbols.functions.get(i) {
            Some(v) => write!(dst, "({}, {} as usize),\n", sig, v).unwrap(),
            None => write!(dst, "({}, f{} as usize),\n", sig, i).unwrap(),
        }
    }
    write_indent(dst, n_indent);
    write!(dst, "];\n").unwrap();
}

fn write_globals(dst: &mut String, n_indent: usize, module: &parity_wasm::elements::Module, _: &SymbolMap) {
    let globals = match module.global_section() {
        Some(e) => e.entries(),
        None => return,
    };
    for (i, global) in globals.iter().enumerate() {
        write_indent(dst, n_indent);
        let ty = global.global_type().content_type();
        write!(dst, "static G{}: core::cell::Cell<{}> = core::cell::Cell::new(", i, type_str(ty)).unwrap();
        match global.init_expr().code() {
            [Instruction::I32Const(n), Instruction::End] => write!(dst, "{});\n", n).unwrap(),
            [Instruction::I64Const(n), Instruction::End] => write!(dst, "{});\n", n).unwrap(),
            [Instruction::F32Const(n), Instruction::End] => write!(dst, "f32::from_bits({}));\n", n).unwrap(),
            [Instruction::F64Const(n), Instruction::End] => write!(dst, "f64::from_bits({}));\n", n).unwrap(),
            _ => panic!(),
        }
    }
}

fn write_functions(dst: &mut String, module: &parity_wasm::elements::Module, symbols: &SymbolMap) {
    let funcs = match module.function_section() {
        Some(e) => e.entries(),
        None => return,
    };
    let types = module.type_section().unwrap().types();
    let codes = module.code_section().unwrap().bodies();
    assert!(funcs.len() == codes.len());
    for (i, (func, body)) in funcs.iter().zip(codes).enumerate() {
        let ftyp = match &types[func.type_ref() as usize] {
            parity_wasm::elements::Type::Function(t) => t,
        };
        assert!(ftyp.form() == 0x60);

        // function declaration.
        match symbols.functions.get(&(i as u32)) {
            Some(v) => write!(dst, "\npub fn {}(", v).unwrap(),
            None => write!(dst, "\nfn f{}(", i).unwrap(),
        }
        let mut local_idx = 0;
        for param in ftyp.params() {
            write!(dst, "mut v{}: {}", local_idx, type_str(*param)).unwrap();
            local_idx += 1;
            if local_idx < ftyp.params().len() {
                dst.push_str(", ");
            }
        }
        dst.push_str(")");
        if let Some(rtype) = ftyp.return_type() {
            dst.push_str(" -> ");
            dst.push_str(type_str(rtype));
        }

        // function body.
        dst.push_str(" {\n");
        for local in body.locals() {
            for _ in 0..local.count() {
                write_indent(dst, 1);
                write!(dst, "let mut v{} = 0{};\n", local_idx, type_str(local.value_type())).unwrap();
                local_idx += 1;
            }
        }
        let coarity = if let Some(_) = ftyp.return_type() { 1 } else { 0 };
        write_code(dst, body.code().elements().iter(), coarity, &module, &symbols);
    }
}

pub fn wasm_to_rust(content: &[u8]) -> result::Result<String, Error> {
    let module = parity_wasm::deserialize_buffer(content)?;
    let symbols = read_symbol_map(&module);

    let mut dst = String::new();
    #[rustfmt::skip]
    dst.push_str("\
        #![allow(unused_mut)]\n\
        #![allow(unreachable_code)]\n\
        #![allow(unused_assignments)]\n\
        #![allow(unused_variables)]\n\
        #![allow(non_snake_case)]\n\
        #![allow(dead_code)]\n\
        \n\
    ");

    dst.push_str("thread_local! {\n");
    write_globals(&mut dst, 1, &module, &symbols);
    write_table(&mut dst, 1, &module, &symbols);
    let has_memory = write_memory(&mut dst, 1, &module);
    dst.push_str("}\n");

    if has_memory {
        #[rustfmt::skip]
        dst.push_str("\
            \n\
            #[inline(always)]\n\
            fn load<T: Copy>(i: u32) -> T {\n\
            \tlet i = i as usize;\n\
            \tlet j = i + core::mem::size_of::<T>();\n\
            \tMEMORY.with(|m| unsafe {\n\
            \t\t*((*m.get())[i..j].as_ptr() as *const T)\n\
            \t})\n\
            }\n\
            \n\
            #[inline(always)]\n\
            fn store<T: Copy>(i: u32, v: T) {\n\
            \tlet i = i as usize;\n\
            \tlet j = i + core::mem::size_of::<T>();\n\
            \tMEMORY.with(|m| unsafe {\n\
            \t\t*((*m.get())[i..j].as_mut_ptr() as *mut T) = v;\n\
            \t})\n\
            }\n\
            \n\
            #[inline(always)]\n\
            fn grow(n: i32) -> i32 {\n\
            \tMEMORY.with(|m| unsafe {\n\
            \t\tlet len = (*m.get()).len();\n\
            \t\t(*m.get()).resize(len + 65536 * n as u32 as usize, 0);\n\
            \t\tassert!(len % 65536 == 0);\n\
            \t\t(len / 65536) as u32 as i32\n\
            \t})\n\
            }\n\
            \n\
            #[inline(always)]\n\
            fn size() -> i32 {\n\
            \tMEMORY.with(|m| unsafe {\n\
            \t\tlet len = (*m.get()).len();\n\
            \t\tassert!(len % 65536 == 0);\n\
            \t\t(len / 65536) as u32 as i32\n\
            \t})\n\
            }\n\
        ");
    }

    write_functions(&mut dst, &module, &symbols);

    Ok(dst)
}
