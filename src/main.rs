use parity_wasm::elements::{BlockType, Instruction, ValueType};
use std::fmt::Write;
use std::*;

enum LabelType {
    Block,
    Loop,
    If,
}

struct Label {
    id: usize,
    ty: LabelType,
    arity: usize,
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

fn dump_indent(dst: &mut String, n: usize) {
    for _ in 0..n {
        dst.push_str("    ");
    }
}

fn dump_bin_op(dst: &mut String, op: &str, sidx: &mut usize, n_indent: usize) {
    dump_indent(dst, n_indent);
    write!(dst, "let s{} = s{} {} s{};\n", *sidx - 2, *sidx - 2, op, *sidx - 1).unwrap();
    *sidx -= 1;
}

fn dump_cmp_op(dst: &mut String, op: &str, sidx: &mut usize, n_indent: usize) {
    dump_indent(dst, n_indent);
    write!(
        dst,
        "let s{} = (s{} {} s{}) as i32;\n",
        *sidx - 2,
        *sidx - 2,
        op,
        *sidx - 1,
    )
    .unwrap();
    *sidx -= 1;
}

fn dump_label(dst: &mut String, labels: &Vec<Label>, n: u32, n_indent: usize) {
    dump_indent(dst, n_indent);
    let label = &labels[labels.len() - 1 - n as usize];
    match label.ty {
        LabelType::Block => write!(dst, "break 'l{};\n", label.id).unwrap(),
        LabelType::Loop => write!(dst, "continue 'l{};\n", label.id).unwrap(),
        LabelType::If => write!(dst, "break 'l{};\n", label.id).unwrap(),
    }
}

fn dump_body(dst: &mut String, module: &parity_wasm::elements::Module, body: &parity_wasm::elements::FuncBody) {
    let types = module.type_section().unwrap().types();
    let funcs = module.function_section().unwrap().entries();

    let mut n_indent = 1;
    let mut sidx = 0;
    let mut lidx = 0;
    let mut labels = Vec::new();
    for inst in body.code().elements() {
        match inst {
            Instruction::GetLocal(i) => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = v{};\n", sidx, i).unwrap();
                sidx += 1;
            }
            Instruction::SetLocal(i) => {
                dump_indent(dst, n_indent);
                write!(dst, "v{} = s{};\n", i, sidx - 1).unwrap();
                sidx -= 1;
            }
            Instruction::I32Const(n) => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = {}i32;\n", sidx, n).unwrap();
                sidx += 1;
            }
            Instruction::I64Const(n) => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = {}i64;\n", sidx, n).unwrap();
                sidx += 1;
            }
            Instruction::I32Eq | Instruction::I64Eq => {
                dump_cmp_op(dst, "==", &mut sidx, n_indent);
            }
            Instruction::I32LtS | Instruction::I64LtS => {
                dump_cmp_op(dst, "<", &mut sidx, n_indent);
            }
            Instruction::I32GtS | Instruction::I64GtS => {
                dump_cmp_op(dst, ">", &mut sidx, n_indent);
            }
            Instruction::I32Add | Instruction::I64Add => {
                dump_bin_op(dst, "+", &mut sidx, n_indent);
            }
            Instruction::I32Sub | Instruction::I64Sub => {
                dump_bin_op(dst, "-", &mut sidx, n_indent);
            }
            Instruction::I32Mul | Instruction::I64Mul => {
                dump_bin_op(dst, "*", &mut sidx, n_indent);
            }
            Instruction::Call(n) => {
                let ftyp = match &types[funcs[*n as usize].type_ref() as usize] {
                    parity_wasm::elements::Type::Function(t) => t,
                };
                dump_indent(dst, n_indent);
                if let Some(_) = ftyp.return_type() {
                    write!(dst, "let s{} = ", sidx - ftyp.params().len()).unwrap();
                }
                write!(dst, "f{}(", n).unwrap();
                for _ in ftyp.params() {
                    write!(dst, "s{},", sidx - 1).unwrap();
                    sidx -= 1;
                }
                dst.push_str(");\n");
                if let Some(_) = ftyp.return_type() {
                    sidx += 1;
                }
            }
            Instruction::Block(BlockType::NoResult) => {
                dump_indent(dst, n_indent);
                write!(dst, "'l{}: loop {{\n", lidx).unwrap();
                labels.push(Label {
                    id: lidx,
                    ty: LabelType::Block,
                    arity: 0,
                });
                lidx += 1;
                n_indent += 1;
            }
            Instruction::Loop(BlockType::NoResult) => {
                dump_indent(dst, n_indent);
                write!(dst, "'l{}: loop {{\n", lidx).unwrap();
                labels.push(Label {
                    id: lidx,
                    ty: LabelType::Loop,
                    arity: 0,
                });
                lidx += 1;
                n_indent += 1;
            }
            Instruction::Br(n) => {
                dump_label(dst, &labels, *n, n_indent);
            }
            Instruction::BrIf(n) => {
                dump_indent(dst, n_indent);
                write!(dst, "if s{} != 0i32 {{\n", sidx - 1).unwrap();
                dump_label(dst, &labels, *n, n_indent + 1);
                dump_indent(dst, n_indent);
                dst.push_str("}\n");
                sidx -= 1;
            }
            Instruction::If(btyp) => {
                dump_indent(dst, n_indent);
                let arity = match btyp {
                    BlockType::NoResult => 0,
                    BlockType::Value(_) => {
                        write!(dst, "let s{} = ", sidx - 1).unwrap();
                        1
                    }
                };
                write!(dst, "'l{}: loop {{ break if s{} != 0i32 {{\n", lidx, sidx - 1).unwrap();
                labels.push(Label {
                    id: lidx,
                    ty: LabelType::If,
                    arity: arity,
                });
                lidx += 1;
                sidx -= 1;
                n_indent += 1;
            }
            Instruction::Else => {
                if labels.last().unwrap().arity == 1 {
                    dump_indent(dst, n_indent);
                    write!(dst, "s{}\n", sidx - 1).unwrap();
                    sidx -= 1;
                }
                dump_indent(dst, n_indent - 1);
                dst.push_str("} else {\n");
            }
            Instruction::End => {
                let label = match labels.pop() {
                    Some(e) => e,
                    None => continue,
                };
                match label.ty {
                    LabelType::Block => {
                        dump_indent(dst, n_indent);
                        dst.push_str("break;\n");
                        n_indent -= 1;
                        dump_indent(dst, n_indent);
                        dst.push_str("};\n");
                    }
                    LabelType::Loop => {
                        dump_indent(dst, n_indent);
                        dst.push_str("break;\n");
                        n_indent -= 1;
                        dump_indent(dst, n_indent);
                        dst.push_str("};\n");
                    }
                    LabelType::If => {
                        if label.arity == 1 {
                            dump_indent(dst, n_indent);
                            write!(dst, "s{}\n", sidx - 1).unwrap();
                            sidx -= 1;
                        }
                        n_indent -= 1;
                        dump_indent(dst, n_indent);
                        dst.push_str("}; };\n");
                    }
                }
            }
            inst => {
                panic!("{:?}", inst);
            }
        }
    }
}

fn main() -> result::Result<(), Box<error::Error>> {
    let module = parity_wasm::deserialize_file("test/fac.wasm")?;
    let types = module.type_section().unwrap().types();
    let funcs = module.function_section().unwrap().entries();
    let codes = module.code_section().unwrap().bodies();
    assert!(funcs.len() == codes.len());

    let mut dst = String::new();
    dst.push_str("#![allow(unused_mut)]\n");
    dst.push_str("#![allow(unreachable_code)]\n");
    dst.push_str("#![allow(unused_assignments)]\n");

    for (i, (func, body)) in funcs.iter().zip(codes).enumerate() {
        let ftyp = match &types[func.type_ref() as usize] {
            parity_wasm::elements::Type::Function(t) => t,
        };
        assert!(ftyp.form() == 0x60);

        // function declaration.
        write!(dst, "\nfn f{}(", i).unwrap();
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
                dump_indent(&mut dst, 1);
                write!(
                    dst,
                    "let mut v{}: {} = unsafe {{ std::mem::uninitialized() }};\n",
                    local_idx,
                    type_str(local.value_type())
                )
                .unwrap();
                local_idx += 1;
            }
        }
        dump_body(&mut dst, &module, body);
        if let Some(_) = ftyp.return_type() {
            dump_indent(&mut dst, 1);
            dst.push_str("s0\n");
        }
        dst.push_str("}\n");
    }

    print!("{}", dst);

    Ok(())
}
