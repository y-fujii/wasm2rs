use parity_wasm::elements::{BlockType, Instruction, ValueType};
use std::fmt::Write;
use std::*;

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

fn dump_body(dst: &mut String, body: &parity_wasm::elements::FuncBody, mut local_idx: usize) {
    let mut n_indent = 1;
    for local in body.locals() {
        for _ in 0..local.count() {
            dump_indent(dst, n_indent);
            write!(dst, "let mut v{}: {} = std::mem::uninitialized();\n", local_idx, type_str(local.value_type())).unwrap();
            local_idx += 1;
        }
    }

    let mut sidx = 0;
    let mut stack_offsets = Vec::new();
    for inst in body.code().elements() {
        match inst {
            Instruction::GetLocal(i) => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = v{};\n", sidx, i).unwrap();
                sidx += 1;
            }
            Instruction::I32Const(n) => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = {}i32;\n", sidx, n).unwrap();
                sidx += 1;
            }
            Instruction::I32Eq => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = (s{} == s{}) as i32;\n", sidx - 2, sidx - 2, sidx - 1).unwrap();
                sidx -= 1;
            }
            Instruction::I32Sub => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = s{} - s{};\n", sidx - 2, sidx - 2, sidx - 1).unwrap();
                sidx -= 1;
            }
            Instruction::I32Mul => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = s{} * s{};\n", sidx - 2, sidx - 2, sidx - 1).unwrap();
                sidx -= 1;
            }
            Instruction::Call(n) => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = f{}(s{});\n", sidx - 1, n, sidx - 1).unwrap();
            }
            Instruction::If(BlockType::Value(_)) => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = if s{} != 0i32 {{\n", sidx - 1, sidx - 1).unwrap();
                stack_offsets.push(1);
                sidx -= 1;
                n_indent += 1;
            }
            Instruction::Else => {
                let offset = *stack_offsets.last().unwrap();
                if offset == 1 {
                    dump_indent(dst, n_indent);
                    write!(dst, "s{}\n", sidx - 1).unwrap();
                }
                dump_indent(dst, n_indent - 1);
                dst.push_str("} else {\n");
                sidx -= offset;
            }
            Instruction::End => {
                if let Some(offset) = stack_offsets.pop() {
                    dump_indent(dst, n_indent);
                    write!(dst, "s{}\n", sidx - 1).unwrap();
                    n_indent -= 1;
                    dump_indent(dst, n_indent);
                    dst.push_str("};\n");
                    sidx -= offset;
                } else {
                    dump_indent(dst, n_indent);
                    dst.push_str("s0\n");
                    n_indent -= 1;
                }
            }
            inst => write!(dst, "XXX: {:?}\n", inst).unwrap(),
        }
    }
}

fn main() -> result::Result<(), Box<error::Error>> {
    let module = parity_wasm::deserialize_file("res/fac.wasm")?;
    let type_section = module.type_section().unwrap();
    let func_section = module.function_section().unwrap();
    let code_section = module.code_section().unwrap();
    assert!(func_section.entries().len() == code_section.bodies().len());

    let mut dst = String::new();
    dst.push_str("#![allow(unused_mut)]\n\n");

    for (i, (func, body)) in func_section.entries().iter().zip(code_section.bodies()).enumerate() {
        let ftyp = match &type_section.types()[func.type_ref() as usize] {
            parity_wasm::elements::Type::Function(t) => t,
        };
        assert!(ftyp.form() == 0x60);

        write!(dst, "fn f{}(", i).unwrap();
        for (i, param) in ftyp.params().iter().enumerate() {
            write!(dst, "mut v{}: {}", i, type_str(*param)).unwrap();
            if i != ftyp.params().len() - 1 {
                dst.push_str(", ");
            }
        }
        dst.push_str(")");
        if let Some(rtype) = ftyp.return_type() {
            dst.push_str(" -> ");
            dst.push_str(type_str(rtype));
        }
        dst.push_str(" {\n");
        dump_body(&mut dst, body, ftyp.params().len());
        dst.push_str("}\n");
    }

    print!("{}", dst);

    Ok(())
}
