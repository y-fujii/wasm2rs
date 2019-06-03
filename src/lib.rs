pub use parity_wasm::elements::Error;
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
    arity: usize, // or stack level?
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

fn dump_bin_op(dst: &mut String, sid: &mut usize, op: &str, n_indent: usize) {
    dump_indent(dst, n_indent);
    write!(dst, "let s{} = s{} {} s{};\n", *sid - 2, *sid - 2, op, *sid - 1).unwrap();
    *sid -= 1;
}

fn dump_cmp_op(dst: &mut String, sid: &mut usize, op: &str, n_indent: usize) {
    dump_indent(dst, n_indent);
    write!(
        dst,
        "let s{} = (s{} {} s{}) as i32;\n",
        *sid - 2,
        *sid - 2,
        op,
        *sid - 1,
    )
    .unwrap();
    *sid -= 1;
}

fn dump_cmp_op_u(dst: &mut String, sid: &mut usize, op: &str, ty: &str, n_indent: usize) {
    dump_indent(dst, n_indent);
    write!(
        dst,
        "let s{} = (s{} as {} {} s{} as {}) as i32;\n",
        *sid - 2,
        *sid - 2,
        ty,
        op,
        *sid - 1,
        ty,
    )
    .unwrap();
    *sid -= 1;
}

fn dump_cmp_op_u_lt(dst: &mut String, sid: &mut usize, ty: &str, n_indent: usize) {
    dump_indent(dst, n_indent);
    write!(
        dst,
        "let s{} = ((s{} as {}) < s{} as {}) as i32;\n",
        *sid - 2,
        *sid - 2,
        ty,
        *sid - 1,
        ty,
    )
    .unwrap();
    *sid -= 1;
}

fn dump_label(dst: &mut String, sid: &mut usize, labels: &Vec<Label>, n: u32, n_indent: usize) {
    dump_indent(dst, n_indent);
    let label = &labels[labels.len() - 1 - n as usize];
    let stmt = match label.ty {
        LabelType::Block => "break",
        LabelType::Loop => "continue",
        LabelType::If => "break",
    };
    write!(dst, "{} 'l{}", stmt, label.id).unwrap();
    if label.arity == 1 {
        write!(dst, " s{}", *sid - 1).unwrap();
    }
    dst.push_str(";\n");
}

fn dump_body(dst: &mut String, module: &parity_wasm::elements::Module, body: &parity_wasm::elements::FuncBody) {
    let types = module.type_section().unwrap().types();
    let funcs = module.function_section().unwrap().entries();

    let mut n_indent = 1;
    let mut sid = 0;
    let mut lid = 0;
    let mut labels = Vec::new();
    for inst in body.code().elements() {
        match inst {
            Instruction::Nop => {}
            Instruction::Drop => {
                sid -= 1;
            }
            Instruction::GrowMemory(_) => {
                sid -= 1;
            }
            Instruction::Select => {
                dump_indent(dst, n_indent);
                write!(
                    dst,
                    "let s{} = if s{} != 0i32 {{ s{} }} else {{ s{} }};\n",
                    sid - 3,
                    sid - 3,
                    sid - 2,
                    sid - 1
                )
                .unwrap();
                sid -= 2;
            }
            Instruction::I32Const(n) => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = {}i32;\n", sid, n).unwrap();
                sid += 1;
            }
            Instruction::I64Const(n) => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = {}i64;\n", sid, n).unwrap();
                sid += 1;
            }
            Instruction::GetLocal(i) => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = v{};\n", sid, i).unwrap();
                sid += 1;
            }
            Instruction::SetLocal(i) => {
                dump_indent(dst, n_indent);
                write!(dst, "v{} = s{};\n", i, sid - 1).unwrap();
                sid -= 1;
            }
            Instruction::GetGlobal(i) => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = G{};\n", sid, i).unwrap();
                sid += 1;
            }
            Instruction::SetGlobal(i) => {
                dump_indent(dst, n_indent);
                write!(dst, "G{} = s{};\n", i, sid - 1).unwrap();
                sid -= 1;
            }
            Instruction::I32Load(_, offset) => {
                dump_indent(dst, n_indent);
                write!(
                    dst,
                    "let s{} = *(({} + s{}) as *const i32);\n",
                    sid - 1,
                    offset,
                    sid - 1
                )
                .unwrap();
            }
            Instruction::I32Store(_, offset) => {
                dump_indent(dst, n_indent);
                write!(
                    dst,
                    "*(({} + s{}) as *mut i32) = s{};\n",
                    offset,
                    sid - 2,
                    sid - 1
                )
                .unwrap();
                sid -= 2;
            }
            Instruction::TeeLocal(i) => {
                dump_indent(dst, n_indent);
                write!(dst, "v{} = s{};\n", i, sid - 1).unwrap();
            }
            Instruction::I32Eq | Instruction::I64Eq => {
                dump_cmp_op(dst, &mut sid, "==", n_indent);
            }
            Instruction::I32Ne | Instruction::I64Ne => {
                dump_cmp_op(dst, &mut sid, "!=", n_indent);
            }
            Instruction::I32LtS | Instruction::I64LtS => {
                dump_cmp_op(dst, &mut sid, "<", n_indent);
            }
            Instruction::I32LeS | Instruction::I64LeS => {
                dump_cmp_op(dst, &mut sid, "<=", n_indent);
            }
            Instruction::I32GtS | Instruction::I64GtS => {
                dump_cmp_op(dst, &mut sid, ">", n_indent);
            }
            Instruction::I32LtU => {
                dump_cmp_op_u_lt(dst, &mut sid, "u32", n_indent);
            }
            Instruction::I64LtU => {
                dump_cmp_op_u_lt(dst, &mut sid, "u64", n_indent);
            }
            Instruction::I32GtU => {
                dump_cmp_op_u(dst, &mut sid, ">", "u32", n_indent);
            }
            Instruction::I64GtU => {
                dump_cmp_op_u(dst, &mut sid, ">", "u64", n_indent);
            }
            Instruction::I32Add | Instruction::I64Add => {
                dump_bin_op(dst, &mut sid, "+", n_indent);
            }
            Instruction::I32Sub | Instruction::I64Sub => {
                dump_bin_op(dst, &mut sid, "-", n_indent);
            }
            Instruction::I32Mul | Instruction::I64Mul => {
                dump_bin_op(dst, &mut sid, "*", n_indent);
            }
            Instruction::I32Ctz => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = s{}.trailing_zeros() as i32;\n", sid - 1, sid - 1).unwrap();
            }
            Instruction::I64Ctz => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = s{}.trailing_zeros() as i64;\n", sid - 1, sid - 1).unwrap();
            }
            Instruction::I32Eqz => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = (s{} == 0i32) as i32;\n", sid - 1, sid - 1).unwrap();
            }
            Instruction::I64Eqz => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = (s{} == 0i64) as i64;\n", sid - 1, sid - 1).unwrap();
            }
            Instruction::Call(n) => {
                let ftyp = match &types[funcs[*n as usize].type_ref() as usize] {
                    parity_wasm::elements::Type::Function(t) => t,
                };
                dump_indent(dst, n_indent);
                if let Some(_) = ftyp.return_type() {
                    write!(dst, "let s{} = ", sid - ftyp.params().len()).unwrap();
                }
                write!(dst, "f{}(", n).unwrap();
                for _ in ftyp.params() {
                    write!(dst, "s{},", sid - 1).unwrap();
                    sid -= 1;
                }
                dst.push_str(");\n");
                if let Some(_) = ftyp.return_type() {
                    sid += 1;
                }
            }
            Instruction::CallIndirect(n, _) => {
                let ftyp = match &types[*n as usize] {
                    parity_wasm::elements::Type::Function(t) => t,
                };
                dump_indent(dst, n_indent);
                if let Some(_) = ftyp.return_type() {
                    write!(dst, "let s{} = ", sid - ftyp.params().len() - 1).unwrap();
                }
                write!(dst, "func_table[s{} as usize](", sid - 1).unwrap();
                sid -= 1;
                for _ in ftyp.params() {
                    write!(dst, "s{},", sid - 1).unwrap();
                    sid -= 1;
                }
                dst.push_str(");\n");
                if let Some(_) = ftyp.return_type() {
                    sid += 1;
                }
            }
            Instruction::Block(ty) => {
                dump_indent(dst, n_indent);
                let arity = match ty {
                    BlockType::NoResult => 0,
                    BlockType::Value(_) => {
                        write!(dst, "let s{} = ", sid).unwrap();
                        1
                    }
                };
                write!(dst, "'l{}: loop {{\n", lid).unwrap();
                labels.push(Label {
                    id: lid,
                    ty: LabelType::Block,
                    arity: arity,
                });
                lid += 1;
                n_indent += 1;
            }
            Instruction::Loop(ty) => {
                dump_indent(dst, n_indent);
                let arity = match ty {
                    BlockType::NoResult => 0,
                    BlockType::Value(_) => {
                        write!(dst, "let s{} = ", sid).unwrap();
                        1
                    }
                };
                write!(dst, "'l{}: loop {{\n", lid).unwrap();
                labels.push(Label {
                    id: lid,
                    ty: LabelType::Loop,
                    arity: arity,
                });
                lid += 1;
                n_indent += 1;
            }
            Instruction::Br(n) => {
                dump_label(dst, &mut sid, &labels, *n, n_indent);
                // XXX: adjust stack level.
            }
            Instruction::BrIf(n) => {
                dump_indent(dst, n_indent);
                write!(dst, "if s{} != 0i32 {{\n", sid - 1).unwrap();
                sid -= 1;
                dump_label(dst, &mut sid, &labels, *n, n_indent + 1);
                dump_indent(dst, n_indent);
                dst.push_str("}\n");
            }
            Instruction::BrTable(_) => {
                sid -= 1;
            }
            Instruction::Return => {
                dump_indent(dst, n_indent);
                write!(dst, "return s{};\n", sid - 1).unwrap();
                // XXX: adjust stack level.
            }
            Instruction::If(ty) => {
                dump_indent(dst, n_indent);
                let arity = match ty {
                    BlockType::NoResult => 0,
                    BlockType::Value(_) => {
                        write!(dst, "let s{} = ", sid - 1).unwrap();
                        1
                    }
                };
                write!(dst, "'l{}: loop {{ break if s{} != 0i32 {{\n", lid, sid - 1).unwrap();
                labels.push(Label {
                    id: lid,
                    ty: LabelType::If,
                    arity: arity,
                });
                lid += 1;
                sid -= 1;
                n_indent += 1;
            }
            Instruction::Else => {
                if labels.last().unwrap().arity == 1 {
                    dump_indent(dst, n_indent);
                    write!(dst, "s{}\n", sid - 1).unwrap();
                    sid -= 1;
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
                    LabelType::Block | LabelType::Loop => {
                        dump_indent(dst, n_indent);
                        if label.arity == 1 {
                            write!(dst, "break s{};\n", sid - 1).unwrap();
                        } else {
                            dst.push_str("break;\n");
                        }
                        n_indent -= 1;
                        dump_indent(dst, n_indent);
                        dst.push_str("};\n");
                    }
                    LabelType::If => {
                        if label.arity == 1 {
                            dump_indent(dst, n_indent);
                            write!(dst, "s{}\n", sid - 1).unwrap();
                            sid -= 1;
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

fn dump_module(dst: &mut String, module: &parity_wasm::elements::Module) {
    if let Some(globals) = module.global_section() {
        for (i, global) in globals.entries().iter().enumerate() {
            write!(dst, "static").unwrap();
            if global.global_type().is_mutable() {
                write!(dst, " mut").unwrap();
            }
            let ty = global.global_type().content_type();
            write!(dst, " G{}: {}", i, type_str(ty)).unwrap();
            match global.init_expr().code() {
                [Instruction::I32Const(n), Instruction::End] => write!(dst, " = {};\n", n).unwrap(),
                [Instruction::I64Const(n), Instruction::End] => write!(dst, " = {};\n", n).unwrap(),
                [Instruction::F32Const(x), Instruction::End] => write!(dst, " = {};\n", x).unwrap(),
                [Instruction::F64Const(x), Instruction::End] => write!(dst, " = {};\n", x).unwrap(),
                _ => panic!(),
            }
        }
    }

    let types = module.type_section().unwrap().types();
    let funcs = module.function_section().unwrap().entries();
    let codes = module.code_section().unwrap().bodies();
    assert!(funcs.len() == codes.len());
    for (i, (func, body)) in funcs.iter().zip(codes).enumerate() {
        let ftyp = match &types[func.type_ref() as usize] {
            parity_wasm::elements::Type::Function(t) => t,
        };
        assert!(ftyp.form() == 0x60);

        // function declaration.
        write!(dst, "\nunsafe fn f{}(", i).unwrap();
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
                dump_indent(dst, 1);
                write!(
                    dst,
                    "let mut v{}: {} = core::mem::uninitialized();\n",
                    local_idx,
                    type_str(local.value_type())
                )
                .unwrap();
                local_idx += 1;
            }
        }
        dump_body(dst, &module, body);
        if let Some(_) = ftyp.return_type() {
            dump_indent(dst, 1);
            dst.push_str("s0\n");
        }
        dst.push_str("}\n");
    }
}

pub fn wasm_to_rust<T: convert::AsRef<path::Path>>(path: T) -> result::Result<String, Error> {
    let module = parity_wasm::deserialize_file(path)?;
    let mut dst = String::new();
    dst.push_str("#![allow(unused_mut)]\n");
    dst.push_str("#![allow(unreachable_code)]\n");
    dst.push_str("#![allow(unused_assignments)]\n");
    dst.push_str("#![allow(unused_variables)]\n");
    dst.push_str("#![allow(dead_code)]\n");
    dst.push_str("\n");
    dump_module(&mut dst, &module);
    Ok(dst)
}
