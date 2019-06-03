pub use parity_wasm::elements::Error;
use parity_wasm::elements::{BlockType, Instruction, ValueType};
use std::fmt::Write;
use std::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LabelType {
    Block,
    Loop,
    If,
}

#[derive(Debug, Clone)]
struct Label {
    lid: usize,
    ty: LabelType,
    arity: usize,
    sid: usize,
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

fn dump_jump(dst: &mut String, sid: usize, labels: &Vec<Label>, n: u32, n_indent: usize) {
    dump_indent(dst, n_indent);
    let label = &labels[labels.len() - 1 - n as usize];
    match label.ty {
        LabelType::Block | LabelType::If => {
            // br instruction allows to resize the stack.
            //assert!(label.sid + label.arity == sid);
            write!(dst, "break 'l{}", label.lid).unwrap();
            if label.arity == 1 {
                write!(dst, " s{}", sid - 1).unwrap();
            }
            dst.push_str(";\n");
        }
        LabelType::Loop => {
            // br instruction allows to resize the stack.
            //assert!(label.sid == sid);
            write!(dst, "continue 'l{};\n", label.lid).unwrap();
        }
    }
}

fn dump_label(
    dst: &mut String,
    sid: usize,
    lid: &mut usize,
    labels: &mut Vec<Label>,
    n_indent: &mut usize,
    block_ty: &BlockType,
    label_ty: LabelType,
) {
    dump_indent(dst, *n_indent);
    let arity = match block_ty {
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
        arity: arity,
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
                dump_indent(dst, *n_indent - 1);
                dst.push_str("} else {\n");
                break;
            }
            Instruction::End => {
                if depth > 0 {
                    depth -= 1;
                    continue;
                }
                let label = match labels.pop() {
                    Some(e) => e,
                    None => return, // XXX
                };
                *sid = label.sid + label.arity;
                *n_indent -= 1;
                dump_indent(dst, *n_indent);
                match label.ty {
                    LabelType::Block | LabelType::Loop => {
                        dst.push_str("};\n");
                    }
                    LabelType::If => {
                        dst.push_str("}; };\n");
                    }
                }
                break;
            }
            _ => (),
        }
    }
}

fn dump_body(dst: &mut String, module: &parity_wasm::elements::Module, body: &parity_wasm::elements::FuncBody) {
    let types = module.type_section().unwrap().types();
    let funcs = module.function_section().unwrap().entries();

    let mut n_indent = 1;
    let mut sid = 0;
    let mut lid = 0;
    let mut labels = Vec::new();
    let mut inst_it = body.code().elements().iter();
    while let Some(inst) = inst_it.next() {
        match inst {
            Instruction::Nop => {}
            Instruction::Unreachable => {}
            Instruction::Drop => {
                sid -= 1;
            }
            Instruction::GrowMemory(_) => {
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
            Instruction::F32Const(n) => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = f32::from_bits({});\n", sid, n).unwrap();
                sid += 1;
            }
            Instruction::F64Const(n) => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = f64::from_bits({});\n", sid, n).unwrap();
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
            Instruction::I32Load8S(_, offset) => {
                dump_indent(dst, n_indent);
                write!(
                    dst,
                    "let s{} = *(({} + s{}) as *const i8) as i32;\n",
                    sid - 1,
                    offset,
                    sid - 1
                )
                .unwrap();
            }
            Instruction::I32Store(_, offset) => {
                dump_indent(dst, n_indent);
                write!(dst, "*(({} + s{}) as *mut i32) = s{};\n", offset, sid - 2, sid - 1).unwrap();
                sid -= 2;
            }
            Instruction::I32Store8(_, offset) => {
                dump_indent(dst, n_indent);
                write!(dst, "*(({} + s{}) as *mut i8) = s{} as i8;\n", offset, sid - 2, sid - 1).unwrap();
                sid -= 2;
            }
            Instruction::I32Store16(_, offset) => {
                dump_indent(dst, n_indent);
                write!(dst, "*(({} + s{}) as *mut i16) = s{} as i16;\n", offset, sid - 2, sid - 1).unwrap();
                sid -= 2;
            }
            Instruction::TeeLocal(i) => {
                dump_indent(dst, n_indent);
                write!(dst, "v{} = s{};\n", i, sid - 1).unwrap();
            }
            Instruction::I32Eq | Instruction::I64Eq | Instruction::F32Eq | Instruction::F64Eq => {
                dump_cmp_op(dst, &mut sid, "==", n_indent);
            }
            Instruction::I32Ne | Instruction::I64Ne | Instruction::F32Ne | Instruction::F64Ne => {
                dump_cmp_op(dst, &mut sid, "!=", n_indent);
            }
            Instruction::I32LtS | Instruction::I64LtS | Instruction::F32Lt | Instruction::F64Lt => {
                dump_cmp_op(dst, &mut sid, "<", n_indent);
            }
            Instruction::I32LeS | Instruction::I64LeS | Instruction::F32Le | Instruction::F64Le => {
                dump_cmp_op(dst, &mut sid, "<=", n_indent);
            }
            Instruction::I32GtS | Instruction::I64GtS | Instruction::F32Gt | Instruction::F64Gt => {
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
            Instruction::I32LeU => {
                dump_cmp_op_u(dst, &mut sid, "<=", "u32", n_indent);
            }
            Instruction::I64LeU => {
                dump_cmp_op_u(dst, &mut sid, "<=", "u64", n_indent);
            }
            Instruction::F32Neg | Instruction::F64Neg => {
                dump_indent(dst, n_indent);
                write!(dst, "let s{} = -s{};\n", sid - 1, sid - 1).unwrap();
            }
            Instruction::I32Add | Instruction::I64Add | Instruction::F32Add | Instruction::F64Add => {
                dump_bin_op(dst, &mut sid, "+", n_indent);
            }
            Instruction::I32Sub | Instruction::I64Sub | Instruction::F32Sub | Instruction::F64Sub => {
                dump_bin_op(dst, &mut sid, "-", n_indent);
            }
            Instruction::I32Mul | Instruction::I64Mul | Instruction::F32Mul | Instruction::F64Mul => {
                dump_bin_op(dst, &mut sid, "*", n_indent);
            }
            Instruction::I32DivS | Instruction::I64DivS | Instruction::F32Div | Instruction::F64Div => {
                dump_bin_op(dst, &mut sid, "/", n_indent);
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
                write!(dst, "let s{} = (s{} == 0i64) as i32;\n", sid - 1, sid - 1).unwrap();
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
                // XXX
                dst.push_str("// ");
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
                dump_label(dst, sid, &mut lid, &mut labels, &mut n_indent, ty, LabelType::Block);
            }
            Instruction::Loop(ty) => {
                dump_label(dst, sid, &mut lid, &mut labels, &mut n_indent, ty, LabelType::Loop);
            }
            Instruction::Br(n) => {
                dump_jump(dst, sid, &labels, *n, n_indent);
                skip_to_end(dst, &mut sid, &mut labels, &mut n_indent, &mut inst_it);
            }
            Instruction::BrIf(n) => {
                dump_indent(dst, n_indent);
                write!(dst, "if s{} != 0i32 {{\n", sid - 1).unwrap();
                sid -= 1;
                dump_jump(dst, sid, &labels, *n, n_indent + 1);
                dump_indent(dst, n_indent);
                dst.push_str("}\n");
            }
            Instruction::BrTable(_) => {
                skip_to_end(dst, &mut sid, &mut labels, &mut n_indent, &mut inst_it);
            }
            Instruction::Return => {
                dump_indent(dst, n_indent);
                write!(dst, "return s{};\n", sid - 1).unwrap();
                skip_to_end(dst, &mut sid, &mut labels, &mut n_indent, &mut inst_it);
            }
            Instruction::If(block_ty) => {
                dump_indent(dst, n_indent);
                let arity = match block_ty {
                    BlockType::NoResult => 0,
                    BlockType::Value(value_ty) => {
                        write!(dst, "let s{}: {} = ", sid - 1, type_str(*value_ty)).unwrap();
                        1
                    }
                };
                write!(dst, "'l{}: loop {{ break if s{} != 0i32 {{\n", lid, sid - 1).unwrap();
                sid -= 1;
                labels.push(Label {
                    lid: lid,
                    ty: LabelType::If,
                    arity: arity,
                    sid: sid,
                });
                lid += 1;
                n_indent += 1;
            }
            Instruction::Else => {
                let label = labels.last().unwrap();
                assert!(label.ty == LabelType::If);
                assert!(label.sid + label.arity == sid);
                if label.arity == 1 {
                    dump_indent(dst, n_indent);
                    write!(dst, "s{}\n", sid - 1).unwrap();
                    sid = label.sid;
                }
                dump_indent(dst, n_indent - 1);
                dst.push_str("} else {\n");
            }
            Instruction::End => {
                let label = match labels.pop() {
                    Some(e) => e,
                    None => continue, // XXX: check sid.
                };
                assert!(label.sid + label.arity == sid);
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
                [Instruction::F32Const(n), Instruction::End] => write!(dst, " = f32::from_bits({});\n", n).unwrap(),
                [Instruction::F64Const(n), Instruction::End] => write!(dst, " = f32::from_bits({});\n", n).unwrap(),
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
