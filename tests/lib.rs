// (c) Yasuhiro Fujii <http://mimosa-pudica.net>, under MIT License.
use std::fmt::Write;
use std::*;

fn compile<T: AsRef<path::Path>>(path: T) {
    let src = path.as_ref();
    let dst = src.with_extension("rs");
    let rust = wasm2rs::wasm_to_rust(&fs::read(src).unwrap()).unwrap();
    fs::write(&dst, rust).unwrap();
    let is_success = process::Command::new("rustc")
        .arg("--crate-type=dylib")
        .arg("-C")
        .arg("opt-level=s")
        .arg("-o")
        .arg(src.with_extension("so"))
        .arg(dst)
        .status()
        .unwrap()
        .success();
    assert!(is_success);
}

fn run_testsuite<T: AsRef<path::Path>>(path: T) {
    let dir = tempfile::tempdir().unwrap();
    let json_file = dir.path().join("test.json");
    let result = process::Command::new("wast2json")
        .arg("-o")
        .arg(&json_file)
        .arg(path.as_ref())
        .output()
        .unwrap();
    if !result.status.success() {
        return;
    }
    let test: serde_json::Value = serde_json::from_str(&fs::read_to_string(&json_file).unwrap()).unwrap();

    let mut tests = Vec::new();
    let mut wasm_file = path::PathBuf::new();
    let mut test_code = String::new();
    for cmd in test["commands"].as_array().unwrap() {
        match cmd["type"].as_str().unwrap() {
            "module" => {
                let wasm_file = mem::replace(&mut wasm_file, dir.path().join(cmd["filename"].as_str().unwrap()));
                let test_code = mem::replace(&mut test_code, String::new());
                if wasm_file != path::PathBuf::new() {
                    tests.push((wasm_file, test_code));
                }
            }
            "assert_return" | "action" => {
                if cmd["action"]["type"].as_str().unwrap() != "invoke" {
                    continue;
                }
                let func = cmd["action"]["field"]
                    .as_str()
                    .unwrap()
                    .replace(|c: char| !c.is_ascii_alphanumeric(), "_");
                let retv = cmd["expected"].as_array().unwrap().first();
                match retv {
                    Some(_) => write!(test_code, "\tassert_eq!(_{}(", func).unwrap(),
                    None => write!(test_code, "\t_{}(", func).unwrap(),
                };
                for arg in cmd["action"]["args"].as_array().unwrap().iter() {
                    let arg_str = arg["value"].as_str().unwrap();
                    match arg["type"].as_str().unwrap() {
                        "i32" => write!(test_code, "{}u32 as i32,", arg_str).unwrap(),
                        "i64" => write!(test_code, "{}u64 as i64,", arg_str).unwrap(),
                        "f32" => write!(test_code, "f32::from_bits({}),", arg_str).unwrap(),
                        "f64" => write!(test_code, "f64::from_bits({}),", arg_str).unwrap(),
                        _ => panic!(),
                    }
                }
                match retv {
                    Some(retv) => {
                        let retv_str = retv["value"].as_str().unwrap();
                        match retv["type"].as_str().unwrap() {
                            "i32" => write!(test_code, ") as u32, {});\n", retv_str).unwrap(),
                            "i64" => write!(test_code, ") as u64, {});\n", retv_str).unwrap(),
                            "f32" | "f64" => write!(test_code, ").to_bits(), {});\n", retv_str).unwrap(),
                            _ => panic!(),
                        }
                    }
                    None => write!(test_code, ");\n").unwrap(),
                }
            }
            _ => continue,
        }
    }
    if wasm_file != path::PathBuf::new() {
        tests.push((wasm_file, test_code));
    }

    for (wasm_file, test_code) in tests.iter() {
        let mut code = wasm2rs::wasm_to_rust(&fs::read(&wasm_file).unwrap()).unwrap();
        code.push_str("\n");
        code.push_str("fn main() {\n");
        code.push_str(test_code);
        code.push_str("}\n");
        //println!("{}", code);
        let rust_file = dir.path().join("test.rs");
        fs::write(&rust_file, code).unwrap();

        let exec_file = dir.path().join("test");
        let result = process::Command::new("rustc")
            .arg("-C")
            .arg("opt-level=s")
            .arg("-o")
            .arg(&exec_file)
            .arg(&rust_file)
            .output()
            .unwrap();
        print!("{}", String::from_utf8(result.stdout).unwrap());
        print!("{}", String::from_utf8(result.stderr).unwrap());
        assert!(result.status.success(), "compilation");

        let result = process::Command::new(&exec_file).output().unwrap();
        print!("{}", String::from_utf8(result.stdout).unwrap());
        print!("{}", String::from_utf8(result.stderr).unwrap());
        assert!(result.status.success(), "execution");
    }
}

#[test]
fn test_address() {
    run_testsuite("testsuite/address.wast");
}
#[test]
fn test_align() {
    run_testsuite("testsuite/align.wast");
}
#[test]
fn test_binary_leb128() {
    run_testsuite("testsuite/binary-leb128.wast");
}
#[test]
fn test_binary() {
    run_testsuite("testsuite/binary.wast");
}
#[test]
fn test_block() {
    run_testsuite("testsuite/block.wast");
}
#[test]
fn test_br() {
    run_testsuite("testsuite/br.wast");
}
#[test]
fn test_br_if() {
    run_testsuite("testsuite/br_if.wast");
}
#[test]
#[ignore]
fn test_br_table() {
    run_testsuite("testsuite/br_table.wast");
}
#[test]
fn test_break_drop() {
    run_testsuite("testsuite/break-drop.wast");
}
#[test]
fn test_call() {
    run_testsuite("testsuite/call.wast");
}
#[test]
fn test_call_indirect() {
    run_testsuite("testsuite/call_indirect.wast");
}
#[test]
fn test_comments() {
    run_testsuite("testsuite/comments.wast");
}
#[test]
fn test_const() {
    run_testsuite("testsuite/const.wast");
}
#[test]
fn test_conversions() {
    run_testsuite("testsuite/conversions.wast");
}
#[test]
fn test_custom() {
    run_testsuite("testsuite/custom.wast");
}
#[test]
fn test_data() {
    run_testsuite("testsuite/data.wast");
}
#[test]
fn test_elem() {
    run_testsuite("testsuite/elem.wast");
}
#[test]
fn test_endianness() {
    run_testsuite("testsuite/endianness.wast");
}
#[test]
fn test_exports() {
    run_testsuite("testsuite/exports.wast");
}
#[test]
fn test_f32() {
    run_testsuite("testsuite/f32.wast");
}
#[test]
fn test_f32_bitwise() {
    run_testsuite("testsuite/f32_bitwise.wast");
}
#[test]
fn test_f32_cmp() {
    run_testsuite("testsuite/f32_cmp.wast");
}
#[test]
fn test_f64() {
    run_testsuite("testsuite/f64.wast");
}
#[test]
fn test_f64_bitwise() {
    run_testsuite("testsuite/f64_bitwise.wast");
}
#[test]
fn test_f64_cmp() {
    run_testsuite("testsuite/f64_cmp.wast");
}
#[test]
fn test_fac() {
    run_testsuite("testsuite/fac.wast");
}
#[test]
fn test_float_exprs() {
    run_testsuite("testsuite/float_exprs.wast");
}
#[test]
fn test_float_literals() {
    run_testsuite("testsuite/float_literals.wast");
}
#[test]
fn test_float_memory() {
    run_testsuite("testsuite/float_memory.wast");
}
#[test]
fn test_float_misc() {
    run_testsuite("testsuite/float_misc.wast");
}
#[test]
fn test_forward() {
    run_testsuite("testsuite/forward.wast");
}
#[test]
fn test_func() {
    run_testsuite("testsuite/func.wast");
}
#[test]
fn test_func_ptrs() {
    run_testsuite("testsuite/func_ptrs.wast");
}
#[test]
fn test_globals() {
    run_testsuite("testsuite/globals.wast");
}
#[test]
fn test_i32() {
    run_testsuite("testsuite/i32.wast");
}
#[test]
fn test_i64() {
    run_testsuite("testsuite/i64.wast");
}
#[test]
fn test_if() {
    run_testsuite("testsuite/if.wast");
}
#[test]
fn test_imports() {
    run_testsuite("testsuite/imports.wast");
}
#[test]
fn test_inline_module() {
    run_testsuite("testsuite/inline-module.wast");
}
#[test]
fn test_int_exprs() {
    run_testsuite("testsuite/int_exprs.wast");
}
#[test]
fn test_int_literals() {
    run_testsuite("testsuite/int_literals.wast");
}
#[test]
fn test_labels() {
    run_testsuite("testsuite/labels.wast");
}
#[test]
fn test_left_to_right() {
    run_testsuite("testsuite/left-to-right.wast");
}
#[test]
fn test_linking() {
    run_testsuite("testsuite/linking.wast");
}
#[test]
fn test_load() {
    run_testsuite("testsuite/load.wast");
}
#[test]
fn test_local_get() {
    run_testsuite("testsuite/local_get.wast");
}
#[test]
fn test_local_set() {
    run_testsuite("testsuite/local_set.wast");
}
#[test]
fn test_local_tee() {
    run_testsuite("testsuite/local_tee.wast");
}
#[test]
fn test_loop() {
    run_testsuite("testsuite/loop.wast");
}
#[test]
fn test_memory() {
    run_testsuite("testsuite/memory.wast");
}
#[test]
fn test_memory_grow() {
    run_testsuite("testsuite/memory_grow.wast");
}
#[test]
fn test_memory_redundancy() {
    run_testsuite("testsuite/memory_redundancy.wast");
}
#[test]
fn test_memory_size() {
    run_testsuite("testsuite/memory_size.wast");
}
#[test]
fn test_memory_trap() {
    run_testsuite("testsuite/memory_trap.wast");
}
#[test]
fn test_names() {
    run_testsuite("testsuite/names.wast");
}
#[test]
fn test_nop() {
    run_testsuite("testsuite/nop.wast");
}
#[test]
fn test_return() {
    run_testsuite("testsuite/return.wast");
}
#[test]
fn test_select() {
    run_testsuite("testsuite/select.wast");
}
#[test]
fn test_skip_stack_guard_page() {
    run_testsuite("testsuite/skip-stack-guard-page.wast");
}
#[test]
fn test_stack() {
    run_testsuite("testsuite/stack.wast");
}
#[test]
fn test_start() {
    run_testsuite("testsuite/start.wast");
}
#[test]
fn test_store() {
    run_testsuite("testsuite/store.wast");
}
#[test]
fn test_switch() {
    run_testsuite("testsuite/switch.wast");
}
#[test]
fn test_token() {
    run_testsuite("testsuite/token.wast");
}
#[test]
fn test_traps() {
    run_testsuite("testsuite/traps.wast");
}
#[test]
fn test_type() {
    run_testsuite("testsuite/type.wast");
}
#[test]
fn test_typecheck() {
    run_testsuite("testsuite/typecheck.wast");
}
#[test]
fn test_unreachable() {
    run_testsuite("testsuite/unreachable.wast");
}
#[test]
fn test_unreached_invalid() {
    run_testsuite("testsuite/unreached-invalid.wast");
}
#[test]
fn test_unwind() {
    run_testsuite("testsuite/unwind.wast");
}
#[test]
fn test_utf8_custom_section_id() {
    run_testsuite("testsuite/utf8-custom-section-id.wast");
}
#[test]
fn test_utf8_import_field() {
    run_testsuite("testsuite/utf8-import-field.wast");
}
#[test]
fn test_utf8_import_module() {
    run_testsuite("testsuite/utf8-import-module.wast");
}
#[test]
fn test_utf8_invalid_encoding() {
    run_testsuite("testsuite/utf8-invalid-encoding.wast");
}

#[test]
#[ignore]
fn test_self_compilation() {
    if let Ok(output) = process::Command::new("cargo")
        .arg("build")
        .arg("--release")
        .arg("--target=wasm32-unknown-unknown")
        .output()
    {
        if output.status.success() {
            compile("target/wasm32-unknown-unknown/release/wasm2rs.wasm");
        }
    }
}
