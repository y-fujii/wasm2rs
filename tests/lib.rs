use std::*;

fn compile<T: AsRef<path::Path>>(path: T) {
    let src = path.as_ref();
    let dst = src.with_extension("rs");
    let rust = wasm2rs::wasm_to_rust(src).unwrap();
    fs::write(&dst, rust).unwrap();
    let is_success = process::Command::new("rustc")
        .arg("--crate-type=dylib")
        .arg("-o")
        .arg(src.with_extension("so"))
        .arg(dst)
        .status()
        .unwrap()
        .success();
    assert!(is_success);
}

#[test]
fn official() {
    compile("test_data/fac.wasm");
    compile("test_data/nop.wasm");
    compile("test_data/block.wasm");
    compile("test_data/loop.wasm");
    compile("test_data/if.wasm");
    compile("test_data/br.wasm");
    compile("test_data/br_if.wasm");
    //compile("test_data/const.wasm");
}

#[test]
fn self_compilation() {
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
