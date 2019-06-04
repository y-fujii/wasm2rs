use std::*;

fn compile<T: AsRef<path::Path>>(path: T) -> result::Result<bool, Box<error::Error>> {
    let src = path.as_ref();
    let dst = src.with_extension("rs");
    let rust = wasm2rs::wasm_to_rust(src)?;
    fs::write(&dst, rust)?;
    let result = process::Command::new("rustc")
        .arg("--crate-type=dylib")
        .arg("-o")
        .arg(src.with_extension("so"))
        .arg(dst)
        .status()?
        .success();
    Ok(result)
}

#[test]
fn test() {
    assert!(compile("test_data/fac.wasm").unwrap_or(false));
    assert!(compile("test_data/nop.wasm").unwrap_or(false));
    assert!(compile("test_data/block.wasm").unwrap_or(false));
    assert!(compile("test_data/loop.wasm").unwrap_or(false));
    assert!(compile("test_data/if.wasm").unwrap_or(false));
    assert!(compile("test_data/br.wasm").unwrap_or(false));
    assert!(compile("test_data/br_if.wasm").unwrap_or(false));
    assert!(compile("test_data/const.wasm").unwrap_or(false));
}
