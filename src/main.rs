use std::*;
use wasm2rs;

fn main() -> result::Result<(), Box<error::Error>> {
    fs::write("test/fac.rs", wasm2rs::wasm_to_rust("test/fac.wasm")?)?;
    fs::write("test/nop.rs", wasm2rs::wasm_to_rust("test/nop.wasm")?)?;
    fs::write("test/block.rs", wasm2rs::wasm_to_rust("test/block.wasm")?)?;
    fs::write("test/loop.rs", wasm2rs::wasm_to_rust("test/loop.wasm")?)?;
    Ok(())
}
