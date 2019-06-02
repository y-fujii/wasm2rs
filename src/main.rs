use std::*;
use wasm2rs;

fn main() -> result::Result<(), Box<error::Error>> {
    println!("{}", wasm2rs::wasm_to_rust("test/fac.wasm")?);
    println!("");
    println!("{}", wasm2rs::wasm_to_rust("test/nop.wasm")?);
    Ok(())
}
