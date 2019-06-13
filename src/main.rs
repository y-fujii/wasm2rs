// (c) Yasuhiro Fujii <http://mimosa-pudica.net>, under MIT License.
use std::*;
use wasm2rs;

fn main() -> result::Result<(), Box<error::Error>> {
    for arg in env::args().skip(1) {
        let path = path::Path::new(&arg);
        let rust = wasm2rs::wasm_to_rust(path)?;
        fs::write(path.with_extension("rs"), rust)?;
    }
    Ok(())
}
