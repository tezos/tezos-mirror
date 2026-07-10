//
// SPDX-License-Identifier: MIT
// SPDX-FileCopyrightText: 2026 Nomadic Labs <contact@nomadic-labs.com>
//

use std::{env, error::Error, fs, path::PathBuf};

use tezos_smart_rollup_instrument::{instrument, DEFAULT_MAX_CALL_DEPTH};

pub fn main() -> Result<(), Box<dyn Error>> {
    let input = env::args().nth(1).expect("usage: instrument <PATH>");
    let bytes = fs::read(&input)?;

    let instrumented = instrument(&bytes, DEFAULT_MAX_CALL_DEPTH)?;

    let mut out = PathBuf::from(&input);
    out.set_extension("instr.wasm");
    fs::write(&out, &instrumented.module)?;
    println!(
        "wrote {} ({} bytes); instrumented {} functions",
        out.display(),
        instrumented.module.len(),
        instrumented.functions,
    );
    Ok(())
}
