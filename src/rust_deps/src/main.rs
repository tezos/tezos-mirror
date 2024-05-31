// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use armerge::{ArMerger, ArmergeKeepOrRemove};
use regex::Regex;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let merger = ArMerger::new_from_paths(
        ["target/release/liboctez_rust_deps.a"],
        "liboctez_rust_deps.a",
    )?;

    merger.merge_and_localize(
        ArmergeKeepOrRemove::KeepSymbols,
        [
            "_?(wat2wasm|wasm2wat)",
            "_?(wasm_|wasmer_|librustzcash_|octez_|rust_).*",
        ]
        .into_iter()
        .map(Regex::new)
        .collect::<Result<Vec<_>, _>>()?,
    )?;

    Ok(())
}
