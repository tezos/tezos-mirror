// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use crate::{cli::BenchOptions, table};
use std::{error::Error, fs::File};

pub use crate::commands::bench::stats::{BenchStats, NamedStats};

pub mod commands;
mod data;
mod stats;

fn save_to_file(stats: &BenchStats, opts: &BenchOptions) -> Result<(), Box<dyn Error>> {
    let mut file = File::create(&opts.output)?;
    serde_json::to_writer(&mut file, &stats)?;
    Ok(())
}

fn show_results(stats: &BenchStats, opts: &BenchOptions) {
    match opts.pretty {
        false => println!("{stats}"),
        true => {
            let table = table::table_from_stats(stats, &opts.output);
            println!("{table}")
        }
    }
}
