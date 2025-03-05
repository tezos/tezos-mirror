// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::collections::BTreeSet;
use std::collections::HashMap;
use std::time::Duration;

use comfy_table::Cell;
use itertools::Itertools;
use numfmt::Formatter;
use numfmt::Numeric;

use crate::commands::bench::BenchStats;
use crate::commands::bench::NamedStats;

/// Produce a string by formatting the number with the given `separator` and `precision`
pub fn thousand_format<N: Numeric>(content: N, num_decimals: u8) -> String {
    let mut fmt = Formatter::new()
        .separator(',')
        .unwrap()
        .precision(numfmt::Precision::Decimals(num_decimals));
    fmt.fmt2(content).to_string()
}

/// Build a [`Vec<Cell>`] from a `&[&T]` and a factory function
pub fn vec_cell_factory_ref<T: ?Sized, F: Fn(&T) -> Cell>(v: &[&T], factory_f: F) -> Vec<Cell> {
    v.iter().map(|el| factory_f(el)).collect()
}

/// Prepend a [`Cell`] to a [`Vec<Cell>`].
pub fn prepend_cell(first_cell: Cell, mut rest_of_cells: Vec<Cell>) -> Vec<Cell> {
    rest_of_cells.insert(0, first_cell);
    rest_of_cells
}

/// Type holding `(Name of benchmark, Option<Named Single instruction stats>)`
pub type NamedBenchInstrStats<'a, 'b> = (&'a str, Option<&'b NamedStats>);

/// Return an array `r[i][j]` = the stats for the i-th instruction and j-th benchmark & benchmark_name
pub fn tableify_bench_stats<'a, 'b>(
    data: &[(&'a BenchStats, &'b String)],
) -> Vec<Vec<NamedBenchInstrStats<'b, 'a>>> {
    // Collect all the instructions sorted by name
    let instr_names = data
        .iter()
        .flat_map(|s| s.0.instr_stats.iter().flatten().map(|i_s| &i_s.name))
        .collect::<BTreeSet<_>>();

    // Transform each BenchStats instruction data into a (BenchName, HashMap by instruction name)
    // Note: BenchStats with None on instruction-level data will just be an empty hashmap
    let stats_by_instr: Vec<(&String, HashMap<_, _>)> = data
        .iter()
        .map(|s| {
            (
                s.1,
                s.0.instr_stats
                    .iter()
                    .flatten()
                    .map(|i_s| (&i_s.name, i_s))
                    .collect(),
            )
        })
        .collect();

    // For each instruction name, get the corresponding NamedStats or None for that benchmark
    instr_names
        .into_iter()
        .map(|instr_name| {
            stats_by_instr
                .iter()
                .map(|(bench_name, map)| (bench_name.as_str(), map.get(instr_name).copied()))
                .collect()
        })
        .collect_vec()
}

pub fn format_opt_duration(d: &Option<Duration>) -> String {
    match d {
        None => "---".to_string(),
        Some(d) => format!("{d:#?}"),
    }
}
