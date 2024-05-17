// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::commands::bench::{BenchStats, NamedStats};
use comfy_table::Cell;
use itertools::Itertools;
use numfmt::{Formatter, Numeric};
use std::collections::{BTreeSet, HashMap};

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

/// Return an array r[i][j] = the stats for the i-th instruction and j-th benchmark
pub fn tableify_bench_stats<'a>(data: &[&'a BenchStats]) -> Vec<Vec<Option<&'a NamedStats>>> {
    // Collect all the instructions sorted by name
    let names = data
        .iter()
        .flat_map(|s| s.instr_stats.iter().flatten().map(|i_s| &i_s.name))
        .collect::<BTreeSet<_>>();

    // Transform each BenchStats instruction data into a HashMap by instruction name
    // Note: BenchStats with None on instruction-level data will just be an empty hashmap
    let maps: Vec<HashMap<_, _>> = data
        .iter()
        .map(|s| {
            s.instr_stats
                .iter()
                .flatten()
                .map(|i_s| (&i_s.name, i_s))
                .collect()
        })
        .collect();

    // For each instruction name, get the corresponding NamedStats or None for that benchmark
    names
        .into_iter()
        .map(|name| maps.iter().map(|map| map.get(name).copied()).collect())
        .collect_vec()
}
