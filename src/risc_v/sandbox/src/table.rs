// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use self::utils::{highlight, thousand_format};
use crate::commands::bench::{BenchStats, NamedStats};
use comfy_table::{
    modifiers::UTF8_ROUND_CORNERS, presets::UTF8_FULL, Attribute, Cell, CellAlignment, Color,
    ContentArrangement, Table,
};

pub mod utils;

fn row_header(table: &mut Table, filename: &str) {
    table.set_header(vec![
        Cell::new("Benchmark name"),
        Cell::new(filename)
            .add_attribute(Attribute::Bold)
            .fg(Color::DarkCyan)
            .set_alignment(CellAlignment::Center),
    ]);
}

fn row_outcome(table: &mut Table, stats: &BenchStats) {
    let result = &stats.run_result;
    table.add_row(vec![Cell::new("Outcome"), Cell::new(result)]);
}

fn row_total_steps(table: &mut Table, stats: &BenchStats) {
    let steps = thousand_format(stats.total_steps, 0);
    table.add_row(vec![Cell::new("Total steps"), Cell::new(steps)]);
}

fn row_instr_speed(table: &mut Table, stats: &BenchStats) {
    let steps = stats.total_steps;
    let instr_duration = stats.instruction_duration();
    let speed = thousand_format(steps as f64 / instr_duration.as_secs_f64(), 2);
    table.add_row(vec![
        highlight(Cell::new("Instruction / s")),
        highlight(Cell::new(speed)).add_attribute(Attribute::Underlined),
    ]);
}

fn rows_named_stats(table: &mut Table, stats: &NamedStats) {
    let mut make_stats_line = |name_cell: Cell, content_cell: Cell| {
        table.add_row(vec![name_cell, content_cell]);
    };
    let name_column = |content: &str| Cell::new(format!("  {content}"));

    make_stats_line(
        Cell::new(format!(" {}", &stats.name))
            .add_attribute(Attribute::Bold)
            .fg(Color::DarkBlue),
        Cell::new(""),
    );
    make_stats_line(
        name_column("Count"),
        Cell::new(thousand_format(stats.count, 0)),
    );
    make_stats_line(
        name_column("Total"),
        Cell::new(format!("{:?}", stats.total)),
    );
    make_stats_line(
        highlight(name_column("Avg")),
        highlight(Cell::new(format!("{:?}", stats.average))),
    );
    make_stats_line(
        highlight(name_column("Median")),
        highlight(Cell::new(format!("{:?}", stats.median))),
    );
    make_stats_line(
        name_column("Stddev"),
        Cell::new(format!("{:?}", stats.stddev)),
    );
}

fn rows_instr_stats(table: &mut Table, stats: &BenchStats) {
    table.add_row::<Vec<Cell>>(vec![]);

    let distinct = match &stats.instr_stats {
        None => "-----".into(),
        Some(data) => data.len().to_string(),
    };
    table.add_row(vec![
        Cell::new("Distinct Instructions")
            .add_attribute(Attribute::Bold)
            .fg(Color::Green),
        Cell::new(distinct),
    ]);
    for instr_data in stats.instr_stats.iter().flatten() {
        rows_named_stats(table, instr_data)
    }
}

pub fn table_from_stats(stats: &BenchStats, json_file: &String) -> Table {
    let filename = match json_file.strip_suffix(".json") {
        Some(f) => f,
        None => json_file,
    };
    let mut table = Table::new();
    table
        .load_preset(UTF8_FULL)
        .apply_modifier(UTF8_ROUND_CORNERS)
        .set_content_arrangement(ContentArrangement::Dynamic);

    row_header(&mut table, filename);
    row_outcome(&mut table, stats);
    row_total_steps(&mut table, stats);
    row_instr_speed(&mut table, stats);
    rows_named_stats(&mut table, &stats.bench_duration_stats);
    rows_instr_stats(&mut table, stats);

    table
}
