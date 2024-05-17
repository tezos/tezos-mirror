// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use self::utils::{prepend_cell, tableify_bench_stats, thousand_format, vec_cell_factory_ref};
use crate::commands::bench::{BenchStats, NamedStats};
use comfy_table::{
    modifiers::UTF8_ROUND_CORNERS, presets::UTF8_FULL, Attribute, Cell, CellAlignment, Color,
    ContentArrangement, Table,
};
use itertools::Itertools;

pub mod utils;

fn row_header_table(table: &mut Table, filename: Vec<&str>) {
    let cells = vec_cell_factory_ref(&filename, |f| {
        Cell::new(f)
            .add_attribute(Attribute::Bold)
            .fg(Color::DarkCyan)
            .set_alignment(CellAlignment::Center)
    });
    table.set_header(prepend_cell(
        Cell::new("Benchmark name")
            .fg(Color::DarkGreen)
            .add_attribute(Attribute::Bold),
        cells,
    ));
}

fn section_summary(table: &mut Table, data: &[(&BenchStats, &String)]) {
    row_header_table(table, vec!["Outcome", "Total steps", "Instruction / s"]);
    for &(stats, name) in data {
        let steps = stats.total_steps;
        let instr_duration = stats.instruction_duration();
        let speed = thousand_format(steps as f64 / instr_duration.as_secs_f64(), 2);

        let name = Cell::new(name);
        let outcome = Cell::new(&stats.run_result);
        let total_steps =
            Cell::new(&thousand_format(stats.total_steps, 0)).set_alignment(CellAlignment::Right);
        let speed = Cell::new(speed).set_alignment(CellAlignment::Right);
        table.add_row(vec![name, outcome, total_steps, speed]);
    }
}

fn content_stats_line_opt(name: &str, stats: &Option<&NamedStats>) -> Vec<Cell> {
    match stats {
        Some(s) => content_stats_line(name, s),
        None => vec![
            Cell::new(name),
            Cell::new("---").set_alignment(CellAlignment::Center),
            Cell::new("---").set_alignment(CellAlignment::Center),
            Cell::new("---").set_alignment(CellAlignment::Center),
            Cell::new("---").set_alignment(CellAlignment::Center),
        ],
    }
}

fn content_stats_line(name: &str, stats: &NamedStats) -> Vec<Cell> {
    vec![
        Cell::new(name),
        Cell::new(thousand_format(stats.count, 0)).set_alignment(CellAlignment::Right),
        Cell::new(format!("{:#?}", stats.total)).set_alignment(CellAlignment::Right),
        Cell::new(format!("{:#?}", stats.average)).set_alignment(CellAlignment::Right),
        Cell::new(format!("{:#?}", stats.median)).set_alignment(CellAlignment::Right),
        Cell::new(format!("{:#?}", stats.stddev)).set_alignment(CellAlignment::Right),
    ]
}

fn row_header_stats(name_cell: Cell) -> Vec<Cell> {
    let prop_cells = vec_cell_factory_ref(&["Count", "Total", "Avg", "Median", "Stddev"], |f| {
        Cell::new(f)
            .add_attribute(Attribute::Bold)
            .fg(Color::DarkCyan)
            .set_alignment(CellAlignment::Center)
    });
    prepend_cell(name_cell, prop_cells)
}

fn section_interpreter_stats(table: &mut Table, data: &[(&BenchStats, &String)]) {
    table.add_row(Vec::<Cell>::new());
    table.add_row(row_header_stats(
        Cell::new("Interpreter stats").fg(Color::DarkGreen),
    ));
    for &(stats, name) in data {
        table.add_row(content_stats_line(name, &stats.bench_duration_stats));
    }
}

fn section_instruction_stats(table: &mut Table, data: &[(&BenchStats, &String)]) {
    table.add_row(Vec::<Cell>::new());

    table.add_row(vec![Cell::new("Distinct Instructions").fg(Color::DarkGreen)]);

    for instr_data in tableify_bench_stats(&data.iter().map(|d| d.0).collect_vec()) {
        // instruction name + props
        let name = match instr_data.iter().flatten().next() {
            Some(s) => &s.name,
            _ => "No instruction data",
        };
        table.add_row(row_header_stats(
            Cell::new(name)
                .add_attribute(Attribute::Bold)
                .fg(Color::DarkBlue),
        ));

        // content
        for (i, instr_stat) in instr_data.iter().enumerate() {
            let bench_name = data[i].1;
            table.add_row(content_stats_line_opt(bench_name, instr_stat));
        }
    }
}

pub fn table_from_stats(data: &[(&BenchStats, &String)]) -> Table {
    let mut table = Table::new();
    table
        .load_preset(UTF8_FULL)
        .apply_modifier(UTF8_ROUND_CORNERS)
        .set_content_arrangement(ContentArrangement::Dynamic);

    section_summary(&mut table, data);
    section_interpreter_stats(&mut table, data);
    section_instruction_stats(&mut table, data);

    table
}
