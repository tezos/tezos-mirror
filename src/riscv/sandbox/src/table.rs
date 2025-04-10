// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use comfy_table::Attribute;
use comfy_table::Cell;
use comfy_table::CellAlignment;
use comfy_table::Color;
use comfy_table::ContentArrangement;
use comfy_table::Table;
use comfy_table::modifiers::UTF8_ROUND_CORNERS;
use comfy_table::presets::UTF8_FULL;
use itertools::Itertools;

use self::utils::NamedBenchInstrStats;
use self::utils::prepend_cell;
use self::utils::tableify_bench_stats;
use self::utils::thousand_format;
use self::utils::vec_cell_factory_ref;
use crate::cli::SortInstr;
use crate::cli::SortRuns;
use crate::cli::TableSortArgs;
use crate::commands::bench::BenchStats;
use crate::commands::bench::NamedStats;
use crate::table::utils::format_opt_duration;

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
            Cell::new(thousand_format(stats.total_steps, 0)).set_alignment(CellAlignment::Right);
        let speed = Cell::new(speed).set_alignment(CellAlignment::Right);
        table.add_row(vec![name, outcome, total_steps, speed]);
    }
}

fn content_stats_line_opt(name: &str, stats: Option<&NamedStats>) -> Vec<Cell> {
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
        Cell::new(format!("±{}", format_opt_duration(&stats.stddev)))
            .set_alignment(CellAlignment::Right),
        Cell::new(format_opt_duration(&stats.median)).set_alignment(CellAlignment::Right),
    ]
}

fn row_header_stats(name_cell: Cell) -> Vec<Cell> {
    let prop_cells = vec_cell_factory_ref(&["Count", "Total", "Avg", "Stddev", "Median"], |f| {
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

fn section_instruction_stats(table: &mut Table, data: &[&[&NamedBenchInstrStats]]) {
    table.add_row(Vec::<Cell>::new());

    table.add_row(vec![
        Cell::new("Distinct Instructions").fg(Color::DarkGreen),
    ]);

    for instr_data in data {
        // instruction name + props
        let name = match instr_data.iter().filter_map(|item| item.1).next() {
            Some(s) => &s.name,
            _ => "No instruction data",
        };
        table.add_row(row_header_stats(
            Cell::new(name)
                .add_attribute(Attribute::Bold)
                .fg(Color::DarkBlue),
        ));

        // content
        for instr_stat in instr_data.iter() {
            table.add_row(content_stats_line_opt(instr_stat.0, instr_stat.1));
        }
    }
}

/// Sort the instruction groups.
fn sort_instruction_groups<'a, 'b, 'c>(
    ordering: &SortInstr,
    data: &'c [Vec<NamedBenchInstrStats<'a, 'b>>],
) -> Vec<&'c Vec<NamedBenchInstrStats<'a, 'b>>> {
    // Obtain an array of references to the lines that will be sorted.
    let mut data = data.iter().collect_vec();

    let instr_name = |instr_data: &Vec<NamedBenchInstrStats<'a, 'b>>| match instr_data.iter().next()
    {
        None => "No instruction data",
        Some(el) => el.0,
    };

    let max_avg = |instr_data: &Vec<NamedBenchInstrStats>| {
        instr_data.iter().map(|el| el.1.map(|e| e.average)).max()
    };

    let total_count = |instr_data: &Vec<NamedBenchInstrStats>| {
        instr_data
            .iter()
            .map(|el| match el.1 {
                None => 0,
                Some(stats) => stats.count,
            })
            .sum::<usize>()
    };

    data.sort_by(|&a, &b| match ordering {
        SortInstr::Alphabetic => instr_name(a).cmp(instr_name(b)),
        SortInstr::MaxAvg => max_avg(a).cmp(&max_avg(b)).reverse(),
        SortInstr::TotalCount => total_count(a).cmp(&total_count(b)).reverse(),
    });

    data
}

fn sort_runs_per_instr<'a, 'b, 'c>(
    ordering: &SortRuns,
    data: &[&'c Vec<NamedBenchInstrStats<'a, 'b>>],
) -> Vec<Vec<&'c NamedBenchInstrStats<'a, 'b>>> {
    let bench_name = |stats: &NamedBenchInstrStats<'a, 'b>| stats.0;

    let average = |stats: &NamedBenchInstrStats| stats.1.map(|s| s.average);

    let total = |stats: &NamedBenchInstrStats| stats.1.map(|s| s.total);

    data.iter()
        .map(|group| {
            let mut group = group.iter().collect_vec();
            group.sort_by(|&a, &b| match ordering {
                SortRuns::Alphabetic => bench_name(a).cmp(bench_name(b)),
                SortRuns::Input => std::cmp::Ordering::Equal,
                SortRuns::Speed => average(a).cmp(&average(b)).reverse(),
                SortRuns::Total => total(a).cmp(&total(b)).reverse(),
            });
            group
        })
        .collect_vec()
}

pub fn table_from_stats(sort_opts: &TableSortArgs, data: &[(&BenchStats, &String)]) -> Table {
    let by_instr_data = tableify_bench_stats(data);
    let sorted_data = sort_instruction_groups(&sort_opts.sort_instr, &by_instr_data);
    let sorted_data = sort_runs_per_instr(&sort_opts.sort_runs, &sorted_data);

    let mut table = Table::new();
    table
        .load_preset(UTF8_FULL)
        .apply_modifier(UTF8_ROUND_CORNERS)
        .set_content_arrangement(ContentArrangement::Dynamic);

    section_summary(&mut table, data);
    section_interpreter_stats(&mut table, data);
    section_instruction_stats(
        &mut table,
        &sorted_data.iter().map(|line| line.as_slice()).collect_vec(),
    );

    table
}
