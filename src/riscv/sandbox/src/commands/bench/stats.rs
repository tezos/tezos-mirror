// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use core::fmt;
use std::time::Duration;

use itertools::Itertools;
use meansd::MeanSD;
use serde::Deserialize;
use serde::Serialize;

use super::data::BenchData;
use super::data::InstructionData;
use crate::format_status;
use crate::table::utils::thousand_format;

/// Serializable data for instruction-level statistics
#[derive(Serialize, Deserialize, Debug, Clone)]
pub struct NamedStats {
    pub name: String,
    pub count: usize,
    pub total: Duration,
    pub average: Duration,
    pub median: Option<Duration>,
    pub stddev: Option<Duration>,
}

impl NamedStats {
    /// Returns [`None`] if either the array is empty or `count` overflows [`u32`]
    pub fn from_sorted_times(times: &Vec<Duration>, name: String) -> Result<Self, String> {
        let err = format!("Could not generate {name} stats from array:\n{times:?}\nIs it empty?");
        let count = times.len();
        let total: Duration = times.iter().sum();
        let average = total
            .checked_div(count.try_into().map_err(|_| &err)?)
            .ok_or(&err)?;
        let median = *times.get(count / 2).ok_or(&err)?;
        let mut sd = MeanSD::default();
        times.iter().for_each(|t| sd.update(t.as_nanos() as f64));
        let stddev = Duration::from_nanos(sd.sstdev().round() as u64);

        Ok(Self {
            name,
            count,
            total,
            average,
            median: Some(median),
            stddev: Some(stddev),
        })
    }

    /// Used to combine statistics from two related statistics measuring the same metric.
    /// Median and Stddev can not be trivially calculated by just knowing what is in [`NamedStats`]
    pub fn aggregate(&mut self, other: &NamedStats, name: &str) -> Self {
        Self {
            name: name.to_string(),
            average: (self.total + other.total).div_f64((self.count + other.count) as f64),
            count: self.count + other.count,
            total: self.total + other.total,
            median: None,
            stddev: None,
        }
    }
}

impl fmt::Display for NamedStats {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let tag_f = format!("Name:   {:?}", self.name);
        let cnt_f = format!("Count:  {:?}", self.count);
        let total_f = format!("Total:  {:?}", self.total);
        let avg_f = format!("Avg:    {:?}", self.average);
        let med_f = format!("Median: {:?}", self.median);
        let stdev_f = format!("Stddev: {:?}", self.stddev);
        writeln!(
            f,
            "{tag_f}\n{cnt_f}\n{total_f}\n{avg_f}\n{med_f}\n{stdev_f}"
        )
    }
}

/// Serializable stats for a benchmark run.
#[derive(Serialize, Deserialize)]
pub struct BenchStats {
    pub bench_duration_stats: NamedStats,
    pub total_steps: usize,
    pub instr_stats: Option<Vec<NamedStats>>,
    pub run_result: String,
}

impl BenchStats {
    fn compute_instr_stats(
        instr_data: impl Iterator<Item = InstructionData>,
    ) -> Result<Option<Vec<NamedStats>>, String> {
        let mut instr_stats: Vec<NamedStats> = instr_data
            .into_iter()
            .flatten()
            .into_group_map()
            .into_iter()
            .map(|(tag, times)| {
                let mut times: Vec<Duration> = times.into_iter().flatten().collect();
                times.sort();
                NamedStats::from_sorted_times(&times, format!("{tag}"))
            })
            // If one element is [Err], then collect will return [Err]
            .collect::<Result<_, _>>()?;

        let instr_stats = if instr_stats.is_empty() {
            None
        } else {
            instr_stats.sort_by(|a, b| b.count.cmp(&a.count));
            Some(instr_stats)
        };

        Ok(instr_stats)
    }

    /// Fails with [`Err`] if for an instruction the corresponding
    /// instruction's [`NamedStats`] can not be created.
    pub(super) fn from_data(data: BenchData) -> Result<Self, String> {
        let instr_stats = Self::compute_instr_stats(data.instr_count.into_iter())?;
        let bench_duration_stats =
            NamedStats::from_sorted_times(&vec![data.duration], "Bench duration".into())?;

        Ok(BenchStats {
            bench_duration_stats,
            total_steps: data.steps,
            instr_stats,
            run_result: format_status(&data.run_result),
        })
    }

    /// Fails with [`Err`] if for an instruction the corresponding
    /// instruction's [`NamedStats`] can not be created.
    pub(super) fn from_data_list(data: Vec<BenchData>) -> Result<Self, String> {
        let bench_times = data.iter().map(|bench| bench.duration).collect::<Vec<_>>();
        let bench_duration_stats =
            NamedStats::from_sorted_times(&bench_times, "Interpreter time".into())?;
        let total_steps = data.iter().map(|i| i.steps).sum();

        let it = data.into_iter().filter_map(|i| i.instr_count);
        let instr_stats = Self::compute_instr_stats(it)?;

        Ok(BenchStats {
            bench_duration_stats,
            total_steps,
            instr_stats,
            run_result: "Multiple iterations results".to_string(),
        })
    }

    /// If multiple binaries have been executed, this function
    /// combines the stats for the same instruction coming from different binaries.
    pub fn normalize_instr_data(&mut self) {
        self.instr_stats = self
            .instr_stats
            .iter()
            .flatten()
            .into_group_map_by(|el| &el.name)
            .into_values()
            .map(|val_list| {
                val_list
                    .iter()
                    .map(|&e| e.clone())
                    .reduce(|mut acc, el| acc.aggregate(&el, &el.name))
            })
            .collect::<Option<Vec<_>>>();
    }

    /// Function used to combine [`BenchStats`] in case multiple binaries are executed.
    pub fn combine(mut self, other: BenchStats) -> Self {
        Self {
            bench_duration_stats: self
                .bench_duration_stats
                .aggregate(&other.bench_duration_stats, "mixed runs"),
            instr_stats: match self.instr_stats {
                None => other.instr_stats,
                Some(mut stats) => Some({
                    stats.append(&mut other.instr_stats.unwrap_or_default());
                    stats
                }),
            },
            run_result: "Mixed runs".to_string(),
            total_steps: self.total_steps + other.total_steps,
        }
    }
}

impl BenchStats {
    pub fn instruction_duration(&self) -> Duration {
        // The time taken for instructions to run, either the whole bench duration,
        // or the added instruction times to account for overhead
        match &self.instr_stats {
            Some(counts) if !counts.is_empty() => counts.iter().map(|i| i.total).sum::<Duration>(),
            _ => self.bench_duration_stats.total,
        }
    }
}

impl fmt::Display for BenchStats {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let instr_duration = self.instruction_duration();
        let instr_speed =
            thousand_format(self.total_steps as f64 / instr_duration.as_secs_f64(), 2);

        writeln!(f, "Outcome:        {}", self.run_result)?;
        writeln!(f, "Total steps:    {}", self.total_steps)?;
        writeln!(f, "Speed:          {} instr / s", instr_speed)?;
        writeln!(f)?;
        writeln!(f, " Bench stats:\n{}", self.bench_duration_stats)?;

        let stats = match &self.instr_stats {
            None => "Not captured".into(),
            Some(counts) => {
                let intro = format!("Distinct instructions: {}", counts.len());

                let bench_overhead = self.bench_duration_stats.total - instr_duration;
                let per_instr_overhead = bench_overhead.div_f64(self.total_steps as f64);
                let instr_duration_f = format!("Instr Duration:        {instr_duration:?}");
                let overhead_f = format!(
                    "Overhead:              {bench_overhead:?} (total) / {per_instr_overhead:?} (per instr.)"
                );

                let instr_data = counts
                    .iter()
                    .map(|stat| format!("{stat}---------------------\n"))
                    .fold("".to_string(), |a, b| a + &b);
                let summary = format!("{intro}\n{instr_duration_f}\n{overhead_f}\n");
                let instr_data = format!(" Individual instructions:\n{instr_data}");
                format!("{summary}\n{instr_data}")
            }
        };
        write!(f, " Instruction statistics:\n{}", stats)
    }
}
