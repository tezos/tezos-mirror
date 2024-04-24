use super::data::{BenchData, InstructionData};
use crate::format_status;
use core::fmt;
use meansd::MeanSD;
use numfmt::Formatter;
use serde::{Deserialize, Serialize};
use std::time::Duration;

/// Serializable data for instruction-level statistics
#[derive(Serialize, Deserialize)]
struct NamedStats {
    name: String,
    count: usize,
    total: Duration,
    average: Duration,
    median: Duration,
    stddev: Duration,
}

impl NamedStats {
    /// Returns [`None`] if either the array is empty or `count` overflows [`u32`]
    pub fn from_sorted_times(times: &Vec<Duration>, name: String) -> Result<Self, String> {
        let err =
            || format!("Could not generate {name} stats from array:\n{times:?}\nIs it empty?");
        let count = times.len();
        let total: Duration = times.iter().sum();
        let average = total
            .checked_div(count.try_into().ok().ok_or(err())?)
            .ok_or(err())?;
        let median = *times.get(count / 2).ok_or(err())?;
        let mut sd = MeanSD::default();
        times.iter().for_each(|t| sd.update(t.as_nanos() as f64));
        let stddev = Duration::from_nanos(sd.sstdev().round() as u64);

        Ok(Self {
            name,
            count,
            total,
            average,
            median,
            stddev,
        })
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
    duration: Duration,
    steps: usize,
    instr_stats: Option<Vec<NamedStats>>,
    run_result: String,
}

impl BenchStats {
    fn compute_instr_stats(
        instr_data: impl Iterator<Item = InstructionData>,
    ) -> Result<Option<Vec<NamedStats>>, String> {
        let mut instr_stats: Vec<NamedStats> = instr_data
            .into_iter()
            .flatten()
            .map(|(tag, mut times)| {
                times.sort();
                NamedStats::from_sorted_times(&times, format!("{tag:?}"))
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

    /// Fails with [`Err`] if for an instruction the corresponding [`InstructionStats`] can not be created.
    pub(super) fn from_data(data: BenchData) -> Result<Self, String> {
        let instr_stats = Self::compute_instr_stats(data.instr_count.into_iter())?;

        Ok(BenchStats {
            duration: data.duration,
            steps: data.steps,
            instr_stats,
            run_result: format_status(&data.run_result),
        })
    }
}

impl fmt::Display for BenchStats {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // The time taken for instructions to run, either the whole bench duration,
        // or the added instruction times to account for overhead
        let instr_duration = match &self.instr_stats {
            None => self.duration,
            Some(counts) => counts.iter().map(|i| i.total).sum::<Duration>(),
        };

        writeln!(f, "Outcome:        {}", self.run_result)?;
        writeln!(f, "Bench Duration: {:?}", self.duration)?;
        writeln!(f, "Steps:          {}", self.steps)?;
        let mut fmt = Formatter::new()
            .separator(',')
            .unwrap()
            .precision(numfmt::Precision::Decimals(2))
            .suffix(" instr / s")
            .unwrap();
        writeln!(
            f,
            "Speed:          {}",
            fmt.fmt2(self.steps as f64 / instr_duration.as_secs_f64())
        )?;

        let stats = match &self.instr_stats {
            None => "Not captured".into(),
            Some(counts) => {
                let intro = format!("Distinct instructions: {}", counts.len());

                let bench_overhead = self.duration - instr_duration;
                let per_instr_overhead = bench_overhead.div_f64(counts.len() as f64);
                let instr_duration_f = format!("Instr Duration:        {instr_duration:?}");
                let overhead_f =       format!("Overhead:              {bench_overhead:?} (total) / {per_instr_overhead:?} (per instr.)");
                let instr_data = counts
                    .iter()
                    .map(|stat| format!("{stat}\n---------------------\n"))
                    .fold("".to_string(), |a, b| a + &b);
                format!("{intro}\n{instr_duration_f}\n{overhead_f}\n{instr_data}")
            }
        };
        write!(f, " Instruction statistics:\n{}", stats)
    }
}
