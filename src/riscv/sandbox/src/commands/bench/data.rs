// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use core::fmt;
use std::collections::HashMap;
use std::fmt::Debug;
use std::fmt::Display;
use std::time::Duration;

use enum_tag::EnumTag;
use octez_riscv::parser::instruction::Instr;
use octez_riscv::stepper::StepperStatus;

pub(super) type InstructionData = HashMap<InstrType, Vec<Duration>>;

/// Underlying data for a benchmark run.
pub(super) struct BenchData {
    pub(super) duration: Duration,
    pub(super) steps: usize,
    pub(super) instr_count: Option<InstructionData>,
    pub(super) run_result: StepperStatus,
}

impl BenchData {
    pub fn from_simple(data: SimpleBenchData, run_result: StepperStatus) -> Self {
        BenchData {
            duration: data.duration,
            steps: data.steps,
            instr_count: None,
            run_result,
        }
    }

    /// [`FineBenchData`] contains only the instruction-level data.
    /// The `total_duration` length should be given to be sanity checked against
    /// the sum of all instruction durations.
    pub fn from_fine(data: FineBenchData, duration: Duration, run_result: StepperStatus) -> Self {
        let steps = data.instr_list.values().map(|d| d.len()).sum();

        BenchData {
            duration,
            steps,
            instr_count: Some(data.instr_list),
            run_result,
        }
    }
}

pub(super) struct SimpleBenchData {
    duration: Duration,
    steps: usize,
}

impl SimpleBenchData {
    pub fn new(duration: Duration, steps: usize) -> Self {
        SimpleBenchData { duration, steps }
    }
}

impl Display for SimpleBenchData {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let f_duration = format!("Bench Duration: {:?}", self.duration);
        let f_steps = format!("Steps:          {}", self.steps);
        write!(f, "Simple bench data:\n {}\n {}", f_duration, f_steps)
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone)]
pub enum InstrGetError {
    Parse,
}

type InstrTag = <Instr as EnumTag>::Tag;

/// Instruction classification type for grouping instruction for further stats.
#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone)]
pub enum InstrType {
    Instr(InstrTag),
    FetchErr(InstrGetError),
}

impl fmt::Display for InstrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            InstrType::FetchErr(err) => write!(f, "FetchErr: {err:?}"),
            InstrType::Instr(tag) => write!(f, "Instruction: {tag:?}"),
        }
    }
}

/// Holds the instruction-level data of a benchmark run.
pub(super) struct FineBenchData {
    instr_list: HashMap<InstrType, Vec<Duration>>,
}

impl FineBenchData {
    pub fn new() -> Self {
        FineBenchData {
            instr_list: HashMap::new(),
        }
    }

    pub fn add_instr(&mut self, instr: InstrType, duration: Duration) {
        self.instr_list.entry(instr).or_default().push(duration);
    }
}
