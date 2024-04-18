// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::{fmt::Display, time::Duration};

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
