// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::time::Duration;

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SequencerConfig {
    pub min_block_time: Duration,
    pub channel_capacity: usize,
}

impl Default for SequencerConfig {
    fn default() -> Self {
        Self {
            min_block_time: Duration::from_millis(500),
            channel_capacity: 10,
        }
    }
}
