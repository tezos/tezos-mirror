// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::error::CliError;
use std::{net::SocketAddr, str::FromStr, time::Duration};

const DEFAULT_RPC_ADDRESS: ([u8; 4], u16) = ([127, 0, 0, 1], 5303);

/// A simple wrapper around the Duration datatype. This wrapper implements
/// the trait [FromStr], which allows for parsing durations as Cli arguments.
#[derive(Clone, Debug)]
pub struct PreblockTime(Duration);

impl FromStr for PreblockTime {
    type Err = CliError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let milliseconds: u64 = s.parse().map_err(CliError::InvalidDuration)?;
        Ok(PreblockTime(Duration::from_millis(milliseconds)))
    }
}

impl From<PreblockTime> for Duration {
    fn from(b: PreblockTime) -> Self {
        b.0
    }
}

/// Command line arguments for the `bin_dsn_node` when running in sequencer mode.`
#[derive(Clone, Debug, clap::Args)]
pub struct Args {
    #[arg(short, long, default_value_t=DEFAULT_RPC_ADDRESS.into())]
    ///Rpc Address
    pub rpc_address: SocketAddr,
    // Minimum time, in milliseconds, to produce a preblock.
    pub preblock_time: PreblockTime,
}
