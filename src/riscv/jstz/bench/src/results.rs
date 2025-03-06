// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::collections::HashSet;
use std::fmt;
use std::fs::read_to_string;
use std::path::Path;
use std::time::Duration;

use regex::Regex;
use serde::Deserialize;
use tezos_smart_rollup::utils::inbox::file::InboxFile;
use tezos_smart_rollup::utils::inbox::file::Message;

use crate::Result;

// Three sets of messages:
// 1. Deployment
// 2. Minting & Transfers
// 3. Balance Checks
// ... but all contained in one level
const EXPECTED_LEVELS: usize = 1;

pub fn handle_results(
    inbox: Box<Path>,
    all_logs: Vec<Box<Path>>,
    expected_transfers: usize,
) -> Result<()> {
    let inbox = InboxFile::load(&inbox)?;

    let all_metrics = all_logs
        .iter()
        .map(|logs| {
            let logs = read_to_string(logs)?
                .lines()
                .map(serde_json::from_str)
                .filter_map(|l| l.map(LogLine::classify).transpose())
                .collect::<std::result::Result<Vec<_>, _>>()?;

            let levels = logs_to_levels(logs)?;

            if inbox.0.len() != levels.len() || levels.len() != EXPECTED_LEVELS {
                return Err(format!(
                    "InboxFile contains {} levels, found {} in logs, expected {EXPECTED_LEVELS}",
                    inbox.0.len(),
                    levels.len()
                )
                .into());
            }

            let [results]: [_; EXPECTED_LEVELS] = levels.try_into().unwrap();

            check_deploy(&results)?;
            let metrics = check_transfer_metrics(&results, expected_transfers)?;
            check_balances(
                &results,
                &inbox.0[0][2 + expected_transfers..],
                expected_transfers,
            )?;

            Ok(metrics)
        })
        .collect::<Result<Vec<_>>>()?;

    if all_metrics.len() > 1 {
        let len = all_metrics.len();

        for (num, metrics) in all_metrics.iter().enumerate() {
            println!("Run {} / {len} => {metrics}", num + 1);
        }

        let agg_metrics = TransferMetrics::aggregate(&all_metrics);
        println!("\nAggregate => {agg_metrics}");
    } else if let Some(metrics) = all_metrics.first() {
        println!("{metrics}");
    }

    Ok(())
}

fn check_deploy(level: &Level) -> Result<()> {
    if level.deployments.len() != 1 {
        return Err("Expected FA2 contract deployment".into());
    }

    if level.executions.is_empty() {
        return Err("Expected FA2 token minting".into());
    }

    Ok(())
}

#[derive(Clone, Debug, Default)]
struct TransferMetrics {
    transfers: usize,
    duration: Duration,
    tps: f64,
}

impl TransferMetrics {
    fn aggregate(metrics: &[TransferMetrics]) -> TransferMetrics {
        let summed = metrics.iter().fold(Self::default(), |acc, m| Self {
            transfers: acc.transfers + m.transfers,
            duration: acc.duration + m.duration,
            tps: acc.tps + m.tps,
        });

        Self {
            tps: summed.tps / metrics.len() as f64,
            ..summed
        }
    }
}

impl fmt::Display for TransferMetrics {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} FA2 transfers took {:?} @ {:.3} TPS",
            self.transfers, self.duration, self.tps
        )
    }
}

fn check_transfer_metrics(level: &Level, expected_transfers: usize) -> Result<TransferMetrics> {
    if expected_transfers + 1 != level.executions.len() {
        return Err(format!(
            "Expected {expected_transfers} transfers, got {}",
            level.executions.len() - 1
        )
        .into());
    }

    let transfers = level.executions.len() - 1;
    // The first execution is the minting call. We collect the time elapsed at the _end_ of the
    // minting, all the way up to the _end_ of the last execution (transfer).
    let duration = level.executions[transfers].elapsed - level.executions[0].elapsed;
    let tps = (transfers as f64) / duration.as_secs_f64();

    Ok(TransferMetrics {
        transfers,
        duration,
        tps,
    })
}

// The generated transfers (for a number of accounts N), has a target final state:
// Every account should hold one of every token.
//
// This requires (N - 1) * num_tokens transfers.
//
// Therefore, if an account has `0` of a token, there's a transfer missing below this maximum
// number.
fn check_balances(level: &Level, messages: &[Message], num_transfers: usize) -> Result<()> {
    let re = Regex::new(r#"^.*"([\w0-9]+) has ([0-9]+) of token ([0-9]+)".*$"#).unwrap();

    let mut accounts = HashSet::new();
    let mut tokens = HashSet::new();
    let mut skipped_receives = 0;

    for m in level.balance_checks.iter().map(|l| &l.message) {
        for (_, [address, balance, token]) in re.captures_iter(m).map(|c| c.extract()) {
            accounts.insert(address);
            tokens.insert(token.parse::<usize>()?);

            let balance = balance.parse::<usize>()?;

            if balance == 0 {
                skipped_receives += 1;
            }
        }
    }

    // Checks
    if accounts.len() != tokens.len() {
        return Err(format!(
            "Expected {} accounts to equal {} tokens",
            accounts.len(),
            tokens.len()
        )
        .into());
    }

    if accounts.len() != messages.len() {
        return Err(format!(
            "Have {} accounts but only {} messages for checking balances",
            accounts.len(),
            messages.len()
        )
        .into());
    }

    let expected_transfers = (accounts.len() - 1) * tokens.len() - skipped_receives;

    if expected_transfers != num_transfers {
        return Err(format!(
            "Found {} transfer messages, vs {} transfers completed",
            num_transfers, expected_transfers
        )
        .into());
    }

    Ok(())
}

fn logs_to_levels(logs: Vec<LogType>) -> Result<Vec<Level>> {
    let mut levels = Vec::new();

    let mut level = Level::default();

    let mut balance_checks = Vec::new();
    for line in logs.into_iter() {
        match line {
            LogType::StartOfLevel(_) => {
                if level != Level::default() {
                    return Err(
                        format!("StartOfLevel message not at start of level {level:?}").into(),
                    );
                }
            }
            LogType::EndOfLevel(_) => {
                levels.push(level);
                level = Default::default();
            }
            LogType::Deploy(l) => level.deployments.push(l),
            LogType::Success(l) if balance_checks.is_empty() => level.executions.push(l),
            LogType::Success(_) => level.balance_checks.append(&mut balance_checks),
            LogType::SmartFunctionLog(l) => balance_checks.push(l),
        }
    }

    if level != Level::default() {
        return Err("Final level missing EndOfLevel message {last:?}".into());
    }

    Ok(levels)
}

#[derive(Deserialize, Debug, PartialEq)]
struct LogLine {
    elapsed: Duration,
    message: String,
}

impl LogLine {
    fn classify(self) -> Option<LogType> {
        let m = &self.message;

        if m.starts_with(SOL) {
            Some(LogType::StartOfLevel(self))
        } else if m.starts_with(EOL) {
            Some(LogType::EndOfLevel(self))
        } else if m.starts_with(DEPLOY) {
            Some(LogType::Deploy(self))
        } else if m.starts_with(SUCCESS) {
            Some(LogType::Success(self))
        } else if m.starts_with(LOG) {
            Some(LogType::SmartFunctionLog(self))
        } else {
            None
        }
    }
}

#[derive(Debug)]
enum LogType {
    StartOfLevel(#[allow(unused)] LogLine),
    Deploy(LogLine),
    Success(LogLine),
    EndOfLevel(#[allow(unused)] LogLine),
    SmartFunctionLog(LogLine),
}

const SOL: &str = "Message: Internal(StartOfLevel)";
const DEPLOY: &str = "[📜] Smart function deployed";
const SUCCESS: &str = "🚀 Smart function executed successfully";
const EOL: &str = "Internal message: end of level";
const LOG: &str = "[JSTZ:SMART_FUNCTION:LOG]";

#[derive(Default, Debug, PartialEq)]
struct Level {
    deployments: Vec<LogLine>,
    executions: Vec<LogLine>,
    balance_checks: Vec<LogLine>,
}
