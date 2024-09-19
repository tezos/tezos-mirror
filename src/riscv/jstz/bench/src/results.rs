// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::Result;
use regex::Regex;
use serde::Deserialize;
use std::collections::HashSet;
use std::fs::read_to_string;
use std::path::Path;
use std::time::Duration;
use tezos_smart_rollup::utils::inbox::file::{InboxFile, Message};

// Three levels:
// 1. Deployment & Minting
// 2. Transfers
// 3. Balance Checks
const EXPECTED_LEVELS: usize = 3;

pub fn handle_results(inbox: Box<Path>, logs: Box<Path>) -> Result<()> {
    let inbox = InboxFile::load(&inbox)?;

    let logs = read_to_string(&logs)?
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

    let [level_1, level_2, level_3] = levels.try_into().unwrap();

    check_deploy(level_1)?;
    let num_transfers = transfer_metrics(level_2, &inbox.0[1])?;
    balance_sanity_check(level_3, &inbox.0[2], num_transfers)?;

    Ok(())
}

fn check_deploy(level: Level) -> Result<()> {
    if level.deployments.len() != 1 {
        return Err("Level 1: expected FA2 contract deployment".into());
    }

    if level.executions.len() != 1 {
        return Err("Level 1: expected FA2 token minting".into());
    }

    if !level.logs.is_empty() {
        return Err(format!("Level 1: unexpected smart function logs {:?}", level.logs).into());
    }

    Ok(())
}

fn minimal_sanity(level_index: usize, level: &Level, messages: &[Message]) -> Result<()> {
    if !level.deployments.is_empty() {
        return Err(format!(
            "Level {level_index}: expected no contract deployments, got {}",
            level.deployments.len()
        )
        .into());
    }

    if level.executions.len() != messages.len() {
        return Err(format!(
            "Level {level_index}: expected {} transfers, got {}",
            messages.len(),
            level.executions.len()
        )
        .into());
    }

    Ok(())
}

fn transfer_metrics(level: Level, messages: &[Message]) -> Result<usize> {
    minimal_sanity(2, &level, messages)?;

    if !level.logs.is_empty() {
        return Err(format!("Level 1: unexpected smart function logs {:?}", level.logs).into());
    }

    let transfers = messages.len();
    let tps = (transfers as f64) / level.duration.as_secs_f64();
    println!(
        "{transfers} FA2 transfers took {:?} @ {tps:.3} TPS",
        level.duration
    );

    Ok(transfers)
}

// The generated transfers (for a number of accounts N), has a target final state:
// Every account should hold one of every token.
//
// This requires (N - 1) * num_tokens transfers.
//
// Therefore, if an account has `0` of a token, there's a transfer missing below this maximum
// number.
fn balance_sanity_check(level: Level, messages: &[Message], num_transfers: usize) -> Result<()> {
    minimal_sanity(3, &level, messages)?;

    let re = Regex::new(r#"^.*"([\w0-9]+) has ([0-9]+) of token ([0-9]+)".*$"#).unwrap();

    let mut accounts = HashSet::new();
    let mut tokens = HashSet::new();
    let mut skipped_receives = 0;

    for m in level.logs.iter().map(|l| &l.message) {
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

    println!("Balances are consistent");
    Ok(())
}

fn logs_to_levels(logs: Vec<LogType>) -> Result<Vec<Level>> {
    let mut levels = Vec::new();

    let mut level = Level::default();

    for line in logs.into_iter() {
        match line {
            LogType::StartOfLevel(l) => {
                if level == Level::default() {
                    level.duration = l.elapsed
                } else {
                    return Err(
                        format!("StartOfLevel message not at start of level {level:?}").into(),
                    );
                }
            }
            LogType::EndOfLevel(l) => {
                level.duration = l.elapsed - level.duration;
                levels.push(level);
                level = Default::default();
            }
            LogType::Deploy(l) => level.deployments.push(l),
            LogType::Success(l) => level.executions.push(l),
            LogType::SmartFunctionLog(l) => level.logs.push(l),
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

enum LogType {
    StartOfLevel(LogLine),
    Deploy(LogLine),
    Success(LogLine),
    EndOfLevel(LogLine),
    SmartFunctionLog(LogLine),
}

const SOL: &str = "Message: Internal(StartOfLevel)";
const DEPLOY: &str = "[📜] Smart function deployed";
const SUCCESS: &str = "🚀 Smart function executed successfully";
const EOL: &str = "Internal message: end of level";
const LOG: &str = "[JSTZ:SMART_FUNCTION:LOG]";

#[derive(Default, Debug, PartialEq)]
struct Level {
    duration: Duration,
    deployments: Vec<LogLine>,
    executions: Vec<LogLine>,
    logs: Vec<LogLine>,
}
