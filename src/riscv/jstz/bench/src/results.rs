// SPDX-FileCopyrightText: 2024 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use crate::inbox::{InboxFile, Message};
use crate::Result;
use serde::Deserialize;
use std::fs::read_to_string;
use std::path::Path;
use std::time::Duration;

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

    let [level_1, level_2, _level_3] = levels.try_into().unwrap();

    check_deploy(level_1)?;
    transfer_metrics(level_2, &inbox.0[1])?;

    Ok(())
}

fn check_deploy(level: Level) -> Result<()> {
    if level.deployments.len() != 1 {
        return Err("Level 1: expected FA2 contract deployment".into());
    }

    if level.executions.len() != 1 {
        return Err("Level 1: expected FA2 token minting".into());
    }
    Ok(())
}

fn transfer_metrics(level: Level, messages: &[Message]) -> Result<()> {
    if !level.deployments.is_empty() {
        return Err(format!(
            "Level 2: expected no contract deployments, got {}",
            level.deployments.len()
        )
        .into());
    }

    if level.executions.len() != messages.len() {
        return Err(format!(
            "Level 2: expected {} transfers, got {}",
            messages.len(),
            level.executions.len()
        )
        .into());
    }

    let transfers = messages.len();
    let tps = (transfers as f64) / level.duration.as_secs_f64();
    println!(
        "{transfers} FA2 transfers took {:?} @ {tps:.3} TPS",
        level.duration
    );

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
}

const SOL: &str = "Message: Internal(StartOfLevel)";
const DEPLOY: &str = "[ð\u{9f}\u{93}\u{9c}] Smart function deployed";
const SUCCESS: &str = "ð\u{9f}\u{9a}\u{80} Smart function executed successfully";
const EOL: &str = "Internal message: end of level";

#[derive(Default, Debug, PartialEq)]
struct Level {
    duration: Duration,
    deployments: Vec<LogLine>,
    executions: Vec<LogLine>,
}
