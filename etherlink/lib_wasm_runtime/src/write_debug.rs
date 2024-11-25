// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>

//! Implementation of the `write_debug` function based on Rust login crate. Isolated in its own
//! module to more easily control if it should be displayed or not.

use log::{debug, error, info, warn};

const INFO_TAG: &'static str = "[Info] ";
const DEBUG_TAG: &'static str = "[Debug] ";
const ERROR_TAG: &'static str = "[Error] ";
const FATAL_TAG: &'static str = "[Fatal] ";

fn trim_log<'a, 'b>(tag: &'a str, msg: &'b str) -> &'b str {
    msg[tag.len()..].trim()
}

pub fn write_debug(msg: &[u8]) {
    match std::str::from_utf8(msg.as_ref()) {
        Ok(log) if log.starts_with(INFO_TAG) => info!("{}", trim_log(INFO_TAG, log)),
        Ok(log) if log.starts_with(DEBUG_TAG) => debug!("{}", trim_log(DEBUG_TAG, log)),
        Ok(log) if log.starts_with(ERROR_TAG) => warn!("{}", trim_log(ERROR_TAG, log)),
        Ok(log) if log.starts_with(FATAL_TAG) => error!("{}", trim_log(FATAL_TAG, log)),
        _ => (),
    }
}
