// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>

//! Implementation of the `write_debug` function based on Rust login crate. Isolated in its own
//! module to more easily control if it should be displayed or not.

use log::{debug, error, info, warn};

use crate::bindings::{end_span, span_add_attrs, start_span};

const INFO_TAG: &'static str = "[Info] ";
const DEBUG_TAG: &'static str = "[Debug] ";
const ERROR_TAG: &'static str = "[Error] ";
const FATAL_TAG: &'static str = "[Fatal] ";
const OTEL_START_TAG: &'static str = "[OTel] [start] ";
const OTEL_ATTRS_TAG: &'static str = "[OTel] [attrs] ";
const OTAL_END_TAG: &'static str = "[OTel] [end] ";

fn trim_log<'a, 'b>(tag: &'a str, msg: &'b str) -> &'b str {
    msg[tag.len()..].trim()
}

fn parse_attrs(log: &str) -> Vec<(String, String)> {
    let trimmed = trim_log(OTEL_ATTRS_TAG, log);
    let mut attrs = Vec::new();

    let parts: Vec<&str> = trimmed
        .split('®')
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .collect();

    let mut iter = parts.chunks_exact(2);
    for chunk in iter.by_ref() {
        attrs.push((chunk[0].to_string(), chunk[1].to_string()));
    }

    attrs
}

pub fn write_debug(msg: &[u8]) {
    match std::str::from_utf8(msg.as_ref()) {
        Ok(log) if log.starts_with(INFO_TAG) => info!("{}", trim_log(INFO_TAG, log)),
        Ok(log) if log.starts_with(DEBUG_TAG) => debug!("{}", trim_log(DEBUG_TAG, log)),
        Ok(log) if log.starts_with(ERROR_TAG) => warn!("{}", trim_log(ERROR_TAG, log)),
        Ok(log) if log.starts_with(FATAL_TAG) => error!("{}", trim_log(FATAL_TAG, log)),
        Ok(log) if log.starts_with(OTEL_START_TAG) => {
            let scope_name = trim_log(OTEL_START_TAG, log);
            start_span(scope_name);
        }
        Ok(log) if log.starts_with(OTEL_ATTRS_TAG) => {
            let attrs = parse_attrs(log);
            span_add_attrs(attrs);
        }
        Ok(log) if log.starts_with(OTAL_END_TAG) => {
            end_span();
        }
        _ => (),
    }
}
