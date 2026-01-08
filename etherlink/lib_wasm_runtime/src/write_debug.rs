// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>

//! Implementation of the `write_debug` function based on Rust login crate. Isolated in its own
//! module to more easily control if it should be displayed or not.

use log::{debug, error, info, warn};

use crate::{
    bindings::{end_span, span_add_attrs, start_span},
    types::OTelAttrValue,
};

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

fn raw_value_to_otel_attribute(type_tag: &str, val_str: &str) -> OTelAttrValue {
    match type_tag {
        "bool" => match val_str.parse::<bool>() {
            Ok(b) => OTelAttrValue::Bool(b),
            Err(_) => OTelAttrValue::String(val_str.to_string()),
        },
        "int" => match val_str.parse::<i32>() {
            Ok(i) => OTelAttrValue::Int(i),
            Err(_) => OTelAttrValue::String(val_str.to_string()),
        },
        "float" => match val_str.parse::<f64>() {
            Ok(f) => OTelAttrValue::Float(f),
            Err(_) => OTelAttrValue::String(val_str.to_string()),
        },
        "string" | _ => OTelAttrValue::String(val_str.to_string()),
    }
}

fn parse_attrs(log: &str) -> Vec<(String, OTelAttrValue)> {
    let trimmed = trim_log(OTEL_ATTRS_TAG, log);
    let mut attrs = Vec::new();

    let parts: Vec<&str> = trimmed
        .split('®')
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .collect();

    let mut iter = parts.chunks_exact(2);
    for chunk in iter.by_ref() {
        let key = chunk[0].to_string();
        let raw_value = chunk[1];

        let mut split = raw_value.splitn(2, ':');
        let type_tag = split.next().unwrap_or("string").to_lowercase();
        let val_str = split.next().unwrap_or("");

        let value = raw_value_to_otel_attribute(type_tag.as_str(), val_str);

        attrs.push((key, value));
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
