// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>

use crate::{
    bindings::{self, BindingsError},
    types::OpenTelemetryScope,
};

pub struct Span(OpenTelemetryScope);

impl Span {
    pub fn start(s: OpenTelemetryScope, top_span_name: &str) -> Result<Self, BindingsError> {
        let s = bindings::open_span(&s, top_span_name)?;

        Ok(Span(s))
    }

    pub fn new(&self, span_name: &str) -> Result<Self, BindingsError> {
        let s = bindings::open_span(&self.0, span_name)?;

        Ok(Span(s))
    }
}

impl Drop for Span {
    fn drop(&mut self) {
        bindings::close_span(&self.0).unwrap();
    }
}
