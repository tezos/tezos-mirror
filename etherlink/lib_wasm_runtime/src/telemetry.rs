// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>

use crate::{
    bindings::{self, BindingsError},
    types::OpenTelemetryScope,
};

#[derive(Clone)]
pub struct Scope {
    root: OpenTelemetryScope,
    scopes: Vec<OpenTelemetryScope>,
}

impl Scope {
    pub fn new(root: OpenTelemetryScope) -> Self {
        Self {
            root,
            scopes: vec![],
        }
    }

    pub fn current(&self) -> &OpenTelemetryScope {
        match self.scopes.last() {
            None => &self.root,
            Some(scope) => scope,
        }
    }

    pub fn start(&mut self, span_name: &str) -> Result<(), BindingsError> {
        let oscope = bindings::open_span(self.current(), span_name)?;
        self.scopes.push(oscope);
        Ok(())
    }

    pub fn close(&mut self) -> Result<(), BindingsError> {
        if let Some(oscope) = self.scopes.pop() {
            bindings::close_span(&oscope)?;
        }
        Ok(())
    }

    pub fn close_all(&mut self) -> Result<(), BindingsError> {
        while self.scopes.len() > 0 {
            self.close()?;
        }
        Ok(())
    }
}
