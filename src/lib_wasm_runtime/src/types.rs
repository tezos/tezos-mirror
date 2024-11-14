// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>

use ocaml::{FromValue, Runtime, ToValue, Value};

#[derive(Clone)]
pub struct EvmTree(Value);

unsafe impl ToValue for EvmTree {
    fn to_value(&self, _gc: &Runtime) -> Value {
        self.0.clone()
    }
}

unsafe impl FromValue for EvmTree {
    fn from_value(value: Value) -> Self {
        EvmTree(value)
    }
}
