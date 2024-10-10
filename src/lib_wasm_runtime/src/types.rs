// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>

//! Types for values creating in the OCaml side, and passed to Rust code and back.

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

#[derive(Clone)]
pub struct OCamlString(Value);

impl OCamlString {
    pub fn as_str(&self) -> &str {
        unsafe { Value::string_val(&self.0) }
    }

    pub fn as_bytes(&self) -> &[u8] {
        unsafe { Value::bytes_val(&self.0) }
    }
}

unsafe impl ToValue for OCamlString {
    fn to_value(&self, gc: &Runtime) -> Value {
        self.0.to_value(gc)
    }
}

unsafe impl FromValue for OCamlString {
    fn from_value(value: Value) -> Self {
        OCamlString(value)
    }
}

pub struct OCamlBytes(Value);

impl OCamlBytes {
    pub fn as_bytes(&self) -> &[u8] {
        unsafe { Value::bytes_val(&self.0) }
    }
}

unsafe impl ToValue for OCamlBytes {
    fn to_value(&self, gc: &Runtime) -> Value {
        self.0.to_value(gc)
    }
}

unsafe impl FromValue for OCamlBytes {
    fn from_value(value: Value) -> Self {
        OCamlBytes(value)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Clone, Ord)]
pub struct ContextHash([u8; 32]);

impl ContextHash {
    pub fn as_bytes(&self) -> &[u8; 32] {
        &self.0
    }
}

unsafe impl ToValue for ContextHash {
    fn to_value(&self, gc: &Runtime) -> Value {
        self.0.to_value(gc)
    }
}

unsafe impl FromValue for ContextHash {
    fn from_value(value: Value) -> Self {
        ContextHash(FromValue::from_value(value))
    }
}

#[derive(PartialEq, Eq, PartialOrd, Clone, Ord, Copy)]
pub struct SmartRollupAddress([u8; 20]);

impl SmartRollupAddress {
    pub fn as_bytes(&self) -> &[u8; 20] {
        &self.0
    }
}

unsafe impl ToValue for SmartRollupAddress {
    fn to_value(&self, gc: &Runtime) -> Value {
        self.0.to_value(gc)
    }
}

unsafe impl FromValue for SmartRollupAddress {
    fn from_value(value: Value) -> Self {
        SmartRollupAddress(FromValue::from_value(value))
    }
}
