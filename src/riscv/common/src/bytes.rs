// SPDX-FileCopyrightText: 2023-2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

/// Wrapper to convert a Rust allocated byte-array to an OCaml allocated Bytes type
pub struct BytesWrapper(Box<[u8]>);

unsafe impl ocaml::ToValue for BytesWrapper {
    fn to_value(&self, _rt: &ocaml::Runtime) -> ocaml::Value {
        unsafe { ocaml::Value::bytes(&self.0) }
    }
}

unsafe impl ocaml::FromValue for BytesWrapper {
    fn from_value(value: ocaml::Value) -> Self {
        // SAFETY: The ToValue implementation for this type uses the OCaml bytes type.
        // and ocaml-rs will only call this function on an OCaml value coming from BytesWrapper.
        let bytes: &[u8] = unsafe { value.bytes_val() };
        BytesWrapper(bytes.into())
    }
}

impl From<BytesWrapper> for Box<[u8]> {
    fn from(value: BytesWrapper) -> Self {
        value.0
    }
}

impl From<Box<[u8]>> for BytesWrapper {
    fn from(value: Box<[u8]>) -> Self {
        Self(value)
    }
}
