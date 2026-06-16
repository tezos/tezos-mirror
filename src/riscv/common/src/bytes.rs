// SPDX-FileCopyrightText: 2023-2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

/// Wrapper to convert a Rust allocated byte-array to an OCaml allocated Bytes type
pub struct BytesWrapper<T: AsRef<[u8]> = Box<[u8]>>(T);

unsafe impl<T: AsRef<[u8]>> ocaml::ToValue for BytesWrapper<T> {
    fn to_value(&self, _rt: &ocaml::Runtime) -> ocaml::Value {
        unsafe { ocaml::Value::bytes(self.0.as_ref()) }
    }
}

unsafe impl ocaml::FromValue for BytesWrapper<Box<[u8]>> {
    fn from_value(value: ocaml::Value) -> Self {
        // SAFETY: The ToValue implementation for this type uses the OCaml bytes type.
        // and ocaml-rs will only call this function on an OCaml value coming from BytesWrapper.
        let bytes: &[u8] = unsafe { value.bytes_val() };
        BytesWrapper(bytes.into())
    }
}

impl From<BytesWrapper<Box<[u8]>>> for Box<[u8]> {
    fn from(value: BytesWrapper) -> Self {
        value.0
    }
}

impl<T: AsRef<[u8]>> From<T> for BytesWrapper<T> {
    fn from(value: T) -> Self {
        Self(value)
    }
}
