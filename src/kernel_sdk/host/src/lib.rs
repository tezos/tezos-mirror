// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2022-2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

#![doc = include_str!("../README.md")]
#![cfg_attr(not(any(feature = "std")), no_std)]
#![deny(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]

#[cfg(feature = "alloc")]
extern crate alloc;

pub mod dal_parameters;
pub mod debug;
pub mod input;
pub mod metadata;
pub mod path;
pub mod runtime;

/// The size of a DAL parameters in bytes: 4 * size(i64) = 32 bytes.
pub use crate::dal_parameters::DAL_PARAMETERS_SIZE;
/// The size of a metadata in bytes: 20 (rollup address) + 4 (origination level).
pub use crate::metadata::METADATA_SIZE;
use path::RefPath;

/// Boot path for kernels
pub const KERNEL_BOOT_PATH: RefPath = RefPath::assert_from(b"/kernel/boot.wasm");

/// Defines the errors possibly returned by an host functions.
#[repr(i32)]
#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum Error {
    /// The store key submitted as an argument of a host function exceeds the
    /// authorized limit.
    StoreKeyTooLarge = tezos_smart_rollup_core::STORE_KEY_TOO_LARGE,
    /// The store key submitted as an argument of a host function cannot be
    /// parsed.
    StoreInvalidKey = tezos_smart_rollup_core::STORE_INVALID_KEY,
    /// The contents (if any) of the store under the key submitted as an
    /// argument of a host function is not a value.
    StoreNotAValue = tezos_smart_rollup_core::STORE_NOT_A_VALUE,
    /// An access in a value of the durable storage has failed, supposedly out
    /// of bounds of a value.
    StoreInvalidAccess = tezos_smart_rollup_core::STORE_INVALID_ACCESS,
    /// Writing a value has exceeded 2^31 bytes.
    StoreValueSizeExceeded = tezos_smart_rollup_core::STORE_VALUE_SIZE_EXCEEDED,
    /// An address is out of bound of the memory.
    MemoryInvalidAccess = tezos_smart_rollup_core::MEMORY_INVALID_ACCESS,
    /// The input or output submitted as an argument of a host function exceeds
    /// the authorized limit.
    InputOutputTooLarge = tezos_smart_rollup_core::INPUT_OUTPUT_TOO_LARGE,
    /// Generic error code for unexpected errors.
    GenericInvalidAccess = tezos_smart_rollup_core::GENERIC_INVALID_ACCESS,
    /// A value cannot be modified if it is readonly.
    StoreReadonlyValue = tezos_smart_rollup_core::STORE_READONLY_VALUE,
    /// The key was not found in storage
    StoreNotANode = tezos_smart_rollup_core::STORE_NOT_A_NODE,
    /// The outbox is full
    FullOutbox = tezos_smart_rollup_core::FULL_OUTBOX,
}

impl core::fmt::Display for Error {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::StoreKeyTooLarge => write!(f, "StoreKeyTooLarge"),
            Self::StoreInvalidKey => write!(f, "StoreInvalidKey"),
            Self::StoreNotAValue => write!(f, "StoreNotAValue"),
            Self::StoreInvalidAccess => write!(f, "StoreInvalidAccess"),
            Self::StoreValueSizeExceeded => write!(f, "StoreValueSizeExceeded"),
            Self::MemoryInvalidAccess => write!(f, "MemoryInvalidAccess"),
            Self::InputOutputTooLarge => write!(f, "InputOutputTooLarge"),
            Self::GenericInvalidAccess => write!(f, "GenericInvalidAccess"),
            Self::StoreReadonlyValue => write!(f, "StoreReadonlyValue"),
            Self::StoreNotANode => write!(f, "StoreNotANode"),
            Self::FullOutbox => write!(f, "FullOutbox"),
        }
    }
}

impl From<i32> for Error {
    fn from(code: i32) -> Self {
        match code {
            tezos_smart_rollup_core::STORE_KEY_TOO_LARGE => Self::StoreKeyTooLarge,
            tezos_smart_rollup_core::STORE_INVALID_KEY => Self::StoreInvalidKey,
            tezos_smart_rollup_core::STORE_NOT_A_VALUE => Self::StoreNotAValue,
            tezos_smart_rollup_core::STORE_VALUE_SIZE_EXCEEDED => {
                Self::StoreValueSizeExceeded
            }
            tezos_smart_rollup_core::STORE_INVALID_ACCESS => Self::StoreInvalidAccess,
            tezos_smart_rollup_core::MEMORY_INVALID_ACCESS => Self::MemoryInvalidAccess,
            tezos_smart_rollup_core::INPUT_OUTPUT_TOO_LARGE => Self::InputOutputTooLarge,
            tezos_smart_rollup_core::GENERIC_INVALID_ACCESS => Self::GenericInvalidAccess,
            tezos_smart_rollup_core::STORE_READONLY_VALUE => Self::StoreReadonlyValue,
            tezos_smart_rollup_core::STORE_NOT_A_NODE => Self::StoreNotANode,
            tezos_smart_rollup_core::FULL_OUTBOX => Self::FullOutbox,
            _ => Error::GenericInvalidAccess,
        }
    }
}

impl From<i64> for Error {
    fn from(code: i64) -> Self {
        match i32::try_from(code) {
            Ok(error) => error.into(),
            Err(_) => Error::GenericInvalidAccess,
        }
    }
}

impl Error {
    /// Extracts the error from the returned value as a result
    pub fn wrap(code: i32) -> Result<usize, Self> {
        if code >= 0 {
            // Casting to usize is safe, since we eluded the negative values
            Ok(code as usize)
        } else {
            Err(code.into())
        }
    }

    /// Returns the code for the given error.
    pub fn code(self) -> i32 {
        self as i32
    }
}
