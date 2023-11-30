// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

#![doc = include_str!("../README.md")]
#![cfg_attr(not(feature = "std"), no_std)]
#![deny(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]

#[cfg(feature = "alloc")]
extern crate alloc;

#[cfg(all(target_arch = "riscv64", target_os = "hermit", feature = "proto-alpha"))]
extern crate hermit;

pub use tezos_smart_rollup_entrypoint::kernel_entry;

pub mod host {
    //! A low level, but safe, wrapper over the Smart Rollup host functions.
    //!
    //! These host functions are exposed at their lowest level by the [core_unsafe]
    //! module. Since they are C-style APIs, however, they are unsafe to call. Therefore
    //! we expose a wrapper over these that may be used more easily, while still
    //! being performant.
    //!
    //! [core_unsafe]: crate::core_unsafe

    pub use tezos_smart_rollup_host::runtime::{Runtime, RuntimeError};
    #[doc(inline)]
    pub use tezos_smart_rollup_host::Error as HostError;
}

/// Lowest level definitions of host functions & associated constants.
///
/// In general, you should prefer using the [`host`] module.
pub use tezos_smart_rollup_core as core_unsafe;

pub mod prelude {
    //! Tezos smart rollup prelude - used by every kernel.
    //!
    //! The prelude brings into scope the [`Runtime`] trait, used
    //! for interacting with the WASM-VM, and the [`debug_msg`] macro.
    //!
    //! # Example
    //! The prelude can be used to write a simple **Hello, world!** kernel:
    //!
    //! ```
    //! use tezos_smart_rollup::prelude::*;
    //! use tezos_smart_rollup::kernel_entry;
    //!
    //! fn kernel_run(host: &mut impl Runtime) {
    //!   debug_msg!(host, "Hello, world!");
    //! }
    //!
    //! kernel_entry!(kernel_run);
    //!
    //! # use tezos_smart_rollup::testing::prelude::MockHost;
    //! # let mut host = MockHost::default();
    //! # host.run_level(kernel_run);
    //! ```
    #[cfg(feature = "debug_alloc")]
    pub use tezos_smart_rollup_debug::debug_msg;
    #[cfg(not(feature = "debug_alloc"))]
    pub use tezos_smart_rollup_debug::debug_str;
    pub use tezos_smart_rollup_host::runtime::Runtime;
}

#[cfg(feature = "alloc")]
pub mod types {
    //! Types used/returned elsewhere in the SDK.

    pub use tezos_smart_rollup_encoding::{
        contract::Contract, entrypoint::Entrypoint, entrypoint::EntrypointError,
        public_key::PublicKey, public_key_hash::PublicKeyHash,
        smart_rollup::SmartRollupAddress, timestamp::Timestamp,
    };
    pub use tezos_smart_rollup_host::input::Message;
    pub use tezos_smart_rollup_host::metadata::RollupMetadata;
}

#[doc(inline)]
pub use tezos_smart_rollup_encoding::dac;
#[cfg(feature = "data-encoding")]
#[doc(inline)]
pub use tezos_smart_rollup_encoding::inbox;
#[cfg(feature = "data-encoding")]
#[doc(inline)]
pub use tezos_smart_rollup_encoding::michelson;
#[cfg(feature = "data-encoding")]
#[doc(inline)]
pub use tezos_smart_rollup_encoding::outbox;

pub mod storage {
    //! Durable Storage allows state to be persisted between
    //! multiple calls to the kernel.

    #[doc(inline)]
    pub use tezos_smart_rollup_host::path;

    #[cfg(feature = "storage")]
    pub use tezos_smart_rollup_storage as accounts;
}

#[cfg(all(feature = "testing", not(target_arch = "wasm32")))]
pub mod testing {
    //! Utilities for writing unit tests for kernels.
    //!
    //! In particular, [`MockHost`] simulates the host functions
    //! exposed by the Smart Rollups, to allow full end-to-end
    //! testing of a kernel's behaviour.
    //!
    //! [`MockHost`]: prelude::MockHost

    #[doc(inline)]
    pub use tezos_smart_rollup_mock as prelude;
}
