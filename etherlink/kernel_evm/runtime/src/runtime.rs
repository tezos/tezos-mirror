// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2023 Functori <contact@functori.com>
// SPDX-FileCopyrightText: 2023 Trilitech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

// The kernel runtime requires both the standard Runtime and the
// new Extended one.

use crate::internal_runtime::{ExtendedRuntime, InternalRuntime};
use tezos_smart_rollup_core::PREIMAGE_HASH_SIZE;
use tezos_smart_rollup_host::{
    dal_parameters::RollupDalParameters,
    input::Message,
    metadata::RollupMetadata,
    path::Path,
    runtime::{Runtime as SdkRuntime, RuntimeError, ValueType},
};

pub trait Runtime: SdkRuntime + InternalRuntime + ExtendedRuntime {}

// If a type implements the Runtime, InternalRuntime and ExtendedRuntime traits,
// it also implements the kernel Runtime.
impl<T: SdkRuntime + InternalRuntime + ExtendedRuntime> Runtime for T {}
