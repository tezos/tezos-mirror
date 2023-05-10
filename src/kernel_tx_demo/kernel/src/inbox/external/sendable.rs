// SPDX-FileCopyrightText: 2022 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Constructing external inbox messages for sending to the kernel.

use tezos_data_encoding::{enc::BinWriter, encoding::HasEncoding};
use tezos_smart_rollup_encoding::dac::certificate::V0Certificate;

use super::v1;

/// Upgradeable representation of external inbox messages.
#[derive(Debug, HasEncoding, BinWriter)]
pub enum ExternalInboxMessage {
    #[encoding(tag = 0)]
    /// DAC certificate
    Dac(V0Certificate),
    #[encoding(tag = 1)]
    /// Version 1 of operation batching
    OpList(v1::sendable::Batch),
}
