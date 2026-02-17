// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
//
// SPDX-License-Identifier: MIT

//! Processing external inbox messages - withdrawals & transactions.

use super::store::dac_payload_path;
use crate::inbox::v1::ParsedBatch;
use crate::inbox::ParsedExternalInboxMessage;
use crate::storage::AccountStorage;
use crate::transactions::withdrawal::Withdrawal;
use crypto::hash::HashTrait;
use crypto::hash::PublicKeyBls;
use crypto::CryptoError;
#[cfg(feature = "debug")]
use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_encoding::dac::certificate::{Certificate, CertificateError};
use tezos_smart_rollup_host::debug::HostDebug;
use tezos_smart_rollup_host::path::concat;
use tezos_smart_rollup_host::path::OwnedPath;
use tezos_smart_rollup_host::path::PathError;
use tezos_smart_rollup_host::path::RefPath;
use tezos_smart_rollup_host::reveal::HostReveal;
use tezos_smart_rollup_host::runtime::RuntimeError;
use tezos_smart_rollup_host::storage::StorageV1;
use thiserror::Error;

// TODO: replace with `dac_committee`, `_-` now allowed in paths by PVM,
//       but `host::path` hasn't been updated yet.
pub(crate) const DAC_COMMITTEE_MEMBER_PATH_PREFIX: RefPath =
    RefPath::assert_from(b"/kernel/dac.committee");

/// Reveal and store a DAC message assuming the DAC message is verified
pub fn reveal_and_store_dac_message<Host>(
    parsed_dac_message: Certificate,
    host: &mut Host,
    idx: u32,
) -> Result<(), ProcessExtMsgError>
where
    Host: StorageV1 + HostReveal,
{
    let dac_payload_path = dac_payload_path(idx).map_err(ProcessExtMsgError::Path)?;
    // preallocate with maximum expected size of DAC content
    parsed_dac_message.reveal_to_store(host, &dac_payload_path)?;
    Ok(())
}

/// Process a list of operations.
pub fn process_batch_message<Host>(
    host: &mut Host,
    account_storage: &mut AccountStorage,
    batch: ParsedBatch,
) -> Vec<Vec<Withdrawal>>
where
    Host: StorageV1 + HostDebug,
{
    let mut all_withdrawals: Vec<Vec<Withdrawal>> = Vec::new();

    for transaction in batch.operations.into_iter() {
        match transaction.execute(host, account_storage) {
            Ok(withdrawals) => {
                all_withdrawals.push(withdrawals);
            }
            Err(_err) => {
                #[cfg(feature = "debug")]
                debug_msg!(host, "Could not execute transaction: {}\n", _err);
            }
        };
    }

    all_withdrawals
}

pub(crate) fn get_dac_committee<Host: StorageV1>(
    host: &Host,
) -> Result<Vec<PublicKeyBls>, ProcessExtMsgError> {
    let num_keys = host
        .store_count_subkeys(&DAC_COMMITTEE_MEMBER_PATH_PREFIX)
        .map_err(ProcessExtMsgError::RuntimeGetDacCommittee)?
        .try_into()
        .expect("key count should be non-negative and hold in memory");
    let mut res = Vec::with_capacity(num_keys);
    for idx in 0..num_keys {
        let path = concat(
            &DAC_COMMITTEE_MEMBER_PATH_PREFIX,
            &OwnedPath::try_from(format!("/{idx}")).map_err(ProcessExtMsgError::Path)?,
        )
        .map_err(ProcessExtMsgError::Path)?;
        let mut dac_member = [0; 48];
        host.store_read_slice(&path, 0, &mut dac_member)
            .map_err(LoadDacCommitteeError::RuntimeError)?;
        let pk = PublicKeyBls::try_from_bytes(&dac_member).unwrap();
        res.push(pk);
    }
    Ok(res)
}

/// Parse external message, logging error if it occurs.
pub(crate) fn parse_external<'a, Host: HostDebug>(
    _host: &Host,
    message: &'a [u8],
) -> Option<ParsedExternalInboxMessage<'a>> {
    match ParsedExternalInboxMessage::parse(message) {
        Ok((remaining, external)) => {
            if !remaining.is_empty() {
                #[cfg(feature = "debug")]
                debug_msg!(
                    _host,
                    "External message had unused remaining bytes: {:?}\n",
                    remaining
                );
            }
            Some(external)
        }
        Err(_err) => {
            #[cfg(feature = "debug")]
            debug_msg!(_host, "Error parsing external message payload {}\n", _err);
            None
        }
    }
}

/// Errors when processing external messages
#[derive(Error, Debug)]
pub enum ProcessExtMsgError {
    /// Parse Error
    #[error("Error parsing external message")]
    ParseError,

    /// Errors when loading roll up id from storage
    #[error("Failed to load rollup id: {0:?}")]
    LoadRollupIdError(RuntimeError),

    /// Errors when loading dac committee from storage
    #[error("Failed to load dac committee: {0}")]
    LoadDacCommitteeError(#[from] LoadDacCommitteeError),

    /// Errors verifying/revealing dac certificates
    #[error(transparent)]
    RevealDacMessageError(#[from] CertificateError),

    /// External message was the wrong variant
    #[error("Unexpected external message variant")]
    UnexpectedExtMsgVariant,

    /// Runtime failed to fetch DAC committee keys
    #[error("Runtime error while fetching DAC committee keys: {0}")]
    RuntimeGetDacCommittee(RuntimeError),

    /// Store path parsing failed
    #[error("Path error: {0:?}")]
    Path(PathError),

    /// Runtime failed to store DAC payload
    #[error("Runtime error while fetching DAC payload: {0}")]
    RuntimeStoreDacPayload(RuntimeError),
}
/// LoadDacCommitteeError variants
#[derive(Error, Debug)]
pub enum LoadDacCommitteeError {
    /// RuntimeError from host
    #[error("{0:?}")]
    RuntimeError(RuntimeError),

    /// Bls errors from decode
    #[error(transparent)]
    CryptoError(#[from] CryptoError),
}
