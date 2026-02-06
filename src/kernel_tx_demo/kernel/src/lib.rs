// SPDX-FileCopyrightText: 2022-2024 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2022-2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Contains core logic of the SCORU wasm kernel.
//!
//! The kernel is an implementation of a **TORU-style** *transactions* kernel.
#![deny(missing_docs)]
#![deny(rustdoc::broken_intra_doc_links)]
#![cfg_attr(feature = "debug", forbid(unsafe_code))]

extern crate alloc;
extern crate tezos_crypto_rs as crypto;

#[cfg(feature = "dal")]
pub mod dal;
pub mod inbox;
pub mod storage;
pub mod transactions;

#[cfg(feature = "testing")]
pub(crate) mod fake_hash;

use crypto::hash::{ContractTz1Hash, HashTrait, HashType, SmartRollupHash};
use inbox::DepositFromInternalPayloadError;
use tezos_smart_rollup::entrypoint;
use tezos_smart_rollup_core::MAX_INPUT_MESSAGE_SIZE;
use tezos_smart_rollup_encoding::inbox::ExternalMessageFrame;
use tezos_smart_rollup_encoding::michelson::ticket::{StringTicket, Ticket};
use tezos_smart_rollup_encoding::michelson::{MichelsonPair, MichelsonString};
use tezos_smart_rollup_host::path::PathError;
use thiserror::Error;

#[cfg(feature = "debug")]
use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_encoding::inbox::{InboxMessage, InternalInboxMessage, Transfer};
use tezos_smart_rollup_host::runtime::{Runtime, RuntimeError, ValueType};
use transactions::external_inbox::ProcessExtMsgError;
use transactions::process::execute_one_operation;
use transactions::store::{CACHED_MESSAGES_STORE_PREFIX, DAL_PAYLOAD_PATH};

use crate::inbox::v1::ParsedBatch;
use crate::inbox::InboxDeposit;
use crate::storage::{
    deposit_ticket_to_storage, init_account_storage, AccountStorage, AccountStorageError,
};
use crate::transactions::external_inbox::process_batch_message;
use crate::transactions::store::cached_message_path;
use crate::transactions::withdrawal::process_withdrawals;

impl TryFrom<MichelsonPair<MichelsonString, StringTicket>> for InboxDeposit {
    type Error = DepositFromInternalPayloadError;

    fn try_from(value: MichelsonPair<MichelsonString, StringTicket>) -> Result<Self, Self::Error> {
        Ok(Self {
            destination: ContractTz1Hash::from_b58check(value.0 .0.as_str())?,
            ticket: value.1,
        })
    }
}

/// Entrypoint of the *transactions* kernel.
#[cfg_attr(feature = "tx-kernel", entrypoint::main)]
pub fn transactions_run<Host>(host: &mut Host)
where
    Host: Runtime,
{
    #[cfg(feature = "debug")]
    host.write_debug("======\nTX: Entry\n======\n");

    let mut account_storage = match init_account_storage() {
        Ok(v) => v,
        Err(_err) => {
            #[cfg(feature = "debug")]
            debug_msg!(host, "Could not get account storage: {:?}\n", _err);
            return;
        }
    };

    let metadata = host.reveal_metadata();

    #[cfg(feature = "debug")]
    debug_msg!(host, "TX: Metadata {metadata:?}\n");

    let rollup_address = metadata.address();
    let mut counter = 0;

    if let Ok(Some(ValueType::Subtree)) = host.store_has(&CACHED_MESSAGES_STORE_PREFIX) {
        #[cfg(feature = "debug")]
        debug_msg!(host, "Found cached messages, processing\n");
        if let Err(_err) = execute_one_operation(host, &mut account_storage) {
            #[cfg(feature = "debug")]
            debug_msg!(host, "Error enumerating cached header payload {}", _err);
        }
        return;
    } else if let Ok(data) = host.store_read_all(&DAL_PAYLOAD_PATH) {
        // TODO: https://gitlab.com/tezos/tezos/-/issues/6393
        // Enable processing DAL payload incrementally with reboots.
        #[cfg(feature = "debug")]
        debug_msg!(host, "Found cached DAL payload, processing\n");
        match ParsedBatch::parse(&data) {
            Ok((_, batch)) => {
                #[cfg(feature = "debug")]
                debug_msg!(host, "Process parsed batch {:?}\n", batch);
                for withdrawals in process_batch_message(host, &mut account_storage, batch) {
                    process_withdrawals(host, withdrawals)
                }
            }
            Err(_e) => {
                #[cfg(feature = "debug")]
                debug_msg!(host, "Failed to parse DAL payload. Error: {:?}\n", _e);
            }
        }
        match host.store_delete(&DAL_PAYLOAD_PATH) {
            Ok(()) => {}
            Err(_e) => {
                #[cfg(feature = "debug")]
                debug_msg!(
                    host,
                    "Failed to delete processed DAL payload. Error: {:?}\n",
                    _e
                );
            }
        }
        return;
    }

    #[cfg(feature = "debug")]
    host.write_debug("Filtering inbox messages...\n");
    let mut reboot = false;

    while let Ok(Some(message)) = host.read_input() {
        reboot = true;
        #[cfg(feature = "debug")]
        debug_msg!(
            host,
            "Processing MessageData {} at level {}\n",
            message.id,
            message.level
        );

        if let Err(_err) = filter_inbox_message(
            host,
            &mut account_storage,
            message.level,
            message.as_ref(),
            &mut counter,
            &rollup_address,
        ) {
            #[cfg(feature = "debug")]
            debug_msg!(host, "Error processing header payload {}\n", _err);
        }
    }

    #[cfg(feature = "debug")]
    host.write_debug("Finished filtering\n");

    if reboot {
        #[cfg(feature = "debug")]
        host.write_debug("Reboot for cached mesages\n");

        if let Err(_e) = host.mark_for_reboot() {
            #[cfg(feature = "debug")]
            debug_msg!(host, "Could not mark host for reboot: {}\n", _e);
        }
    }
}

#[derive(Error, Debug)]
enum TransactionError<'a> {
    #[error("Unable to parse header inbox message {0:?}")]
    MalformedInboxMessage(nom::Err<tezos_data_encoding::nom::NomError<'a>>),
    #[error("Invalid internal inbox message, expected deposit: {0}")]
    InvalidInternalInbox(#[from] DepositFromInternalPayloadError),
    #[error("Error storing ticket on rollup")]
    AccountStorageError(#[from] AccountStorageError),
    #[error("Error storing DAL related parameters")]
    DalStorageError(#[from] storage::dal::StorageError),
    #[error("Failed to process external message: {0}")]
    ProcessExternalMsgError(#[from] ProcessExtMsgError),
    #[error("Failed to construct path: {0:?}")]
    Path(PathError),
    #[error("Error caching messages in store: {0}")]
    CacheMessage(RuntimeError),
}

fn filter_inbox_message<'a, Host: Runtime>(
    host: &mut Host,
    account_storage: &mut AccountStorage,
    _inbox_level: u32,
    inbox_message: &'a [u8],
    counter: &mut u32,
    rollup_address: &SmartRollupHash,
) -> Result<(), TransactionError<'a>> {
    let (remaining, message) = InboxMessage::<
        MichelsonPair<MichelsonString, Ticket<MichelsonString>>,
    >::parse(inbox_message)
    .map_err(TransactionError::MalformedInboxMessage)?;

    match message {
        InboxMessage::Internal(_msg @ InternalInboxMessage::StartOfLevel) => {
            #[cfg(feature = "debug")]
            debug_msg!(host, "InboxMetadata: {}\n", _msg);
            #[cfg(feature = "dal")]
            {
                // TODO: https://gitlab.com/tezos/tezos/-/issues/6270
                // Make DAL parameters available to the kernel.
                let attestation_lag = 4;
                let slot_size = 32768;
                let page_size = 128;
                let num_pages = slot_size / page_size;
                // Before importing a slot, we wait 2 blocks for finality + 1 block for DAL node processing
                let import_extra_delay = 3;
                let published_level = (_inbox_level - attestation_lag - import_extra_delay) as i32;
                // TODO: https://gitlab.com/tezos/tezos/-/issues/6400
                // Make it possible to track multiple slot indexes.
                let slot_index = storage::dal::get_or_set_slot_index(host, 0 as u8)?;
                dal::store_dal_slot(
                    host,
                    published_level,
                    num_pages,
                    page_size,
                    slot_index,
                    DAL_PAYLOAD_PATH.into(),
                );
            }
            Ok(())
        }
        InboxMessage::Internal(InternalInboxMessage::Transfer(Transfer {
            payload,
            destination,
            ..
        })) => {
            if rollup_address != destination.hash() {
                #[cfg(feature = "debug")]
                debug_msg!(
                    host,
                    "Skipping message: Internal message targets another rollup. Expected: {}. Found: {}",
                    rollup_address,
                    destination.hash()
                );
            } else {
                let InboxDeposit {
                    destination,
                    ticket,
                } = payload
                    .try_into()
                    .map_err(TransactionError::InvalidInternalInbox)?;
                deposit_ticket_to_storage(host, account_storage, &destination, &ticket)?;
                debug_assert!(remaining.is_empty());
            }
            Ok(())
        }

        #[cfg(feature = "proto-alpha")]
        InboxMessage::Internal(
            _msg @ (InternalInboxMessage::EndOfLevel
            | InternalInboxMessage::InfoPerLevel(..)
            | InternalInboxMessage::ProtocolMigration(..)
            | InternalInboxMessage::DalAttestedSlots(..)),
        ) => {
            #[cfg(feature = "debug")]
            debug_msg!(host, "InboxMetadata: {}\n", _msg);
            Ok(())
        }

        #[cfg(not(feature = "proto-alpha"))]
        InboxMessage::Internal(
            _msg @ (InternalInboxMessage::EndOfLevel
            | InternalInboxMessage::InfoPerLevel(..)
            | InternalInboxMessage::ProtocolMigration(..)),
        ) => {
            #[cfg(feature = "debug")]
            debug_msg!(host, "InboxMetadata: {}\n", _msg);
            Ok(())
        }

        InboxMessage::External(message) => {
            match ExternalMessageFrame::parse(message)
                .map_err(|e| TransactionError::MalformedInboxMessage(nom::Err::Failure(e)))?
            {
                ExternalMessageFrame::Targetted { address, contents } => {
                    debug_assert!(remaining.is_empty());
                    let metadata = host.reveal_metadata();
                    let rollup_address = metadata.address();
                    if &rollup_address != address.hash() {
                        #[cfg(feature = "debug")]
                        debug_msg!(
                            host,
                            "Skipping message: External message targets another rollup. Expected: {}. Found: {}",
                            rollup_address,
                            address.hash()
                        );
                    } else {
                        // persist message to store first
                        let path = cached_message_path(*counter).map_err(TransactionError::Path)?;
                        host.store_write_all(&path, contents)
                            .map_err(TransactionError::CacheMessage)?;
                        *counter = counter.checked_add(1).expect("too many messages");
                    }
                    Ok(())
                }
            }
        }
    }
}

#[derive(Debug, Error)]
enum CachedTransactionError {
    #[error("while reading progress from store: {0}")]
    ReadProgress(RuntimeError),
    #[error("while writing progress into store: {0}")]
    WriteProgress(RuntimeError),
    #[error("general store operation failed: {0}")]
    Store(RuntimeError),
    #[error("general path operation failed: {0:?}")]
    Path(PathError),
    #[error("while processing external message: {0}")]
    ProcessExternalMessage(ProcessExtMsgError),
    #[error("while issuing reboot: {0}")]
    Reboot(RuntimeError),
    #[error("cached message cannot be parsed")]
    MessageUnparsable,
}

const MAX_ENVELOPE_CONTENT_SIZE: usize =
    MAX_INPUT_MESSAGE_SIZE - HashType::SmartRollupHash.size() - 2;

#[cfg(test)]
mod test {
    use tezos_protocol::contract::Contract;
    use tezos_smart_rollup_encoding::{
        michelson::ticket::StringTicket,
        michelson::{MichelsonPair, MichelsonString},
    };

    use super::*;

    use crate::inbox::external::testing::gen_ed25519_keys;
    use crypto::hash::{ContractKt1Hash, ContractTz1Hash, HashTrait};
    use crypto::PublicKeyWithHash;
    use tezos_smart_rollup_core::MAX_FILE_CHUNK_SIZE;
    use tezos_smart_rollup_encoding::smart_rollup::SmartRollupAddress;
    use tezos_smart_rollup_host::path::OwnedPath;
    use tezos_smart_rollup_host::storage::StorageV1;
    use tezos_smart_rollup_mock::MockHost;
    use tezos_smart_rollup_mock::TransferMetadata;

    #[test]
    fn deposit_ticket() {
        // Arrange
        let mut mock_runtime = MockHost::default();

        let destination =
            ContractTz1Hash::from_b58check("tz1XdRrrqrMfsFKA8iuw53xHzug9ipr6MuHq").unwrap();

        let ticket_creator =
            Contract::from_b58check("KT1JW6PwhfaEJu6U3ENsxUeja48AdtqSoekd").unwrap();
        let ticket_content = "hello, ticket!";
        let ticket_quantity = 5_u64;

        let ticket = StringTicket::new(
            ticket_creator,
            MichelsonString(ticket_content.to_string()),
            ticket_quantity,
        )
        .expect("Invalid ticket");

        let transfer = MichelsonPair(MichelsonString(destination.to_base58_check()), ticket);

        let transfer_metadata = TransferMetadata::new(
            "KT1VsSxSXUkgw6zkBGgUuDXXuJs9ToPqkrCg",
            "tz3SvEa4tSowHC5iQ8Aw6DVKAAGqBPdyK1MH",
        );

        mock_runtime.add_transfer(transfer, &transfer_metadata);

        // Act
        mock_runtime.run_level(transactions_run);

        // Assert
        let amount = ticket_amount(&mock_runtime, &destination, 0);

        assert_eq!(amount, ticket_quantity);
    }

    #[test]
    fn test_filter_internal_transfers() {
        // setup rollup state
        let mut mock_runtime = MockHost::default();

        // setup message
        let receiver = gen_ed25519_keys().0.pk_hash();
        let originator = Contract::Originated(
            ContractKt1Hash::from_b58check("KT1ThEdxfUcWUwqsdergy3QnbCWGHSUHeHJq").unwrap(),
        );
        let contents = "Hello, Ticket!".to_string();
        let ticket = StringTicket::new(originator, contents, 500).unwrap();

        let mut transfer_metadata = TransferMetadata::new(
            "KT1PWx2mnDueood7fEmfbBDKx1D9BAnnXitn",
            "tz1Ke2h7sDdakHJQh8WX4Z372du1KChsksyU",
        );

        // Transfers - first will succeed, second targets a different rollup
        let transfer1 = MichelsonPair(
            MichelsonString(receiver.to_b58check()),
            ticket.testing_clone(),
        );

        let transfer2 = MichelsonPair(MichelsonString(receiver.to_b58check()), ticket);

        // Act && Assert
        mock_runtime.add_transfer(transfer1, &transfer_metadata);

        mock_runtime.run_level(transactions_run);

        let amount = ticket_amount(&mock_runtime, &receiver, 0);
        assert_eq!(500, amount);

        transfer_metadata.override_destination(
            SmartRollupAddress::from_b58check("sr1VEw81u5kYf8nJ3cwqgVEaVRiMZMixueFJ").unwrap(),
        );
        mock_runtime.add_transfer(transfer2, &transfer_metadata);

        let amount = ticket_amount(&mock_runtime, &receiver, 0);
        assert_eq!(500, amount);
    }

    fn ticket_amount(mock_host: &MockHost, account: &ContractTz1Hash, ticket_id: u64) -> u64 {
        let ticket_path =
            OwnedPath::try_from(format!("/accounts/{account}/{ticket_id}")).expect("Invalid path");

        mock_host
            .store_read(&ticket_path, 0, MAX_FILE_CHUNK_SIZE)
            .expect("Ticket path missing")
            .try_into()
            .map(u64::from_le_bytes)
            .expect("Invalid u64 in store")
    }
}
