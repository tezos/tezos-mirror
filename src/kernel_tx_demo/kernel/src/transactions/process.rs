// SPDX-FileCopyrightText: 2023 Marigold <contact@marigold.dev>
// SPDX-FileCopyrightText: 2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Transaction processor: external message processor

use std::slice::from_mut;

use crate::inbox::external::dac_iterator::IteratorState;
use crate::inbox::ParsedExternalInboxMessage;
use crate::storage::dal;
use crate::storage::AccountStorage;
use crate::transactions::external_inbox;
use crate::transactions::external_inbox::process_batch_message;
use crate::transactions::withdrawal::process_withdrawals;
use crate::CachedTransactionError;
use crate::MAX_ENVELOPE_CONTENT_SIZE;
#[cfg(feature = "debug")]
use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_host::path::Path;
use tezos_smart_rollup_host::runtime::Runtime;
use thiserror::Error;

use super::external_inbox::reveal_and_store_dac_message;
use super::store::cached_message_path;
use super::store::cached_message_stage_path;
use super::store::KERNEL_PRIVATE_STATE;
use super::store::PROGRESS_KEY;
use super::store::TRANSACTIONS_PER_KERNEL_RUN;
use super::utils::read_large_store_chunk;

pub(crate) fn execute_one_operation<Host: Runtime>(
    host: &mut Host,
    account_storage: &mut AccountStorage,
) -> Result<(), CachedTransactionError> {
    let progress = if host
        .store_has(&PROGRESS_KEY)
        .map_err(CachedTransactionError::Store)?
        .is_some()
    {
        let mut buf = [0u8; 4];
        host.store_read_slice(&PROGRESS_KEY, 0, &mut buf)
            .map_err(CachedTransactionError::ReadProgress)?;
        u32::from_le_bytes(buf)
    } else {
        0
    };
    make_decision(host, account_storage, progress)
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub(crate) enum MessageStage {
    Start = 0,
    Verified = 1,
    Revealed = 2,
}

/// Error for invalid message stage marker
#[derive(Error, Debug)]
#[error("invalid message stage")]
pub struct InvalidMessageStage;

impl TryFrom<u8> for MessageStage {
    type Error = InvalidMessageStage;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        Ok(match value {
            0 => Self::Start,
            1 => Self::Verified,
            2 => Self::Revealed,
            _ => return Err(InvalidMessageStage),
        })
    }
}

#[derive(Debug)]
pub(crate) enum Decision {
    NextMessage,
    MessageNextStage(MessageStage),
}

fn cleanup(host: &mut impl Runtime) -> Result<(), CachedTransactionError> {
    host.store_delete(&KERNEL_PRIVATE_STATE)
        .map_err(CachedTransactionError::Store)
}

fn message_next_stage(
    host: &mut impl Runtime,
    stage_path: &impl Path,
    stage: MessageStage,
) -> Result<(), CachedTransactionError> {
    host.mark_for_reboot()
        .map_err(CachedTransactionError::Reboot)?;

    host.store_write(stage_path, &[stage as u8], 0)
        .map_err(CachedTransactionError::Store)
}

fn next_message_or_next_level(
    host: &mut impl Runtime,
    idx: u32,
) -> Result<(), CachedTransactionError> {
    let next = idx + 1;
    let has_next_message = host
        .store_has(&cached_message_path(next).map_err(CachedTransactionError::Path)?)
        .map_err(CachedTransactionError::Store)?
        .is_some();
    let has_reboot_left = host.reboot_left().map_err(CachedTransactionError::Reboot)? > 0;
    if has_next_message && has_reboot_left {
        host.store_write(&PROGRESS_KEY, &next.to_le_bytes(), 0)
            .map_err(CachedTransactionError::WriteProgress)?;
        host.mark_for_reboot()
            .map_err(CachedTransactionError::Reboot)
    } else {
        // Skip to next level by not rebooting
        cleanup(host)
    }
}

pub(crate) fn make_decision<Host: Runtime>(
    host: &mut Host,
    account_storage: &mut AccountStorage,
    idx: u32,
) -> Result<(), CachedTransactionError> {
    let stage_path = cached_message_stage_path(idx).map_err(CachedTransactionError::Path)?;

    let stage: MessageStage = {
        let mut payload = 0;
        if let Ok(1) = host.store_read_slice(&stage_path, 0, from_mut(&mut payload)) {
            if let Ok(stage) = payload.try_into() {
                stage
            } else {
                return next_message_or_next_level(host, idx);
            }
        } else {
            MessageStage::Start
        }
    };
    #[cfg(feature = "debug")]
    debug_msg!(host, "message@{} stage {:?}\n", idx, stage);
    let decision = match stage {
        MessageStage::Start => process_at_start(host, account_storage, idx)?,
        MessageStage::Verified => process_verified(host, idx)?,
        MessageStage::Revealed => process_revealed(host, account_storage, idx)?,
    };
    #[cfg(feature = "debug")]
    debug_msg!(host, "message@{} decision {:?}\n", idx, decision);
    match decision {
        Decision::NextMessage => next_message_or_next_level(host, idx),
        Decision::MessageNextStage(stage) => message_next_stage(host, &stage_path, stage),
    }
}

fn get_cached_message<'a, Host: Runtime>(
    host: &mut Host,
    idx: u32,
    payload: &'a mut [u8; MAX_ENVELOPE_CONTENT_SIZE],
) -> Result<ParsedExternalInboxMessage<'a>, CachedTransactionError> {
    let message = read_large_store_chunk(
        host,
        &cached_message_path(idx).map_err(CachedTransactionError::Path)?,
        payload,
    )
    .map_err(CachedTransactionError::Store)?;
    external_inbox::parse_external(host, message).ok_or(CachedTransactionError::MessageUnparsable)
}

fn process_at_start<Host: Runtime>(
    host: &mut Host,
    account_storage: &mut AccountStorage,
    idx: u32,
) -> Result<Decision, CachedTransactionError> {
    let mut payload = [0; MAX_ENVELOPE_CONTENT_SIZE];
    let message = match get_cached_message(host, idx, &mut payload) {
        Ok(message) => message,
        Err(_e) => {
            #[cfg(feature = "debug")]
            debug_msg!(host, "cached message: {}\n", _e);
            return Ok(Decision::NextMessage);
        }
    };
    match message {
        ParsedExternalInboxMessage::OpList(batch) => {
            for withdrawals in process_batch_message(host, account_storage, batch) {
                process_withdrawals(host, withdrawals)
            }
            Ok(Decision::NextMessage)
        }
        ParsedExternalInboxMessage::DAC(parsed_dac_message) => {
            let dac_committee = external_inbox::get_dac_committee(host)
                .map_err(CachedTransactionError::ProcessExternalMessage)?;
            #[cfg(feature = "debug")]
            tezos_smart_rollup_debug::debug_msg!(
                host,
                "Verifying dac certificate {parsed_dac_message:?}. Have committee: {dac_committee:?}"
            );
            match parsed_dac_message.verify(&dac_committee, dac_committee.len() as u8) {
                Ok(_) => Ok(Decision::MessageNextStage(MessageStage::Verified)),
                Err(_e) => {
                    #[cfg(feature = "debug")]
                    debug_msg!(host, "failed to verify signature {} for cert {parsed_dac_message:?} and committee {dac_committee:?}\n", _e);
                    Ok(Decision::NextMessage)
                }
            }
        }
        ParsedExternalInboxMessage::ChangeDalSlot(slot_index) => {
            match dal::set_slot_index(host, slot_index) {
                Ok(_) => Ok(Decision::NextMessage),
                Err(dal::StorageError::PathError(e)) => Err(CachedTransactionError::Path(e)),
                Err(dal::StorageError::RuntimeError(e)) => Err(CachedTransactionError::Store(e)),
            }
        }
    }
}

fn process_verified<Host: Runtime>(
    host: &mut Host,
    idx: u32,
) -> Result<Decision, CachedTransactionError> {
    let mut payload = [0; MAX_ENVELOPE_CONTENT_SIZE];
    let message = match get_cached_message(host, idx, &mut payload) {
        Ok(message) => message,
        Err(_e) => {
            #[cfg(feature = "debug")]
            debug_msg!(host, "cached message: {}\n", _e);
            return Ok(Decision::NextMessage);
        }
    };
    match message {
        ParsedExternalInboxMessage::OpList(_) => {
            unreachable!("op_list processed by previous step");
        }
        ParsedExternalInboxMessage::DAC(parsed_dac_message) => {
            match reveal_and_store_dac_message(parsed_dac_message, host, idx) {
                Ok(()) => Ok(Decision::MessageNextStage(MessageStage::Revealed)),
                Err(_e) => {
                    #[cfg(feature = "debug")]
                    debug_msg!(host, "cached message: {}\n", _e);
                    Ok(Decision::NextMessage)
                }
            }
        }
        ParsedExternalInboxMessage::ChangeDalSlot(_) => {
            unreachable!("ChangeDalSlot processed by previous step");
        }
    }
}

fn process_revealed(
    host: &mut impl Runtime,
    account_storage: &mut AccountStorage,
    idx: u32,
) -> Result<Decision, CachedTransactionError> {
    let mut tx_per_kernel_run_buffer = [0; core::mem::size_of::<i32>()];
    let read_tx_per_kernel_run_result = host.store_read_slice(
        &TRANSACTIONS_PER_KERNEL_RUN,
        0,
        &mut tx_per_kernel_run_buffer,
    );
    let max_messages = match read_tx_per_kernel_run_result {
        Ok(_) => i32::from_le_bytes(tx_per_kernel_run_buffer),
        Err(_e) => {
            #[cfg(feature = "debug")]
            debug_msg!(host, "Could not read transactions per kernel run value: {}\nSetting a default value of 150", _e);
            150
        }
    };
    let mut dac_iterator = match IteratorState::load(host, idx) {
        Ok(iterator) => iterator,
        Err(_e) => {
            #[cfg(feature = "debug")]
            debug_msg!(host, "iterator state error: {:?}\n", _e);
            return Ok(Decision::NextMessage);
        }
    };
    for _ in 0..max_messages {
        match dac_iterator.next(host) {
            Ok(Some(transaction)) => {
                match transaction.execute(host, account_storage) {
                    Ok(ws) => {
                        process_withdrawals(host, ws);
                    }
                    Err(_e) => {
                        #[cfg(feature = "debug")]
                        debug_msg!(host, "Process transactions error: {:?}\n", _e);
                    }
                };
            }
            Ok(None) => {
                #[cfg(feature = "debug")]
                debug_msg!(host, "Finished parsing DAC payload\n");
                return Ok(Decision::NextMessage);
            }
            Err(_e) => {
                #[cfg(feature = "debug")]
                debug_msg!(host, "iterator state error: {:?}\n", _e);
                return Ok(Decision::NextMessage);
            }
        };
    }
    match dac_iterator.persist(host) {
        Ok(()) => {
            #[cfg(feature = "debug")]
            debug_msg!(host, "Batch of messages executed. Iterator state has been persisted. Kernel will reboot before processing the rest of the DAC payload\n");
            Ok(Decision::MessageNextStage(MessageStage::Revealed))
        }
        Err(_e) => {
            #[cfg(feature = "debug")]
            debug_msg!(host, "Failed to persist the iterator state: {:?}\n", _e);
            Ok(Decision::NextMessage)
        }
    }
}
