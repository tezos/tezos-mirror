// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

//! Withdrawals

use crypto::hash::ContractKt1Hash;
use tezos_data_encoding::enc::BinWriter;
use tezos_protocol::contract::Contract;
use tezos_smart_rollup_core::MAX_OUTPUT_SIZE;
#[cfg(feature = "debug")]
use tezos_smart_rollup_debug::debug_msg;
use tezos_smart_rollup_encoding::entrypoint::Entrypoint;
use tezos_smart_rollup_encoding::michelson::ticket::StringTicket;
use tezos_smart_rollup_encoding::outbox::{OutboxMessage, OutboxMessageTransaction};
use tezos_smart_rollup_host::runtime::Runtime;

/// Withdrawal to be sent to L1.
#[derive(Debug, PartialEq, Eq)]
pub struct Withdrawal {
    /// Id of the ticket.
    pub ticket: StringTicket,
    /// Destination contract
    pub destination: ContractKt1Hash,
    /// Entrypoint of the destination contract.
    pub entrypoint: Entrypoint,
    /// Amount withdrawn of the ticket.
    pub withdrawn_amount: u64,
}

/// Handle a vector of withdrawals
///
/// The tickets have already been removed from the source account
/// but we need to remove the ticket from durable storage and an
/// withdrawal message goes into the rollup outbox.
pub fn process_withdrawals(host: &mut impl Runtime, withdrawals: Vec<Withdrawal>) {
    if withdrawals.is_empty() {
        return;
    }

    let mut transactions: Vec<OutboxMessageTransaction<StringTicket>> =
        Vec::with_capacity(withdrawals.len());

    for withdrawal in withdrawals.into_iter() {
        // TODO remove `withdrawn_amount` field altogether as it is no longer needed
        let Withdrawal {
            ticket,
            destination,
            entrypoint,
            withdrawn_amount: _ignore,
        } = withdrawal;

        transactions.push(OutboxMessageTransaction {
            parameters: ticket,
            entrypoint,
            destination: Contract::Originated(destination),
        });
    }

    let output = OutboxMessage::AtomicTransactionBatch(transactions.into());

    let mut encoded = Vec::with_capacity(MAX_OUTPUT_SIZE);

    if let Err(_err) = output.bin_write(&mut encoded) {
        #[cfg(feature = "debug")]
        debug_msg!(host, "Failed to encode outbox message: {}\n", _err);
    } else {
        // TODO: need to make sure withdrawals will never hit this limit
        // - part of the 'verify' step
        if let Err(err) = host.write_output(encoded.as_slice()) {
            panic!("Failed to write outbox message {err:?}\n");
        } else {
            #[cfg(feature = "debug")]
            debug_msg!(host, "Withdrawal executed: {output:?}\n");
        }
    }
}
