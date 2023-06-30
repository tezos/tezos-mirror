// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

//! Processing external inbox messages.

use crate::error::ApplicationError;
use debug::debug_msg;
use evm_execution::account_storage::EthereumAccountStorage;
use evm_execution::precompiles::PrecompileBTreeMap;
use host::runtime::Runtime;
use primitive_types::{H256, U256};
use rlp::{Decodable, Rlp};
use sha3::{Digest, Keccak256};
use tezos_ethereum::block::BlockConstants;
use tezos_ethereum::signatures::EthereumTransactionCommon;

use tezos_smart_rollup_encoding::inbox::{
    InboxMessage, InfoPerLevel, InternalInboxMessage, Transfer,
};
use tezos_smart_rollup_encoding::michelson::ticket::StringTicket;
use tezos_smart_rollup_encoding::michelson::{MichelsonPair, MichelsonString};

/// Process external message
///
/// An external message is a batch of transactions. Each transaction
/// may be either a series of signed operations or a single Ethereum
/// transaction.

pub fn process_inbox_message<'a, Host: Runtime>(
    host: &mut Host,
    evm_account_storage: &mut EthereumAccountStorage,
    level: u32,
    message: &'a [u8],
) -> Result<(), ApplicationError> {
    debug_msg!(host, "Processing an inbox message at level {}", level);

    let precompiles = evm_execution::precompiles::precompile_set();

    let (_remaining, message) =
        InboxMessage::<MichelsonPair<MichelsonString, StringTicket>>::parse(message)
            .map_err(|e| ApplicationError::MalformedInboxMessage(format!("{e:?}")))?;

    match message {
        InboxMessage::Internal(InternalInboxMessage::Transfer(Transfer { .. })) => Ok(()),
        InboxMessage::Internal(InternalInboxMessage::InfoPerLevel(info)) => {
            debug_msg!(host, "InfoPerLevel: {}", info);
            process_info(host, level, info)
        }

        InboxMessage::Internal(
            msg @ (InternalInboxMessage::StartOfLevel | InternalInboxMessage::EndOfLevel),
        ) => {
            debug_msg!(host, "InboxMetadata: {}", msg);
            Ok(())
        }
        InboxMessage::External(message) => {
            debug_msg!(host, "Got an external message");
            let current_block = BlockConstants::first_block(U256::zero());
            // decoding
            let decoder = Rlp::new(message);
            let ethereum_transaction = EthereumTransactionCommon::decode(&decoder)
                .map_err(ApplicationError::MalformedRlpTransaction)?;
            process_ethereum_transaction(
                host,
                evm_account_storage,
                &current_block,
                &precompiles,
                ethereum_transaction,
            )
        }
    }
}

fn process_info<Host: Runtime>(
    host: &mut Host,
    level: u32,
    info: InfoPerLevel,
) -> Result<(), ApplicationError> {
    let metadata = Runtime::reveal_metadata(host);
    // Origination level considered to be block level with number 0
    // Next one is with number 1
    let block_level: u64 = u64::from(level - metadata.origination_level);
    let block_number = U256::from(block_level);
    evm_execution::storage::blocks::add_new_block(
        host,
        block_number,
        H256::from_slice(&Keccak256::digest(info.predecessor.0.as_slice())),
        info.predecessor_timestamp.as_u64().into(),
    )
    .map_err(ApplicationError::EvmStorage)
}
/// Process one Ethereum transaction
fn process_ethereum_transaction<Host>(
    host: &mut Host,
    evm_account_storage: &mut EthereumAccountStorage,
    block: &BlockConstants,
    precompiles: &PrecompileBTreeMap<Host>,
    e: EthereumTransactionCommon,
) -> Result<(), ApplicationError>
where
    Host: Runtime,
{
    let outcome = evm_execution::run_transaction(
        host,
        block,
        evm_account_storage,
        precompiles,
        evm::Config::london(),
        e.to,
        e.caller()?,
        e.data,
        Some(e.gas_limit),
        Some(e.value),
    )?;

    debug_msg!(host, "Transaction executed, outcome: {:?}", outcome);

    Ok(())
}
