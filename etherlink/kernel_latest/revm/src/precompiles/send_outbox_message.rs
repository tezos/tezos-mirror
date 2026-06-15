// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025-2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use alloy_sol_types::{sol, SolInterface, SolValue};
use num_bigint::{BigInt, Sign};
use revm::{
    context::{Block, ContextTr, Transaction},
    interpreter::{CallInputs, Gas, InstructionResult, InterpreterResult},
    primitives::{Address, Bytes, FixedBytes, U256},
};
use tezos_crypto_rs::base58::FromBase58CheckError;
use tezos_data_encoding::{
    enc::{BinError, BinWriter},
    nom::{error::DecodeError, NomReader},
    types::Zarith,
};
use tezos_protocol::contract::Contract;
use tezos_smart_rollup_core::MAX_OUTPUT_SIZE;
use tezos_smart_rollup_encoding::michelson::{
    ticket::TicketError, MichelsonBytes, MichelsonContract, MichelsonNat,
    MichelsonOption, MichelsonPair, MichelsonTimestamp,
};
use tezos_smart_rollup_encoding::outbox::OutboxMessage;
use tezos_smart_rollup_encoding::{
    entrypoint::Entrypoint, michelson::ticket::FA2_1Ticket,
    outbox::OutboxMessageTransaction,
};

use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::Registry;

use crate::{
    database::EtherlinkVMDB,
    helpers::storage::u256_to_bigint,
    journal::Journal,
    precompiles::{
        constants::{
            FA_BRIDGE_SOL_ADDR, SEND_OUTBOX_MESSAGE_BASE_COST,
            SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS, XTZ_BRIDGE_SOL_ADDR,
        },
        guard::{charge, guard},
    },
};
use evm_types::{
    CustomPrecompileError, DatabasePrecompileStateChanges, IntoWithRemainder,
    PrecompileStateError,
};

sol! {
    contract SendOutboxMessage {
        function push_withdrawal_to_outbox(
            string target,
            uint256 amount
        ) external;

        function push_fast_withdrawal_to_outbox(
            string target,
            string fast_withdrawal_contract,
            bytes  payload,
            uint256 amount,
            uint256 withdrawal_id
        ) external;

        function push_fa_withdrawal_to_outbox(
            bytes   routing_info,
            uint256 amount,
            bytes22 ticketer,
            bytes   content,
        ) external;

        function push_fast_fa_withdrawal_to_outbox(
            bytes   routing_info,
            uint256 amount,
            bytes22 ticketer,
            bytes   content,
            string  fast_withdrawal_contract_address,
            bytes   payload,
            uint256 withdrawal_id,
        ) external;

        struct RoutingInfo {
            bytes22 target;
            bytes22 proxy;
        }
    }
}

pub use michelson_types::{FastWithdrawalInterface, RouterInterface, Withdrawal};

#[derive(Debug, thiserror::Error)]
pub(crate) enum SendOutboxRevertReason {
    #[error("Custom decode error: {0}")]
    CustomDecode(String),

    #[error("Solidity input data error: {0}")]
    InputData(#[from] alloy_sol_types::Error),

    #[error("Contract decode error (from base58): {0}")]
    Base58ContractDecode(#[from] FromBase58CheckError),

    #[error("Contract decode error (from binary): {0}")]
    GenericContractDecode(String),

    #[error("Contract binary writing error: {0}")]
    ContractBinWriter(#[from] BinError),

    #[error("Ticket creation error: {0}")]
    CreateTicket(#[from] TicketError),

    #[error("Fixed byte array conversion error: {0}")]
    IntoFixedInvalidSize(usize),

    #[error("Database access error: {0}")]
    DatabaseAccess(#[from] PrecompileStateError),

    #[error("Outbox message too large: {size} bytes exceeds the {max} bytes limit")]
    OutboxMessageTooLarge { size: usize, max: usize },
}

impl IntoWithRemainder for SendOutboxRevertReason {
    fn into_with_remainder(self, gas: revm::interpreter::Gas) -> CustomPrecompileError {
        match self {
            SendOutboxRevertReason::DatabaseAccess(e) => e.into_with_remainder(gas),
            other => CustomPrecompileError::Revert(other.to_string(), gas),
        }
    }
}

impl<'a> From<nom::Err<DecodeError<&'a [u8]>>> for SendOutboxRevertReason {
    fn from(e: nom::Err<DecodeError<&'a [u8]>>) -> Self {
        SendOutboxRevertReason::GenericContractDecode(e.to_string())
    }
}

impl From<Vec<u8>> for SendOutboxRevertReason {
    fn from(e: Vec<u8>) -> Self {
        SendOutboxRevertReason::IntoFixedInvalidSize(e.len())
    }
}

fn build_fast_parameters(
    ticket: FA2_1Ticket,
    target: Contract,
    payload: Vec<u8>,
    timestamp: U256,
    withdrawal_id: MichelsonNat,
    caller: Address,
) -> FastWithdrawalInterface {
    let bytes_payload = MichelsonBytes(payload);
    let caller = MichelsonBytes(caller.to_vec());
    let timestamp: MichelsonTimestamp =
        MichelsonTimestamp(Zarith(u256_to_bigint(timestamp)));
    let target = MichelsonContract(target.clone());

    MichelsonPair(
        withdrawal_id,
        MichelsonPair(
            ticket,
            MichelsonPair(
                timestamp,
                MichelsonPair(target, MichelsonPair(bytes_payload, caller)),
            ),
        ),
    )
}

enum MessageInput {
    Standard {
        target: Contract,
    },
    Fast {
        target: Contract,
        payload: Vec<u8>,
        withdrawal_id: MichelsonNat,
        caller: Address,
        timestamp: U256,
    },
}

fn prepare_message(
    ticket: FA2_1Ticket,
    parameters: MessageInput,
    entrypoint: Entrypoint,
    destination: Contract,
) -> Withdrawal {
    match parameters {
        MessageInput::Standard { target } => {
            let parameters = MichelsonPair(MichelsonContract(target), ticket);
            let message = OutboxMessageTransaction {
                parameters,
                entrypoint,
                destination,
            };
            Withdrawal::Standard(OutboxMessage::AtomicTransactionBatch(
                vec![message].into(),
            ))
        }
        MessageInput::Fast {
            target,
            payload,
            timestamp,
            withdrawal_id,
            caller,
        } => {
            let parameters = build_fast_parameters(
                ticket,
                target,
                payload,
                timestamp,
                withdrawal_id,
                caller,
            );
            let message = OutboxMessageTransaction {
                parameters,
                entrypoint,
                destination,
            };
            Withdrawal::Fast(OutboxMessage::AtomicTransactionBatch(vec![message].into()))
        }
    }
}

/// Ensure a withdrawal message fits in the outbox before it is queued.
///
/// The serialized outbox message is bounded by [`MAX_OUTPUT_SIZE`]. A
/// caller-controlled field (e.g. the fast-withdrawal `payload`, or the FA
/// `routing_info`/`content`) can push the message past that limit. If we let
/// such a message reach the outbox queue, serialization fails there with
/// `InputOutputTooLarge` *after* the EVM transaction has already succeeded --
/// and for a forced (delayed-inbox) transaction that error aborts block
/// production and wedges the sequencer.
///
/// Instead, replicate the same serialization here (matching `queue_message`)
/// and reject oversized messages eagerly. The precompile reverts, the
/// surrounding `require` in the predeployed contract reverts the whole
/// transaction, and no unserializable message is ever produced.
pub(crate) fn check_outbox_message_size(
    withdrawal: &Withdrawal,
) -> Result<(), SendOutboxRevertReason> {
    let mut buffer = Vec::with_capacity(MAX_OUTPUT_SIZE);
    match withdrawal {
        Withdrawal::Standard(message) => message.bin_write(&mut buffer),
        Withdrawal::Fast(message) => message.bin_write(&mut buffer),
    }?;
    if buffer.len() > MAX_OUTPUT_SIZE {
        return Err(SendOutboxRevertReason::OutboxMessageTooLarge {
            size: buffer.len(),
            max: MAX_OUTPUT_SIZE,
        });
    }
    Ok(())
}

fn parse_l1_routing_info(
    routing_info: &[u8],
) -> Result<(Contract, Contract), SendOutboxRevertReason> {
    let (rest, receiver) = Contract::nom_read(routing_info)?;
    let (rest, proxy) = Contract::nom_read(rest)?;

    if let Contract::Implicit(_) = proxy {
        return Err(SendOutboxRevertReason::CustomDecode(
            "Proxy address must be an originated contract".to_string(),
        ));
    }

    if !rest.is_empty() {
        return Err(SendOutboxRevertReason::CustomDecode(
            "Remaining bytes after routing info consumer".to_string(),
        ));
    }

    Ok((receiver, proxy))
}

fn send_outbox_methods<'j, CTX, Host, R>(
    input: &[u8],
    context: &mut CTX,
) -> Result<Bytes, SendOutboxRevertReason>
where
    Host: StorageV1 + 'j,
    R: Registry + 'j,
    CTX: ContextTr<Db = EtherlinkVMDB<'j, Host, R>, Journal = Journal<'j, Host, R>>,
{
    match SendOutboxMessage::SendOutboxMessageCalls::abi_decode(input)? {
        SendOutboxMessage::SendOutboxMessageCalls::push_withdrawal_to_outbox(
            SendOutboxMessage::push_withdrawal_to_outboxCall { target, amount },
        ) => {
            // Decode
            let target = Contract::from_b58check(&target)?;
            let amount = BigInt::from_bytes_be(
                Sign::Plus,
                &amount.to_be_bytes::<{ U256::BYTES }>(),
            );

            // Previous parsing step prevents failure on returned values
            let fixed_target: FixedBytes<22> =
                FixedBytes::new(target.to_bytes()?.try_into()?);

            // Build
            let ticketer = Contract::Originated(context.db().ticketer()?);
            let content = MichelsonPair(0.into(), MichelsonOption(None));
            let destination = ticketer.clone();
            let ticket = FA2_1Ticket::new(ticketer, content, amount)?;
            let entrypoint = Entrypoint::try_from(String::from("burn")).unwrap(); // Never fails
            let withdrawal = prepare_message(
                ticket,
                MessageInput::Standard { target },
                entrypoint,
                destination,
            );
            context.journal_mut().push_withdrawal(withdrawal)?;
            Ok(Bytes::from(fixed_target))
        }
        SendOutboxMessage::SendOutboxMessageCalls::push_fast_withdrawal_to_outbox(
            SendOutboxMessage::push_fast_withdrawal_to_outboxCall {
                target,
                fast_withdrawal_contract,
                payload,
                amount,
                withdrawal_id,
            },
        ) => {
            // Decode
            let target = Contract::from_b58check(&target)?;
            let fast_withdrawal_contract =
                Contract::from_b58check(&fast_withdrawal_contract)?;
            let amount = BigInt::from_bytes_be(
                Sign::Plus,
                &amount.to_be_bytes::<{ U256::BYTES }>(),
            );
            let withdrawal_id = MichelsonNat::new(Zarith(u256_to_bigint(withdrawal_id)))
                .ok_or(SendOutboxRevertReason::CustomDecode(
                    "Negative withdrawal ID".to_string(),
                ))?;

            // Previous parsing step prevents failure on returned values
            let fixed_target: FixedBytes<22> =
                FixedBytes::new(target.to_bytes()?.try_into()?);

            // Build
            let ticketer = Contract::Originated(context.db().ticketer()?);
            let content = MichelsonPair(0.into(), MichelsonOption(None));
            let ticket = FA2_1Ticket::new(ticketer, content, amount)?;
            let entrypoint = Entrypoint::try_from(String::from("default")).unwrap(); // Never fails
            let withdrawal = prepare_message(
                ticket,
                MessageInput::Fast {
                    target,
                    payload: payload.to_vec(),
                    timestamp: context.block().timestamp(),
                    withdrawal_id,
                    caller: context.tx().caller(),
                },
                entrypoint,
                fast_withdrawal_contract,
            );
            context.journal_mut().push_withdrawal(withdrawal)?;
            Ok(Bytes::from(fixed_target))
        }
        SendOutboxMessage::SendOutboxMessageCalls::push_fa_withdrawal_to_outbox(
            SendOutboxMessage::push_fa_withdrawal_to_outboxCall {
                routing_info,
                amount,
                ticketer,
                content,
            },
        ) => {
            // Decode
            let (target, proxy) = parse_l1_routing_info(&routing_info)?;
            let (_, ticketer) = Contract::nom_read(ticketer.as_slice())?;
            let amount = BigInt::from_bytes_be(
                Sign::Plus,
                &amount.to_be_bytes::<{ U256::BYTES }>(),
            );
            let (_, content) = MichelsonPair::<
                MichelsonNat,
                MichelsonOption<MichelsonBytes>,
            >::nom_read(&content)?;

            // Previous parsing step prevents failure on returns values
            let fixed_target: FixedBytes<22> =
                FixedBytes::new(target.to_bytes()?.try_into()?);
            let fixed_proxy: FixedBytes<22> =
                FixedBytes::new(proxy.to_bytes()?.try_into()?);

            // Build
            let ticket = FA2_1Ticket::new(ticketer, content, amount)?;
            let entrypoint = Entrypoint::try_from(String::from("withdraw")).unwrap(); // Never fails
            let withdrawal = prepare_message(
                ticket,
                MessageInput::Standard { target },
                entrypoint,
                proxy,
            );
            let routing_info = SendOutboxMessage::RoutingInfo {
                target: fixed_target,
                proxy: fixed_proxy,
            };
            context.journal_mut().push_withdrawal(withdrawal)?;
            Ok(Bytes::copy_from_slice(&routing_info.abi_encode()))
        }
        SendOutboxMessage::SendOutboxMessageCalls::push_fast_fa_withdrawal_to_outbox(
            SendOutboxMessage::push_fast_fa_withdrawal_to_outboxCall {
                routing_info,
                amount,
                ticketer,
                content,
                fast_withdrawal_contract_address,
                payload,
                withdrawal_id,
            },
        ) => {
            // Decode
            let (target, proxy) = parse_l1_routing_info(&routing_info)?;
            let (_, ticketer) = Contract::nom_read(ticketer.as_slice())?;
            let amount = BigInt::from_bytes_be(
                Sign::Plus,
                &amount.to_be_bytes::<{ U256::BYTES }>(),
            );
            let (_, content) = MichelsonPair::<
                MichelsonNat,
                MichelsonOption<MichelsonBytes>,
            >::nom_read(&content)?;
            let fast_withdrawal_contract =
                Contract::from_b58check(&fast_withdrawal_contract_address)?;
            let withdrawal_id = MichelsonNat::new(Zarith(u256_to_bigint(withdrawal_id)))
                .ok_or(SendOutboxRevertReason::CustomDecode(
                    "Negative withdrawal ID".to_string(),
                ))?;

            // Previous parsing step prevents failure on returned values
            let fixed_target: FixedBytes<22> =
                FixedBytes::new(target.to_bytes()?.try_into()?);
            let fixed_proxy: FixedBytes<22> =
                FixedBytes::new(proxy.to_bytes()?.try_into()?);

            // Build
            let ticket = FA2_1Ticket::new(ticketer, content, amount)?;
            let entrypoint = Entrypoint::try_from(String::from("default")).unwrap(); // Never fails
            let withdrawal = prepare_message(
                ticket,
                MessageInput::Fast {
                    target,
                    payload: payload.to_vec(),
                    timestamp: context.block().timestamp(),
                    withdrawal_id,
                    caller: context.tx().caller(),
                },
                entrypoint,
                fast_withdrawal_contract,
            );
            let routing_info = SendOutboxMessage::RoutingInfo {
                target: fixed_target,
                proxy: fixed_proxy,
            };
            context.journal_mut().push_withdrawal(withdrawal)?;
            Ok(Bytes::copy_from_slice(&routing_info.abi_encode()))
        }
    }
}

pub(crate) fn send_outbox_message_precompile<'j, CTX, Host, R>(
    calldata: &[u8],
    context: &mut CTX,
    inputs: &CallInputs,
) -> Result<InterpreterResult, CustomPrecompileError>
where
    Host: StorageV1 + 'j,
    R: Registry + 'j,
    CTX: ContextTr<Db = EtherlinkVMDB<'j, Host, R>, Journal = Journal<'j, Host, R>>,
{
    let mut gas = Gas::new(inputs.gas_limit);
    guard(
        SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS,
        &[XTZ_BRIDGE_SOL_ADDR, FA_BRIDGE_SOL_ADDR],
        inputs,
        gas,
    )?;

    charge(&mut gas, SEND_OUTBOX_MESSAGE_BASE_COST)?;

    let output =
        send_outbox_methods(calldata, context).map_err(|e| e.into_with_remainder(gas))?;

    Ok(InterpreterResult {
        result: InstructionResult::Return,
        gas,
        output,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use tezos_crypto_rs::hash::ContractKt1Hash;

    fn fast_withdrawal_with_payload(payload_len: usize) -> Withdrawal {
        let ticketer = Contract::Originated(
            ContractKt1Hash::from_base58_check("KT1NgXQ6Mwu3XKFDcKdYFS6dkkY3iNKdBKEc")
                .unwrap(),
        );
        let content = MichelsonPair(0.into(), MichelsonOption(None));
        let ticket = FA2_1Ticket::new(ticketer.clone(), content, 1).unwrap();
        let withdrawal_id = MichelsonNat::new(Zarith(BigInt::from(0))).unwrap();
        let entrypoint = Entrypoint::try_from(String::from("default")).unwrap();

        prepare_message(
            ticket,
            MessageInput::Fast {
                target: ticketer.clone(),
                payload: vec![0u8; payload_len],
                timestamp: U256::ZERO,
                withdrawal_id,
                caller: Address::ZERO,
            },
            entrypoint,
            ticketer,
        )
    }

    #[test]
    fn small_fast_withdrawal_payload_is_accepted() {
        assert!(check_outbox_message_size(&fast_withdrawal_with_payload(64)).is_ok());
    }

    #[test]
    fn oversized_fast_withdrawal_payload_is_rejected() {
        // 8 KiB payload, as in the reported attack: well above MAX_OUTPUT_SIZE.
        let result = check_outbox_message_size(&fast_withdrawal_with_payload(8 * 1024));
        assert!(matches!(
            result,
            Err(SendOutboxRevertReason::OutboxMessageTooLarge { .. })
        ));
    }
}
