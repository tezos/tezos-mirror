// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
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
use tezos_smart_rollup_encoding::michelson::{
    ticket::TicketError, MichelsonBytes, MichelsonContract, MichelsonNat,
    MichelsonOption, MichelsonPair, MichelsonTimestamp,
};
use tezos_smart_rollup_encoding::outbox::OutboxMessage;
use tezos_smart_rollup_encoding::{
    entrypoint::Entrypoint, michelson::ticket::FA2_1Ticket,
    outbox::OutboxMessageTransaction,
};

use crate::{
    database::DatabasePrecompileStateChanges,
    helpers::storage::u256_to_bigint,
    journal::Journal,
    precompiles::{
        constants::{
            FA_BRIDGE_SOL_ADDR, SEND_OUTBOX_MESSAGE_BASE_COST,
            SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS, WITHDRAWAL_SOL_ADDR,
        },
        error::CustomPrecompileError,
        guard::{guard, out_of_gas},
    },
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

/// Withdrawal interface of the ticketer contract
pub type RouterInterface = MichelsonPair<MichelsonContract, FA2_1Ticket>;

/// Interface of the default entrypoint of the fast withdrawal contract.
///
/// The parameters corresponds to (from left to right w.r.t. `MichelsonPair`):
/// * withdrawal_id
/// * ticket
/// * timestamp
/// * withdrawer's address
/// * generic payload
/// * l2 caller's address
pub type FastWithdrawalInterface = MichelsonPair<
    MichelsonNat,
    MichelsonPair<
        FA2_1Ticket,
        MichelsonPair<
            MichelsonTimestamp,
            MichelsonPair<
                MichelsonContract,
                MichelsonPair<MichelsonBytes, MichelsonBytes>,
            >,
        >,
    >,
>;

/// Outbox messages that implements the different withdrawal interfaces,
/// ready to be encoded and posted.
#[derive(Debug, PartialEq, Eq)]
pub enum Withdrawal {
    Standard(OutboxMessage<RouterInterface>),
    Fast(OutboxMessage<FastWithdrawalInterface>),
}

#[derive(Debug, thiserror::Error)]
enum SendOutboxRevertReason {
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

    #[error("Database access error")]
    DatabaseAccess(CustomPrecompileError),
}

impl From<CustomPrecompileError> for SendOutboxRevertReason {
    fn from(e: CustomPrecompileError) -> Self {
        SendOutboxRevertReason::DatabaseAccess(e)
    }
}

impl From<SendOutboxRevertReason> for CustomPrecompileError {
    fn from(e: SendOutboxRevertReason) -> Self {
        CustomPrecompileError::Revert(e.to_string())
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

fn send_outbox_methods<CTX, DB>(
    input: &[u8],
    context: &mut CTX,
) -> Result<Bytes, SendOutboxRevertReason>
where
    DB: DatabasePrecompileStateChanges,
    CTX: ContextTr<Db = DB, Journal = Journal<DB>>,
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
            context.journal_mut().push_withdrawal(withdrawal);
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
            context.journal_mut().push_withdrawal(withdrawal);
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
            context.journal_mut().push_withdrawal(withdrawal);
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
            context.journal_mut().push_withdrawal(withdrawal);
            Ok(Bytes::copy_from_slice(&routing_info.abi_encode()))
        }
    }
}

pub(crate) fn send_outbox_message_precompile<CTX, DB>(
    calldata: &[u8],
    context: &mut CTX,
    inputs: &CallInputs,
) -> Result<InterpreterResult, CustomPrecompileError>
where
    CTX: ContextTr,
    CTX: ContextTr<Db = DB, Journal = Journal<DB>>,
    DB: DatabasePrecompileStateChanges,
{
    guard(
        SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS,
        &[WITHDRAWAL_SOL_ADDR, FA_BRIDGE_SOL_ADDR],
        inputs,
    )?;

    let mut gas = Gas::new(inputs.gas_limit);
    if !gas.record_cost(SEND_OUTBOX_MESSAGE_BASE_COST) {
        return Ok(out_of_gas(inputs.gas_limit));
    }

    let output = send_outbox_methods(calldata, context)?;

    Ok(InterpreterResult {
        result: InstructionResult::Return,
        gas,
        output,
    })
}
