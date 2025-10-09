// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023-2024 PK Lab <contact@pklab.io>
// SPDX-FileCopyrightText: 2024-2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use std::borrow::Cow;

use crate::fast_withdrawals_enabled;
use crate::handler::EvmHandler;
use crate::handler::FastWithdrawalInterface;
use crate::handler::RouterInterface;
use crate::handler::Withdrawal;
use crate::precompiles::tick_model;
use crate::precompiles::PrecompileOutcome;
use crate::precompiles::{SYSTEM_ACCOUNT_ADDRESS, WITHDRAWAL_ADDRESS};
use crate::read_ticketer;
use crate::utilities::alloy::h160_to_alloy;
use crate::utilities::alloy::u256_to_alloy;
use crate::utilities::u256_to_bigint;
use crate::withdrawal_counter::WithdrawalCounter;
use crate::EthereumError;
use alloy_primitives::FixedBytes;
use alloy_sol_types::SolEvent;
use crypto::hash::ContractKt1Hash;
use crypto::hash::HashTrait;
use evm::Handler;
use evm::{Context, ExitReason, ExitRevert, ExitSucceed, Transfer};
use primitive_types::H160;
use primitive_types::H256;
use primitive_types::U256;
use tezos_data_encoding::enc::BinWriter;
use tezos_data_encoding::types::Zarith;
use tezos_ethereum::wei::mutez_from_wei;
use tezos_ethereum::wei::ErrorMutezFromWei;
use tezos_ethereum::Log;
use tezos_evm_logging::log;
use tezos_evm_logging::Level::Info;
use tezos_evm_runtime::runtime::Runtime;
use tezos_smart_rollup_encoding::contract::Contract;
use tezos_smart_rollup_encoding::entrypoint::Entrypoint;
use tezos_smart_rollup_encoding::michelson::ticket::FA2_1Ticket;
use tezos_smart_rollup_encoding::michelson::MichelsonBytes;
use tezos_smart_rollup_encoding::michelson::MichelsonNat;
use tezos_smart_rollup_encoding::michelson::MichelsonTimestamp;
use tezos_smart_rollup_encoding::michelson::{
    MichelsonContract, MichelsonOption, MichelsonPair,
};
use tezos_smart_rollup_encoding::outbox::{OutboxMessage, OutboxMessageTransaction};

/// Added cost of doing a withdrawal.
///
/// This is roughly the implied costs of executing the outbox message on L1
/// as a spam prevention mechanism (outbox queue clogging).
/// In particular it prevents cases when a big number of withdrawals is batched
/// together in a single transaction which exploits the system.
///
/// An execution of a single outbox message carrying a XTZ withdrawal
/// costs around 0.0025ꜩ on L1; the equivalent amount of gas units on L2 is:
///
///  0.0025 * 10^18 / GAS_PRICE
///
/// Multiplying the numerator by 2 for a safe reserve and this is our cost in Wei.
const WITHDRAWAL_PRECOMPILE_ADDED_COST: u64 = 5_000_000_000_000_000_000;

/// Hard cap for the added gas cost (0.5 of the maximum gas limit per transaction).
/// If gas price drops the gas amount rises, but we don't want it to hit the transaction
/// gas limit.
const WITHDRAWAL_PRECOMPILE_MAX_ADDED_CAS_COST: u64 = 15_000_000;

/// Keccak256 of Withdrawal(uint256,address,bytes22,uint256)
/// This is main topic (non-anonymous event): https://docs.soliditylang.org/en/latest/abi-spec.html#events
pub const WITHDRAWAL_EVENT_TOPIC: [u8; 32] = [
    45, 90, 215, 147, 24, 31, 91, 107, 215, 39, 192, 194, 22, 70, 30, 1, 158, 207, 228,
    102, 53, 63, 221, 233, 204, 248, 19, 244, 91, 132, 250, 130,
];

/// Keccak256 of FastWithdrawal(bytes22,uint256,uint256,uint256,bytes,address)
/// Arguments in this order: l1 target address, withdrawal_id, amount, timestamp
pub const FAST_WITHDRAWAL_EVENT_TOPIC: [u8; 32] = [
    0x62, 0xe8, 0xe0, 0x1e, 0x31, 0xb8, 0x30, 0x84, 0xb9, 0x7c, 0x32, 0xb1, 0xb1, 0x1a,
    0xd5, 0xa2, 0x4, 0x23, 0x82, 0xf5, 0xf6, 0x5e, 0xe4, 0x10, 0x6a, 0xd4, 0xa0, 0xd0,
    0xf8, 0xb9, 0x94, 0x2c,
];

alloy_sol_types::sol! {
    event SolStandardWithdrawalEvent (
        uint256 amount,
        address sender,
        bytes22 receiver,
        uint256 withdrawal_id,
    );
}

alloy_sol_types::sol! {
    event SolStandardWithdrawalInput (
        string target,
    );
}

alloy_sol_types::sol! {
    event SolFastWithdrawalEvent (
        bytes22 receiver,
        uint256 withdrawal_id,
        uint256 amount,
        uint256 timestamp,
        bytes   payload,
        address l2_caller,
    );
}

alloy_sol_types::sol! {
    event SolFastWithdrawalInput (
        string target,
        string fast_withdrawal_contract,
        bytes  payload,
    );
}

/// Calculate precompile gas cost given the estimated amount of ticks and gas price.
fn estimate_gas_cost(estimated_ticks: u64, gas_price: U256) -> u64 {
    // Using 1 gas unit ~= 1000 ticks convert ratio
    let execution_cost = estimated_ticks / 1000;
    let added_cost = U256::from(WITHDRAWAL_PRECOMPILE_MAX_ADDED_CAS_COST)
        .min(U256::from(WITHDRAWAL_PRECOMPILE_ADDED_COST) / gas_price);
    execution_cost + added_cost.as_u64()
}

fn prepare_message(
    parameters: RouterInterface,
    destination: Contract,
    entrypoint: Option<&str>,
) -> Option<Withdrawal> {
    let entrypoint =
        Entrypoint::try_from(String::from(entrypoint.unwrap_or("default"))).ok()?;

    let message = OutboxMessageTransaction {
        parameters,
        entrypoint,
        destination,
    };

    let outbox_message =
        Withdrawal::Standard(OutboxMessage::AtomicTransactionBatch(vec![message].into()));
    Some(outbox_message)
}

fn prepare_fast_withdraw_message(
    parameters: FastWithdrawalInterface,
    destination: Contract,
    entrypoint: Option<&str>,
) -> Option<Withdrawal> {
    let entrypoint =
        Entrypoint::try_from(String::from(entrypoint.unwrap_or("default"))).ok()?;

    let message = OutboxMessageTransaction {
        parameters,
        entrypoint,
        destination,
    };

    let outbox_message =
        Withdrawal::Fast(OutboxMessage::AtomicTransactionBatch(vec![message].into()));
    Some(outbox_message)
}

fn revert_withdrawal() -> PrecompileOutcome {
    PrecompileOutcome {
        exit_status: ExitReason::Revert(ExitRevert::Reverted),
        output: vec![],
        withdrawals: vec![],
        estimated_ticks: tick_model::ticks_of_withdraw(),
    }
}

pub struct WithdrawalBase {
    ticketer: ContractKt1Hash,
    base_transfer_value: U256,
    target: Contract,
    withdrawal_id: U256,
    ticket: FA2_1Ticket,
}

pub type BaseWithdrawOutcome = (Option<WithdrawalBase>, Option<PrecompileOutcome>);

pub fn base_withdrawal_preliminary<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    address_str: String,
    transfer: Transfer,
) -> Result<BaseWithdrawOutcome, EthereumError> {
    let Some(target) = Contract::from_b58check(&address_str).ok() else {
        log!(
            handler.borrow_host(),
            Info,
            "Withdrawal precompiled contract: invalid target address string"
        );
        return Ok((None, Some(revert_withdrawal())));
    };

    let base_transfer_value = transfer.value;

    let amount = match mutez_from_wei(base_transfer_value) {
        Ok(amount) => amount,
        Err(ErrorMutezFromWei::NonNullRemainder) => {
            log!(
                handler.borrow_host(),
                Info,
                "Withdrawal precompiled contract: rounding would lose wei"
            );
            return Ok((None, Some(revert_withdrawal())));
        }
        Err(ErrorMutezFromWei::AmountTooLarge) => {
            log!(
                handler.borrow_host(),
                Info,
                "Withdrawal precompiled contract: amount is too large"
            );
            return Ok((None, Some(revert_withdrawal())));
        }
    };

    // Burn the withdrawn amount
    let mut withdrawal_precompiled = handler.get_or_create_account(WITHDRAWAL_ADDRESS)?;
    withdrawal_precompiled.balance_remove(handler.borrow_host(), base_transfer_value)?;

    log!(
        handler.borrow_host(),
        Info,
        "Withdrawal of {} to {:?}",
        amount,
        target
    );

    let Some(ticketer) = read_ticketer(handler.borrow_host()) else {
        log!(
            handler.borrow_host(),
            Info,
            "Withdrawal precompiled contract: failed to read ticketer"
        );
        return Ok((None, Some(revert_withdrawal())));
    };

    let mut system = handler.get_or_create_account(SYSTEM_ACCOUNT_ADDRESS)?;
    let withdrawal_id =
        system.withdrawal_counter_get_and_increment(handler.borrow_host())?;

    let Some(ticket) = FA2_1Ticket::new(
        Contract::Originated(ticketer.clone()),
        MichelsonPair(0.into(), MichelsonOption(None)),
        amount,
    )
    .ok() else {
        log!(
            handler.borrow_host(),
            Info,
            "Withdrawal precompiled contract: ticket amount is invalid"
        );
        return Ok((None, Some(revert_withdrawal())));
    };

    let outcome = WithdrawalBase {
        ticketer,
        base_transfer_value,
        target,
        withdrawal_id,
        ticket,
    };

    Ok((Some(outcome), None))
}

pub fn emit_log_and_return<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    message: Withdrawal,
    estimated_ticks: u64,
    withdrawal_event: Log,
) -> Result<PrecompileOutcome, EthereumError> {
    // TODO we need to measure number of ticks and translate this number into
    // Ethereum gas units

    let withdrawals = vec![message];

    handler
        .add_log(withdrawal_event)
        .map_err(|e| EthereumError::WrappedError(Cow::from(format!("{e:?}"))))?;

    Ok(PrecompileOutcome {
        exit_status: ExitReason::Succeed(ExitSucceed::Returned),
        output: vec![],
        withdrawals,
        estimated_ticks,
    })
}

/// Implementation of Etherlink specific withdrawals precompiled contract.
pub fn withdrawal_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    context: &Context,
    is_static: bool,
    transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    let estimated_ticks = tick_model::ticks_of_withdraw();

    let estimated_gas_cost = estimate_gas_cost(estimated_ticks, handler.gas_price());
    if let Err(err) = handler.record_cost(estimated_gas_cost) {
        log!(
            handler.borrow_host(),
            Info,
            "Couldn't record the cost of withdrawal {:?}",
            err
        );
        return Ok(PrecompileOutcome {
            exit_status: ExitReason::Error(err),
            output: vec![],
            withdrawals: vec![],
            estimated_ticks,
        });
    }

    let Some(transfer) = transfer else {
        log!(
            handler.borrow_host(),
            Info,
            "Withdrawal precompiled contract: no transfer"
        );
        return Ok(revert_withdrawal());
    };

    if transfer.target != WITHDRAWAL_ADDRESS
        || context.address != WITHDRAWAL_ADDRESS
        || context.caller == WITHDRAWAL_ADDRESS
        || is_static
    {
        return Ok(revert_withdrawal());
    }

    match input {
        // "cda4fee2" is the function selector for `withdraw_base58(string)`
        [0xcd, 0xa4, 0xfe, 0xe2, input_data @ ..] => {
            // Execute base withdrawal preliminary
            let Ok((address_str,)) =
                SolStandardWithdrawalInput::abi_decode_data(input_data)
            else {
                log!(
                    handler.borrow_host(),
                    Info,
                    "Withdrawal precompiled contract: unable to get address argument"
                );
                return Ok(revert_withdrawal());
            };

            let (base_withdraw, precompile_outcome) =
                base_withdrawal_preliminary(handler, address_str, transfer)?;

            if let Some(precompile_outcome) = precompile_outcome {
                return Ok(precompile_outcome);
            }

            let Some(WithdrawalBase {
                ticketer,
                base_transfer_value,
                target,
                withdrawal_id,
                ticket,
            }) = base_withdraw
            else {
                log!(
                    handler.borrow_host(),
                    Info,
                    "Withdrawal precompiled contract: failed to execute base withdrawal"
                );
                return Ok(revert_withdrawal());
            };

            // Prepare the outbox message
            let parameters = MichelsonPair::<MichelsonContract, FA2_1Ticket>(
                MichelsonContract(target.clone()),
                ticket,
            );

            let Some(message) =
                prepare_message(parameters, Contract::Originated(ticketer), Some("burn"))
            else {
                log!(
                    handler.borrow_host(),
                    Info,
                    "Withdrawal precompiled contract: failed to encode outbox message"
                );
                return Ok(revert_withdrawal());
            };

            // Emit log and return
            let withdrawal_event = event_log(
                &base_transfer_value,
                &context.caller,
                &target,
                &withdrawal_id,
            );

            emit_log_and_return(handler, message, estimated_ticks, withdrawal_event)
        }
        // "67a32cd7" is the function selector for `fast_withdraw_base58(string,string,bytes)`
        [0x67, 0xa3, 0x2c, 0xd7, input_data @ ..] => {
            if !fast_withdrawals_enabled(handler.host) {
                let output = "The fast withdrawal feature flag is not enabled, \
                              cannot call this entrypoint.";
                return Ok(PrecompileOutcome {
                    exit_status: ExitReason::Revert(ExitRevert::Reverted),
                    output: output.as_bytes().to_vec(),
                    withdrawals: vec![],
                    estimated_ticks: tick_model::ticks_of_withdraw(),
                });
            };

            let Ok((target, fast_withdrawal_contract, payload)) =
                SolFastWithdrawalInput::abi_decode_data(input_data)
            else {
                log!(
                    handler.borrow_host(),
                    Info,
                    "Withdrawal precompiled contract: unable to get address argument"
                );
                return Ok(revert_withdrawal());
            };

            // Execute base withdrawal preliminary
            let (base_withdraw, precompile_outcome) =
                base_withdrawal_preliminary(handler, target, transfer)?;

            if let Some(precompile_outcome) = precompile_outcome {
                return Ok(precompile_outcome);
            }

            let Some(WithdrawalBase {
                ticketer: _,
                base_transfer_value,
                target,
                withdrawal_id,
                ticket,
            }) = base_withdraw
            else {
                log!(
                    handler.borrow_host(),
                    Info,
                    "Withdrawal precompiled contract: failed to execute base withdrawal"
                );
                return Ok(revert_withdrawal());
            };

            // Prepare the outbox message
            let Some(fast_withdrawal) =
                ContractKt1Hash::from_b58check(&fast_withdrawal_contract).ok()
            else {
                log!(
                    handler.borrow_host(),
                    Info,
                    "Withdrawal precompiled contract: failed to read the fast withdrawal
                     contract address"
                );
                return Ok(revert_withdrawal());
            };

            let timestamp_u256 = handler.block_timestamp();

            let Some(withdrawal_id_nat) =
                MichelsonNat::new(Zarith(u256_to_bigint(withdrawal_id)))
            else {
                log!(
                    handler.borrow_host(),
                    Info,
                    "Withdrawal precompiled contract: the withdrawal id is negative"
                );
                return Ok(revert_withdrawal());
            };

            let bytes_payload = MichelsonBytes(payload.to_vec());

            let caller = MichelsonBytes(context.caller.to_fixed_bytes().to_vec());

            let timestamp: MichelsonTimestamp =
                MichelsonTimestamp(Zarith(u256_to_bigint(timestamp_u256)));
            let contract_address = MichelsonContract(target.clone());

            let parameters = MichelsonPair(
                withdrawal_id_nat,
                MichelsonPair(
                    ticket,
                    MichelsonPair(
                        timestamp,
                        MichelsonPair(
                            contract_address,
                            MichelsonPair(bytes_payload, caller),
                        ),
                    ),
                ),
            );

            let Some(message) = prepare_fast_withdraw_message(
                parameters,
                Contract::Originated(fast_withdrawal),
                Some("default"),
            ) else {
                log!(
                    handler.borrow_host(),
                    Info,
                    "Withdrawal precompiled contract: failed to encode outbox message"
                );
                return Ok(revert_withdrawal());
            };

            // Emit log and return
            let withdrawal_event = event_log_fast_withdrawal(
                &withdrawal_id,
                &base_transfer_value,
                &timestamp_u256,
                &target,
                payload.to_vec(),
                &context.caller,
            );

            emit_log_and_return(handler, message, estimated_ticks, withdrawal_event)
        }
        // TODO A contract "function" to do withdrawal to byte encoded address
        _ => {
            log!(
                handler.borrow_host(),
                Info,
                "Withdrawal precompiled contract: invalid function selector"
            );
            Ok(revert_withdrawal())
        }
    }
}

/// Construct withdrawal event log from parts:
/// * `amount` - TEZ amount in wei
/// * `sender` - account on L2
/// * `receiver` - account on L1
/// * `withdrawal_id` - unique withdrawal ID (incremented on every successful or failed XTZ/FA withdrawal)
fn event_log(
    amount: &U256,
    sender: &H160,
    receiver: &Contract,
    withdrawal_id: &U256,
) -> Log {
    let mut receiver_bytes = vec![];
    // It is safe to unwrap, underlying implementation never fails (always returns Ok(()))
    receiver.bin_write(&mut receiver_bytes).unwrap();
    let receiver_bytes: [u8; 22] = receiver_bytes.try_into().unwrap();

    let event_data = SolStandardWithdrawalEvent {
        sender: h160_to_alloy(sender),
        amount: u256_to_alloy(amount).unwrap_or_default(),
        receiver: FixedBytes::<22>::from(&receiver_bytes),
        withdrawal_id: u256_to_alloy(withdrawal_id).unwrap_or_default(),
    };

    let data = SolStandardWithdrawalEvent::encode_data(&event_data);

    Log {
        address: WITHDRAWAL_ADDRESS,
        topics: vec![H256(WITHDRAWAL_EVENT_TOPIC)],
        data,
    }
}

/// Construct fast withdrawal event log from parts:
/// * `withdrawal_id` - unique withdrawal ID (incremented on every successful or failed XTZ/FA withdrawal)
/// * `amount` - TEZ amount in wei
/// * `timestamp` - timestamp in milliseconds since Epoch
/// * `receiver` - account on L1 that initiated the fast withdrawal
/// * `payload` - generic payload
/// * `caller` - L2 caller's address
fn event_log_fast_withdrawal(
    withdrawal_id: &U256,
    amount: &U256,
    timestamp: &U256,
    receiver: &Contract,
    payload: Vec<u8>,
    caller: &H160,
) -> Log {
    let mut receiver_bytes = vec![];
    // It is safe to unwrap, underlying implementation never fails (always returns Ok(()))
    receiver.bin_write(&mut receiver_bytes).unwrap();
    let receiver_bytes: [u8; 22] = receiver_bytes.try_into().unwrap();

    let event_data = SolFastWithdrawalEvent {
        receiver: FixedBytes::<22>::from(&receiver_bytes),
        withdrawal_id: u256_to_alloy(withdrawal_id).unwrap_or_default(),
        amount: u256_to_alloy(amount).unwrap_or_default(),
        timestamp: u256_to_alloy(timestamp).unwrap_or_default(),
        payload: payload.into(),
        l2_caller: h160_to_alloy(caller),
    };

    let data = SolFastWithdrawalEvent::encode_data(&event_data);

    Log {
        address: WITHDRAWAL_ADDRESS,
        topics: vec![H256(FAST_WITHDRAWAL_EVENT_TOPIC)],
        data,
    }
}

#[cfg(test)]
mod tests {
    use crate::{
        handler::{ExecutionOutcome, ExecutionResult, Withdrawal},
        precompiles::{
            test_helpers::{execute_precompiled, DUMMY_TICKETER},
            withdrawal::{FAST_WITHDRAWAL_EVENT_TOPIC, WITHDRAWAL_EVENT_TOPIC},
            WITHDRAWAL_ADDRESS,
        },
    };
    use alloy_sol_types::SolEvent;
    use evm::{ExitSucceed, Transfer};
    use pretty_assertions::assert_eq;
    use primitive_types::{H160, H256, U256};
    use sha3::{Digest, Keccak256};
    use std::str::FromStr;
    use tezos_ethereum::{wei::eth_from_mutez, Log};
    use tezos_smart_rollup_encoding::contract::Contract;
    use tezos_smart_rollup_encoding::michelson::ticket::FA2_1Ticket;
    use tezos_smart_rollup_encoding::michelson::{
        MichelsonContract, MichelsonOption, MichelsonPair,
    };

    use super::prepare_message;

    mod events {
        alloy_sol_types::sol! {
            event Withdrawal (
                uint256 amount,
                address sender,
                bytes22 receiver,
                uint256 withdrawalId,
            );
        }
    }

    fn make_message(ticketer: &str, target: &str, amount: u64) -> Withdrawal {
        let target = Contract::from_b58check(target).unwrap();
        let ticketer = Contract::from_b58check(ticketer).unwrap();

        let ticket = FA2_1Ticket::new(
            ticketer.clone(),
            MichelsonPair(0.into(), MichelsonOption(None)),
            amount,
        )
        .unwrap();

        let parameters = MichelsonPair::<MichelsonContract, FA2_1Ticket>(
            MichelsonContract(target),
            ticket,
        );

        prepare_message(parameters, ticketer, Some("burn")).unwrap()
    }

    #[test]
    fn withdrawal_event_signature() {
        assert_eq!(
            WITHDRAWAL_EVENT_TOPIC.to_vec(),
            Keccak256::digest(b"Withdrawal(uint256,address,bytes22,uint256)").to_vec()
        );
    }

    #[test]
    fn fast_withdrawal_event_signature() {
        assert_eq!(
            FAST_WITHDRAWAL_EVENT_TOPIC.to_vec(),
            Keccak256::digest(
                b"FastWithdrawal(bytes22,uint256,uint256,uint256,bytes,address)"
            )
            .to_vec()
        );
    }

    #[test]
    fn withdrawal_event_codec() {
        assert_eq!(events::Withdrawal::SIGNATURE_HASH.0, WITHDRAWAL_EVENT_TOPIC);

        let amount = [
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            9, 24, 78, 114, 160, 0,
        ];
        let sender = [
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 118,
        ];
        let receiver = [
            0, 0, 66, 236, 118, 95, 39, 0, 19, 78, 158, 14, 254, 137, 208, 51, 142, 46,
            132, 60, 83, 220, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
        ];
        let withdrawal_id = [1u8; 32];

        let log = Log {
            address: H160::zero(),
            topics: vec![H256(WITHDRAWAL_EVENT_TOPIC)],
            data: [amount, sender, receiver, withdrawal_id].concat(),
        };

        let log_data = alloy_primitives::LogData::new_unchecked(
            log.topics.iter().map(|x| x.0.into()).collect(),
            log.data.clone().into(),
        );
        let event = events::Withdrawal::decode_log_data(&log_data).unwrap();
        assert_eq!(event.amount, alloy_primitives::U256::from_be_bytes(amount));
        assert_eq!(
            event.sender,
            alloy_primitives::Address::from_slice(&sender[12..])
        );
        assert_eq!(
            event.receiver,
            alloy_primitives::FixedBytes::from_slice(&receiver[..22])
        );
        assert_eq!(
            event.withdrawalId,
            alloy_primitives::U256::from_be_bytes(withdrawal_id)
        );
    }

    #[test]
    fn call_withdraw_with_implicit_address() {
        // Format of input - generated by eg remix to match withdrawal ABI
        // 1. function identifier (_not_ the parameter block)
        // 2. location of first parameter (measured from start of parameter block)
        // 3. Number of bytes in string argument
        // 4. A Layer 1 contract address, hex-encoded
        // 5. Zero padding for hex-encoded address

        // cast calldata "withdraw_base58(string)" "tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w":
        let input: &[u8] = &hex::decode(
            "cda4fee2\
             0000000000000000000000000000000000000000000000000000000000000020\
             0000000000000000000000000000000000000000000000000000000000000024\
             747a31526a745a5556654c6841444648444c385577445a4136766a5757686f6a70753577\
             00000000000000000000000000000000000000000000000000000000",
        )
        .unwrap();

        let source = H160::from_low_u64_be(118u64);
        let target = WITHDRAWAL_ADDRESS;
        let value_mutez = 10;

        let transfer = Some(Transfer {
            source,
            target,
            value: eth_from_mutez(value_mutez),
        });

        let result =
            execute_precompiled(target, input, transfer, Some(30_000_000), false);

        let expected_output = vec![];
        let message = make_message(
            DUMMY_TICKETER,
            "tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w",
            value_mutez,
        );

        let expected_gas = 21000 // base cost, no additional cost for withdrawal
    + 1032 // transaction data cost (90 zero bytes + 42 non zero bytes)
    + 880 // gas for ticks
    + 15_000_000; // cost of calling withdrawal precompiled contract (hard cap because of low gas price)

        let expected_log = Log {
            address: WITHDRAWAL_ADDRESS,
            topics: vec![H256(WITHDRAWAL_EVENT_TOPIC)],
            data: [
                [
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 9, 24, 78, 114, 160, 0,
                ],
                [
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 118,
                ],
                [
                    0, 0, 66, 236, 118, 95, 39, 0, 19, 78, 158, 14, 254, 137, 208, 51,
                    142, 46, 132, 60, 83, 220, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                ],
                [0u8; 32],
            ]
            .concat(),
        };

        let expected = ExecutionOutcome {
            gas_used: expected_gas,
            logs: vec![expected_log],
            result: ExecutionResult::CallSucceeded(
                ExitSucceed::Returned,
                expected_output,
            ),
            withdrawals: vec![message],
            estimated_ticks_used: 880_000,
        };

        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn call_withdraw_with_kt1_address() {
        // Format of input - generated by eg remix to match withdrawal ABI
        // 1. function identifier (_not_ the parameter block)
        // 2. location of first parameter (measured from start of parameter block)
        // 3. Number of bytes in string argument
        // 4. A Layer 1 contract address, hex-encoded
        // 5. Zero padding for hex-encoded address

        let input: &[u8] = &hex::decode(
            "cda4fee2\
             0000000000000000000000000000000000000000000000000000000000000020\
             0000000000000000000000000000000000000000000000000000000000000024\
             4b54314275455a7462363863315134796a74636b634e6a47454c71577435365879657363\
             00000000000000000000000000000000000000000000000000000000",
        )
        .unwrap();

        let source = H160::from_low_u64_be(118u64);
        let target = H160::from_str("ff00000000000000000000000000000000000001").unwrap();
        let value_mutez = 10;

        let transfer = Some(Transfer {
            source,
            target,
            value: eth_from_mutez(value_mutez),
        });

        let result =
            execute_precompiled(target, input, transfer, Some(30_000_000), false);

        let expected_output = vec![];
        let message = make_message(
            DUMMY_TICKETER,
            "KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc",
            value_mutez,
        );

        let expected_gas = 21000 // base cost, no additional cost for withdrawal
    + 1032 // transaction data cost (90 zero bytes + 42 non zero bytes)
    + 880 // gas for ticks
    + 15_000_000; // cost of calling withdrawal precompiled contract (hard cap because of low gas price)

        let expected_log = Log {
            address: WITHDRAWAL_ADDRESS,
            topics: vec![H256(WITHDRAWAL_EVENT_TOPIC)],
            data: [
                [
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 9, 24, 78, 114, 160, 0,
                ],
                [
                    0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                    0, 0, 0, 0, 0, 0, 0, 0, 118,
                ],
                [
                    1, 36, 102, 103, 169, 49, 254, 11, 210, 251, 28, 182, 4, 247, 20, 96,
                    30, 136, 40, 69, 80, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                ],
                [0u8; 32],
            ]
            .concat(),
        };

        let expected = ExecutionOutcome {
            gas_used: expected_gas,
            logs: vec![expected_log],
            result: ExecutionResult::CallSucceeded(
                ExitSucceed::Returned,
                expected_output,
            ),
            withdrawals: vec![message],
            // TODO (#6426): estimate the ticks consumption of precompiled contracts
            estimated_ticks_used: 880_000,
        };

        assert_eq!(Ok(expected), result);
    }

    #[test]
    fn call_withdrawal_fails_without_transfer() {
        let input: &[u8] = &hex::decode(
            "cda4fee2\
             0000000000000000000000000000000000000000000000000000000000000020\
             0000000000000000000000000000000000000000000000000000000000000024\
             747a31526a745a5556654c6841444648444c385577445a4136766a5757686f6a70753577\
             00000000000000000000000000000000000000000000000000000000",
        )
        .unwrap();

        // 1. Fails with no transfer
        let target = H160::from_str("ff00000000000000000000000000000000000001").unwrap();

        let transfer: Option<Transfer> = None;

        let result =
            execute_precompiled(target, input, transfer, Some(30_000_000), false);

        let expected_gas = 21000 // base cost, no additional cost for withdrawal
    + 1032 // transaction data cost (90 zero bytes + 42 non zero bytes)
    + 880 // gas for ticks
    + 15_000_000; // cost of calling withdrawal precompiled contract (hard cap because of low gas price)

        let expected = ExecutionOutcome {
            gas_used: expected_gas,
            logs: vec![],
            result: ExecutionResult::CallReverted(vec![]),
            withdrawals: vec![],
            estimated_ticks_used: 880_000,
        };

        assert_eq!(Ok(expected), result);

        // 2. Fails with transfer of 0 amount.

        let source = H160::from_low_u64_be(118u64);

        let transfer: Option<Transfer> = Some(Transfer {
            target,
            source,
            value: U256::zero(),
        });

        let expected = ExecutionOutcome {
            gas_used: expected_gas,
            logs: vec![],
            result: ExecutionResult::CallReverted(vec![]),
            withdrawals: vec![],
            estimated_ticks_used: 880_000,
        };

        let result =
            execute_precompiled(target, input, transfer, Some(30_000_000), false);

        assert_eq!(Ok(expected), result);

        // 3. Fails if static is true.

        let source = H160::from_low_u64_be(118u64);

        let transfer: Option<Transfer> = Some(Transfer {
            target,
            source,
            value: eth_from_mutez(13),
        });

        let expected = ExecutionOutcome {
            gas_used: expected_gas,
            logs: vec![],
            result: ExecutionResult::CallReverted(vec![]),
            withdrawals: vec![],
            estimated_ticks_used: 880_000,
        };

        let result = execute_precompiled(target, input, transfer, Some(30_000_000), true);

        assert_eq!(Ok(expected), result);
    }
}
