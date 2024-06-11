// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2023-2024 PK Lab <contact@pklab.io>
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::handler::EvmHandler;
use crate::handler::RouterInterface;
use crate::handler::Withdrawal;
use crate::precompiles::tick_model;
use crate::precompiles::PrecompileOutcome;
use crate::read_ticketer;
use crate::{abi, fail_if_too_much, EthereumError};
use evm::{Context, ExitReason, ExitRevert, ExitSucceed, Transfer};
use host::runtime::Runtime;
use tezos_ethereum::wei::mutez_from_wei;
use tezos_ethereum::wei::ErrorMutezFromWei;
use tezos_evm_logging::log;
use tezos_evm_logging::Level::Info;
use tezos_smart_rollup_encoding::contract::Contract;
use tezos_smart_rollup_encoding::entrypoint::Entrypoint;
use tezos_smart_rollup_encoding::michelson::ticket::FA2_1Ticket;
use tezos_smart_rollup_encoding::michelson::{
    MichelsonContract, MichelsonOption, MichelsonPair,
};
use tezos_smart_rollup_encoding::outbox::{OutboxMessage, OutboxMessageTransaction};

/// Cost of doing a withdrawal. A valid call to this precompiled contract
/// takes almost 880000 ticks, and one gas unit takes 1000 ticks.
/// The ticks/gas ratio is from benchmarks on `ecrecover`.
const WITHDRAWAL_COST: u64 = 880;

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

    let outbox_message = OutboxMessage::AtomicTransactionBatch(vec![message].into());
    Some(outbox_message)
}

/// Implementation of Etherlink specific withdrawals precompiled contract.
pub fn withdrawal_precompile<Host: Runtime>(
    handler: &mut EvmHandler<Host>,
    input: &[u8],
    _context: &Context,
    _is_static: bool,
    transfer: Option<Transfer>,
) -> Result<PrecompileOutcome, EthereumError> {
    let estimated_ticks = fail_if_too_much!(tick_model::ticks_of_withdraw(), handler);
    fn revert_withdrawal() -> PrecompileOutcome {
        PrecompileOutcome {
            exit_status: ExitReason::Revert(ExitRevert::Reverted),
            output: vec![],
            withdrawals: vec![],
            estimated_ticks: tick_model::ticks_of_withdraw(),
        }
    }

    if let Err(err) = handler.record_cost(WITHDRAWAL_COST) {
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
        log!(handler.borrow_host(), Info, "Withdrawal precompiled contract: no transfer");
        return Ok(revert_withdrawal())
    };

    match input {
        // "cda4fee2" is the function selector for `withdraw_base58(string)`
        [0xcd, 0xa4, 0xfe, 0xe2, input_data @ ..] => {
            let Some(address_str) = abi::string_parameter(input_data, 0) else {
                log!(handler.borrow_host(), Info, "Withdrawal precompiled contract: unable to get address argument");
                return Ok(revert_withdrawal())
            };

            let Some(target) = Contract::from_b58check(address_str).ok() else {
                log!(handler.borrow_host(), Info, "Withdrawal precompiled contract: invalid target address string");
                return Ok(revert_withdrawal())
            };

            let amount = match mutez_from_wei(transfer.value) {
                Ok(amount) => amount,
                Err(ErrorMutezFromWei::NonNullRemainder) => {
                    log!(
                        handler.borrow_host(),
                        Info,
                        "Withdrawal precompiled contract: rounding would lose wei"
                    );
                    return Ok(revert_withdrawal());
                }
                Err(ErrorMutezFromWei::AmountTooLarge) => {
                    log!(
                        handler.borrow_host(),
                        Info,
                        "Withdrawal precompiled contract: amount is too large"
                    );
                    return Ok(revert_withdrawal());
                }
            };

            log!(
                handler.borrow_host(),
                Info,
                "Withdrawal of {} to {:?}",
                amount,
                target
            );

            let Some(ticketer) = read_ticketer(handler.borrow_host()) else {
                log!(handler.borrow_host(), Info, "Withdrawal precompiled contract: failed to read ticketer");
                return Ok(revert_withdrawal())
            };

            let Some(ticket) = FA2_1Ticket::new(
                Contract::Originated(ticketer.clone()),
                MichelsonPair(0.into(), MichelsonOption(None)),
                amount,
            ).ok() else {
                log!(handler.borrow_host(), Info, "Withdrawal precompiled contract: ticket amount is invalid");
                return Ok(revert_withdrawal())
            };

            let parameters = MichelsonPair::<MichelsonContract, FA2_1Ticket>(
                MichelsonContract(target),
                ticket,
            );

            let Some(message) = prepare_message(
                parameters,
                Contract::Originated(ticketer),
                Some("burn"),
            ) else {
                log!(handler.borrow_host(), Info, "Withdrawal precompiled contract: failed to encode outbox message");
                return Ok(revert_withdrawal());
            };

            // TODO we need to measure number of ticks and translate this number into
            // Ethereum gas units

            let withdrawals = vec![message];

            Ok(PrecompileOutcome {
                exit_status: ExitReason::Succeed(ExitSucceed::Returned),
                output: vec![],
                withdrawals,
                estimated_ticks,
            })
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

#[cfg(test)]
mod tests {
    use crate::{
        handler::{ExecutionOutcome, Withdrawal},
        precompiles::{
            test_helpers::{execute_precompiled, DUMMY_TICKETER},
            withdrawal::WITHDRAWAL_COST,
        },
    };
    use evm::{ExitReason, ExitRevert, ExitSucceed, Transfer};
    use pretty_assertions::assert_eq;
    use primitive_types::{H160, U256};
    use std::str::FromStr;
    use tezos_ethereum::wei::eth_from_mutez;
    use tezos_smart_rollup_encoding::contract::Contract;
    use tezos_smart_rollup_encoding::michelson::ticket::FA2_1Ticket;
    use tezos_smart_rollup_encoding::michelson::{
        MichelsonContract, MichelsonOption, MichelsonPair,
    };

    use super::prepare_message;

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
        let target = H160::from_str("ff00000000000000000000000000000000000001").unwrap();
        let value_mutez = 10;

        let transfer = Some(Transfer {
            source,
            target,
            value: eth_from_mutez(value_mutez),
        });

        let result = execute_precompiled(target, input, transfer, Some(25000));

        let expected_output = vec![];
        let message = make_message(
            DUMMY_TICKETER,
            "tz1RjtZUVeLhADFHDL8UwDZA6vjWWhojpu5w",
            value_mutez,
        );

        let expected_gas = 21000 // base cost, no additional cost for withdrawal
    + 1032 // transaction data cost (90 zero bytes + 42 non zero bytes)
    + WITHDRAWAL_COST; // cost of calling withdrawal precompiled contract

        let expected = ExecutionOutcome {
            gas_used: expected_gas,
            reason: ExitReason::Succeed(ExitSucceed::Returned).into(),
            new_address: None,
            logs: vec![],
            result: Some(expected_output),
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

        let result = execute_precompiled(target, input, transfer, Some(25000));

        let expected_output = vec![];
        let message = make_message(
            DUMMY_TICKETER,
            "KT1BuEZtb68c1Q4yjtckcNjGELqWt56Xyesc",
            value_mutez,
        );

        let expected_gas = 21000 // base cost, no additional cost for withdrawal
    + 1032 // transaction data cost (90 zero bytes + 42 non zero bytes)
    + WITHDRAWAL_COST; // cost of calling withdrawal precompiled contract

        let expected = ExecutionOutcome {
            gas_used: expected_gas,
            reason: ExitReason::Succeed(ExitSucceed::Returned).into(),
            new_address: None,
            logs: vec![],
            result: Some(expected_output),
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

        let result = execute_precompiled(target, input, transfer, Some(25000));

        let expected_gas = 21000 // base cost, no additional cost for withdrawal
    + 1032 // transaction data cost (90 zero bytes + 42 non zero bytes)
    + WITHDRAWAL_COST; // cost of calling the withdrawals precompiled contract.

        let expected = ExecutionOutcome {
            gas_used: expected_gas,
            reason: ExitReason::Revert(ExitRevert::Reverted).into(),
            new_address: None,
            logs: vec![],
            result: Some(vec![]),
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
            reason: ExitReason::Revert(ExitRevert::Reverted).into(),
            new_address: None,
            logs: vec![],
            result: Some(vec![]),
            withdrawals: vec![],
            estimated_ticks_used: 880_000,
        };

        let result = execute_precompiled(target, input, transfer, Some(25000));

        assert_eq!(Ok(expected), result);
    }
}
