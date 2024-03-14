// SPDX-FileCopyrightText: 2022-2023 TriliTech <contact@trili.tech>
// SPDX-FileCopyrightText: 2024 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use crate::handler::EvmHandler;
use crate::precompiles::tick_model;
use crate::precompiles::PrecompileOutcome;
use crate::{abi, fail_if_too_much, EthereumError};
use evm::{Context, ExitReason, ExitRevert, ExitSucceed, Transfer};
use host::runtime::Runtime;
use primitive_types::U256;
use tezos_ethereum::withdrawal::Withdrawal;
use tezos_evm_logging::log;
use tezos_evm_logging::Level::Info;

/// Cost of doing a withdrawal. A valid call to this precompiled contract
/// takes almost 880000 ticks, and one gas unit takes 1000 ticks.
/// The ticks/gas ratio is from benchmarks on `ecrecover`.
pub const WITHDRAWAL_COST: u64 = 880;

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

    if U256::is_zero(&transfer.value) {
        log!(
            handler.borrow_host(),
            Info,
            "Withdrawal precompiled contract: transfer of 0"
        );
        return Ok(revert_withdrawal());
    }

    match input {
        [0xcd, 0xa4, 0xfe, 0xe2, rest @ ..] => {
            let Some(address_str) = abi::string_parameter(rest, 0) else {
                log!(handler.borrow_host(), Info, "Withdrawal precompiled contract: unable to get address argument");
                return Ok(revert_withdrawal())
            };

            log!(
                handler.borrow_host(),
                Info,
                "Withdrawal to {:?}",
                address_str
            );

            let Some(target) = Withdrawal::address_from_str(address_str) else {
                log!(handler.borrow_host(), Info, "Withdrawal precompiled contract: invalid target address string");
                return Ok(revert_withdrawal())
            };

            // TODO Check that the outbox ain't full yet

            // TODO we need to measure number of ticks and translate this number into
            // Ethereum gas units

            let withdrawals = vec![Withdrawal {
                target,
                amount: transfer.value,
            }];

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
