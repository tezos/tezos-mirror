// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use alloy_sol_types::{sol, SolInterface, SolValue};
use primitive_types::U256;
use revm::{
    context::ContextTr,
    interpreter::{Gas, InputsImpl, InstructionResult, InterpreterResult},
    primitives::Bytes,
};

use crate::{
    database::PrecompileDatabase,
    helpers::legacy::{h160_to_alloy, u256_to_alloy},
    precompiles::{
        constants::{
            FA_WITHDRAWAL_SOL_ADDR, TABLE_PRECOMPILE_ADDRESS, TICKET_TABLE_BASE_COST,
        },
        provider::revert,
    },
    Error,
};

sol! {
    contract Table {
        function ticket_balance_add(
            uint256 ticket_hash,
            address owner,
            uint256 amount,
        ) external;

        function ticket_balance_remove(
            uint256 ticket_hash,
            address owner,
            uint256 amount,
        ) external;

        function find_deposit(uint256 deposit_id) external;

        function remove_deposit(uint256 deposit_id) external;

        struct SolFaDepositWithProxy {
            uint256 amount;
            address receiver;
            address proxy;
            uint256 ticket_hash;
            uint256 inbox_level;
            uint256 inbox_msg_id;
        }
    }
}

pub(crate) fn table_precompile<CTX>(
    input: &[u8],
    context: &mut CTX,
    is_static: bool,
    transfer: &InputsImpl,
    gas_limit: u64,
) -> Result<InterpreterResult, Error>
where
    CTX: ContextTr,
    CTX::Db: PrecompileDatabase,
{
    if transfer.target_address != TABLE_PRECOMPILE_ADDRESS {
        return Ok(revert("invalid transfer target address"));
    }

    if is_static {
        return Ok(revert("static calls are not allowed"));
    }

    if transfer.caller_address != FA_WITHDRAWAL_SOL_ADDR {
        return Ok(revert("unauthorized caller"));
    }

    let mut gas = Gas::new(gas_limit);

    if !gas.record_cost(TICKET_TABLE_BASE_COST) {
        return Ok(revert("OutOfGas"));
    }

    let interface = match Table::TableCalls::abi_decode(input) {
        Ok(data) => data,
        Err(e) => return Ok(revert(e)),
    };

    let output = match interface {
        Table::TableCalls::ticket_balance_add(Table::ticket_balance_addCall {
            ticket_hash,
            owner,
            amount,
        }) => {
            let Ok(added) =
                context
                    .db_mut()
                    .ticket_balance_add(&ticket_hash, &owner, amount)
            else {
                return Ok(revert(format!("adding {amount} balance to {owner} failed, ref. ticket hash: {ticket_hash}")));
            };
            if !added {
                return Ok(revert("ticket balance overflow"));
            }
            None
        }
        Table::TableCalls::ticket_balance_remove(Table::ticket_balance_removeCall {
            ticket_hash,
            owner,
            amount,
        }) => {
            let Ok(removed) =
                context
                    .db_mut()
                    .ticket_balance_remove(&ticket_hash, &owner, amount)
            else {
                return Ok(revert(format!("removing {amount} balance from {owner} failed, ref. ticket hash: {ticket_hash}")));
            };
            if !removed {
                return Ok(revert("insufficient ticket balance"));
            }
            None
        }
        Table::TableCalls::find_deposit(Table::find_depositCall { deposit_id }) => {
            // Only internal error is emitted here
            let Some(deposit) = context.db_mut().read_deposit_from_queue(&deposit_id)?
            else {
                return Ok(revert(format!(
                    "fetching deposit with id {deposit_id} failed"
                )));
            };
            let sol_deposit = Table::SolFaDepositWithProxy {
                amount: u256_to_alloy(&deposit.amount).unwrap_or_default(),
                receiver: h160_to_alloy(&deposit.receiver),
                proxy: h160_to_alloy(&deposit.proxy),
                inbox_level: u256_to_alloy(&U256::from(deposit.inbox_level))
                    .unwrap_or_default(),
                inbox_msg_id: u256_to_alloy(&U256::from(deposit.inbox_msg_id))
                    .unwrap_or_default(),
                ticket_hash: u256_to_alloy(&U256::from_big_endian(
                    deposit.ticket_hash.as_bytes(),
                ))
                .unwrap_or_default(),
            };
            Some(Bytes::copy_from_slice(&sol_deposit.abi_encode_params()))
        }
        Table::TableCalls::remove_deposit(Table::remove_depositCall { deposit_id }) => {
            if context
                .db_mut()
                .remove_deposit_from_queue(&deposit_id)
                .is_err()
            {
                return Ok(revert(format!(
                    "removing deposit with id {deposit_id} failed"
                )));
            }
            None
        }
    };

    Ok(InterpreterResult {
        result: InstructionResult::Return,
        gas,
        output: output.unwrap_or_default(),
    })
}
