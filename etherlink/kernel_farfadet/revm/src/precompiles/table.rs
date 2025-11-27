// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use alloy_sol_types::{sol, SolInterface, SolValue};
use primitive_types::{H256, U256};
use revm::{
    context::ContextTr,
    interpreter::{CallInputs, Gas, InstructionResult, InterpreterResult},
    primitives::Bytes,
};

use crate::{
    database::DatabasePrecompileStateChanges,
    helpers::legacy::{
        alloy_to_h160, alloy_to_u256, h160_to_alloy, u256_to_alloy, FaDepositWithProxy,
    },
    journal::Journal,
    precompiles::{
        constants::{
            FA_BRIDGE_SOL_ADDR, TABLE_PRECOMPILE_ADDRESS, TICKET_TABLE_BASE_COST,
        },
        error::CustomPrecompileError,
        guard::{guard, out_of_gas, revert},
    },
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

        function queue_deposit(
            SolFaDepositWithProxy memory deposit,
            uint256 deposit_id
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

pub(crate) fn table_precompile<CTX, DB>(
    calldata: &[u8],
    context: &mut CTX,
    inputs: &CallInputs,
) -> Result<InterpreterResult, CustomPrecompileError>
where
    DB: DatabasePrecompileStateChanges,
    CTX: ContextTr<Db = DB, Journal = Journal<DB>>,
{
    guard(TABLE_PRECOMPILE_ADDRESS, &[FA_BRIDGE_SOL_ADDR], inputs)?;

    let mut gas = Gas::new(inputs.gas_limit);
    if !gas.record_cost(TICKET_TABLE_BASE_COST) {
        return Ok(out_of_gas(inputs.gas_limit));
    }

    let interface = match Table::TableCalls::abi_decode(calldata) {
        Ok(data) => data,
        Err(e) => return Ok(revert(e, gas)),
    };

    let output = match interface {
        Table::TableCalls::ticket_balance_add(Table::ticket_balance_addCall {
            ticket_hash,
            owner,
            amount,
        }) => {
            context
                .journal_mut()
                .ticket_balance_add(ticket_hash, owner, amount)?;
            None
        }
        Table::TableCalls::ticket_balance_remove(Table::ticket_balance_removeCall {
            ticket_hash,
            owner,
            amount,
        }) => {
            context
                .journal_mut()
                .ticket_balance_remove(ticket_hash, owner, amount)?;
            None
        }
        Table::TableCalls::queue_deposit(Table::queue_depositCall {
            deposit,
            deposit_id,
        }) => {
            context.journal_mut().queue_deposit(
                FaDepositWithProxy {
                    amount: alloy_to_u256(&deposit.amount),
                    receiver: alloy_to_h160(&deposit.receiver),
                    proxy: alloy_to_h160(&deposit.proxy),
                    inbox_level: alloy_to_u256(&deposit.inbox_level).try_into().map_err(
                        |_| {
                            CustomPrecompileError::Revert(
                                "invalid inbox level".to_string(),
                            )
                        },
                    )?,
                    inbox_msg_id: alloy_to_u256(&deposit.inbox_msg_id)
                        .try_into()
                        .map_err(|_| {
                            CustomPrecompileError::Revert(
                                "invalid message id".to_string(),
                            )
                        })?,
                    ticket_hash: H256::from_slice(
                        &deposit.ticket_hash.to_be_bytes::<32>(),
                    ),
                },
                deposit_id,
            );
            None
        }
        Table::TableCalls::find_deposit(Table::find_depositCall { deposit_id }) => {
            let deposit = context.journal().find_deposit_in_queue(&deposit_id)?;
            let sol_deposit = Table::SolFaDepositWithProxy {
                amount: u256_to_alloy(&deposit.amount),
                receiver: h160_to_alloy(&deposit.receiver),
                proxy: h160_to_alloy(&deposit.proxy),
                inbox_level: u256_to_alloy(&U256::from(deposit.inbox_level)),
                inbox_msg_id: u256_to_alloy(&U256::from(deposit.inbox_msg_id)),
                ticket_hash: u256_to_alloy(&U256::from_big_endian(
                    deposit.ticket_hash.as_bytes(),
                )),
            };
            Some(Bytes::copy_from_slice(&sol_deposit.abi_encode_params()))
        }
        Table::TableCalls::remove_deposit(Table::remove_depositCall { deposit_id }) => {
            context
                .journal_mut()
                .remove_deposit_from_queue(deposit_id)?;
            None
        }
    };

    Ok(InterpreterResult {
        result: InstructionResult::Return,
        gas,
        output: output.unwrap_or_default(),
    })
}
