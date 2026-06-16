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

use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::Registry;

use crate::{
    database::EtherlinkVMDB,
    helpers::legacy::{alloy_to_h160, alloy_to_u256, h160_to_alloy, u256_to_alloy},
    journal::Journal,
    precompiles::{
        constants::{
            FA_BRIDGE_SOL_ADDR, TABLE_PRECOMPILE_ADDRESS, TICKET_TABLE_BASE_COST,
        },
        guard::{charge, guard},
    },
};
use evm_types::{CustomPrecompileError, FaDepositWithProxy, IntoWithRemainder};

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

pub(crate) fn table_precompile<'j, CTX, Host, R>(
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
    guard(TABLE_PRECOMPILE_ADDRESS, &[FA_BRIDGE_SOL_ADDR], inputs, gas)?;

    charge(&mut gas, TICKET_TABLE_BASE_COST)?;

    let interface = Table::TableCalls::abi_decode(calldata)
        .map_err(|e| CustomPrecompileError::Revert(e.to_string(), gas))?;

    let output = match interface {
        Table::TableCalls::ticket_balance_add(Table::ticket_balance_addCall {
            ticket_hash,
            owner,
            amount,
        }) => {
            context
                .journal_mut()
                .ticket_balance_add(ticket_hash, owner, amount)
                .map_err(|e| e.into_with_remainder(gas))?;
            None
        }
        Table::TableCalls::ticket_balance_remove(Table::ticket_balance_removeCall {
            ticket_hash,
            owner,
            amount,
        }) => {
            context
                .journal_mut()
                .ticket_balance_remove(ticket_hash, owner, amount)
                .map_err(|e| e.into_with_remainder(gas))?;
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
                                gas,
                            )
                        },
                    )?,
                    inbox_msg_id: alloy_to_u256(&deposit.inbox_msg_id)
                        .try_into()
                        .map_err(|_| {
                            CustomPrecompileError::Revert(
                                "invalid message id".to_string(),
                                gas,
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
            let deposit = context
                .journal()
                .find_deposit_in_queue(&deposit_id)
                .map_err(|e| e.into_with_remainder(gas))?;
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
                .remove_deposit_from_queue(deposit_id)
                .map_err(|e| e.into_with_remainder(gas))?;
            None
        }
    };

    Ok(InterpreterResult {
        result: InstructionResult::Return,
        gas,
        output: output.unwrap_or_default(),
    })
}
