// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use alloy_sol_types::{sol, SolEvent, SolValue};
use primitive_types::U256;
use revm::{
    context::{ContextTr, Transaction},
    interpreter::{Gas, InputsImpl, InstructionResult, InterpreterResult},
    primitives::{Address, Bytes},
};

use crate::{
    constants::FA_WITHDRAWAL_SOL_ADDR,
    database::PrecompileDatabase,
    helpers::legacy::{h160_to_alloy, u256_to_alloy},
    send_outbox_message::revert,
};

sol! {
    event TicketTableInput (
        uint256 ticket_hash,
        address owner,
        uint256 amount,
    );
}

sol! {
    event DepositTableInput (
        uint256 deposit_id
    );
}

sol! {
    struct SolFaDepositWithProxy {
        uint256 amount;
        address receiver;
        address proxy;
        uint256 ticket_hash;
        uint256 inbox_level;
        uint256 inbox_msg_id;
    }
}

pub(crate) fn table_precompile<CTX>(
    input: &[u8],
    context: &mut CTX,
    is_static: bool,
    transfer: &InputsImpl,
    current: &Address,
    gas_limit: u64,
) -> Result<InterpreterResult, String>
where
    CTX: ContextTr,
    CTX::Db: PrecompileDatabase,
{
    if transfer.target_address != *current
        || is_static
        || context.tx().caller() == *current
        || transfer.caller_address != FA_WITHDRAWAL_SOL_ADDR
    {
        return Ok(revert());
    }

    match input {
        // "0xf9d70f69" is the function selector for `ticket_balance_add(uint256,address,uint256)`
        [0xf9, 0xd7, 0x0f, 0x69, input_data @ ..] => {
            let (ticket_hash, owner, amount) =
                TicketTableInput::abi_decode_data(input_data)
                    .map_err(|e| e.to_string())?;

            let added = context
                .db_mut()
                .ticket_balance_add(&ticket_hash, &owner, amount)
                .map_err(|e| e.to_string())?;

            if !added {
                return Ok(revert());
            }

            let result = InterpreterResult {
                result: InstructionResult::Return,
                gas: Gas::new(gas_limit),
                output: Bytes::new(),
            };
            Ok(result)
        }
        // "0xd93ad063" is the function selector for `ticket_balance_remove(uint256,address,uint256)`
        [0xd9, 0x3a, 0xd0, 0x63, input_data @ ..] => {
            let (ticket_hash, owner, amount) =
                TicketTableInput::abi_decode_data(input_data)
                    .map_err(|e| e.to_string())?;

            let removed = context
                .db_mut()
                .ticket_balance_remove(&ticket_hash, &owner, amount)
                .map_err(|e| e.to_string())?;

            if !removed {
                return Ok(revert());
            }

            let result = InterpreterResult {
                result: InstructionResult::Return,
                gas: Gas::new(gas_limit),
                output: Bytes::new(),
            };
            Ok(result)
        }
        // "0x15b0773a" is the function selector for `find_deposit(uint256)`
        [0x15, 0xb0, 0x77, 0x3a, input_data @ ..] => {
            let (deposit_id,) = DepositTableInput::abi_decode_data(input_data)
                .map_err(|e| e.to_string())?;

            let deposit = context
                .db()
                .read_deposit_from_queue(&deposit_id)
                .map_err(|e| e.to_string())?;

            let sol_deposit = SolFaDepositWithProxy {
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

            let output = Bytes::copy_from_slice(&sol_deposit.abi_encode_params());

            let result = InterpreterResult {
                result: InstructionResult::Return,
                gas: Gas::new(gas_limit),
                output,
            };
            Ok(result)
        }
        // "0x14db7b18" is the function selector for `remove_deposit(uint256)`
        [0x14, 0xdb, 0x7b, 0x18, input_data @ ..] => {
            let (deposit_id,) = DepositTableInput::abi_decode_data(input_data)
                .map_err(|e| e.to_string())?;

            context
                .db_mut()
                .remove_deposit_from_queue(&deposit_id)
                .map_err(|e| e.to_string())?;

            let result = InterpreterResult {
                result: InstructionResult::Return,
                gas: Gas::new(gas_limit),
                output: Bytes::new(),
            };
            Ok(result)
        }
        _ => Ok(revert()),
    }
}
