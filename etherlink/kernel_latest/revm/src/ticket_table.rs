// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use alloy_sol_types::{sol, SolEvent};
use revm::{
    context::{ContextTr, Transaction},
    interpreter::{Gas, InputsImpl, InstructionResult, InterpreterResult},
    primitives::{Address, Bytes},
};

use crate::{
    constants::FA_WITHDRAWAL_SOL_ADDR, database::PrecompileDatabase,
    send_outbox_message::revert,
};

sol! {
    event TicketTableInput (
        uint256 ticket_hash,
        address owner,
        uint256 amount,
    );
}

pub fn ticket_table_precompile<CTX>(
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
        _ => Ok(revert()),
    }
}
