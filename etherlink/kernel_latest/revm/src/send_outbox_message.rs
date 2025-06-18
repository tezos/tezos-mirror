// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use alloy_sol_types::{sol, SolEvent};
use num_bigint::{BigInt, Sign};
use revm::{
    context::{ContextTr, Transaction},
    interpreter::{Gas, InputsImpl, InstructionResult, InterpreterResult},
    primitives::{Address, Bytes, U256},
};
use tezos_data_encoding::nom::NomReader;
use tezos_smart_rollup_encoding::michelson::{
    MichelsonBytes, MichelsonContract, MichelsonNat, MichelsonOption, MichelsonPair,
    MichelsonTimestamp,
};
use tezos_smart_rollup_encoding::outbox::OutboxMessage;
use tezos_smart_rollup_encoding::{
    contract::Contract, entrypoint::Entrypoint, michelson::ticket::FA2_1Ticket,
    outbox::OutboxMessageTransaction,
};

use crate::database::PrecompileDatabase;

pub(crate) const SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS: &str =
    "0xff00000000000000000000000000000000000003";

sol! {
    event SendWithdrawalInput (
        bytes22 target,
        bytes22 ticketer,
        uint256 amount,
    );
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
#[allow(dead_code)]
#[derive(Debug, PartialEq, Eq)]
pub enum Withdrawal {
    Standard(OutboxMessage<RouterInterface>),
    Fast(OutboxMessage<FastWithdrawalInterface>),
}

fn revert() -> InterpreterResult {
    InterpreterResult {
        result: InstructionResult::Revert,
        gas: Gas::new(0),
        output: Bytes::new(),
    }
}

fn prepare_message(target: Contract, ticketer: Contract, amount: BigInt) -> Withdrawal {
    let ticket = FA2_1Ticket::new(
        ticketer.clone(),
        MichelsonPair(0.into(), MichelsonOption(None)),
        amount,
    )
    .unwrap();

    let parameters = MichelsonPair::<MichelsonContract, FA2_1Ticket>(
        MichelsonContract(target.clone()),
        ticket,
    );

    let entrypoint = Entrypoint::try_from(String::from("burn")).unwrap();

    let message = OutboxMessageTransaction {
        parameters,
        entrypoint,
        destination: ticketer,
    };

    Withdrawal::Standard(OutboxMessage::AtomicTransactionBatch(vec![message].into()))
}

pub fn send_outbox_message_precompile<CTX>(
    input: &[u8],
    context: &mut CTX,
    is_static: bool,
    transfer: &InputsImpl,
    current: &Address,
) -> Result<InterpreterResult, String>
where
    CTX: ContextTr,
    CTX::Db: PrecompileDatabase,
{
    if transfer.target_address != *current
        || context.tx().caller() == *current
        || is_static
    {
        return Ok(revert());
    }

    match input {
        // "0c22d28f" is the function selector for `push_withdrawal_to_outbox(bytes22,bytes22,uint256)`
        [0x0c, 0x22, 0xd2, 0x8f, input_data @ ..] => {
            // Decode
            let (target, ticketer, amount) =
                SendWithdrawalInput::abi_decode_data(input_data, true)
                    .map_err(|e| e.to_string())?;
            let (_, target) =
                Contract::nom_read(target.as_slice()).map_err(|e| e.to_string())?;
            let (_, ticketer) =
                Contract::nom_read(ticketer.as_slice()).map_err(|e| e.to_string())?;
            let amount = BigInt::from_bytes_be(
                Sign::Plus,
                &amount.to_be_bytes::<{ U256::BYTES }>(),
            );

            // Build
            let withdrawal = prepare_message(target, ticketer, amount);

            // Push
            context.db_mut().push_withdrawal(withdrawal);

            let result = InterpreterResult {
                result: InstructionResult::Return,
                gas: Gas::new(0),
                output: Bytes::new(),
            };
            Ok(result)
        }
        _ => Ok(revert()),
    }
}
