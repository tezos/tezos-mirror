// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
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

use crate::{constants::WITHDRAWAL_SOL_ADDR, database::PrecompileDatabase};

pub(crate) const SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS: &str =
    "0xff00000000000000000000000000000000000003";

sol! {
    event SendWithdrawalInput (
        string target,
        uint256 amount,
    );
}

sol! {
    event SendFAWithdrawalInput (
        bytes22 target,
        bytes22 ticketer,
        bytes22 proxy,
        uint256 amount,
        bytes   content,
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

fn prepare_standard_message(
    target: Contract,
    ticketer: Contract,
    content: MichelsonPair<MichelsonNat, MichelsonOption<MichelsonBytes>>,
    amount: BigInt,
    entrypoint: Entrypoint,
    destination: Contract,
) -> Withdrawal {
    let ticket = FA2_1Ticket::new(ticketer, content, amount).unwrap();

    let parameters = MichelsonPair::<MichelsonContract, FA2_1Ticket>(
        MichelsonContract(target),
        ticket,
    );

    let message = OutboxMessageTransaction {
        parameters,
        entrypoint,
        destination,
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
        || transfer.caller_address != WITHDRAWAL_SOL_ADDR
        || is_static
    {
        return Ok(revert());
    }

    match input {
        // "0x7bced8e4" is the function selector for `push_withdrawal_to_outbox(string,uint256)`
        [0x7b, 0xce, 0xd8, 0xe4, input_data @ ..] => {
            // Decode
            let (target, amount) = SendWithdrawalInput::abi_decode_data(input_data, true)
                .map_err(|e| e.to_string())?;
            let target = Contract::from_b58check(&target).unwrap();
            let amount = BigInt::from_bytes_be(
                Sign::Plus,
                &amount.to_be_bytes::<{ U256::BYTES }>(),
            );

            // Build
            let ticketer = Contract::Originated(context.db().ticketer().unwrap());
            let content = MichelsonPair(0.into(), MichelsonOption(None));
            let entrypoint = Entrypoint::try_from(String::from("burn")).unwrap();
            let destination = ticketer.clone();
            let withdrawal = prepare_standard_message(
                target,
                ticketer,
                content,
                amount,
                entrypoint,
                destination,
            );

            // Push
            context.db_mut().push_withdrawal(withdrawal);

            let result = InterpreterResult {
                result: InstructionResult::Return,
                gas: Gas::new(0),
                output: Bytes::new(),
            };
            Ok(result)
        }
        // "0x0cd79fc7" is the function selector for `push_fa_withdrawal_to_outbox(bytes22,bytes22,bytes22,uint256,bytes)`
        [0x0c, 0xd7, 0x9f, 0xc7, input_data @ ..] => {
            // Decode
            let (target, ticketer, proxy, amount, content) =
                SendFAWithdrawalInput::abi_decode_data(input_data, true)
                    .map_err(|e| e.to_string())?;
            let (_, target) =
                Contract::nom_read(target.as_slice()).map_err(|e| e.to_string())?;
            let (_, ticketer) =
                Contract::nom_read(ticketer.as_slice()).map_err(|e| e.to_string())?;
            let (_, destination) =
                Contract::nom_read(proxy.as_slice()).map_err(|e| e.to_string())?;
            let amount = BigInt::from_bytes_be(
                Sign::Plus,
                &amount.to_be_bytes::<{ U256::BYTES }>(),
            );
            let (_, content) = MichelsonPair::<
                MichelsonNat,
                MichelsonOption<MichelsonBytes>,
            >::nom_read(&content)
            .map_err(|e| e.to_string())?;

            // Build
            let entrypoint = Entrypoint::try_from(String::from("withdraw")).unwrap();
            let withdrawal = prepare_standard_message(
                target,
                ticketer,
                content,
                amount,
                entrypoint,
                destination,
            );

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
