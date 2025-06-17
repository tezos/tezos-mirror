use revm::{
    context::{ContextTr, JournalTr, Transaction},
    interpreter::{Gas, InputsImpl, InstructionResult, InterpreterResult},
    primitives::{Address, Bytes, Log, B256},
};
use tezos_smart_rollup_encoding::michelson::ticket::FA2_1Ticket;
use tezos_smart_rollup_encoding::michelson::{
    MichelsonBytes, MichelsonContract, MichelsonNat, MichelsonPair, MichelsonTimestamp,
};
use tezos_smart_rollup_encoding::outbox::OutboxMessage;

use crate::database::AccountDatabase;

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

/// Keccak256 of Withdrawal(uint256,address,bytes22,uint256)
/// This is main topic (non-anonymous event): https://docs.soliditylang.org/en/latest/abi-spec.html#events
pub const WITHDRAWAL_EVENT_TOPIC: [u8; 32] = [
    45, 90, 215, 147, 24, 31, 91, 107, 215, 39, 192, 194, 22, 70, 30, 1, 158, 207, 228,
    102, 53, 63, 221, 233, 204, 248, 19, 244, 91, 132, 250, 130,
];

// `ticks_of_withdraw()` is 880_000 / 1000 (gas to tick ratio)
const WITHDRAWAL_GAS: u64 = 880;

fn revert_withdrawal() -> InterpreterResult {
    InterpreterResult {
        result: InstructionResult::Revert,
        gas: Gas::new(WITHDRAWAL_GAS),
        output: Bytes::new(),
    }
}

pub fn withdrawal_precompile<Host, CTX>(
    _host: &'_ mut Host,
    input: &[u8],
    context: &mut CTX,
    is_static: bool,
    transfer: &InputsImpl,
    current: &Address,
) -> Result<InterpreterResult, String>
where
    CTX: ContextTr,
    CTX::Db: AccountDatabase,
{
    if transfer.target_address != *current
        || context.tx().caller() == *current
        || is_static
    {
        return Ok(revert_withdrawal());
    }

    let _account = context.db_mut().get_or_create_account(*current);

    match input {
        // "cda4fee2" is the function selector for `withdraw_base58(string)`
        [0xcd, 0xa4, 0xfe, 0xe2, input_data @ ..] => {
            context.journal_mut().log(
                Log::new(
                    *current,
                    vec![B256::new(WITHDRAWAL_EVENT_TOPIC)],
                    Bytes::copy_from_slice(input_data),
                )
                .unwrap(),
            );
            let result = InterpreterResult {
                result: InstructionResult::Return,
                gas: Gas::new(0),
                output: Bytes::new(),
            };
            Ok(result)
        }
        _ => Ok(revert_withdrawal()),
    }
}
