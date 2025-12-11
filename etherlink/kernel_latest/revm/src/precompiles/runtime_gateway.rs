use alloy_sol_types::{sol, SolInterface};
use revm::{
    context::ContextTr,
    interpreter::{CallInputs, Gas, InstructionResult, InterpreterResult},
    primitives::{alloy_primitives::IntoLogData, Bytes, Log},
};

use crate::{
    database::DatabasePrecompileStateChanges,
    journal::Journal,
    precompiles::{
        constants::{
            RUNTIME_GATEWAY_PRECOMPILE_ADDRESS, RUNTIME_GATEWAY_TRANSFER_BASE_COST,
        },
        error::CustomPrecompileError,
        guard::out_of_gas,
        runtime_gateway::RuntimeGateway::RuntimeGatewayCalls,
    },
};

sol! {
    contract RuntimeGateway {
        function transfer(
            string implicitAddress,
        ) external;
    }

    event TransferEvent(
        string implicitAddress,
        uint256 amount
    );
}

pub(crate) fn runtime_gateway_precompile<CTX, DB>(
    calldata: &[u8],
    context: &mut CTX,
    inputs: &CallInputs,
) -> Result<InterpreterResult, CustomPrecompileError>
where
    DB: DatabasePrecompileStateChanges,
    CTX: ContextTr<Db = DB, Journal = Journal<DB>>,
{
    // TODO: Do we need protection for STATICCALL, DELEGATECALL, CALLCODE?

    let mut gas = Gas::new(inputs.gas_limit);

    let Ok(function_call) = RuntimeGatewayCalls::abi_decode(calldata) else {
        return Err(CustomPrecompileError::Revert(String::from(
            "invalid input encoding",
        )));
    };

    match function_call {
        RuntimeGatewayCalls::transfer(call) => {
            if !gas.record_cost(RUNTIME_GATEWAY_TRANSFER_BASE_COST) {
                return Ok(out_of_gas(inputs.gas_limit));
            }

            let implicit_address = call.implicitAddress;
            let amount = inputs.value.get();

            // Perform the transfer
            context.db_mut().tezosx_transfer_tez(
                inputs.caller,
                &implicit_address,
                amount,
            )?;

            // Emit event
            let log_data = TransferEvent {
                implicitAddress: implicit_address,
                amount,
            };
            let log = Log {
                address: RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
                data: log_data.into_log_data(),
            };
            context.journal_mut().log(log);
        }
    }

    Ok(InterpreterResult {
        result: InstructionResult::Return,
        gas,
        output: Bytes::new(),
    })
}
