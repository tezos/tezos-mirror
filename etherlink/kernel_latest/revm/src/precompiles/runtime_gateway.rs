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

        function call(
            string destination,
            string entrypoint,
            bytes parameters,
        ) external;
    }

    event TransferEvent(
        string implicitAddress,
        uint256 amount
    );

    event CallEvent(
        string destination,
        string entrypoint,
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

            // Perform the transfer (no entrypoint/parameters)
            context.db_mut().tezosx_call_michelson(
                inputs.caller,
                &implicit_address,
                amount,
                &[],
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
        RuntimeGatewayCalls::call(call) => {
            if !gas.record_cost(RUNTIME_GATEWAY_TRANSFER_BASE_COST) {
                return Ok(out_of_gas(inputs.gas_limit));
            }

            let destination = call.destination;
            let entrypoint = call.entrypoint;
            let parameters = call.parameters;
            let amount = inputs.value.get();

            // Encode entrypoint + parameters into the bridge data field
            // Format: [2 bytes: entrypoint length][entrypoint UTF-8][Micheline bytes]
            let entrypoint_bytes = entrypoint.as_bytes();
            let ep_len: u16 = entrypoint_bytes.len().try_into().map_err(|_| {
                CustomPrecompileError::Revert("entrypoint name too long".into())
            })?;
            let mut data =
                Vec::with_capacity(2 + entrypoint_bytes.len() + parameters.len());
            data.extend_from_slice(&ep_len.to_be_bytes());
            data.extend_from_slice(entrypoint_bytes);
            data.extend_from_slice(&parameters);

            // Perform the cross-runtime call
            context.db_mut().tezosx_call_michelson(
                inputs.caller,
                &destination,
                amount,
                &data,
            )?;

            // Emit event
            let log_data = CallEvent {
                destination: destination.clone(),
                entrypoint: entrypoint.clone(),
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
