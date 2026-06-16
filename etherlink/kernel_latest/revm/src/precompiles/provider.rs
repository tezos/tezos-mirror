// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025-2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use evm_types::{CustomPrecompileAbort, CustomPrecompileError};
use revm::{
    context::{Cfg, ContextTr, LocalContextTr},
    handler::{EthPrecompiles, PrecompileProvider},
    interpreter::{CallInput, CallInputs, Gas, InstructionResult, InterpreterResult},
    primitives::{Address, Bytes},
};

use tezos_smart_rollup_host::storage::StorageV1;
use tezosx_interfaces::Registry;

use crate::{
    database::EtherlinkVMDB,
    journal::Journal,
    precompiles::{
        change_sequencer_key::change_sequencer_key_precompile,
        constants::{
            CHANGE_SEQUENCER_KEY_PRECOMPILE_ADDRESS, CUSTOMS,
            GLOBAL_COUNTER_PRECOMPILE_ADDRESS, RUNTIME_GATEWAY_PRECOMPILE_ADDRESS,
            SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS, TABLE_PRECOMPILE_ADDRESS,
            VERIFY_TEZOS_SIGNATURE_PRECOMPILE_ADDRESS,
        },
        global_counter::global_counter_precompile,
        runtime_gateway::runtime_gateway_precompile,
        send_outbox_message::send_outbox_message_precompile,
        table::table_precompile,
        verify_tezos_signature::verify_tezos_signature_precompile,
    },
    storage::version::EVMVersion,
};

#[derive(Debug, Clone)]
pub struct EtherlinkPrecompiles {
    pub builtins: EthPrecompiles,
}

impl Default for EtherlinkPrecompiles {
    fn default() -> Self {
        Self::new()
    }
}

impl EtherlinkPrecompiles {
    pub fn new() -> Self {
        Self {
            builtins: EthPrecompiles::new(EVMVersion::default().into()),
        }
    }

    fn warm_addresses(&self) -> Box<impl Iterator<Item = Address>> {
        Box::new(self.builtins.warm_addresses().chain(CUSTOMS))
    }

    fn contains(&self, address: &Address) -> bool {
        CUSTOMS.contains(address) || self.builtins.contains(address)
    }

    fn run_custom_precompile<'j, CTX, Host, R>(
        &mut self,
        context: &mut CTX,
        inputs: &CallInputs,
    ) -> Result<Option<InterpreterResult>, CustomPrecompileAbort>
    where
        Host: StorageV1 + 'j,
        R: Registry + 'j,
        CTX: ContextTr<Db = EtherlinkVMDB<'j, Host, R>, Journal = Journal<'j, Host, R>>,
    {
        // NIT: can probably do this more efficiently by keeping an immutable
        // reference on the slice but next mutable call makes it nontrivial
        let calldata = match &inputs.input {
            CallInput::SharedBuffer(range) => {
                if let Some(slice) =
                    context.local().shared_memory_buffer_slice(range.clone())
                {
                    slice.to_vec()
                } else {
                    vec![]
                }
            }
            CallInput::Bytes(bytes) => bytes.to_vec(),
        };

        let result = match inputs.bytecode_address {
            SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS => {
                send_outbox_message_precompile(&calldata, context, inputs)
            }
            TABLE_PRECOMPILE_ADDRESS => table_precompile(&calldata, context, inputs),
            GLOBAL_COUNTER_PRECOMPILE_ADDRESS => {
                global_counter_precompile(&calldata, context, inputs)
            }
            CHANGE_SEQUENCER_KEY_PRECOMPILE_ADDRESS => {
                change_sequencer_key_precompile(&calldata, context, inputs)
            }
            RUNTIME_GATEWAY_PRECOMPILE_ADDRESS => {
                runtime_gateway_precompile(&calldata, context, inputs)
            }
            VERIFY_TEZOS_SIGNATURE_PRECOMPILE_ADDRESS => {
                verify_tezos_signature_precompile(&calldata, inputs)
            }
            _ => return Ok(None),
        };

        let interpreter_result = match result {
            Ok(interpreter_result) => interpreter_result,
            Err(CustomPrecompileError::Revert(reason, gas)) => InterpreterResult {
                result: InstructionResult::Revert,
                gas,
                output: Bytes::copy_from_slice(reason.as_bytes()),
            },
            Err(CustomPrecompileError::OutOfGas) => InterpreterResult {
                result: InstructionResult::OutOfGas,
                gas: Gas::new_spent(inputs.gas_limit),
                output: Bytes::new(),
            },
            Err(CustomPrecompileError::Abort(abort)) => return Err(abort),
        };

        Ok(Some(interpreter_result))
    }
}

impl<'j, CTX, Host, R> PrecompileProvider<CTX> for EtherlinkPrecompiles
where
    Host: StorageV1 + 'j,
    R: Registry + 'j,
    CTX: ContextTr<Db = EtherlinkVMDB<'j, Host, R>, Journal = Journal<'j, Host, R>>,
{
    type Output = InterpreterResult;

    fn set_spec(&mut self, spec: <CTX::Cfg as Cfg>::Spec) -> bool {
        <EthPrecompiles as PrecompileProvider<CTX>>::set_spec(&mut self.builtins, spec)
    }

    fn run(
        &mut self,
        context: &mut CTX,
        inputs: &CallInputs,
    ) -> Result<Option<Self::Output>, String> {
        if let Some(custom_result) = self
            .run_custom_precompile(context, inputs)
            .map_err(|e| e.to_string())?
        {
            return Ok(Some(custom_result));
        }

        self.builtins.run(context, inputs)
    }

    fn warm_addresses(&self) -> Box<impl Iterator<Item = Address>> {
        self.warm_addresses()
    }

    fn contains(&self, address: &Address) -> bool {
        self.contains(address)
    }
}
