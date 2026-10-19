// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025-2026 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use evm_types::{CustomPrecompileAbort, CustomPrecompileError};
use revm::{
    context::{Cfg, ContextTr, LocalContextTr},
    handler::{EthPrecompiles, PrecompileProvider},
    interpreter::{CallInput, CallInputs, Gas, InstructionResult, InterpreterResult},
    primitives::{Address, AddressSet, Bytes},
};

use tezosx_interfaces::Registry;

use crate::{
    database::EtherlinkVMDB,
    journal::Journal,
    precompiles::{
        change_sequencer_key::change_sequencer_key_precompile,
        constants::{
            CHANGE_SEQUENCER_KEY_PRECOMPILE_ADDRESS, CUSTOMS,
            GLOBAL_COUNTER_PRECOMPILE_ADDRESS, PANIC_PRECOMPILE_ADDRESS,
            RUNTIME_GATEWAY_PRECOMPILE_ADDRESS, SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS,
            TABLE_PRECOMPILE_ADDRESS, VERIFY_TEZOS_SIGNATURE_PRECOMPILE_ADDRESS,
        },
        global_counter::global_counter_precompile,
        panic::panic_precompile,
        runtime_gateway::runtime_gateway_precompile,
        send_outbox_message::send_outbox_message_precompile,
        table::table_precompile,
        verify_tezos_signature::verify_tezos_signature_precompile,
    },
    storage::version::EVMVersion,
};

use super::constants::DEBUGS;
use tezos_smart_rollup_host::storage::StorageV1;
use tezos_smart_rollup_keyspace::KeySpaceLoader;

#[derive(Debug, Clone)]
pub struct EtherlinkPrecompiles {
    builtins: EthPrecompiles,
    enable_debug_precompiles: bool,
    warm_addresses: AddressSet,
}

impl Default for EtherlinkPrecompiles {
    fn default() -> Self {
        Self::new(
            false, // no debug precompiles by default
        )
    }
}

impl EtherlinkPrecompiles {
    pub fn new(enable_debug_precompiles: bool) -> Self {
        let builtins = EthPrecompiles::new(EVMVersion::default().into());
        let warm_addresses =
            Self::compute_warm_addresses(&builtins, enable_debug_precompiles);
        Self {
            builtins,
            enable_debug_precompiles,
            warm_addresses,
        }
    }

    fn compute_warm_addresses(
        builtins: &EthPrecompiles,
        enable_debug_precompiles: bool,
    ) -> AddressSet {
        let debugs: &'static [Address] = if enable_debug_precompiles {
            &DEBUGS
        } else {
            &[]
        };

        builtins
            .warm_addresses()
            .iter()
            .copied()
            .chain(CUSTOMS)
            .chain(debugs.iter().copied())
            .collect()
    }

    /// Whether `address` is an Ethereum or Etherlink precompile.
    pub fn contains(&self, address: &Address) -> bool {
        CUSTOMS.contains(address)
            || self.builtins.contains(address)
            || (self.enable_debug_precompiles && DEBUGS.contains(address))
    }

    fn run_custom_precompile<'j, 'host, CTX, Host, R>(
        &mut self,
        context: &mut CTX,
        inputs: &CallInputs,
    ) -> Result<Option<InterpreterResult>, CustomPrecompileAbort>
    where
        'host: 'j,
        Host: StorageV1 + KeySpaceLoader + 'host,
        R: Registry<Journal = tezosx_journal::TezosXJournal> + 'j,
        CTX: ContextTr<
            Db = EtherlinkVMDB<'j, 'host, Host, R>,
            Journal = Journal<'j, 'host, Host, R>,
        >,
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
            PANIC_PRECOMPILE_ADDRESS if self.enable_debug_precompiles => {
                panic_precompile(&calldata, inputs)
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
            Err(CustomPrecompileError::RevertWithData(data, gas)) => InterpreterResult {
                result: InstructionResult::Revert,
                gas,
                output: Bytes::from(data),
            },
            Err(CustomPrecompileError::OutOfGas) => InterpreterResult {
                result: InstructionResult::OutOfGas,
                gas: Gas::new_spent_with_reservoir(inputs.gas_limit, 0),
                output: Bytes::new(),
            },
            Err(CustomPrecompileError::Abort(abort)) => return Err(abort),
        };

        Ok(Some(interpreter_result))
    }
}

impl<'j, 'host, CTX, Host, R> PrecompileProvider<CTX> for EtherlinkPrecompiles
where
    'host: 'j,
    Host: StorageV1 + KeySpaceLoader + 'host,
    R: Registry<Journal = tezosx_journal::TezosXJournal> + 'j,
    CTX: ContextTr<
        Db = EtherlinkVMDB<'j, 'host, Host, R>,
        Journal = Journal<'j, 'host, Host, R>,
    >,
{
    type Output = InterpreterResult;

    fn set_spec(&mut self, spec: <CTX::Cfg as Cfg>::Spec) -> bool {
        let changed = <EthPrecompiles as PrecompileProvider<CTX>>::set_spec(
            &mut self.builtins,
            spec,
        );
        if changed {
            self.warm_addresses = Self::compute_warm_addresses(
                &self.builtins,
                self.enable_debug_precompiles,
            );
        }
        changed
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

    fn warm_addresses(&self) -> &AddressSet {
        &self.warm_addresses
    }

    fn contains(&self, address: &Address) -> bool {
        self.contains(address)
    }
}
