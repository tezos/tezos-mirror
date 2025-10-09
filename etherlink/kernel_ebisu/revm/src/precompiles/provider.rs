// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2025 Functori <contact@functori.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    context::{Cfg, ContextTr, LocalContextTr},
    handler::{EthPrecompiles, PrecompileProvider},
    interpreter::{CallInput, Gas, InputsImpl, InterpreterResult},
    primitives::Address,
};

use crate::{
    database::DatabasePrecompileStateChanges,
    journal::Journal,
    precompiles::{
        change_sequencer_key::change_sequencer_key_precompile,
        constants::{
            CHANGE_SEQUENCER_KEY_PRECOMPILE_ADDRESS, CUSTOMS,
            GLOBAL_COUNTER_PRECOMPILE_ADDRESS, SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS,
            TABLE_PRECOMPILE_ADDRESS,
        },
        error::CustomPrecompileError,
        global_counter::global_counter_precompile,
        guard::revert,
        send_outbox_message::send_outbox_message_precompile,
        table::table_precompile,
    },
    Error,
};

#[derive(Debug, Default, Clone)]
pub struct EtherlinkPrecompiles {
    pub builtins: EthPrecompiles,
}

impl EtherlinkPrecompiles {
    pub fn new() -> Self {
        Self {
            builtins: EthPrecompiles::default(),
        }
    }

    fn warm_addresses(&self) -> Box<impl Iterator<Item = Address>> {
        Box::new(self.builtins.warm_addresses().chain(CUSTOMS))
    }

    fn contains(&self, address: &Address) -> bool {
        CUSTOMS.contains(address) || self.builtins.contains(address)
    }

    fn run_custom_precompile<CTX, DB>(
        &mut self,
        context: &mut CTX,
        address: &Address,
        inputs: &InputsImpl,
        is_static: bool,
        gas_limit: u64,
    ) -> Result<Option<InterpreterResult>, Error>
    where
        DB: DatabasePrecompileStateChanges,
        CTX: ContextTr<Db = DB, Journal = Journal<DB>>,
    {
        // NIT: can probably do this more efficiently by keeping an immutable
        // reference on the slice but next mutable call makes it nontrivial
        let input_bytes = match &inputs.input {
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

        let result = match *address {
            SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS => send_outbox_message_precompile(
                &input_bytes,
                context,
                is_static,
                inputs,
                gas_limit,
            ),
            TABLE_PRECOMPILE_ADDRESS => {
                table_precompile(&input_bytes, context, is_static, inputs, gas_limit)
            }
            GLOBAL_COUNTER_PRECOMPILE_ADDRESS => global_counter_precompile(
                &input_bytes,
                context,
                is_static,
                inputs,
                gas_limit,
            ),
            CHANGE_SEQUENCER_KEY_PRECOMPILE_ADDRESS => change_sequencer_key_precompile(
                &input_bytes,
                context,
                is_static,
                inputs,
                gas_limit,
            ),
            _ => return Ok(None),
        };

        let interpreter_result = match result {
            Ok(interpreter_result) => interpreter_result,
            Err(CustomPrecompileError::Revert(reason)) => {
                revert(reason, Gas::new_spent(gas_limit))
            }
            Err(CustomPrecompileError::Abort(runtime)) => {
                return Err(Error::Runtime(runtime))
            }
        };

        Ok(Some(interpreter_result))
    }
}

impl<CTX, DB> PrecompileProvider<CTX> for EtherlinkPrecompiles
where
    DB: DatabasePrecompileStateChanges,
    CTX: ContextTr<Db = DB, Journal = Journal<DB>>,
{
    type Output = InterpreterResult;

    fn set_spec(&mut self, spec: <CTX::Cfg as Cfg>::Spec) -> bool {
        <EthPrecompiles as PrecompileProvider<CTX>>::set_spec(&mut self.builtins, spec)
    }

    fn run(
        &mut self,
        context: &mut CTX,
        address: &Address,
        inputs: &InputsImpl,
        is_static: bool,
        gas_limit: u64,
    ) -> Result<Option<Self::Output>, String> {
        if let Some(custom_result) = self
            .run_custom_precompile(context, address, inputs, is_static, gas_limit)
            .map_err(|e| e.to_string())?
        {
            return Ok(Some(custom_result));
        }

        self.builtins
            .run(context, address, inputs, is_static, gas_limit)
    }

    fn warm_addresses(&self) -> Box<impl Iterator<Item = Address>> {
        self.warm_addresses()
    }

    fn contains(&self, address: &Address) -> bool {
        self.contains(address)
    }
}
