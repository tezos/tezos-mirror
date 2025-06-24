// SPDX-FileCopyrightText: 2025 Nomadic Labs <contact@nomadic-labs.com>
//
// SPDX-License-Identifier: MIT

use revm::{
    context::{Cfg, ContextTr, LocalContextTr},
    handler::{EthPrecompiles, PrecompileProvider},
    interpreter::{CallInput, InputsImpl, InterpreterResult},
    primitives::Address,
};

use crate::{
    constants::{
        CUSTOMS, SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS, TICKET_TABLE_PRECOMPILE_ADDRESS,
    },
    database::PrecompileDatabase,
    send_outbox_message::send_outbox_message_precompile,
    ticket_table::ticket_table_precompile,
};

#[derive(Debug, Default)]
pub struct EtherlinkPrecompiles {
    builtins: EthPrecompiles,
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

    fn run_custom_precompile<CTX>(
        &mut self,
        context: &mut CTX,
        address: &Address,
        inputs: &InputsImpl,
        is_static: bool,
        gas_limit: u64,
    ) -> Result<Option<InterpreterResult>, String>
    where
        CTX: ContextTr,
        CTX::Db: PrecompileDatabase,
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

        match *address {
            SEND_OUTBOX_MESSAGE_PRECOMPILE_ADDRESS => {
                let result = send_outbox_message_precompile(
                    &input_bytes,
                    context,
                    is_static,
                    inputs,
                    address,
                )?;
                Ok(Some(result))
            }
            TICKET_TABLE_PRECOMPILE_ADDRESS => {
                let result = ticket_table_precompile(
                    &input_bytes,
                    context,
                    is_static,
                    inputs,
                    address,
                    gas_limit,
                )?;
                Ok(Some(result))
            }
            _ => Ok(None),
        }
    }
}

impl<CTX> PrecompileProvider<CTX> for EtherlinkPrecompiles
where
    CTX: ContextTr,
    CTX::Db: PrecompileDatabase,
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
        if let Some(custom_result) =
            self.run_custom_precompile(context, address, inputs, is_static, gas_limit)?
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
